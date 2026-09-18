##----------------------------------------------------------------------------##
## Expression levels of cells in projection.
##
## bindCache() was attempted here but backed out: the reactive depends on
## expression_selected_genes(), which is an eventReactive that req()s
## input$expression_analysis_mode, and the chain with isolate() inside a
## bindCache key reliably produced inconsistent body-execution behaviour on
## repeated gene switches (some clicks hit cache even when the gene had just
## changed, risking stale plots). Leaving the per-gene compute in place for
## now; step 4's extractExpression refactor is the safer place to reclaim
## repeated-click latency on this reactive.
##----------------------------------------------------------------------------##
expression_projection_progress <- new.env(parent = emptyenv())
expression_projection_progress$serial <- 0L
expression_projection_progress$current <- NULL
expression_projection_progress$token <- NULL
expression_projection_render_ready <- shiny::reactiveVal(FALSE)

expression_projection_values_cache <- new.env(parent = emptyenv())
expression_projection_values_cache$dataset <- NULL
expression_projection_values_cache$cells <- NULL
expression_projection_values_cache$values <- NULL

expressionProjectionValues <- function(cells, genes) {
  genes <- unique(as.character(genes))
  if (length(genes) == 0) {
    return(list())
  }
  dataset <- data_set()
  cached <- expression_projection_values_cache
  if (
    identical(cached$dataset, dataset) &&
      identical(cached$cells, cells) &&
      all(genes %in% names(cached$values))
  ) {
    return(cached$values[genes])
  }
  values <- viewerExpressionValues(dataset, cells, genes)
  ## Separate panels are capped at nine genes. Keeping only that last slice
  ## makes mode switches cheap without retaining an unbounded gene-set matrix.
  if (length(genes) <= 9L) {
    cached$dataset <- dataset
    cached$cells <- cells
    cached$values <- values
  }
  values
}

expressionProjectionProgressClose <- function(token = NULL) {
  if (
    !is.null(token) &&
      !identical(as.integer(token), expression_projection_progress$token)
  ) {
    return(invisible(FALSE))
  }
  if (!is.null(expression_projection_progress$current)) {
    expression_projection_progress$current$close()
  }
  expression_projection_progress$current <- NULL
  expression_projection_progress$token <- NULL
  invisible(TRUE)
}

expressionProjectionProgressStart <- function() {
  expressionProjectionProgressClose()
  expression_projection_render_ready(FALSE)
  expression_projection_progress$serial <-
    expression_projection_progress$serial + 1L
  expression_projection_progress$token <- expression_projection_progress$serial
  expression_projection_progress$current <- shiny::Progress$new(
    session,
    min = 0,
    max = 1
  )
  expression_projection_progress$current$set(
    message = "Calculating expression levels...",
    value = 0.2
  )
  expression_projection_progress$token
}

expressionProjectionProgressSet <- function(token, value, detail) {
  if (
    identical(as.integer(token), expression_projection_progress$token) &&
      !is.null(expression_projection_progress$current)
  ) {
    expression_projection_progress$current$set(value = value, detail = detail)
  }
}

expressionProjectionProgressToken <- function() {
  expression_projection_progress$token
}

expressionProjectionProgressEnsure <- function() {
  expressionProjectionProgressToken() %||% expressionProjectionProgressStart()
}

observeEvent(
  input[["expression_projection_render_complete"]],
  {
    completion <- input[["expression_projection_render_complete"]]
    token <- completion[["render_token"]]
    if (expressionProjectionProgressClose(token)) {
      session$onFlushed(
        function() expression_projection_render_ready(TRUE),
        once = TRUE
      )
    }
  },
  ignoreInit = TRUE
)

expressionProjectionRenderReady <- function() {
  isTRUE(expression_projection_render_ready())
}

observeEvent(
  input[["expression_projection_genes_in_separate_panels"]],
  {
    if (identical(input[["sidebar"]], "geneExpression")) {
      expressionProjectionProgressStart()
    }
  },
  ignoreInit = TRUE
)

observeEvent(
  input[["sidebar"]],
  {
    if (!identical(input[["sidebar"]], "geneExpression")) {
      expressionProjectionProgressClose()
    }
  },
  ignoreInit = TRUE
)

session$onSessionEnded(function() expressionProjectionProgressClose())

expression_projection_request_raw <- reactive({
  req(
    expression_projection_cells_to_show(),
    expression_selected_genes(),
    expression_projection_coordinates()
  )
  genes_data <- expression_selected_genes()
  genes_present <- intersect(
    genes_data$genes_to_display_present,
    getGeneNames()
  )
  list(
    cells_to_show = expression_projection_cells_to_show(),
    genes_data = genes_data,
    genes_present = genes_present,
    display_mode = expressionSummaryMode(
      input[["expression_projection_genes_in_separate_panels"]],
      length(genes_present),
      ncol(expression_projection_coordinates())
    )
  )
})

## RGB initialisation updates three selectize inputs in quick succession. Settle
## that transaction before touching an out-of-core expression backend.
expression_projection_request <- debounceAfterFirst(
  expression_projection_request_raw,
  350
)

expression_projection_expression_levels <- reactive({
  request <- expression_projection_request()
  req(request)

  render_token <- expressionProjectionProgressEnsure()
  prepared <- FALSE
  on.exit(
    {
      if (!prepared) expressionProjectionProgressClose(render_token)
    },
    add = TRUE
  )

  cells_to_show <- request$cells_to_show
  ## Keep the canonical numeric indices returned by the shared projection
  ## sampler. Converting them to barcodes here only makes the backend match
  ## the same million names back to the original column indices.
  n_cells <- length(cells_to_show)
  genes_data <- request$genes_data

  ## expression_selected_genes() is an eventReactive bound to the
  ## "Plot Expression" button, so its cached `genes_to_display_present` is
  ## NOT refreshed on a dataset switch. If the user previously plotted
  ## genes that exist in the old dataset but not in the new one, the cache
  ## still holds them and getExpressionMatrix(genes=) below would crash
  ## with vctrs::vec_slice "Element X doesn't exist". Re-filter against
  ## the current dataset's gene names every time this reactive fires.
  genes_present <- request$genes_present
  display_mode <- request$display_mode

  if (length(genes_present) == 0) {
    expression_levels <- if (identical(display_mode, "rgb")) {
      list(r = rep(0, n_cells), g = rep(0, n_cells), b = rep(0, n_cells))
    } else {
      rep(0, n_cells)
    }
  } else {
    req(expression_projection_coordinates())
    ## All branches keep the requested slice in canonical index order. The
    ## class accessors dispatch those indices across dgCMatrix, DelayedArray,
    ## and IterableMatrix without a barcode lookup.
    if (identical(display_mode, "rgb")) {
      expressionProjectionProgressSet(
        render_token,
        0.5,
        "Calculating RGB co-expression..."
      )
      rgb_genes <- genes_data[["rgb_genes"]]
      requested_genes <- intersect(
        unique(unlist(rgb_genes, use.names = FALSE)),
        genes_present
      )
      expression_values <- expressionProjectionValues(
        cells_to_show,
        requested_genes
      )
      expression_levels <- lapply(rgb_genes, function(gene) {
        if (is.null(gene) || !gene %in% names(expression_values)) {
          return(rep(0, n_cells))
        }
        unname(expression_values[[gene]])
      })
    } else if (identical(display_mode, "separate")) {
      expressionProjectionProgressSet(
        render_token,
        0.5,
        "Extracting multiple gene panels..."
      )
      expression_levels <- expressionProjectionValues(
        cells_to_show,
        genes_present
      )
    } else if (length(genes_present) == 1) {
      expressionProjectionProgressSet(
        render_token,
        0.5,
        "Extracting single gene expression..."
      )
      expression_levels <- unname(viewerExpressionRow(
        data_set(),
        cells_to_show,
        genes_present[[1L]]
      ))
    } else if (length(genes_present) >= 2 && length(genes_present) <= 9L) {
      expressionProjectionProgressSet(
        render_token,
        0.5,
        "Calculating mean expression..."
      )
      expression_values <- expressionProjectionValues(
        cells_to_show,
        genes_present
      )
      expression_levels <- unname(
        Reduce(`+`, expression_values) / length(expression_values)
      )
    } else if (length(genes_present) >= 2) {
      expressionProjectionProgressSet(
        render_token,
        0.5,
        "Calculating mean expression..."
      )
      ## Per-cell mean across the requested genes, restricted to cells_to_show.
      expression_levels <- unname(
        data_set()$getMeanExpressionForCells(
          cells = viewerExpressionCells(data_set(), cells_to_show),
          genes = genes_present
        )
      )
    }
  }
  prepared <- TRUE
  return(expression_levels)
})

expression_summary_data <- reactive({
  request <- expression_projection_request()
  req(request)
  genes <- unique(as.character(request$genes_present))
  req(length(genes) > 0)

  spec <- expressionSummarySpec(
    request$display_mode,
    genes,
    request$genes_data[["rgb_genes"]],
    ncol(expression_projection_coordinates())
  )
  expression_levels <- expression_projection_expression_levels()
  spec$series <- lapply(spec$series, function(series) {
    series$values <- if (identical(spec$kind, "mean")) {
      expression_levels
    } else {
      expression_levels[[series$key]]
    }
    series
  })
  spec$genes <- genes
  spec
})
