##----------------------------------------------------------------------------##
## Expression levels of cells in projection.
##
## Cache only exact full-canonical, single-gene rows. Keeping this cache outside
## the reactive graph avoids stale bindCache() hits while still making revisits
## cheap. Dataset identity and a small LRU bound prevent cross-dataset reuse and
## unbounded million-value vectors.
##----------------------------------------------------------------------------##
expression_projection_row_cache <- new.env(parent = emptyenv())
expression_projection_row_cache_order <- character()
expression_projection_row_cache_limit <- 4L

expressionProjectionDatasetKey <- function() {
  identity <- tryCatch(viewerDatasetIdentity(), error = function(error) NULL)
  selected <- tryCatch(
    as.character(available_crb_files$selected),
    error = function(error) ""
  )
  key <- as.character(identity$pack_fingerprint %||% "")
  if (length(key) != 1L || is.na(key) || !nzchar(key)) {
    key <- selected
  }
  if (length(key) != 1L || is.na(key) || !nzchar(key)) {
    key <- as.character(identity$fingerprint %||% "")
  }
  if (length(key) == 1L && !is.na(key) && nzchar(key)) key else NULL
}

expressionProjectionFullCellSelection <- function(data, cells) {
  n_cells <- tryCatch(ncol(data$expression), error = function(error) NA_integer_)
  length(n_cells) == 1L &&
    !is.na(n_cells) &&
    length(cells) == n_cells &&
    identical(as.integer(cells), seq_len(n_cells))
}

expressionProjectionCachedRow <- function(data, cells, gene) {
  dataset_key <- expressionProjectionDatasetKey()
  cacheable <- !is.null(dataset_key) &&
    expressionProjectionFullCellSelection(data, cells)
  key <- if (cacheable) paste(dataset_key, gene, sep = "::") else NULL
  if (
    !is.null(key) &&
      exists(key, envir = expression_projection_row_cache, inherits = FALSE)
  ) {
    expression_projection_row_cache_order <<- c(
      setdiff(expression_projection_row_cache_order, key),
      key
    )
    return(get(key, envir = expression_projection_row_cache, inherits = FALSE))
  }
  value <- unname(viewerExpressionRow(data, cells, gene))
  if (!is.null(key)) {
    assign(key, value, envir = expression_projection_row_cache)
    expression_projection_row_cache_order <<- c(
      setdiff(expression_projection_row_cache_order, key),
      key
    )
    while (length(expression_projection_row_cache_order) >
      expression_projection_row_cache_limit) {
      evict <- expression_projection_row_cache_order[[1L]]
      expression_projection_row_cache_order <<-
        expression_projection_row_cache_order[-1L]
      rm(list = evict, envir = expression_projection_row_cache)
    }
  }
  value
}

expressionProjectionRenderKey <- function(genes, display_mode) {
  present <- as.character(genes[["genes_to_display_present"]] %||% character())
  rgb <- genes[["rgb_genes"]] %||% list()
  rgb <- vapply(c("r", "g", "b"), function(channel) {
    value <- as.character(rgb[[channel]] %||% "")
    if (length(value) && !is.na(value[[1L]])) value[[1L]] else ""
  }, character(1))
  paste(
    as.character(display_mode %||% "single"),
    paste(present, collapse = "\034"),
    paste(rgb, collapse = "\034"),
    sep = "\035"
  )
}

expression_projection_render_key <- reactive({
  expressionProjectionRenderKey(
    expression_selected_genes(),
    input[["expression_projection_genes_in_separate_panels"]]
  )
})

expression_projection_summary_ready <- reactive({
  identical(
    input[["expression_projection_rendered_key"]],
    expression_projection_render_key()
  )
})

## Prime the BPCells row-access path only after the empty UMAP has painted.
## This keeps the fast page-open path intact while moving one-time method and
## backend initialization out of the user's first gene selection.
expression_projection_backend_warmed <- reactiveVal(FALSE)
observeEvent(input[["expression_projection_rendered_key"]], {
  if (isTRUE(expression_projection_backend_warmed())) {
    return()
  }
  genes <- expression_selected_genes()[["genes_to_display_present"]]
  if (length(genes)) {
    return()
  }
  available <- getGeneNames()
  if (!length(available)) {
    expression_projection_backend_warmed(TRUE)
    return()
  }
  try(
    viewerExpressionRow(data_set(), 1L, available[[1L]]),
    silent = TRUE
  )
  expression_projection_backend_warmed(TRUE)
}, ignoreInit = TRUE)

expression_projection_expression_levels <- reactive({
  req(
    expression_projection_cells_to_show(),
    expression_selected_genes()
  )

  withProgress(message = 'Calculating expression levels...', value = 0.2, {
    cells_to_show <- expression_projection_cells_to_show()
    ## Keep the canonical numeric indices returned by the shared projection
    ## sampler. Converting them to barcodes here only makes the backend match
    ## the same million names back to the original column indices.
    n_cells <- length(cells_to_show)
    genes_data <- expression_selected_genes()

    ## The debounced selection can briefly retain genes from the previous data
    ## set during a switch. Re-filter before touching the expression backend.
    genes_present <- intersect(
      genes_data$genes_to_display_present,
      getGeneNames()
    )
    display_mode <- expressionSummaryMode(
      input[["expression_projection_genes_in_separate_panels"]],
      length(genes_present),
      ncol(expression_projection_coordinates())
    )

    if (length(genes_present) == 0) {
      ## No gene means one constant renderer colour, not a synthetic
      ## million-cell expression vector. The transport marks this empty value
      ## as the explicit zero-colour fast path.
      expression_levels <- numeric()
    } else {
      req(expression_projection_coordinates())
      ## All branches keep the requested slice in canonical index order. The
      ## class accessors dispatch those indices across dgCMatrix, DelayedArray,
      ## and IterableMatrix without a barcode lookup.
      if (identical(display_mode, "rgb")) {
        incProgress(0.3, detail = "Calculating RGB co-expression...")
        rgb_genes <- genes_data[["rgb_genes"]]
        requested_genes <- intersect(
          unique(unlist(rgb_genes, use.names = FALSE)),
          genes_present
        )
        expression_values <- viewerExpressionValues(
          data_set(),
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
        incProgress(0.3, detail = "Extracting multiple gene panels...")
        expression_levels <- viewerExpressionValues(
          data_set(),
          cells_to_show,
          genes_present
        )
      } else if (length(genes_present) == 1) {
        incProgress(0.3, detail = "Extracting single gene expression...")
        expression_levels <- expressionProjectionCachedRow(
          data_set(),
          cells_to_show,
          genes_present[[1L]]
        )
      } else if (length(genes_present) >= 2) {
        incProgress(0.3, detail = "Calculating mean expression...")
        ## Per-cell mean across the requested genes, restricted to cells_to_show.
        expression_levels <- unname(
          data_set()$getMeanExpressionForCells(
            cells = viewerExpressionCells(data_set(), cells_to_show),
            genes = genes_present
          )
        )
      }
    }
    return(expression_levels)
  })
})

expression_summary_data <- reactive({
  req(
    expression_projection_cells_to_show(),
    expression_selected_genes(),
    input[["expression_projection_genes_in_separate_panels"]]
  )
  genes <- unique(intersect(
    as.character(expression_selected_genes()[["genes_to_display_present"]]),
    getGeneNames()
  ))
  req(length(genes) > 0)

  spec <- expressionSummarySpec(
    input[["expression_projection_genes_in_separate_panels"]],
    genes,
    expression_selected_genes()[["rgb_genes"]],
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
