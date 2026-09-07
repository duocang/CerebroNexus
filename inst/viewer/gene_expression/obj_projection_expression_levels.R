##----------------------------------------------------------------------------##
## Expression levels of cells in projection.
##
## The file-backed expression object stays on the Shiny process. Only the
## requested genes x cells slice is materialised there; conversion to RGB
## panels, separate panels, or a per-cell mean runs in mirai.
##----------------------------------------------------------------------------##
expression_projection_job <- cerebro_async_latest_value(session)

expression_projection_request <- reactive({
  req(
    expression_projection_cells_to_show(),
    expression_selected_genes()
  )

  cells_to_show <- expression_projection_cells_to_show()
  cells_to_show_bc <- colnames(data_set()$expression)[cells_to_show]
  n_cells <- length(cells_to_show)
  genes_data <- expression_selected_genes()
  genes_present <- intersect(
    genes_data$genes_to_display_present,
    getGeneNames()
  )
  panel_mode <- input[["expression_projection_genes_in_separate_panels"]]
  req(panel_mode)

  separate <- FALSE
  if (length(genes_present) > 0L) {
    req(expression_projection_coordinates())
    separate <- ncol(expression_projection_coordinates()) == 2L &&
      identical(panel_mode, "separate") &&
      length(genes_present) >= 2L &&
      length(genes_present) <= 9L
  }
  mode <- if (identical(panel_mode, "rgb")) {
    "rgb"
  } else if (separate) {
    "separate"
  } else {
    "aggregate"
  }

  if (length(genes_present)) {
    expression_matrix <- data_set()$getExpressionMatrix(
      cells = cells_to_show_bc,
      genes = genes_present
    )
  } else {
    expression_matrix <- matrix(numeric(0), nrow = 0L, ncol = n_cells)
  }

  list(
    key = cerebro_async_cache_key(
      available_crb_files$selected,
      mode,
      genes_present,
      genes_data[["rgb_genes"]],
      cells_to_show_bc
    ),
    args = list(
      root = file.path(
        Cerebro.options[["cerebro_root"]],
        "viewer",
        "gene_expression"
      ),
      files = "async_workers.R",
      function_name = "expression_prepare_levels",
      args = list(
        expression_matrix = expression_matrix,
        mode = mode,
        genes_present = genes_present,
        rgb_genes = genes_data[["rgb_genes"]],
        n_cells = n_cells
      )
    )
  )
})

expression_projection_expression_levels <- reactive({
  request <- expression_projection_request()
  expression_projection_job$invoke(request$key, request$args)
  expression_projection_job$result()
})
