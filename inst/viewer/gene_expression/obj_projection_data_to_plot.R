##----------------------------------------------------------------------------##
## Object that combines all data required for updating projection plot.
##----------------------------------------------------------------------------##
expression_projection_data_to_plot_raw <- reactive({
  coordinates <- expression_projection_coordinates()
  parameters <- expression_projection_parameters_plot()
  expression_levels <- expression_projection_expression_levels()
  no_gene_selected <- length(
    expression_selected_genes()[["genes_to_display_present"]]
  ) == 0L
  ## The no-gene first frame needs only the canonical cell count/order. Full
  ## metadata is used exclusively by deferred hover/selection auxiliary data,
  ## so do not materialize it until the browser requests that supplement.
  metadata <- if (no_gene_selected) NULL else expression_projection_data()
  req(
    coordinates,
    parameters,
    expression_projection_parameters_color(),
    expression_projection_trajectory(),
    no_gene_selected ||
      nrow(coordinates) == length(expression_levels) ||
      nrow(coordinates) == length(expression_levels[[1]]),
    !is.null(input[["expression_projection_genes_in_separate_panels"]])
  )
  if (parameters[['is_trajectory']]) {
    req(
      nrow(coordinates) == nrow(expression_projection_trajectory()[['meta']])
    )
  }
  to_return <- list(
    coordinates = coordinates,
    reset_axes = isolate(expression_projection_parameters_other[[
      'reset_axes'
    ]]),
    expression_levels = expression_levels,
    plot_parameters = parameters,
    color_settings = expression_projection_parameters_color(),
    metadata = metadata,
    trajectory = expression_projection_trajectory(),
    display_mode = input[["expression_projection_genes_in_separate_panels"]],
    cell_indices = expression_projection_cells_to_show(),
    projection_resource_failed = input[[
      "expression_projection_projection_resource_failed"
    ]],
    separate_panels = identical(
      input[["expression_projection_genes_in_separate_panels"]],
      "separate"
    )
  )
  return(to_return)
})

expression_projection_data_to_plot <- debounceAfterFirst(
  expression_projection_data_to_plot_raw,
  50
)
