##----------------------------------------------------------------------------##
## Object that combines all data required for updating projection plot.
##----------------------------------------------------------------------------##
expression_projection_data_to_plot_raw <- reactive({
  coordinates <- expression_projection_coordinates()
  parameters <- expression_projection_parameters_plot()
  expression_levels <- expression_projection_expression_levels()
  request <- expression_projection_request()
  metadata <- expression_projection_data()
  req(
    coordinates,
    parameters,
    expression_projection_parameters_color(),
    expression_projection_trajectory(),
    nrow(coordinates) == length(expression_levels) ||
      nrow(coordinates) == length(expression_levels[[1]]),
    !is.null(request$display_mode)
  )
  if (parameters[['is_trajectory']]) {
    req(
      nrow(coordinates) == nrow(expression_projection_trajectory()[['meta']])
    )
  }
  to_return <- list(
    coordinates = coordinates,
    render_token = expressionProjectionProgressEnsure(),
    reset_axes = isolate(expression_projection_parameters_other[[
      'reset_axes'
    ]]),
    expression_levels = expression_levels,
    plot_parameters = parameters,
    color_settings = expression_projection_parameters_color(),
    selection_keys = as.character(metadata[["cell_barcode"]]),
    hover_columns = if (isTRUE(parameters[["hover_info"]])) {
      cerebroProjectionHoverColumns(metadata)
    } else {
      list()
    },
    trajectory = expression_projection_trajectory(),
    display_mode = request$display_mode,
    separate_panels = identical(request$display_mode, "separate")
  )
  return(to_return)
})

expression_projection_data_to_plot <- debounceAfterFirst(
  expression_projection_data_to_plot_raw,
  50
)
