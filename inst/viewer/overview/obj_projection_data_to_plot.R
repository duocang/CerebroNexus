##----------------------------------------------------------------------------##
## Collect data required to update projection.
##----------------------------------------------------------------------------##
overview_projection_data_to_plot_raw <- reactive({
  req(overview_projection_parameters_plot())
  req(reactive_colors())
  plot_parameters <- overview_projection_parameters_plot()
  cell_indices <- overview_projection_cells_to_show()
  color_variable <- plot_parameters[['color_variable']]
  metadata <- viewerProjectionFirstFrameMetadata()
  color_input <- metadata[[color_variable]]
  if (!length(cell_indices)) {
    color_assignments <- character(0)
  } else if (is.numeric(color_input)) {
    color_assignments <- NULL
  } else {
    color_assignments <- assignColorsToGroups(metadata, color_variable)
  }
  projection_resource <- viewerProjectionAsset(
    plot_parameters[["projection"]],
    cell_indices
  )
  failed_resource <- input[[
    "overview_projection_projection_resource_failed"
  ]]
  if (
    is.list(projection_resource) &&
      length(failed_resource) == 1L &&
      !is.na(failed_resource) &&
      identical(failed_resource, projection_resource$url)
  ) {
    projection_resource <- NULL
  }
  categorical_resource <- if (
    is.list(projection_resource) && !is.numeric(color_input)
  ) {
    viewerMetadataCodesAsset(color_variable, names(color_assignments))
  } else {
    NULL
  }
  resource_first <- is.list(projection_resource) &&
    is.list(categorical_resource)
  cells_df <- if (resource_first) NULL else overview_projection_data()
  coordinates <- if (resource_first) NULL else overview_projection_coordinates()
  list(
    cells_df = cells_df,
    cell_indices = cell_indices,
    cell_count = length(cell_indices),
    coordinates = coordinates,
    reset_axes = isolate(overview_projection_parameters_other[['reset_axes']]),
    plot_parameters = plot_parameters,
    color_assignments = color_assignments,
    projection_resource = projection_resource,
    categorical_resource = categorical_resource,
    resource_first = resource_first
  )
})

overview_projection_data_to_plot <- debounceAfterFirst(
  overview_projection_data_to_plot_raw,
  150
)
