##----------------------------------------------------------------------------##
## Cell meta data and position in projection.
##----------------------------------------------------------------------------##
overview_projection_data <- reactive({
  metadata <- viewerProjectionFirstFrameMetadata()
  color_variable <- overview_projection_parameters_plot()$color_variable
  columns <- viewerProjectionFirstFrameColumns(
    metadata,
    color_variable
  )
  cells_df <- viewerProjectionSubsetRows(
    metadata,
    overview_projection_cells_to_show(),
    columns
  )
  return(cells_df)
})
