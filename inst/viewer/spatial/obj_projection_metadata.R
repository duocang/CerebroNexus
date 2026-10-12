##----------------------------------------------------------------------------##
## Cell meta data and position in projection.
##----------------------------------------------------------------------------##
spatial_projection_metadata <- reactive({
  req(spatial_projection_cells_to_show())
  color_variable <- if (
    identical(input[["spatial_projection_plot_type"]], "ImageDimPlot")
  ) {
    input[["spatial_projection_point_color"]]
  } else {
    character()
  }
  metadata <- viewerProjectionSubsetRows(
    viewerProjectionFirstFrameMetadata(),
    spatial_projection_cells_to_show(),
    unique(Filter(
      nzchar,
      c(
        color_variable,
        spatial_projection_parameters_plot()$split_by
      )
    ))
  )
  metadata[["cell_index"]] <- spatial_projection_cells_to_show()

  return(metadata)
})
