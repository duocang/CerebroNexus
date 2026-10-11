##----------------------------------------------------------------------------##
## Indices of cells to show in projection.
##----------------------------------------------------------------------------##
overview_projection_cells_to_show <- reactive({
  percentage <- input[["overview_projection_percentage_cells_to_show"]]
  if (is.null(percentage)) {
    percentage <- current_scatter_defaults()$percentage_cells_to_show
  }
  viewerProjectionCellIndices(
    "overview_projection",
    metadata = viewerProjectionFirstFrameMetadata(),
    percentage = percentage
  )
})
