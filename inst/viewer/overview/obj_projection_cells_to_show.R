##----------------------------------------------------------------------------##
## Indices of cells to show in projection.
##----------------------------------------------------------------------------##
overview_projection_cells_to_show <- reactive({
  percentage <- current_scatter_percentage()
  viewerProjectionCellIndices(
    "overview_projection",
    metadata = viewerProjectionFirstFrameMetadata(),
    percentage = percentage
  )
})
