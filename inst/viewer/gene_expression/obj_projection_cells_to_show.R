##----------------------------------------------------------------------------##
## Indices of cells to show in projection.
##----------------------------------------------------------------------------##
## With a Viewer Pack the frame carries every cell and the browser hides a
## deterministic share of them, as on Overview, so the "% of cells" slider does
## not resample and resend the expression of a million cells from R.
expression_projection_client_percentage <- reactive({
  pack <- viewerPackCurrent()
  is.list(pack) && isTRUE(pack$canonical_order)
})

expression_projection_cells_to_show <- reactive({
  percentage <- if (isTRUE(expression_projection_client_percentage())) {
    100
  } else {
    current_scatter_percentage()
  }
  viewerProjectionCellIndices(
    "expression_projection",
    metadata = viewerProjectionFirstFrameMetadata(),
    percentage = percentage
  )
})
