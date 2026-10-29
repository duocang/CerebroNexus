##----------------------------------------------------------------------------##
## Indices of cells to show in projection.
##----------------------------------------------------------------------------##
## With a Viewer Pack the browser fetches the full canonical projection from a
## static resource and hides a deterministic share of cells itself, so the "%
## of cells" slider no longer resamples and resends coordinates from R.
overview_projection_client_percentage <- reactive({
  viewerPackCanonical(viewerPackCurrent())
})

## With a Viewer Pack the browser applies the group filters to a frame of
## every cell, which they then do not rebuild.
overview_projection_cells <- viewerProjectionCells(
  "overview_projection",
  "overview",
  client = function() overview_projection_client_percentage(),
  rows = function(filters) {
    client <- isTRUE(overview_projection_client_percentage())
    viewerProjectionCellIndices(
      "overview_projection",
      metadata = viewerProjectionFirstFrameMetadata(),
      percentage = if (client) 100 else current_scatter_percentage(),
      filters = filters
    )
  }
)
overview_projection_group_filters <- overview_projection_cells$group_filters
overview_projection_client_filters <- overview_projection_cells$client_filters
overview_projection_cells_to_show <- overview_projection_cells$cells_to_show
