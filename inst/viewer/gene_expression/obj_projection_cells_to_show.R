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

## The canonical rows of the selected trajectory's cells, in its row order, or
## NULL for a projection. A trajectory may leave cells out, so its rows are not
## the canonical cells by position.
expression_projection_trajectory_rows <- reactive({
  projection <- expression_projection_selected_projection()
  if (!projection %in% available_trajectories()) {
    return(NULL)
  }
  selection <- strsplit(projection, split = " // ")[[1]]
  req(
    selection[1] %in% getMethodsForTrajectories(),
    selection[2] %in% getNamesOfTrajectories(selection[1])
  )
  viewerTrajectoryCellIndex(
    getTrajectory(selection[1], selection[2]),
    selection[1],
    selection[2]
  )
})

## Canonical rows of the cells drawn; on a trajectory, its cells in its order.
expression_projection_cells_to_show <- reactive({
  percentage <- if (isTRUE(expression_projection_client_percentage())) {
    100
  } else {
    current_scatter_percentage()
  }
  cells <- viewerProjectionCellIndices(
    "expression_projection",
    metadata = viewerProjectionFirstFrameMetadata(),
    percentage = percentage
  )
  trajectory_rows <- expression_projection_trajectory_rows()
  if (is.null(trajectory_rows)) {
    return(cells)
  }
  trajectory_rows[trajectory_rows %in% cells]
})

## The rows of a trajectory's per-cell data for the cells drawn.
expression_projection_trajectory_meta <- function(meta) {
  cells <- expression_projection_cells_to_show()
  rows <- expression_projection_trajectory_rows()
  if (identical(cells, rows)) {
    meta
  } else {
    meta[match(cells, rows), , drop = FALSE]
  }
}
