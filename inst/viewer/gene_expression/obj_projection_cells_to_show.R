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

## The group filters that leave cells out, and, with a Viewer Pack and a
## projection (whose frame holds the canonical cells), the same filters for the
## browser to apply to a frame of every cell (NULL: R filters).
expression_projection_group_filters <- reactive({
  viewerProjectionGroupFilters(
    "expression_projection",
    viewerProjectionFirstFrameMetadata()
  )
})
expression_projection_client_filters <- reactive({
  if (
    !isTRUE(expression_projection_client_percentage()) ||
      !is.null(expression_projection_trajectory_rows()) ||
      isTRUE(input[["expression_projection_client_filters_failed"]])
  ) {
    return(NULL)
  }
  viewerClientFilterDescriptors(expression_projection_group_filters())
})

## Canonical rows of the cells drawn; on a trajectory, its cells in its order.
expression_projection_cells_to_show_current <- reactive({
  percentage <- if (isTRUE(expression_projection_client_percentage())) {
    100
  } else {
    current_scatter_percentage()
  }
  filters <- if (is.null(expression_projection_client_filters())) {
    expression_projection_group_filters()
  } else {
    list()
  }
  cells <- viewerProjectionCellIndices(
    "expression_projection",
    metadata = viewerProjectionFirstFrameMetadata(),
    percentage = percentage,
    filters = filters
  )
  trajectory_rows <- expression_projection_trajectory_rows()
  if (is.null(trajectory_rows)) {
    return(cells)
  }
  trajectory_rows[trajectory_rows %in% cells]
})

## Published on change only: a filter the browser applies leaves every cell in
## the frame, and must not read the expression of a million cells again.
expression_projection_cells_state <- reactiveVal(NULL)
observe(
  {
    ## Off the page, keep the last cells; returning re-evaluates.
    if (
      "sidebar" %in%
        isolate(names(input)) &&
        !identical(input[["sidebar"]], "geneExpression")
    ) {
      return()
    }
    viewerPublishChanged(
      expression_projection_cells_state,
      expression_projection_cells_to_show_current()
    )
  },
  priority = 950
)
expression_projection_cells_to_show <- reactive({
  expression_projection_cells_state() %||%
    expression_projection_cells_to_show_current()
})

## The rows of the frame the group filters keep (NULL: every row), for the
## summaries of the cells shown when the browser applies the filters.
expression_projection_shown_rows <- reactive({
  filters <- expression_projection_group_filters()
  if (is.null(expression_projection_client_filters()) || !length(filters)) {
    return(NULL)
  }
  metadata <- viewerProjectionSubsetRows(
    viewerProjectionFirstFrameMetadata(),
    expression_projection_cells_to_show(),
    names(filters)
  )
  which(cerebroGroupFilterMask(metadata, filters))
})

## The browser's filters follow the controls.
observe({
  req(identical(input[["sidebar"]], "geneExpression"))
  viewerSendClientFilters(
    "expression_projection",
    expression_projection_client_filters(),
    viewer_loaded_dataset_context()
  )
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
