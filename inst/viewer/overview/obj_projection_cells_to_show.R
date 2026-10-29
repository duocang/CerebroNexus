##----------------------------------------------------------------------------##
## Indices of cells to show in projection.
##----------------------------------------------------------------------------##
## With a Viewer Pack the browser fetches the full canonical projection from a
## static resource and hides a deterministic share of cells itself, so the "%
## of cells" slider no longer resamples and resends coordinates from R.
overview_projection_client_percentage <- reactive({
  pack <- viewerPackCurrent()
  is.list(pack) && isTRUE(pack$canonical_order)
})

## The group filters that leave cells out, and, with a Viewer Pack, the same
## filters for the browser to apply to a frame of every cell (NULL: R filters).
overview_projection_group_filters <- reactive({
  viewerProjectionGroupFilters(
    "overview_projection",
    viewerProjectionFirstFrameMetadata()
  )
})
overview_projection_client_filters <- reactive({
  if (
    !isTRUE(overview_projection_client_percentage()) ||
      isTRUE(input[["overview_projection_client_filters_failed"]])
  ) {
    return(NULL)
  }
  viewerClientFilterDescriptors(overview_projection_group_filters())
})

overview_projection_cells_to_show_current <- reactive({
  client <- isTRUE(overview_projection_client_percentage())
  percentage <- if (client) 100 else current_scatter_percentage()
  filters <- if (is.null(overview_projection_client_filters())) {
    overview_projection_group_filters()
  } else {
    list()
  }
  viewerProjectionCellIndices(
    "overview_projection",
    metadata = viewerProjectionFirstFrameMetadata(),
    percentage = percentage,
    filters = filters
  )
})

## Published on change only: a filter the browser applies leaves every cell in
## the frame, and must not rebuild it.
overview_projection_cells_state <- reactiveVal(NULL)
observe(
  {
    ## Off the page, keep the last cells; returning re-evaluates.
    if (
      "sidebar" %in%
        isolate(names(input)) &&
        !identical(input[["sidebar"]], "overview")
    ) {
      return()
    }
    viewerPublishChanged(
      overview_projection_cells_state,
      overview_projection_cells_to_show_current()
    )
  },
  priority = 950
)
overview_projection_cells_to_show <- reactive({
  overview_projection_cells_state() %||%
    overview_projection_cells_to_show_current()
})

## The browser's filters follow the controls.
observe({
  req(identical(input[["sidebar"]], "overview"))
  viewerSendClientFilters(
    "overview_projection",
    overview_projection_client_filters(),
    viewer_loaded_dataset_context()
  )
})
