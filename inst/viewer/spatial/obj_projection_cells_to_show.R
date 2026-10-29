##----------------------------------------------------------------------------##
## Indices of cells to show in projection.
##----------------------------------------------------------------------------##
## A change in colour/point appearance must not draw a new random sample.
spatial_projection_sampling <- reactiveVal(NULL)
observe(
  {
    controls <- spatial_projection_controls()
    req(controls)
    sampling <- controls[c("projection", "percentage_cells_to_show")]
    viewerPublishChanged(spatial_projection_sampling, sampling)
  },
  priority = 950
)

spatial_projection_cell_index <- reactive({
  controls <- spatial_projection_sampling()
  req(controls)
  spatial_name <- controls$projection
  req(spatial_name %in% availableSpatial())
  coordinates <- viewerSpatialCoordinates(spatial_name)
  req(coordinates)
  spatial_section_rows(spatial_name, coordinates)
})

## The group filters that leave cells out, and, with a Viewer Pack, the same
## filters for the browser to apply to the drawn section (NULL: R filters).
spatial_projection_group_filters <- reactive({
  viewerProjectionGroupFilters(
    "spatial_projection",
    viewerProjectionFirstFrameMetadata()
  )
})
spatial_projection_client_filters <- reactive({
  pack <- viewerPackCurrent()
  if (
    !is.list(pack) ||
      !isTRUE(pack$canonical_order) ||
      isTRUE(input[["spatial_projection_client_filters_failed"]])
  ) {
    return(NULL)
  }
  viewerClientFilterDescriptors(spatial_projection_group_filters())
})

spatial_projection_cells_to_show_current <- reactive({
  controls <- spatial_projection_sampling()
  req(controls, controls$percentage_cells_to_show)
  filters <- if (is.null(spatial_projection_client_filters())) {
    spatial_projection_group_filters()
  } else {
    list()
  }
  index <- spatial_projection_cell_index()
  frame_metadata <- viewerProjectionFirstFrameMetadata()
  for (scope in list(
    list(
      value = spatial_projection_selected_sample(),
      fields = spatial_sample_fields
    ),
    list(
      value = spatial_roi_value(input[["spatial_projection_roi"]] %||% ""),
      fields = spatial_roi_fields
    )
  )) {
    selected <- scope$value %||% ""
    if (
      !is.character(selected) ||
        length(selected) != 1L ||
        is.na(selected) ||
        !nzchar(selected)
    ) {
      next
    }
    field_index <- match(
      tolower(scope$fields),
      tolower(colnames(frame_metadata)),
      nomatch = 0L
    )
    field_index <- field_index[field_index > 0L]
    if (!length(field_index)) {
      next
    }
    field <- colnames(frame_metadata)[[field_index[[1L]]]]
    values <- as.character(frame_metadata[[field]][index])
    if (!selected %in% values) {
      next
    }
    index <- index[!is.na(values) & values == selected]
  }
  if (length(filters)) {
    metadata <- viewerProjectionSubsetRows(
      viewerProjectionFirstFrameMetadata(),
      index,
      names(filters)
    )
    index <- index[cerebroGroupFilterMask(metadata, filters)]
  }
  percentage <- controls$percentage_cells_to_show
  if (length(index) && percentage < 100) {
    index <- sample(index, ceiling(length(index) * percentage / 100))
  }
  index
})

## Group filter and ROI controls bind after the first frame, usually keeping
## every cell. Publish the cells on change only, so binding does not rebuild
## the million-cell frame downstream.
spatial_projection_cells_state <- reactiveVal(NULL)
observe(
  {
    ## Off the Spatial page, keep the last cells; returning re-evaluates.
    if (
      "sidebar" %in%
        isolate(names(input)) &&
        !identical(input[["sidebar"]], "spatial")
    ) {
      return()
    }
    value <- tryCatch(
      spatial_projection_cells_to_show_current(),
      error = function(error) NULL
    )
    viewerPublishChanged(spatial_projection_cells_state, value)
  },
  priority = 900
)
spatial_projection_cells_to_show <- reactive({
  value <- spatial_projection_cells_state()
  req(!is.null(value))
  value
})

## The browser's filters follow the controls.
observe({
  req(identical(input[["sidebar"]], "spatial"))
  viewerSendClientFilters(
    "spatial_projection",
    spatial_projection_client_filters(),
    viewer_loaded_dataset_context()
  )
})
