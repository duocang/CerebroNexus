##----------------------------------------------------------------------------##
## Indices of cells to show in projection.
##----------------------------------------------------------------------------##
spatial_projection_cell_index <- reactive({
  controls <- spatial_projection_controls()
  req(controls)
  spatial_name <- controls$projection
  req(spatial_name %in% availableSpatial())
  parameters <- spatial_projection_parameters_plot()
  req(identical(parameters[["projection"]], spatial_name))
  coordinates <- viewerSpatialCoordinates(spatial_name)
  req(coordinates)
  spatial_cells <- rownames(coordinates)
  pack <- viewerPackCurrent()
  pack_cell_count <- suppressWarnings(as.integer(pack$cell_count))
  valid_pack_count <- length(pack_cell_count) == 1L &&
    !is.na(pack_cell_count)
  index <- viewerPackSpatialIndex(pack, spatial_name)
  valid_index <- length(index) == nrow(coordinates) &&
    !anyNA(index) &&
    isTRUE(all(index >= 1L))
  if (!isTRUE(valid_index)) {
    index <- NULL
  }
  pack_order_matches <-
    is.null(index) &&
    !is.null(pack) &&
    is.list(pack) &&
    valid_pack_count &&
    length(spatial_cells) == pack_cell_count &&
    identical(
      viewerPackCellOrderFingerprint(spatial_cells),
      as.character(pack$manifest$cell_order_fingerprint)
    )
  if (isTRUE(pack_order_matches)) {
    index <- seq_len(pack_cell_count)
  }
  if (is.null(index)) {
    index <- match(spatial_cells, getMetaData()[["cell_barcode"]])
  }
  if (length(index) != nrow(coordinates) || anyNA(index)) {
    stop("Spatial cells do not match the canonical dataset order.")
  }
  as.integer(index)
})

spatial_projection_cells_to_show <- reactive({
  controls <- spatial_projection_controls()
  req(controls, controls$percentage_cells_to_show)
  groups <- getGroups()
  filters <- stats::setNames(
    lapply(groups, function(group) {
      input[[paste0("spatial_projection_group_filter_", group)]]
    }),
    groups
  )
  filters <- filters[!vapply(filters, is.null, logical(1))]
  index <- spatial_projection_cell_index()
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
