##----------------------------------------------------------------------------##
## Indices of cells to show in projection.
##----------------------------------------------------------------------------##
spatial_projection_cells_to_show <- reactive({
  req(input[["spatial_projection_percentage_cells_to_show"]])
  meta_data <- getMetaData()
  req(!is.null(meta_data))
  spatial_name <- input[["spatial_projection_to_display"]]
  req(is.character(spatial_name), length(spatial_name) == 1L)
  spatial_data <- getSpatialData(spatial_name)
  scene_cells <- rownames(spatial_data[["coordinates"]]) %||% character()
  metadata_cells <- if ("cell_barcode" %in% colnames(meta_data)) {
    as.character(meta_data[["cell_barcode"]])
  } else {
    rownames(meta_data)
  }
  keep <- metadata_cells %in% scene_cells

  hierarchy <- list(
    sample = c("sample", "sample_id", "orig.ident"),
    roi = c("sample_roi", "roi", "roi_id", "region_of_interest")
  )
  for (level in names(hierarchy)) {
    selected <- input[[paste0("spatial_projection_", level)]] %||% ""
    selected_is_active <- if (identical(level, "roi")) {
      spatial_roi_is_specific(selected)
    } else {
      is.character(selected) && length(selected) == 1L && nzchar(selected)
    }
    if (selected_is_active) {
      facet <- spatial_metadata_facet(
        meta_data,
        scene_cells,
        hierarchy[[level]]
      )
      allowed <- names(facet$by_cell)[
        !is.na(facet$by_cell) & facet$by_cell == selected
      ]
      keep <- keep & metadata_cells %in% allowed
    }
  }
  viewerProjectionCellIndices(
    "spatial_projection",
    meta_data,
    include = keep,
    groups = getGroups(),
    input_values = input
  )
})
