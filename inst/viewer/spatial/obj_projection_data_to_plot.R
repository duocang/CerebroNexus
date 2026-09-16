##----------------------------------------------------------------------------##
## Collect data required to update projection.
##----------------------------------------------------------------------------##
spatial_projection_full_ranges <- reactive({
  spatial_name <- input[["spatial_projection_to_display"]]
  req(spatial_name %in% availableSpatial())

  dataset <- spatial_dataset_name(
    available_crb_files$files,
    available_crb_files$selected
  )
  rotation_angle <- spatialPlotRotation(
    Cerebro.options,
    dataset,
    spatial_name
  )
  full_coords <- rotateSpatialCoordinates(
    getSpatialData(spatial_name)$coordinates,
    rotation_angle
  )
  x_full <- range(full_coords[[1]], na.rm = TRUE)
  y_full <- range(full_coords[[2]], na.rm = TRUE)
  if (!all(is.finite(c(x_full, y_full)))) {
    return(list(x_range = NULL, y_range = NULL))
  }

  list(
    x_range = x_full + c(-1, 1) * diff(x_full) * 0.02,
    y_range = y_full + c(-1, 1) * diff(y_full) * 0.02
  )
})

spatial_projection_data_to_plot_raw <- reactive({
  req(
    spatial_projection_metadata(),
    spatial_projection_coordinates(),
    spatial_projection_parameters_plot(),
    reactive_colors()
  )
  metadata <- spatial_projection_metadata()
  plot_parameters <- spatial_projection_parameters_plot()
  cells_to_extract <- spatial_projection_cells_to_show()

  ## Handle ImageFeaturePlot (add gene expression data)
  if (
    plot_parameters$plot_type == 'ImageFeaturePlot' &&
      !is.null(plot_parameters$feature_to_display)
  ) {
    gene <- plot_parameters$feature_to_display
    if (gene %in% getGeneNames()) {
      expr_values <- viewerExpressionRow(
        data_set(),
        cells_to_extract,
        gene
      )
      if (!is.null(expr_values)) {
        metadata[[gene]] <- expr_values
      }
    }
  }

  ## Co-expression: pull each channel's gene expression into metadata columns
  ## keyed by a stable channel name, so the renderer can blend them onto RGB.
  if (plot_parameters$plot_type == "Co-expression (RGB)") {
    ## Use a list, not c(): an empty channel is NULL, and c() would DROP it and
    ## shift the remaining names, misaligning genes to channels.
    coexpr_genes <- list(
      coexpr_r = plot_parameters$coexpr_r,
      coexpr_g = plot_parameters$coexpr_g,
      coexpr_b = plot_parameters$coexpr_b
    )
    requested_genes <- unique(unlist(coexpr_genes, use.names = FALSE))
    requested_genes <- requested_genes[
      !is.na(requested_genes) &
        nzchar(requested_genes) &
        requested_genes %in% getGeneNames()
    ]
    expression_values <- viewerExpressionValues(
      data_set(),
      cells_to_extract,
      requested_genes
    )
    for (channel in names(coexpr_genes)) {
      gene <- coexpr_genes[[channel]]
      metadata[[channel]] <- NA_real_
      if (!is.null(gene) && gene %in% names(expression_values)) {
        metadata[[channel]] <- expression_values[[gene]]
      }
    }
  }

  ## get colors for groups (if applicable)
  if (
    plot_parameters[['color_variable']] %in%
      colnames(metadata) &&
      is.numeric(metadata[[plot_parameters[['color_variable']]]])
  ) {
    color_assignments <- NULL
  } else {
    color_assignments <- assignColorsToGroups(
      metadata,
      plot_parameters[['color_variable']]
    )
  }

  ## Plot rotation belongs to one exact dataset + spatial entry. Image rotation
  ## is resolved independently from spatial_image_settings.
  current_name <- viewerDatasetName(
    available_crb_files$files,
    available_crb_files$selected
  )
  rotation_angle <- spatialPlotRotation(
    Cerebro.options,
    current_name,
    plot_parameters[["projection"]]
  )
  roi_settings <- spatialRoiSettings(
    Cerebro.options,
    current_name,
    plot_parameters[["projection"]]
  )
  full_coordinate_frame <- getSpatialData(
    plot_parameters[["projection"]]
  )$coordinates
  roi_context <- spatial_roi_transform_context(
    full_coordinate_frame,
    getMetaData(),
    input[["spatial_projection_sample"]] %||% "",
    rotation_angle
  )
  roi_pivots <- roi_context$pivots
  selected_roi <- spatial_roi_value(plot_parameters[["roi_selection"]])
  displayed_coordinates <- spatial_projection_coordinates()
  displayed_cells <- rownames(displayed_coordinates) %||% character()
  roi_values <- if (nzchar(selected_roi)) {
    rep(selected_roi, nrow(metadata))
  } else if (identical(plot_parameters[["roi_mode"]], "separate")) {
    as.character(roi_context$roi_by_cell[displayed_cells])
  } else {
    rep(NA_character_, nrow(metadata))
  }
  ## Apply rotation to the displayed (subset) coordinates.
  coordinates <- rotateSpatialCoordinatesByRoi(
    displayed_coordinates,
    roi_values,
    roi_settings,
    rotation_angle,
    pivots = roi_pivots
  )
  cell_boundaries <- list()
  if (isTRUE(plot_parameters[["show_cell_boundaries"]])) {
    spatial_data <- getSpatialData(plot_parameters[["projection"]])
    cells <- if ("cell_barcode" %in% colnames(metadata)) {
      as.character(metadata[["cell_barcode"]])
    } else {
      rownames(metadata)
    }
    cell_boundaries <- spatial_cell_boundaries(
      spatial_data[["boundaries"]],
      cells
    )
    if (length(cell_boundaries)) {
      boundary_roi <- if (nzchar(selected_roi)) {
        rep(selected_roi, length(cell_boundaries$x))
      } else if (identical(plot_parameters[["roi_mode"]], "separate")) {
        as.character(roi_context$roi_by_cell[cell_boundaries$cell_barcode])
      } else {
        rep(NA_character_, length(cell_boundaries$x))
      }
      rotated_boundaries <- rotateSpatialCoordinatesByRoi(
        data.frame(x = cell_boundaries$x, y = cell_boundaries$y),
        boundary_roi,
        roi_settings,
        rotation_angle,
        pivots = roi_pivots
      )
      cell_boundaries$x <- rotated_boundaries[[1L]]
      cell_boundaries$y <- rotated_boundaries[[2L]]
    }
  }
  molecule_points <- list()
  if (
    isTRUE(plot_parameters[["show_molecules"]]) &&
      !nzchar(plot_parameters[["split_by"]] %||% "")
  ) {
    spatial_data <- getSpatialData(plot_parameters[["projection"]])
    molecule_points <- spatial_molecule_overlay(
      spatial_data[["molecules"]],
      plot_parameters[["molecule_gene"]],
      scoped = nzchar(input[["spatial_projection_sample"]] %||% "") ||
        spatial_roi_is_specific(
          input[["spatial_projection_roi"]] %||% ""
        )
    )
    if (length(molecule_points)) {
      rotated_molecules <- rotateSpatialCoordinates(
        data.frame(x = molecule_points$x, y = molecule_points$y),
        rotation_angle
      )
      molecule_points$x <- rotated_molecules[[1L]]
      molecule_points$y <- rotated_molecules[[2L]]
    }
  }
  group_hulls <- if (
    isTRUE(plot_parameters[["show_region_outlines"]]) &&
      identical(plot_parameters[["plot_type"]], "ImageDimPlot") &&
      !is.numeric(metadata[[plot_parameters[["color_variable"]]]]) &&
      ncol(coordinates) == 2L
  ) {
    compute_group_hulls(
      coordinates[[1L]],
      coordinates[[2L]],
      as.character(metadata[[plot_parameters[["color_variable"]]]])
    )
  } else {
    list()
  }

  ## Pin the axes to the FULL cell extent, not the currently displayed subset.
  ## Otherwise, changing "Show % of observations" rescales the axes to whatever subset
  ## is plotted and the plot visibly jitters. We compute the range over ALL cells
  ## (in the same rotated frame) and pass it as an explicit x/y range. A small
  ## margin keeps edge points off the frame.
  full_cells <- roi_context$scoped_cells
  if (nzchar(selected_roi)) {
    full_cells <- full_cells[
      !is.na(roi_context$roi_by_cell[full_cells]) &
        roi_context$roi_by_cell[full_cells] == selected_roi
    ]
  }
  full_coords <- full_coordinate_frame[full_cells, , drop = FALSE]
  full_roi <- if (
    nzchar(selected_roi) ||
      identical(plot_parameters[["roi_mode"]], "separate")
  ) {
    as.character(roi_context$roi_by_cell[full_cells])
  } else {
    rep(NA_character_, nrow(full_coords))
  }
  full_coords <- rotateSpatialCoordinatesByRoi(
    full_coords,
    full_roi,
    roi_settings,
    rotation_angle,
    pivots = roi_pivots
  )
  if (identical(plot_parameters[["roi_mode"]], "separate")) {
    plot_parameters[["roi_extents"]] <- spatial_roi_extents(
      full_coords,
      full_roi
    )
  }
  if (
    is.null(plot_parameters[["x_range"]]) ||
      length(plot_parameters[["x_range"]]) < 2 ||
      is.null(plot_parameters[["y_range"]]) ||
      length(plot_parameters[["y_range"]]) < 2
  ) {
    x_full <- range(full_coords[[1]], na.rm = TRUE)
    y_full <- range(full_coords[[2]], na.rm = TRUE)
    x_margin <- diff(x_full) * 0.02
    y_margin <- diff(y_full) * 0.02
    if (all(is.finite(x_full)) && all(is.finite(y_full))) {
      plot_parameters[["x_range"]] <- c(
        x_full[1] - x_margin,
        x_full[2] + x_margin
      )
      plot_parameters[["y_range"]] <- c(
        y_full[1] - y_margin,
        y_full[2] + y_margin
      )
    }
  }

  ## With an explicit full-extent range we must NOT let the JS autorange (which
  ## would refit to the subset). reset_axes is meant to snap back to the full
  ## view on a dataset switch — that is exactly the full-extent range we set, so
  ## keep the fixed range.
  reset_axes <- isolate(spatial_projection_parameters_other[['reset_axes']])
  if (
    length(plot_parameters[["x_range"]]) >= 2 &&
      length(plot_parameters[["y_range"]]) >= 2
  ) {
    reset_axes <- FALSE
  }

  ## return collect data
  to_return <- list(
    cells_df = metadata,
    coordinates = coordinates,
    reset_axes = reset_axes,
    plot_parameters = plot_parameters,
    color_assignments = color_assignments,
    group_hulls = group_hulls,
    hover_columns = if (isTRUE(plot_parameters[["hover_info"]])) {
      cerebroProjectionHoverColumns(metadata)
    } else {
      list()
    },
    hover_info = spatial_projection_hover_info(),
    cell_boundaries = cell_boundaries,
    molecule_points = molecule_points
  )

  return(to_return)
})

spatial_projection_data_to_plot <- debounceAfterFirst(
  spatial_projection_data_to_plot_raw,
  150
)
