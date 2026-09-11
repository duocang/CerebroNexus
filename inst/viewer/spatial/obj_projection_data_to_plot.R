##----------------------------------------------------------------------------##
## Collect data required to update projection.
##----------------------------------------------------------------------------##
spatial_projection_data_to_plot_raw <- reactive({
  req(
    spatial_projection_metadata(),
    spatial_projection_coordinates(),
    spatial_projection_parameters_plot(),
    reactive_colors(),
    spatial_projection_hover_info(),
    nrow(spatial_projection_metadata()) ==
      length(spatial_projection_hover_info()) ||
      spatial_projection_hover_info() == "none"
  )
  metadata <- spatial_projection_metadata()
  plot_parameters <- spatial_projection_parameters_plot()

  ## Handle ImageFeaturePlot (add gene expression data)
  if (
    plot_parameters$plot_type == 'ImageFeaturePlot' &&
      !is.null(plot_parameters$feature_to_display)
  ) {
    gene <- plot_parameters$feature_to_display
    if (gene %in% getGeneNames()) {
      # Use cell_barcode column if available, otherwise fallback to rownames
      if ("cell_barcode" %in% colnames(metadata)) {
        cells_to_extract <- metadata$cell_barcode
      } else {
        cells_to_extract <- rownames(metadata)
      }
      # Slice only the requested gene x cells to avoid materializing the full
      # dense matrix on every call. getExpressionMatrix is a Cerebro R6 method,
      # not a bare function — reach it through data_set() like the gene-
      # expression module does.
      expression_data <- data_set()$getExpressionMatrix(
        cells = cells_to_extract,
        genes = gene
      )
      if (!is.null(expression_data) && gene %in% rownames(expression_data)) {
        expr_values <- as.vector(expression_data[gene, cells_to_extract])
        metadata[[gene]] <- expr_values
      }
    }
  }

  ## Co-expression: pull each channel's gene expression into metadata columns
  ## keyed by a stable channel name, so the renderer can blend them onto RGB.
  if (plot_parameters$plot_type == "Co-expression (RGB)") {
    if ("cell_barcode" %in% colnames(metadata)) {
      cells_to_extract <- metadata$cell_barcode
    } else {
      cells_to_extract <- rownames(metadata)
    }
    ## Use a list, not c(): an empty channel is NULL, and c() would DROP it and
    ## shift the remaining names, misaligning genes to channels.
    coexpr_genes <- list(
      coexpr_r = plot_parameters$coexpr_r,
      coexpr_g = plot_parameters$coexpr_g,
      coexpr_b = plot_parameters$coexpr_b
    )
    for (channel in names(coexpr_genes)) {
      gene <- coexpr_genes[[channel]]
      metadata[[channel]] <- NA_real_
      if (!is.null(gene) && nzchar(gene) && gene %in% getGeneNames()) {
        expression_data <- data_set()$getExpressionMatrix(
          cells = cells_to_extract,
          genes = gene
        )
        if (!is.null(expression_data) && gene %in% rownames(expression_data)) {
          metadata[[channel]] <- as.vector(
            expression_data[gene, cells_to_extract]
          )
        }
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
  selected_roi <- spatial_roi_value(plot_parameters[["roi_selection"]])
  roi_values <- if (nzchar(selected_roi)) {
    rep(selected_roi, nrow(metadata))
  } else if (
    identical(plot_parameters[["roi_mode"]], "separate") &&
      plot_parameters[["split_by"]] %in% colnames(metadata)
  ) {
    as.character(metadata[[plot_parameters[["split_by"]]]])
  } else {
    rep(NA_character_, nrow(metadata))
  }
  ## Apply rotation to the displayed (subset) coordinates.
  coordinates <- rotateSpatialCoordinatesByRoi(
    spatial_projection_coordinates(),
    roi_values,
    roi_settings,
    rotation_angle
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
      metadata_cells <- if ("cell_barcode" %in% colnames(metadata)) {
        as.character(metadata[["cell_barcode"]])
      } else {
        rownames(metadata)
      }
      boundary_roi <- if (nzchar(selected_roi)) {
        rep(selected_roi, length(cell_boundaries$x))
      } else if (
        identical(plot_parameters[["roi_mode"]], "separate") &&
          plot_parameters[["split_by"]] %in% colnames(metadata)
      ) {
        as.character(metadata[[plot_parameters[["split_by"]]]])[
          match(cell_boundaries$cell_barcode, metadata_cells)
        ]
      } else {
        rep(NA_character_, length(cell_boundaries$x))
      }
      rotated_boundaries <- rotateSpatialCoordinatesByRoi(
        data.frame(x = cell_boundaries$x, y = cell_boundaries$y),
        boundary_roi,
        roi_settings,
        rotation_angle
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

  ## Pin the axes to the FULL cell extent, not the currently displayed subset.
  ## Otherwise, changing "Show % of observations" rescales the axes to whatever subset
  ## is plotted and the plot visibly jitters. We compute the range over ALL cells
  ## (in the same rotated frame) and pass it as an explicit x/y range. A small
  ## margin keeps edge points off the frame.
  if (
    is.null(plot_parameters[["x_range"]]) ||
      length(plot_parameters[["x_range"]]) < 2 ||
      is.null(plot_parameters[["y_range"]]) ||
      length(plot_parameters[["y_range"]]) < 2
  ) {
    full_coords <- getSpatialData(plot_parameters[["projection"]])$coordinates
    full_cells <- rownames(full_coords) %||% character()
    scope <- list(
      list(
        value = input[["spatial_projection_sample"]] %||% "",
        fields = c("sample", "sample_id", "orig.ident")
      ),
      list(
        value = spatial_roi_value(plot_parameters[["roi_selection"]]),
        fields = c("sample_roi", "roi", "roi_id", "region_of_interest")
      )
    )
    for (item in scope) {
      if (nzchar(item$value)) {
        facet <- spatial_metadata_facet(getMetaData(), full_cells, item$fields)
        allowed <- names(facet$by_cell)[
          !is.na(facet$by_cell) & facet$by_cell == item$value
        ]
        full_coords <- full_coords[full_cells %in% allowed, , drop = FALSE]
        full_cells <- rownames(full_coords) %||% character()
      }
    }
    full_roi <- if (nzchar(selected_roi)) {
      rep(selected_roi, nrow(full_coords))
    } else if (identical(plot_parameters[["roi_mode"]], "separate")) {
      facet <- spatial_metadata_facet(
        getMetaData(),
        full_cells,
        c("sample_roi", "roi", "roi_id", "region_of_interest")
      )
      as.character(facet$by_cell[full_cells])
    } else {
      rep(NA_character_, nrow(full_coords))
    }
    full_coords <- rotateSpatialCoordinatesByRoi(
      full_coords,
      full_roi,
      roi_settings,
      rotation_angle
    )
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
    hover_info = spatial_projection_hover_info(),
    cell_boundaries = cell_boundaries,
    molecule_points = molecule_points
  )

  return(to_return)
})

spatial_projection_data_to_plot <- debounce(
  spatial_projection_data_to_plot_raw,
  150
)
