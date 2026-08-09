##----------------------------------------------------------------------------##
## Collect data required to update projection.
##----------------------------------------------------------------------------##
build_spatial_projection_data <- function(
  metadata,
  coordinates,
  plot_parameters,
  hover_info,
  reset_axes = FALSE
) {
  metadata <- as.data.frame(metadata)

  ## The metadata, coordinates and hover payload are aligned before this helper
  ## is called. Keeping the builder explicit lets every Spatial panel reuse the
  ## same rendering path without rebinding the global input object.
  stopifnot(
    nrow(metadata) == nrow(coordinates),
    length(hover_info) == nrow(metadata) || identical(hover_info, "none")
  )

  ## Handle ImageFeaturePlot (add gene expression data)
  ##
  ## Expression retrieval remains here rather than in each panel: every panel
  ## receives the same filtered cell order, so the data-set method returns the
  ## same aligned vector for all coordinate systems.
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
    color_assignments <- NA
  } else {
    color_assignments <- assignColorsToGroups(
      metadata,
      plot_parameters[['color_variable']]
    )
  }

  ## Rotation angle (if configured for the current dataset). Applied to BOTH the
  ## displayed subset and the full-extent coordinates below so the axis range and
  ## the points stay in the same frame.
  rotate_coords <- function(co) {
    co
  }
  if (
    exists("Cerebro.options") &&
      !is.null(Cerebro.options[["spatial_plot_rotation"]]) &&
      exists("available_crb_files") &&
      !is.null(available_crb_files$selected)
  ) {
    match_idx <- which(
      available_crb_files$files == available_crb_files$selected
    )
    if (length(match_idx) > 0) {
      current_name <- names(available_crb_files$files)[match_idx[1]]
      if (
        !is.null(current_name) &&
          current_name %in% names(Cerebro.options[["spatial_plot_rotation"]])
      ) {
        rotation_angle <- Cerebro.options[["spatial_plot_rotation"]][[
          current_name
        ]]
        if (!is.null(rotation_angle) && rotation_angle != 0) {
          theta <- rotation_angle * pi / 180
          cos_theta <- cos(theta)
          sin_theta <- sin(theta)
          rotate_coords <- function(co) {
            x <- co[, 1]
            y <- co[, 2]
            co[, 1] <- x * cos_theta - y * sin_theta
            co[, 2] <- x * sin_theta + y * cos_theta
            co
          }
        }
      }
    }
  }

  ## Apply rotation to the displayed (subset) coordinates.
  coordinates <- rotate_coords(coordinates)

  ## By default each panel frames the cells that actually remain after the
  ## shared filters and "Show % of cells" setting. This is intentionally
  ## panel-local: different tissues can occupy very different coordinate spans.
  ## Turning visible fitting off restores the shared manual X/Y controls.
  axis_ranges <- spatial_projection_axis_ranges(
    coordinates,
    fit_visible = plot_parameters[["fit_visible_cells"]],
    x_range = plot_parameters[["x_range"]],
    y_range = plot_parameters[["y_range"]]
  )
  plot_parameters[["x_range"]] <- axis_ranges$x
  plot_parameters[["y_range"]] <- axis_ranges$y

  ## Legacy/manual fallback: if visible fitting is disabled but the range inputs
  ## are not ready yet, use the full cell extent rather than sending an invalid
  ## viewport. A small margin keeps edge points off the frame.
  if (
    is.null(plot_parameters[["x_range"]]) ||
      length(plot_parameters[["x_range"]]) < 2 ||
      is.null(plot_parameters[["y_range"]]) ||
      length(plot_parameters[["y_range"]]) < 2
  ) {
    full_coords <- rotate_coords(
      getSpatialData(plot_parameters[["projection"]])$coordinates
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

  ## Every valid path now supplies an explicit range: either the visible-cell
  ## fit, the user's manual values, or the full-extent fallback above. Keep that
  ## range instead of asking Plotly to autorange a second time.
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
    hover_info = hover_info
  )

  to_return
}

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
  build_spatial_projection_data(
    metadata = spatial_projection_metadata(),
    coordinates = spatial_projection_coordinates(),
    plot_parameters = spatial_projection_parameters_plot(),
    hover_info = spatial_projection_hover_info(),
    reset_axes = isolate(
      spatial_projection_parameters_other[['reset_axes']]
    )
  )
})

spatial_projection_data_to_plot <- debounce(
  spatial_projection_data_to_plot_raw,
  150
)
