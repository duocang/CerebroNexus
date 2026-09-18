## function to be executed to update figure
expression_projection_update_plot <- function(input) {
  coordinates <- input[['coordinates']]
  reset_axes <- input[['reset_axes']]
  expression_levels <- input[['expression_levels']]
  plot_parameters <- input[['plot_parameters']]
  color_settings <- input[['color_settings']]
  metadata <- input[['metadata']]
  trajectory <- input[['trajectory']]
  display_mode <- input[['display_mode']]
  cell_indices <- input[['cell_indices']]
  separate_panels <- input[['separate_panels']]
  if (is.null(cell_indices)) {
    cell_indices <- seq_len(nrow(metadata))
  }
  appearance <- list(
    group_labels = FALSE,
    draw_border = isTRUE(plot_parameters[["draw_border"]]),
    keep_square = isTRUE(plot_parameters[["keep_square"]])
  )
  ## define output_data
  output_data <- list(
    x = coordinates[[1]],
    y = coordinates[[2]],
    color = expression_levels,
    selection_key = seq_len(nrow(metadata)),
    point_size = plot_parameters[["point_size"]],
    point_opacity = plot_parameters[["point_opacity"]],
    point_line = list(),
    x_range = plot_parameters[["x_range"]],
    y_range = plot_parameters[["y_range"]],
    paint_order = if (
      identical(plot_parameters[["plot_order"]], "Highest expression on top")
    ) {
      "highest"
    } else {
      "natural"
    },
    reset_axes = reset_axes
  )
  if (plot_parameters[["draw_border"]]) {
    output_data[['point_line']] <- list(
      color = "rgb(196,196,196)",
      width = 1
    )
  }
  output_data[["colorscale"]] <- expressionColorScale(
    color_settings[["color_scale"]]
  )
  if (
    is.list(expression_levels) &&
      !identical(display_mode, "rgb") &&
      identical(color_settings[["color_mode"]], "different")
  ) {
    output_data[["panel_colorscales"]] <- expressionPanelColorScales(
      names(expression_levels),
      color_settings[["color_mode"]],
      color_settings[["color_scale"]]
    )
  }
  output_data[["color_range"]] <- color_settings[["color_range"]]
  output_data[["reversescale"]] <- expressionReverseColorScale(
    color_settings[["color_scale"]]
  )
  ## process trajectory data
  trajectory_lines <- list()
  if (plot_parameters[['is_trajectory']]) {
    ## convert trajectory edges to the shared renderer's shape format
    trajectory_edges <- trajectory[['edges']]
    for (i in seq_len(nrow(trajectory_edges))) {
      line <- list(
        type = "line",
        line = list(color = "black", width = 1),
        xref = "x",
        yref = "y",
        x0 = trajectory_edges$source_dim_1[i],
        y0 = trajectory_edges$source_dim_2[i],
        x1 = trajectory_edges$target_dim_1[i],
        y1 = trajectory_edges$target_dim_2[i]
      )
      trajectory_lines <- c(trajectory_lines, list(line))
    }
  }
  output_hover <- list(hoverinfo = "skip", text = list(), columns = list())
  deferred_aux <- function() {
    full_metadata <- if ("cell_barcode" %in% colnames(metadata)) {
      metadata
    } else {
      getMetaData()
    }
    hover <- isTRUE(plot_parameters[["hover_info"]])
    hover_columns <- if (hover) {
      cerebroProjectionHoverColumns(full_metadata)
    } else {
      list()
    }
    if (hover && plot_parameters[['is_trajectory']]) {
      states <- as.character(trajectory[['meta']]$state)
      states[is.na(states)] <- "NA"
      state_levels <- unique(states)
      hover_columns <- c(
        hover_columns,
        list(
          list(
            label = "State",
            levels = state_levels,
            values = match(states, state_levels) - 1L
          ),
          list(
            label = "Pseudotime",
            format = "fixed",
            digits = 2L,
            values = as.numeric(trajectory[['meta']]$pseudotime)
          )
        )
      )
    }
    cerebroCellViewDeferredAux(
      selection_rows = cell_indices,
      cell_barcodes = full_metadata[["cell_barcode"]],
      hover_columns = hover_columns,
      hover = hover
    )
  }
  if (identical(display_mode, "rgb")) {
    output_data[["rgb"]] <- expression_levels[c("r", "g", "b")]
    output_data[["rgb_genes"]] <- color_settings[["rgb_genes"]]
    cerebroCellViewRender(
      "expression_projection",
      list(
        color_type = "rgb",
        color_variable = "RGB co-expression",
        space_label = plot_parameters[["projection"]],
        appearance = appearance
      ),
      output_data,
      output_hover,
      extra = list(shapes = trajectory_lines),
      deferred_aux = deferred_aux
    )
    return(invisible(NULL))
  }
  n_dimensions <- plot_parameters[["n_dimensions"]]
  multi <- n_dimensions == 2 &&
    isTRUE(separate_panels) &&
    is.list(input[["expression_levels"]])
  if (n_dimensions == 3) {
    output_data[['z']] <- coordinates[[3]]
  }
  if (n_dimensions == 3 || !is.list(input[["expression_levels"]]) || multi) {
    legend_label <- if (is.list(expression_levels)) {
      "Expression"
    } else if (length(color_settings[["genes"]]) == 1) {
      color_settings[["genes"]][[1]]
    } else {
      paste0("Mean expression (", length(color_settings[["genes"]]), " genes)")
    }
    output_data[["shared_zero_color"]] <-
      length(color_settings[["genes"]]) == 0L
    cerebroCellViewRender(
      "expression_projection",
      list(
        color_type = "continuous",
        color_variable = legend_label,
        space_label = plot_parameters[["projection"]],
        appearance = appearance
      ),
      output_data,
      output_hover,
      extra = list(shapes = trajectory_lines),
      deferred_aux = deferred_aux
    )
  }
}
