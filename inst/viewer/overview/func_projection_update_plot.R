##----------------------------------------------------------------------------##
## Function that updates projections.
##----------------------------------------------------------------------------##
overview_projection_update_plot <- function(input) {
  cells_df <- input[["cells_df"]]
  cell_indices <- input[["cell_indices"]]
  coordinates <- input[["coordinates"]]
  reset_axes <- input[["reset_axes"]]
  plot_parameters <- input[["plot_parameters"]]
  color_assignments <- input[["color_assignments"]]
  color_variable <- plot_parameters[["color_variable"]]
  n_dimensions <- plot_parameters[["n_dimensions"]]
  resource_first <- isTRUE(input[["resource_first"]])
  payload <- cerebroCellViewScatterPayload(
    coordinates = if (resource_first) list() else coordinates,
    color = if (resource_first) NULL else cells_df[[color_variable]],
    color_variable = color_variable,
    selection_keys = if (resource_first) integer() else seq_len(nrow(cells_df)),
    point_size = plot_parameters[["point_size"]],
    point_opacity = plot_parameters[["point_opacity"]],
    group_labels = plot_parameters[["group_labels"]],
    keep_square = plot_parameters[["keep_square"]],
    point_line = if (plot_parameters[["draw_border"]]) {
      list(color = "rgb(196,196,196)", width = 1)
    } else {
      list()
    },
    x_range = plot_parameters[["x_range"]],
    y_range = plot_parameters[["y_range"]],
    reset_axes = reset_axes,
    n_dimensions = n_dimensions,
    color_assignments = color_assignments,
    hover_columns = list(),
    hover = FALSE,
    space_label = plot_parameters[["projection"]],
    cell_count = input[["cell_count"]],
    coordinate_resource = if (resource_first) {
      input[["projection_resource"]]
    } else {
      NULL
    },
    categorical_resource = if (resource_first) {
      input[["categorical_resource"]]
    } else {
      NULL
    }
  )
  if (!resource_first && is.list(input[["projection_resource"]])) {
    payload$data$x <- NULL
    payload$data$y <- NULL
    payload$data$z <- NULL
    payload$data$projection_resource <- input[["projection_resource"]]
  }
  selection_rows <- if (resource_first) {
    seq_len(input[["cell_count"]])
  } else {
    payload$data$selection_key
  }
  deferred_aux <- function() {
    metadata <- getMetaData()
    groups <- getGroups()
    hover_columns <- if (isTRUE(plot_parameters[["hover_info"]])) {
      columns <- viewerProjectionMetadataColumns(
        metadata,
        color_variable,
        hover_info = TRUE,
        groups = groups
      )
      cerebroProjectionHoverColumns(
        viewerProjectionSubsetRows(metadata, cell_indices, columns)
      )
    } else {
      list()
    }
    cerebroCellViewDeferredAux(
      selection_rows = selection_rows,
      cell_barcodes = metadata[["cell_barcode"]][cell_indices],
      hover_columns = hover_columns,
      hover = plot_parameters[["hover_info"]]
    )
  }

  cerebroCellViewRender(
    "overview_projection",
    payload[["meta"]],
    payload[["data"]],
    payload[["hover"]],
    deferred_aux = deferred_aux
  )
}
