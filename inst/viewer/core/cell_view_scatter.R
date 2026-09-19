## Build shared Canvas scatter payloads independently from page observers.

cerebroCellViewScatterPayload <- function(
  coordinates,
  color,
  color_variable,
  selection_keys,
  point_size,
  point_opacity,
  group_labels = TRUE,
  keep_square = FALSE,
  color_assignments = NULL,
  hover_info = NULL,
  hover_columns = NULL,
  hover = TRUE,
  point_line = list(),
  x_range = list(),
  y_range = list(),
  reset_axes = FALSE,
  n_dimensions = 2L,
  space_label = NULL
) {
  dimensions <- if (as.integer(n_dimensions) == 3L) 3L else 2L
  if (length(coordinates) < dimensions) {
    stop("coordinates do not contain the requested dimensions")
  }
  cell_counts <- c(
    vapply(coordinates[seq_len(dimensions)], length, integer(1)),
    color = length(color),
    selection_keys = length(selection_keys)
  )
  if (length(unique(cell_counts)) != 1L) {
    stop(
      "coordinates, color, and selection_keys must describe the same number of cells"
    )
  }

  continuous <- is.numeric(color)
  has_z <- as.integer(n_dimensions) == 3L && length(coordinates) >= 3L
  meta <- list(
    color_type = if (continuous) "continuous" else "categorical",
    color_variable = color_variable,
    appearance = list(
      group_labels = isTRUE(group_labels),
      draw_border = isTRUE(
        suppressWarnings(as.numeric(point_line[["width"]])) > 0
      ),
      keep_square = isTRUE(keep_square)
    )
  )
  if (!is.null(space_label)) {
    meta[["space_label"]] <- space_label
  }
  data <- list(
    x = if (continuous) I(coordinates[[1L]]) else list(),
    y = if (continuous) I(coordinates[[2L]]) else list(),
    selection_key = if (continuous) I(selection_keys) else list(),
    color = if (continuous) I(color) else list(),
    point_size = point_size,
    point_opacity = point_opacity,
    point_line = point_line,
    x_range = x_range,
    y_range = y_range,
    reset_axes = reset_axes
  )
  if (has_z) {
    data[["z"]] <- if (continuous) I(coordinates[[3L]]) else list()
  }

  show_hover <- isTRUE(hover)
  structured_hover <- if (show_hover && length(hover_columns)) {
    lapply(hover_columns, function(column) {
      if (!is.list(column) || is.null(column$label) || is.null(column$values)) {
        stop("hover columns require label and values")
      }
      if (length(column$values) != cell_counts[[1L]]) {
        stop("hover columns must describe the same number of cells")
      }
      column
    })
  } else {
    list()
  }
  hover_data <- list(
    hoverinfo = if (show_hover) "text" else "skip",
    text = if (continuous && show_hover && !length(structured_hover)) {
      I(unname(hover_info))
    } else {
      list()
    }
  )
  if (length(structured_hover)) {
    hover_data$columns <- lapply(structured_hover, function(column) {
      column$values <- if (continuous) I(unname(column$values)) else list()
      if (!is.null(column$levels)) {
        column$levels <- I(unname(column$levels))
      }
      column
    })
  }
  if (continuous) {
    return(list(meta = meta, data = data, hover = hover_data))
  }
  if (is.null(color_assignments)) {
    stop("color_assignments are required for categorical cell views")
  }
  color <- as.character(color)
  color[is.na(color)] <- "(missing)"
  levels_in_view <- unique(color)
  if (
    !("(missing)" %in% names(color_assignments)) &&
      "(missing)" %in% levels_in_view
  ) {
    color_assignments <- c(color_assignments, `(missing)` = "#7b8794")
  }
  missing_levels <- setdiff(levels_in_view, names(color_assignments))
  if (length(missing_levels)) {
    stop(
      "color_assignments are missing categorical levels: ",
      paste(missing_levels, collapse = ", ")
    )
  }

  canonical_layout <- length(color) >= 4096L &&
    identical(as.integer(selection_keys), seq_along(selection_keys)) &&
    !show_hover &&
    !length(structured_hover)
  if (canonical_layout) {
    traces <- names(color_assignments)[
      names(color_assignments) %in% levels_in_view
    ]
    meta[["traces"]] <- as.list(traces)
    data[["x"]] <- I(as.numeric(coordinates[[1L]]))
    data[["y"]] <- I(as.numeric(coordinates[[2L]]))
    if (has_z) {
      data[["z"]] <- I(as.numeric(coordinates[[3L]]))
    }
    data[["selection_key"]] <- I(selection_keys)
    data[["color"]] <- as.list(unname(color_assignments[traces]))
    data[["canonical_group"]] <- I(as.integer(match(color, traces) - 1L))
    return(list(meta = meta, data = data, hover = hover_data))
  }

  meta[["traces"]] <- list()
  cells_by_group <- split(seq_along(color), color)
  hover_names <- names(hover_info)
  aligned_hover <- if (!show_hover) {
    NULL
  } else if (
    !is.null(hover_names) &&
      any(!is.na(hover_names) & nzchar(hover_names))
  ) {
    unname(hover_info[match(selection_keys, hover_names)])
  } else {
    unname(hover_info)
  }
  index <- 1L
  for (group in names(color_assignments)) {
    cells <- cells_by_group[[group]]
    if (is.null(cells)) {
      next
    }
    meta[["traces"]][[index]] <- group
    data[["x"]][[index]] <- I(coordinates[[1L]][cells])
    data[["y"]][[index]] <- I(coordinates[[2L]][cells])
    if (has_z) {
      data[["z"]][[index]] <- I(coordinates[[3L]][cells])
    }
    data[["selection_key"]][[index]] <- I(selection_keys[cells])
    data[["color"]][[index]] <- unname(color_assignments[[group]])
    if (show_hover && !length(structured_hover)) {
      hover_data[["text"]][[index]] <- I(aligned_hover[cells])
    }
    for (column_index in seq_along(structured_hover)) {
      hover_data$columns[[column_index]]$values[[index]] <- I(
        unname(structured_hover[[column_index]]$values[cells])
      )
    }
    index <- index + 1L
  }

  if (
    length(color) >= 4096L &&
      identical(as.integer(selection_keys), seq_along(selection_keys))
  ) {
    data[["canonical_group"]] <- I(as.integer(
      match(color, unlist(meta[["traces"]], use.names = FALSE)) - 1L
    ))
  }

  list(meta = meta, data = data, hover = hover_data)
}
