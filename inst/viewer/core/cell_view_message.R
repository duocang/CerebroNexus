## Normalize shared Canvas messages before JSON or binary transport.
##
## This module is deliberately session-free so array wrapping and copy
## behaviour can be tested independently from Shiny observers.

cerebroCellViewMessage <- function(
  id,
  meta,
  data,
  hover = list(),
  extra = list()
) {
  wire_array <- function(value) {
    if (is.null(value)) NULL else I(unname(value))
  }
  wire_nested <- function(value) {
    if (is.null(value)) {
      return(NULL)
    }
    if (!is.list(value)) {
      return(wire_array(value))
    }
    lapply(value, wire_array)
  }

  for (field in intersect(
    c("traces", "group_colors", "axes", "coexpr_colors"),
    names(meta)
  )) {
    meta[[field]] <- wire_array(meta[[field]])
  }

  nested <- identical(meta$color_type, "categorical") && is.null(data$panels)
  for (field in intersect(
    c("x", "y", "z", "selection_key", "color", "group", "point_sizes"),
    names(data)
  )) {
    value <- data[[field]]
    data[[field]] <- if (nested || is.list(value)) {
      wire_nested(value)
    } else {
      wire_array(value)
    }
  }
  for (field in intersect(
    c("x_range", "y_range", "color_range"),
    names(data)
  )) {
    data[[field]] <- wire_array(data[[field]])
  }
  if (is.list(data$rgb)) {
    data$rgb <- lapply(data$rgb, wire_array)
  }
  if (is.list(data$panels)) {
    data$panels <- lapply(data$panels, function(panel) {
      for (field in intersect(
        c(
          "selection_key",
          "x",
          "y",
          "z",
          "hover",
          "from_x",
          "from_y",
          "to_x",
          "to_y"
        ),
        names(panel)
      )) {
        panel[[field]] <- wire_array(panel[[field]])
      }
      panel
    })
  }
  if (!is.null(hover$text)) {
    hover$text <- if (nested || is.list(hover$text)) {
      wire_nested(hover$text)
    } else {
      wire_array(hover$text)
    }
  }
  if (is.list(hover$columns)) {
    hover$columns <- lapply(hover$columns, function(column) {
      column$values <- wire_nested(column$values)
      if (!is.null(column$levels)) {
        column$levels <- wire_array(column$levels)
      }
      column
    })
  }
  if (is.list(extra$group_hulls)) {
    for (field in intersect(c("x", "y"), names(extra$group_hulls))) {
      extra$group_hulls[[field]] <- wire_nested(extra$group_hulls[[field]])
    }
  }
  if (is.list(extra$edges)) {
    for (field in intersect(c("x0", "y0", "x1", "y1"), names(extra$edges))) {
      extra$edges[[field]] <- wire_array(extra$edges[[field]])
    }
  }

  list(id = id, meta = meta, data = data, hover = hover, extra = extra)
}
