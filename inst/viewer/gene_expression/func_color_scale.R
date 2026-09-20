expressionColorScale <- function(name) {
  if (!identical(name, "Cerebro orange")) {
    return(name)
  }
  list(
    list(0, "#aeb5bb"),
    list(0.08, "#f7c89d"),
    list(0.38, "#f49a4c"),
    list(0.7, "#e75f25"),
    list(1, "#9f251f")
  )
}

expressionValueRange <- function(expression_levels) {
  series <- if (is.list(expression_levels)) {
    expression_levels
  } else {
    list(expression_levels)
  }
  ranges <- lapply(series, function(values) {
    if (!length(values)) return(NULL)
    value_range <- suppressWarnings(range(values, finite = TRUE))
    if (any(!is.finite(value_range))) NULL else value_range
  })
  ranges <- Filter(Negate(is.null), ranges)
  if (!length(ranges)) {
    return(c(0, 1))
  }
  low <- min(vapply(ranges, `[[`, numeric(1), 1L))
  high <- max(vapply(ranges, `[[`, numeric(1), 2L))
  if (low == 0 && high == 0) c(0, 1) else round(c(low, high), digits = 2)
}

expressionPanelColorScales <- function(genes, mode, shared_scale) {
  if (!length(genes)) {
    return(list())
  }
  if (!identical(mode, "different")) {
    return(stats::setNames(
      rep(list(expressionColorScale(shared_scale)), length(genes)),
      genes
    ))
  }
  high <- c(
    "#b2182b",
    "#2166ac",
    "#1b7837",
    "#762a83",
    "#e08214",
    "#008080",
    "#c51b7d",
    "#7f3b08",
    "#4d4d4d"
  )
  scales <- lapply(seq_along(genes), function(i) {
    colors <- grDevices::colorRampPalette(c("#d9dde0", high[[i]]))(5)
    Map(list, seq(0, 1, length.out = length(colors)), colors)
  })
  stats::setNames(scales, genes)
}

expressionReverseColorScale <- function(name) {
  !identical(name, "Cerebro orange")
}
