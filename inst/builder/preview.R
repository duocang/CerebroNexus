##----------------------------------------------------------------------------##
## Show the projection before it is written.
##
## Cheap insurance: an object can pass every structural check and still be the
## wrong one, or be coloured by a variable that turns out to be a batch label.
## One look answers that before a file exists.
##
## Pure: object in, plotly object out. No Shiny.
##----------------------------------------------------------------------------##

## Above this many cells the plot stops being informative and starts being slow.
BUILDER_PREVIEW_MAX <- 12000L

#' The points to draw: three short columns, whatever the object's size.
#'
#' This runs in the worker process, so what crosses back to the page is a
#' data.frame of at most `max_cells` rows rather than anything holding a
#' reference to the object.
#'
#' @return A data.frame with x, y, group -- or `NULL` when there is nothing
#'   to draw.
builder_preview_frame <- function(
  object,
  reduction,
  group = NULL,
  max_cells = BUILDER_PREVIEW_MAX
) {
  if (is.null(reduction) || !nzchar(reduction)) {
    return(NULL)
  }
  if (!reduction %in% names(object@reductions)) {
    return(NULL)
  }
  emb <- SeuratObject::Embeddings(object[[reduction]])
  if (is.null(emb) || ncol(emb) < 2) {
    return(NULL)
  }

  df <- data.frame(
    x = as.numeric(emb[, 1]),
    y = as.numeric(emb[, 2]),
    stringsAsFactors = FALSE
  )
  if (!is.null(group) && group %in% colnames(object@meta.data)) {
    df$group <- as.character(object@meta.data[[group]])
    df$group[is.na(df$group)] <- "N/A"
  } else {
    df$group <- "cells"
  }

  ## Deterministic downsampling: the same object always previews the same way,
  ## so a difference on screen means a difference in the data.
  if (nrow(df) > max_cells) {
    set.seed(42L)
    df <- df[sort(sample.int(nrow(df), max_cells)), , drop = FALSE]
  }
  attr(df, "reduction") <- reduction
  df
}

#' Draw the frame the worker sent back.
builder_preview_plot <- function(df, colors = NULL) {
  if (is.null(df) || !nrow(df)) {
    return(NULL)
  }
  reduction <- attr(df, "reduction") %||% "dim"

  levels_present <- sort(unique(df$group))
  palette <- builder_preview_palette(length(levels_present))
  names(palette) <- levels_present
  if ("N/A" %in% levels_present) {
    palette[["N/A"]] <- "#898989"
  }
  ## What the user chose wins, for the levels it covers. The preview exists to
  ## answer "what will this look like", so it has to show the real palette --
  ## a preview in different colours from the app is worse than none.
  if (length(colors)) {
    shared <- intersect(levels_present, names(colors))
    if (length(shared)) {
      palette[shared] <- as.character(colors[shared])
    }
  }

  plotly::plot_ly(
    data = df,
    x = ~x,
    y = ~y,
    color = ~group,
    colors = palette,
    type = "scattergl",
    mode = "markers",
    marker = list(size = 4, opacity = 0.75, line = list(width = 0)),
    hoverinfo = "text",
    text = ~group
  ) |>
    plotly::layout(
      xaxis = list(
        title = paste0(reduction, "_1"),
        zeroline = FALSE,
        showgrid = TRUE,
        gridcolor = "#eef1ee"
      ),
      yaxis = list(
        title = paste0(reduction, "_2"),
        zeroline = FALSE,
        showgrid = TRUE,
        gridcolor = "#eef1ee"
      ),
      legend = list(orientation = "h", y = -0.16),
      margin = list(l = 50, r = 10, t = 10, b = 50),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    ) |>
    plotly::config(displayModeBar = FALSE)
}

#' The viewer's own qualitative palette, so the preview is not a different
#' picture from the one the data set will produce.
builder_preview_palette <- function(n) {
  base <- c(
    "#FFC312",
    "#C4E538",
    "#12CBC4",
    "#FDA7DF",
    "#ED4C67",
    "#F79F1F",
    "#A3CB38",
    "#1289A7",
    "#D980FA",
    "#B53471",
    "#EE5A24",
    "#009432",
    "#0652DD",
    "#9980FA",
    "#833471",
    "#EA2027",
    "#006266",
    "#1B1464",
    "#5758BB",
    "#6F1E51",
    "#40407a",
    "#706fd3",
    "#f7f1e3",
    "#34ace0",
    "#33d9b2",
    "#2c2c54",
    "#474787",
    "#aaa69d",
    "#227093",
    "#218c74",
    "#ff5252",
    "#ff793f",
    "#d1ccc0",
    "#ffb142",
    "#ffda79",
    "#b33939",
    "#cd6133",
    "#84817a",
    "#cc8e35",
    "#ccae62"
  )
  n <- max(1L, as.integer(n))
  if (n <= length(base)) {
    return(base[seq_len(n)])
  }
  grDevices::colorRampPalette(base)(n)
}

#' The alignment view: cells drawn over the background image, in the same
#' coordinate space the viewer will use.
#'
#' Numbers cannot answer "is this aligned"; only the picture can. Plotly places
#' the image as a layout layer under the points, so what shows here is what the
#' Spatial tab will show.
builder_overlay_plot <- function(coords, uri = NULL, bounds = NULL) {
  if (is.null(coords)) {
    return(NULL)
  }
  df <- data.frame(x = coords$sx, y = coords$sy)
  p <- plotly::plot_ly(
    data = df,
    x = ~x,
    y = ~y,
    type = "scattergl",
    mode = "markers",
    marker = list(
      size = 5,
      color = "#0d6a63",
      opacity = 0.85,
      line = list(width = 0.5, color = "#ffffff")
    ),
    hoverinfo = "skip"
  )
  images <- list()
  if (!is.null(uri) && !is.null(bounds)) {
    images <- list(list(
      source = uri,
      xref = "x",
      yref = "y",
      x = bounds$xmin,
      y = bounds$ymax,
      sizex = bounds$xmax - bounds$xmin,
      sizey = bounds$ymax - bounds$ymin,
      sizing = "stretch",
      opacity = 1,
      layer = "below"
    ))
  }
  plotly::layout(
    p,
    images = images,
    xaxis = list(title = "", zeroline = FALSE, showgrid = FALSE),
    yaxis = list(
      title = "",
      zeroline = FALSE,
      showgrid = FALSE,
      scaleanchor = "x"
    ),
    margin = list(l = 30, r = 10, t = 10, b = 30),
    showlegend = FALSE,
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "#fafbfa"
  ) |>
    plotly::config(displayModeBar = FALSE)
}

## ---------------------------------------------------------------------------
## Palettes
## ---------------------------------------------------------------------------

#' The palettes on offer, as functions of the number of levels.
#'
#' `cerebro` is the viewer's own, so "leave it alone" is the default and the
#' preview matches what the app will draw. The rest exist because a lab usually
#' has a convention to match, and because the viewer's palette is tuned for
#' distinctness rather than for being readable by someone with a colour vision
#' deficiency.
builder_palettes <- function() {
  list(
    list(
      id = "cerebro",
      label = "Cerebro default",
      note = "The viewer's built-in palette",
      colors = function(n) builder_preview_palette(n)
    ),
    list(
      id = "okabe_ito",
      label = "Okabe–Ito (colour-vision friendly)",
      note = "Eight colours designed for red–green colour-vision deficiency; interpolated beyond eight groups",
      colors = function(n) {
        base <- c(
          "#E69F00",
          "#56B4E9",
          "#009E73",
          "#F0E442",
          "#0072B2",
          "#D55E00",
          "#CC79A7",
          "#000000"
        )
        builder_ramp(base, n)
      }
    ),
    list(
      id = "tol_bright",
      label = "Paul Tol Bright (colour-vision friendly)",
      note = "Seven colours that remain distinct in print and projection",
      colors = function(n) {
        base <- c(
          "#4477AA",
          "#EE6677",
          "#228833",
          "#CCBB44",
          "#66CCEE",
          "#AA3377",
          "#BBBBBB"
        )
        builder_ramp(base, n)
      }
    ),
    list(
      id = "grey",
      label = "Greyscale",
      note = "Emphasises density without a categorical colour rhythm",
      colors = function(n) {
        grDevices::colorRampPalette(c("#1c1c1e", "#c8c8cc"))(max(1L, n))
      }
    )
  )
}

#' Take the first n of a base palette, interpolating only when it runs out.
#'
#' Interpolating a colour-blind-safe palette does not stay colour-blind-safe, so
#' the interface says how many colours each one really has.
builder_ramp <- function(base, n) {
  n <- max(1L, as.integer(n))
  if (n <= length(base)) {
    return(base[seq_len(n)])
  }
  grDevices::colorRampPalette(base)(n)
}

#' A level -> hex vector for one grouping variable.
#'
#' `overrides` are the swatches the user has actually touched; everything else
#' comes from the chosen palette, so switching palette does not throw away a
#' hand-picked colour.
builder_level_colors <- function(
  levels,
  palette_id = "cerebro",
  overrides = NULL
) {
  if (!length(levels)) {
    return(character())
  }
  pal <- Filter(function(p) identical(p$id, palette_id), builder_palettes())
  fn <- if (length(pal)) pal[[1]]$colors else builder_palettes()[[1]]$colors
  out <- fn(length(levels))
  names(out) <- levels
  ## The viewer forces N/A to grey; matching it keeps the preview honest.
  if ("N/A" %in% levels) {
    out[["N/A"]] <- "#898989"
  }
  if (length(overrides)) {
    shared <- intersect(levels, names(overrides))
    if (length(shared)) {
      out[shared] <- as.character(overrides[shared])
    }
  }
  out
}
