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

.builder_alignment_sample <- function(rows, max_cells) {
  max_cells <- suppressWarnings(as.integer(max_cells %||% BUILDER_PREVIEW_MAX))
  if (is.na(max_cells) || max_cells < 1L) {
    max_cells <- BUILDER_PREVIEW_MAX
  }
  if (rows <= max_cells) {
    return(seq_len(rows))
  }
  set.seed(42L)
  sort(sample.int(rows, max_cells))
}

.builder_alignment_bounds <- function(frame) {
  if (is.null(frame) || !nrow(frame)) {
    return(NULL)
  }
  xr <- range(frame$x, finite = TRUE)
  yr <- range(frame$y, finite = TRUE)
  if (length(xr) != 2L || length(yr) != 2L || !all(is.finite(c(xr, yr)))) {
    return(NULL)
  }
  list(xmin = xr[[1L]], xmax = xr[[2L]], ymin = yr[[1L]], ymax = yr[[2L]])
}

builder_alignment_coordinate_frame <- function(bounds, transform = NULL) {
  if (!.builder_alignment_valid_bounds(bounds)) {
    return(NULL)
  }
  frame <- data.frame(
    x = c(bounds$xmin, bounds$xmax, bounds$xmax, bounds$xmin, bounds$xmin),
    y = c(bounds$ymin, bounds$ymin, bounds$ymax, bounds$ymax, bounds$ymin)
  )
  if (is.null(transform)) {
    return(frame)
  }
  angle <- as.numeric(transform$rotation_degrees %||% 0) * pi / 180
  scale <- as.numeric(transform$scale %||% 1)
  pivot <- transform$pivot %||%
    c(
      x = (bounds$xmin + bounds$xmax) / 2,
      y = (bounds$ymin + bounds$ymax) / 2
    )
  centered_x <- frame$x - pivot[["x"]]
  centered_y <- frame$y - pivot[["y"]]
  frame$x <- pivot[["x"]] +
    scale * (centered_x * cos(angle) - centered_y * sin(angle))
  frame$y <- pivot[["y"]] +
    scale * (centered_x * sin(angle) + centered_y * cos(angle))
  frame
}

.builder_alignment_unavailable <- function(sections = list(), message) {
  list(
    available = FALSE,
    message = message,
    sections = sections,
    section = NULL,
    projection_name = NULL,
    transcriptome = NULL,
    spatial = NULL,
    bounds = NULL,
    coordinate_frame = NULL,
    capped = FALSE
  )
}

#' A bounded pair of transcriptome and physical coordinates for alignment.
#'
#' Both frames are sampled with one shared index after joining by canonical
#' cell barcode. That makes linked selection exact and prevents row-order drift.
#' Full-data physical bounds are retained for deterministic image fitting. The
#' worker returns raw sampled spatial coordinates; draft rotation belongs to the
#' persistent browser renderer and canonical transformation remains an R concern.
builder_alignment_preview_model <- function(
  object,
  default_projection = NULL,
  group = NULL,
  section_id = NULL,
  assay = NULL,
  layer = "data",
  coordinate_transforms = NULL,
  max_cells = BUILDER_PREVIEW_MAX
) {
  sections <- builder_spatial_alignment_sections(object)
  if (!length(sections)) {
    return(.builder_alignment_unavailable(
      sections,
      "Spatial or Trekker coordinates are not available for this dataset."
    ))
  }
  section_ids <- vapply(sections, `[[`, character(1), "id")
  if (is.null(section_id) || !section_id %in% section_ids) {
    section_id <- section_ids[[1L]]
  }
  section <- sections[[match(section_id, section_ids)]]

  if (identical(section$kind, "trekker")) {
    payload <- tryCatch(object@misc$trekker, error = function(error) NULL)
    required <- c("barcodes", "x", "y", "ux", "uy")
    lengths <- vapply(
      required,
      function(name) {
        length(payload[[name]] %||% NULL)
      },
      integer(1)
    )
    valid <- is.list(payload) &&
      length(unique(lengths)) == 1L &&
      lengths[[1L]] > 0L &&
      !anyNA(payload$barcodes) &&
      all(nzchar(as.character(payload$barcodes))) &&
      all(vapply(
        c("x", "y", "ux", "uy"),
        function(name) {
          values <- payload[[name]]
          is.numeric(values) &&
            !is.object(values) &&
            !anyNA(values) &&
            all(is.finite(values))
        },
        logical(1)
      ))
    if (!valid) {
      return(.builder_alignment_unavailable(
        sections,
        "Trekker does not contain a valid paired physical projection."
      ))
    }
    groups <- as.character(payload$clusters %||% rep("cells", lengths[[1L]]))
    if (length(groups) != lengths[[1L]]) {
      groups <- rep("cells", lengths[[1L]])
    }
    groups[is.na(groups) | !nzchar(groups)] <- "N/A"
    transcriptome_full <- data.frame(
      cell_barcode = as.character(payload$barcodes),
      x = as.numeric(payload$ux),
      y = as.numeric(payload$uy),
      group = groups,
      stringsAsFactors = FALSE
    )
    spatial_full <- data.frame(
      cell_barcode = as.character(payload$barcodes),
      x = as.numeric(payload$x),
      y = as.numeric(payload$y),
      group = groups,
      stringsAsFactors = FALSE
    )
    keep <- .builder_alignment_sample(nrow(spatial_full), max_cells)
    return(list(
      available = TRUE,
      message = NULL,
      sections = sections,
      section = section,
      projection_name = "Trekker UMAP",
      transcriptome = transcriptome_full[keep, , drop = FALSE],
      spatial = spatial_full[keep, , drop = FALSE],
      coverage = list(x = spatial_full$x, y = spatial_full$y),
      bounds = .builder_alignment_bounds(spatial_full),
      coordinate_frame = .builder_alignment_bounds(spatial_full),
      capped = nrow(spatial_full) > length(keep)
    ))
  }

  reduction <- builder_alignment_projection(
    tryCatch(names(object@reductions), error = function(error) character()),
    default_projection
  )
  if (is.null(reduction)) {
    return(.builder_alignment_unavailable(
      sections,
      "No UMAP, current projection, or PCA is available for transcriptome space."
    ))
  }
  transcriptome_full <- builder_preview_frame(
    object,
    reduction,
    group,
    max_cells = .Machine$integer.max
  )
  assay <- assay %||%
    tryCatch(
      SeuratObject::DefaultAssay(object),
      error = function(error) NULL
    )
  expression_cells <- tryCatch(
    {
      expression <- CerebroNexus:::.getExpressionMatrix(
        seurat = object,
        assay = assay,
        slot = layer,
        join_samples = TRUE,
        allow_cross_semantic_fallback = TRUE,
        verbose = FALSE
      )
      colnames(expression)
    },
    error = function(error) NULL
  )
  if (
    is.null(expression_cells) ||
      !is.character(expression_cells) ||
      !length(expression_cells) ||
      anyNA(expression_cells) ||
      any(!nzchar(expression_cells))
  ) {
    return(.builder_alignment_unavailable(
      sections,
      "The selected assay and layer do not provide a safe expression cohort."
    ))
  }
  physical <- tryCatch(
    builder_spatial_contract(
      object,
      cells = expression_cells,
      image = section$source_id
    )$coordinates,
    error = function(error) NULL
  )
  if (is.null(transcriptome_full) || is.null(physical) || !nrow(physical)) {
    return(.builder_alignment_unavailable(
      sections,
      "The selected section has no safe paired spatial coordinates."
    ))
  }
  coordinate_frame <- .builder_alignment_bounds(physical)
  common <- intersect(
    transcriptome_full$cell_barcode,
    physical$cell_barcode
  )
  if (!length(common)) {
    return(.builder_alignment_unavailable(
      sections,
      "The transcriptome and spatial views share no cell identities."
    ))
  }
  transcriptome_full <- transcriptome_full[
    match(common, transcriptome_full$cell_barcode),
    c("cell_barcode", "x", "y", "group"),
    drop = FALSE
  ]
  spatial_full <- physical[
    match(common, physical$cell_barcode),
    c("cell_barcode", "x", "y"),
    drop = FALSE
  ]
  spatial_full$group <- transcriptome_full$group
  keep <- .builder_alignment_sample(nrow(spatial_full), max_cells)
  list(
    available = TRUE,
    message = NULL,
    sections = sections,
    section = section,
    projection_name = reduction,
    transcriptome = transcriptome_full[keep, , drop = FALSE],
    spatial = spatial_full[keep, , drop = FALSE],
    coverage = list(x = spatial_full$x, y = spatial_full$y),
    bounds = .builder_alignment_bounds(spatial_full),
    coordinate_frame = coordinate_frame,
    coordinate_transform = NULL,
    capped = nrow(spatial_full) > length(keep)
  )
}

builder_spatial_canvas_scene <- function(
  preview,
  colors,
  record = NULL,
  coordinate_transform = NULL,
  identity,
  generation,
  reset_token = 0L,
  dataset = NULL,
  snapshot_identity = NULL,
  section = NULL
) {
  if (!isTRUE(preview$available)) {
    return(list(
      available = FALSE,
      message = preview$message %||% "Spatial preview is unavailable.",
      viewKey = identity,
      dataset = dataset,
      snapshotIdentity = snapshot_identity,
      section = section,
      generation = generation,
      resetToken = reset_token
    ))
  }
  points <- preview$spatial
  levels <- unique(as.character(points$group))
  palette <- builder_level_colors(levels)
  shared <- intersect(levels, names(colors %||% character()))
  palette[shared] <- colors[shared]
  counts <- table(points$group)
  list(
    available = TRUE,
    viewKey = identity,
    dataset = dataset,
    snapshotIdentity = snapshot_identity,
    section = section,
    generation = generation,
    resetToken = reset_token,
    capped = isTRUE(preview$capped),
    points = list(
      x = unname(as.numeric(points$x)),
      y = unname(as.numeric(points$y)),
      barcode = as.character(points$cell_barcode),
      group = as.character(points$group),
      color = unname(palette[as.character(points$group)]),
      count = unname(as.integer(counts[as.character(points$group)]))
    ),
    bounds = preview$coordinate_frame %||% preview$bounds,
    image = if (is.null(record)) {
      NULL
    } else {
      list(
        uri = record$source_uri %||% record$uri,
        baseBounds = record$base_bounds
      )
    },
    controls = utils::modifyList(
      list(coordinateRotation = coordinate_transform$rotation_degrees %||% 0),
      as.list(
        if (is.null(record)) {
          builder_alignment_defaults()
        } else {
          .builder_alignment_parameters(record)
        }
      )
    )
  )
}

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
  cells <- SeuratObject::Cells(object)
  embedding_match <- builder_match_cells(
    rownames(emb),
    cells,
    mode = "exact"
  )
  metadata_match <- builder_match_cells(
    rownames(object@meta.data),
    cells,
    mode = "exact"
  )
  assert_preview_identity <- function(match, component) {
    if (isTRUE(match$valid)) {
      return(invisible(match))
    }
    if (length(match$duplicates) || length(match$expected_duplicates)) {
      stop(
        component,
        " preview identity contains duplicate cell barcodes.",
        call. = FALSE
      )
    }
    if (length(match$blanks) || length(match$expected_blanks)) {
      stop(
        component,
        " preview identity contains blank cell barcodes.",
        call. = FALSE
      )
    }
    stop(
      component,
      " preview identity does not match the dataset.",
      call. = FALSE
    )
  }
  assert_preview_identity(embedding_match, "Projection")
  assert_preview_identity(metadata_match, "Metadata")
  emb <- emb[embedding_match$reorder_index, , drop = FALSE]
  metadata <- object@meta.data[
    metadata_match$reorder_index,
    ,
    drop = FALSE
  ]

  df <- data.frame(
    cell_barcode = cells,
    x = as.numeric(emb[, 1]),
    y = as.numeric(emb[, 2]),
    stringsAsFactors = FALSE
  )
  if (!is.null(group) && group %in% colnames(metadata)) {
    df$group <- as.character(metadata[[group]])
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
  attr(df, "capped") <- nrow(emb) > max_cells
  df
}

#' Build every projection thumbnail in one bounded worker pass.
#'
#' Only the existing two-dimensional embeddings and one metadata column are
#' touched. Expression assays are deliberately outside this contract.
builder_projection_preview_catalog <- function(
  object,
  projections,
  group = NULL,
  max_cells = 600L
) {
  projections <- unique(as.character(projections %||% character()))
  projections <- projections[
    !is.na(projections) &
      nzchar(projections) &
      projections %in% names(object@reductions)
  ]
  if (!length(projections)) {
    return(list())
  }
  max_cells <- suppressWarnings(as.integer(max_cells))
  if (is.na(max_cells) || max_cells < 1L) {
    max_cells <- 600L
  }
  previews <- lapply(projections, function(projection) {
    builder_preview_frame(
      object,
      reduction = projection,
      group = group,
      max_cells = max_cells
    )
  })
  names(previews) <- projections
  Filter(Negate(is.null), previews)
}

.builder_trajectory_preview_key <- function(method, name) {
  paste0(method, "::", name)
}

#' Build bounded trajectory thumbnails without consulting expression assays.
builder_trajectory_preview_catalog <- function(
  object,
  trajectories,
  max_cells = 600L,
  max_edges = 250L
) {
  if (!is.list(trajectories) || !length(trajectories)) {
    return(list())
  }
  source <- tryCatch(object@misc$trajectories, error = function(error) NULL)
  if (!is.list(source) || !length(source)) {
    return(list())
  }
  max_cells <- suppressWarnings(as.integer(max_cells))
  max_edges <- suppressWarnings(as.integer(max_edges))
  if (is.na(max_cells) || max_cells < 1L) {
    max_cells <- 600L
  }
  if (is.na(max_edges) || max_edges < 1L) {
    max_edges <- 250L
  }

  previews <- list()
  for (method in intersect(names(trajectories), names(source))) {
    names_for_method <- unique(as.character(
      trajectories[[method]] %||% character()
    ))
    available <- source[[method]]
    if (!is.list(available)) {
      next
    }
    for (name in intersect(names_for_method, names(available))) {
      payload <- available[[name]]
      if (!is.list(payload)) {
        next
      }
      meta <- payload$meta
      edges <- payload$edges
      if (
        !is.data.frame(meta) ||
          !all(c("DR_1", "DR_2", "pseudotime", "state") %in% names(meta))
      ) {
        next
      }
      x <- suppressWarnings(as.numeric(meta$DR_1))
      y <- suppressWarnings(as.numeric(meta$DR_2))
      pseudotime <- suppressWarnings(as.numeric(meta$pseudotime))
      cells <- rownames(meta)
      valid <- is.finite(x) &
        is.finite(y) &
        is.finite(pseudotime) &
        !is.na(cells) &
        nzchar(cells)
      if (!any(valid)) {
        next
      }
      points <- data.frame(
        cell_barcode = cells[valid],
        x = x[valid],
        y = y[valid],
        group = as.character(meta$state[valid]),
        pseudotime = pseudotime[valid],
        stringsAsFactors = FALSE
      )
      points$group[is.na(points$group) | !nzchar(points$group)] <- "N/A"
      keep <- .builder_alignment_sample(nrow(points), max_cells)
      points <- points[keep, , drop = FALSE]

      edge_frame <- data.frame(
        x = numeric(),
        y = numeric(),
        xend = numeric(),
        yend = numeric()
      )
      edge_columns <- c(
        "source_dim_1",
        "source_dim_2",
        "target_dim_1",
        "target_dim_2"
      )
      if (is.data.frame(edges) && all(edge_columns %in% names(edges))) {
        edge_frame <- data.frame(
          x = suppressWarnings(as.numeric(edges$source_dim_1)),
          y = suppressWarnings(as.numeric(edges$source_dim_2)),
          xend = suppressWarnings(as.numeric(edges$target_dim_1)),
          yend = suppressWarnings(as.numeric(edges$target_dim_2))
        )
        edge_frame <- edge_frame[
          apply(is.finite(as.matrix(edge_frame)), 1L, all),
          ,
          drop = FALSE
        ]
        if (nrow(edge_frame) > max_edges) {
          set.seed(43L)
          edge_frame <- edge_frame[
            sort(sample.int(nrow(edge_frame), max_edges)),
            ,
            drop = FALSE
          ]
        }
      }
      previews[[.builder_trajectory_preview_key(method, name)]] <- list(
        points = points,
        edges = edge_frame
      )
    }
  }
  previews
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

  group_counts <- table(df$group)
  df$hover <- paste0(
    df$group,
    " (",
    unname(group_counts[df$group]),
    " cells)",
    "<br>x: ",
    signif(df$x, 4),
    " · y: ",
    signif(df$y, 4)
  )
  opacity <- if (nrow(df) > 5000L) 0.42 else 0.75
  plotly::plot_ly(
    data = df,
    x = ~x,
    y = ~y,
    color = ~group,
    colors = palette,
    type = "scattergl",
    mode = "markers",
    marker = list(size = 4, opacity = opacity, line = list(width = 0)),
    hoverinfo = "text",
    text = ~hover
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

#' Draw either half of the linked alignment workbench.
builder_alignment_plot <- function(
  frame,
  colors = NULL,
  image_uri = NULL,
  image_bounds = NULL,
  coordinate_frame = NULL,
  coordinate_transform = NULL,
  image_opacity = 0.8,
  point_opacity = 0.85,
  point_size = 5
) {
  if (is.null(frame) || !nrow(frame)) {
    return(NULL)
  }
  levels <- unique(as.character(frame$group))
  fallback <- builder_level_colors(levels)
  if (length(colors)) {
    shared <- intersect(levels, names(colors))
    fallback[shared] <- colors[shared]
  }
  counts <- table(frame$group)
  hover <- paste0(
    frame$cell_barcode,
    "<br>",
    frame$group,
    " · ",
    unname(counts[frame$group]),
    " cells"
  )
  plot <- plotly::plot_ly(
    data = frame,
    x = ~x,
    y = ~y,
    type = "scattergl",
    mode = "markers",
    marker = list(
      color = unname(fallback[frame$group]),
      size = point_size,
      opacity = point_opacity,
      line = list(width = 0)
    ),
    text = hover,
    hoverinfo = "text",
    showlegend = FALSE
  )
  frame_bounds <- coordinate_frame %||% .builder_alignment_bounds(frame)
  initial_outline <- builder_alignment_coordinate_frame(frame_bounds)
  frame_outline <- builder_alignment_coordinate_frame(
    frame_bounds,
    coordinate_transform
  )
  if (!is.null(initial_outline)) {
    plot <- plotly::add_trace(
      plot,
      data = initial_outline,
      x = ~x,
      y = ~y,
      type = "scatter",
      mode = "lines",
      line = list(color = "#9a958d", width = 1, dash = "dot"),
      hoverinfo = "skip",
      showlegend = FALSE,
      inherit = FALSE
    )
  }
  if (!is.null(frame_outline)) {
    plot <- plotly::add_trace(
      plot,
      data = frame_outline,
      x = ~x,
      y = ~y,
      type = "scatter",
      mode = "lines",
      line = list(color = "#5f5a54", width = 1.5),
      hoverinfo = "skip",
      showlegend = FALSE,
      inherit = FALSE
    )
    rotation <- as.numeric(coordinate_transform$rotation_degrees %||% 0)
    reference_edge <- frame_outline[1:2, , drop = FALSE]
    rotation_label <- format(
      round(rotation, 1),
      trim = TRUE,
      scientific = FALSE
    )
    reference_label <- data.frame(
      x = mean(reference_edge$x),
      y = mean(reference_edge$y),
      label = paste0(if (rotation > 0) "+" else "", rotation_label, "°")
    )
    plot <- plotly::add_trace(
      plot,
      data = reference_edge,
      x = ~x,
      y = ~y,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "#d45500", width = 3),
      marker = list(color = "#d45500", size = 6),
      hoverinfo = "skip",
      showlegend = FALSE,
      inherit = FALSE
    )
    plot <- plotly::add_trace(
      plot,
      data = reference_label,
      x = ~x,
      y = ~y,
      text = ~label,
      type = "scatter",
      mode = "text",
      textposition = "top center",
      textfont = list(color = "#d45500", size = 12),
      hoverinfo = "skip",
      showlegend = FALSE,
      inherit = FALSE
    )
  }
  images <- list()
  if (!is.null(image_uri) && .builder_alignment_valid_bounds(image_bounds)) {
    images <- list(list(
      source = image_uri,
      xref = "x",
      yref = "y",
      x = image_bounds$xmin,
      y = image_bounds$ymax,
      sizex = image_bounds$xmax - image_bounds$xmin,
      sizey = image_bounds$ymax - image_bounds$ymin,
      sizing = "stretch",
      opacity = image_opacity,
      layer = "below"
    ))
  }
  plot <- plotly::layout(
    plot,
    images = images,
    dragmode = FALSE,
    xaxis = list(
      zeroline = FALSE,
      showgrid = TRUE,
      gridcolor = "rgba(0, 0, 0, 0.08)",
      showticklabels = TRUE,
      ticks = "outside",
      fixedrange = TRUE
    ),
    yaxis = list(
      zeroline = FALSE,
      showgrid = TRUE,
      gridcolor = "rgba(0, 0, 0, 0.08)",
      showticklabels = TRUE,
      ticks = "outside",
      fixedrange = TRUE,
      scaleanchor = "x"
    ),
    margin = list(l = 12, r = 6, t = 6, b = 12),
    showlegend = FALSE,
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)"
  ) |>
    plotly::config(
      staticPlot = TRUE,
      displayModeBar = FALSE,
      displaylogo = FALSE,
      responsive = TRUE,
      scrollZoom = FALSE
    )
  plot
}

#' One bounded legend shared by both alignment plots.
builder_alignment_legend_ui <- function(frame, colors = NULL) {
  if (is.null(frame) || !nrow(frame)) {
    return(NULL)
  }
  counts <- sort(table(as.character(frame$group)), decreasing = TRUE)
  levels <- names(counts)
  palette <- builder_level_colors(levels)
  if (length(colors)) {
    shared <- intersect(levels, names(colors))
    palette[shared] <- colors[shared]
  }
  htmltools::tags$div(
    class = "spatial-alignment-legend",
    role = "list",
    lapply(levels, function(level) {
      htmltools::tags$span(
        class = "spatial-alignment-legend-item",
        role = "listitem",
        htmltools::tags$i(
          class = "spatial-alignment-legend-swatch",
          style = paste0("background:", palette[[level]])
        ),
        htmltools::tags$span(level),
        htmltools::tags$small(format(unname(counts[[level]]), big.mark = ","))
      )
    })
  )
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

builder_normalize_hex_color <- function(value) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !grepl("^#[0-9A-Fa-f]{6}$", value)
  ) {
    return(NULL)
  }
  toupper(value)
}

builder_group_level_label <- function(level) {
  if (identical(as.character(level), "N/A")) "Missing" else as.character(level)
}

builder_settings_color_overrides <- function(settings) {
  normalize_group <- function(values) {
    if (!is.atomic(values) || is.null(names(values))) {
      return(stats::setNames(character(), character()))
    }
    normalized <- lapply(as.character(values), builder_normalize_hex_color)
    valid <- !vapply(normalized, is.null, logical(1)) & nzchar(names(values))
    stats::setNames(
      unlist(normalized[valid], use.names = FALSE),
      names(values)[valid]
    )
  }
  legacy <- settings$colors %||% list()
  previous <- settings$color_overrides %||% list()
  canonical <- settings$group_color_overrides %||% list()
  groups <- unique(c(names(legacy), names(previous), names(canonical)))
  out <- list()
  for (group in groups) {
    source <- if (group %in% names(canonical)) {
      canonical[[group]]
    } else if (group %in% names(previous)) {
      previous[[group]]
    } else {
      legacy[[group]]
    }
    normalized <- normalize_group(source)
    if (length(normalized)) {
      out[[group]] <- normalized
    }
  }
  out
}

builder_update_color_override <- function(overrides, group, level, value) {
  normalized <- builder_normalize_hex_color(value)
  if (
    is.null(normalized) ||
      !is.character(group) ||
      length(group) != 1L ||
      is.na(group) ||
      !nzchar(group) ||
      !is.character(level) ||
      length(level) != 1L ||
      is.na(level) ||
      !nzchar(level)
  ) {
    return(overrides %||% list())
  }
  next_overrides <- overrides %||% list()
  group_overrides <- next_overrides[[group]] %||%
    stats::setNames(character(), character())
  group_overrides[[level]] <- normalized
  next_overrides[[group]] <- group_overrides
  next_overrides
}

builder_reset_color_overrides <- function(overrides, group) {
  next_overrides <- overrides %||% list()
  if (
    is.character(group) &&
      length(group) == 1L &&
      !is.na(group) &&
      nzchar(group)
  ) {
    next_overrides[[group]] <- NULL
  }
  next_overrides
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
  out <- toupper(fn(length(levels)))
  names(out) <- levels
  ## The viewer forces N/A to grey; matching it keeps the preview honest.
  if ("N/A" %in% levels) {
    out[["N/A"]] <- "#898989"
  }
  if (length(overrides)) {
    shared <- intersect(levels, names(overrides))
    if (length(shared)) {
      normalized <- lapply(overrides[shared], builder_normalize_hex_color)
      valid <- !vapply(normalized, is.null, logical(1))
      if (any(valid)) {
        out[shared[valid]] <- unlist(normalized[valid], use.names = FALSE)
      }
    }
  }
  out
}
