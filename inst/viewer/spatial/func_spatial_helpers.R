##----------------------------------------------------------------------------##
## Spatial helper functions, sourced into the app so the Spatial tab works in a
## plain `runApp("inst")` session without the package installed.
##
## This file is the single implementation used by the Shiny runtime and the
## unit tests. Keep runtime-only helpers here instead of duplicating them under
## R/.
##
## External calls stay namespaced (ape::Moran.I, grDevices::chull, stats::*), so
## only the host packages need to be installed, not CerebroNexus itself.
##----------------------------------------------------------------------------##

spatial_panel_descriptors <- function(spatial_names) {
  spatial_names <- as.character(spatial_names)
  lapply(seq_along(spatial_names), function(index) {
    key <- sprintf("spatial_panel_%03d", index)
    plot_id <- if (index == 1L) {
      "spatial_projection"
    } else {
      paste0("spatial_projection_", sprintf("%03d", index))
    }
    list(
      name = spatial_names[[index]],
      key = key,
      plot_id = plot_id,
      background_id = if (index == 1L) {
        "spatial_projection_background_image"
      } else {
        paste0(plot_id, "_background_image")
      },
      morans_id = paste0(plot_id, "_morans_i"),
      count_id = paste0(plot_id, "_selected_count"),
      zoom_id = paste0(plot_id, "_zoom_to_selection"),
      clear_id = paste0(plot_id, "_clear_selection")
    )
  })
}

normalize_spatial_panel_selection <- function(selected, available) {
  if (is.null(selected) || !length(selected)) {
    return(character())
  }
  selected <- as.character(selected)
  available <- as.character(available)
  unique(selected[!is.na(selected) & selected %in% available])
}

spatial_projection_axis_ranges <- function(
  coordinates,
  fit_visible = TRUE,
  x_range = NULL,
  y_range = NULL,
  margin_fraction = 0.02
) {
  manual_ranges <- list(x = x_range, y = y_range)
  if (
    !isTRUE(fit_visible) || NROW(coordinates) == 0L || NCOL(coordinates) < 2L
  ) {
    return(manual_ranges)
  }

  x <- suppressWarnings(as.numeric(coordinates[, 1L]))
  y <- suppressWarnings(as.numeric(coordinates[, 2L]))
  keep <- is.finite(x) & is.finite(y)
  if (!any(keep)) {
    return(manual_ranges)
  }

  padded_range <- function(values) {
    extent <- range(values, na.rm = TRUE)
    span <- diff(extent)
    margin <- if (is.finite(span) && span > 0) {
      span * margin_fraction
    } else {
      max(abs(extent), 1) * margin_fraction
    }
    c(extent[[1L]] - margin, extent[[2L]] + margin)
  }

  list(
    x = padded_range(x[keep]),
    y = padded_range(y[keep])
  )
}

resolve_spatial_image_preset <- function(
  option_name,
  fallback,
  options,
  crb_files,
  selected
) {
  if (
    is.null(options) ||
      is.null(options[[option_name]]) ||
      is.null(crb_files) ||
      is.null(selected)
  ) {
    return(fallback)
  }
  idx <- which(crb_files == selected)
  if (length(idx) == 0) {
    return(fallback)
  }
  current_name <- names(crb_files)[idx[1]]
  if (
    is.null(current_name) ||
      !(current_name %in% names(options[[option_name]]))
  ) {
    return(fallback)
  }
  val <- options[[option_name]][[current_name]]
  if (is.null(val) || length(val) != 1 || is.na(val)) fallback else val
}

## Resolve the server-side allowlist for the selected dataset. Browser-provided
## selectInput values are never an authority for which files may be read.
configured_spatial_images <- function(
  options,
  crb_files = NULL,
  selected = NULL,
  crb_names = NULL
) {
  if (is.null(options) || is.null(options[["spatial_images"]])) {
    return(character())
  }

  spatial_images <- options[["spatial_images"]]
  if (length(spatial_images) == 0L) {
    return(character())
  }

  if (is.null(crb_files) || is.null(selected)) {
    return(character())
  }
  index <- which(crb_files == selected)
  if (length(index) == 0L) {
    return(character())
  }
  dataset <- names(crb_files)[index[[1L]]]
  if (
    (is.null(dataset) || is.na(dataset) || !nzchar(dataset)) &&
      length(crb_names) >= index[[1L]]
  ) {
    dataset <- crb_names[[index[[1L]]]]
  }
  if (
    (is.null(dataset) || is.na(dataset) || !nzchar(dataset)) &&
      length(crb_files) == 1L &&
      length(spatial_images) == 1L
  ) {
    dataset <- names(spatial_images)[[1L]]
  }
  if (is.null(dataset) || is.na(dataset) || !nzchar(dataset)) {
    return(character())
  }
  configured <- which(names(spatial_images) == dataset)
  if (length(configured) == 0L) {
    return(character())
  }
  images <- unlist(spatial_images[configured], use.names = FALSE)

  if (!is.character(images)) {
    return(character())
  }
  unique(images[!is.na(images) & nzchar(images)])
}

normalize_spatial_background_choice <- function(
  background_image,
  configured_images,
  has_embedded_image = FALSE
) {
  embedded_ids <- if (isTRUE(has_embedded_image)) {
    "__embedded__"
  } else if (is.character(has_embedded_image)) {
    unique(has_embedded_image[
      !is.na(has_embedded_image) & nzchar(has_embedded_image)
    ])
  } else {
    character()
  }
  allowed <- c(
    "No Background",
    configured_images,
    embedded_ids
  )
  if (
    !is.character(background_image) ||
      length(background_image) != 1L ||
      is.na(background_image) ||
      !(background_image %in% allowed)
  ) {
    return("No Background")
  }
  background_image
}

resolve_spatial_background_mode <- function(
  background_image,
  mode = "auto",
  configured_images = character(),
  has_embedded_image = FALSE
) {
  embedded_ids <- if (isTRUE(has_embedded_image)) {
    "__embedded__"
  } else if (is.character(has_embedded_image)) {
    unique(has_embedded_image[
      !is.na(has_embedded_image) & nzchar(has_embedded_image)
    ])
  } else {
    character()
  }
  default_image <- if (length(embedded_ids)) {
    embedded_ids[[1L]]
  } else if (length(configured_images)) {
    configured_images[[1L]]
  } else {
    "No Background"
  }
  mode <- if (
    is.character(mode) &&
      length(mode) == 1L &&
      !is.na(mode) &&
      mode %in% c("auto", "none", "custom")
  ) {
    mode
  } else {
    "auto"
  }
  if (identical(mode, "auto")) {
    return(default_image)
  }
  if (identical(mode, "none")) {
    return("No Background")
  }
  if (is.logical(background_image) && length(background_image) == 1L) {
    return(if (isTRUE(background_image)) default_image else "No Background")
  }
  if (is.null(background_image)) {
    return(default_image)
  }
  normalize_spatial_background_choice(
    background_image,
    configured_images,
    has_embedded_image
  )
}

spatial_embedded_backgrounds <- function(spatial_data) {
  if (is.null(spatial_data) || !is.list(spatial_data)) {
    return(list())
  }
  shared_bounds <- spatial_data$histology_image_bounds
  images <- spatial_data$histology_images
  if (is.null(images) || !length(images)) {
    images <- if (!is.null(spatial_data$histology_image)) {
      list("Embedded histology" = spatial_data$histology_image)
    } else {
      list()
    }
  }
  output <- list()
  for (index in seq_along(images)) {
    entry <- images[[index]]
    entry_name <- names(images)[[index]] %||% ""
    if (is.list(entry)) {
      image <- entry$image %||% entry$uri %||% entry$data
      bounds <- entry$bounds %||% shared_bounds
      label <- entry$label %||% entry_name
    } else {
      image <- entry
      bounds <- shared_bounds
      label <- entry_name
    }
    if (
      !is.character(image) ||
        length(image) != 1L ||
        is.na(image) ||
        !nzchar(image)
    ) {
      next
    }
    if (!nzchar(label)) {
      label <- if (length(images) == 1L) {
        "Embedded histology"
      } else {
        paste("Background", index)
      }
    }
    id <- if (length(output) == 0L) {
      "__embedded__"
    } else {
      paste0("__embedded__", length(output) + 1L)
    }
    output[[id]] <- list(label = label, image = image, bounds = bounds)
  }
  output
}

format_spatial_preset_code <- function(
  label,
  offset_x,
  offset_y,
  scale_x,
  scale_y,
  flip_x,
  flip_y
) {
  key <- function(value) paste0('c("', label, '" = ', value, ")")
  lines <- character(0)
  add <- function(option_name, value) {
    lines[[length(lines) + 1]] <<- paste0(
      '"',
      option_name,
      '" = ',
      key(value)
    )
  }
  if (isTRUE(offset_x != 0)) {
    add("spatial_images_offset_x", offset_x)
  }
  if (isTRUE(offset_y != 0)) {
    add("spatial_images_offset_y", offset_y)
  }
  if (isTRUE(scale_x != 1)) {
    add("spatial_images_scale_x", scale_x)
  }
  if (isTRUE(scale_y != 1)) {
    add("spatial_images_scale_y", scale_y)
  }
  if (isTRUE(flip_x)) {
    add("spatial_images_flip_x", "TRUE")
  }
  if (isTRUE(flip_y)) {
    add("spatial_images_flip_y", "TRUE")
  }
  if (length(lines) == 0) {
    return(
      "## No adjustments to persist — the overlay is at its default alignment."
    )
  }
  paste(lines, collapse = ",\n")
}

compute_group_hulls <- function(x, y, group) {
  result <- list()
  if (length(x) == 0) {
    return(result)
  }
  ok <- !is.na(x) & !is.na(y)
  x <- x[ok]
  y <- y[ok]
  group <- group[ok]
  for (g in unique(group)) {
    in_g <- group == g
    gx <- x[in_g]
    gy <- y[in_g]
    if (length(gx) < 3) {
      next
    }
    ## chull needs at least 3 non-collinear points; collinear input returns a
    ## degenerate hull (< 3 vertices) that encloses no area — skip it.
    idx <- grDevices::chull(gx, gy)
    if (length(idx) < 3) {
      next
    }
    ## close the ring by repeating the first vertex
    idx <- c(idx, idx[1])
    result[[g]] <- list(x = gx[idx], y = gy[idx])
  }
  result
}

blend_genes_to_rgb <- function(r = NULL, g = NULL, b = NULL) {
  ## Determine the cell count from whichever channel is supplied.
  n <- max(length(r), length(g), length(b))
  channel <- function(values) {
    if (is.null(values)) {
      return(rep(0L, n))
    }
    values[is.na(values)] <- 0
    mx <- max(values)
    if (mx <= 0) {
      return(rep(0L, n))
    }
    as.integer(round(values / mx * 255))
  }
  rc <- channel(r)
  gc <- channel(g)
  bc <- channel(b)
  paste0("rgb(", rc, ",", gc, ",", bc, ")")
}

morans_i <- function(x, y, values, k = 6) {
  ok <- !is.na(x) & !is.na(y) & !is.na(values)
  x <- x[ok]
  y <- y[ok]
  values <- values[ok]
  n <- length(values)
  if (n < k + 1) {
    return(NA_real_)
  }
  if (stats::sd(values) == 0) {
    return(0)
  }
  ## Euclidean distance matrix, then a binary weight for each cell's k nearest
  ## neighbours (excluding itself). O(n^2); callers down-sample large inputs.
  dmat <- as.matrix(stats::dist(cbind(x, y)))
  weight <- matrix(0, n, n)
  for (i in seq_len(n)) {
    di <- dmat[i, ]
    di[i] <- Inf # never neighbour itself
    nn <- order(di)[seq_len(k)]
    weight[i, nn] <- 1
  }
  ## kNN adjacency is directional (i may be j's neighbour without the reverse),
  ## which yields an asymmetric, un-normalised weight matrix and pushes
  ## ape::Moran.I's statistic outside the documented [-1, 1] range. Symmetrise
  ## (undirected edge if either cell lists the other) then row-normalise so the
  ## weights sum to 1 per cell, giving a well-scaled statistic.
  weight <- pmax(weight, t(weight))
  row_sums <- rowSums(weight)
  row_sums[row_sums == 0] <- 1 # avoid 0/0 for isolated cells
  weight <- weight / row_sums
  ## Moran's I observed statistic, computed natively (matches ape::Moran.I()
  ## $observed to floating-point precision) so the viewer needs no ape dependency:
  ##   I = (n / W) * sum_ij w_ij (x_i - xbar)(x_j - xbar) / sum_i (x_i - xbar)^2
  z <- values - mean(values)
  W <- sum(weight)
  (n / W) * sum(weight * outer(z, z)) / sum(z^2)
}
