##----------------------------------------------------------------------------##
## Two things that attach to a data set without touching the expression matrix:
## supplementary tables, and a histology background for spatial data.
##
## Both are the kind of thing a collaborator asks for and that otherwise means
## going back to R: "can I see the DE table next to the UMAP", "can you put the
## H&E behind the spots". Neither needs an analysis to run.
##
## Pure: no Shiny.
##----------------------------------------------------------------------------##

## ---------------------------------------------------------------------------
## Supplementary tables -> @misc$extra_material$tables
## ---------------------------------------------------------------------------

#' Read a delimited file into a data.frame for the Extra material page.
#'
#' @param path File to read.
#' @param name What to call it in the interface.
#'
#' @return A list with `name` and `table`, or `error`.
builder_read_table <- function(path, name = NULL) {
  if (!file.exists(path)) {
    return(list(error = "File not found."))
  }
  ext <- tolower(tools::file_ext(path))
  sep <- switch(ext, csv = ",", tsv = "\t", txt = "\t", NULL)
  if (is.null(sep)) {
    return(list(
      error = paste0(
        "Supported table formats are .csv, .tsv and .txt, not .",
        ext,
        "."
      )
    ))
  }
  df <- try(
    utils::read.delim(
      path,
      sep = sep,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    silent = TRUE
  )
  if (inherits(df, "try-error")) {
    return(list(
      error = paste0(
        "Could not read the table: ",
        conditionMessage(attr(df, "condition"))
      )
    ))
  }
  if (!is.data.frame(df) || nrow(df) == 0 || ncol(df) == 0) {
    return(list(error = "This file has no usable tabular content."))
  }
  list(
    name = if (is.null(name) || !nzchar(name)) {
      tools::file_path_sans_ext(basename(path))
    } else {
      name
    },
    table = df
  )
}

#' Put the collected tables on the object, where exportFromSeurat looks.
builder_attach_tables <- function(object, tables) {
  if (!length(tables)) {
    return(object)
  }
  existing <- object@misc$extra_material$tables
  if (is.null(existing)) {
    existing <- list()
  }
  for (t in tables) {
    existing[[t$name]] <- t$table
  }
  object@misc$extra_material$tables <- existing
  object
}

## ---------------------------------------------------------------------------
## Histology background -> the spatial slot of the written .crb
## ---------------------------------------------------------------------------

#' Read an image file into an array png::writePNG can write back out.
builder_read_image <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("png")) {
    if (!requireNamespace("png", quietly = TRUE)) {
      return(list(error = "Reading PNG images requires the png package."))
    }
    arr <- try(png::readPNG(path), silent = TRUE)
  } else if (ext %in% c("jpg", "jpeg")) {
    if (!requireNamespace("jpeg", quietly = TRUE)) {
      return(list(
        error = paste0(
          "Reading JPEG images requires the jpeg package ",
          "(install.packages(\"jpeg\")), or convert the image to PNG."
        )
      ))
    }
    arr <- try(jpeg::readJPEG(path), silent = TRUE)
  } else {
    return(list(
      error = paste0(
        "Supported image formats are PNG and JPEG, not .",
        ext,
        "."
      )
    ))
  }
  if (inherits(arr, "try-error")) {
    return(list(
      error = paste0(
        "Could not read the image: ",
        conditionMessage(attr(arr, "condition"))
      )
    ))
  }
  list(array = arr, width = dim(arr)[2], height = dim(arr)[1])
}

#' Downscale and encode an image array as a data URI.
#'
#' The image is the single biggest thing in a spatial `.crb` -- a
#' full-resolution slide scan is not viable -- so `max_px` is a real control,
#' not a formality.
builder_encode_image <- function(
  arr,
  max_px = 1400,
  flip_y = FALSE,
  flip_x = FALSE,
  rotate = 0
) {
  if (!requireNamespace("png", quietly = TRUE)) {
    return(list(error = "Embedding images requires the png package."))
  }
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    return(list(error = "Embedding images requires the base64enc package."))
  }
  if (length(dim(arr)) == 2) {
    arr <- array(arr, dim = c(dim(arr), 1L))
  }
  h <- dim(arr)[1]
  w <- dim(arr)[2]

  scale <- min(1, max_px / max(h, w))
  if (scale < 1) {
    rows <- unique(round(seq(1, h, length.out = max(1, floor(h * scale)))))
    cols <- unique(round(seq(1, w, length.out = max(1, floor(w * scale)))))
    arr <- arr[rows, cols, , drop = FALSE]
  }
  if (isTRUE(flip_y)) {
    arr <- arr[rev(seq_len(dim(arr)[1])), , , drop = FALSE]
  }
  if (isTRUE(flip_x)) {
    arr <- arr[, rev(seq_len(dim(arr)[2])), , drop = FALSE]
  }
  if (is.numeric(rotate) && abs(rotate %% 360) > 0.01) {
    arr <- builder_rotate_array(arr, rotate)
  }

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  ok <- try(png::writePNG(arr, tmp), silent = TRUE)
  if (inherits(ok, "try-error")) {
    return(list(
      error = paste0(
        "Could not encode the PNG: ",
        conditionMessage(attr(ok, "condition"))
      )
    ))
  }
  list(
    uri = paste0(
      "data:image/png;base64,",
      base64enc::base64encode(tmp)
    ),
    bytes = file.size(tmp),
    width = dim(arr)[2],
    height = dim(arr)[1]
  )
}

#' Where the image sits, in the same coordinate space as the cells.
#'
#' Three ways to answer, because nothing in the data says which is right:
#' the cells may already be in image pixels, or in physical units with a known
#' scale, or the user may only know "it covers the tissue".
builder_image_bounds <- function(mode, coords, image, um_per_px = 1) {
  x <- coords[[1]]
  y <- coords[[2]]
  if (identical(mode, "pixels")) {
    return(list(xmin = 0, xmax = image$width, ymin = 0, ymax = image$height))
  }
  if (identical(mode, "physical")) {
    if (!is.finite(um_per_px) || um_per_px <= 0) {
      return(list(error = "Physical units per pixel must be positive."))
    }
    return(list(
      xmin = 0,
      xmax = image$width * um_per_px,
      ymin = 0,
      ymax = image$height * um_per_px
    ))
  }
  ## Last resort: the cells' own bounding box. Usually wrong -- a slide is
  ## bigger than the area the cells cover -- so it is labelled as such.
  list(
    xmin = min(x, na.rm = TRUE),
    xmax = max(x, na.rm = TRUE),
    ymin = min(y, na.rm = TRUE),
    ymax = max(y, na.rm = TRUE)
  )
}

#' Does the image cover every cell?
#'
#' Cells outside the image render on empty background, which reads as a bad
#' alignment rather than a bad extent, so it is worth saying out loud.
builder_bounds_cover <- function(bounds, coords) {
  x <- coords[[1]]
  y <- coords[[2]]
  outside <- sum(
    x < bounds$xmin | x > bounds$xmax | y < bounds$ymin | y > bounds$ymax,
    na.rm = TRUE
  )
  list(outside = outside, total = length(x))
}

#' Carry `@misc$trekker` into a `.crb` that has already been exported.
#'
#' The other modalities need nothing from the builder: `exportFromSeurat()`
#' reads immune repertoire and HLA typing straight off `@misc`. Trekker is the
#' exception -- the exporter never looks at it, so an object carrying a Trekker
#' map exports without one and the page silently never appears. The repository's
#' own demo build works around this the same way, with `crb$addTrekker()` after
#' the fact.
#'
#' @param crb_path The `.crb` just written.
#' @param trekker The `@misc$trekker` payload, or `NULL` to do nothing.
builder_attach_trekker <- function(crb_path, trekker) {
  if (is.null(trekker) || !length(trekker)) {
    return(list(applied = FALSE))
  }
  if (!is.list(trekker)) {
    return(list(error = "Trekker data must be a list."))
  }
  crb <- try(readRDS(crb_path), silent = TRUE)
  if (inherits(crb, "try-error")) {
    return(list(error = "The exported .crb could not be read back."))
  }
  ok <- try(crb$addTrekker(trekker), silent = TRUE)
  if (inherits(ok, "try-error")) {
    return(list(
      error = paste0(
        "Could not attach Trekker data: ",
        conditionMessage(attr(ok, "condition"))
      )
    ))
  }
  saveRDS(crb, crb_path, compress = "xz")
  list(applied = TRUE)
}

#' Pair one shared picture with the extent computed for each section.
#'
#' Sections cut from one block share a slide scan but not a position: they sit
#' at different offsets in the coordinate space. An earlier version of "apply to
#' all" copied the whole entry, extent included, so four slides out of five were
#' written thousands of units from their own cells -- selectable in the viewer,
#' invisible on screen, and reported as done. Only `uri` and the picture's own
#' dimensions may be shared; `bounds` and the coverage count belong to the
#' section.
#'
#' @param picture The encoded image: `uri`, `bytes`, `width`, `height`.
#' @param per_section Named list, one entry per section, each `list(bounds =,
#'   cover = list(outside =, total =))`.
builder_pair_sections <- function(picture, per_section) {
  out <- list()
  for (nm in names(per_section)) {
    got <- per_section[[nm]]
    out[[nm]] <- list(
      uri = picture$uri,
      bounds = got$bounds,
      bytes = picture$bytes,
      width = picture$width,
      height = picture$height,
      outside = got$cover$outside,
      total = got$cover$total
    )
  }
  out
}

#' Write the background into a `.crb` that has already been exported.
#'
#' `exportFromSeurat()` cannot carry an image, so this is a read-modify-write
#' on the file it produced -- the same thing the repository's own demo builds
#' do by hand.
builder_attach_histology <- function(crb_path, images) {
  if (!length(images)) {
    return(list(applied = character()))
  }
  crb <- try(readRDS(crb_path), silent = TRUE)
  if (inherits(crb, "try-error")) {
    return(list(error = "The exported .crb could not be read back."))
  }
  available <- try(crb$availableSpatial(), silent = TRUE)
  if (inherits(available, "try-error") || !length(available)) {
    return(list(error = "The .crb contains no spatial data."))
  }
  targets <- intersect(names(images), available)
  if (!length(targets)) {
    return(list(error = "Configured image sections are absent from the .crb."))
  }
  ## One image and one extent PER SECTION. Writing a single pair into every
  ## section -- which is what this did -- puts one slide's histology behind
  ## every other slide's cells, at an extent computed from the wrong
  ## coordinates. The .crb has carried per-section fields all along.
  for (nm in targets) {
    sd <- crb$getSpatialData(nm)
    sd$histology_image <- images[[nm]]$uri
    sd$histology_image_bounds <- images[[nm]]$bounds
    crb$addSpatialData(nm, sd)
  }
  saveRDS(crb, crb_path, compress = "xz")
  list(applied = targets)
}

#' Attach every post-export payload with one atomic CRB replacement.
#'
#' Histology and Trekker both require a read-modify-write after
#' `exportFromSeurat()`. Doing them separately writes the same large CRB twice
#' and leaves a partially augmented file when the second write fails. This
#' helper validates and applies both in memory, writes a sibling temporary file,
#' then replaces the original with rollback.
builder_attach_crb_extras <- function(
  crb_path,
  images = list(),
  trekker = NULL
) {
  if (!length(images) && (is.null(trekker) || !length(trekker))) {
    return(list(applied = character(), trekker = FALSE))
  }
  if (!is.null(trekker) && length(trekker) && !is.list(trekker)) {
    return(list(error = "Trekker data must be a list."))
  }

  crb <- try(readRDS(crb_path), silent = TRUE)
  if (inherits(crb, "try-error")) {
    return(list(error = "The exported .crb could not be read back."))
  }

  applied <- character()
  if (length(images)) {
    available <- try(crb$availableSpatial(), silent = TRUE)
    if (inherits(available, "try-error") || !length(available)) {
      return(list(error = "The .crb contains no spatial data."))
    }
    applied <- intersect(names(images), available)
    if (!length(applied)) {
      return(list(
        error = "Configured image sections are absent from the .crb."
      ))
    }
    for (name in applied) {
      spatial <- crb$getSpatialData(name)
      spatial$histology_image <- images[[name]]$uri
      spatial$histology_image_bounds <- images[[name]]$bounds
      crb$addSpatialData(name, spatial)
    }
  }

  trekker_applied <- FALSE
  if (!is.null(trekker) && length(trekker)) {
    added <- try(crb$addTrekker(trekker), silent = TRUE)
    if (inherits(added, "try-error")) {
      return(list(
        error = paste0(
          "Could not attach Trekker data: ",
          conditionMessage(attr(added, "condition"))
        )
      ))
    }
    trekker_applied <- TRUE
  }

  temporary <- tempfile(
    paste0(".", basename(crb_path), "-"),
    tmpdir = dirname(crb_path)
  )
  backup <- tempfile(
    paste0(".", basename(crb_path), "-backup-"),
    tmpdir = dirname(crb_path)
  )
  on.exit(unlink(c(temporary, backup), force = TRUE), add = TRUE)

  written <- try(saveRDS(crb, temporary, compress = "xz"), silent = TRUE)
  if (inherits(written, "try-error") || !file.exists(temporary)) {
    return(list(error = "Could not write the augmented .crb."))
  }
  if (!file.rename(crb_path, backup)) {
    return(list(
      error = "Could not protect the exported .crb before updating it."
    ))
  }
  if (!file.rename(temporary, crb_path)) {
    file.rename(backup, crb_path)
    return(list(
      error = "Could not replace the exported .crb; it was restored."
    ))
  }
  unlink(backup, force = TRUE)

  list(applied = applied, trekker = trekker_applied)
}

#' The coordinates of the first spatial slice, for bounds decisions.
#'
#' `@images` entries differ per platform; the coordinate slot is what they
#' agree on.
builder_spatial_coords <- function(object, image = NULL) {
  images <- tryCatch(names(object@images), error = function(e) NULL)
  if (!length(images)) {
    return(NULL)
  }
  ## Which section. Taking the first unconditionally is what made the alignment
  ## preview draw section one's cells no matter which section was being aligned.
  if (is.null(image) || !(image %in% images)) {
    image <- images[1]
  }
  co <- tryCatch(
    SeuratObject::GetTissueCoordinates(object[[image]]),
    error = function(e) NULL
  )
  if (is.null(co) || ncol(co) < 2) {
    return(NULL)
  }
  numeric_cols <- which(vapply(co, is.numeric, logical(1)))
  if (length(numeric_cols) < 2) {
    return(NULL)
  }
  list(co[[numeric_cols[1]]], co[[numeric_cols[2]]])
}

#' Rotate an image array by an arbitrary angle.
#'
#' Nearest-neighbour: this is a background behind points, and the alternative
#' is pulling in an imaging package for a picture nobody will zoom into.
#' The canvas grows so nothing is cropped, and the new corners are transparent
#' where the source does not reach.
builder_rotate_array <- function(arr, degrees) {
  theta <- degrees * pi / 180
  h <- dim(arr)[1]
  w <- dim(arr)[2]
  ch <- dim(arr)[3]

  ## Big enough for the rotated rectangle at any angle.
  nh <- ceiling(abs(h * cos(theta)) + abs(w * sin(theta)))
  nw <- ceiling(abs(w * cos(theta)) + abs(h * sin(theta)))

  ## Alpha channel, so the corners the source does not cover are transparent
  ## rather than black.
  out <- array(0, dim = c(nh, nw, 4L))
  cy0 <- (h + 1) / 2
  cx0 <- (w + 1) / 2
  cy1 <- (nh + 1) / 2
  cx1 <- (nw + 1) / 2

  yy <- matrix(seq_len(nh), nrow = nh, ncol = nw)
  xx <- matrix(seq_len(nw), nrow = nh, ncol = nw, byrow = TRUE)
  dy <- yy - cy1
  dx <- xx - cx1
  ## Rotate the *sample* point backwards to find where it came from.
  sy <- round(cy0 + dy * cos(theta) + dx * sin(theta))
  sx <- round(cx0 - dy * sin(theta) + dx * cos(theta))
  inside <- sy >= 1 & sy <= h & sx >= 1 & sx <= w

  src_index <- ifelse(inside, (sx - 1) * h + sy, NA_integer_)
  for (k in seq_len(min(ch, 3L))) {
    plane <- arr[,, k]
    out[,, k][inside] <- plane[src_index[inside]]
  }
  if (ch >= 4L) {
    plane <- arr[,, 4L]
    out[,, 4L][inside] <- plane[src_index[inside]]
  } else {
    out[,, 4L][inside] <- 1
  }
  if (ch == 1L) {
    out[,, 2L] <- out[,, 1L]
    out[,, 3L] <- out[,, 1L]
  }
  out
}

#' Shift and scale the image extent, the way a user nudges an overlay.
#'
#' Position is expressed as bounds rather than baked into the picture, so
#' moving it costs nothing and can be undone.
builder_adjust_bounds <- function(bounds, dx = 0, dy = 0, scale = 1) {
  w <- (bounds$xmax - bounds$xmin) * scale
  h <- (bounds$ymax - bounds$ymin) * scale
  cx <- (bounds$xmin + bounds$xmax) / 2 + dx
  cy <- (bounds$ymin + bounds$ymax) / 2 + dy
  list(
    xmin = cx - w / 2,
    xmax = cx + w / 2,
    ymin = cy - h / 2,
    ymax = cy + h / 2
  )
}
