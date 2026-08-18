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

builder_table_default_name <- function(filename) {
  name <- tools::file_path_sans_ext(basename(as.character(filename %||% "")))
  name <- trimws(name)
  if (nzchar(name)) name else "Table"
}

builder_table_unique_name <- function(name, existing = character()) {
  if (!name %in% existing) {
    return(name)
  }
  suffix <- 2L
  repeat {
    candidate <- paste0(name, " ", suffix)
    if (!candidate %in% existing) {
      return(candidate)
    }
    suffix <- suffix + 1L
  }
}

#' Read a delimited file into a data.frame for the Extra material page.
#'
#' @param path File to read.
#' @param name What to call it in the interface.
#' @param filename Original client filename, used to identify the format when
#'   an uploaded temporary path has no extension.
#'
#' @return A list with `name` and `table`, or `error`.
builder_read_table <- function(path, name = NULL, filename = path) {
  if (!file.exists(path)) {
    return(list(error = "File not found."))
  }
  ext <- tolower(tools::file_ext(filename))
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
  df <- suppressWarnings(try(
    utils::read.delim(
      path,
      sep = sep,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    silent = TRUE
  ))
  if (inherits(df, "try-error")) {
    return(list(
      error = paste0(
        "Could not read this table. Check that it is a valid ",
        "CSV, TSV or TXT file."
      )
    ))
  }
  if (!is.data.frame(df) || nrow(df) == 0 || ncol(df) == 0) {
    return(list(error = "This file has no usable tabular content."))
  }
  list(
    name = if (is.null(name) || !nzchar(name)) {
      builder_table_default_name(path)
    } else {
      name
    },
    table = df
  )
}

#' Safe client-side file metadata for compact Builder file lists.
builder_safe_file_name <- function(name, fallback = "File") {
  name <- as.character(name %||% character())
  if (length(name) != 1L || is.na(name) || !nzchar(name)) {
    return(fallback)
  }
  name <- basename(gsub("\\", "/", name, fixed = TRUE))
  if (nzchar(name)) name else fallback
}

builder_file_type_label <- function(name, type = NULL) {
  extension <- toupper(tools::file_ext(builder_safe_file_name(name, "")))
  if (nzchar(extension)) {
    return(extension)
  }
  type <- as.character(type %||% character())
  if (length(type) == 1L && !is.na(type) && nzchar(type)) {
    return(toupper(sub("^.*/", "", type)))
  }
  "FILE"
}

builder_file_human_size <- function(bytes) {
  bytes <- suppressWarnings(as.numeric(bytes %||% 0))
  if (length(bytes) != 1L || is.na(bytes) || !is.finite(bytes) || bytes < 0) {
    return("Size unavailable")
  }
  units <- c("bytes", "KB", "MB", "GB", "TB")
  unit <- 1L
  while (bytes >= 1024 && unit < length(units)) {
    bytes <- bytes / 1024
    unit <- unit + 1L
  }
  value <- if (unit == 1L) {
    round(bytes)
  } else {
    round(bytes, if (bytes < 10) 1L else 0L)
  }
  paste(format(value, trim = TRUE, scientific = FALSE), units[[unit]])
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

builder_alignment_defaults <- function() {
  list(
    dx = 0,
    dy = 0,
    scale = 1,
    rotation = 0,
    flip_x = FALSE,
    flip_y = FALSE,
    image_opacity = 0.8,
    point_opacity = 0.85,
    point_size = 5
  )
}

#' Read the saved coverage diagnostic without accepting malformed values.
#'
#' Older Builder projects may omit `outside`; that means no recorded failures.
#' Once present, the value must be one finite, non-negative integer count.
builder_alignment_outside_count <- function(record) {
  if (!is.list(record)) {
    return(NA_integer_)
  }
  if (is.null(record[["outside"]])) {
    return(0L)
  }
  value <- record[["outside"]]
  if (
    !is.numeric(value) ||
      is.object(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value < 0 ||
      value != floor(value) ||
      value > .Machine$integer.max
  ) {
    return(NA_integer_)
  }
  as.integer(value)
}

.builder_alignment_valid_bounds <- function(bounds) {
  is.list(bounds) &&
    all(c("xmin", "xmax", "ymin", "ymax") %in% names(bounds)) &&
    all(is.finite(as.numeric(unlist(bounds[c(
      "xmin",
      "xmax",
      "ymin",
      "ymax"
    )])))) &&
    bounds$xmax > bounds$xmin &&
    bounds$ymax > bounds$ymin
}

#' Fit the original image into the full physical coordinate range.
#'
#' The image is centred and aspect-preserving. It is a stable starting point,
#' not an edit to cell coordinates, and every later transform starts here.
builder_alignment_fit_bounds <- function(bounds, image_dimensions) {
  if (!.builder_alignment_valid_bounds(bounds)) {
    stop("Alignment requires finite, non-empty physical bounds.", call. = FALSE)
  }
  image_dimensions <- as.numeric(image_dimensions)
  if (
    length(image_dimensions) != 2L ||
      anyNA(image_dimensions) ||
      !all(is.finite(image_dimensions)) ||
      any(image_dimensions <= 0)
  ) {
    stop("Alignment requires positive image dimensions.", call. = FALSE)
  }
  image_ratio <- image_dimensions[[1L]] / image_dimensions[[2L]]
  available_width <- bounds$xmax - bounds$xmin
  available_height <- bounds$ymax - bounds$ymin
  available_ratio <- available_width / available_height
  if (image_ratio >= available_ratio) {
    height <- available_height
    width <- height * image_ratio
  } else {
    width <- available_width
    height <- width / image_ratio
  }
  ## Decimal extrema can land a few ulps outside an algebraically identical
  ## centred rectangle (for example 3.92 after subtracting half the height).
  ## Preserve the image aspect ratio while adding an invisible numerical guard
  ## so the default fit really does cover every boundary point.
  safety_factor <- 1 + 64 * .Machine$double.eps
  width <- width * safety_factor
  height <- height * safety_factor
  centre_x <- (bounds$xmin + bounds$xmax) / 2
  centre_y <- (bounds$ymin + bounds$ymax) / 2
  list(
    xmin = centre_x - width / 2,
    xmax = centre_x + width / 2,
    ymin = centre_y - height / 2,
    ymax = centre_y + height / 2
  )
}

.builder_alignment_parameters <- function(parameters = list()) {
  supplied <- parameters %||% list()
  parameters <- builder_alignment_defaults()
  shared <- intersect(names(parameters), names(supplied))
  parameters[shared] <- supplied[shared]
  numeric_fields <- c(
    "dx",
    "dy",
    "scale",
    "rotation",
    "image_opacity",
    "point_opacity",
    "point_size"
  )
  for (name in numeric_fields) {
    value <- suppressWarnings(as.numeric(parameters[[name]]))
    if (length(value) != 1L || is.na(value) || !is.finite(value)) {
      stop("Alignment parameters must be finite.", call. = FALSE)
    }
    parameters[[name]] <- value
  }
  if (
    parameters$scale <= 0 ||
      parameters$point_size <= 0 ||
      parameters$image_opacity < 0 ||
      parameters$image_opacity > 1 ||
      parameters$point_opacity < 0 ||
      parameters$point_opacity > 1
  ) {
    stop(
      "Alignment scale, opacity, or point size is outside its range.",
      call. = FALSE
    )
  }
  parameters$flip_x <- isTRUE(parameters$flip_x)
  parameters$flip_y <- isTRUE(parameters$flip_y)
  parameters
}

#' Apply translation and scale to the immutable default-fit bounds.
builder_alignment_transform_bounds <- function(
  base_bounds,
  parameters = list()
) {
  if (!.builder_alignment_valid_bounds(base_bounds)) {
    stop("Alignment base bounds are invalid.", call. = FALSE)
  }
  parameters <- .builder_alignment_parameters(parameters)
  builder_adjust_bounds(
    base_bounds,
    dx = parameters$dx,
    dy = parameters$dy,
    scale = parameters$scale
  )
}

#' Expand the immutable source-image bounds to the encoded rotation canvas.
#'
#' Arbitrary image rotation grows a transparent canvas around the source. The
#' encoded canvas must keep the same data-units-per-pixel in both axes; fitting
#' that larger canvas back into the unrotated bounds would stretch it.
builder_alignment_oriented_bounds <- function(base_bounds, image_geometry) {
  if (!.builder_alignment_valid_bounds(base_bounds)) {
    stop("Alignment base bounds are invalid.", call. = FALSE)
  }
  geometry <- image_geometry %||% list()
  values <- suppressWarnings(as.numeric(c(
    geometry$source_width,
    geometry$source_height,
    geometry$extent_width,
    geometry$extent_height
  )))
  if (
    length(values) != 4L ||
      anyNA(values) ||
      any(!is.finite(values)) ||
      any(values <= 0)
  ) {
    return(base_bounds)
  }
  source_width <- values[[1L]]
  source_height <- values[[2L]]
  extent_width <- values[[3L]]
  extent_height <- values[[4L]]
  units_per_pixel <- mean(c(
    (base_bounds$xmax - base_bounds$xmin) / source_width,
    (base_bounds$ymax - base_bounds$ymin) / source_height
  ))
  centre_x <- (base_bounds$xmin + base_bounds$xmax) / 2
  centre_y <- (base_bounds$ymin + base_bounds$ymax) / 2
  width <- extent_width * units_per_pixel
  height <- extent_height * units_per_pixel
  list(
    xmin = centre_x - width / 2,
    xmax = centre_x + width / 2,
    ymin = centre_y - height / 2,
    ymax = centre_y + height / 2
  )
}

#' Create the canonical per-section alignment record.
builder_alignment_record <- function(
  source,
  source_uri,
  uri,
  base_bounds,
  parameters = list(),
  image_geometry = NULL,
  section = list()
) {
  parameters <- .builder_alignment_parameters(parameters)
  oriented_bounds <- builder_alignment_oriented_bounds(
    base_bounds,
    image_geometry
  )
  c(
    list(
      source = source,
      source_uri = source_uri,
      uri = uri,
      base_bounds = base_bounds,
      bounds = builder_alignment_transform_bounds(oriented_bounds, parameters)
    ),
    parameters,
    list(
      section_id = as.character(section$id %||% "")[[1L]],
      section_kind = as.character(section$kind %||% "spatial")[[1L]]
    )
  )
}

#' Upgrade an older URI/bounds record without invalidating existing projects.
builder_alignment_normalize <- function(
  record,
  section_id = NULL,
  section_kind = NULL
) {
  if (!is.list(record) || is.null(record$uri) || is.null(record$bounds)) {
    return(NULL)
  }
  parameters <- .builder_alignment_parameters(record)
  base_bounds <- record$base_bounds %||% record$bounds
  normalized <- builder_alignment_record(
    source = record$source %||%
      list(name = "Embedded tissue image", type = "image/png"),
    source_uri = record$source_uri %||% record$uri,
    uri = record$uri,
    base_bounds = base_bounds,
    parameters = parameters,
    image_geometry = record,
    section = list(
      id = section_id %||% record$section_id %||% "",
      kind = section_kind %||% record$section_kind %||% "spatial"
    )
  )
  carried <- setdiff(names(record), c(names(normalized), "saved"))
  normalized[carried] <- record[carried]
  normalized
}

#' Reset one section to its deterministic default fit and appearance.
builder_alignment_reset <- function(record) {
  normalized <- builder_alignment_normalize(record)
  if (is.null(normalized)) {
    return(NULL)
  }
  reset <- builder_alignment_record(
    source = normalized$source,
    source_uri = normalized$source_uri,
    uri = normalized$source_uri,
    base_bounds = normalized$base_bounds,
    parameters = builder_alignment_defaults(),
    image_geometry = list(
      source_width = normalized$source_width,
      source_height = normalized$source_height,
      extent_width = normalized$source_width,
      extent_height = normalized$source_height
    ),
    section = list(
      id = normalized$section_id,
      kind = normalized$section_kind
    )
  )
  carried <- intersect(
    c(
      "bytes",
      "width",
      "height",
      "source_width",
      "source_height",
      "extent_width",
      "extent_height",
      "display_width",
      "display_height",
      "outside",
      "total"
    ),
    names(normalized)
  )
  reset[carried] <- normalized[carried]
  if (!is.null(reset$source_width) && !is.null(reset$source_height)) {
    reset$extent_width <- reset$source_width
    reset$extent_height <- reset$source_height
    reset$display_width <- reset$source_width
    reset$display_height <- reset$source_height
    reset$width <- reset$source_width
    reset$height <- reset$source_height
  }
  reset
}

#' Copy only transform parameters to sections that already own an image.
builder_alignment_apply_transform_to_all <- function(images, source_section) {
  if (!is.list(images) || !source_section %in% names(images)) {
    return(images)
  }
  source <- builder_alignment_normalize(
    images[[source_section]],
    source_section
  )
  if (is.null(source)) {
    return(images)
  }
  fields <- c(
    "dx",
    "dy",
    "scale",
    "rotation",
    "flip_x",
    "flip_y",
    "image_opacity",
    "point_opacity",
    "point_size"
  )
  for (name in setdiff(names(images), source_section)) {
    target <- builder_alignment_normalize(images[[name]], name)
    if (is.null(target)) {
      next
    }
    for (field in fields) {
      target[[field]] <- source[[field]]
    }
    target$bounds <- builder_alignment_transform_bounds(
      builder_alignment_oriented_bounds(target$base_bounds, target),
      target
    )
    images[[name]] <- target
  }
  images[[source_section]] <- source
  images
}

#' The small alignment contract written to a generated Viewer payload.
builder_alignment_payload <- function(record) {
  normalized <- builder_alignment_normalize(record)
  if (is.null(normalized)) {
    return(NULL)
  }
  list(
    source = basename(as.character(normalized$source$name %||% "Tissue image")),
    builder_managed = TRUE,
    dx = normalized$dx,
    dy = normalized$dy,
    scale = normalized$scale,
    rotation = normalized$rotation,
    flip_x = normalized$flip_x,
    flip_y = normalized$flip_y,
    image_opacity = normalized$image_opacity,
    point_opacity = normalized$point_opacity,
    point_size = normalized$point_size
  )
}

#' Convert one Builder alignment to the canonical multi-image leaf contract.
builder_histology_image_payload <- function(record) {
  normalized <- builder_alignment_normalize(record)
  if (is.null(normalized)) {
    return(NULL)
  }
  required <- c("xmin", "xmax", "ymin", "ymax")
  bounds <- stats::setNames(
    as.numeric(unlist(normalized$bounds[required], use.names = FALSE)),
    required
  )
  list(
    histology_image = normalized$uri,
    histology_image_bounds = bounds,
    histology_alignment = builder_alignment_payload(normalized)
  )
}

#' Store one Builder background in the canonical multi-image CRB contract.
#'
#' The Builder currently aligns one uploaded background per spatial section,
#' while a CRB may already contain several embedded images. Preserve those and
#' add the Builder image under its source filename instead of reviving the
#' removed singular `histology_image` fields.
builder_attach_spatial_image <- function(
  spatial,
  record,
  label = NULL,
  replace_managed = TRUE
) {
  normalized <- builder_alignment_normalize(record)
  if (is.null(normalized)) {
    return(NULL)
  }
  images <- spatial[["histology_images", exact = TRUE]] %||% list()
  legacy_image <- spatial[["histology_image", exact = TRUE]]
  legacy_bounds <- spatial[["histology_image_bounds", exact = TRUE]]
  previous_alignment <- spatial[["histology_alignment", exact = TRUE]]
  previous_label <- if (is.list(previous_alignment)) {
    source <- as.character(previous_alignment[["source"]] %||% character())
    if (length(source) == 1L && !is.na(source) && nzchar(source)) {
      basename(source)
    } else {
      NULL
    }
  } else {
    NULL
  }
  previous_builder_label <- if (
    is.list(previous_alignment) &&
      isTRUE(previous_alignment[["builder_managed"]])
  ) {
    previous_label
  } else {
    NULL
  }
  valid_legacy_image <- is.character(legacy_image) &&
    length(legacy_image) == 1L &&
    !is.na(legacy_image) &&
    grepl("^data:image/", legacy_image)
  if (!length(images) && valid_legacy_image) {
    legacy_builder_fields <- c(
      "source",
      "dx",
      "dy",
      "scale",
      "rotation",
      "flip_x",
      "flip_y",
      "image_opacity",
      "point_opacity",
      "point_size"
    )
    legacy_parameters_valid <- isTRUE(tryCatch(
      {
        .builder_alignment_parameters(previous_alignment)
        TRUE
      },
      error = function(error) FALSE
    ))
    legacy_was_builder_managed <- is.list(previous_alignment) &&
      !is.null(previous_label) &&
      all(legacy_builder_fields %in% names(previous_alignment)) &&
      legacy_parameters_valid
    ## The previous Builder wrote its managed upload into the singular fields.
    ## Replace that value during canonical migration. A plain legacy image with
    ## no Builder alignment is user data and remains as an embedded background.
    if (!legacy_was_builder_managed) {
      images <- list(
        `Tissue background` = list(
          histology_image = legacy_image,
          histology_image_bounds = legacy_bounds
        )
      )
    }
  }
  ## Rebuilding or re-aligning replaces the one image managed by the previous
  ## Builder run. Other embedded images remain untouched.
  if (
    isTRUE(replace_managed) &&
      !is.null(previous_builder_label) &&
      previous_builder_label %in% names(images)
  ) {
    images[[previous_builder_label]] <- NULL
  }
  label <- builder_safe_file_name(
    label %||% normalized$source$name,
    fallback = "Builder tissue image"
  )
  if (label %in% names(images)) {
    label <- utils::tail(make.unique(c(names(images), label)), 1L)
  }
  payload <- builder_histology_image_payload(normalized)
  payload$histology_alignment$source <- label
  images[[label]] <- payload
  alignment <- builder_alignment_payload(normalized)
  alignment$source <- label
  spatial[["histology_images"]] <- images
  spatial[["histology_image"]] <- NULL
  spatial[["histology_image_bounds"]] <- NULL
  spatial[["histology_alignment"]] <- alignment
  spatial
}

#' Keep Trekker's physical image out of Seurat spatial section matching.
builder_image_collection_normalize <- function(images) {
  images <- images %||% list()
  if (!is.list(images) || is.object(images)) {
    stop("Spatial image collection must be a named list.", call. = FALSE)
  }
  sections <- names(images)
  if (is.null(sections)) {
    sections <- character()
  }
  if (
    length(images) &&
      (anyNA(sections) || any(!nzchar(sections)) || anyDuplicated(sections))
  ) {
    stop("Spatial section names must be unique and non-empty.", call. = FALSE)
  }
  normalized <- list()
  for (section_id in sections) {
    section <- images[[section_id]]
    legacy <- builder_alignment_normalize(section, section_id = section_id)
    if (!is.null(legacy)) {
      label <- builder_safe_file_name(
        legacy$source$name %||% "Embedded tissue image",
        fallback = "Embedded tissue image"
      )
      normalized[[section_id]] <- stats::setNames(list(legacy), label)
      next
    }
    if (!is.list(section) || is.object(section)) {
      stop(
        "Each Spatial section must contain named image records.",
        call. = FALSE
      )
    }
    labels <- names(section)
    if (
      is.null(labels) ||
        anyNA(labels) ||
        any(!nzchar(trimws(labels)))
    ) {
      stop("Spatial image labels must be non-empty.", call. = FALSE)
    }
    if (anyDuplicated(labels)) {
      stop(
        "Spatial image labels must be unique within each section.",
        call. = FALSE
      )
    }
    records <- lapply(labels, function(label) {
      record <- builder_alignment_normalize(
        section[[label]],
        section_id = section_id,
        section_kind = "spatial"
      )
      if (is.null(record)) {
        stop(
          "Spatial image records must contain an image URI and bounds.",
          call. = FALSE
        )
      }
      record
    })
    names(records) <- labels
    normalized[[section_id]] <- records
  }
  normalized
}

builder_image_collection_flatten <- function(images) {
  images <- builder_image_collection_normalize(images)
  unlist(
    lapply(names(images), function(section_id) {
      lapply(names(images[[section_id]]), function(image_label) {
        c(
          list(section_id = section_id, image_label = image_label),
          images[[section_id]][[image_label]]
        )
      })
    }),
    recursive = FALSE,
    use.names = FALSE
  )
}

builder_image_collection_count <- function(images) {
  as.integer(sum(lengths(builder_image_collection_normalize(images))))
}

builder_image_collection_add <- function(images, section, label, record) {
  images <- builder_image_collection_normalize(images)
  label <- trimws(as.character(label %||% ""))
  if (!nzchar(section) || !nzchar(label)) {
    stop("Spatial section and image label must be non-empty.", call. = FALSE)
  }
  if (label %in% names(images[[section]] %||% list())) {
    stop(
      "Spatial image labels must be unique within each section.",
      call. = FALSE
    )
  }
  normalized <- builder_alignment_normalize(
    record,
    section_id = section,
    section_kind = "spatial"
  )
  if (is.null(normalized)) {
    stop("Spatial image record is invalid.", call. = FALSE)
  }
  images[[section]][[label]] <- normalized
  images
}

builder_image_collection_rename <- function(images, section, from, to) {
  images <- builder_image_collection_normalize(images)
  to <- trimws(as.character(to %||% ""))
  section_images <- images[[section]] %||% list()
  if (!from %in% names(section_images)) {
    stop("The spatial image to rename does not exist.", call. = FALSE)
  }
  if (!nzchar(to) || (to %in% names(section_images) && !identical(to, from))) {
    stop("Spatial image labels must be non-empty and unique.", call. = FALSE)
  }
  if (identical(from, to)) {
    return(images)
  }
  position <- match(from, names(section_images))
  names(section_images)[[position]] <- to
  images[[section]] <- section_images
  images
}

builder_image_collection_remove <- function(images, section, label) {
  images <- builder_image_collection_normalize(images)
  section_images <- images[[section]] %||% list()
  if (!label %in% names(section_images)) {
    return(images)
  }
  section_images[[label]] <- NULL
  images[[section]] <- if (length(section_images)) section_images else NULL
  images
}

builder_coordinate_drafts_get <- function(drafts, dataset, section) {
  drafts <- drafts %||% list()
  drafts[[dataset]][[section]] %||% NULL
}

builder_coordinate_drafts_put <- function(
  drafts,
  dataset,
  snapshot_identity,
  section,
  spec,
  sequence = NULL,
  force = FALSE
) {
  scalar_text <- function(value) {
    is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
  }
  if (
    !scalar_text(dataset) ||
      !scalar_text(snapshot_identity) ||
      !scalar_text(section)
  ) {
    stop("Coordinate draft identity must be non-empty text.", call. = FALSE)
  }
  normalized <- .spx_coordinate_transform_spec_normalize(
    spec,
    context = "Coordinate draft"
  )
  current <- builder_coordinate_drafts_get(drafts, dataset, section)
  if (!isTRUE(force)) {
    if (
      !is.numeric(sequence) ||
        length(sequence) != 1L ||
        is.na(sequence) ||
        !is.finite(sequence) ||
        sequence < 1 ||
        sequence != floor(sequence)
    ) {
      stop(
        "Coordinate draft sequence must be a positive integer.",
        call. = FALSE
      )
    }
    if (!is.null(current) && sequence <= current$sequence) {
      return(list(drafts = drafts, accepted = FALSE, record = current))
    }
  } else {
    sequence <- current$sequence %||% 0
  }
  record <- list(
    dataset = dataset,
    snapshot_identity = snapshot_identity,
    section = section,
    spec = normalized,
    sequence = as.numeric(sequence)
  )
  drafts <- drafts %||% list()
  drafts[[dataset]][[section]] <- record
  list(drafts = drafts, accepted = TRUE, record = record)
}

builder_coordinate_drafts_drop <- function(drafts, dataset, section = NULL) {
  drafts <- drafts %||% list()
  if (is.null(section)) {
    drafts[[dataset]] <- NULL
    return(drafts)
  }
  drafts[[dataset]][[section]] <- NULL
  if (!length(drafts[[dataset]] %||% list())) {
    drafts[[dataset]] <- NULL
  }
  drafts
}

builder_coordinate_drafts_prune <- function(drafts, entries) {
  drafts <- drafts %||% list()
  identities <- stats::setNames(
    vapply(
      entries,
      function(entry) as.character(entry$snapshot_identity %||% "")[[1L]],
      character(1)
    ),
    vapply(entries, function(entry) entry$id, character(1))
  )
  removed <- character()
  for (dataset in names(drafts) %||% character()) {
    for (section in names(drafts[[dataset]]) %||% character()) {
      record <- drafts[[dataset]][[section]]
      if (
        !dataset %in% names(identities) ||
          !identical(record$snapshot_identity, identities[[dataset]])
      ) {
        removed <- c(removed, paste(dataset, section, sep = "::"))
        drafts <- builder_coordinate_drafts_drop(drafts, dataset, section)
      }
    }
  }
  list(drafts = drafts, removed = removed)
}

builder_coordinate_drafts_apply_entry <- function(
  entry,
  records,
  snapshot_identity
) {
  records <- records %||% list()
  transforms <- entry$settings$spatial_coordinate_transforms %||% list()
  if (is.null(names(transforms))) {
    transforms <- list()
  }
  images <- entry$settings$images %||% list()
  changed_sections <- character()
  for (section in names(records) %||% character()) {
    record <- records[[section]]
    if (
      !identical(record$dataset, entry$id) ||
        !identical(record$snapshot_identity, snapshot_identity) ||
        !identical(record$section, section)
    ) {
      next
    }
    spec <- .spx_coordinate_transform_spec_normalize(
      record$spec,
      context = paste0("Coordinate draft ", section)
    )
    previous <- transforms[[section]]
    identity <- identical(spec$rotation_degrees, 0) &&
      identical(spec$scale, 1)
    if (identity) {
      transforms[[section]] <- NULL
    } else {
      transforms[[section]] <- spec
    }
    if (!identical(previous, transforms[[section]])) {
      changed_sections <- c(changed_sections, section)
    }
  }
  entry$settings$spatial_coordinate_transforms <- transforms
  entry$settings$images <- images
  list(
    entry = entry,
    sections = changed_sections,
    changed = length(changed_sections) > 0L
  )
}

builder_alignment_apply_transform_to_matching_label <- function(
  images,
  source_section,
  label
) {
  images <- builder_image_collection_normalize(images)
  source <- images[[source_section]][[label]]
  if (is.null(source)) {
    return(images)
  }
  fields <- c(
    "dx",
    "dy",
    "scale",
    "rotation",
    "flip_x",
    "flip_y",
    "image_opacity",
    "point_opacity",
    "point_size"
  )
  for (section in setdiff(names(images), source_section)) {
    target <- images[[section]][[label]]
    if (is.null(target)) {
      next
    }
    for (field in fields) {
      target[[field]] <- source[[field]]
    }
    target$bounds <- builder_alignment_transform_bounds(
      target$base_bounds,
      target
    )
    images[[section]][[label]] <- target
  }
  images
}

builder_partition_alignments <- function(images) {
  spatial <- list()
  trekker <- NULL
  for (name in names(images %||% list())) {
    record <- builder_alignment_normalize(images[[name]], section_id = name)
    if (
      identical(record$section_kind %||% "", "trekker") ||
        identical(name, "trekker")
    ) {
      if (is.null(record)) {
        stop(
          "Trekker alignment must remain a single image record.",
          call. = FALSE
        )
      }
      trekker <- record
    } else {
      spatial[[name]] <- images[[name]]
    }
  }
  list(
    spatial = builder_image_collection_normalize(spatial),
    trekker = trekker
  )
}

#' Read an image file into an array png::writePNG can write back out.
builder_read_image <- function(path, filename = path) {
  ext <- tolower(tools::file_ext(filename))
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
  } else if (ext %in% c("tif", "tiff")) {
    return(list(
      error = paste0(
        "TIFF and OME-TIFF images are not decoded by the Builder; ",
        "convert the image to PNG or JPEG first."
      )
    ))
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
        "Could not read this image. Check that it is a valid ",
        "PNG or JPEG file."
      )
    ))
  }
  list(array = arr, width = dim(arr)[2], height = dim(arr)[1])
}

#' Decode the bounded PNG data URI retained in Builder state.
builder_read_image_uri <- function(uri) {
  prefix <- "data:image/png;base64,"
  if (
    !is.character(uri) ||
      length(uri) != 1L ||
      is.na(uri) ||
      !startsWith(uri, prefix)
  ) {
    return(list(
      error = "The saved tissue image is not a supported PNG payload."
    ))
  }
  if (
    !requireNamespace("base64enc", quietly = TRUE) ||
      !requireNamespace("png", quietly = TRUE)
  ) {
    return(list(error = "Reopening tissue images requires png and base64enc."))
  }
  decoded <- try(
    base64enc::base64decode(substring(uri, nchar(prefix) + 1L)),
    silent = TRUE
  )
  if (inherits(decoded, "try-error")) {
    return(list(error = "The saved tissue image could not be decoded."))
  }
  image <- try(png::readPNG(decoded), silent = TRUE)
  if (inherits(image, "try-error")) {
    return(list(error = "The saved tissue image could not be reopened."))
  }
  list(array = image, width = dim(image)[2L], height = dim(image)[1L])
}

builder_parse_image_uri <- function(uri) {
  if (
    !is.character(uri) ||
      length(uri) != 1L ||
      is.na(uri) ||
      !grepl("^data:image/[^;,]+;base64,", uri)
  ) {
    stop("Builder image URI is invalid.", call. = FALSE)
  }
  separator <- regexpr(",", uri, fixed = TRUE)[[1L]]
  header <- substring(uri, 6L, separator - 1L)
  mime <- sub(";base64$", "", header)
  payload <- substring(uri, separator + 1L)
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    stop("Materializing Builder images requires base64enc.", call. = FALSE)
  }
  bytes <- tryCatch(
    base64enc::base64decode(payload),
    error = function(error) NULL
  )
  if (is.null(bytes) || !is.raw(bytes)) {
    stop("Builder image URI could not be decoded.", call. = FALSE)
  }
  if (!mime %in% c("image/png", "image/jpeg")) {
    stop("Builder image URI has an unsupported MIME type.", call. = FALSE)
  }
  png_signature <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
  jpeg_signature <- as.raw(c(0xff, 0xd8, 0xff))
  valid_signature <- if (identical(mime, "image/png")) {
    length(bytes) >= length(png_signature) &&
      identical(bytes[seq_along(png_signature)], png_signature)
  } else {
    length(bytes) >= length(jpeg_signature) &&
      identical(bytes[seq_along(jpeg_signature)], jpeg_signature)
  }
  if (!valid_signature) {
    stop(
      "Builder image URI content does not match its MIME type.",
      call. = FALSE
    )
  }
  list(mime = mime, bytes = bytes)
}

builder_materialize_image_uri <- function(uri, path) {
  parsed <- builder_parse_image_uri(uri)
  expected_extension <- if (identical(parsed$mime, "image/png")) {
    "png"
  } else {
    c("jpg", "jpeg")
  }
  if (!tolower(tools::file_ext(path)) %in% expected_extension) {
    stop(
      "Builder image target extension does not match its MIME type.",
      call. = FALSE
    )
  }
  writeBin(parsed$bytes, path)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.builder_rotation_quarter_turn <- function(degrees) {
  normalized_degrees <- degrees %% 360
  quarter_turn <- round(normalized_degrees / 90)
  if (
    isTRUE(
      abs(normalized_degrees - quarter_turn * 90) < sqrt(.Machine$double.eps)
    )
  ) {
    return(quarter_turn %% 4L)
  }
  NA_integer_
}

builder_rotation_extent <- function(width, height, degrees) {
  valid_dimensions <- is.numeric(width) &&
    length(width) == 1L &&
    !is.na(width) &&
    is.finite(width) &&
    width >= 1 &&
    width <= .Machine$integer.max &&
    is.numeric(height) &&
    length(height) == 1L &&
    !is.na(height) &&
    is.finite(height) &&
    height >= 1 &&
    height <= .Machine$integer.max
  valid_rotation <- is.numeric(degrees) &&
    length(degrees) == 1L &&
    !is.na(degrees) &&
    is.finite(degrees)
  if (!valid_dimensions || !valid_rotation) {
    stop(
      "Rotation geometry requires finite positive dimensions.",
      call. = FALSE
    )
  }
  width <- as.integer(floor(width))
  height <- as.integer(floor(height))
  turn <- .builder_rotation_quarter_turn(degrees)
  if (!is.na(turn)) {
    if (turn %in% c(1L, 3L)) {
      return(c(width = height, height = width))
    }
    return(c(width = width, height = height))
  }

  theta <- degrees * pi / 180
  extent <- c(
    width = ceiling(abs(width * cos(theta)) + abs(height * sin(theta))),
    height = ceiling(abs(height * cos(theta)) + abs(width * sin(theta)))
  )
  if (any(extent > .Machine$integer.max)) {
    stop("Rotated image extent is too large.", call. = FALSE)
  }
  as.integer(extent) |>
    stats::setNames(c("width", "height"))
}

builder_rotation_plan <- function(width, height, degrees, max_edge) {
  valid_limit <- is.numeric(max_edge) &&
    length(max_edge) == 1L &&
    !is.na(max_edge) &&
    is.finite(max_edge) &&
    max_edge >= 1 &&
    max_edge <= .Machine$integer.max
  if (!valid_limit) {
    stop("Maximum rotation edge must be positive and finite.", call. = FALSE)
  }
  width <- as.integer(width)
  height <- as.integer(height)
  max_edge <- as.integer(floor(max_edge))
  full_extent <- builder_rotation_extent(width, height, degrees)
  scale <- min(1, max_edge / max(full_extent))
  input_dimensions <- pmax(
    1L,
    as.integer(floor(c(width = width, height = height) * scale))
  )
  names(input_dimensions) <- c("width", "height")
  output_dimensions <- builder_rotation_extent(
    input_dimensions[["width"]],
    input_dimensions[["height"]],
    degrees
  )
  while (
    max(output_dimensions) > max_edge &&
      any(input_dimensions > 1L)
  ) {
    input_dimensions[] <- pmax(1L, input_dimensions - 1L)
    output_dimensions <- builder_rotation_extent(
      input_dimensions[["width"]],
      input_dimensions[["height"]],
      degrees
    )
  }
  if (max(output_dimensions) > max_edge) {
    output_dimensions[] <- max_edge
  }

  list(
    source_dimensions = c(width = width, height = height),
    full_extent_dimensions = full_extent,
    input_max_edge = max(input_dimensions),
    input_dimensions = input_dimensions,
    output_dimensions = output_dimensions,
    prescaled = any(input_dimensions < c(width = width, height = height))
  )
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
  valid_rotation <- is.numeric(rotate) &&
    length(rotate) == 1L &&
    !is.na(rotate) &&
    is.finite(rotate)
  if (!valid_rotation) {
    return(list(error = "Image rotation must be one finite number."))
  }
  dimensions <- dim(arr)
  valid_dimensions <- length(dimensions) %in%
    c(2L, 3L) &&
    all(!is.na(dimensions)) &&
    all(dimensions > 0L)
  rotation_plan <- if (valid_dimensions) {
    tryCatch(
      builder_rotation_plan(
        dimensions[[2L]],
        dimensions[[1L]],
        rotate,
        max_px
      ),
      error = function(error) NULL
    )
  } else {
    NULL
  }
  normalization_edge <- if (is.null(rotation_plan)) {
    max_px
  } else {
    rotation_plan$input_max_edge
  }
  normalized <- builder_normalize_image(
    arr,
    max_display_px = normalization_edge,
    display_dimensions = if (is.null(rotation_plan)) {
      NULL
    } else {
      rotation_plan$input_dimensions
    }
  )
  if (!is.null(normalized$error)) {
    return(normalized)
  }
  arr <- normalized$array
  normalized$array <- NULL
  if (is.null(rotation_plan)) {
    return(list(error = "Image rotation geometry is invalid."))
  }
  if (isTRUE(flip_y) || isTRUE(flip_x)) {
    rows <- if (isTRUE(flip_y)) {
      rev(seq_len(dim(arr)[1L]))
    } else {
      seq_len(dim(arr)[1L])
    }
    columns <- if (isTRUE(flip_x)) {
      rev(seq_len(dim(arr)[2L]))
    } else {
      seq_len(dim(arr)[2L])
    }
    arr <- arr[rows, columns, , drop = FALSE]
  }
  arr <- .builder_rotate_rgba(
    arr,
    rotate,
    rotation_plan$output_dimensions
  )

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
  display_dimensions <- c(
    width = as.integer(dim(arr)[2L]),
    height = as.integer(dim(arr)[1L])
  )
  extent_dimensions <- rotation_plan$full_extent_dimensions
  list(
    uri = paste0(
      "data:image/png;base64,",
      base64enc::base64encode(tmp)
    ),
    bytes = file.size(tmp),
    width = display_dimensions[["width"]],
    height = display_dimensions[["height"]],
    source_width = normalized$source_width,
    source_height = normalized$source_height,
    extent_width = extent_dimensions[["width"]],
    extent_height = extent_dimensions[["height"]],
    display_width = display_dimensions[["width"]],
    display_height = display_dimensions[["height"]],
    source_dimensions = normalized$source_dimensions,
    extent_dimensions = extent_dimensions,
    display_dimensions = display_dimensions,
    source_channels = normalized$source_channels,
    source_channel_kind = normalized$source_channel_kind,
    display_channels = 4L,
    display_channel_kind = "rgba",
    channel_kind = "rgba"
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
  image_width <- if (!is.null(image$extent_width)) {
    image$extent_width
  } else if (!is.null(image$source_width)) {
    image$source_width
  } else {
    image$width
  }
  image_height <- if (!is.null(image$extent_height)) {
    image$extent_height
  } else if (!is.null(image$source_height)) {
    image$source_height
  } else {
    image$height
  }
  if (identical(mode, "pixels")) {
    return(list(xmin = 0, xmax = image_width, ymin = 0, ymax = image_height))
  }
  if (identical(mode, "physical")) {
    if (!is.finite(um_per_px) || um_per_px <= 0) {
      return(list(error = "Physical units per pixel must be positive."))
    }
    return(list(
      xmin = 0,
      xmax = image_width * um_per_px,
      ymin = 0,
      ymax = image_height * um_per_px
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
      source_width = picture$source_width,
      source_height = picture$source_height,
      extent_width = picture$extent_width,
      extent_height = picture$extent_height,
      display_width = picture$display_width,
      display_height = picture$display_height,
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
    sd <- builder_attach_spatial_image(sd, images[[nm]])
    if (is.null(sd)) {
      return(list(
        error = paste0(
          "The configured image for spatial section `",
          nm,
          "` is invalid."
        )
      ))
    }
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
  trekker = NULL,
  trekker_alignment = NULL
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
      records <- images[[name]]
      if (!is.null(builder_alignment_normalize(records, section_id = name))) {
        records <- list(records)
      }
      record_labels <- names(records)
      for (record_index in seq_along(records)) {
        record <- records[[record_index]]
        record_label <- if (!is.null(record_labels)) {
          record_labels[[record_index]]
        } else {
          NULL
        }
        spatial <- builder_attach_spatial_image(
          spatial,
          record,
          label = record_label,
          replace_managed = identical(record_index, 1L)
        )
        if (is.null(spatial)) {
          return(list(
            error = paste0(
              "The configured image for spatial section `",
              name,
              "` is invalid."
            )
          ))
        }
      }
      crb$addSpatialData(name, spatial)
    }
  }

  trekker_applied <- FALSE
  if (!is.null(trekker) && length(trekker)) {
    alignment <- builder_alignment_normalize(
      trekker_alignment,
      section_id = "trekker",
      section_kind = "trekker"
    )
    if (!is.null(alignment)) {
      trekker$histology_image <- alignment$uri
      trekker$histology_image_bounds <- alignment$bounds
      trekker$histology_alignment <- builder_alignment_payload(alignment)
    }
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

builder_attach_external_spatial_appearance <- function(crb_path, images) {
  collection <- builder_image_collection_normalize(images)
  if (!length(collection)) {
    return(list(applied = character()))
  }
  crb <- try(readRDS(crb_path), silent = TRUE)
  if (inherits(crb, "try-error")) {
    return(list(error = "The exported .crb could not be read back."))
  }
  available <- try(crb$availableSpatial(), silent = TRUE)
  if (inherits(available, "try-error")) {
    return(list(error = "The .crb contains no spatial data."))
  }
  applied <- intersect(names(collection), available)
  for (section_id in applied) {
    spatial <- crb$getSpatialData(section_id)
    previous <- spatial$histology_alignment %||% list()
    embedded <- spatial$histology_images %||% list()
    if (
      isTRUE(previous$builder_managed) && previous$source %in% names(embedded)
    ) {
      embedded[[previous$source]] <- NULL
    }
    active_label <- utils::tail(names(collection[[section_id]]), 1L)
    active <- collection[[section_id]][[active_label]]
    alignment <- builder_alignment_payload(active)
    alignment$source <- active_label
    spatial$histology_images <- embedded
    spatial$histology_image <- NULL
    spatial$histology_image_bounds <- NULL
    spatial$histology_alignment <- alignment
    crb$addSpatialData(section_id, spatial)
  }
  temporary <- tempfile(
    paste0(".", basename(crb_path), "-external-"),
    tmpdir = dirname(crb_path)
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  written <- try(saveRDS(crb, temporary, compress = "xz"), silent = TRUE)
  if (inherits(written, "try-error") || !file.exists(temporary)) {
    return(list(error = "Could not write external-image CRB appearance."))
  }
  if (!file.rename(temporary, crb_path)) {
    return(list(error = "Could not replace the external-image CRB."))
  }
  list(applied = applied)
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
  contract <- builder_spatial_contract(object, image = image)
  list(contract$coordinates$x, contract$coordinates$y)
}

## Rotate an already-normalized RGBA array without another full-size copy.
.builder_rotate_rgba <- function(arr, degrees, output_dimensions) {
  turn <- .builder_rotation_quarter_turn(degrees)
  if (!is.na(turn)) {
    if (turn == 0L) {
      return(arr)
    }
    h <- dim(arr)[1L]
    w <- dim(arr)[2L]
    nh <- output_dimensions[["height"]]
    nw <- output_dimensions[["width"]]
    out <- array(0, dim = c(nh, nw, 4L))
    source_plane_size <- h * w
    columns <- seq_len(nw)
    reverse_columns <- rev(columns)
    for (row in seq_len(nh)) {
      source_index <- if (turn == 1L) {
        columns + (w - row) * h
      } else if (turn == 2L) {
        (h - row + 1L) + (reverse_columns - 1L) * h
      } else {
        reverse_columns + (row - 1L) * h
      }
      for (channel in seq_len(4L)) {
        out[row, columns, channel] <- arr[
          source_index + (channel - 1L) * source_plane_size
        ]
      }
    }
    return(out)
  }
  h <- dim(arr)[1]
  w <- dim(arr)[2]
  theta <- degrees * pi / 180
  nh <- output_dimensions[["height"]]
  nw <- output_dimensions[["width"]]

  ## Alpha channel, so the corners the source does not cover are transparent
  ## rather than black.
  out <- array(0, dim = c(nh, nw, 4L))
  continuous_width <- abs(w * cos(theta)) + abs(h * sin(theta))
  continuous_height <- abs(h * cos(theta)) + abs(w * sin(theta))
  dx <- ((seq_len(nw) - 0.5) / nw - 0.5) * continuous_width
  source_plane_size <- h * w
  for (row in seq_len(nh)) {
    dy <- ((row - 0.5) / nh - 0.5) * continuous_height
    source_x <- dx * cos(theta) - dy * sin(theta)
    source_y <- dx * sin(theta) + dy * cos(theta)
    inside <- source_x >= -w / 2 &
      source_x < w / 2 &
      source_y >= -h / 2 &
      source_y < h / 2
    if (!any(inside)) {
      next
    }
    columns <- which(inside)
    sx <- pmin(w, pmax(1L, floor(source_x[inside] + w / 2) + 1L))
    sy <- pmin(h, pmax(1L, floor(source_y[inside] + h / 2) + 1L))
    source_index <- (sx - 1L) * h + sy
    for (channel in seq_len(4L)) {
      out[row, columns, channel] <- arr[
        source_index + (channel - 1L) * source_plane_size
      ]
    }
  }
  out
}

#' Rotate an image array by an arbitrary angle.
#'
#' Nearest-neighbour: this is a background behind points, and the alternative
#' is pulling in an imaging package for a picture nobody will zoom into.
#' The canvas grows so nothing is cropped, and the new corners are transparent
#' where the source does not reach.
builder_rotate_array <- function(
  arr,
  degrees,
  max_edge = max(dim(arr)[1:2])
) {
  plan <- builder_rotation_plan(
    width = dim(arr)[2L],
    height = dim(arr)[1L],
    degrees = degrees,
    max_edge = max_edge
  )
  normalized <- builder_normalize_image(
    arr,
    max_display_px = plan$input_max_edge,
    display_dimensions = plan$input_dimensions
  )
  if (!is.null(normalized$error)) {
    stop(normalized$error, call. = FALSE)
  }
  arr <- normalized$array
  normalized$array <- NULL
  .builder_rotate_rgba(arr, degrees, plan$output_dimensions)
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
