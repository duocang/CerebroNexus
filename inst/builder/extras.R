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

#' List the tables available in one uploaded file without reading their data.
builder_table_inventory <- function(path, filename = path) {
  if (!file.exists(path) || dir.exists(path)) {
    return(list(error = "File not found."))
  }
  ext <- tolower(tools::file_ext(filename))
  if (ext %in% c("csv", "tsv", "txt")) {
    name <- builder_table_default_name(filename)
    return(stats::setNames(
      list(list(
        name = name,
        workbook_name = basename(filename),
        sheet_name = name,
        display_name = name,
        source_path = path
      )),
      name
    ))
  }
  if (!ext %in% c("xls", "xlsx", "xlsm")) {
    return(list(
      error = paste0(
        "Supported table formats are CSV, TSV, TXT, XLS, XLSX and XLSM, not .",
        ext,
        "."
      )
    ))
  }
  sheets <- suppressWarnings(try(readxl::excel_sheets(path), silent = TRUE))
  if (inherits(sheets, "try-error") || !length(sheets)) {
    return(list(error = "Could not read this Excel workbook."))
  }
  workbook <- builder_table_default_name(filename)
  records <- lapply(sheets, function(sheet) {
    list(
      name = paste(workbook, sheet, sep = " · "),
      workbook_name = basename(filename),
      sheet_name = sheet,
      display_name = sheet,
      sheet = sheet,
      source_path = path
    )
  })
  names(records) <- sheets
  records
}

builder_table_inventory_metadata <- function(path, filename = path) {
  records <- builder_table_inventory(path, filename)
  if (!is.null(records$error)) {
    return(records)
  }
  lapply(records, function(record) {
    record$source_path <- NULL
    record
  })
}

#' Read one selected table only when it becomes part of a build.
builder_read_table_source <- function(record) {
  if (is.data.frame(record$table)) {
    return(record)
  }
  path <- record$source_path %||% ""
  filename <- record$file_name %||% record$workbook_name %||% path
  if (!file.exists(path)) {
    return(list(error = paste0(filename, ": File not found.")))
  }
  extension <- tolower(tools::file_ext(filename))
  table <- suppressWarnings(try(
    if (extension %in% c("csv", "tsv", "txt")) {
      utils::read.table(
        path,
        header = TRUE,
        sep = if (extension == "csv") "," else "\t",
        stringsAsFactors = FALSE,
        check.names = FALSE,
        comment.char = ""
      )
    } else if (extension %in% c("xls", "xlsx", "xlsm")) {
      readxl::read_excel(path, sheet = record$sheet_name %||% record$sheet)
    } else {
      stop("Unsupported table format.")
    },
    silent = TRUE
  ))
  if (inherits(table, "try-error") || !is.data.frame(table)) {
    return(list(
      error = paste0(
        filename,
        ": ",
        record$sheet_name %||%
          record$sheet %||%
          record$display_name %||%
          "Table",
        " could not be read."
      )
    ))
  }
  if (!nrow(table) || !ncol(table)) {
    record$empty <- TRUE
    return(record)
  }
  record$table <- as.data.frame(table, stringsAsFactors = FALSE)
  record
}

builder_materialize_tables <- function(tables) {
  records <- lapply(tables %||% list(), builder_read_table_source)
  errors <- vapply(records, function(record) !is.null(record$error), logical(1))
  if (any(errors)) {
    stop(records[[which(errors)[[1L]]]]$error, call. = FALSE)
  }
  records <- Filter(function(record) !isTRUE(record$empty), records)
  if (!length(records)) {
    stop("No selected attachments contain tabular data.", call. = FALSE)
  }
  records
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
  table_index <- object@misc$extra_material$table_index
  if (is.null(table_index)) {
    table_index <- list()
  }
  for (t in tables) {
    existing[[t$name]] <- t$table
    table_index[[t$name]] <- list(
      workbook_name = t$workbook_name %||% basename(t$file_name %||% ""),
      file_name = t$file_name %||% "",
      sheet_name = t$sheet_name %||% t$sheet %||% t$name,
      display_name = t$display_name %||% t$sheet_name %||% t$sheet %||% t$name
    )
  }
  object@misc$extra_material$tables <- existing
  object@misc$extra_material$table_index <- table_index
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

#' Expand a coordinate viewport to its displayed extent after rotation.
builder_alignment_rotated_bounds <- function(bounds, rotation = 0) {
  if (!.builder_alignment_valid_bounds(bounds)) {
    stop("Alignment requires finite, non-empty physical bounds.", call. = FALSE)
  }
  rotation <- suppressWarnings(as.numeric(rotation))
  if (length(rotation) != 1L || is.na(rotation) || !is.finite(rotation)) {
    stop("Alignment rotation must be finite.", call. = FALSE)
  }
  radians <- rotation * pi / 180
  width <- bounds$xmax - bounds$xmin
  height <- bounds$ymax - bounds$ymin
  extent_width <- abs(width * cos(radians)) + abs(height * sin(radians))
  extent_height <- abs(width * sin(radians)) + abs(height * cos(radians))
  centre_x <- (bounds$xmin + bounds$xmax) / 2
  centre_y <- (bounds$ymin + bounds$ymax) / 2
  list(
    xmin = centre_x - extent_width / 2,
    xmax = centre_x + extent_width / 2,
    ymin = centre_y - extent_height / 2,
    ymax = centre_y + extent_height / 2
  )
}

#' Fit the original image inside the physical coordinate viewport.
#'
#' The image is centred, aspect-preserving, and fills the available viewport.
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
  available_width <- bounds$xmax - bounds$xmin
  available_height <- bounds$ymax - bounds$ymin
  fit <- min(
    available_width / image_dimensions[[1L]],
    available_height / image_dimensions[[2L]]
  )
  width <- image_dimensions[[1L]] * fit
  height <- image_dimensions[[2L]] * fit
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
  parameters$dx <- round(parameters$dx)
  parameters$dy <- round(parameters$dy)
  if (
    parameters$scale < 0 ||
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

#' Derive alignment slider ranges without discarding a restored transform.
#'
#' During project hydration the saved alignment is available before a fresh
#' spatial preview has reported its coordinate bounds.  Slider updates must
#' therefore include both the known coordinate span and the restored value;
#' otherwise Shiny clamps an out-of-range saved offset back to the temporary
#' default range.
builder_alignment_control_ranges <- function(record = NULL, bounds = NULL) {
  parameters <- .builder_alignment_parameters(record %||% list())
  saved_bounds <- if (is.list(record)) {
    record$base_bounds %||% record$bounds
  } else {
    NULL
  }
  effective_bounds <- if (.builder_alignment_valid_bounds(bounds)) {
    bounds
  } else if (.builder_alignment_valid_bounds(saved_bounds)) {
    saved_bounds
  } else {
    NULL
  }
  span_x <- if (is.null(effective_bounds)) {
    1
  } else {
    effective_bounds$xmax - effective_bounds$xmin
  }
  span_y <- if (is.null(effective_bounds)) {
    1
  } else {
    effective_bounds$ymax - effective_bounds$ymin
  }
  nice <- function(value) max(signif(value, 2), .Machine$double.eps)
  x_limit <- nice(max(1, abs(parameters$dx), abs(span_x)))
  y_limit <- nice(max(1, abs(parameters$dy), abs(span_y)))
  list(
    dx = list(
      min = -x_limit,
      max = x_limit,
      step = 10
    ),
    dy = list(
      min = -y_limit,
      max = y_limit,
      step = 10
    )
  )
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

#' Center an existing image on the active spatial viewport.
builder_alignment_center <- function(record, bounds) {
  if (
    !is.list(record) ||
      !.builder_alignment_valid_bounds(record$base_bounds) ||
      !.builder_alignment_valid_bounds(bounds)
  ) {
    stop(
      "Alignment centering requires valid image and viewport bounds.",
      call. = FALSE
    )
  }
  parameters <- .builder_alignment_parameters(record)
  parameters$dx <- round(
    (bounds$xmin +
      bounds$xmax -
      record$base_bounds$xmin -
      record$base_bounds$xmax) /
      2
  )
  parameters$dy <- round(
    (bounds$ymin +
      bounds$ymax -
      record$base_bounds$ymin -
      record$base_bounds$ymax) /
      2
  )
  record[names(parameters)] <- parameters
  record$bounds <- builder_alignment_transform_bounds(
    builder_alignment_oriented_bounds(record$base_bounds, record),
    parameters
  )
  record
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

.builder_alignment_roi_scope <- function(record) {
  fields <- c("roi_field", "roi_value")
  present <- fields %in% names(record)
  if (!any(present)) {
    return(list())
  }
  if (!all(present)) {
    stop(
      "ROI image scope requires both roi_field and roi_value.",
      call. = FALSE
    )
  }
  values <- record[fields]
  valid <- vapply(
    values,
    function(value) {
      is.character(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        nzchar(trimws(value))
    },
    logical(1)
  )
  if (!all(valid)) {
    stop("ROI image scope values must be non-empty strings.", call. = FALSE)
  }
  lapply(values, trimws)
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
  normalized[c("roi_field", "roi_value")] <- NULL
  scope <- .builder_alignment_roi_scope(record)
  normalized[names(scope)] <- scope
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
      "source_content_md5",
      "image_label",
      "roi_field",
      "roi_value",
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

#' The small alignment contract written to a generated Viewer payload.
builder_alignment_payload <- function(record) {
  normalized <- builder_alignment_normalize(record)
  if (is.null(normalized)) {
    return(NULL)
  }
  payload <- list(
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
  scope <- intersect(c("roi_field", "roi_value"), names(normalized))
  payload[scope] <- normalized[scope]
  payload
}

#' Convert one Builder alignment to the canonical multi-image leaf contract.
builder_histology_image_payload <- function(record) {
  normalized <- builder_alignment_normalize(record)
  if (is.null(normalized)) {
    return(NULL)
  }
  required <- c("xmin", "xmax", "ymin", "ymax")
  bounds <- stats::setNames(
    as.numeric(unlist(normalized$base_bounds[required], use.names = FALSE)),
    required
  )
  payload <- list(
    histology_image = normalized$source_uri,
    histology_image_bounds = bounds
  )
  scope <- intersect(c("roi_field", "roi_value"), names(normalized))
  payload[scope] <- normalized[scope]
  payload$image_label <- normalized$image_label %||%
    basename(as.character(normalized$source$name %||% "Tissue image"))
  payload$histology_alignment <- builder_alignment_payload(normalized)
  payload
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
  payload$image_label <- normalized$image_label %||% label
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
      legacy$image_label <- label
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
      display_label <- record$image_label %||% label
      if (
        !is.character(display_label) ||
          length(display_label) != 1L ||
          is.na(display_label) ||
          !nzchar(trimws(display_label))
      ) {
        stop("Spatial image labels must be non-empty.", call. = FALSE)
      }
      record$image_label <- trimws(display_label)
      record
    })
    names(records) <- labels
    scopes <- vapply(
      records,
      function(record) {
        as.character(record$roi_value %||% "")[[1L]]
      },
      character(1)
    )
    display_labels <- vapply(records, `[[`, character(1), "image_label")
    if (anyDuplicated(data.frame(scopes, display_labels))) {
      stop(
        "Spatial image labels must be unique within the same ROI.",
        call. = FALSE
      )
    }
    normalized[[section_id]] <- records
  }
  normalized
}

builder_image_collection_choices <- function(images, section, roi = "") {
  images <- builder_image_collection_normalize(images)
  records <- images[[section]] %||% list()
  keys <- names(records) %||% character()
  roi <- as.character(roi %||% "")[[1L]]
  scopes <- vapply(
    records,
    function(record) {
      as.character(record$roi_value %||% "")[[1L]]
    },
    character(1)
  )
  keys <- keys[scopes == roi]
  stats::setNames(
    keys,
    vapply(
      keys,
      function(key) {
        records[[key]]$image_label %||% key
      },
      character(1)
    )
  )
}

builder_image_collection_flatten <- function(images) {
  images <- builder_image_collection_normalize(images)
  unlist(
    lapply(names(images), function(section_id) {
      lapply(names(images[[section_id]]), function(image_label) {
        record <- images[[section_id]][[image_label]]
        c(
          list(
            section_id = section_id,
            image_key = image_label,
            image_label = record$image_label %||% image_label
          ),
          record[setdiff(names(record), "image_label")]
        )
      })
    }),
    recursive = FALSE,
    use.names = FALSE
  )
}

builder_image_collection_rename <- function(images, section, from, to) {
  images <- builder_image_collection_normalize(images)
  to <- trimws(as.character(to %||% ""))
  section_images <- images[[section]] %||% list()
  if (!from %in% names(section_images)) {
    stop("The spatial image to rename does not exist.", call. = FALSE)
  }
  record <- section_images[[from]]
  scope <- as.character(record$roi_value %||% "")[[1L]]
  other_keys <- setdiff(names(section_images), from)
  duplicate <- any(vapply(
    other_keys,
    function(key) {
      other <- section_images[[key]]
      identical(as.character(other$roi_value %||% "")[[1L]], scope) &&
        identical(other$image_label %||% key, to)
    },
    logical(1)
  ))
  if (!nzchar(to) || duplicate) {
    stop("Spatial image labels must be non-empty and unique.", call. = FALSE)
  }
  if (identical(record$image_label %||% from, to)) {
    attr(images, "renamed_image_key") <- from
    return(images)
  }
  position <- match(from, names(section_images))
  key <- if (!to %in% other_keys) {
    to
  } else {
    utils::tail(make.unique(c(other_keys, to)), 1L)
  }
  record$image_label <- to
  section_images[[position]] <- record
  names(section_images)[[position]] <- key
  images[[section]] <- section_images
  attr(images, "renamed_image_key") <- key
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

builder_roi_drafts_drop <- function(drafts, dataset, section = NULL) {
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

builder_roi_drafts_prune <- function(drafts, entries) {
  drafts <- drafts %||% list()
  identities <- stats::setNames(
    vapply(
      entries,
      function(entry) as.character(entry$snapshot_identity %||% "")[[1L]],
      character(1)
    ),
    vapply(entries, function(entry) entry$id, character(1))
  )
  for (dataset in names(drafts) %||% character()) {
    for (section in names(drafts[[dataset]]) %||% character()) {
      records <- drafts[[dataset]][[section]] %||% list()
      valid <- vapply(
        records,
        function(record) {
          dataset %in%
            names(identities) &&
            identical(record$snapshot_identity, identities[[dataset]])
        },
        logical(1)
      )
      drafts[[dataset]][[section]] <- records[valid]
      if (!length(drafts[[dataset]][[section]])) {
        drafts <- builder_roi_drafts_drop(drafts, dataset, section)
      }
    }
  }
  drafts
}

builder_roi_drafts_apply_entry <- function(
  entry,
  coordinate_records,
  appearance_records,
  snapshot_identity
) {
  settings <- entry$settings$spatial_roi_settings %||% list()
  before <- settings
  sections <- union(
    names(coordinate_records) %||% character(),
    names(appearance_records) %||% character()
  )
  changed_sections <- character()
  for (section in sections) {
    coordinates <- coordinate_records[[section]] %||% list()
    appearances <- appearance_records[[section]] %||% list()
    rois <- union(
      names(coordinates) %||% character(),
      names(appearances) %||% character()
    )
    for (roi in rois) {
      coordinate <- coordinates[[roi]]
      appearance <- appearances[[roi]]
      valid_record <- function(record) {
        is.list(record) &&
          identical(record$dataset, entry$id) &&
          identical(record$snapshot_identity, snapshot_identity) &&
          identical(record$section, section) &&
          identical(record$roi, roi)
      }
      leaf <- settings[[section]][[roi]] %||%
        list(
          rotation_degrees = 0,
          point_opacity = builder_alignment_defaults()$point_opacity,
          point_size = builder_alignment_defaults()$point_size
        )
      if (valid_record(coordinate)) {
        spec <- .spx_coordinate_transform_spec_normalize(
          coordinate$spec,
          context = paste0("ROI coordinate draft ", section, "/", roi)
        )
        leaf$rotation_degrees <- spec$rotation_degrees
      }
      if (valid_record(appearance)) {
        leaf$point_opacity <- appearance$point_opacity
        leaf$point_size <- appearance$point_size
      }
      leaf <- .builder_state_spatial_roi_leaf(
        leaf,
        paste0(section, "/", roi)
      )
      defaults <- builder_alignment_defaults()
      if (
        identical(leaf$rotation_degrees, 0) &&
          identical(leaf$point_opacity, defaults$point_opacity) &&
          identical(leaf$point_size, defaults$point_size)
      ) {
        settings[[section]][[roi]] <- NULL
      } else {
        settings[[section]][[roi]] <- leaf
      }
    }
    if (!length(settings[[section]] %||% list())) {
      settings[[section]] <- NULL
    }
    if (!identical(before[[section]], settings[[section]])) {
      changed_sections <- c(changed_sections, section)
    }
  }
  entry$settings$spatial_roi_settings <- settings
  list(
    entry = entry,
    sections = changed_sections,
    changed = length(changed_sections) > 0L
  )
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

BUILDER_IMAGE_MAX_ENCODED_BYTES <- 1024^3

.builder_image_uint32_be <- function(bytes) {
  if (length(bytes) != 4L) {
    return(NA_real_)
  }
  values <- as.numeric(as.integer(bytes))
  sum(values * c(256^3, 256^2, 256, 1))
}

.builder_png_dimensions <- function(bytes) {
  signature <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
  if (
    length(bytes) < 24L ||
      !identical(bytes[seq_len(8L)], signature) ||
      !identical(bytes[9:12], as.raw(c(0x00, 0x00, 0x00, 0x0d))) ||
      !identical(bytes[13:16], charToRaw("IHDR"))
  ) {
    return(NULL)
  }
  width <- .builder_image_uint32_be(bytes[17:20])
  height <- .builder_image_uint32_be(bytes[21:24])
  if (!all(is.finite(c(width, height))) || width < 1 || height < 1) {
    return(NULL)
  }
  if (width <= .Machine$integer.max && height <= .Machine$integer.max) {
    return(c(width = as.integer(width), height = as.integer(height)))
  }
  c(width = width, height = height)
}

.builder_jpeg_dimensions <- function(bytes) {
  unsafe <- function() {
    list(
      error = "JPEG metadata could not be read. Check that the file is valid."
    )
  }
  if (
    length(bytes) < 2L ||
      !identical(bytes[1:2], as.raw(c(0xff, 0xd8)))
  ) {
    return(NULL)
  }
  value_at <- function(index) as.integer(bytes[[index]])
  total <- length(bytes)
  start_of_frame <- c(
    0xc0,
    0xc1,
    0xc2,
    0xc3,
    0xc5,
    0xc6,
    0xc7,
    0xc9,
    0xca,
    0xcb,
    0xcd,
    0xce,
    0xcf
  )
  standalone <- c(0x01, 0xd8, 0xd9, 0xd0:0xd7)
  cursor <- 3L
  while (cursor <= total) {
    while (cursor <= total && value_at(cursor) != 0xffL) {
      cursor <- cursor + 1L
    }
    if (cursor > total) {
      return(unsafe())
    }
    marker_index <- cursor + 1L
    while (marker_index <= total && value_at(marker_index) == 0xffL) {
      marker_index <- marker_index + 1L
    }
    if (marker_index > total) {
      return(unsafe())
    }
    marker <- value_at(marker_index)
    if (marker == 0L) {
      cursor <- marker_index + 1L
      next
    }
    if (marker %in% standalone) {
      cursor <- marker_index + 1L
      next
    }
    if (marker == 0xdaL || marker_index + 2L > total) {
      return(unsafe())
    }
    segment_length <- value_at(marker_index + 1L) *
      256 +
      value_at(marker_index + 2L)
    if (
      !is.finite(segment_length) ||
        segment_length < 2L ||
        marker_index + segment_length > total
    ) {
      return(unsafe())
    }
    if (marker %in% start_of_frame) {
      if (segment_length < 7L || marker_index + 7L > total) {
        return(unsafe())
      }
      height <- value_at(marker_index + 4L) * 256 + value_at(marker_index + 5L)
      width <- value_at(marker_index + 6L) * 256 + value_at(marker_index + 7L)
      if (!all(is.finite(c(width, height))) || width < 1 || height < 1) {
        return(unsafe())
      }
      return(c(width = as.integer(width), height = as.integer(height)))
    }
    cursor <- marker_index + segment_length + 1L
  }
  unsafe()
}

#' Read PNG/JPEG dimensions without decoding the raster.
builder_image_file_dimensions <- function(path, filename = path) {
  ext <- tolower(tools::file_ext(filename))
  if (!file.exists(path) || !ext %in% c("png", "jpg", "jpeg")) {
    return(NULL)
  }
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)

  if (identical(ext, "png")) {
    header <- readBin(connection, what = "raw", n = 24L)
    return(.builder_png_dimensions(header))
  }
  header <- readBin(connection, what = "raw", n = file.size(path))
  .builder_jpeg_dimensions(header)
}

#' Read PNG/JPEG metadata while retaining the original bytes.
builder_read_image <- function(
  path,
  filename = path,
  max_encoded_bytes = BUILDER_IMAGE_MAX_ENCODED_BYTES
) {
  valid_budget <- is.numeric(max_encoded_bytes) &&
    length(max_encoded_bytes) == 1L &&
    !is.na(max_encoded_bytes) &&
    is.finite(max_encoded_bytes) &&
    max_encoded_bytes >= 1
  if (!valid_budget) {
    return(list(error = "The image file-size limit is invalid."))
  }
  ext <- tolower(tools::file_ext(filename))
  if (ext %in% c("png", "jpg", "jpeg")) {
    encoded_bytes <- suppressWarnings(as.numeric(file.info(path)$size[[1L]]))
    if (!is.finite(encoded_bytes)) {
      return(list(error = "Could not read this image."))
    }
    if (encoded_bytes > max_encoded_bytes) {
      return(list(error = "This image is larger than the 1 GiB file limit."))
    }
    dimensions <- builder_image_file_dimensions(path, filename)
    if (
      is.list(dimensions) &&
        is.character(dimensions$error %||% NULL) &&
        length(dimensions$error) == 1L &&
        !is.na(dimensions$error) &&
        nzchar(dimensions$error)
    ) {
      return(list(error = dimensions$error))
    }
    if (is.null(dimensions)) {
      return(list(
        error = paste0(
          "Could not read this image. Check that it is a valid ",
          "PNG or JPEG file."
        )
      ))
    }
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
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    return(list(error = "Reading tissue images requires base64enc."))
  }
  mime <- if (identical(ext, "png")) "image/png" else "image/jpeg"
  width <- unname(dimensions[["width"]])
  height <- unname(dimensions[["height"]])
  source_uri <- paste0(
    "data:",
    mime,
    ";base64,",
    base64enc::base64encode(path)
  )
  list(
    mime = mime,
    source_uri = source_uri,
    uri = source_uri,
    source_content_md5 = unname(as.character(tools::md5sum(path))),
    bytes = unname(file.size(path)),
    width = width,
    height = height,
    source_width = width,
    source_height = height,
    extent_width = width,
    extent_height = height,
    display_width = width,
    display_height = height,
    source_dimensions = c(width = width, height = height)
  )
}

#' Read source-image metadata retained in Builder state.
builder_read_image_uri <- function(
  uri,
  max_encoded_bytes = BUILDER_IMAGE_MAX_ENCODED_BYTES
) {
  if (
    !is.character(uri) ||
      length(uri) != 1L ||
      is.na(uri) ||
      !grepl("^data:image/(png|jpeg);base64,", uri)
  ) {
    return(list(
      error = "The saved tissue image is not a supported PNG or JPEG payload."
    ))
  }
  valid_limits <- is.numeric(max_encoded_bytes) &&
    length(max_encoded_bytes) == 1L &&
    !is.na(max_encoded_bytes) &&
    is.finite(max_encoded_bytes) &&
    max_encoded_bytes >= 1
  if (!valid_limits) {
    return(list(error = "The saved image file-size limit is invalid."))
  }
  payload <- substring(uri, regexpr(",", uri, fixed = TRUE)[[1L]] + 1L)
  padding <- if (endsWith(payload, "==")) {
    2L
  } else if (endsWith(payload, "=")) {
    1L
  } else {
    0L
  }
  decoded_size <- nchar(payload, type = "bytes") * 3 / 4 - padding
  if (decoded_size > max_encoded_bytes) {
    return(list(
      error = "The saved tissue image is larger than the 1 GiB file limit."
    ))
  }
  parsed <- tryCatch(
    builder_parse_image_uri(uri),
    error = function(error) NULL
  )
  if (is.null(parsed)) {
    return(list(error = "The saved tissue image could not be decoded."))
  }
  dimensions <- if (identical(parsed$mime, "image/png")) {
    .builder_png_dimensions(parsed$bytes)
  } else {
    .builder_jpeg_dimensions(parsed$bytes)
  }
  if (is.list(dimensions) || is.null(dimensions)) {
    return(list(
      error = "The saved tissue image has invalid or unsafe image metadata."
    ))
  }
  list(
    width = unname(dimensions[["width"]]),
    height = unname(dimensions[["height"]])
  )
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

#' Attach every post-export payload with one atomic CRB replacement.
#'
#' Histology and Trekker both require a read-modify-write after
#' `exportFromSeurat()`. Doing them separately writes the same large CRB twice
#' and leaves a partially augmented file when the second write fails. This
#' helper validates and applies both in memory, writes a sibling temporary file,
#' then replaces the original with rollback.
.builder_apply_external_spatial_appearance <- function(crb, images) {
  collection <- builder_image_collection_normalize(images)
  if (!length(collection)) {
    return(list(object = crb, applied = character()))
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
  list(object = crb, applied = applied)
}

builder_attach_crb_extras <- function(
  crb_path,
  images = list(),
  trekker = NULL,
  trekker_alignment = NULL,
  external_images = list()
) {
  if (
    !length(images) &&
      (is.null(trekker) || !length(trekker)) &&
      !length(external_images)
  ) {
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
      trekker$histology_image <- alignment$source_uri
      trekker$histology_image_bounds <- alignment$base_bounds
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

  external <- .builder_apply_external_spatial_appearance(crb, external_images)
  if (!is.null(external$error)) {
    return(list(error = external$error))
  }
  crb <- external$object

  temporary <- tempfile(
    paste0(".", basename(crb_path), "-"),
    tmpdir = dirname(crb_path)
  )
  backup <- tempfile(
    paste0(".", basename(crb_path), "-backup-"),
    tmpdir = dirname(crb_path)
  )
  on.exit(unlink(c(temporary, backup), force = TRUE), add = TRUE)

  written <- try(saveRDS(crb, temporary, compress = "gzip"), silent = TRUE)
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
  if (!length(builder_image_collection_normalize(images))) {
    return(list(applied = character()))
  }
  crb <- try(readRDS(crb_path), silent = TRUE)
  if (inherits(crb, "try-error")) {
    return(list(error = "The exported .crb could not be read back."))
  }
  appearance <- .builder_apply_external_spatial_appearance(crb, images)
  if (!is.null(appearance$error)) {
    return(list(error = appearance$error))
  }
  crb <- appearance$object
  temporary <- tempfile(
    paste0(".", basename(crb_path), "-external-"),
    tmpdir = dirname(crb_path)
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  written <- try(saveRDS(crb, temporary, compress = "gzip"), silent = TRUE)
  if (inherits(written, "try-error") || !file.exists(temporary)) {
    return(list(error = "Could not write external-image CRB appearance."))
  }
  if (!file.rename(temporary, crb_path)) {
    return(list(error = "Could not replace the external-image CRB."))
  }
  list(applied = appearance$applied)
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
