.trekker_categories <- c(
  "location",
  "positioning",
  "metrics",
  "cluster_markers",
  "moran",
  "report"
)

.trekker_patterns <- c(
  location = "Location_ConfPositionedNuclei.*\\.csv$",
  positioning = "^coords_.*\\.txt$",
  metrics = "summary_metrics.*\\.csv$",
  cluster_markers = "variable_features_clusters.*\\.csv$",
  moran = "variable_features_spatial_moransi.*\\.txt$",
  report = "Trekker_Report.*\\.html?$"
)

.trekker_sha256 <- function(path) {
  commands <- Sys.which(c("sha256sum", "shasum", "openssl", "certutil"))
  quoted_path <- shQuote(path)
  output <- if (nzchar(commands[["sha256sum"]])) {
    system2(commands[["sha256sum"]], quoted_path, stdout = TRUE, stderr = TRUE)
  } else if (nzchar(commands[["shasum"]])) {
    system2(
      commands[["shasum"]],
      c("-a", "256", quoted_path),
      stdout = TRUE,
      stderr = TRUE
    )
  } else if (nzchar(commands[["openssl"]])) {
    system2(
      commands[["openssl"]],
      c("dgst", "-sha256", quoted_path),
      stdout = TRUE,
      stderr = TRUE
    )
  } else if (nzchar(commands[["certutil"]])) {
    system2(
      commands[["certutil"]],
      c("-hashfile", quoted_path, "SHA256"),
      stdout = TRUE,
      stderr = TRUE
    )
  } else {
    stop(
      "A SHA-256 command is required (sha256sum, shasum, openssl, or certutil).",
      call. = FALSE
    )
  }
  matches <- regmatches(output, regexpr("[[:xdigit:]]{64}", output))
  matches <- tolower(matches[nchar(matches) == 64L])
  if (!length(matches)) {
    stop("Could not calculate SHA-256 for: ", path, call. = FALSE)
  }
  matches[[1L]]
}

.normalize_trekker_files <- function(trekker_data, require_location = TRUE) {
  if (is.null(trekker_data)) {
    return(NULL)
  }
  if (
    is.character(trekker_data) &&
      length(trekker_data) == 1L &&
      !is.na(trekker_data) &&
      dir.exists(trekker_data)
  ) {
    candidates <- list.files(
      trekker_data,
      full.names = TRUE,
      recursive = FALSE,
      all.files = FALSE
    )
    files <- list()
    for (category in names(.trekker_patterns)) {
      matched <- candidates[grepl(
        .trekker_patterns[[category]],
        basename(candidates),
        ignore.case = TRUE
      )]
      if (length(matched) > 1L) {
        stop(
          "Multiple Trekker ",
          category,
          " files were found. Supply a named list to select one explicitly.",
          call. = FALSE
        )
      }
      if (length(matched) == 1L) {
        files[[category]] <- matched[[1L]]
      }
    }
  } else {
    if (!is.list(trekker_data) || is.null(names(trekker_data))) {
      stop(
        "`trekker_data` must be a directory or a named list of companion files.",
        call. = FALSE
      )
    }
    if (
      anyNA(names(trekker_data)) ||
        any(!nzchar(names(trekker_data))) ||
        anyDuplicated(names(trekker_data))
    ) {
      stop(
        "`trekker_data` category names must be non-empty and unique.",
        call. = FALSE
      )
    }
    aliases <- c(
      location = "location",
      positioning = "positioning",
      positioning_evidence = "positioning",
      metrics = "metrics",
      cluster_markers = "cluster_markers",
      markers = "cluster_markers",
      moran = "moran",
      report = "report"
    )
    unknown <- setdiff(names(trekker_data), names(aliases))
    if (length(unknown)) {
      stop(
        "Unknown `trekker_data` category: ",
        unknown[[1L]],
        paste0(
          ". Valid categories are location, positioning, metrics, ",
          "cluster_markers, moran, and report."
        ),
        call. = FALSE
      )
    }
    normalized_names <- unname(aliases[names(trekker_data)])
    if (anyDuplicated(normalized_names)) {
      stop(
        "Each Trekker file category may be supplied only once.",
        call. = FALSE
      )
    }
    files <- trekker_data
    names(files) <- normalized_names
  }
  if (require_location && is.null(files$location)) {
    stop(
      "Trekker data requires exactly one *_Location_ConfPositionedNuclei*.csv file.",
      call. = FALSE
    )
  }
  if (!length(files)) {
    stop(
      "`trekker_data` did not contain a recognized companion file.",
      call. = FALSE
    )
  }
  valid <- vapply(
    files,
    function(path) {
      is.character(path) &&
        length(path) == 1L &&
        !is.na(path) &&
        nzchar(path) &&
        file.exists(path) &&
        !dir.exists(path)
    },
    logical(1)
  )
  if (!all(valid)) {
    stop(
      "Trekker companion file not found: ",
      files[[which(!valid)[1L]]],
      call. = FALSE
    )
  }
  files <- lapply(files, normalizePath, winslash = "/", mustWork = TRUE)
  files[intersect(.trekker_categories, names(files))]
}

.read_trekker_row_table <- function(path, sep = ",") {
  table <- utils::read.table(
    path,
    header = TRUE,
    sep = sep,
    quote = '"',
    comment.char = "",
    check.names = FALSE,
    stringsAsFactors = FALSE,
    row.names = 1L
  )
  data.frame(row_id = rownames(table), table, check.names = FALSE)
}

.trekker_barcode_key <- function(value) {
  sub("-[0-9]+$", "", as.character(value))
}

.read_trekker_positioning <- function(path, cells) {
  positioning <- utils::read.table(
    path,
    header = TRUE,
    quote = '"',
    comment.char = "",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (!"cell_bc" %in% names(positioning)) {
    stop("Trekker positioning evidence must contain `cell_bc`.", call. = FALSE)
  }
  source_barcodes <- as.character(positioning$cell_bc)
  if (
    anyNA(source_barcodes) ||
      any(!nzchar(source_barcodes)) ||
      anyDuplicated(source_barcodes)
  ) {
    stop(
      "Trekker positioning evidence barcodes must be non-empty and unique.",
      call. = FALSE
    )
  }

  index <- match(cells, source_barcodes)
  unresolved <- which(is.na(index))
  if (length(unresolved)) {
    source_keys <- .trekker_barcode_key(source_barcodes)
    cell_keys <- .trekker_barcode_key(cells[unresolved])
    needed <- source_keys %in% cell_keys
    if (
      anyDuplicated(source_keys[needed]) ||
        anyDuplicated(cell_keys)
    ) {
      stop(
        "Trekker positioning evidence has ambiguous barcode suffixes.",
        call. = FALSE
      )
    }
    index[unresolved] <- match(cell_keys, source_keys)
  }
  if (anyNA(index) || anyDuplicated(index)) {
    missing <- cells[is.na(index)]
    if (length(missing)) {
      stop(
        "Trekker positioning evidence is missing ",
        length(missing),
        " CRB cell(s), including: ",
        paste(utils::head(missing, 5L), collapse = ", "),
        call. = FALSE
      )
    }
    stop(
      "Trekker positioning evidence has ambiguous barcode suffixes.",
      call. = FALSE
    )
  }

  aligned <- positioning[
    index,
    setdiff(names(positioning), "cell_bc"),
    drop = FALSE
  ]
  rownames(aligned) <- NULL
  data.frame(
    barcode = as.character(cells),
    source_barcode = source_barcodes[index],
    aligned,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

.trekker_report_metadata <- function(path) {
  html <- paste(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    collapse = " "
  )
  meta_tags <- regmatches(
    html,
    gregexpr("(?is)<meta\\s+[^>]*>", html, perl = TRUE)
  )[[1L]]
  meta_content <- if (length(meta_tags) && !identical(meta_tags, "")) {
    sub(
      "(?is).*content\\s*=\\s*\"([^\"]*)\".*",
      "\\1",
      meta_tags,
      perl = TRUE
    )
  } else {
    character()
  }
  html <- gsub("(?is)<(script|style)[^>]*>.*?</\\1>", " ", html, perl = TRUE)
  title <- sub(
    "(?is).*?<title[^>]*>(.*?)</title>.*",
    "\\1",
    html,
    perl = TRUE
  )
  if (identical(title, html)) {
    title <- NA_character_
  }
  text <- trimws(gsub(
    "[[:space:]]+",
    " ",
    gsub("(?s)<[^>]+>", " ", html, perl = TRUE)
  ))
  metadata_text <- paste(c(meta_content, text), collapse = " | ")
  after_label <- function(label) {
    match <- regexec(
      paste0("(?i)", label, "[[:space:]:-]{0,8}([^|]{1,160})"),
      metadata_text,
      perl = TRUE
    )
    value <- regmatches(metadata_text, match)[[1L]]
    if (length(value) < 2L) {
      return(NA_character_)
    }
    trimws(substr(value[[2L]], 1L, 120L))
  }
  list(
    title = if (is.na(title)) {
      NA_character_
    } else {
      trimws(substr(title, 1L, 200L))
    },
    sample = if (is.na(title)) {
      after_label("Sample")
    } else {
      trimws(substr(title, 1L, 200L))
    },
    tile_id = after_label("Tile ID"),
    pipeline_version = after_label("Pipeline Version"),
    assay_platform = after_label("Single cell assay platform"),
    analysis_date = after_label("Analysis Date")
  )
}

.trekker_file_descriptor <- function(category, path, relative_path) {
  info <- file.info(path)
  list(
    category = category,
    name = basename(path),
    path = relative_path,
    sha256 = .trekker_sha256(path),
    size = unname(info$size[[1L]])
  )
}

.trekker_named_list <- function(value, context) {
  if (is.null(value) || (is.list(value) && !length(value))) {
    return(list())
  }
  if (
    !is.list(value) ||
      is.null(names(value)) ||
      anyNA(names(value)) ||
      any(!nzchar(names(value))) ||
      anyDuplicated(names(value))
  ) {
    stop(
      context,
      " must be a named list with non-empty, unique names.",
      call. = FALSE
    )
  }
  value
}

.normalize_trekker_sections <- function(
  trekker_sections,
  cells,
  location,
  object = NULL,
  declared_sections = character()
) {
  section <- NULL
  if (!is.null(trekker_sections)) {
    if (
      is.data.frame(trekker_sections) &&
        all(c("barcode", "section") %in% names(trekker_sections))
    ) {
      if (anyDuplicated(trekker_sections$barcode)) {
        stop("`trekker_sections` barcodes must be unique.", call. = FALSE)
      }
      section <- as.character(trekker_sections$section[match(
        cells,
        trekker_sections$barcode
      )])
    } else if (
      is.character(trekker_sections) &&
        !is.null(names(trekker_sections)) &&
        !anyDuplicated(names(trekker_sections))
    ) {
      section <- as.character(trekker_sections[match(
        cells,
        names(trekker_sections)
      )])
    } else {
      stop(
        "`trekker_sections` must be a named cell-to-section vector or a barcode/section data frame.",
        call. = FALSE
      )
    }
  }

  if (is.null(section) && !is.null(object)) {
    memberships <- list()
    image_names <- tryCatch(
      SeuratObject::Images(object),
      error = function(error) character()
    )
    for (image_name in image_names) {
      image_cells <- tryCatch(
        SeuratObject::Cells(object[[image_name]]),
        error = function(error) character()
      )
      memberships[[image_name]] <- intersect(cells, image_cells)
    }
    assigned <- unlist(memberships, use.names = FALSE)
    if (
      length(assigned) && !anyDuplicated(assigned) && setequal(assigned, cells)
    ) {
      section <- rep(NA_character_, length(cells))
      for (image_name in names(memberships)) {
        section[match(memberships[[image_name]], cells)] <- image_name
      }
    }
  }

  if (is.null(section)) {
    clean <- tolower(gsub("[^a-z0-9]", "", names(location)))
    section_column <- match(
      c("section", "slice", "fov", "tile", "tileid"),
      clean,
      nomatch = 0L
    )
    section_column <- section_column[section_column > 0L]
    if (length(section_column)) {
      section <- as.character(location[[section_column[[1L]]]][match(
        cells,
        location$row_id
      )])
    }
  }

  declared_sections <- unique(as.character(declared_sections))
  declared_sections <- declared_sections[
    !is.na(declared_sections) & nzchar(declared_sections)
  ]
  if (is.null(section)) {
    if (length(declared_sections) > 1L) {
      stop(
        "Multiple Trekker sections were declared, but cell membership could not be resolved. Supply `trekker_sections`.",
        call. = FALSE
      )
    }
    section <- rep(
      if (length(declared_sections)) declared_sections[[1L]] else "slice1",
      length(cells)
    )
  }
  if (
    length(section) != length(cells) || anyNA(section) || any(!nzchar(section))
  ) {
    stop(
      "Every CRB cell must belong to exactly one non-empty Trekker section.",
      call. = FALSE
    )
  }
  section
}

.trekker_image_defaults <- list(
  rotation = 0,
  flip_x = FALSE,
  flip_y = FALSE,
  scale_x = 1,
  scale_y = 1,
  offset_x = 0,
  offset_y = 0,
  image_opacity = 0.6,
  visible = TRUE
)

.normalize_trekker_image_settings <- function(
  value,
  context,
  base = .trekker_image_defaults
) {
  if (is.null(value)) {
    return(base)
  }
  if (!is.list(value) || is.null(names(value)) || anyDuplicated(names(value))) {
    stop(context, " settings must be a named list.", call. = FALSE)
  }
  unknown <- setdiff(names(value), names(.trekker_image_defaults))
  if (length(unknown)) {
    stop(context, " has unknown setting: ", unknown[[1L]], call. = FALSE)
  }
  result <- base
  result[names(value)] <- value
  numeric_fields <- c(
    "rotation",
    "scale_x",
    "scale_y",
    "offset_x",
    "offset_y",
    "image_opacity"
  )
  for (field in numeric_fields) {
    current <- result[[field]]
    if (!is.numeric(current) || length(current) != 1L || !is.finite(current)) {
      stop(
        context,
        " setting `",
        field,
        "` must be one finite number.",
        call. = FALSE
      )
    }
  }
  if (result$scale_x <= 0 || result$scale_y <= 0) {
    stop(context, " scale_x and scale_y must be positive.", call. = FALSE)
  }
  if (result$image_opacity < 0 || result$image_opacity > 1) {
    stop(context, " image_opacity must be between 0 and 1.", call. = FALSE)
  }
  for (field in c("flip_x", "flip_y", "visible")) {
    if (
      !is.logical(result[[field]]) ||
        length(result[[field]]) != 1L ||
        is.na(result[[field]])
    ) {
      stop(
        context,
        " setting `",
        field,
        "` must be TRUE or FALSE.",
        call. = FALSE
      )
    }
  }
  result
}

.normalize_trekker_images <- function(
  images,
  settings,
  coordinates,
  sidecar_name
) {
  images <- .trekker_named_list(images, "`trekker_images`")
  settings <- .trekker_named_list(settings, "`trekker_image_settings`")
  if (!length(images)) {
    if (length(settings)) {
      stop("`trekker_image_settings` requires `trekker_images`.", call. = FALSE)
    }
    return(list(descriptors = list(), sources = list()))
  }
  unknown_settings <- setdiff(names(settings), names(images))
  if (length(unknown_settings)) {
    stop(
      "`trekker_image_settings` contains unknown section: ",
      unknown_settings[[1L]],
      call. = FALSE
    )
  }
  descriptors <- sources <- list()
  allowed <- c("png", "jpg", "jpeg", "svg")
  mime <- c(
    png = "image/png",
    jpg = "image/jpeg",
    jpeg = "image/jpeg",
    svg = "image/svg+xml"
  )
  for (section_index in seq_along(images)) {
    section <- names(images)[[section_index]]
    declarations <- .trekker_named_list(
      images[[section]],
      paste0("Trekker section `", section, "`")
    )
    section_settings <- .trekker_named_list(
      settings[[section]],
      paste0("`trekker_image_settings` section `", section, "`")
    )
    unknown_labels <- setdiff(names(section_settings), names(declarations))
    if (length(unknown_labels)) {
      stop(
        "`trekker_image_settings` contains unknown image: ",
        section,
        "/",
        unknown_labels[[1L]],
        call. = FALSE
      )
    }
    section_coordinates <- coordinates[
      coordinates$section == section,
      c("x", "y"),
      drop = FALSE
    ]
    if (!nrow(section_coordinates)) {
      stop("Trekker section has no cells: ", section, call. = FALSE)
    }
    for (image_index in seq_along(declarations)) {
      label <- names(declarations)[[image_index]]
      declaration <- declarations[[label]]
      if (is.character(declaration) && length(declaration) == 1L) {
        declaration <- list(path = declaration)
      }
      if (!is.list(declaration) || is.null(declaration$path)) {
        stop(
          "Trekker image `",
          section,
          "/",
          label,
          "` must supply `path`.",
          call. = FALSE
        )
      }
      path <- declaration$path
      if (
        !is.character(path) ||
          length(path) != 1L ||
          is.na(path) ||
          !file.exists(path) ||
          dir.exists(path)
      ) {
        stop("Trekker image not found: ", path, call. = FALSE)
      }
      path <- normalizePath(path, winslash = "/", mustWork = TRUE)
      extension <- tolower(tools::file_ext(path))
      if (!extension %in% allowed) {
        stop(
          "Trekker images must be PNG, JPEG/JPG, or SVG: ",
          path,
          call. = FALSE
        )
      }
      context <- paste0("Trekker image `", section, "/", label, "`")
      bounds <- .spatialImageBounds(
        declaration$bounds,
        section_coordinates,
        context
      )
      preset <- .normalize_trekker_image_settings(
        section_settings[[label]],
        context
      )
      relative <- paste0(
        gsub("\\\\", "/", sidecar_name),
        "/images/",
        sprintf("%02d-%02d.%s", section_index, image_index, extension)
      )
      descriptors[[section]][[label]] <- c(
        .trekker_file_descriptor("image", path, relative),
        list(
          section = section,
          label = label,
          mime = unname(mime[[extension]]),
          bounds = as.list(bounds),
          settings = preset,
          provenance = declaration$provenance %||% NULL
        )
      )
      sources[[section]][[label]] <- path
    }
  }
  list(descriptors = descriptors, sources = sources)
}

.build_trekker_payload <- function(
  files,
  cells,
  sidecar_name,
  declared_by,
  spatial_coordinates = NULL,
  trekker_images = NULL,
  trekker_sections = NULL,
  trekker_image_settings = NULL,
  object = NULL
) {
  files <- .normalize_trekker_files(files, require_location = TRUE)
  location <- .read_trekker_row_table(files$location)
  required_location <- c("row_id", "SPATIAL_1", "SPATIAL_2")
  if (!all(required_location %in% names(location))) {
    stop(
      "The Trekker Location file must contain SPATIAL_1 and SPATIAL_2.",
      call. = FALSE
    )
  }
  if (
    anyNA(location$row_id) ||
      any(!nzchar(location$row_id)) ||
      anyDuplicated(location$row_id)
  ) {
    stop(
      "Trekker Location barcodes must be non-empty and unique.",
      call. = FALSE
    )
  }
  index <- match(cells, location$row_id)
  if (anyNA(index)) {
    stop(
      "The Trekker Location file is missing ",
      sum(is.na(index)),
      " CRB cell(s), including: ",
      paste(utils::head(cells[is.na(index)], 5L), collapse = ", "),
      call. = FALSE
    )
  }
  x <- suppressWarnings(as.numeric(location$SPATIAL_1[index]))
  y <- suppressWarnings(as.numeric(location$SPATIAL_2[index]))
  if (any(!is.finite(x)) || any(!is.finite(y))) {
    stop(
      "Trekker Location coordinates for CRB cells must be finite numbers.",
      call. = FALSE
    )
  }
  image_sections <- if (is.list(trekker_images)) {
    names(trekker_images)
  } else {
    character()
  }
  section <- .normalize_trekker_sections(
    trekker_sections,
    cells,
    location,
    object = object,
    declared_sections = image_sections
  )
  coordinates <- data.frame(
    barcode = cells,
    section = section,
    x = x,
    y = y,
    stringsAsFactors = FALSE
  )
  image_plan <- .normalize_trekker_images(
    trekker_images,
    trekker_image_settings,
    coordinates,
    sidecar_name
  )

  metrics <- NULL
  if (!is.null(files$metrics)) {
    metrics <- utils::read.csv(
      files$metrics,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    if (!all(c("Metrics", "Value") %in% names(metrics))) {
      stop(
        "The Trekker metrics file must contain Metrics and Value columns.",
        call. = FALSE
      )
    }
  }
  positioning <- if (is.null(files$positioning)) {
    NULL
  } else {
    .read_trekker_positioning(files$positioning, cells)
  }
  cluster_markers <- NULL
  if (!is.null(files$cluster_markers)) {
    cluster_markers <- .read_trekker_row_table(files$cluster_markers)
    required <- c(
      "gene",
      "cluster",
      "p_val",
      "avg_log2FC",
      "pct.1",
      "pct.2",
      "p_val_adj"
    )
    if (!all(required %in% names(cluster_markers))) {
      stop(
        "The Trekker cluster marker file is missing required columns: ",
        paste(setdiff(required, names(cluster_markers)), collapse = ", "),
        call. = FALSE
      )
    }
  }
  moran <- NULL
  if (!is.null(files$moran)) {
    moran <- .read_trekker_row_table(files$moran, sep = "\t")
    names(moran)[names(moran) == "row_id"] <- "gene"
    required <- c(
      "gene",
      "MoransI_observed",
      "MoransI_p.value",
      "moransi.spatially.variable",
      "moransi.spatially.variable.rank"
    )
    if (!all(required %in% names(moran))) {
      stop(
        "The Trekker Moran file is missing required columns: ",
        paste(setdiff(required, names(moran)), collapse = ", "),
        call. = FALSE
      )
    }
  }

  relation <- NULL
  if (!is.null(spatial_coordinates)) {
    spatial_coordinates <- as.matrix(spatial_coordinates)
    spatial_index <- match(cells, rownames(spatial_coordinates))
    if (ncol(spatial_coordinates) >= 2L && !anyNA(spatial_index)) {
      sx <- as.numeric(spatial_coordinates[spatial_index, 1L])
      sy <- as.numeric(spatial_coordinates[spatial_index, 2L])
      relation <- if (isTRUE(all.equal(sx, x)) && isTRUE(all.equal(sy, y))) {
        "same"
      } else if (isTRUE(all.equal(sx, x)) && isTRUE(all.equal(sy, -y))) {
        "y_flipped"
      } else {
        "different"
      }
    }
  }

  descriptors <- lapply(names(files), function(category) {
    .trekker_file_descriptor(
      category,
      files[[category]],
      paste0(gsub("\\\\", "/", sidecar_name), "/", basename(files[[category]]))
    )
  })
  names(descriptors) <- names(files)
  payload <- list(
    schema_version = 1L,
    entity_type = "nucleus",
    source = list(
      declared_by = declared_by,
      imported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      spatial_relation = relation
    ),
    files = descriptors,
    coordinates = coordinates,
    positioning = positioning,
    images = image_plan$descriptors,
    metrics = metrics,
    cluster_markers = cluster_markers,
    moran = moran,
    report_metadata = if (is.null(files$report)) {
      NULL
    } else {
      .trekker_report_metadata(files$report)
    }
  )
  attr(payload, "trekker_image_sources") <- image_plan$sources
  payload
}

.validate_trekker_payload <- function(data) {
  if (!is.list(data) || !identical(data$schema_version, 1L)) {
    stop("Trekker data must use schema_version 1.", call. = FALSE)
  }
  required <- c("entity_type", "source", "files", "coordinates")
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop(
      "Trekker data is missing required schema field: ",
      missing[[1L]],
      ".",
      call. = FALSE
    )
  }
  if (!is.data.frame(data$coordinates)) {
    stop("Trekker coordinates must be a data frame.", call. = FALSE)
  }
  if (!identical(data$entity_type, "nucleus")) {
    stop("Trekker entity_type must be `nucleus`.", call. = FALSE)
  }
  if (!all(c("barcode", "section", "x", "y") %in% names(data$coordinates))) {
    stop(
      "Trekker coordinates must contain barcode, section, x, and y.",
      call. = FALSE
    )
  }
  coordinates <- data$coordinates
  if (
    anyNA(coordinates$barcode) ||
      any(!nzchar(coordinates$barcode)) ||
      anyDuplicated(coordinates$barcode) ||
      anyNA(coordinates$section) ||
      any(!nzchar(coordinates$section)) ||
      any(!is.finite(as.numeric(coordinates$x))) ||
      any(!is.finite(as.numeric(coordinates$y)))
  ) {
    stop(
      "Trekker coordinates contain invalid barcodes or values.",
      call. = FALSE
    )
  }
  if (is.null(data$files$location)) {
    stop(
      "Trekker data must retain its Location file descriptor.",
      call. = FALSE
    )
  }
  if (!is.null(data$positioning)) {
    positioning <- data$positioning
    if (
      !is.data.frame(positioning) ||
        !all(c("barcode", "source_barcode") %in% names(positioning)) ||
        !identical(
          as.character(positioning$barcode),
          as.character(coordinates$barcode)
        ) ||
        anyNA(positioning$source_barcode) ||
        any(!nzchar(as.character(positioning$source_barcode)))
    ) {
      stop(
        "Trekker positioning evidence must align one row to every coordinate.",
        call. = FALSE
      )
    }
    numeric_fields <- vapply(positioning, is.numeric, logical(1))
    invalid_numeric <- vapply(
      positioning[numeric_fields],
      function(value) any(!is.na(value) & !is.finite(value)),
      logical(1)
    )
    if (any(invalid_numeric)) {
      stop(
        "Trekker positioning evidence contains non-finite values.",
        call. = FALSE
      )
    }
  }
  if (xor(is.null(data$positioning), is.null(data$files$positioning))) {
    stop(
      paste(
        "Trekker positioning evidence and its file descriptor",
        "must be supplied together."
      ),
      call. = FALSE
    )
  }
  if (any(!names(data$files) %in% .trekker_categories)) {
    stop("Trekker data contains an unknown file category.", call. = FALSE)
  }
  valid_files <- vapply(
    names(data$files),
    function(category) {
      file <- data$files[[category]]
      is.list(file) &&
        all(c("category", "name", "path", "sha256", "size") %in% names(file)) &&
        identical(file$category, category) &&
        is.character(file$name) &&
        length(file$name) == 1L &&
        !is.na(file$name) &&
        nzchar(file$name) &&
        is.character(file$path) &&
        length(file$path) == 1L &&
        !is.na(file$path) &&
        nzchar(file$path) &&
        is.character(file$sha256) &&
        length(file$sha256) == 1L &&
        grepl("^[[:xdigit:]]{64}$", file$sha256) &&
        is.numeric(file$size) &&
        length(file$size) == 1L &&
        is.finite(file$size) &&
        file$size >= 0
    },
    logical(1)
  )
  if (!all(valid_files)) {
    stop("Trekker file descriptors are invalid.", call. = FALSE)
  }
  images <- data$images %||% list()
  if (
    !is.list(images) ||
      (length(images) &&
        (is.null(names(images)) || anyDuplicated(names(images))))
  ) {
    stop("Trekker image sections are invalid.", call. = FALSE)
  }
  for (section in names(images)) {
    if (!section %in% coordinates$section) {
      stop("Trekker image section has no cells: ", section, call. = FALSE)
    }
    layers <- images[[section]]
    if (
      !is.list(layers) || is.null(names(layers)) || anyDuplicated(names(layers))
    ) {
      stop(
        "Trekker image layers are invalid for section: ",
        section,
        call. = FALSE
      )
    }
    for (label in names(layers)) {
      image <- layers[[label]]
      if (
        !is.list(image) ||
          !all(
            c(
              "category",
              "name",
              "path",
              "sha256",
              "size",
              "section",
              "label",
              "mime",
              "bounds",
              "settings"
            ) %in%
              names(image)
          ) ||
          !identical(image$category, "image") ||
          !identical(image$section, section) ||
          !identical(image$label, label) ||
          !image$mime %in% c("image/png", "image/jpeg", "image/svg+xml")
      ) {
        stop(
          "Trekker image descriptor is invalid: ",
          section,
          "/",
          label,
          call. = FALSE
        )
      }
      .normalize_trekker_image_settings(
        image$settings,
        paste0("Trekker image `", section, "/", label, "`")
      )
      .spatialImageBounds(
        unlist(image$bounds),
        coordinates[coordinates$section == section, c("x", "y")],
        paste0("Trekker image `", section, "/", label, "`")
      )
    }
  }
  invisible(data)
}

.copy_trekker_files <- function(files, target_dir) {
  if (
    !dir.create(target_dir, recursive = TRUE, showWarnings = FALSE) &&
      !dir.exists(target_dir)
  ) {
    stop(
      "Failed to create Trekker sidecar directory: ",
      target_dir,
      call. = FALSE
    )
  }
  for (path in files) {
    target <- file.path(target_dir, basename(path))
    if (!file.copy(path, target, overwrite = FALSE, copy.mode = TRUE)) {
      stop("Failed to copy Trekker companion file: ", path, call. = FALSE)
    }
    if (!identical(.trekker_sha256(path), .trekker_sha256(target))) {
      stop(
        "Trekker companion checksum changed while copying: ",
        path,
        call. = FALSE
      )
    }
  }
  invisible(target_dir)
}

.copy_trekker_images <- function(sources, descriptors, target_dir) {
  if (!length(sources)) {
    return(invisible(target_dir))
  }
  for (section in names(sources)) {
    for (label in names(sources[[section]])) {
      source <- sources[[section]][[label]]
      target <- file.path(
        target_dir,
        "images",
        basename(descriptors[[section]][[label]]$path)
      )
      dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
      if (!file.copy(source, target, overwrite = FALSE, copy.mode = TRUE)) {
        stop("Failed to copy Trekker image: ", source, call. = FALSE)
      }
      if (!identical(.trekker_sha256(source), .trekker_sha256(target))) {
        stop(
          "Trekker image checksum changed while copying: ",
          source,
          call. = FALSE
        )
      }
    }
  }
  invisible(target_dir)
}

.normalize_app_trekker_data <- function(trekker_data, dataset_names) {
  if (is.null(trekker_data)) {
    return(setNames(vector("list", length(dataset_names)), dataset_names))
  }
  companion_names <- c(names(.trekker_patterns), "markers")
  is_companion_list <- is.list(trekker_data) &&
    !is.null(names(trekker_data)) &&
    length(trekker_data) > 0L &&
    all(names(trekker_data) %in% companion_names)
  if (
    length(dataset_names) == 1L &&
      (is.character(trekker_data) || is_companion_list)
  ) {
    return(setNames(list(trekker_data), dataset_names))
  }
  if (!is.list(trekker_data) || is.null(names(trekker_data))) {
    stop(
      "For multiple datasets, `trekker_data` must be a named list keyed by dataset.",
      call. = FALSE
    )
  }
  if (
    anyNA(names(trekker_data)) ||
      any(!nzchar(names(trekker_data))) ||
      anyDuplicated(names(trekker_data))
  ) {
    stop(
      "`trekker_data` dataset names must be non-empty and unique.",
      call. = FALSE
    )
  }
  unknown <- setdiff(names(trekker_data), dataset_names)
  if (length(unknown)) {
    stop("Unknown `trekker_data` dataset: ", unknown[[1L]], call. = FALSE)
  }
  result <- setNames(vector("list", length(dataset_names)), dataset_names)
  result[names(trekker_data)] <- trekker_data
  result
}

.normalize_app_trekker_option <- function(value, dataset_names, argument) {
  result <- setNames(vector("list", length(dataset_names)), dataset_names)
  if (is.null(value) || !length(value)) {
    return(result)
  }
  if (
    length(dataset_names) == 1L &&
      (!is.list(value) ||
        is.null(names(value)) ||
        !all(names(value) %in% dataset_names))
  ) {
    result[[1L]] <- value
    return(result)
  }
  if (
    !is.list(value) ||
      is.null(names(value)) ||
      anyNA(names(value)) ||
      any(!nzchar(names(value))) ||
      anyDuplicated(names(value))
  ) {
    stop(
      "`",
      argument,
      "` must be a named list keyed by dataset.",
      call. = FALSE
    )
  }
  unknown <- setdiff(names(value), dataset_names)
  if (length(unknown)) {
    stop("Unknown `", argument, "` dataset: ", unknown[[1L]], call. = FALSE)
  }
  result[names(value)] <- value
  result
}

.normalize_trekker_image_replace <- function(value, dataset_names) {
  value <- .normalize_app_trekker_option(
    value,
    dataset_names,
    "trekker_image_replace"
  )
  for (dataset in names(value)) {
    if (is.null(value[[dataset]])) {
      next
    }
    sections <- .trekker_named_list(
      value[[dataset]],
      paste0("`trekker_image_replace` dataset `", dataset, "`")
    )
    for (section in names(sections)) {
      labels <- sections[[section]]
      if (!is.character(labels) || anyNA(labels) || any(!nzchar(labels))) {
        stop(
          "Trekker image replacement labels must be non-empty strings.",
          call. = FALSE
        )
      }
      sections[[section]] <- unique(labels)
    }
    value[[dataset]] <- sections
  }
  value
}

.normalize_trekker_replace <- function(trekker_replace, dataset_names) {
  allowed <- c(
    "positioning",
    "metrics",
    "cluster_markers",
    "moran",
    "report"
  )
  result <- setNames(vector("list", length(dataset_names)), dataset_names)
  if (is.null(trekker_replace)) {
    return(result)
  }
  if (is.character(trekker_replace) && length(dataset_names) == 1L) {
    trekker_replace <- setNames(list(trekker_replace), dataset_names)
  }
  if (!is.list(trekker_replace) || is.null(names(trekker_replace))) {
    stop(
      "`trekker_replace` must be a named list keyed by dataset.",
      call. = FALSE
    )
  }
  unknown_datasets <- setdiff(names(trekker_replace), dataset_names)
  if (length(unknown_datasets)) {
    stop(
      "Unknown `trekker_replace` dataset: ",
      unknown_datasets[[1L]],
      call. = FALSE
    )
  }
  for (dataset in names(trekker_replace)) {
    categories <- as.character(trekker_replace[[dataset]])
    unknown <- setdiff(categories, allowed)
    if (length(unknown)) {
      stop(
        "Trekker category '",
        unknown[[1L]],
        "' cannot be replaced. Location must be changed by re-exporting the CRB.",
        call. = FALSE
      )
    }
    result[[dataset]] <- unique(categories)
  }
  result
}

.resolve_trekker_files <- function(trekker, crb_path) {
  if (is.null(trekker)) {
    return(list())
  }
  .validate_trekker_payload(trekker)
  root <- normalizePath(dirname(crb_path), winslash = "/", mustWork = TRUE)
  resolved <- list()
  for (category in names(trekker$files)) {
    descriptor <- trekker$files[[category]]
    relative <- descriptor$path
    if (
      !is.character(relative) ||
        length(relative) != 1L ||
        is.na(relative) ||
        !nzchar(relative) ||
        grepl("^([A-Za-z]:|/|\\\\)", relative) ||
        any(
          strsplit(gsub("\\\\", "/", relative), "/", fixed = TRUE)[[1L]] %in%
            c("", ".", "..")
        )
    ) {
      stop(
        "Unsafe Trekker sidecar path in ",
        basename(crb_path),
        ".",
        call. = FALSE
      )
    }
    path <- file.path(root, relative)
    if (!file.exists(path) || dir.exists(path)) {
      stop("Trekker companion file is missing: ", path, call. = FALSE)
    }
    path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    if (!startsWith(path, paste0(sub("/+$", "", root), "/"))) {
      stop("Trekker sidecar path escapes the CRB directory.", call. = FALSE)
    }
    checksum <- .trekker_sha256(path)
    if (!identical(checksum, descriptor$sha256)) {
      stop("Trekker companion checksum mismatch: ", path, call. = FALSE)
    }
    resolved[[category]] <- path
  }
  resolved
}

.resolve_trekker_images <- function(trekker, crb_path) {
  if (is.null(trekker) || !length(trekker$images)) {
    return(list())
  }
  root <- normalizePath(dirname(crb_path), winslash = "/", mustWork = TRUE)
  result <- list()
  for (section in names(trekker$images)) {
    for (label in names(trekker$images[[section]])) {
      descriptor <- trekker$images[[section]][[label]]
      relative <- descriptor$path
      if (
        !is.character(relative) ||
          length(relative) != 1L ||
          is.na(relative) ||
          !nzchar(relative) ||
          grepl("^([A-Za-z]:|/|\\\\)", relative) ||
          any(
            strsplit(gsub("\\\\", "/", relative), "/", fixed = TRUE)[[1L]] %in%
              c("", ".", "..")
          )
      ) {
        stop(
          "Unsafe Trekker image path in ",
          basename(crb_path),
          ".",
          call. = FALSE
        )
      }
      path <- normalizePath(
        file.path(root, relative),
        winslash = "/",
        mustWork = TRUE
      )
      if (!startsWith(path, paste0(sub("/+$", "", root), "/"))) {
        stop("Trekker image path escapes the CRB directory.", call. = FALSE)
      }
      if (!identical(.trekker_sha256(path), descriptor$sha256)) {
        stop("Trekker image checksum mismatch: ", path, call. = FALSE)
      }
      result[[section]][[label]] <- path
    }
  }
  result
}

.prepare_app_trekker <- function(
  crb_path,
  supplied,
  replace,
  target_root,
  dataset,
  supplied_images = NULL,
  supplied_sections = NULL,
  supplied_image_settings = NULL,
  image_replace = NULL
) {
  object <- readRDS(crb_path)
  existing <- tryCatch(object$getTrekker(), error = function(error) NULL)
  existing_files <- .resolve_trekker_files(existing, crb_path)
  existing_images <- .resolve_trekker_images(existing, crb_path)
  incoming <- if (is.null(supplied)) {
    list()
  } else {
    .normalize_trekker_files(supplied, require_location = is.null(existing))
  }
  if (is.null(existing) && !length(incoming)) {
    if (
      !is.null(supplied_images) ||
        !is.null(supplied_sections) ||
        !is.null(supplied_image_settings) ||
        !is.null(image_replace)
    ) {
      stop(
        "Dataset '",
        dataset,
        "' needs `trekker_data` with a Location file before Trekker images or sections can be added.",
        call. = FALSE
      )
    }
    return(NULL)
  }
  selected <- existing_files
  changed <- is.null(existing)
  for (category in names(incoming)) {
    if (is.null(selected[[category]])) {
      selected[[category]] <- incoming[[category]]
      changed <- TRUE
      next
    }
    if (
      identical(
        .trekker_sha256(selected[[category]]),
        .trekker_sha256(incoming[[category]])
      )
    ) {
      next
    }
    if (identical(category, "location")) {
      stop(
        "Dataset '",
        dataset,
        "' supplies a different Location file. Re-export the CRB to replace it.",
        call. = FALSE
      )
    }
    if (!category %in% replace) {
      stop(
        "Dataset '",
        dataset,
        "' has different Trekker ",
        category,
        " content. Add '",
        category,
        "' to `trekker_replace` to replace it explicitly.",
        call. = FALSE
      )
    }
    selected[[category]] <- incoming[[category]]
    changed <- TRUE
  }
  if (is.null(selected$location)) {
    stop("Trekker enrichment requires a Location file.", call. = FALSE)
  }

  image_declarations <- image_settings <- list()
  if (!is.null(existing)) {
    for (section in names(existing_images)) {
      for (label in names(existing_images[[section]])) {
        descriptor <- existing$images[[section]][[label]]
        image_declarations[[section]][[label]] <- list(
          path = existing_images[[section]][[label]],
          bounds = unlist(descriptor$bounds),
          provenance = descriptor$provenance %||% NULL
        )
        image_settings[[section]][[label]] <- descriptor$settings
      }
    }
  }
  supplied_images <- .trekker_named_list(
    supplied_images,
    paste0("`trekker_images` dataset `", dataset, "`")
  )
  image_replace <- image_replace %||% list()
  for (section in names(supplied_images)) {
    incoming_layers <- .trekker_named_list(
      supplied_images[[section]],
      paste0("`trekker_images` section `", section, "`")
    )
    for (label in names(incoming_layers)) {
      declaration <- incoming_layers[[label]]
      path <- if (is.character(declaration)) declaration else declaration$path
      if (
        !is.character(path) ||
          length(path) != 1L ||
          is.na(path) ||
          !file.exists(path) ||
          dir.exists(path)
      ) {
        stop("Trekker image not found: ", path, call. = FALSE)
      }
      current <- image_declarations[[section]][[label]]$path %||% NULL
      if (
        !is.null(current) &&
          !identical(.trekker_sha256(current), .trekker_sha256(path)) &&
          !label %in% (image_replace[[section]] %||% character())
      ) {
        stop(
          "Dataset '",
          dataset,
          "' has different Trekker image content for ",
          section,
          "/",
          label,
          ". Add that exact identity to `trekker_image_replace`.",
          call. = FALSE
        )
      }
      image_declarations[[section]][[label]] <- declaration
      changed <- TRUE
    }
  }
  supplied_image_settings <- .trekker_named_list(
    supplied_image_settings,
    paste0("`trekker_image_settings` dataset `", dataset, "`")
  )
  for (section in names(supplied_image_settings)) {
    for (label in names(.trekker_named_list(
      supplied_image_settings[[section]],
      paste0("`trekker_image_settings` section `", section, "`")
    ))) {
      if (is.null(image_declarations[[section]][[label]])) {
        stop(
          "Trekker image settings refer to unknown image: ",
          section,
          "/",
          label,
          call. = FALSE
        )
      }
      image_settings[[section]][[label]] <- .normalize_trekker_image_settings(
        supplied_image_settings[[section]][[label]],
        paste0("Trekker image `", section, "/", label, "`"),
        base = image_settings[[section]][[label]] %||% .trekker_image_defaults
      )
      changed <- TRUE
    }
  }
  cells <- object$getCellNames()
  sections <- supplied_sections
  if (!is.null(supplied_sections)) {
    changed <- TRUE
  }
  if (is.null(sections) && !is.null(existing)) {
    sections <- stats::setNames(
      as.character(existing$coordinates$section),
      existing$coordinates$barcode
    )
  }
  payload <- .build_trekker_payload(
    selected,
    cells = cells,
    sidecar_name = target_root,
    declared_by = if (changed) {
      "createShinyApp"
    } else {
      existing$source$declared_by
    },
    trekker_images = image_declarations,
    trekker_sections = sections,
    trekker_image_settings = image_settings
  )
  image_sources <- attr(payload, "trekker_image_sources")
  attr(payload, "trekker_image_sources") <- NULL
  if (!changed) {
    payload$source <- existing$source
  } else if (!is.null(existing$source$spatial_relation)) {
    payload$source$spatial_relation <- existing$source$spatial_relation
  }
  list(payload = payload, files = selected, images = image_sources)
}
