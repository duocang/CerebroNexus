##----------------------------------------------------------------------------##
## Barcode identity, normalized spatial coordinates, and image normalization.
##
## These contracts are pure and are sourced before every Builder consumer.
## Coordinates are matched by barcode before their row order can affect a
## preview or export, and every supported raster becomes RGBA exactly once.
##----------------------------------------------------------------------------##

.builder_match_ids <- function(ids) {
  if (is.null(ids)) {
    return(character())
  }
  as.character(ids)
}

#' Match one cell-associated component to canonical dataset barcodes.
#'
#' Exact matches require the complete dataset identity. Spatial matches may be
#' a subset and may report rows outside the dataset, but only dataset rows are
#' returned in `matched_ids` and `input_index`.
builder_match_cells <- function(ids, expected, mode = c("exact", "subset")) {
  mode <- match.arg(mode)
  ids <- .builder_match_ids(ids)
  expected <- .builder_match_ids(expected)

  blank <- is.na(ids) | !nzchar(ids)
  expected_blank <- is.na(expected) | !nzchar(expected)
  usable <- ids[!blank]
  expected_usable <- expected[!expected_blank]
  duplicates <- unique(usable[duplicated(usable)])
  expected_duplicates <- unique(
    expected_usable[duplicated(expected_usable)]
  )
  missing <- setdiff(unique(expected_usable), unique(usable))
  extra <- setdiff(unique(usable), unique(expected_usable))
  matched_ids <- unique(expected_usable[expected_usable %in% usable])
  input_index <- match(matched_ids, ids)
  denominator <- length(unique(expected_usable))
  coverage <- if (!denominator) {
    1
  } else {
    length(intersect(unique(usable), unique(expected_usable))) / denominator
  }
  structurally_valid <- !any(blank) &&
    !any(expected_blank) &&
    !length(duplicates) &&
    !length(expected_duplicates)
  exact <- structurally_valid &&
    !length(missing) &&
    !length(extra) &&
    length(ids) == length(expected)
  subset <- structurally_valid && length(matched_ids) > 0L
  valid <- if (identical(mode, "exact")) exact else subset
  relation <- if (!valid) {
    "invalid"
  } else if (exact) {
    "full"
  } else {
    "partial"
  }

  list(
    ids = ids,
    expected = expected,
    count = length(ids),
    valid = valid,
    relation = relation,
    duplicates = duplicates,
    expected_duplicates = expected_duplicates,
    blanks = unique(ids[blank]),
    expected_blanks = unique(expected[expected_blank]),
    missing = missing,
    extra = extra,
    order_matches = identical(ids, expected),
    coverage = coverage,
    canonical_ids = expected,
    reorder_index = match(expected, ids),
    matched_ids = matched_ids,
    input_index = input_index
  )
}

.builder_spatial_abort <- function(message) {
  stop(message, call. = FALSE)
}

.builder_spatial_assert_match <- function(match) {
  if (isTRUE(match$valid)) {
    return(invisible(match))
  }
  if (length(match$duplicates) || length(match$expected_duplicates)) {
    .builder_spatial_abort(
      "Spatial cell identity contains duplicate barcodes."
    )
  }
  if (length(match$blanks) || length(match$expected_blanks)) {
    .builder_spatial_abort("Spatial cell identity contains blank barcodes.")
  }
  .builder_spatial_abort(
    "Spatial cell identity has no valid dataset barcode overlap."
  )
}

#' Admit coordinate tables without dispatching container methods.
builder_spatial_coordinate_table <- function(data) {
  invalid <- function() {
    list(valid = FALSE, data = NULL)
  }
  if (isS4(data)) {
    return(invalid())
  }

  explicit_class <- attr(data, "class", exact = TRUE)
  if (identical(explicit_class, "data.frame")) {
    row_names <- attr(data, "row.names", exact = TRUE)
    safe_row_names <- !isS4(row_names) &&
      !is.object(row_names) &&
      typeof(row_names) %in% c("integer", "character")
    if (!safe_row_names) {
      return(invalid())
    }
    return(list(valid = TRUE, data = data))
  }

  dimensions <- attr(data, "dim", exact = TRUE)
  unclassed_matrix <- is.null(explicit_class) &&
    !isS4(dimensions) &&
    !is.object(dimensions) &&
    typeof(dimensions) == "integer" &&
    length(dimensions) == 2L &&
    !anyNA(dimensions) &&
    all(dimensions >= 0L)
  if (!unclassed_matrix) {
    return(invalid())
  }

  converted <- tryCatch(
    base::as.data.frame.matrix(data, stringsAsFactors = FALSE),
    error = function(error) NULL
  )
  if (is.null(converted)) {
    return(invalid())
  }
  list(valid = TRUE, data = converted)
}

#' Read coordinate columns without dispatching methods on classed numerics.
builder_spatial_coordinate_values <- function(data, columns, rows = NULL) {
  coordinate_table <- builder_spatial_coordinate_table(data)
  data <- coordinate_table$data
  valid_table <- coordinate_table$valid &&
    is.character(columns) &&
    length(columns) == 2L &&
    all(columns %in% names(data))
  if (!valid_table) {
    return(list(
      valid = FALSE,
      finite = FALSE,
      values = matrix(numeric(), nrow = 0L, ncol = 0L)
    ))
  }
  coordinate_columns <- lapply(columns, function(column) {
    .subset2(data, column)
  })
  safe_columns <- vapply(
    coordinate_columns,
    function(column) {
      !is.object(column) &&
        !isS4(column) &&
        typeof(column) %in% c("integer", "double")
    },
    logical(1)
  )
  if (!all(safe_columns)) {
    return(list(
      valid = FALSE,
      finite = FALSE,
      values = matrix(numeric(), nrow = 0L, ncol = 0L)
    ))
  }

  values <- matrix(
    unlist(coordinate_columns, recursive = FALSE, use.names = FALSE),
    nrow = nrow(data),
    ncol = 2L
  )
  colnames(values) <- columns
  if (!is.null(rows)) {
    values <- values[rows, , drop = FALSE]
  }
  list(
    valid = TRUE,
    finite = !anyNA(values) && all(is.finite(values)),
    values = values
  )
}

.builder_spatial_image_table <- function(object, image, cells) {
  images <- tryCatch(names(object@images), error = function(error) character())
  if (!length(images)) {
    .builder_spatial_abort("The object contains no spatial images.")
  }
  if (is.null(image) || !length(image) || !image[[1L]] %in% images) {
    image <- images[[1L]]
  } else {
    image <- image[[1L]]
  }
  coordinates <- tryCatch(
    SeuratObject::GetTissueCoordinates(object[[image]]),
    error = function(error) NULL
  )
  coordinate_table <- builder_spatial_coordinate_table(coordinates)
  if (!coordinate_table$valid) {
    .builder_spatial_abort(paste0(
      "The spatial image has no safe coordinate table; coordinates require ",
      "an unclassed base matrix or exact base data frame."
    ))
  }
  coordinates <- coordinate_table$data

  row_barcodes <- rownames(coordinates)
  barcode_column <- .spx_find_barcode_column(coordinates, cells)
  column_barcodes <- if (is.null(barcode_column)) {
    NULL
  } else {
    .spx_contract_barcode_values(coordinates, barcode_column)
  }
  row_overlap <- sum(row_barcodes %in% cells, na.rm = TRUE)
  column_overlap <- if (is.null(column_barcodes)) {
    0L
  } else {
    sum(column_barcodes %in% cells, na.rm = TRUE)
  }
  barcodes <- if (!is.null(column_barcodes) && column_overlap >= row_overlap) {
    column_barcodes
  } else {
    row_barcodes
  }

  list(data = coordinates, barcodes = barcodes, image = image)
}

#' Normalize spatial coordinates once for previews and exports.
#'
#' Metadata coordinates require an explicit two-column selection. Coordinate
#' aliases are inferred only for Seurat image coordinate tables, using the
#' shared bundle-safe coordinate contract.
builder_spatial_contract <- function(
  data,
  cells = NULL,
  coord_cols = NULL,
  barcodes = NULL,
  source = c("metadata", "seurat_image"),
  image = NULL
) {
  is_seurat <- isS4(data) && methods::is(data, "Seurat")
  selected_image <- NULL
  if (is_seurat) {
    object <- data
    cells <- SeuratObject::Cells(object)
    use_image <- is.null(coord_cols) || !is.null(image)
    if (use_image) {
      extracted <- .builder_spatial_image_table(object, image, cells)
      data <- extracted$data
      barcodes <- extracted$barcodes
      selected_image <- extracted$image
      source <- "seurat_image"
    } else {
      data <- object@meta.data
      source <- "metadata"
    }
  } else {
    source <- match.arg(source)
  }

  coordinate_table <- builder_spatial_coordinate_table(data)
  if (!coordinate_table$valid) {
    .builder_spatial_abort(paste0(
      "Spatial coordinates require an unclassed base matrix or exact base ",
      "data frame."
    ))
  }
  data <- coordinate_table$data
  if (!nrow(data)) {
    .builder_spatial_abort("Spatial coordinates require a non-empty table.")
  }
  if (is.null(cells)) {
    .builder_spatial_abort("Spatial coordinates require dataset cell barcodes.")
  }
  if (is.null(barcodes)) {
    barcodes <- rownames(data)
  }
  if (length(barcodes) != nrow(data)) {
    .builder_spatial_abort(
      "Spatial cell identity does not align with coordinate rows."
    )
  }

  if (identical(source, "metadata") && is.null(coord_cols)) {
    .builder_spatial_abort(
      "Metadata spatial mapping requires explicit x and y columns."
    )
  }
  if (
    is.character(coord_cols) &&
      length(coord_cols) == 2L &&
      !anyNA(coord_cols) &&
      identical(coord_cols[[1L]], coord_cols[[2L]])
  ) {
    .builder_spatial_abort(
      "Spatial x and y coordinate columns must be distinct."
    )
  }
  coordinate_match <- .spx_find_coordinate_columns(
    data,
    coord_cols = coord_cols,
    hard_error = TRUE
  )
  if (is.null(coordinate_match)) {
    .builder_spatial_abort(
      "The spatial x and y coordinate columns were not recognized."
    )
  }
  coordinate_columns <- c(
    x = unname(coordinate_match$x),
    y = unname(coordinate_match$y)
  )
  match <- builder_match_cells(barcodes, cells, mode = "subset")
  .builder_spatial_assert_match(match)
  coordinate_values <- builder_spatial_coordinate_values(
    data,
    unname(coordinate_columns),
    rows = match$input_index
  )
  if (!coordinate_values$valid) {
    .builder_spatial_abort(paste0(
      "Spatial coordinates must use unclassed base integer or ",
      "base double values."
    ))
  }
  if (!coordinate_values$finite) {
    .builder_spatial_abort("Spatial coordinates must be numeric and finite.")
  }
  values <- coordinate_values$values

  normalized <- data.frame(
    cell_barcode = match$matched_ids,
    x = as.numeric(values[, 1L]),
    y = as.numeric(values[, 2L]),
    stringsAsFactors = FALSE
  )

  list(
    coordinates = normalized,
    preview = normalized,
    export = normalized,
    match = match,
    coordinate_columns = coordinate_columns,
    source = source,
    image = selected_image
  )
}

.builder_image_error <- function(message) {
  list(error = message)
}

#' Convert grayscale, grayscale-alpha, RGB, or RGBA images to bounded RGBA.
builder_normalize_image <- function(
  image,
  max_display_px,
  display_dimensions = NULL
) {
  valid_limit <- is.numeric(max_display_px) &&
    length(max_display_px) == 1L &&
    !is.na(max_display_px) &&
    is.finite(max_display_px) &&
    max_display_px >= 1 &&
    max_display_px <= .Machine$integer.max
  if (!valid_limit) {
    return(.builder_image_error(
      "Maximum display edge must be one positive finite pixel count."
    ))
  }
  max_display_px <- as.integer(floor(max_display_px))
  dimensions <- dim(image)
  valid_dimensions <- length(dimensions) %in%
    c(2L, 3L) &&
    all(!is.na(dimensions)) &&
    all(dimensions > 0L)
  if (!valid_dimensions) {
    return(.builder_image_error("Image dimensions are invalid."))
  }
  if (!is.numeric(image)) {
    return(.builder_image_error("Image pixels must be numeric."))
  }
  if (anyNA(image)) {
    return(.builder_image_error("Image pixels must be finite."))
  }
  pixel_range <- range(image)
  if (!all(is.finite(pixel_range))) {
    return(.builder_image_error("Image pixels must be finite."))
  }
  if (pixel_range[[1L]] < 0 || pixel_range[[2L]] > 1) {
    return(.builder_image_error("Image pixels must be between 0 and 1."))
  }

  height <- as.integer(dimensions[[1L]])
  width <- as.integer(dimensions[[2L]])
  channels <- if (length(dimensions) == 2L) {
    1L
  } else {
    as.integer(dimensions[[3L]])
  }
  if (!channels %in% 1:4) {
    return(.builder_image_error(
      "Images must have grayscale, grayscale-alpha, RGB, or RGBA channels."
    ))
  }
  channel_kind <- c(
    "grayscale",
    "grayscale_alpha",
    "rgb",
    "rgba"
  )[[channels]]

  if (is.null(display_dimensions)) {
    scale <- min(1, max_display_px / max(height, width))
    display_height <- max(1L, as.integer(round(height * scale)))
    display_width <- max(1L, as.integer(round(width * scale)))
    display_height <- min(display_height, max_display_px)
    display_width <- min(display_width, max_display_px)
  } else {
    valid_display_dimensions <- is.numeric(display_dimensions) &&
      !is.object(display_dimensions) &&
      length(display_dimensions) == 2L &&
      !anyNA(display_dimensions) &&
      all(is.finite(display_dimensions)) &&
      all(display_dimensions >= 1) &&
      all(display_dimensions == floor(display_dimensions))
    if (!valid_display_dimensions) {
      return(.builder_image_error("Display image dimensions are invalid."))
    }
    display_width <- if ("width" %in% names(display_dimensions)) {
      display_dimensions[["width"]]
    } else {
      display_dimensions[[1L]]
    }
    display_height <- if ("height" %in% names(display_dimensions)) {
      display_dimensions[["height"]]
    } else {
      display_dimensions[[2L]]
    }
    if (
      display_width > width ||
        display_height > height ||
        max(display_width, display_height) > max_display_px
    ) {
      return(.builder_image_error("Display image dimensions are invalid."))
    }
    display_width <- as.integer(display_width)
    display_height <- as.integer(display_height)
  }
  rows <- floor((seq_len(display_height) - 0.5) * height / display_height) + 1L
  columns <- floor((seq_len(display_width) - 0.5) * width / display_width) + 1L

  rgba <- array(1, dim = c(display_height, display_width, 4L))
  if (channels == 1L) {
    sampled <- image[rows, columns, drop = FALSE]
    for (channel in 1:3) {
      rgba[,, channel] <- sampled
    }
  } else if (channels == 2L) {
    for (channel in 1:3) {
      rgba[,, channel] <- image[rows, columns, 1L, drop = FALSE]
    }
    rgba[,, 4L] <- image[rows, columns, 2L, drop = FALSE]
  } else {
    for (channel in 1:3) {
      rgba[,, channel] <- image[rows, columns, channel, drop = FALSE]
    }
    if (channels == 4L) {
      rgba[,, 4L] <- image[rows, columns, 4L, drop = FALSE]
    }
  }

  list(
    array = rgba,
    width = display_width,
    height = display_height,
    source_width = width,
    source_height = height,
    display_width = display_width,
    display_height = display_height,
    source_dimensions = c(width = width, height = height),
    display_dimensions = c(
      width = display_width,
      height = display_height
    ),
    source_channels = channels,
    source_channel_kind = channel_kind,
    display_channels = 4L,
    display_channel_kind = "rgba",
    channel_kind = "rgba"
  )
}
