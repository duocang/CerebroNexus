#' Normalize spatial image coordinate bounds
#'
#' @keywords internal
#' @noRd
.spatialImageBounds <- function(bounds, coordinates, context) {
  valid_coordinates <- is.data.frame(coordinates) &&
    all(c("x", "y") %in% colnames(coordinates)) &&
    is.numeric(coordinates[["x"]]) &&
    is.numeric(coordinates[["y"]]) &&
    nrow(coordinates) > 0L
  if (!valid_coordinates) {
    stop(
      context,
      " requires non-empty numeric x and y coordinates.",
      call. = FALSE
    )
  }

  xy <- coordinates[, c("x", "y"), drop = FALSE]
  if (any(!is.finite(as.matrix(xy)))) {
    stop(context, " coordinates must be finite.", call. = FALSE)
  }

  required <- c("xmin", "xmax", "ymin", "ymax")
  if (is.null(bounds)) {
    bounds <- c(
      xmin = min(coordinates[["x"]]),
      xmax = max(coordinates[["x"]]),
      ymin = min(coordinates[["y"]]),
      ymax = max(coordinates[["y"]])
    )
  } else {
    valid_bounds <- is.numeric(bounds) &&
      length(bounds) == 4L &&
      !is.null(names(bounds)) &&
      setequal(names(bounds), required) &&
      !anyDuplicated(names(bounds))
    if (!valid_bounds) {
      stop(
        context,
        " bounds must contain exactly xmin, xmax, ymin, and ymax.",
        call. = FALSE
      )
    }
    bounds <- bounds[required]
  }

  if (any(!is.finite(bounds))) {
    stop(context, " bounds must be finite.", call. = FALSE)
  }
  if (bounds[["xmin"]] >= bounds[["xmax"]]) {
    stop(context, " requires xmin to be less than xmax.", call. = FALSE)
  }
  if (bounds[["ymin"]] >= bounds[["ymax"]]) {
    stop(context, " requires ymin to be less than ymax.", call. = FALSE)
  }

  outside <- coordinates[["x"]] < bounds[["xmin"]] |
    coordinates[["x"]] > bounds[["xmax"]] |
    coordinates[["y"]] < bounds[["ymin"]] |
    coordinates[["y"]] > bounds[["ymax"]]
  if (any(outside)) {
    stop(
      context,
      " has coordinates outside its declared bounds.",
      call. = FALSE
    )
  }

  bounds
}

#' Normalize embedded images for one spatial entry
#'
#' @keywords internal
#' @noRd
.normalizeEmbeddedSpatialImages <- function(images, coordinates, context) {
  if (!is.list(images)) {
    stop(context, " `histology_images` must be a named list.", call. = FALSE)
  }
  if (length(images) == 0L) {
    return(list())
  }

  image_names <- names(images)
  if (
    is.null(image_names) ||
      anyNA(image_names) ||
      any(!nzchar(image_names))
  ) {
    stop(context, " image labels must be non-empty.", call. = FALSE)
  }
  if (anyDuplicated(image_names)) {
    stop(context, " image labels must be unique.", call. = FALSE)
  }

  normalized <- lapply(seq_along(images), function(i) {
    label <- image_names[[i]]
    payload <- images[[i]]
    payload_context <- paste0(context, " image `", label, "`")
    valid_fields <- c("histology_image", "histology_image_bounds")
    if (
      !is.list(payload) ||
        is.null(names(payload)) ||
        !"histology_image" %in% names(payload) ||
        anyDuplicated(names(payload)) ||
        any(!names(payload) %in% valid_fields)
    ) {
      stop(
        payload_context,
        " must contain `histology_image` and optional ",
        "`histology_image_bounds`.",
        call. = FALSE
      )
    }

    image <- payload[["histology_image"]]
    valid_image <- is.character(image) &&
      length(image) == 1L &&
      !is.na(image) &&
      grepl(
        "^data:image/[A-Za-z0-9.+-]+;base64,[A-Za-z0-9+/]+={0,2}$",
        image
      )
    if (!valid_image) {
      stop(
        payload_context,
        " must contain one base64 `data:image/...` URI.",
        call. = FALSE
      )
    }

    list(
      histology_image = image,
      histology_image_bounds = .spatialImageBounds(
        payload[["histology_image_bounds"]],
        coordinates,
        payload_context
      )
    )
  })
  names(normalized) <- image_names
  normalized
}

#' Normalize embedded images in one spatial-data entry
#'
#' @keywords internal
#' @noRd
.normalizeSpatialDataImages <- function(data, spatial_name) {
  if ("histology_images" %in% names(data)) {
    images <- data[["histology_images"]]
  } else if ("histology_image" %in% names(data)) {
    images <- list(
      `Tissue background` = list(
        histology_image = data[["histology_image"]],
        histology_image_bounds = data[["histology_image_bounds"]]
      )
    )
  } else {
    images <- list()
  }

  data[["histology_images"]] <- .normalizeEmbeddedSpatialImages(
    images,
    data[["coordinates"]],
    paste0("Spatial data `", spatial_name, "`")
  )
  data[["histology_image"]] <- NULL
  data[["histology_image_bounds"]] <- NULL
  data
}
