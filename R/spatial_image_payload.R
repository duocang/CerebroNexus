#' Validate embedded spatial-image declarations
#'
#' @keywords internal
#' @noRd
.validateCerebroSpatialImages <- function(payloads, available_images) {
  if (is.null(payloads)) {
    return(NULL)
  }
  if (!is.list(payloads)) {
    stop(
      "`object@misc$cerebro_spatial_images` must be a named list.",
      call. = FALSE
    )
  }
  payload_names <- names(payloads)
  if (
    is.null(payload_names) ||
      anyNA(payload_names) ||
      any(!nzchar(payload_names))
  ) {
    stop(
      "`object@misc$cerebro_spatial_images` must use non-empty image names.",
      call. = FALSE
    )
  }
  if (anyDuplicated(payload_names)) {
    stop(
      "`object@misc$cerebro_spatial_images` must use unique image names.",
      call. = FALSE
    )
  }
  unknown <- setdiff(payload_names, available_images)
  if (length(unknown) > 0L) {
    stop(
      "Spatial image payload `",
      unknown[[1L]],
      "` is not present in `Seurat::Images(object)`.",
      call. = FALSE
    )
  }
  payloads
}

#' Validate one embedded spatial image and its coordinate bounds
#'
#' @keywords internal
#' @noRd
.validateCerebroSpatialImage <- function(payload, image_name, coordinates) {
  required <- c("histology_image", "histology_image_bounds")
  if (
    !is.list(payload) ||
      is.null(names(payload)) ||
      !setequal(names(payload), required) ||
      anyDuplicated(names(payload))
  ) {
    stop(
      "Spatial image payload `",
      image_name,
      "` must contain exactly `histology_image` and ",
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
      "Spatial image payload `",
      image_name,
      "` must contain one base64 `data:image/...` URI.",
      call. = FALSE
    )
  }

  bounds <- payload[["histology_image_bounds"]]
  required_bounds <- c("xmin", "xmax", "ymin", "ymax")
  if (
    !is.numeric(bounds) ||
      length(bounds) != 4L ||
      is.null(names(bounds)) ||
      !setequal(names(bounds), required_bounds) ||
      anyDuplicated(names(bounds))
  ) {
    stop(
      "Spatial image payload `",
      image_name,
      "` bounds must contain exactly xmin, xmax, ymin, and ymax.",
      call. = FALSE
    )
  }
  bounds <- bounds[required_bounds]
  if (any(!is.finite(bounds))) {
    stop(
      "Spatial image payload `",
      image_name,
      "` bounds must be finite.",
      call. = FALSE
    )
  }
  if (bounds[["xmin"]] >= bounds[["xmax"]]) {
    stop(
      "Spatial image payload `",
      image_name,
      "` requires xmin to be less than xmax.",
      call. = FALSE
    )
  }
  if (bounds[["ymin"]] >= bounds[["ymax"]]) {
    stop(
      "Spatial image payload `",
      image_name,
      "` requires ymin to be less than ymax.",
      call. = FALSE
    )
  }

  valid_coordinates <- is.data.frame(coordinates) &&
    all(c("x", "y") %in% colnames(coordinates)) &&
    is.numeric(coordinates[["x"]]) &&
    is.numeric(coordinates[["y"]]) &&
    nrow(coordinates) > 0L
  if (!valid_coordinates) {
    stop(
      "Spatial image payload `",
      image_name,
      "` requires non-empty numeric x and y coordinates.",
      call. = FALSE
    )
  }
  xy <- coordinates[, c("x", "y"), drop = FALSE]
  if (any(!is.finite(as.matrix(xy)))) {
    stop(
      "Spatial image payload `",
      image_name,
      "` coordinates must be finite.",
      call. = FALSE
    )
  }
  outside <- coordinates[["x"]] < bounds[["xmin"]] |
    coordinates[["x"]] > bounds[["xmax"]] |
    coordinates[["y"]] < bounds[["ymin"]] |
    coordinates[["y"]] > bounds[["ymax"]]
  if (any(outside)) {
    stop(
      "Spatial image payload `",
      image_name,
      "` has coordinates outside its declared bounds.",
      call. = FALSE
    )
  }

  payload[required]
}
