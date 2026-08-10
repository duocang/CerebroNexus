#' Validate embedded spatial-image declarations
#'
#' @keywords internal
#' @noRd
.isLegacySpatialImagePayload <- function(payload) {
  if (!is.list(payload) || is.null(names(payload))) {
    return(FALSE)
  }
  valid_fields <- c("histology_image", "histology_image_bounds")
  image <- payload[["histology_image"]]
  bounds <- payload[["histology_image_bounds"]]
  valid_image <- is.character(image) && length(image) == 1L && !is.na(image)
  valid_bounds <- is.null(bounds) ||
    (is.numeric(bounds) && length(bounds) == 4L)
  !anyDuplicated(names(payload)) &&
    all(names(payload) %in% valid_fields) &&
    "histology_image" %in% names(payload) &&
    valid_image &&
    valid_bounds
}

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
  lapply(payloads, function(images) {
    if (.isLegacySpatialImagePayload(images)) {
      return(list(`Tissue background` = images))
    }
    images
  })
}

#' Validate one embedded spatial image and its coordinate bounds
#'
#' @keywords internal
#' @noRd
.validateCerebroSpatialImage <- function(payload, image_name, coordinates) {
  if (.isLegacySpatialImagePayload(payload)) {
    payload <- list(`Tissue background` = payload)
  }
  list(
    histology_images = .normalizeEmbeddedSpatialImages(
      payload,
      coordinates,
      paste0("Spatial image payload `", image_name, "`")
    )
  )
}
