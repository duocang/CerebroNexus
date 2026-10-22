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
  for (i in seq_along(payloads)) {
    images <- payloads[[i]]
    if (is.list(images) && is.character(images[["histology_image"]])) {
      stop(
        "Spatial image payload `",
        payload_names[[i]],
        "` uses the singular `histology_image` / `histology_image_bounds` ",
        "fields, which are no longer accepted. Declare each image under a ",
        "label: `object@misc$cerebro_spatial_images$",
        payload_names[[i]],
        " <- list(<label> = list(histology_image = <data URI>, ",
        "histology_image_bounds = <bounds>))`.",
        call. = FALSE
      )
    }
  }
  payloads
}
