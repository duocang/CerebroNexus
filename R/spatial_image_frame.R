## One coordinate frame for spots and the tissue picture they were measured on.
##
## Visium spot coordinates are full-resolution picture pixels (column, row),
## rows counting downwards, and a stored picture is the full-resolution image
## scaled by its scale factor. The Viewer draws y upwards and a picture's first
## row at `ymax`. Negating y once at import (y = -pxl_row) and placing the
## picture at [0, width / scale] x [-height / scale, 0] puts both in the
## Viewer's frame: the picture needs no flip, and the section is drawn the way
## up it was imaged instead of as its mirror image.
##
## The frame is recorded where it applies, so it can be undone on the way out:
## `metadata(sce)$spatial_coordinate_frame`, Seurat's
## `@misc$spatial_coordinate_frame` (named by section) and each spatial record's
## `coordinate_frame` in the .crb.

#' Name of the frame of negated full-resolution pixel coordinates
#' @keywords internal
#' @noRd
.spatialPixelFrame <- function() {
  "pixels_y_negated"
}

#' TRUE when `frame` names the negated pixel frame
#' @keywords internal
#' @noRd
.isSpatialPixelFrame <- function(frame) {
  identical(frame, .spatialPixelFrame())
}

#' Spot coordinates (x = pixel column, y = pixel row) in the negated frame
#'
#' Negating is its own inverse, so this also takes coordinates back to pixels.
#' @param coordinates A matrix or data frame whose second column is y.
#' @keywords internal
#' @noRd
.spatialPixelFlipY <- function(coordinates) {
  coordinates[, 2L] <- -coordinates[, 2L]
  coordinates
}

#' Bounds of a picture in the negated frame
#'
#' @param width,height The stored picture's size in pixels.
#' @param scale Its scale factor (stored pixels per full-resolution pixel).
#' @keywords internal
#' @noRd
.spatialPixelImageBounds <- function(width, height, scale) {
  if (
    !is.numeric(scale) ||
      length(scale) != 1L ||
      !is.finite(scale) ||
      scale <= 0
  ) {
    stop("A tissue picture's scale factor must be positive.", call. = FALSE)
  }
  c(xmin = 0, xmax = width / scale, ymin = -height / scale, ymax = 0)
}
