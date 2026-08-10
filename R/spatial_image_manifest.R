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
    invalid_label <- if (is.null(image_names)) {
      "<unnamed>"
    } else if (anyNA(image_names)) {
      "<NA>"
    } else {
      "<empty>"
    }
    stop(
      context,
      " image label `",
      invalid_label,
      "` is invalid; labels must be non-empty.",
      call. = FALSE
    )
  }
  if (anyDuplicated(image_names)) {
    duplicate_label <- unique(image_names[duplicated(image_names)])[[1L]]
    stop(
      context,
      " has duplicate image label `",
      duplicate_label,
      "`; labels must be unique.",
      call. = FALSE
    )
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

#' Normalize file-backed image declarations for Seurat spatial entries
#'
#' @keywords internal
#' @noRd
.normalizeSpatialImagePaths <- function(images, spatial_names, context) {
  if (is.null(images)) {
    return(NULL)
  }
  if (!is.list(images)) {
    stop(context, " must be a named list.", call. = FALSE)
  }
  image_spatials <- names(images)
  if (
    is.null(image_spatials) ||
      anyNA(image_spatials) ||
      any(!nzchar(image_spatials))
  ) {
    stop(context, " must use non-empty spatial names.", call. = FALSE)
  }
  if (anyDuplicated(image_spatials)) {
    stop(context, " must use unique spatial names.", call. = FALSE)
  }
  unknown <- setdiff(image_spatials, spatial_names)
  if (length(unknown) > 0L) {
    stop(
      context,
      " spatial `",
      unknown[[1L]],
      "` is not present in `Seurat::Images(object)`.",
      call. = FALSE
    )
  }

  normalized <- lapply(seq_along(images), function(i) {
    spatial_name <- image_spatials[[i]]
    spatial_context <- paste0(context, " spatial `", spatial_name, "`")
    declarations <- images[[i]]
    if (is.character(declarations)) {
      labels <- names(declarations)
      declarations <- lapply(declarations, function(path) list(path = path))
      names(declarations) <- labels
    }
    if (!is.list(declarations)) {
      stop(
        spatial_context,
        " must contain named image declarations.",
        call. = FALSE
      )
    }
    if (length(declarations) == 0L) {
      return(list())
    }
    labels <- names(declarations)
    if (is.null(labels) || anyNA(labels) || any(!nzchar(labels))) {
      invalid_label <- if (is.null(labels)) {
        "<unnamed>"
      } else if (anyNA(labels)) {
        "<NA>"
      } else {
        "<empty>"
      }
      stop(
        spatial_context,
        " image label `",
        invalid_label,
        "` is invalid; labels must be non-empty.",
        call. = FALSE
      )
    }
    if (anyDuplicated(labels)) {
      duplicate_label <- unique(labels[duplicated(labels)])[[1L]]
      stop(
        spatial_context,
        " has duplicate image label `",
        duplicate_label,
        "`; labels must be unique.",
        call. = FALSE
      )
    }

    descriptors <- lapply(seq_along(declarations), function(j) {
      label <- labels[[j]]
      descriptor <- declarations[[j]]
      descriptor_context <- paste0(spatial_context, " image `", label, "`")
      if (is.character(descriptor)) {
        descriptor <- list(path = descriptor)
      }
      valid_descriptor <- is.list(descriptor) &&
        !is.null(names(descriptor)) &&
        "path" %in% names(descriptor) &&
        !anyDuplicated(names(descriptor)) &&
        all(names(descriptor) %in% c("path", "bounds"))
      if (!valid_descriptor) {
        stop(
          descriptor_context,
          " must contain `path` and optional `bounds`.",
          call. = FALSE
        )
      }
      path <- descriptor[["path"]]
      if (
        !is.character(path) ||
          length(path) != 1L ||
          is.na(path) ||
          !nzchar(path)
      ) {
        stop(
          descriptor_context,
          " path must be one non-empty string.",
          call. = FALSE
        )
      }
      if (!file.exists(path)) {
        stop(descriptor_context, " path does not exist: ", path, call. = FALSE)
      }
      if (dir.exists(path) || is.na(file.info(path)$isdir)) {
        stop(
          descriptor_context,
          " path must be a regular file: ",
          path,
          call. = FALSE
        )
      }
      extension <- tolower(tools::file_ext(path))
      if (!(extension %in% c("png", "jpg", "jpeg", "svg"))) {
        stop(
          descriptor_context,
          " must use a png, jpg, jpeg, or svg file.",
          call. = FALSE
        )
      }
      bounds <- descriptor[["bounds"]]
      if (!is.null(bounds)) {
        required <- c("xmin", "xmax", "ymin", "ymax")
        valid_bounds <- is.numeric(bounds) &&
          length(bounds) == 4L &&
          !is.null(names(bounds)) &&
          setequal(names(bounds), required) &&
          !anyDuplicated(names(bounds))
        if (!valid_bounds) {
          stop(
            descriptor_context,
            " bounds must contain exactly xmin, xmax, ymin, and ymax.",
            call. = FALSE
          )
        }
        bounds <- bounds[required]
        midpoint <- data.frame(
          x = mean(c(bounds[["xmin"]], bounds[["xmax"]])),
          y = mean(c(bounds[["ymin"]], bounds[["ymax"]]))
        )
        .spatialImageBounds(bounds, midpoint, descriptor_context)
      }
      compact <- list(path = path)
      if (!is.null(bounds)) {
        compact$bounds <- bounds
      }
      compact
    })
    names(descriptors) <- labels
    descriptors
  })
  names(normalized) <- image_spatials
  normalized
}

#' Encode one file-backed spatial image as a canonical embedded payload
#'
#' @keywords internal
#' @noRd
.encodeSpatialImageDescriptor <- function(descriptor, coordinates, context) {
  extension <- tolower(tools::file_ext(descriptor$path))
  mime <- switch(
    extension,
    png = "image/png",
    jpg = "image/jpeg",
    jpeg = "image/jpeg",
    svg = "image/svg+xml",
    stop(context, " has an unsupported image format.", call. = FALSE)
  )
  list(
    histology_image = paste0(
      "data:",
      mime,
      ";base64,",
      base64enc::base64encode(descriptor$path)
    ),
    histology_image_bounds = .spatialImageBounds(
      descriptor$bounds,
      coordinates,
      context
    )
  )
}

#' Merge misc payloads with file-backed spatial image declarations
#'
#' @keywords internal
#' @noRd
.mergeSpatialImageDeclarations <- function(misc, argument, coordinates = NULL) {
  spatial_names <- union(names(misc), names(argument))
  merged <- setNames(vector("list", length(spatial_names)), spatial_names)
  for (spatial_name in spatial_names) {
    misc_images <- misc[[spatial_name]]
    argument_images <- argument[[spatial_name]]
    conflicts <- intersect(names(misc_images), names(argument_images))
    if (length(conflicts) > 0L) {
      stop(
        "Spatial `",
        spatial_name,
        "` image label `",
        conflicts[[1L]],
        "` is declared in both `object@misc$cerebro_spatial_images` and ",
        "`spatial_images`.",
        call. = FALSE
      )
    }
    if (is.null(coordinates) || is.null(coordinates[[spatial_name]])) {
      merged[[spatial_name]] <- c(misc_images, argument_images)
      next
    }
    coords <- coordinates[[spatial_name]]
    normalized_misc <- if (length(misc_images) > 0L) {
      .normalizeEmbeddedSpatialImages(
        misc_images,
        coords,
        paste0("Spatial image payload `", spatial_name, "`")
      )
    } else {
      list()
    }
    encoded_argument <- lapply(names(argument_images), function(label) {
      .encodeSpatialImageDescriptor(
        argument_images[[label]],
        coords,
        paste0(
          "`spatial_images` spatial `",
          spatial_name,
          "` image `",
          label,
          "`"
        )
      )
    })
    names(encoded_argument) <- names(argument_images)
    merged[[spatial_name]] <- c(normalized_misc, encoded_argument)
  }
  merged
}
