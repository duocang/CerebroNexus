.viewerPackSchemaVersion <- 1L

.viewerPackPath <- function(file) {
  file.path(
    dirname(normalizePath(file, mustWork = FALSE)),
    paste0(tools::file_path_sans_ext(basename(file)), ".viewer")
  )
}

.viewerPackMd5 <- function(path) {
  value <- unname(tools::md5sum(path))
  if (length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop("Could not checksum Viewer Pack input.", call. = FALSE)
  }
  value
}

.viewerPackCellOrderFingerprint <- function(cells) {
  cells <- enc2utf8(as.character(cells))
  if (anyNA(cells) || any(!nzchar(cells)) || anyDuplicated(cells)) {
    stop("Viewer Pack cells must be unique, non-empty strings.", call. = FALSE)
  }
  path <- tempfile("viewer-pack-cell-order-")
  on.exit(unlink(path), add = TRUE)
  writeBin(
    charToRaw(paste0(nchar(cells, type = "bytes"), ":", cells, collapse = "")),
    path
  )
  paste0("md5-cell-order-v1:", .viewerPackMd5(path))
}

.viewerPackCells <- function(object) {
  metadata <- object$getMetaData()
  if (!is.data.frame(metadata) || !("cell_barcode" %in% colnames(metadata))) {
    stop(
      "Viewer Pack requires cell-aligned metadata with `cell_barcode`.",
      call. = FALSE
    )
  }
  as.character(metadata$cell_barcode)
}

.viewerPackWriteAsset <- function(root, path, value, dtype, dimensions = NULL) {
  target <- file.path(root, path)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  extension <- tools::file_ext(path)
  if (identical(extension, "json")) {
    jsonlite::write_json(value, target, auto_unbox = TRUE)
  } else if (identical(extension, "bin")) {
    if (identical(dtype, "uint8")) {
      writeBin(as.raw(value), target)
    } else if (identical(dtype, "uint16")) {
      writeBin(as.integer(value), target, size = 2L, endian = "little")
    } else if (identical(dtype, "uint32")) {
      writeBin(as.integer(value), target, size = 4L, endian = "little")
    } else {
      writeBin(as.numeric(value), target, size = 4L, endian = "little")
    }
  } else {
    qs2::qs_save(value, target)
  }
  data.frame(
    path = path,
    dtype = dtype,
    dimensions = if (is.null(dimensions)) {
      ""
    } else {
      paste(dimensions, collapse = "x")
    },
    bytes = as.numeric(file.info(target)$size),
    checksum = .viewerPackMd5(target),
    stringsAsFactors = FALSE
  )
}

.viewerPackCodeDtype <- function(n) {
  if (n <= 255L) {
    "uint8"
  } else if (n <= 65535L) {
    "uint16"
  } else {
    "uint32"
  }
}

.viewerPackBuildAssets <- function(object, stage) {
  cells <- .viewerPackCells(object)
  assets <- .viewerPackWriteAsset(
    stage,
    "common/cell_order.qs2",
    cells,
    "utf8",
    length(cells)
  )
  modules <- "common"
  projections <- object$availableProjections()
  if (length(projections)) {
    modules <- c(modules, "projections")
    for (i in seq_along(projections)) {
      value <- as.matrix(object$getProjection(projections[[i]]))
      if (!is.numeric(value) || nrow(value) != length(cells)) {
        stop("Viewer Pack projection is not cell-aligned.", call. = FALSE)
      }
      assets <- rbind(
        assets,
        .viewerPackWriteAsset(
          stage,
          file.path("projections", sprintf("%03d.bin", i)),
          as.numeric(t(value)),
          "float32",
          dim(value)
        )
      )
    }
  }
  metadata <- object$getMetaData()
  metadata_names <- setdiff(colnames(metadata), "cell_barcode")
  if (length(metadata_names)) {
    modules <- c(modules, "metadata")
    for (i in seq_along(metadata_names)) {
      value <- metadata[[metadata_names[[i]]]]
      prefix <- file.path("metadata", sprintf("%03d", i))
      if (is.factor(value) || is.character(value) || is.logical(value)) {
        dictionary <- if (is.factor(value)) {
          levels(value)
        } else {
          unique(as.character(value[!is.na(value)]))
        }
        codes <- match(as.character(value), dictionary)
        codes[is.na(codes)] <- 0L
        dtype <- .viewerPackCodeDtype(length(dictionary) + 1L)
        assets <- rbind(
          assets,
          .viewerPackWriteAsset(
            stage,
            paste0(prefix, ".codes.bin"),
            codes,
            dtype,
            length(codes)
          ),
          .viewerPackWriteAsset(
            stage,
            paste0(prefix, ".dictionary.json"),
            as.character(dictionary),
            "utf8",
            length(dictionary)
          )
        )
      } else if (is.numeric(value) || is.integer(value)) {
        assets <- rbind(
          assets,
          .viewerPackWriteAsset(
            stage,
            paste0(prefix, ".bin"),
            value,
            "float32",
            length(value)
          )
        )
      }
    }
  }
  repertoire <- tryCatch(object$getImmuneRepertoire(), error = function(error) {
    list()
  })
  chains <- intersect(hla_detect_chains(repertoire), c("TRA", "TRB"))
  if (length(chains)) {
    modules <- c(modules, "hla_tcr")
    annotated <- hla_annotate_ir_metadata(repertoire, metadata)
    for (chain in chains) {
      value <- hla_parse_ir_segments(annotated, chain)
      if (!is.null(value) && nrow(value)) {
        assets <- rbind(
          assets,
          .viewerPackWriteAsset(
            stage,
            file.path("hla_tcr", paste0(chain, ".qs2")),
            value,
            "table",
            dim(value)
          )
        )
      }
    }
  }
  list(
    assets = assets,
    modules = unique(modules),
    projection_names = projections,
    metadata_names = metadata_names,
    hla_chains = chains
  )
}

.viewerPackInvalid <- function(reason) list(valid = FALSE, reason = reason)

#' Validate a Viewer Pack
#' @param file Path to the canonical CRB.
#' @param pack Optional Viewer Pack directory.
#' @param full Verify every asset size and checksum.
#' @return A validation result.
#' @export
validateViewerPack <- function(
  file,
  pack = .viewerPackPath(file),
  full = TRUE
) {
  if (!file.exists(file) || dir.exists(file)) {
    return(.viewerPackInvalid("CRB is missing"))
  }
  manifest_file <- file.path(pack, "manifest.json")
  if (!dir.exists(pack) || !file.exists(manifest_file)) {
    return(.viewerPackInvalid("Viewer Pack manifest is missing"))
  }
  manifest <- tryCatch(
    jsonlite::read_json(manifest_file, simplifyVector = TRUE),
    error = function(error) NULL
  )
  if (
    !is.list(manifest) ||
      !identical(manifest$schema_version, .viewerPackSchemaVersion)
  ) {
    return(.viewerPackInvalid("Viewer Pack schema is incompatible"))
  }
  if (
    !identical(
      manifest$dataset_fingerprint,
      paste0("md5-crb-v1:", .viewerPackMd5(file))
    )
  ) {
    return(.viewerPackInvalid("Viewer Pack dataset fingerprint mismatch"))
  }
  object <- tryCatch(readCerebro(file), error = function(error) NULL)
  if (is.null(object)) {
    return(.viewerPackInvalid("CRB could not be read"))
  }
  cells <- tryCatch(.viewerPackCells(object), error = function(error) NULL)
  if (
    is.null(cells) || !identical(as.integer(manifest$n_cells), length(cells))
  ) {
    return(.viewerPackInvalid("Viewer Pack cell count mismatch"))
  }
  if (
    !identical(
      manifest$cell_order_fingerprint,
      .viewerPackCellOrderFingerprint(cells)
    )
  ) {
    return(.viewerPackInvalid("Viewer Pack cell-order fingerprint mismatch"))
  }
  assets <- manifest$assets
  if (
    !is.data.frame(assets) ||
      !all(c("path", "bytes", "checksum") %in% names(assets))
  ) {
    return(.viewerPackInvalid("Viewer Pack asset manifest is invalid"))
  }
  paths <- file.path(pack, assets$path)
  if (any(!file.exists(paths)) || any(dir.exists(paths))) {
    return(.viewerPackInvalid("Viewer Pack asset is missing"))
  }
  if (
    isTRUE(full) &&
      !identical(
        unname(as.numeric(file.info(paths)$size)),
        as.numeric(assets$bytes)
      )
  ) {
    return(.viewerPackInvalid("Viewer Pack asset size mismatch"))
  }
  if (
    isTRUE(full) &&
      !identical(unname(tools::md5sum(paths)), as.character(assets$checksum))
  ) {
    return(.viewerPackInvalid("Viewer Pack asset checksum mismatch"))
  }
  list(valid = TRUE, reason = NULL, manifest = manifest, path = pack)
}

#' Read a valid Viewer Pack descriptor
#' @inheritParams validateViewerPack
#' @return A descriptor or `NULL`.
#' @export
readViewerPack <- function(file, pack = .viewerPackPath(file)) {
  result <- validateViewerPack(file, pack, full = TRUE)
  if (isTRUE(result$valid)) result else NULL
}

#' Build a derived Viewer Pack for an existing CRB
#' @param file Existing canonical CRB path.
#' @param viewer_binary One of `"auto"`, `"always"`, or `"never"`.
#' @param viewer_binary_threshold Dataset-level threshold used by `"auto"`.
#' @param overwrite Replace an existing valid or invalid pack.
#' @return The pack path invisibly, or `NULL` when disabled.
#' @export
buildViewerPack <- function(
  file,
  viewer_binary = c("auto", "always", "never"),
  viewer_binary_threshold = 500000L,
  overwrite = FALSE
) {
  viewer_binary <- match.arg(viewer_binary)
  threshold <- suppressWarnings(as.integer(viewer_binary_threshold))
  if (length(threshold) != 1L || is.na(threshold) || threshold < 0L) {
    stop(
      "`viewer_binary_threshold` must be one non-negative integer.",
      call. = FALSE
    )
  }
  file <- normalizePath(file, mustWork = TRUE)
  object <- readCerebro(file)
  cells <- .viewerPackCells(object)
  enabled <- identical(viewer_binary, "always") ||
    (identical(viewer_binary, "auto") && length(cells) >= threshold)
  if (!enabled) {
    return(NULL)
  }
  pack <- .viewerPackPath(file)
  if (dir.exists(pack) && !isTRUE(overwrite)) {
    stop(
      "Viewer Pack already exists; use `overwrite = TRUE` to rebuild it.",
      call. = FALSE
    )
  }
  stage <- tempfile(paste0(".", basename(pack), "-"), dirname(pack))
  if (!dir.create(stage, showWarnings = FALSE)) {
    stop("Could not create the Viewer Pack staging directory.", call. = FALSE)
  }
  published <- FALSE
  backup <- NULL
  on.exit(
    {
      if (dir.exists(stage)) {
        unlink(stage, recursive = TRUE, force = TRUE)
      }
      if (
        !published &&
          !is.null(backup) &&
          dir.exists(backup) &&
          !dir.exists(pack)
      ) {
        file.rename(backup, pack)
      }
    },
    add = TRUE
  )
  built <- .viewerPackBuildAssets(object, stage)
  manifest <- list(
    schema_version = .viewerPackSchemaVersion,
    dataset_fingerprint = paste0("md5-crb-v1:", .viewerPackMd5(file)),
    cell_order_fingerprint = .viewerPackCellOrderFingerprint(cells),
    n_cells = length(cells),
    modules = built$modules,
    projection_names = built$projection_names,
    metadata_names = built$metadata_names,
    hla_chains = built$hla_chains,
    assets = built$assets
  )
  jsonlite::write_json(
    manifest,
    file.path(stage, "manifest.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    dataframe = "rows"
  )
  check <- validateViewerPack(file, stage, full = TRUE)
  if (!isTRUE(check$valid)) {
    stop("Built Viewer Pack failed validation: ", check$reason, call. = FALSE)
  }
  if (dir.exists(pack)) {
    backup <- tempfile(paste0(".", basename(pack), "-backup-"), dirname(pack))
    if (!file.rename(pack, backup)) {
      stop("Could not preserve the existing Viewer Pack.", call. = FALSE)
    }
  }
  if (!file.rename(stage, pack)) {
    stop("Could not publish the Viewer Pack.", call. = FALSE)
  }
  published <- TRUE
  if (!is.null(backup) && dir.exists(backup)) {
    unlink(backup, recursive = TRUE, force = TRUE)
  }
  invisible(pack)
}
