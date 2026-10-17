.setExportArtifactMode <- function(path, mode, label) {
  if (identical(.Platform$OS.type, "windows")) {
    return(invisible(TRUE))
  }
  expected <- bitwAnd(as.integer(as.octmode(mode)), 511L)
  changed <- suppressWarnings(Sys.chmod(path, mode = as.octmode(expected)))
  actual <- bitwAnd(as.integer(file.info(path)$mode), 511L)
  if (!isTRUE(changed) || is.na(actual) || actual != expected) {
    stop(
      "Failed to set private permissions on ",
      label,
      ": ",
      path,
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.exportStageFailureMessage <- function(
  final_dir,
  stage,
  writable = file.access(final_dir, mode = 2L) == 0L,
  os_type = .Platform$OS.type
) {
  if (!isTRUE(writable)) {
    return(paste(
      "CRB preparation could not create a temporary folder because the output folder is not writable.",
      "Save the Project in a writable local folder and try again."
    ))
  }
  if (
    identical(os_type, "windows") &&
      nchar(stage, type = "bytes") >= 240L
  ) {
    return(paste(
      "CRB preparation could not create a temporary folder because the Project path is too long for this Windows setup.",
      "Move the Project to a shorter folder and try again."
    ))
  }
  paste(
    "CRB preparation could not create a temporary folder because the filesystem rejected it.",
    "Save the Project in a writable local folder instead of a synced or network folder, then try again."
  )
}

.createPrivateExportStage <- function(final_file) {
  final_dir <- dirname(final_file)
  if (!dir.exists(final_dir)) {
    dir.create(final_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(final_dir)) {
    stop("Failed to create output directory: ", final_dir, call. = FALSE)
  }

  stage <- tempfile(
    pattern = ".crb-stage-",
    tmpdir = final_dir
  )
  creation_warning <- NULL
  created <- withCallingHandlers(
    dir.create(stage, mode = "0700", showWarnings = TRUE),
    warning = function(warning) {
      creation_warning <<- conditionMessage(warning)
      invokeRestart("muffleWarning")
    }
  )
  if (!isTRUE(created)) {
    diagnostic <- if (is.null(creation_warning)) {
      "dir.create() returned FALSE without a warning."
    } else {
      creation_warning
    }
    message(
      "CRB staging diagnostic: ",
      diagnostic,
      " Attempted path: ",
      stage
    )
    stop(.exportStageFailureMessage(final_dir, stage), call. = FALSE)
  }
  tryCatch(
    .setExportArtifactMode(stage, "0700", "the export staging directory"),
    error = function(error) {
      unlink(stage, recursive = TRUE, force = TRUE)
      stop(error)
    }
  )
  stage
}

.exportSidecarName <- function(final_file, expression_matrix_mode) {
  suffix <- switch(
    expression_matrix_mode,
    h5 = ".h5",
    bpcells = ".bpcells",
    stop("Only external backends have a sidecar name.", call. = FALSE)
  )
  stem <- tools::file_path_sans_ext(basename(final_file))
  paste0(stem, suffix)
}

.writeBpcellsGeneMajor <- function(matrix, dir) {
  if (identical(BPCells::storage_order(matrix), "row")) {
    BPCells::write_matrix_dir(matrix, dir = dir)
    return(BPCells::open_matrix_dir(dir))
  }
  BPCells::transpose_storage_order(matrix, outdir = dir)
}

.validatePortableExportBasename <- function(final_file) {
  name <- basename(final_file)
  stem <- tools::file_path_sans_ext(name)
  windows_stem <- toupper(trimws(
    sub("\\..*$", "", name),
    which = "right"
  ))
  invalid_windows_name <-
    windows_stem %in%
    c("CON", "PRN", "AUX", "NUL") ||
    grepl("^(COM|LPT)[1-9]$", windows_stem)
  if (
    !nzchar(stem) ||
      identical(tolower(name), ".crb") ||
      name %in% c(".", "..") ||
      grepl("[[:cntrl:]<>:\"/\\\\|?*]", name) ||
      grepl("[. ]$", name) ||
      invalid_windows_name
  ) {
    stop(
      "External exports require a portable file name; reserved names, an empty ",
      "stem, control characters, and Windows-invalid punctuation are not ",
      "allowed: ",
      name,
      call. = FALSE
    )
  }
  invisible(name)
}

.validateExportSidecarName <- function(location) {
  if (
    !is.character(location) ||
      length(location) != 1L ||
      is.na(location) ||
      !nzchar(location) ||
      location %in% c(".", "..") ||
      grepl("[/\\\\]", location)
  ) {
    stop(
      "The expression sidecar location must be one relative file name.",
      call. = FALSE
    )
  }
  invisible(location)
}

.readPublishedExportBackend <- function(final_file) {
  if (!file.exists(final_file) || dir.exists(final_file)) {
    return(NULL)
  }
  object <- tryCatch(.readCerebroPayload(final_file), error = function(error) {
    NULL
  })
  if (
    !is.environment(object) ||
      !any(grepl("^Cerebro", class(object))) ||
      !exists("expression_backend", envir = object, inherits = FALSE) ||
      bindingIsActive("expression_backend", object) ||
      isTRUE(rlang::env_binding_are_lazy(object, "expression_backend"))
  ) {
    return(NULL)
  }

  backend <- object[["expression_backend"]]
  if (
    !is.list(backend) ||
      !is.character(backend$type) ||
      length(backend$type) != 1L ||
      is.na(backend$type) ||
      !(backend$type %in% c("embedded", "h5", "bpcells"))
  ) {
    return(NULL)
  }
  if (identical(backend$type, "embedded")) {
    return(list(type = "embedded", location = NULL))
  }
  invalid_location <- tryCatch(
    {
      .validateExportSidecarName(backend$location)
      FALSE
    },
    error = function(error) TRUE
  )
  if (invalid_location) {
    return(NULL)
  }
  list(type = backend$type, location = backend$location)
}

.readPublishedSpatialBackend <- function(final_file) {
  if (!file.exists(final_file) || dir.exists(final_file)) {
    return(NULL)
  }
  object <- tryCatch(.readCerebroPayload(final_file), error = function(error) {
    NULL
  })
  if (
    !is.environment(object) ||
      !exists("spatial_molecule_backend", envir = object, inherits = FALSE) ||
      bindingIsActive("spatial_molecule_backend", object) ||
      isTRUE(rlang::env_binding_are_lazy(object, "spatial_molecule_backend"))
  ) {
    return(NULL)
  }
  backend <- object[["spatial_molecule_backend"]]
  valid <- is.list(backend) &&
    identical(backend$type, "directory") &&
    is.character(backend$location) &&
    length(backend$location) == 1L &&
    !is.na(backend$location) &&
    nzchar(backend$location) &&
    !backend$location %in% c(".", "..") &&
    !grepl("[/\\\\]", backend$location)
  if (!valid) NULL else backend[c("type", "location")]
}

.publishCerebroExport <- function(
  export,
  final_file,
  stage_dir,
  expression_matrix_mode,
  codec = "rds",
  .open_gz = gzfile
) {
  final_dir <- dirname(final_file)
  if (!dir.exists(final_dir)) {
    dir.create(final_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(final_dir)) {
    stop("Failed to create output directory: ", final_dir, call. = FALSE)
  }

  stage_crb <- file.path(stage_dir, basename(final_file))
  backend <- export$getExpressionBackend()
  if (
    !is.list(backend) ||
      !is.character(backend$type) ||
      length(backend$type) != 1L ||
      is.na(backend$type) ||
      !identical(backend$type, expression_matrix_mode)
  ) {
    stop(
      "The staged expression backend does not match expression_matrix_mode.",
      call. = FALSE
    )
  }
  previous_backend <- .readPublishedExportBackend(final_file)
  previous_spatial_backend <- .readPublishedSpatialBackend(final_file)
  stage_sidecar <- NULL
  final_sidecar <- NULL
  stage_spatial <- NULL
  final_spatial <- NULL
  if (!identical(backend$type, "embedded")) {
    .validateExportSidecarName(backend$location)
    expected_location <- .exportSidecarName(final_file, backend$type)
    if (!identical(backend$location, expected_location)) {
      stop(
        "The staged expression backend must use the fixed sidecar name `",
        expected_location,
        "`.",
        call. = FALSE
      )
    }
    stage_sidecar <- file.path(stage_dir, backend$location)
    final_sidecar <- file.path(final_dir, backend$location)
    if (
      .pathIsSymbolicLink(final_sidecar) ||
        .pathIsSymbolicLink(final_file)
    ) {
      stop(
        "Refusing to replace a symbolic-link export target.",
        call. = FALSE
      )
    }
  } else if (.pathIsSymbolicLink(final_file)) {
    stop(
      "Refusing to replace a symbolic-link export target.",
      call. = FALSE
    )
  }

  old_crb_backup <- NULL
  old_sidecar_backup <- NULL
  old_spatial_backup <- NULL
  retired_sidecar <- NULL
  if (
    !is.null(previous_backend) &&
      previous_backend$type %in% c("h5", "bpcells") &&
      (identical(backend$type, "embedded") ||
        !identical(previous_backend$location, backend$location)) &&
      identical(
        previous_backend$location,
        .exportSidecarName(final_file, previous_backend$type)
      )
  ) {
    retired_sidecar <- file.path(final_dir, previous_backend$location)
  }
  old_crb_mode <- if (file.exists(final_file)) {
    as.octmode(bitwAnd(as.integer(file.info(final_file)$mode), 511L))
  } else {
    as.octmode("0600")
  }
  installed_crb <- FALSE
  installed_sidecar <- FALSE
  installed_spatial <- FALSE
  committed <- FALSE
  on.exit(
    {
      if (!committed) {
        if (identical(backend$type, "bpcells")) {
          ## The published-path handle is reopened before CRB serialisation.
          ## Release it before rollback touches the directory: Windows will
          ## otherwise refuse both removal of the failed sidecar and restore
          ## of the previous one.
          try(export$expression <- NULL, silent = TRUE)
          if (exists("payload", inherits = FALSE)) {
            try(payload$expression <- NULL, silent = TRUE)
          }
          invisible(gc(full = TRUE))
        }
        if (installed_crb && file.exists(final_file)) {
          unlink(final_file, force = TRUE)
        }
        if (!is.null(old_crb_backup) && file.exists(old_crb_backup)) {
          if (!file.rename(old_crb_backup, final_file)) {
            warning(
              "Export rollback could not restore the previous .crb from: ",
              old_crb_backup,
              call. = FALSE
            )
          }
        }
        if (
          installed_sidecar &&
            (file.exists(final_sidecar) || dir.exists(final_sidecar))
        ) {
          unlink(final_sidecar, recursive = TRUE, force = TRUE)
        }
        if (
          !is.null(old_sidecar_backup) &&
            (file.exists(old_sidecar_backup) ||
              dir.exists(old_sidecar_backup))
        ) {
          if (!file.rename(old_sidecar_backup, final_sidecar)) {
            warning(
              "Export rollback could not restore the previous expression ",
              "sidecar from: ",
              old_sidecar_backup,
              call. = FALSE
            )
          }
        }
        if (installed_spatial && dir.exists(final_spatial)) {
          unlink(final_spatial, recursive = TRUE, force = TRUE)
        }
        if (!is.null(old_spatial_backup) && dir.exists(old_spatial_backup)) {
          if (!file.rename(old_spatial_backup, final_spatial)) {
            warning(
              "Export rollback could not restore the previous spatial ",
              "molecule sidecar from: ",
              old_spatial_backup,
              call. = FALSE
            )
          }
        }
      }
    },
    add = TRUE
  )

  if (!is.null(stage_sidecar)) {
    if (.pathIsSymbolicLink(stage_sidecar)) {
      stop(
        "Refusing to publish a symbolic-link staged expression sidecar: ",
        stage_sidecar,
        call. = FALSE
      )
    }
    if (!(file.exists(stage_sidecar) || dir.exists(stage_sidecar))) {
      stop(
        "The staged ",
        backend$type,
        " expression matrix is missing.",
        call. = FALSE
      )
    }
    if (identical(backend$type, "h5") && dir.exists(stage_sidecar)) {
      stop("The staged h5 sidecar must be a regular file.", call. = FALSE)
    }
    if (identical(backend$type, "bpcells") && !dir.exists(stage_sidecar)) {
      stop("The staged bpcells sidecar must be a directory.", call. = FALSE)
    }
    if (.pathIsSymbolicLink(final_sidecar)) {
      stop(
        "Refusing to replace a symbolic-link expression sidecar: ",
        final_sidecar,
        call. = FALSE
      )
    }
    if (identical(backend$type, "bpcells")) {
      ## transpose_storage_order() keeps native file handles open until its
      ## MatrixDir external pointer is finalized. Windows will not rename the
      ## staged directory while those handles are live, so drop the object's
      ## last reference and force finalization before either sidecar move.
      export$expression <- NULL
      invisible(gc(full = TRUE))
    }
    if (file.exists(final_sidecar) || dir.exists(final_sidecar)) {
      owns_target <-
        !is.null(previous_backend) &&
        identical(previous_backend$type, backend$type) &&
        identical(previous_backend$location, backend$location)
      if (!owns_target) {
        stop(
          "Refusing to replace an existing expression sidecar that is not ",
          "owned by the published CRB: ",
          final_sidecar,
          call. = FALSE
        )
      }
      old_sidecar_backup <- tempfile(
        pattern = ".side-b-",
        tmpdir = final_dir
      )
      if (!file.rename(final_sidecar, old_sidecar_backup)) {
        stop(
          "Failed to preserve the previous expression sidecar.",
          call. = FALSE
        )
      }
    }
    sidecar_mode <- if (dir.exists(stage_sidecar)) "0700" else "0600"
    .setExportArtifactMode(
      stage_sidecar,
      sidecar_mode,
      "the staged expression sidecar"
    )
    if (!file.rename(stage_sidecar, final_sidecar)) {
      stop("Failed to install the staged expression sidecar.", call. = FALSE)
    }
    installed_sidecar <- TRUE

    ## A BPCells handle serialises its absolute directory. Reopen it only after
    ## the staged directory has reached its published path, then serialise the
    ## CRB. H5 stores no live handle and needs no equivalent step.
    if (identical(expression_matrix_mode, "bpcells")) {
      export$setExpression(
        BPCells::open_matrix_dir(dir = final_sidecar),
        backend = "external"
      )
    }
  }

  payload <- if (.recognizedCerebroObject(export)) {
    .thinCerebroPayload(
      export,
      final_file,
      sidecar = if (identical(backend$type, "bpcells")) final_sidecar else NULL
    )
  } else {
    export
  }
  spatial <- .stageSpatialMolecules(payload, stage_crb, codec)
  payload <- spatial$payload
  stage_spatial <- spatial$stage
  if (!is.null(spatial$location)) {
    final_spatial <- file.path(final_dir, spatial$location)
    if (.pathIsSymbolicLink(final_spatial)) {
      stop(
        "Refusing to replace a symbolic-link spatial sidecar.",
        call. = FALSE
      )
    }
    if (dir.exists(final_spatial)) {
      owns_target <- !is.null(previous_spatial_backend) &&
        identical(previous_spatial_backend$location, spatial$location)
      if (!owns_target) {
        stop(
          "Refusing to replace an existing spatial sidecar that is not owned ",
          "by the published CRB: ",
          final_spatial,
          call. = FALSE
        )
      }
      old_spatial_backup <- tempfile(
        pattern = ".spatial-b-",
        tmpdir = final_dir
      )
      if (!file.rename(final_spatial, old_spatial_backup)) {
        stop("Failed to preserve the previous spatial sidecar.", call. = FALSE)
      }
    }
    .setExportArtifactMode(stage_spatial, "0700", "the spatial sidecar")
    if (!file.rename(stage_spatial, final_spatial)) {
      stop("Failed to install the staged spatial sidecar.", call. = FALSE)
    }
    installed_spatial <- TRUE
  }
  if (identical(codec, "rds")) {
    connection <- .open_gz(stage_crb, open = "wb", compression = 1L)
    tryCatch(saveRDS(payload, connection), finally = close(connection))
  } else {
    .writeCerebroPayload(payload, stage_crb, codec)
  }
  if (!file.exists(stage_crb)) {
    stop("Failed to serialise the staged Cerebro object.", call. = FALSE)
  }
  .setExportArtifactMode(stage_crb, old_crb_mode, "the staged Cerebro object")

  if (file.exists(final_file)) {
    old_crb_backup <- tempfile(
      pattern = ".crb-b-",
      tmpdir = final_dir
    )
    if (!file.rename(final_file, old_crb_backup)) {
      stop("Failed to preserve the previous .crb file.", call. = FALSE)
    }
  }
  if (!file.rename(stage_crb, final_file)) {
    stop("Failed to publish the staged .crb file.", call. = FALSE)
  }
  installed_crb <- TRUE
  .setExportArtifactMode(
    final_file,
    old_crb_mode,
    "the published Cerebro object"
  )
  committed <- TRUE

  for (backup in c(old_crb_backup, old_sidecar_backup, old_spatial_backup)) {
    if (
      !is.null(backup) &&
        (file.exists(backup) || dir.exists(backup))
    ) {
      status <- unlink(backup, recursive = TRUE, force = TRUE)
      if (!identical(status, 0L)) {
        warning(
          "The new export was published, but an old backup remains at: ",
          backup,
          call. = FALSE
        )
      }
    }
  }
  if (
    !is.null(retired_sidecar) &&
      (file.exists(retired_sidecar) || dir.exists(retired_sidecar))
  ) {
    if (.pathIsSymbolicLink(retired_sidecar)) {
      warning(
        "The previous sidecar is a symbolic link and was not removed: ",
        retired_sidecar,
        call. = FALSE
      )
    } else {
      status <- unlink(retired_sidecar, recursive = TRUE, force = TRUE)
      if (!identical(status, 0L)) {
        warning(
          "The new export was published, but the previous sidecar remains at: ",
          retired_sidecar,
          call. = FALSE
        )
      }
    }
  }
  invisible(final_file)
}

.spx_export_projection_coordinates <- function(coordinates) {
  if (!is.data.frame(coordinates)) {
    return(NULL)
  }
  coordinate_columns <- .spx_find_coordinate_columns(coordinates)
  if (is.null(coordinate_columns)) {
    return(NULL)
  }
  projection <- coordinates[,
    c(coordinate_columns$x, coordinate_columns$y),
    drop = FALSE
  ]
  names(projection) <- c("x", "y")
  projection
}

#' @title
#' Export Seurat object to Cerebro.
#'
#' @description
#' This function allows to export a Seurat object to visualize in Cerebro.
#'
#' @param object Seurat object.
#' @param assay Assay to pull expression values from; defaults to \code{RNA}.
#' @param slot Slot to pull expression values from; defaults to \code{data}. It
#' is recommended to use sparse data (such as log-transformed or raw counts)
#' instead of dense data (such as the \code{scaled} slot) to avoid performance
#' bottlenecks in the Cerebro interface.
#' @param file Where to save the output. External backends require a
#'   \code{.crb} filename and store the matrix under a sibling name derived
#'   from the stem.
#' @param experiment_name Experiment name.
#' @param organism Organism, e.g. \code{hg} (human), \code{mm} (mouse), etc.
#' @param groups Names of grouping variables in meta data
#' (\code{object@meta.data}), e.g. \code{c("sample","cluster")}; at least one
#' must be provided; defaults to \code{NULL}.
#' @param main_group The primary grouping variable to use for display in Cerebro;
#' must be one of the grouping variables specified in \code{groups}; defaults to
#' \code{NULL}.
#' @param cell_cycle Names of columns in meta data
#' (\code{object@meta.data}) that contain cell cycle information, e.g.
#' \code{c("Phase")}; defaults to \code{NULL}.
#' @param nUMI Column in \code{object@meta.data} that contains information about
#' number of transcripts per cell; defaults to \code{nUMI}.
#' @param nGene Column in \code{object@meta.data} that contains information
#' about number of expressed genes per cell; defaults to \code{nGene}.
#' @param add_all_meta_data If set to \code{TRUE}, all further meta data columns
#' will be extracted as well.
#' @param use_delayed_array When set to \code{TRUE}, the expression matrix will
#' be stored as an \code{RleMatrix} (see \code{DelayedArray} package). This can
#' be useful for very large data sets, as the matrix won't be loaded into memory
#' and instead values will be read from the disk directly, at the cost of
#' performance. Note that it is necessary to install the \code{DelayedArray}
#' package. If set to \code{FALSE} (default), the expression matrix will be
#' copied from the input object as is. It is recommended to use a sparse format,
#' such as \code{dgCMatrix} from the \code{Matrix} package. Ignored when
#' \code{expression_matrix_mode} is set to an external backend.
#' @param expression_matrix_mode How to persist the expression matrix. One of
#' \code{"embedded"} (default), \code{"bpcells"}, or \code{"h5"}.
#' \itemize{
#'   \item \code{"embedded"} stores the matrix inside the \code{.crb} file, as
#'   before. Compatible with all existing \code{.crb} readers.
#'   \item \code{"bpcells"} writes the matrix to a BPCells on-disk directory
#'   next to the \code{.crb} and keeps only a lightweight handle in the
#'   serialised object. Recommended for large sparse matrices. The resulting
#'   \code{.crb} is portable as long as the sibling \code{.bpcells/} directory
#'   travels with it; the Shiny runtime re-resolves paths via
#'   \code{getExpressionBackend()$location} relative to the \code{.crb}'s
#'   parent directory (step 7.3 runtime attach).
#'   Delayed/Rle matrix inputs additionally require \pkg{HDF5Array}; they use
#'   a temporary block-written HDF5 file instead of a full dense realization.
#'   \item \code{"h5"} writes the matrix via \code{HDF5Array::writeTENxMatrix()}
#'   to a TENx-format sparse HDF5 file next to the \code{.crb} (sibling
#'   \code{<stem>.h5}) and tags the backend with that relative location. The
#'   on-disk layout matches \code{inst/extdata/examples/example.h5}: a single
#'   \code{/expression} group with \code{data}, \code{indices}, \code{indptr},
#'   \code{shape}, \code{genes}, and \code{barcodes} datasets. The matrix is
#'   stored cells x genes (TENx column-favoured, optimised for per-gene
#'   reads); the Shiny runtime attach reads it back as a lazy
#'   \code{HDF5Array::TENxMatrix} seed and transposes it lazily to Cerebro's
#'   internal genes x cells layout via \code{DelayedArray::t()} (free). The
#'   in-memory \code{dgCMatrix} is never materialised on attach, so RAM stays
#'   close to the \code{.crb} metadata size. Requires the \pkg{HDF5Array}
#'   package.
#' }
#' The CRB and any sidecar are built and validated in a private sibling stage.
#' On POSIX systems, new stages and external matrices are owner-only; replacing
#' a CRB preserves its existing mode. An existing sidecar is replaced only when
#' the current CRB identifies that exact path as its backend; backend changes
#' remove the previous owned sidecar after the new CRB is committed. Stop all
#' readers before replacing an existing export, because a reader can otherwise
#' observe the two-path replacement between steps.
#' Ordinary R errors trigger best-effort restoration. Process termination and
#' concurrent writers remain outside this multi-path transaction guarantee.
#' Only POSIX mode bits are set or preserved; ownership, ACLs, extended
#' attributes, and security labels remain the deployment system's
#' responsibility on every platform.
#' @param codec Serialization codec for the CRB payload. Defaults to
#' \code{"qs2"}; use \code{"rds"} when direct compatibility with
#' \code{readRDS()} is required.
#' @param viewer_binary Build the derived Viewer Pack \code{"auto"}matically
#' for large datasets, \code{"always"}, or \code{"never"}.
#' @param viewer_binary_threshold Dataset-level cell threshold used by
#' \code{viewer_binary = "auto"}.
#' @param spatial_images Optional named list mapping Seurat image names to named
#'   image paths or descriptors of the form \code{list(path = ..., bounds = ...)}.
#'   Supported file extensions are png, jpg, jpeg, and svg. Missing bounds are
#'   derived from the exported x/y coordinate range.
#' @param verbose Set this to \code{TRUE} if you want additional log messages;
#' defaults to \code{FALSE}.
#' @param .expression_resolution Internal handoff used by
#' \code{convertSeuratToCerebro()} to reuse a matrix that has already been
#' resolved and validated. Users should leave this as \code{NULL}.
#' @param projections Optional ordered names of dimensional reductions to
#'   export. When supplied, every named reduction is exported in that order,
#'   including PCA beside other projections. The default \code{NULL} preserves
#'   the legacy behavior that uses non-PCA reductions when they are available.
#' @param spatial_coordinate_transforms Optional named list of coordinate
#'   transforms keyed by exact Seurat spatial FOV/image name. Each entry may
#'   specify \code{rotation_degrees} and positive uniform \code{scale}; the
#'   pivot is the coordinate bounds center. The transform is applied once to
#'   the exported spatial coordinates and its normalized provenance is stored
#'   in the corresponding CRB spatial record. \code{NULL} preserves
#'   coordinates exactly as extracted from Seurat.
#'
#' @section Immune Repertoire:
#' If \code{object@misc$immune_repertoire} contains a named list of
#' data.frames (one per sample, with \code{barcode}, \code{CTgene},
#' \code{CTnt}, \code{CTaa}, and \code{CTstrict}), it will be automatically
#' exported into the Cerebro object via \code{addImmuneRepertoire()}. Legacy
#' \code{bcr_data} /
#' \code{tcr_data} slots are also supported as a fallback.
#'
#' @section HLA typing:
#' If \code{object@misc$hla_typing} holds an HLA genotype table -- a canonical
#' long \code{data.frame}, a wide \code{sample} + \code{HLA-*_1/_2}
#' \code{data.frame}, or a named list (sample -> allele vector) -- it is
#' exported via \code{addHLATyping()}, parallel to the immune repertoire. The
#' provenance in \code{object@misc$hla_typing_source_type} (one of
#' \code{"genotyped"}, \code{"imputed"}, \code{"synthetic"}, \code{"unknown"};
#' default \code{"unknown"}) is carried through, so a predicted or fabricated
#' genotype is never mistaken for a directly typed one.
#'
#' @return
#' No data returned.
#'
#' @examples
#' pbmc <- readRDS(system.file("extdata/examples/pbmc_seurat.rds",
#'   package = "CerebroNexus"))
#' exportFromSeurat(
#'   object = pbmc,
#'   file = file.path(tempdir(), 'pbmc_Seurat.crb'),
#'   experiment_name = 'PBMC',
#'   organism = 'hg',
#'   groups = c('sample','seurat_clusters'),
#'   nUMI = 'nCount_RNA',
#'   nGene = 'nFeature_RNA',
#'   use_delayed_array = FALSE,
#'   verbose = TRUE
#' )
#'
#' @import dplyr
#' @importFrom methods as
#' @importFrom rlang .data
#'
#' @export
#'
exportFromSeurat <- function(
  object,
  assay = 'RNA',
  slot = 'data',
  file,
  experiment_name,
  organism,
  groups,
  main_group = NULL,
  cell_cycle = NULL,
  nUMI = 'nUMI',
  nGene = 'nGene',
  add_all_meta_data = TRUE,
  use_delayed_array = FALSE,
  expression_matrix_mode = c("embedded", "bpcells", "h5"),
  codec = c("qs2", "rds"),
  viewer_binary = c("auto", "always", "never"),
  viewer_binary_threshold = 500000L,
  spatial_images = NULL,
  verbose = FALSE,
  .expression_resolution = NULL,
  projections = NULL,
  spatial_coordinate_transforms = NULL
) {
  ##--------------------------------------------------------------------------##
  ## safety checks before starting to do anything
  ##--------------------------------------------------------------------------##

  expression_matrix_mode <- match.arg(expression_matrix_mode)
  codec <- match.arg(codec)
  viewer_options <- .viewerPackOptions(viewer_binary, viewer_binary_threshold)
  .exportCheckOutputArgs(file, expression_matrix_mode, use_delayed_array, verbose)

  ## check if Seurat is installed
  if (!requireNamespace("Seurat", quietly = TRUE)) {
    stop(
      "The 'Seurat' package is needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  ## Check Seurat package version using compareVersion
  seurat_version <- as.character(utils::packageVersion("Seurat"))
  if (utils::compareVersion(seurat_version, "3.0.0") < 0) {
    stop(
      paste0(
        "The installed Seurat package is of version `",
        seurat_version,
        "`, but at least v3.0 is required."
      ),
      call. = FALSE
    )
  }

  ## check if provided object is of class "Seurat"
  if (!inherits(object, "Seurat")) {
    stop(
      paste0(
        "Provided object is of class `",
        paste(class(object), collapse = ", "),
        "` but must be of class 'Seurat'."
      ),
      call. = FALSE
    )
  }

  ## check version of Seurat object and stop if it is lower than 3
  obj_version <- as.character(object@version)
  if (utils::compareVersion(obj_version, "3.0.0") < 0) {
    stop(
      paste0(
        "Provided Seurat object has version `",
        obj_version,
        "` but must be at least 3.0."
      ),
      call. = FALSE
    )
  }

  meta <- object@meta.data
  .exportCheckGroups(names(meta), groups, main_group)

  if (
    !is.null(projections) &&
      (!is.character(projections) ||
        !length(projections) ||
        anyNA(projections) ||
        any(!nzchar(projections)) ||
        anyDuplicated(projections) ||
        any(!projections %in% names(object@reductions)))
  ) {
    stop(
      "`projections` must name unique dimensional reductions in the object.",
      call. = FALSE
    )
  }

  .exportCheckMetaColumns(names(meta), nUMI, nGene, cell_cycle)

  ## check if provided assay exists
  if ((assay %in% names(object@assays) == FALSE)) {
    stop(
      paste0(
        'Specified assay `',
        assay,
        '` could not be found in provided Seurat ',
        'object.'
      ),
      call. = FALSE
    )
  }

  ## Stage every output artefact beside the final destination. External
  ## matrices can be expensive to write, but no published .crb or sidecar is
  ## touched until the complete object has passed every later validation.
  final_file <- file
  export_stage_dir <- .exportPrepareOutput(final_file)
  on.exit(
    unlink(export_stage_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )

  ##--------------------------------------------------------------------------##
  ## initialize Cerebro object
  ##--------------------------------------------------------------------------##
  export <- .exportInit(experiment_name, organism, object@misc, verbose)

  ##--------------------------------------------------------------------------##
  ## add transcript counts
  ##--------------------------------------------------------------------------##

  ## get expression data using shared utility function. This is a top-level,
  ## user-facing export entry point whose job is to get the object out to a
  ## .crb, so it opts into legacy cross-semantic layer fallback (e.g. a
  ## Seurat v5 RNA assay with only a `counts` layer, requested at the default
  ## slot = "data"). Without this the export would hard-stop here — before even
  ## reaching the spatial block — on objects that exported fine on master. The
  ## fallback itself warns, so the substitution is never silent.
  ## Split (layered) Seurat v5 objects are joined first, matching
  ## `convertSeuratToCerebro()`. Without it only the first sample's layer is
  ## read: the meta data still describes every cell while the matrix describes
  ## one sample, and the two are never compared again in the `h5` and `bpcells`
  ## modes. `JoinLayers()` materialises the merged matrix, so a very large split
  ## object pays one memory peak here -- the price of exporting all of it.
  if (is.null(.expression_resolution)) {
    expression_resolution <- .getExpressionMatrix(
      seurat = object,
      assay = assay,
      slot = slot,
      join_samples = TRUE,
      allow_cross_semantic_fallback = TRUE,
      allow_iterable_matrix = identical(expression_matrix_mode, "bpcells"),
      verbose = verbose,
      return_resolution = TRUE
    )
  } else {
    required_resolution_fields <- c(
      "data",
      "assay",
      "requested",
      "resolved",
      "joined",
      "fallback"
    )
    if (
      !is.list(.expression_resolution) ||
        !all(required_resolution_fields %in% names(.expression_resolution)) ||
        !identical(.expression_resolution$assay, as.character(assay)) ||
        !identical(.expression_resolution$requested, as.character(slot))
    ) {
      stop(
        "`.expression_resolution` must be the validated resolution for the ",
        "requested `assay` and `slot`. It is an internal conversion handoff; ",
        "users should leave it as NULL.",
        call. = FALSE
      )
    }
    expression_resolution <- .expression_resolution
  }

  ## Backend-independent guard. Validate once before the storage modes diverge,
  ## and retain both the requested and physically resolved layer in diagnostics.
  object_cells <- Seurat::Cells(object)
  expression_data <- .validate_expression_cells(
    expression_data = expression_resolution$data,
    object_cells = object_cells,
    assay = assay,
    requested_layer = expression_resolution$requested,
    resolved_layer = expression_resolution$resolved
  )

  expression_data <- .exportWriteExpression(
    export = export,
    expression_data = expression_data,
    expression_matrix_mode = expression_matrix_mode,
    use_delayed_array = use_delayed_array,
    export_stage_dir = export_stage_dir,
    final_file = final_file,
    verbose = verbose
  )

  .exportAddExperimentMisc(export, object@misc)

  if (verbose) {
    message(
      paste0(
        '[',
        format(Sys.time(), '%H:%M:%S'),
        '] Collecting available meta data...'
      )
    )
  }
  .exportAddMetaData(
    export = export,
    meta = meta,
    cells = Seurat::Cells(object),
    groups = groups,
    nUMI = nUMI,
    nGene = nGene,
    cell_cycle = cell_cycle,
    add_all_meta_data = add_all_meta_data,
    main_group = main_group,
    verbose = verbose
  )

  explicit_projections <- !is.null(projections)
  .exportAddProjections(
    export = export,
    projection_names = if (explicit_projections) {
      projections
    } else {
      names(object@reductions)
    },
    get_embedding = function(name) {
      as.data.frame(object@reductions[[name]]@cell.embeddings)
    },
    explicit_projections = explicit_projections,
    verbose = verbose
  )

  .exportAddMiscResults(
    export = export,
    misc = object@misc,
    groups = groups,
    cells = Seurat::Cells(object),
    verbose = verbose
  )

  ##--------------------------------------------------------------------------##
  ## spatial data
  ##--------------------------------------------------------------------------##
  if (verbose) {
    message(
      paste0(
        '[',
        format(Sys.time(), '%H:%M:%S'),
        '] Checking for spatial data...'
      )
    )
  }

  ## Spatial images live in the `@images` slot on Seurat v3+ objects, so this
  ## path handles both Seurat v4 (VisiumV1, SlideSeq) and v5 (VisiumV2, FOV)
  ## objects. `.getSpatialData()` resolves coordinates version-agnostically via
  ## GetTissueCoordinates + S4 slot access, so no version branch is needed here.
  has_images <- .spx_has_slot(object, "images") &&
    !is.null(object@images) &&
    length(object@images) > 0
  image_names <- if (has_images) names(object@images) else character()
  reduction_names <- tryCatch(
    names(object@reductions),
    error = function(error) character()
  )
  reduction_names <- reduction_names[
    tolower(reduction_names) == "spatial" &
      !reduction_names %in% image_names
  ]
  reduction_names <- Filter(
    function(name) {
      dimensions <- tryCatch(
        dim(SeuratObject::Embeddings(object[[name]])),
        error = function(error) NULL
      )
      length(dimensions) == 2L && dimensions[[2L]] >= 2L
    },
    reduction_names
  )
  spatial_names <- c(reduction_names, image_names)
  misc_spatial_images <- .validateCerebroSpatialImages(
    object@misc$cerebro_spatial_images,
    spatial_names
  )
  path_spatial_images <- .normalizeSpatialImagePaths(
    spatial_images,
    spatial_names,
    "`spatial_images`"
  )
  .mergeSpatialImageDeclarations(misc_spatial_images, path_spatial_images)

  spatial_coordinate_transform_specs <- list()
  if (
    is.list(spatial_coordinate_transforms) &&
      !is.object(spatial_coordinate_transforms) &&
      !length(spatial_coordinate_transforms)
  ) {
    spatial_coordinate_transforms <- NULL
  }
  if (!is.null(spatial_coordinate_transforms)) {
    transform_names <- names(spatial_coordinate_transforms)
    if (
      !is.list(spatial_coordinate_transforms) ||
        is.object(spatial_coordinate_transforms) ||
        is.null(transform_names) ||
        !is.character(transform_names) ||
        is.object(transform_names) ||
        anyNA(transform_names) ||
        any(!nzchar(transform_names)) ||
        anyDuplicated(transform_names)
    ) {
      stop(
        "`spatial_coordinate_transforms` must be an ordinary named list ",
        "with unique non-blank FOV names.",
        call. = FALSE
      )
    }
    unknown_transforms <- setdiff(transform_names, spatial_names)
    if (length(unknown_transforms)) {
      stop(
        "`spatial_coordinate_transforms` contains unknown FOV(s): ",
        paste(unknown_transforms, collapse = ", "),
        call. = FALSE
      )
    }
    spatial_coordinate_transform_specs <- lapply(
      transform_names,
      function(image_name) {
        .spx_coordinate_transform_spec_normalize(
          spatial_coordinate_transforms[[image_name]],
          context = paste0("spatial_coordinate_transforms$", image_name)
        )
      }
    )
    names(spatial_coordinate_transform_specs) <- transform_names
  }

  if (length(spatial_names)) {
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] ',
          'Spatial data found. Extracting spatial coordinates...'
        )
      )
    }

    for (image_name in spatial_names) {
      spatial_data <- tryCatch(
        {
          # Extract spatial data (coordinates + expression)
          # Using .getSpatialData helper which handles Visium, FOV/Xenium, etc.
          spatial_data <- if (image_name %in% reduction_names) {
            embeddings <- SeuratObject::Embeddings(object[[image_name]])
            cells <- intersect(rownames(embeddings), colnames(expression_data))
            coordinates <- data.frame(
              .cell = cells,
              .image = image_name,
              .coordinate_source = paste0("reduction.", image_name),
              x = as.numeric(embeddings[cells, 1L]),
              y = as.numeric(embeddings[cells, 2L]),
              row.names = cells,
              stringsAsFactors = FALSE
            )
            if (
              !length(cells) ||
                anyDuplicated(rownames(embeddings)) ||
                any(!is.finite(coordinates$x)) ||
                any(!is.finite(coordinates$y))
            ) {
              stop("The spatial reduction has no safe paired coordinates.")
            }
            list(
              coordinates = coordinates,
              expression = expression_data[, cells, drop = FALSE],
              assay = assay,
              requested_layer = slot,
              layer = expression_resolution$resolved,
              image = image_name,
              coordinate_source = paste0("reduction.", image_name)
            )
          } else {
            extracted <- .getSpatialData(
              object,
              image = image_name,
              layer = slot,
              assay = assay,
              expression_data = expression_data,
              expression_layer = expression_resolution$resolved
            )
            extracted$boundaries <- .getSpatialBoundaries(
              object,
              image_name,
              rownames(extracted$coordinates)
            )
            extracted$molecules <- .getSpatialMolecules(object, image_name)
            extracted
          }

          # Also add coordinates as a projection for compatibility with existing visualization functions
          coords_df <- spatial_data$coordinates

          projection <- .spx_export_projection_coordinates(coords_df)
          if (!is.null(projection)) {
            coords_df <- projection
            if (verbose) {
              message(paste0(
                '[',
                format(Sys.time(), '%H:%M:%S'),
                '] ',
                'Added spatial projection: ',
                image_name
              ))
            }
          }
          transform_spec <- spatial_coordinate_transform_specs[[image_name]]
          if (!is.null(transform_spec)) {
            source_coordinate_fingerprint <-
              .spx_coordinate_transform_fingerprint(coords_df)
            coordinate_transform <- .spx_coordinate_transform_normalize(
              spatial_coordinate_transforms[[image_name]],
              coords_df,
              context = paste0("spatial_coordinate_transforms$", image_name)
            )
            coords_df <- .spx_apply_coordinate_transform(
              coords_df,
              spatial_coordinate_transforms[[image_name]]
            )
            transform_overlay <- function(data) {
              if (is.null(data) || !nrow(data)) {
                return(data)
              }
              angle <- coordinate_transform$rotation_degrees * pi / 180
              centered_x <- data$x -
                coordinate_transform$pivot[["x"]]
              centered_y <- data$y -
                coordinate_transform$pivot[["y"]]
              data$x <-
                coordinate_transform$pivot[["x"]] +
                coordinate_transform$scale *
                  (centered_x * cos(angle) - centered_y * sin(angle))
              data$y <-
                coordinate_transform$pivot[["y"]] +
                coordinate_transform$scale *
                  (centered_x * sin(angle) + centered_y * cos(angle))
              data
            }
            spatial_data$boundaries <- transform_overlay(
              spatial_data$boundaries
            )
            if (!is.null(spatial_data$molecules)) {
              spatial_data$molecules$data <- transform_overlay(
                spatial_data$molecules$data
              )
            }
            coordinate_transform$transformed_coordinate_fingerprint <-
              .spx_coordinate_transform_fingerprint(coords_df)
            coordinate_transform$coordinate_fingerprint_version <- 2L
            coordinate_transform$source_coordinate_fingerprint <-
              source_coordinate_fingerprint
            spatial_data$coordinate_transform <- coordinate_transform
          }
          spatial_data$coordinates <- coords_df
          spatial_data
        },
        error = function(e) {
          if (!is.null(spatial_coordinate_transform_specs[[image_name]])) {
            stop(
              "Could not apply spatial coordinate transform for FOV `",
              image_name,
              "`: ",
              conditionMessage(e),
              call. = FALSE
            )
          }
          ## Never drop a spatial image silently: an object that clearly has
          ## `@images` but whose extraction fails (e.g. requested layer=slot is
          ## absent) would otherwise export "successfully" with no Spatial tab
          ## and no clue why. Always warn, regardless of `verbose`.
          warning(
            'Could not extract spatial data for image `',
            image_name,
            '` (layer = "',
            slot,
            '"): ',
            conditionMessage(e),
            call. = FALSE
          )
          NULL
        }
      )
      if (is.null(spatial_data)) {
        next
      }
      merged_images <- .mergeSpatialImageDeclarations(
        misc_spatial_images[image_name],
        path_spatial_images[image_name],
        setNames(list(spatial_data$coordinates), image_name)
      )
      spatial_data$histology_images <- merged_images[[image_name]]

      export$addSpatialData(image_name, spatial_data)

      if (verbose) {
        message(
          paste0(
            '[',
            format(Sys.time(), '%H:%M:%S'),
            '] ',
            'Added spatial data: ',
            image_name,
            ' (',
            nrow(spatial_data$coordinates),
            ' cells)'
          )
        )
      }
    }
  }

  .exportFinalize(
    export = export,
    final_file = final_file,
    export_stage_dir = export_stage_dir,
    expression_matrix_mode = expression_matrix_mode,
    codec = codec,
    viewer_options = viewer_options
  )
}
