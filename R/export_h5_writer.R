## ---- TENx HDF5 sidecar writer ---------------------------------------------- ##
## HDF5Array::writeTENxMatrix() writes the sidecar block by block through the
## DelayedArray machinery and slows down sharply as the number of non-zero
## values grows (about 15 MB/s at 10^9 values). An in-memory dgCMatrix already
## holds the three TENx arrays, so they can go to the file in a few large
## slabs. The file is the one writeTENxMatrix() makes: the same datasets,
## types, chunk shapes and extendible maximum sizes, but data and indices have
## no filters at level 0 (see .tenxFilter()); rhdf5 and HDF5Array read them the
## same. The Viewer reads single genes by hyperslab from
## /expression/{indptr,indices,data}, and those reads depend on that layout.

.TENX_DATA_CHUNK <- 16384L
.TENX_INDPTR_CHUNK <- 4096L
.TENX_SLAB_LENGTH <- 4194304L # 256 chunks

## The writer handles a plain dgCMatrix with character dimnames; everything
## else (delayed inputs, empty matrices, missing packages) keeps the
## HDF5Array path.
.canWriteTENxDirect <- function(x) {
  inherits(x, "dgCMatrix") &&
    length(x@x) > 0L &&
    requireNamespace("rhdf5", quietly = TRUE) &&
    all(vapply(
      dimnames(x),
      function(names) is.character(names) && !anyNA(names),
      logical(1L)
    ))
}

## The cells x genes dgCMatrix behind a genes x cells matrix that is only its
## lazy transpose (what readH5adAsSCE(.cells_by_genes =) returns), else NULL.
## That is the sidecar's layout already, so it is written without a transpose.
.cellsByGenesSeed <- function(x) {
  if (
    !inherits(x, "DelayedMatrix") ||
      !requireNamespace("DelayedArray", quietly = TRUE)
  ) {
    return(NULL)
  }
  flipped <- DelayedArray::t(x)
  if (!DelayedArray::isPristine(flipped)) {
    return(NULL)
  }
  seed <- DelayedArray::seed(flipped)
  if (
    inherits(seed, "dgCMatrix") && identical(dimnames(seed), dimnames(flipped))
  ) {
    seed
  } else {
    NULL
  }
}

## The TENx file and group behind a genes x cells matrix that is only the lazy
## transpose of an HDF5Array::TENxMatrix (how a sidecar is read back), else
## NULL.
.tenxSidecarOf <- function(x) {
  if (
    !inherits(x, "DelayedMatrix") ||
      !requireNamespace("HDF5Array", quietly = TRUE)
  ) {
    return(NULL)
  }
  ## a seed whose file has gone (moved by the export) is no longer valid
  flipped <- tryCatch(DelayedArray::t(x), error = function(e) NULL)
  if (is.null(flipped) || !DelayedArray::isPristine(flipped)) {
    return(NULL)
  }
  seed <- DelayedArray::seed(flipped)
  if (!methods::is(seed, "TENxMatrixSeed")) {
    return(NULL)
  }
  list(file = HDF5Array::path(seed), group = sub("^/", "", seed@group))
}

## Sidecars that readH5adAsSCE(.sidecar_dir =) wrote straight from an h5ad.
## They are temporary until the export moves one next to its .crb; only these
## are ever moved, never a file the caller brought. Each keeps the group
## summaries computed while it was written, as list(codes, n_groups, total,
## positive) per grouping.
.tempSidecars <- new.env(parent = emptyenv())

.tempSidecarKey <- function(file) {
  normalizePath(file, winslash = "/", mustWork = FALSE)
}

.registerTempSidecar <- function(file, group_stats = list()) {
  assign(.tempSidecarKey(file), group_stats, envir = .tempSidecars)
  invisible(file)
}

## The group summaries kept with a temporary sidecar for exactly these codes
## (one per cell, from 1, NA for none), else NULL.
.tempSidecarGroupStats <- function(file, codes, n_groups) {
  key <- .tempSidecarKey(file)
  if (!exists(key, envir = .tempSidecars, inherits = FALSE)) {
    return(NULL)
  }
  for (stats in get(key, envir = .tempSidecars, inherits = FALSE)) {
    if (
      identical(stats$n_groups, as.integer(n_groups)) &&
        identical(stats$codes, as.integer(codes))
    ) {
      return(stats)
    }
  }
  NULL
}

.releaseTempSidecar <- function(file, delete = FALSE) {
  key <- .tempSidecarKey(file)
  if (exists(key, envir = .tempSidecars, inherits = FALSE)) {
    rm(list = key, envir = .tempSidecars)
  }
  if (delete) {
    unlink(file)
  }
  invisible(file)
}

## The temporary sidecar behind a matrix, already in the sidecar layout, else
## NULL.
.tempSidecarOf <- function(x) {
  sidecar <- .tenxSidecarOf(x)
  if (
    is.null(sidecar) ||
      !identical(sidecar$group, "expression") ||
      !exists(
        .tempSidecarKey(sidecar$file),
        envir = .tempSidecars,
        inherits = FALSE
      )
  ) {
    return(NULL)
  }
  sidecar$file
}

## Threads for the compiled sidecar code; more than four rarely help, as the
## transpose is bound by memory bandwidth.
.sidecarThreads <- function() {
  max(1L, as.integer(getOption("CerebroNexus.threads", 4L))[[1L]])
}

## rhdf5 adds the shuffle and deflate filters even at level 0, where they
## store the values as they are and only cost time on every write and read.
.tenxFilter <- function(level) if (level > 0) "gzip" else "NONE"

.tenxMaxStringSize <- function(x) max(1L, nchar(x, type = "bytes"))

.writeTENxStrings <- function(file, name, values, chunk) {
  rhdf5::h5createDataset(
    file,
    name,
    dims = length(values),
    storage.mode = "character",
    size = .tenxMaxStringSize(values),
    chunk = min(chunk, length(values)),
    level = 0L
  )
  rhdf5::h5write(values, file, name)
}

## The shape and the two name datasets of a TENx group. writeTENxMatrix()
## names the axes the 10x way round: "genes" holds the row names and
## "barcodes" the column names, whatever they are.
.writeTENxNames <- function(file, group, dims, row_names, col_names) {
  path <- function(name) paste0(group, "/", name)
  rhdf5::h5createDataset(
    file,
    path("shape"),
    dims = 2L,
    storage.mode = "integer",
    H5type = "H5T_STD_U32LE",
    chunk = 2L,
    level = 0L
  )
  rhdf5::h5write(as.integer(dims), file, path("shape"))
  .writeTENxStrings(file, path("genes"), row_names, 2048L)
  .writeTENxStrings(file, path("barcodes"), col_names, 4096L)
}

.writeTENxSlabs <- function(file, name, values) {
  handle <- rhdf5::H5Fopen(file)
  on.exit(rhdf5::H5Fclose(handle), add = TRUE)
  n <- length(values)
  start <- 1
  while (start <= n) {
    end <- min(start + .TENX_SLAB_LENGTH - 1, n)
    rhdf5::h5writeDataset(
      values[start:end],
      handle,
      name,
      start = start,
      count = end - start + 1
    )
    start <- end + 1
  }
}

#' Write a genes x cells dgCMatrix as the cells x genes sidecar by compiled
#' code: its slots already list each cell's genes, so they are transposed on
#' several threads and written without a transposed copy in R. The file is
#' the one `.writeTENxH5(t(x), file, group, level)` writes. FALSE (and no
#' file) when it cannot: anything but a named, non-empty dgCMatrix, a group
#' other than "expression", or a gene listed twice for a cell. The caller
#' then transposes and writes as before.
#' @keywords internal
#' @noRd
.writeSidecarFromGenesByCells <- function(
  x,
  file,
  group = "expression",
  level = NULL
) {
  if (!identical(group, "expression") || !.canWriteTENxDirect(x)) {
    return(FALSE)
  }
  if (is.null(level)) {
    level <- HDF5Array::getHDF5DumpCompressionLevel()
  }
  written <- tryCatch(
    dgcToSidecarCpp(
      x@i,
      x@p,
      x@x,
      nrow(x),
      ncol(x),
      file,
      .sidecarThreads(),
      as.integer(level)
    ),
    error = function(e) {
      unlink(file)
      stop(conditionMessage(e), call. = FALSE)
    }
  )
  if (!isTRUE(written$written)) {
    unlink(file)
    return(FALSE)
  }
  .writeTENxNames(file, group, c(ncol(x), nrow(x)), colnames(x), rownames(x))
  TRUE
}

#' Write a dgCMatrix as a TENx sparse HDF5 group, like
#' `HDF5Array::writeTENxMatrix(x, file, group = group, level = level)`.
#' @keywords internal
#' @noRd
.writeTENxH5 <- function(x, file, group = "expression", level = NULL) {
  if (is.null(level)) {
    level <- HDF5Array::getHDF5DumpCompressionLevel()
  }
  if (!.canWriteTENxDirect(x)) {
    return(invisible(HDF5Array::writeTENxMatrix(
      x,
      file,
      group = group,
      level = level
    )))
  }

  dims <- dim(x)
  nnz <- length(x@x)
  max_nnz <- prod(as.numeric(dims))
  path <- function(name) paste0(group, "/", name)

  rhdf5::h5createFile(file)
  rhdf5::h5createGroup(file, group)
  .writeTENxNames(file, group, dims, rownames(x), colnames(x))

  rhdf5::h5createDataset(
    file,
    path("data"),
    dims = nnz,
    maxdims = max_nnz,
    storage.mode = "double",
    chunk = min(.TENX_DATA_CHUNK, max_nnz),
    level = level,
    filter = .tenxFilter(level),
    shuffle = level > 0
  )
  rhdf5::h5createDataset(
    file,
    path("indices"),
    dims = nnz,
    maxdims = max_nnz,
    storage.mode = "integer",
    H5type = "H5T_STD_U32LE",
    chunk = min(.TENX_DATA_CHUNK, max_nnz),
    level = level,
    filter = .tenxFilter(level),
    shuffle = level > 0
  )
  rhdf5::h5createDataset(
    file,
    path("indptr"),
    dims = length(x@p),
    maxdims = length(x@p),
    storage.mode = "integer",
    H5type = "H5T_STD_I64LE",
    chunk = min(.TENX_INDPTR_CHUNK, length(x@p)),
    level = 0L
  )

  .writeTENxSlabs(file, path("data"), x@x)
  .writeTENxSlabs(file, path("indices"), x@i)
  .writeTENxSlabs(file, path("indptr"), x@p)
  invisible(file)
}
