## ---- TENx HDF5 sidecar writer ---------------------------------------------- ##
## HDF5Array::writeTENxMatrix() writes the sidecar block by block through the
## DelayedArray machinery and slows down sharply as the number of non-zero
## values grows (about 15 MB/s at 10^9 values). An in-memory dgCMatrix already
## holds the three TENx arrays, so they can go to the file in a few large
## slabs. The file is the one writeTENxMatrix() makes: the same datasets,
## types, chunk shapes, filters and extendible maximum sizes. The Viewer reads
## single genes by hyperslab from /expression/{indptr,indices,data}, and those
## reads depend on exactly that layout.

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

  rhdf5::h5createDataset(
    file,
    path("shape"),
    dims = 2L,
    storage.mode = "integer",
    H5type = "H5T_STD_U32LE",
    chunk = 2L,
    level = 0L
  )
  rhdf5::h5write(dims, file, path("shape"))
  .writeTENxStrings(file, path("genes"), rownames(x), 2048L)
  .writeTENxStrings(file, path("barcodes"), colnames(x), 4096L)

  rhdf5::h5createDataset(
    file,
    path("data"),
    dims = nnz,
    maxdims = max_nnz,
    storage.mode = "double",
    chunk = min(.TENX_DATA_CHUNK, max_nnz),
    level = level
  )
  rhdf5::h5createDataset(
    file,
    path("indices"),
    dims = nnz,
    maxdims = max_nnz,
    storage.mode = "integer",
    H5type = "H5T_STD_U32LE",
    chunk = min(.TENX_DATA_CHUNK, max_nnz),
    level = level
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
