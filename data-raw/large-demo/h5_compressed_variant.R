## A compressed twin of a demo's expression sidecar. The demo builds write the
## counts uncompressed (level 0), which reads fastest; this adds
## `<stem>_compressed.crb` + `<stem>_compressed.h5` next to them so both can be
## opened and compared. Measured on the CosMx sidecar (97.6M non-zero values,
## 1.18 GB): deflate levels 1 to 9 all end up within 1.1% of each other
## (0.728 to 0.720 GB, about 39% smaller than uncompressed), so a low level is
## enough. The chunk length matters more: reading one gene took 17 to 22 ms
## with 65,536-value chunks and 90 to 106 ms with 1,048,576-value chunks.
##
## CEREBRO_DEMO_H5_LEVEL (default 3) and CEREBRO_DEMO_H5_CHUNK (default 65536)
## change the setting; a variant written with other values is rebuilt.

.demoCompressedFormat <- 1L

.demoCompressionLevel <- function() {
  level <- suppressWarnings(as.integer(Sys.getenv("CEREBRO_DEMO_H5_LEVEL", "3")))
  if (length(level) != 1L || is.na(level) || level < 1L || level > 9L) {
    stop("CEREBRO_DEMO_H5_LEVEL must be an integer from 1 to 9.", call. = FALSE)
  }
  level
}

.demoCompressionChunk <- function() {
  chunk <- suppressWarnings(
    as.integer(Sys.getenv("CEREBRO_DEMO_H5_CHUNK", "65536"))
  )
  if (length(chunk) != 1L || is.na(chunk) || chunk < 1024L) {
    stop("CEREBRO_DEMO_H5_CHUNK must be an integer of at least 1024.", call. = FALSE)
  }
  chunk
}

.demoCompressedPaths <- function(crb) {
  stem <- sub("[.]crb$", "", crb)
  list(
    crb = paste0(stem, "_compressed.crb"),
    h5 = paste0(stem, "_compressed.h5"),
    source_h5 = paste0(stem, ".h5"),
    ## Written last: a variant that stopped halfway has none.
    stamp = paste0(stem, "_compressed.stamp")
  )
}

## What a finished variant must say: the format of this file, the deflate
## settings and the size of the sidecar it was made from.
.demoCompressedStamp <- function(paths, level, chunk) {
  sprintf(
    "format=%d;level=%d;chunk=%d;source_bytes=%.0f",
    .demoCompressedFormat,
    level,
    chunk,
    file.size(paths$source_h5)
  )
}

.demoCompressedComplete <- function(paths, stamp) {
  file.exists(paths$crb) &&
    !dir.exists(paths$crb) &&
    file.exists(paths$h5) &&
    !dir.exists(paths$h5) &&
    file.exists(paths$stamp) &&
    identical(readLines(paths$stamp, warn = FALSE), stamp)
}

## Copies the TENx sidecar `src` to `dst` with deflate `level` and `chunk`
## values per chunk on the two long datasets. The values are read and written
## `step` at a time, so memory stays at a few hundred MB for any file size.
.demoRecompressH5 <- function(
  src,
  dst,
  level,
  chunk,
  group = "expression",
  step = 5e7,
  verbose = TRUE
) {
  if (!requireNamespace("rhdf5", quietly = TRUE)) {
    stop("Compressing the H5 expression file requires rhdf5.", call. = FALSE)
  }
  at <- function(dataset) paste0(group, "/", dataset)
  indptr <- as.numeric(rhdf5::h5read(
    src,
    at("indptr"),
    bit64conversion = "double"
  ))
  total <- indptr[[length(indptr)]]
  if (file.exists(dst)) {
    file.remove(dst)
  }
  rhdf5::h5createFile(dst)
  rhdf5::h5createGroup(dst, group)
  for (dataset in c("data", "indices")) {
    if (total < 1) {
      next
    }
    rhdf5::h5createDataset(
      dst,
      at(dataset),
      dims = total,
      chunk = min(chunk, total),
      H5type = if (dataset == "data") "H5T_IEEE_F64LE" else "H5T_STD_U32LE",
      level = level
    )
    for (start in seq(1, total, by = step)) {
      count <- min(step, total - start + 1)
      rhdf5::h5write(
        as.vector(rhdf5::h5read(src, at(dataset), start = start, count = count)),
        dst,
        at(dataset),
        start = start,
        count = count
      )
    }
    if (isTRUE(verbose)) {
      message(format(Sys.time(), "[%H:%M:%S] "), "compressed ", at(dataset))
    }
  }
  rhdf5::h5createDataset(
    dst,
    at("indptr"),
    dims = length(indptr),
    H5type = "H5T_STD_I64LE",
    chunk = length(indptr),
    level = 0L
  )
  rhdf5::h5write(indptr, dst, at("indptr"))
  rhdf5::h5createDataset(
    dst,
    at("shape"),
    dims = 2L,
    H5type = "H5T_STD_U32LE",
    chunk = 2L,
    level = 0L
  )
  rhdf5::h5write(
    as.integer(as.vector(rhdf5::h5read(src, at("shape")))),
    dst,
    at("shape")
  )
  for (dataset in c("genes", "barcodes")) {
    rhdf5::h5write(
      as.character(as.vector(rhdf5::h5read(src, at(dataset)))),
      dst,
      at(dataset)
    )
  }
  invisible(dst)
}

## Makes `<stem>_compressed.crb` + `.h5` from a finished demo CRB and returns the
## new CRB path. A finished variant with the same settings is reused. The CRB is
## a copy of the original that points at the compressed sidecar (the Viewer
## Pack and the immune repertoire sidecar are written for it by saveCerebro()).
prepareCompressedDemoVariant <- function(
  crb,
  level = .demoCompressionLevel(),
  chunk = .demoCompressionChunk()
) {
  paths <- .demoCompressedPaths(crb)
  if (!file.exists(crb) || !file.exists(paths$source_h5)) {
    stop("The demo CRB and its H5 sidecar must exist first.", call. = FALSE)
  }
  stamp <- .demoCompressedStamp(paths, level, chunk)
  if (.demoCompressedComplete(paths, stamp)) {
    return(paths$crb)
  }
  message(
    "Compressing ",
    basename(paths$source_h5),
    " (deflate level ",
    level,
    ", chunks of ",
    format(chunk, big.mark = ","),
    " values)..."
  )
  unlink(c(paths$stamp, paths$crb), force = TRUE)
  partial <- paste0(paths$h5, ".part")
  on.exit(unlink(partial, force = TRUE), add = TRUE)
  .demoRecompressH5(paths$source_h5, partial, level, chunk)
  if (!file.rename(partial, paths$h5)) {
    stop("Could not publish ", basename(paths$h5), ".", call. = FALSE)
  }
  object <- CerebroNexus::readCerebro(crb)
  object$setExpressionBackend("h5", basename(paths$h5))
  CerebroNexus::saveCerebro(object, paths$crb)
  writeLines(stamp, paths$stamp)
  message(
    sprintf(
      "Compressed sidecar: %.2f GB (uncompressed %.2f GB)",
      file.size(paths$h5) / 1e9,
      file.size(paths$source_h5) / 1e9
    )
  )
  paths$crb
}
