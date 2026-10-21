## Writes a genes x cells matrix that is stored cell by cell (an AnnData `X`, a
## TENx file with cell columns) to the cells x genes TENx H5 layout that
## exportFromSeurat() writes, without HDF5Array::writeTENxMatrix().
##
## writeTENxMatrix() can only write one gene after the other, and a source that
## is stored by cells has to be scanned from start to end for every block of
## genes: with the default block size that is thousands of passes over a file
## of tens of gigabytes. The transposition is done here in two passes instead:
## the cells are read once in chunks and split into gene blocks on disk, then
## each gene block is assembled from its pieces and appended to the H5 file.
## The file is laid out like writeTENxMatrix() lays it out (group "expression":
## data F64, indices U32, indptr I64, shape U32, genes = CELL names,
## barcodes = GENE names), uncompressed.
.demoWriteH5Transposed <- function(
  matrix,
  h5,
  group = "expression",
  chunk_cells = 20000L,
  gene_blocks = 24L,
  scratch_dir = dirname(h5),
  verbose = TRUE
) {
  for (package in c("rhdf5", "Matrix", "qs2")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop("Writing the H5 expression file requires ", package, ".", call. = FALSE)
    }
  }
  genes <- rownames(matrix)
  cells <- colnames(matrix)
  if (is.null(genes) || is.null(cells) || anyNA(genes) || anyNA(cells)) {
    stop("The matrix needs gene and cell names.", call. = FALSE)
  }
  n_genes <- nrow(matrix)
  n_cells <- ncol(matrix)
  gene_blocks <- max(1L, min(as.integer(gene_blocks), n_genes))
  block_of <- cut(
    seq_len(n_genes),
    breaks = gene_blocks,
    labels = FALSE
  )
  blocks <- split(seq_len(n_genes), block_of)
  starts <- seq.int(1L, n_cells, by = chunk_cells)

  scratch <- tempfile("h5-transpose-", scratch_dir)
  dir.create(scratch, recursive = TRUE)
  on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)
  piece_path <- function(block, chunk) {
    file.path(scratch, sprintf("b%03d-c%05d.qs2", block, chunk))
  }
  say <- function(...) {
    if (isTRUE(verbose)) {
      message(format(Sys.time(), "[%H:%M:%S] "), ...)
    }
  }

  ## Pass 1: read the cells once, one chunk at a time.
  for (chunk in seq_along(starts)) {
    index <- starts[[chunk]]:min(starts[[chunk]] + chunk_cells - 1L, n_cells)
    part <- Matrix::t(methods::as(matrix[, index, drop = FALSE], "CsparseMatrix"))
    for (block in seq_along(blocks)) {
      qs2::qs_save(
        part[, blocks[[block]], drop = FALSE],
        piece_path(block, chunk)
      )
    }
    say("read cells ", format(max(index), big.mark = ","), " of ", format(n_cells, big.mark = ","))
  }

  ## Pass 2: append each gene block to the H5 file.
  if (file.exists(h5)) {
    file.remove(h5)
  }
  rhdf5::h5createFile(h5)
  rhdf5::h5createGroup(h5, group)
  chunk_size <- 1048576
  for (dataset in c("data", "indices")) {
    rhdf5::h5createDataset(
      h5,
      paste0(group, "/", dataset),
      dims = 1,
      maxdims = rhdf5::H5Sunlimited(),
      chunk = chunk_size,
      H5type = if (dataset == "data") "H5T_IEEE_F64LE" else "H5T_STD_U32LE",
      level = 0L
    )
  }
  indptr <- numeric(n_genes + 1L)
  written <- 0
  for (block in seq_along(blocks)) {
    pieces <- lapply(seq_along(starts), function(chunk) {
      qs2::qs_read(piece_path(block, chunk))
    })
    part <- methods::as(do.call(rbind, pieces), "CsparseMatrix")
    rm(pieces)
    unlink(vapply(seq_along(starts), piece_path, "", block = block))
    nnz <- length(part@x)
    if (nnz) {
      for (dataset in c("data", "indices")) {
        path <- paste0(group, "/", dataset)
        rhdf5::h5set_extent(h5, path, written + nnz)
        rhdf5::h5write(
          if (dataset == "data") part@x else part@i,
          h5,
          path,
          start = written + 1,
          count = nnz
        )
      }
    }
    columns <- blocks[[block]]
    indptr[columns + 1L] <- written + part@p[-1L]
    written <- written + nnz
    say("wrote gene block ", block, " of ", length(blocks))
  }
  rhdf5::h5createDataset(
    h5,
    paste0(group, "/indptr"),
    dims = length(indptr),
    H5type = "H5T_STD_I64LE",
    chunk = length(indptr),
    level = 0L
  )
  rhdf5::h5write(indptr, h5, paste0(group, "/indptr"))
  rhdf5::h5createDataset(
    h5,
    paste0(group, "/shape"),
    dims = 2L,
    H5type = "H5T_STD_U32LE",
    chunk = 2L,
    level = 0L
  )
  rhdf5::h5write(as.integer(c(n_cells, n_genes)), h5, paste0(group, "/shape"))
  rhdf5::h5write(cells, h5, paste0(group, "/genes"))
  rhdf5::h5write(genes, h5, paste0(group, "/barcodes"))
  invisible(h5)
}
