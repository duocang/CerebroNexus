## Writes a genes x cells matrix that is stored cell by cell (an AnnData `X`, a
## TENx file with cell columns) to the cells x genes TENx H5 layout that
## exportFromSeurat() writes, without HDF5Array::writeTENxMatrix().
##
## writeTENxMatrix() can only write one gene after the other, and a source that
## is stored by cells has to be scanned from start to end for every block of
## genes: with the default block size that is thousands of passes over a file
## of tens of gigabytes. The transposition is done here in two passes instead,
## both spread over `workers` R processes (default 2; set CEREBRO_DEMO_WORKERS
## to use more cores at the cost of about 3-5 GB of memory per process):
##
## 1. The cells are read once in chunks. Each chunk is transposed (cells x genes,
##    sorted by gene) and written to one scratch file: the column pointers, the
##    row indices and the values, uncompressed.
## 2. The genes are cut into blocks of about `block_nnz` values. For each block,
##    the pieces of that block are read from every chunk file, put in gene order
##    (cells ascending within a gene) and written to a block file; the blocks
##    are then appended to the H5 file in order.
##
## The file is laid out like writeTENxMatrix() lays it out (group "expression":
## data F64, indices U32, indptr I64, shape U32, genes = CELL names,
## barcodes = GENE names), uncompressed.

## A source that can be read without going through DelayedArray: an AnnData
## matrix stored as CSR (cells x genes), which is how h5ad files keep `X` and
## `raw/X`. Returns NULL for any other layout, which is then read through the
## matrix object.
.demoAnnDataCsrSource <- function(h5ad, group) {
  attributes <- tryCatch(
    rhdf5::h5readAttributes(h5ad, group),
    error = function(error) list()
  )
  format <- tolower(as.character(
    attributes[["encoding-type"]] %||% attributes[["h5sparse_format"]] %||% ""
  ))
  if (!any(format %in% c("csr_matrix", "csr"))) {
    return(NULL)
  }
  list(kind = "anndata_csr", file = h5ad, group = group)
}

## Reads cells `from`..`to` of a source as a cells x genes dgCMatrix. Runs in the
## worker processes, so it uses only package-qualified calls.
.demoReadCells <- function(source, from, to, n_genes) {
  if (identical(source$kind, "anndata_csr")) {
    loadNamespace("Matrix")
    pointers <- as.numeric(rhdf5::h5read(
      source$file,
      paste0(source$group, "/indptr"),
      start = from,
      count = to - from + 2L,
      ## int64 pointers beyond 2^31 become NA as 32-bit integers
      bit64conversion = "double"
    ))
    first <- pointers[[1L]]
    count <- pointers[[length(pointers)]] - first
    indices <- as.vector(rhdf5::h5read(
      source$file,
      paste0(source$group, "/indices"),
      start = first + 1,
      count = count
    ))
    values <- as.vector(rhdf5::h5read(
      source$file,
      paste0(source$group, "/data"),
      start = first + 1,
      count = count
    ))
    ## The CSR arrays of cells x genes are the CSC arrays of genes x cells; the
    ## indices need not be sorted within a cell, which a validity check would
    ## reject, so the slots are set directly and the transpose sorts them.
    by_cell <- methods::new("dgCMatrix")
    by_cell@Dim <- c(as.integer(n_genes), as.integer(to - from + 1L))
    by_cell@p <- as.integer(pointers - first)
    by_cell@i <- as.integer(indices)
    by_cell@x <- as.numeric(values)
    return(Matrix::t(by_cell))
  }
  Matrix::t(methods::as(
    source$matrix[, from:to, drop = FALSE],
    "CsparseMatrix"
  ))
}

## Pass 1 for one chunk. Writes <dir>/chunk-<k>.bin = column pointers (int32,
## n_genes + 1), row indices (int32) and values (float64) of the chunk
## transposed to cells x genes, and returns the column pointers.
.demoTransposeChunk <- function(task, spec) {
  chunk <- .demoReadCells(spec$source, task$from, task$to, spec$n_genes)
  stopifnot(length(chunk@p) == spec$n_genes + 1L)
  connection <- file(
    file.path(spec$dir, sprintf("chunk-%05d.bin", task$chunk)),
    open = "wb"
  )
  on.exit(close(connection))
  writeBin(as.integer(chunk@p), connection)
  writeBin(as.integer(chunk@i), connection)
  writeBin(as.numeric(chunk@x), connection)
  as.integer(chunk@p)
}

## Pass 2 for one block of genes `task$from`..`task$to`. Puts the values of those
## genes in gene order over all chunks and writes <dir>/block-<b>-i.bin (int32,
## 0-based cell index) and block-<b>-x.bin (float64). `pointers` is the
## chunks x (genes + 1) matrix of column pointers from pass 1.
.demoAssembleBlock <- function(task, spec) {
  pointers <- spec$pointers
  n_chunks <- nrow(pointers)
  n_genes <- ncol(pointers) - 1L
  genes <- task$from:task$to
  n_block <- length(genes)
  counts <- pointers[, genes + 1L, drop = FALSE] - pointers[, genes, drop = FALSE]
  gene_total <- colSums(counts)
  total <- sum(gene_total)
  out_index <- integer(total)
  out_value <- numeric(total)
  if (total > 0) {
    gene_start <- cumsum(c(0, gene_total))[seq_len(n_block)]
    ## values of each gene in the chunks before this one
    before <- vapply(
      seq_len(n_block),
      function(j) c(0, cumsum(counts[, j]))[seq_len(n_chunks)],
      numeric(n_chunks)
    )
    dim(before) <- c(n_chunks, n_block)
    for (chunk in seq_len(n_chunks)) {
      count <- counts[chunk, ]
      length_chunk <- sum(count)
      if (length_chunk == 0) {
        next
      }
      path <- file.path(spec$dir, sprintf("chunk-%05d.bin", chunk))
      nnz <- pointers[chunk, n_genes + 1L]
      connection <- file(path, open = "rb")
      seek(connection, 4 * (n_genes + 1) + 4 * pointers[chunk, task$from])
      index <- readBin(connection, "integer", n = length_chunk)
      seek(connection, 4 * (n_genes + 1) + 4 * nnz + 8 * pointers[chunk, task$from])
      value <- readBin(connection, "double", n = length_chunk)
      close(connection)
      segment_start <- cumsum(c(0, count))[seq_len(n_block)]
      target <- rep(gene_start + before[chunk, ] - segment_start, times = count) +
        seq_len(length_chunk)
      out_index[target] <- index + as.integer(spec$offsets[[chunk]])
      out_value[target] <- value
    }
  }
  writeBin(out_index, file.path(spec$dir, sprintf("block-%03d-i.bin", task$block)))
  writeBin(out_value, file.path(spec$dir, sprintf("block-%03d-x.bin", task$block)))
  gene_total
}

.demoWriteH5Transposed <- function(
  matrix,
  h5,
  source = NULL,
  group = "expression",
  chunk_cells = 40000L,
  block_nnz = 1e8,
  workers = as.integer(Sys.getenv("CEREBRO_DEMO_WORKERS", "2")),
  scratch_dir = dirname(h5),
  verbose = TRUE
) {
  for (package in c("rhdf5", "Matrix")) {
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
  spec_source <- if (is.null(source)) {
    list(kind = "matrix", matrix = matrix)
  } else {
    source
  }
  starts <- seq.int(1L, n_cells, by = chunk_cells)
  n_chunks <- length(starts)
  workers <- max(1L, min(as.integer(workers), n_chunks))

  scratch <- tempfile("h5-transpose-", scratch_dir)
  dir.create(scratch, recursive = TRUE)
  on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)
  say <- function(...) {
    if (isTRUE(verbose)) {
      message(format(Sys.time(), "[%H:%M:%S] "), ...)
    }
  }

  cluster <- NULL
  if (workers > 1L) {
    cluster <- parallel::makeCluster(workers)
    on.exit(parallel::stopCluster(cluster), add = TRUE)
    libraries <- .libPaths()
    parallel::clusterCall(cluster, function(paths) .libPaths(paths), libraries)
  }
  run <- function(tasks, worker, spec) {
    if (is.null(cluster)) {
      lapply(tasks, worker, spec = spec)
    } else {
      ## shipped without this script's environment; the workers find the other
      ## helpers in their global environment (exported below)
      environment(worker) <- globalenv()
      parallel::parLapplyLB(cluster, tasks, worker, spec = spec)
    }
  }
  ## The workers run the helpers of this file, which may live in a script
  ## environment rather than the global one; ship the ones they call.
  if (!is.null(cluster)) {
    parallel::clusterExport(
      cluster,
      c(".demoReadCells", ".demoTransposeChunk", ".demoAssembleBlock"),
      envir = environment(.demoWriteH5Transposed)
    )
  }

  ## Pass 1: read the cells once, one chunk per task.
  say(
    "pass 1: reading ", format(n_cells, big.mark = ","), " cells in ", n_chunks,
    " chunks on ", workers, " process(es)"
  )
  tasks <- lapply(seq_len(n_chunks), function(chunk) {
    list(
      chunk = chunk,
      from = starts[[chunk]],
      to = min(starts[[chunk]] + chunk_cells - 1L, n_cells)
    )
  })
  pointer_list <- run(
    tasks,
    .demoTransposeChunk,
    list(source = spec_source, n_genes = n_genes, dir = scratch)
  )
  pointers <- do.call(rbind, lapply(pointer_list, as.numeric))
  rm(pointer_list)
  counts <- pointers[, -1L, drop = FALSE] - pointers[, -(n_genes + 1L), drop = FALSE]
  gene_total <- colSums(counts)
  rm(counts)
  say("pass 1 done: ", format(sum(gene_total), big.mark = ","), " non-zero values")

  ## Pass 2: cut the genes into blocks of about `block_nnz` values.
  block_of <- floor((cumsum(gene_total) - gene_total) / block_nnz)
  block_of <- match(block_of, unique(block_of))
  blocks <- lapply(split(seq_len(n_genes), block_of), function(index) {
    list(from = min(index), to = max(index))
  })
  for (b in seq_along(blocks)) {
    blocks[[b]]$block <- b
  }

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
  spec_blocks <- list(
    dir = scratch,
    pointers = pointers,
    offsets = starts - 1L
  )
  indptr <- numeric(n_genes + 1L)
  written <- 0
  waves <- split(blocks, ceiling(seq_along(blocks) / workers))
  for (wave in waves) {
    totals <- run(wave, .demoAssembleBlock, spec_blocks)
    for (k in seq_along(wave)) {
      block <- wave[[k]]
      total <- sum(totals[[k]])
      path_i <- file.path(scratch, sprintf("block-%03d-i.bin", block$block))
      path_x <- file.path(scratch, sprintf("block-%03d-x.bin", block$block))
      if (total > 0) {
        for (dataset in c("data", "indices")) {
          path <- paste0(group, "/", dataset)
          rhdf5::h5set_extent(h5, path, written + total)
          rhdf5::h5write(
            if (dataset == "data") {
              readBin(path_x, "double", n = total)
            } else {
              readBin(path_i, "integer", n = total)
            },
            h5,
            path,
            start = written + 1,
            count = total
          )
        }
      }
      unlink(c(path_i, path_x))
      indptr[(block$from:block$to) + 1L] <- written + cumsum(totals[[k]])
      written <- written + total
    }
    say("wrote gene block ", max(vapply(wave, function(b) b$block, 1L)), " of ", length(blocks))
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
