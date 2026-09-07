# Lazy full-source helpers for Panel C2.

bench_validate_full_matrix <- function(matrix, spec = list()) {
  if (!inherits(matrix, "IterableMatrix")) {
    stop("full source must be a BPCells IterableMatrix", call. = FALSE)
  }
  if (
    length(dim(matrix)) != 2L ||
      any(!is.finite(dim(matrix))) ||
      any(dim(matrix) < 1L)
  ) {
    stop("full source must be a non-empty matrix", call. = FALSE)
  }
  labels <- dimnames(matrix)
  if (
    length(labels) != 2L ||
      any(vapply(labels, is.null, logical(1))) ||
      any(vapply(labels, function(x) anyNA(x) || any(!nzchar(x)), logical(1)))
  ) {
    stop("full source must have non-empty gene and cell names", call. = FALSE)
  }
  if (!is.null(spec$full_cells) && ncol(matrix) != spec$full_cells) {
    stop("full source cell count does not match source metadata", call. = FALSE)
  }
  matrix
}

bench_open_full_source <- function(spec, path) {
  if (length(spec$kind) != 1L || !spec$kind %in% c("tenx", "h5ad")) {
    stop("full source kind must be 10x or AnnData", call. = FALSE)
  }
  if (!file.exists(path) || dir.exists(path)) {
    stop("full source file is missing: ", path, call. = FALSE)
  }
  matrix <- switch(
    spec$kind,
    tenx = BPCells::open_matrix_10x_hdf5(path),
    h5ad = BPCells::open_matrix_anndata_hdf5(path, group = "X")
  )
  bench_validate_full_matrix(matrix, spec)
}

bench_write_full_backend <- function(matrix, backend, path) {
  bench_validate_full_matrix(matrix)
  if (backend == "bpcells") {
    BPCells::write_matrix_dir(matrix, path, overwrite = TRUE)
    written <- BPCells::open_matrix_dir(path)
  } else if (backend == "h5") {
    if (file.exists(path)) {
      unlink(path)
    }
    disk_matrix <- BPCells::transpose_storage_order(DelayedArray::t(matrix))
    BPCells::write_matrix_10x_hdf5(
      disk_matrix,
      path,
      type = "auto"
    )
    fid <- rhdf5::H5Fopen(path)
    on.exit(try(rhdf5::H5Fclose(fid), silent = TRUE), add = TRUE)
    rhdf5::H5Lmove(fid, "/matrix", fid, "/expression")
    rhdf5::H5Fclose(fid)
    fid <- NULL
    written <- DelayedArray::t(
      HDF5Array::TENxMatrix(path, group = "expression")
    )
  } else {
    stop("backend must be bpcells or h5", call. = FALSE)
  }
  if (
    !identical(dim(written), dim(matrix)) ||
      !identical(dimnames(written), dimnames(matrix))
  ) {
    stop("written backend dimensions or names changed", call. = FALSE)
  }
  invisible(path)
}

bench_build_lazy_query_plan <- function(matrix, n_genes = 12L) {
  bench_validate_full_matrix(matrix)
  stats <- BPCells::matrix_stats(matrix, row_stats = "nonzero")$row_stats
  counts <- as.numeric(stats["nonzero", ])
  panel <- bench_stratified_gene_panel(rownames(matrix), counts, n_genes)
  first_values <- as.numeric(as.matrix(matrix[panel$gene[1L], , drop = FALSE]))
  block_values <- as.matrix(matrix[panel$gene, , drop = FALSE])
  plan <- list(
    schema_version = 1L,
    n_cells = ncol(matrix),
    n_genes = nrow(matrix),
    nnz = sum(counts),
    panel = panel,
    reference_row_fingerprint = bench_numeric_fingerprint(first_values),
    reference_block_fingerprint = bench_numeric_fingerprint(block_values)
  )
  plan$query_plan_fingerprint <- bench_serialized_fingerprint(plan)
  plan
}

bench_make_full_shell <- function(
  matrix,
  backend,
  location,
  source_name,
  run_id
) {
  bench_validate_full_matrix(matrix)
  cells <- colnames(matrix)
  index <- seq_along(cells)
  metadata <- data.frame(
    sample = factor(sprintf("sample_%02d", (index - 1L) %% 8L + 1L)),
    cluster = factor(sprintf("cluster_%02d", (index - 1L) %% 20L + 1L)),
    row.names = cells
  )
  projection <- data.frame(
    x = sin(index / 1000),
    y = cos(index / 1000),
    row.names = cells
  )
  obj <- CerebroNexus::Cerebro$new()
  obj$setVersion(utils::packageVersion("CerebroNexus"))
  obj$addExperiment("experiment_name", paste0("Panel C2: ", source_name))
  obj$addExperiment("date_of_export", Sys.Date())
  obj$addTechnicalInfo("benchmark_run_id", run_id)
  obj$addTechnicalInfo("benchmark_source", source_name)
  obj$setMetaData(metadata)
  obj$addGroup("sample", levels(metadata$sample))
  obj$addGroup("cluster", levels(metadata$cluster))
  obj$addProjection("benchmark", projection)
  obj$setExpressionBackend(backend, location)
  obj
}
