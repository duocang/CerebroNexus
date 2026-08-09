bench_assert_integer_scalar <- function(value, name, minimum, maximum) {
  if (
    length(value) != 1L ||
      !is.numeric(value) ||
      !is.finite(value) ||
      value != floor(value) ||
      value < minimum ||
      value > maximum
  ) {
    stop(
      name,
      " must be a finite integer-valued scalar in [",
      minimum,
      ", ",
      maximum,
      "]",
      call. = FALSE
    )
  }
  invisible(value)
}

bench_assert_tiers <- function(tiers, expected_names) {
  if (
    !is.numeric(tiers) ||
      length(tiers) != length(expected_names) ||
      !identical(names(tiers), expected_names) ||
      any(!is.finite(tiers)) ||
      any(tiers != floor(tiers)) ||
      any(tiers < 1) ||
      any(tiers > .Machine$integer.max)
  ) {
    stop(
      "tiers must be a named finite integer-valued vector with the protocol tier names",
      call. = FALSE
    )
  }
  invisible(tiers)
}

bench_assert_comparison_tiers <- function(tiers) {
  bench_assert_tiers(tiers, names(tiers))
  fixed_names <- head(names(tiers), -1L)
  if (
    length(fixed_names) < 1L ||
      tail(names(tiers), 1L) != "common" ||
      anyDuplicated(names(tiers)) ||
      any(!grepl("^tier_[0-9]+[km]$", fixed_names)) ||
      any(diff(as.double(tiers)) <= 0)
  ) {
    stop(
      "comparison tiers must be increasing named tiers followed by common",
      call. = FALSE
    )
  }
  invisible(tiers)
}

bench_stratified_blocks <- function(n_total, n_take) {
  bench_assert_integer_scalar(n_total, "n_total", 1, .Machine$integer.max)
  bench_assert_integer_scalar(n_take, "n_take", 1, n_total)

  stratum <- as.double(1:4)
  n_total <- as.double(n_total)
  n_take <- as.double(n_take)
  lo <- floor((stratum - 1) * n_total / 4) + 1
  hi <- floor(stratum * n_total / 4)
  capacity <- hi - lo + 1
  quota <- floor(stratum * n_take / 4) - floor((stratum - 1) * n_take / 4)
  start <- lo + floor((capacity - quota) / 2)
  end <- start + quota - 1

  values <- c(lo, hi, capacity, quota, start, end)
  in_range <- all(is.finite(values)) &&
    all(values == floor(values)) &&
    all(lo >= 1 & lo <= .Machine$integer.max) &&
    all(hi >= 0 & hi <= .Machine$integer.max) &&
    all(capacity >= 0 & capacity <= .Machine$integer.max) &&
    all(quota >= 0 & quota <= .Machine$integer.max) &&
    all(start >= 1 & start <= .Machine$integer.max) &&
    all(end >= 0 & end <= .Machine$integer.max)
  if (!in_range) {
    stop(
      "fixed-four block arithmetic exceeded the integer range",
      call. = FALSE
    )
  }

  data.frame(
    stratum = as.integer(stratum),
    start = as.integer(start),
    end = as.integer(end),
    n = as.integer(quota)
  )
}

bench_stratified_indices <- function(n_total, n_take) {
  blocks <- bench_stratified_blocks(n_total, n_take)
  selected <- lapply(seq_len(nrow(blocks)), function(i) {
    if (blocks$n[[i]] == 0L) {
      integer()
    } else {
      seq.int(blocks$start[[i]], blocks$end[[i]])
    }
  })
  as.integer(unlist(selected, use.names = FALSE))
}

bench_validate_nnz <- function(n_total, nnz_per_cell) {
  bench_assert_integer_scalar(n_total, "n_total", 1, .Machine$integer.max)
  if (
    !is.numeric(nnz_per_cell) ||
      length(nnz_per_cell) != n_total ||
      any(!is.finite(nnz_per_cell)) ||
      any(nnz_per_cell < 0) ||
      any(nnz_per_cell != floor(nnz_per_cell)) ||
      any(nnz_per_cell > 2^53)
  ) {
    stop(
      "nnz_per_cell must contain finite, nonnegative, exact integer-valued doubles",
      call. = FALSE
    )
  }
  invisible(nnz_per_cell)
}

bench_exact_sum <- function(values) {
  total <- 0
  for (value in values) {
    if (value > 2^53 - total) {
      stop(
        "selected nnz sum exceeds the exact-double integer range",
        call. = FALSE
      )
    }
    total <- total + value
  }
  total
}

bench_exact_selected_nnz <- function(n_total, n_take, nnz_per_cell) {
  bench_validate_nnz(n_total, nnz_per_cell)
  selected <- bench_stratified_indices(n_total, n_take)
  bench_exact_sum(nnz_per_cell[selected])
}

.bench_sha256_pattern <- "^[0-9a-f]{64}$"

.bench_require_namespace <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("package '", package, "' is required", call. = FALSE)
  }
}

bench_sha256_file <- function(path) {
  .bench_require_namespace("digest")
  if (
    !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)
  ) {
    stop("path must name one existing regular file", call. = FALSE)
  }
  info <- file.info(path)
  if (is.na(info$isdir) || info$isdir) {
    stop("path must name one existing regular file", call. = FALSE)
  }
  digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE)
}

bench_sha256_object <- function(object) {
  .bench_require_namespace("digest")
  payload <- serialize(object, NULL, ascii = FALSE, xdr = TRUE, version = 3)
  digest::digest(payload, algo = "sha256", serialize = FALSE)
}

.bench_canonical_ids <- function(ids, name = "identities") {
  if (!is.character(ids)) {
    stop(name, " must be character", call. = FALSE)
  }
  if (anyNA(ids)) {
    stop(name, " must not contain missing values", call. = FALSE)
  }
  if (any(Encoding(ids) == "bytes")) {
    stop(name, " must not use bytes encoding", call. = FALSE)
  }
  ids <- enc2utf8(ids)
  valid <- !is.na(iconv(ids, from = "UTF-8", to = "UTF-8", sub = NA_character_))
  if (!all(valid)) {
    stop(name, " must contain valid UTF-8", call. = FALSE)
  }
  if (any(!nzchar(ids))) {
    stop(name, " must not contain empty values", call. = FALSE)
  }
  if (anyDuplicated(ids)) {
    stop(name, " must not contain duplicate values", call. = FALSE)
  }
  unname(ids)
}

bench_identity_fingerprint <- function(ids) {
  ids <- .bench_canonical_ids(ids)
  bench_sha256_object(list(
    schema = "bench-identity-v1",
    type = "character",
    ids = ids
  ))
}

bench_numeric_fingerprint <- function(values, gene_ids, cell_ids) {
  genes <- .bench_canonical_ids(gene_ids, "gene_ids")
  cells <- .bench_canonical_ids(cell_ids, "cell_ids")
  expected_dim <- c(length(genes), length(cells))
  if (!is.numeric(values)) {
    stop("values must be numeric", call. = FALSE)
  }
  if (is.null(dim(values))) {
    if (length(genes) != 1L || length(values) != length(cells)) {
      stop(
        "vector values must have dimensions 1 x length(cell_ids)",
        call. = FALSE
      )
    }
  } else if (!identical(as.integer(dim(values)), as.integer(expected_dim))) {
    stop(
      "values dimensions must exactly match gene_ids x cell_ids",
      call. = FALSE
    )
  }
  values <- unname(as.double(values))
  if (any(!is.finite(values))) {
    stop("values must be finite", call. = FALSE)
  }
  bench_sha256_object(list(
    schema = "bench-numeric-v1",
    dimensions = as.integer(expected_dim),
    gene_ids = genes,
    cell_identity_sha256 = bench_identity_fingerprint(cells),
    values = values
  ))
}

.bench_source_identity <- function(path) {
  path <- .bench_scalar_string(path, "source path")
  if (.bench_is_symlink(path)) {
    stop("source identity refuses a symlink", call. = FALSE)
  }
  path <- normalizePath(path, mustWork = TRUE)
  if (.bench_is_symlink(path)) {
    stop("source identity refuses a symlink", call. = FALSE)
  }
  info <- file.info(path)
  if (
    is.na(info$isdir) ||
      info$isdir ||
      is.na(info$size) ||
      is.na(info$mode)
  ) {
    stop("source identity requires one existing regular file", call. = FALSE)
  }
  list(
    path = path,
    bytes = as.double(info$size),
    mode = as.integer(info$mode)
  )
}

bench_validate_source_file <- function(path, source_spec) {
  if (!is.list(source_spec)) {
    stop("source_spec must be a list", call. = FALSE)
  }
  if (
    !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)
  ) {
    stop("path must name one existing regular file", call. = FALSE)
  }
  if (.bench_is_symlink(path)) {
    stop("source path must not be a symlink", call. = FALSE)
  }
  if (
    !is.numeric(source_spec$expected_bytes) ||
      length(source_spec$expected_bytes) != 1L ||
      !is.finite(source_spec$expected_bytes) ||
      source_spec$expected_bytes < 0 ||
      source_spec$expected_bytes != floor(source_spec$expected_bytes)
  ) {
    stop(
      "source_spec expected_bytes must be one exact nonnegative integer",
      call. = FALSE
    )
  }
  if (
    !is.character(source_spec$expected_sha256) ||
      length(source_spec$expected_sha256) != 1L ||
      is.na(source_spec$expected_sha256) ||
      !grepl(.bench_sha256_pattern, source_spec$expected_sha256)
  ) {
    stop(
      "source_spec expected_sha256 must be 64 lowercase hexadecimal characters",
      call. = FALSE
    )
  }
  before <- .bench_source_identity(path)
  normalized <- before$path
  actual_bytes <- before$bytes
  if (
    !identical(as.double(actual_bytes), as.double(source_spec$expected_bytes))
  ) {
    stop("source file size does not match expected_bytes", call. = FALSE)
  }
  actual_sha256 <- bench_sha256_file(normalized)
  after <- .bench_source_identity(normalized)
  if (!identical(after, before)) {
    stop("source identity changed while hashing", call. = FALSE)
  }
  if (!identical(actual_sha256, source_spec$expected_sha256)) {
    stop("source file SHA-256 does not match expected_sha256", call. = FALSE)
  }
  list(
    path = normalized,
    bytes = actual_bytes,
    sha256 = actual_sha256,
    identity = before
  )
}

.bench_source_snapshot <- function(path, expected_bytes, expected_sha256) {
  before <- .bench_source_identity(path)
  if (!identical(before$bytes, as.double(expected_bytes))) {
    stop("source snapshot byte identity changed", call. = FALSE)
  }
  sha256 <- bench_sha256_file(before$path)
  after <- .bench_source_identity(before$path)
  if (!identical(after, before)) {
    stop("source identity changed while hashing", call. = FALSE)
  }
  if (!identical(sha256, expected_sha256)) {
    stop("source snapshot SHA-256 changed", call. = FALSE)
  }
  list(
    path = before$path,
    bytes = before$bytes,
    sha256 = sha256,
    identity = before
  )
}

.bench_assert_source_identity <- function(snapshot) {
  if (
    !is.list(snapshot) ||
      !identical(names(snapshot), c("path", "bytes", "sha256", "identity")) ||
      !is.list(snapshot$identity) ||
      !identical(
        names(snapshot$identity),
        c("path", "bytes", "mode")
      ) ||
      !identical(snapshot$path, snapshot$identity$path) ||
      !identical(as.double(snapshot$bytes), snapshot$identity$bytes) ||
      !is.character(snapshot$sha256) ||
      length(snapshot$sha256) != 1L ||
      is.na(snapshot$sha256) ||
      !grepl(.bench_sha256_pattern, snapshot$sha256)
  ) {
    stop("source snapshot identity is invalid", call. = FALSE)
  }
  observed <- .bench_source_identity(snapshot$path)
  if (!identical(observed, snapshot$identity)) {
    stop("source lightweight identity changed", call. = FALSE)
  }
  invisible(TRUE)
}

.bench_assert_source_snapshot <- function(snapshot) {
  .bench_assert_source_identity(snapshot)
  observed <- .bench_source_snapshot(
    snapshot$path,
    snapshot$bytes,
    snapshot$sha256
  )
  if (!identical(observed, snapshot)) {
    stop("source snapshot changed", call. = FALSE)
  }
  invisible(TRUE)
}

.bench_h5_dataset_dims <- function(path, dataset) {
  .bench_require_namespace("rhdf5")
  file_id <- rhdf5::H5Fopen(path, flags = "H5F_ACC_RDONLY")
  on.exit(rhdf5::H5Fclose(file_id))
  dataset_id <- rhdf5::H5Dopen(file_id, paste0("/", sub("^/", "", dataset)))
  on.exit(rhdf5::H5Dclose(dataset_id), add = TRUE)
  space_id <- rhdf5::H5Dget_space(dataset_id)
  on.exit(rhdf5::H5Sclose(space_id), add = TRUE)
  extent <- rhdf5::H5Sget_simple_extent_dims(space_id)
  as.double(extent$size)
}

.bench_h5ad_csr_metadata <- function(path, group) {
  if (
    !is.character(group) ||
      length(group) != 1L ||
      is.na(group) ||
      !nzchar(group)
  ) {
    stop("group must be one non-empty string", call. = FALSE)
  }
  attributes <- rhdf5::h5readAttributes(path, paste0("/", sub("^/", "", group)))
  encoding <- attributes[["encoding-type"]]
  if (
    !is.character(encoding) ||
      length(encoding) != 1L ||
      !identical(as.character(encoding), "csr_matrix")
  ) {
    stop("H5AD encoding-type must be exactly csr_matrix", call. = FALSE)
  }
  shape <- attributes[["shape"]]
  if (
    !is.numeric(shape) ||
      length(shape) != 2L ||
      any(!is.finite(shape)) ||
      any(shape != floor(shape)) ||
      any(shape <= 0) ||
      any(shape > .Machine$integer.max)
  ) {
    stop(
      "H5AD shape must contain two positive integer-valued dimensions",
      call. = FALSE
    )
  }
  as.integer(shape)
}

bench_read_h5ad_indptr <- function(path, group = "X") {
  .bench_require_namespace("rhdf5")
  if (
    !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)
  ) {
    stop("path must name one existing local file", call. = FALSE)
  }
  normalized <- normalizePath(path, mustWork = FALSE)
  info <- file.info(normalized)
  if (is.na(info$isdir) || info$isdir) {
    stop("source must be an existing local file", call. = FALSE)
  }
  shape <- .bench_h5ad_csr_metadata(normalized, group)
  dataset <- paste0(sub("/$", "", group), "/indptr")
  dims <- .bench_h5_dataset_dims(normalized, dataset)
  if (length(dims) != 1L) {
    stop("indptr dataset must have rank 1", call. = FALSE)
  }
  if (!identical(dims, as.double(shape[[1L]] + 1L))) {
    stop("indptr dataset length must equal shape[1] + 1", call. = FALSE)
  }
  indptr <- withCallingHandlers(
    rhdf5::h5read(
      normalized,
      paste0("/", dataset),
      bit64conversion = "double",
      drop = TRUE
    ),
    warning = function(warning) {
      message <- conditionMessage(warning)
      precision_loss <- grepl(
        "integer precision lost while converting 64-bit integer from HDF5 to double",
        message,
        fixed = TRUE
      )
      if (precision_loss) stop(message, call. = FALSE)
    }
  )
  indptr <- as.double(indptr)
  if (
    length(indptr) != shape[[1L]] + 1L ||
      any(!is.finite(indptr)) ||
      any(indptr != floor(indptr))
  ) {
    stop("indptr must contain finite integer-valued values", call. = FALSE)
  }
  if (indptr[[1L]] != 0) {
    stop("indptr must start at zero", call. = FALSE)
  }
  if (any(diff(indptr) < 0)) {
    stop("indptr must be monotone nondecreasing", call. = FALSE)
  }
  if (tail(indptr, 1L) > 2^53) {
    stop("indptr tail must not exceed 2^53", call. = FALSE)
  }
  increments <- diff(indptr)
  if (
    any(!is.finite(increments)) ||
      any(increments < 0) ||
      any(increments != floor(increments))
  ) {
    stop(
      "indptr differences must be nonnegative integer-valued values",
      call. = FALSE
    )
  }
  indptr
}

bench_open_source <- function(path, group = "X") {
  .bench_require_namespace("BPCells")
  BPCells::open_matrix_anndata_hdf5(path, group = group)
}

bench_source_inventory <- function(path, source_spec) {
  identity <- bench_validate_source_file(path, source_spec)
  group <- source_spec$group
  if (
    !is.character(group) ||
      length(group) != 1L ||
      is.na(group) ||
      !nzchar(group)
  ) {
    stop("source_spec group must be one non-empty string", call. = FALSE)
  }
  shape <- .bench_h5ad_csr_metadata(identity$path, group)
  if (
    !is.numeric(source_spec$n_cells) ||
      length(source_spec$n_cells) != 1L ||
      !is.finite(source_spec$n_cells) ||
      source_spec$n_cells != floor(source_spec$n_cells) ||
      source_spec$n_cells < 1 ||
      source_spec$n_cells != shape[[1L]]
  ) {
    stop("source_spec n_cells must exactly match H5AD shape[1]", call. = FALSE)
  }
  indptr <- bench_read_h5ad_indptr(identity$path, group)
  nnz <- tail(indptr, 1L)
  for (dataset in c("data", "indices")) {
    dims <- .bench_h5_dataset_dims(identity$path, paste0(group, "/", dataset))
    if (length(dims) != 1L || !identical(dims, nnz)) {
      stop(
        dataset,
        " dataset must have rank 1 and extent exactly equal to indptr tail",
        call. = FALSE
      )
    }
  }
  matrix <- bench_open_source(identity$path, group)
  list(
    path = identity$path,
    bytes = identity$bytes,
    sha256 = identity$sha256,
    identity = identity$identity,
    n_cells = as.integer(shape[[1L]]),
    n_genes = as.integer(shape[[2L]]),
    nnz = nnz,
    indptr = indptr,
    nnz_per_cell = diff(indptr),
    matrix = matrix
  )
}

.bench_validate_source_indices <- function(source_matrix, indices) {
  if (
    !is.numeric(indices) ||
      anyNA(indices) ||
      any(!is.finite(indices)) ||
      any(indices != floor(indices)) ||
      any(indices < 1) ||
      any(indices > ncol(source_matrix))
  ) {
    stop("indices must be positive in-range integer values", call. = FALSE)
  }
  indices <- as.integer(indices)
  if (anyDuplicated(indices)) {
    stop("indices must be unique", call. = FALSE)
  }
  if (is.unsorted(indices, strictly = TRUE)) {
    stop("indices must be in source order", call. = FALSE)
  }
  indices
}

bench_select_query_genes <- function(source_matrix, smallest_indices, n = 5L) {
  .bench_require_namespace("BPCells")
  bench_assert_integer_scalar(n, "n", 1, .Machine$integer.max)
  genes <- .bench_canonical_ids(
    rownames(source_matrix),
    "source row identities"
  )
  if (length(genes) != nrow(source_matrix)) {
    stop("source row identities are incomplete", call. = FALSE)
  }
  indices <- .bench_validate_source_indices(source_matrix, smallest_indices)
  stats <- BPCells::matrix_stats(
    source_matrix[, indices, drop = FALSE],
    row_stats = "nonzero",
    col_stats = "none"
  )
  counts <- as.double(stats$row_stats["nonzero", , drop = TRUE])
  if (
    length(counts) != length(genes) ||
      any(!is.finite(counts)) ||
      any(counts < 0) ||
      any(counts != floor(counts))
  ) {
    stop("BPCells returned invalid streaming nonzero counts", call. = FALSE)
  }
  active <- which(counts > 0)
  if (length(active) < n) {
    stop("source has fewer than n expressed genes", call. = FALSE)
  }
  density <- counts / length(indices)
  ordered_active <- active[order(density[active], active)]
  positions <- unique(as.integer(round(seq(
    1,
    length(ordered_active),
    length.out = n
  ))))
  if (length(positions) != n) {
    positions <- unique(
      as.integer(floor(seq(0, length(ordered_active) - 1, length.out = n))) + 1L
    )
  }
  selected <- ordered_active[positions]
  active_median <- stats::median(density[active])
  first <- selected[order(abs(density[selected] - active_median), selected)][[
    1L
  ]]
  selected <- c(first, selected[selected != first])
  tie_rank <- integer(length(genes))
  tie_rank[ordered_active] <- seq_along(ordered_active)
  data.frame(
    gene = genes[selected],
    role = c("first", rep("block", n - 1L)),
    density = unname(density[selected]),
    source_row = as.integer(selected),
    tie_break_rank = as.integer(tie_rank[selected]),
    stringsAsFactors = FALSE
  )
}

.bench_validate_query_genes <- function(genes, source_matrix) {
  schema <- c("gene", "role", "density", "source_row", "tie_break_rank")
  if (
    !is.data.frame(genes) ||
      !identical(names(genes), schema) ||
      nrow(genes) != 5L
  ) {
    stop(
      "genes must be a frozen five-gene panel with the fixed schema",
      call. = FALSE
    )
  }
  canonical <- .bench_canonical_ids(genes$gene, "genes")
  if (!identical(genes$role, c("first", rep("block", 4L)))) {
    stop("genes roles must be first followed by four block rows", call. = FALSE)
  }
  numeric_columns <- c("density", "source_row", "tie_break_rank")
  if (any(!vapply(genes[numeric_columns], is.numeric, logical(1)))) {
    stop("gene metadata columns must be numeric", call. = FALSE)
  }
  if (
    any(!is.finite(genes$density)) ||
      any(genes$density <= 0) ||
      any(!is.finite(genes$source_row)) ||
      any(genes$source_row != floor(genes$source_row)) ||
      any(genes$source_row < 1) ||
      any(genes$source_row > nrow(source_matrix)) ||
      any(!is.finite(genes$tie_break_rank)) ||
      any(genes$tie_break_rank != floor(genes$tie_break_rank))
  ) {
    stop("gene metadata values are invalid", call. = FALSE)
  }
  source_genes <- .bench_canonical_ids(
    rownames(source_matrix),
    "source row identities"
  )
  if (!identical(canonical, source_genes[as.integer(genes$source_row)])) {
    stop("genes do not match their source_row metadata", call. = FALSE)
  }
  genes$gene <- canonical
  genes
}

bench_build_query_plan <- function(source_matrix, indices, genes) {
  indices <- .bench_validate_source_indices(source_matrix, indices)
  expected <- bench_stratified_indices(ncol(source_matrix), length(indices))
  if (!identical(indices, expected)) {
    stop("indices must match fixed-four selection", call. = FALSE)
  }
  genes <- .bench_validate_query_genes(genes, source_matrix)
  if (!exists("BENCH_CONFIG", inherits = TRUE)) {
    stop("BENCH_CONFIG is required", call. = FALSE)
  }
  config <- get("BENCH_CONFIG", inherits = TRUE)
  source_sha256 <- config$source$expected_sha256
  if (
    !is.character(source_sha256) ||
      length(source_sha256) != 1L ||
      !grepl(.bench_sha256_pattern, source_sha256)
  ) {
    stop("BENCH_CONFIG source expected_sha256 is invalid", call. = FALSE)
  }
  block <- as.matrix(source_matrix[genes$gene, indices, drop = FALSE])
  cell_ids <- .bench_canonical_ids(
    colnames(source_matrix)[indices],
    "selected cell identities"
  )
  tier_density <- rowSums(block != 0) / length(indices)
  gene_metadata <- genes
  gene_metadata$density <- unname(as.double(tier_density))
  ordered_indices_sha256 <- bench_sha256_object(list(
    schema = "bench-ordered-indices-v1",
    indices = as.integer(indices)
  ))
  cell_identity_sha256 <- bench_identity_fingerprint(cell_ids)
  sampling_sha256 <- bench_sha256_object(list(
    schema = "bench-sampling-v1",
    dimensions = c(cells = as.integer(length(indices))),
    ordered_indices_sha256 = ordered_indices_sha256,
    cell_identity_sha256 = cell_identity_sha256
  ))
  payload <- list(
    schema = "bench-query-plan-v1",
    source_sha256 = source_sha256,
    sampling_sha256 = sampling_sha256,
    source_dimensions = c(
      genes = as.integer(nrow(source_matrix)),
      cells = as.integer(ncol(source_matrix))
    ),
    dimensions = c(genes = 5L, cells = as.integer(length(indices))),
    genes = gene_metadata,
    ordered_indices_sha256 = ordered_indices_sha256,
    cell_identity_sha256 = cell_identity_sha256,
    first_row_numeric_sha256 = bench_numeric_fingerprint(
      block[1L, , drop = FALSE],
      genes$gene[[1L]],
      cell_ids
    ),
    block_numeric_sha256 = bench_numeric_fingerprint(
      block,
      genes$gene,
      cell_ids
    ),
    boundaries = bench_stratified_blocks(ncol(source_matrix), length(indices))
  )
  payload$query_plan_sha256 <- bench_sha256_object(payload)
  payload
}

.bench_atomic_write_csv <- function(path, rows) {
  if (
    !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)
  ) {
    stop("path must be one non-empty string", call. = FALSE)
  }
  directory <- dirname(path)
  if (!dir.exists(directory)) {
    stop("manifest directory does not exist", call. = FALSE)
  }
  temporary <- tempfile(
    pattern = paste0(".", basename(path), "."),
    tmpdir = directory
  )
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  utils::write.table(
    rows,
    temporary,
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = TRUE,
    na = "NA",
    qmethod = "double",
    fileEncoding = "UTF-8"
  )
  if (!file.rename(temporary, path)) {
    stop("could not atomically install manifest", call. = FALSE)
  }
  invisible(normalizePath(path, mustWork = TRUE))
}

bench_write_sampling_manifest <- function(path, rows) {
  schema <- c(
    "tier_label",
    "n_cells",
    "stratum",
    "start",
    "end",
    "n",
    "exact_nnz",
    "indices_sha256",
    "cell_identity_sha256",
    "shell_sha256"
  )
  if (!is.data.frame(rows) || !identical(names(rows), schema)) {
    stop("sampling manifest schema is invalid", call. = FALSE)
  }
  if (
    !is.character(rows$tier_label) ||
      anyNA(rows$tier_label) ||
      any(!nzchar(rows$tier_label))
  ) {
    stop("sampling manifest tier_label must be nonempty", call. = FALSE)
  }
  if (
    !is.integer(rows$n_cells) || anyNA(rows$n_cells) || any(rows$n_cells < 1L)
  ) {
    stop("sampling manifest n_cells must be positive integers", call. = FALSE)
  }
  for (column in c("stratum", "start", "end", "n")) {
    if (
      !is.integer(rows[[column]]) ||
        anyNA(rows[[column]]) ||
        any(rows[[column]] < 0L)
    ) {
      stop(
        "sampling manifest ",
        column,
        " must be nonnegative integers",
        call. = FALSE
      )
    }
  }
  if (
    any(rows$stratum < 1L | rows$stratum > 4L) ||
      anyDuplicated(paste(rows$tier_label, rows$stratum, sep = "\r"))
  ) {
    stop(
      "sampling manifest tier_label and stratum keys must be unique",
      call. = FALSE
    )
  }
  for (tier in unique(rows$tier_label)) {
    tier_rows <- rows[rows$tier_label == tier, , drop = FALSE]
    if (!setequal(tier_rows$stratum, 1:4) || nrow(tier_rows) != 4L) {
      stop(
        "sampling manifest must contain exact strata 1:4 for every tier",
        call. = FALSE
      )
    }
    if (
      any(tier_rows$start < 1L) ||
        any(
          as.double(tier_rows$end) !=
            as.double(tier_rows$start) + as.double(tier_rows$n) - 1
        )
    ) {
      stop("sampling manifest boundary arithmetic is invalid", call. = FALSE)
    }
    constant_columns <- c(
      "n_cells",
      "exact_nnz",
      "indices_sha256",
      "cell_identity_sha256",
      "shell_sha256"
    )
    if (
      any(vapply(
        tier_rows[constant_columns],
        function(x) {
          length(unique(x)) != 1L
        },
        logical(1)
      ))
    ) {
      stop(
        "sampling manifest tier metadata must be consistent across strata",
        call. = FALSE
      )
    }
    if (sum(as.double(tier_rows$n)) != unique(tier_rows$n_cells)) {
      stop(
        "sampling manifest stratum n values must sum to n_cells",
        call. = FALSE
      )
    }
  }
  if (
    !is.numeric(rows$exact_nnz) ||
      anyNA(rows$exact_nnz) ||
      any(!is.finite(rows$exact_nnz)) ||
      any(rows$exact_nnz < 0) ||
      any(rows$exact_nnz != floor(rows$exact_nnz)) ||
      any(rows$exact_nnz > 2^53)
  ) {
    stop(
      "sampling manifest exact_nnz must be exact nonnegative integers",
      call. = FALSE
    )
  }
  for (column in c("indices_sha256", "cell_identity_sha256")) {
    if (
      !is.character(rows[[column]]) ||
        anyNA(rows[[column]]) ||
        any(!grepl(.bench_sha256_pattern, rows[[column]]))
    ) {
      stop("sampling manifest ", column, " is invalid", call. = FALSE)
    }
  }
  if (
    !is.character(rows$shell_sha256) ||
      any(
        !is.na(rows$shell_sha256) &
          !grepl(.bench_sha256_pattern, rows$shell_sha256)
      )
  ) {
    stop("sampling manifest shell_sha256 is invalid", call. = FALSE)
  }
  rows$tier_label <- enc2utf8(rows$tier_label)
  .bench_atomic_write_csv(path, rows)
}

bench_write_query_manifest <- function(path, plans) {
  if (
    !is.list(plans) ||
      is.null(names(plans)) ||
      anyNA(names(plans)) ||
      any(!nzchar(names(plans))) ||
      anyDuplicated(names(plans))
  ) {
    stop("query plan tier names must be nonempty and unique", call. = FALSE)
  }
  rows <- lapply(seq_along(plans), function(i) {
    plan <- plans[[i]]
    required <- c(
      "schema",
      "source_sha256",
      "sampling_sha256",
      "source_dimensions",
      "dimensions",
      "genes",
      "ordered_indices_sha256",
      "cell_identity_sha256",
      "first_row_numeric_sha256",
      "block_numeric_sha256",
      "boundaries",
      "query_plan_sha256"
    )
    if (
      !is.list(plan) ||
        !setequal(names(plan), required) ||
        length(plan) != length(required) ||
        !identical(plan$schema, "bench-query-plan-v1")
    ) {
      stop("query plan schema is invalid", call. = FALSE)
    }
    if (
      !is.data.frame(plan$genes) ||
        nrow(plan$genes) == 0L ||
        !identical(
          names(plan$genes),
          c("gene", "role", "density", "source_row", "tie_break_rank")
        )
    ) {
      stop("query plan gene schema is invalid", call. = FALSE)
    }
    valid_dimensions <- function(x) {
      is.numeric(x) &&
        identical(names(x), c("genes", "cells")) &&
        length(x) == 2L &&
        all(is.finite(x)) &&
        all(x == floor(x)) &&
        all(x > 0) &&
        all(x <= .Machine$integer.max)
    }
    if (
      nrow(plan$genes) != 5L ||
        !valid_dimensions(plan$dimensions) ||
        !identical(names(plan$dimensions), c("genes", "cells")) ||
        !identical(as.integer(plan$dimensions[["genes"]]), 5L)
    ) {
      stop("query plan must describe exactly five genes", call. = FALSE)
    }
    if (
      !valid_dimensions(plan$source_dimensions) ||
        plan$source_dimensions[["genes"]] < 5 ||
        plan$source_dimensions[["cells"]] < plan$dimensions[["cells"]]
    ) {
      stop("query plan source dimensions are invalid", call. = FALSE)
    }
    if (!identical(plan$genes$role, c("first", rep("block", 4L)))) {
      stop(
        "query plan gene roles must be first followed by four block rows",
        call. = FALSE
      )
    }
    .bench_canonical_ids(plan$genes$gene, "query plan genes")
    if (
      !is.numeric(plan$genes$density) ||
        any(!is.finite(plan$genes$density)) ||
        any(plan$genes$density < 0) ||
        any(plan$genes$density > 1) ||
        !is.numeric(plan$genes$source_row) ||
        anyNA(plan$genes$source_row) ||
        any(plan$genes$source_row != floor(plan$genes$source_row)) ||
        any(plan$genes$source_row < 1) ||
        any(plan$genes$source_row > plan$source_dimensions[["genes"]]) ||
        anyDuplicated(plan$genes$source_row) ||
        !is.numeric(plan$genes$tie_break_rank) ||
        anyNA(plan$genes$tie_break_rank) ||
        any(plan$genes$tie_break_rank != floor(plan$genes$tie_break_rank)) ||
        any(plan$genes$tie_break_rank < 1)
    ) {
      stop("query plan gene metadata is invalid", call. = FALSE)
    }
    expected_boundaries <- bench_stratified_blocks(
      as.integer(plan$source_dimensions[["cells"]]),
      as.integer(plan$dimensions[["cells"]])
    )
    if (!identical(plan$boundaries, expected_boundaries)) {
      stop("query plan boundaries are invalid", call. = FALSE)
    }
    hashes <- c(
      "source_sha256",
      "sampling_sha256",
      "ordered_indices_sha256",
      "cell_identity_sha256",
      "first_row_numeric_sha256",
      "block_numeric_sha256",
      "query_plan_sha256"
    )
    if (
      any(
        !vapply(
          plan[hashes],
          function(x) {
            is.character(x) &&
              length(x) == 1L &&
              !is.na(x) &&
              grepl(.bench_sha256_pattern, x)
          },
          logical(1)
        )
      )
    ) {
      stop("query plan hashes are invalid", call. = FALSE)
    }
    expected_plan_sha256 <- bench_sha256_object(plan[
      names(plan) != "query_plan_sha256"
    ])
    if (!identical(plan$query_plan_sha256, expected_plan_sha256)) {
      stop(
        "query plan query_plan_sha256 does not match its typed payload",
        call. = FALSE
      )
    }
    data.frame(
      schema = as.character(plan$schema),
      tier_label = names(plans)[[i]],
      source_sha256 = plan$source_sha256,
      sampling_sha256 = plan$sampling_sha256,
      n_genes = as.integer(plan$dimensions[["genes"]]),
      n_cells = as.integer(plan$dimensions[["cells"]]),
      gene = enc2utf8(plan$genes$gene),
      role = plan$genes$role,
      density = as.double(plan$genes$density),
      source_row = as.integer(plan$genes$source_row),
      tie_break_rank = as.integer(plan$genes$tie_break_rank),
      ordered_indices_sha256 = plan$ordered_indices_sha256,
      cell_identity_sha256 = plan$cell_identity_sha256,
      first_row_numeric_sha256 = plan$first_row_numeric_sha256,
      block_numeric_sha256 = plan$block_numeric_sha256,
      query_plan_sha256 = plan$query_plan_sha256,
      stringsAsFactors = FALSE
    )
  })
  rows <- do.call(rbind, rows)
  rownames(rows) <- NULL
  if (anyDuplicated(paste(rows$tier_label, rows$gene, sep = "\r"))) {
    stop("query manifest tier and gene rows must be unique", call. = FALSE)
  }
  .bench_atomic_write_csv(path, rows)
}

.bench_validate_shell_inputs <- function(expression, source_indices) {
  dimensions <- dim(expression)
  if (
    !is.numeric(dimensions) ||
      length(dimensions) != 2L ||
      anyNA(dimensions) ||
      any(dimensions < 1) ||
      any(dimensions != floor(dimensions))
  ) {
    stop(
      "expression must be a non-empty two-dimensional matrix-like object",
      call. = FALSE
    )
  }
  genes <- .bench_canonical_ids(
    rownames(expression),
    "expression row identities"
  )
  cells <- .bench_canonical_ids(
    colnames(expression),
    "expression column identities"
  )
  if (length(genes) != dimensions[[1L]] || length(cells) != dimensions[[2L]]) {
    stop(
      "expression identities must exactly match its dimensions",
      call. = FALSE
    )
  }
  if (
    !is.numeric(source_indices) ||
      length(source_indices) != dimensions[[2L]] ||
      anyNA(source_indices) ||
      any(!is.finite(source_indices)) ||
      any(source_indices != floor(source_indices)) ||
      any(source_indices < 1) ||
      any(source_indices > .Machine$integer.max)
  ) {
    stop(
      "source_indices must contain one finite positive integer for each column length",
      call. = FALSE
    )
  }
  source_indices <- as.integer(source_indices)
  if (anyDuplicated(source_indices)) {
    stop("source_indices must be unique", call. = FALSE)
  }
  list(
    dimensions = as.integer(dimensions),
    genes = genes,
    cells = cells,
    source_indices = source_indices
  )
}

bench_make_seurat_shell <- function(expression, source_indices) {
  .bench_require_namespace("SeuratObject")
  validated <- .bench_validate_shell_inputs(expression, source_indices)
  assay <- SeuratObject::CreateAssay5Object(data = expression)
  object <- suppressWarnings(
    SeuratObject::CreateSeuratObject(counts = assay, assay = "RNA")
  )
  object$sample <- factor(paste0(
    "sample_",
    validated$source_indices %% 8L + 1L
  ))
  object$cluster <- factor(paste0(
    "cluster_",
    validated$source_indices %% 32L + 1L
  ))
  object$nUMI <- 0
  object$nGene <- 0
  embedding <- cbind(
    UMAP_1 = sin(validated$source_indices / 1000),
    UMAP_2 = cos(validated$source_indices / 1000)
  )
  rownames(embedding) <- validated$cells
  object[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings = embedding,
    key = "UMAP_",
    assay = "RNA"
  )
  object
}

bench_shell_fingerprint <- function(object, source_indices) {
  .bench_require_namespace("SeuratObject")
  if (!inherits(object, "Seurat") || !"RNA" %in% names(object)) {
    stop(
      "object must be a Seurat shell containing the RNA assay",
      call. = FALSE
    )
  }
  layers <- SeuratObject::Layers(object[["RNA"]])
  if (!identical(layers, "data")) {
    stop(
      "RNA assay must contain exactly one complete data layer",
      call. = FALSE
    )
  }
  expression <- SeuratObject::LayerData(object[["RNA"]], layer = "data")
  validated <- .bench_validate_shell_inputs(expression, source_indices)
  metadata <- object[[]]
  metadata_ids <- .bench_canonical_ids(
    rownames(metadata),
    "metadata row identities"
  )
  if (
    length(metadata_ids) != validated$dimensions[[2L]] ||
      !identical(metadata_ids, validated$cells)
  ) {
    stop(
      "metadata row identities must exactly match expression cell identities",
      call. = FALSE
    )
  }
  required_metadata <- c("sample", "cluster", "nUMI", "nGene")
  if (!all(required_metadata %in% names(metadata))) {
    stop("synthetic shell metadata columns are incomplete", call. = FALSE)
  }
  expected_sample <- factor(paste0(
    "sample_",
    validated$source_indices %% 8L + 1L
  ))
  expected_cluster <- factor(paste0(
    "cluster_",
    validated$source_indices %% 32L + 1L
  ))
  if (
    !identical(metadata$sample, expected_sample) ||
      !identical(metadata$cluster, expected_cluster)
  ) {
    stop(
      "synthetic shell group metadata does not match the source-index rules",
      call. = FALSE
    )
  }
  if (
    !is.numeric(metadata$nUMI) ||
      !is.numeric(metadata$nGene) ||
      anyNA(metadata$nUMI) ||
      anyNA(metadata$nGene) ||
      any(metadata$nUMI != 0) ||
      any(metadata$nGene != 0)
  ) {
    stop("synthetic shell QC metadata must be numeric zeros", call. = FALSE)
  }
  if (!"umap" %in% names(object@reductions)) {
    stop("synthetic shell must contain the umap projection", call. = FALSE)
  }
  embedding <- SeuratObject::Embeddings(object[["umap"]])
  expected_embedding <- cbind(
    UMAP_1 = sin(validated$source_indices / 1000),
    UMAP_2 = cos(validated$source_indices / 1000)
  )
  rownames(expected_embedding) <- validated$cells
  if (
    !identical(dim(embedding), dim(expected_embedding)) ||
      !identical(dimnames(embedding), dimnames(expected_embedding)) ||
      !isTRUE(all.equal(
        unname(embedding),
        unname(expected_embedding),
        tolerance = 0
      ))
  ) {
    stop(
      "synthetic shell UMAP does not match the source-index rules",
      call. = FALSE
    )
  }
  bench_sha256_object(list(
    schema = "bench-seurat-shell-v1",
    dimensions = c(
      genes = validated$dimensions[[1L]],
      cells = validated$dimensions[[2L]]
    ),
    ordered_source_indices_sha256 = bench_sha256_object(list(
      schema = "bench-ordered-source-indices-v1",
      indices = validated$source_indices
    )),
    cell_identity_sha256 = bench_identity_fingerprint(validated$cells),
    metadata_columns = enc2utf8(names(metadata)),
    metadata_factor_levels = lapply(
      metadata[vapply(metadata, is.factor, logical(1L))],
      function(column) enc2utf8(levels(column))
    ),
    sample = list(
      rule = "sample_(source_index %% 8 + 1)",
      values = as.character(metadata$sample),
      levels = levels(metadata$sample)
    ),
    cluster = list(
      rule = "cluster_(source_index %% 32 + 1)",
      values = as.character(metadata$cluster),
      levels = levels(metadata$cluster)
    ),
    qc = list(
      rule = "nUMI=nGene=0",
      nUMI = as.double(metadata$nUMI),
      nGene = as.double(metadata$nGene)
    ),
    umap = list(
      rule = "UMAP_1=sin(source_index/1000);UMAP_2=cos(source_index/1000)",
      column_names = colnames(embedding)
    )
  ))
}

bench_timed_value <- function(expr) {
  started <- proc.time()[["elapsed"]]
  value <- force(expr)
  list(
    seconds = unname(proc.time()[["elapsed"]] - started),
    value = value
  )
}

.bench_query_measurement_fields <- function(plan) {
  if (
    !is.list(plan) ||
      !is.data.frame(plan$genes) ||
      !all(c("gene", "role") %in% names(plan$genes)) ||
      nrow(plan$genes) != 5L ||
      !identical(as.character(plan$genes$role), c("first", rep("block", 4L)))
  ) {
    stop("plan must contain the frozen five-gene query panel", call. = FALSE)
  }
  genes <- .bench_canonical_ids(
    as.character(plan$genes$gene),
    "query plan genes"
  )
  for (field in c("first_row_numeric_sha256", "block_numeric_sha256")) {
    value <- plan[[field]]
    if (
      !is.character(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !grepl(.bench_sha256_pattern, value)
    ) {
      stop("plan ", field, " is invalid", call. = FALSE)
    }
  }
  list(first_gene = genes[[1L]], block_genes = genes)
}

.bench_validate_timed_result <- function(result) {
  if (
    !is.list(result) ||
      !identical(names(result), c("seconds", "value")) ||
      !is.numeric(result$seconds) ||
      length(result$seconds) != 1L ||
      !is.finite(result$seconds) ||
      result$seconds < 0
  ) {
    stop(
      "timer must return one nonnegative duration and its value",
      call. = FALSE
    )
  }
  result
}

bench_measure_queries <- function(object, plan, timer = bench_timed_value) {
  fields <- .bench_query_measurement_fields(plan)
  if (
    !is.function(timer) ||
      !is.function(object$getExpressionRow) ||
      !is.function(object$getExpressionBlock)
  ) {
    stop(
      "object and timer do not implement the query measurement interface",
      call. = FALSE
    )
  }
  first <- .bench_validate_timed_result(timer(object$getExpressionRow(
    fields$first_gene
  )))
  warmed <- vapply(
    seq_len(5L),
    function(i) {
      .bench_validate_timed_result(timer(object$getExpressionRow(
        fields$first_gene
      )))$seconds
    },
    numeric(1L)
  )
  prepared <- .bench_validate_timed_result(timer(object$getExpressionBlock(
    fields$block_genes
  )))
  materialized <- .bench_validate_timed_result(timer(as.matrix(prepared$value)))
  observed <- materialized$value
  list(
    first_query_secs = first$seconds,
    warmed_secs = warmed,
    warmed_median_secs = stats::median(warmed),
    block_prepare_secs = prepared$seconds,
    block_materialize_secs = materialized$seconds,
    block_ready_secs = prepared$seconds + materialized$seconds,
    row_fingerprint = bench_numeric_fingerprint(
      first$value,
      fields$first_gene,
      names(first$value)
    ),
    block_fingerprint = bench_numeric_fingerprint(
      observed,
      rownames(observed),
      colnames(observed)
    )
  )
}

bench_validate_query_measurement <- function(measurement, plan) {
  .bench_query_measurement_fields(plan)
  required <- c(
    "first_query_secs",
    "warmed_secs",
    "warmed_median_secs",
    "block_prepare_secs",
    "block_materialize_secs",
    "block_ready_secs",
    "row_fingerprint",
    "block_fingerprint"
  )
  if (
    !is.list(measurement) ||
      !identical(names(measurement), required) ||
      !is.character(measurement$row_fingerprint) ||
      length(measurement$row_fingerprint) != 1L ||
      !is.character(measurement$block_fingerprint) ||
      length(measurement$block_fingerprint) != 1L
  ) {
    stop("query measurement schema is invalid", call. = FALSE)
  }
  scalar_timing_fields <- c(
    "first_query_secs",
    "warmed_median_secs",
    "block_prepare_secs",
    "block_materialize_secs",
    "block_ready_secs"
  )
  valid_scalar_timing <- function(value) {
    is.numeric(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      is.finite(value) &&
      value >= 0
  }
  if (
    !all(vapply(
      measurement[scalar_timing_fields],
      valid_scalar_timing,
      logical(1L)
    ))
  ) {
    stop(
      "query measurement timing fields must be finite nonnegative numeric scalars",
      call. = FALSE
    )
  }
  if (
    !is.numeric(measurement$warmed_secs) ||
      length(measurement$warmed_secs) != 5L ||
      anyNA(measurement$warmed_secs) ||
      any(!is.finite(measurement$warmed_secs)) ||
      any(measurement$warmed_secs < 0)
  ) {
    stop(
      "query measurement warmed_secs must contain five finite nonnegative numbers",
      call. = FALSE
    )
  }
  if (
    !identical(
      as.double(measurement$warmed_median_secs),
      as.double(stats::median(measurement$warmed_secs))
    )
  ) {
    stop(
      "query measurement warmed median is inconsistent with warmed_secs",
      call. = FALSE
    )
  }
  if (
    !identical(
      as.double(measurement$block_ready_secs),
      as.double(
        measurement$block_prepare_secs + measurement$block_materialize_secs
      )
    )
  ) {
    stop(
      "query measurement block_ready is inconsistent with its timed components",
      call. = FALSE
    )
  }
  if (!identical(measurement$row_fingerprint, plan$first_row_numeric_sha256)) {
    stop(
      "observed row fingerprint does not match the frozen query plan",
      call. = FALSE
    )
  }
  if (!identical(measurement$block_fingerprint, plan$block_numeric_sha256)) {
    stop(
      "observed block fingerprint does not match the frozen query plan",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

bench_freeze_common_tier <- function(
  target,
  minimum_exclusive,
  nnz_per_cell,
  limit
) {
  n_total <- length(nnz_per_cell)
  bench_validate_nnz(n_total, nnz_per_cell)
  bench_assert_integer_scalar(target, "target", 1, n_total)
  bench_assert_integer_scalar(
    minimum_exclusive,
    "minimum_exclusive",
    0,
    target - 1
  )
  bench_assert_integer_scalar(limit, "limit", 0, 2^53)

  low <- 0
  high <- target
  while (low < high) {
    candidate <- floor((low + high + 1) / 2)
    candidate_nnz <- bench_exact_selected_nnz(n_total, candidate, nnz_per_cell)
    if (candidate_nnz <= limit) low <- candidate else high <- candidate - 1
  }
  if (low <= minimum_exclusive) {
    stop("no legal common tier exceeds minimum_exclusive", call. = FALSE)
  }

  exact_nnz <- bench_exact_selected_nnz(n_total, low, nnz_per_cell)
  if (exact_nnz > limit) {
    stop("frozen common tier exceeds limit", call. = FALSE)
  }
  if (
    low < target &&
      bench_exact_selected_nnz(n_total, low + 1, nnz_per_cell) <= limit
  ) {
    stop("frozen common tier is not maximal", call. = FALSE)
  }

  list(
    common_target_actual = as.integer(low),
    exact_nnz = exact_nnz,
    target_reduced = low < target
  )
}

bench_rotate <- function(values, shift) {
  values[((seq_along(values) - 1L + shift) %% length(values)) + 1L]
}

bench_schedule_frame <- function(
  panel,
  tiers,
  backends,
  repeats,
  export_tier_shift,
  access_tier_shift,
  export_backend_shift,
  access_backend_shift
) {
  rows <- lapply(seq_len(repeats), function(repeat_id) {
    canonical <- expand.grid(
      backend = backends,
      tier_label = names(tiers),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    canonical <- canonical[, c("tier_label", "backend")]
    pair_id <- paste(
      panel,
      repeat_id,
      canonical$tier_label,
      canonical$backend,
      sep = ":"
    )
    result <- data.frame(
      pair_id = pair_id,
      panel = rep(panel, nrow(canonical)),
      repeat_id = rep(as.integer(repeat_id), nrow(canonical)),
      tier_label = canonical$tier_label,
      n_cells = as.integer(unname(tiers[canonical$tier_label])),
      backend = canonical$backend,
      export_order = integer(nrow(canonical)),
      access_order = integer(nrow(canonical)),
      stringsAsFactors = FALSE
    )
    names(result)[[3L]] <- "repeat"
    result
  })
  schedule <- do.call(rbind, rows)
  rownames(schedule) <- NULL

  ordered_ids <- function(tier_shift, backend_shift) {
    unlist(
      lapply(seq_len(repeats), function(repeat_id) {
        tier_order <- bench_rotate(names(tiers), tier_shift(repeat_id))
        backend_order <- bench_rotate(backends, backend_shift(repeat_id))
        unlist(
          lapply(tier_order, function(tier_label) {
            paste(panel, repeat_id, tier_label, backend_order, sep = ":")
          }),
          use.names = FALSE
        )
      }),
      use.names = FALSE
    )
  }
  export_ids <- ordered_ids(export_tier_shift, export_backend_shift)
  access_ids <- ordered_ids(access_tier_shift, access_backend_shift)
  schedule$export_order <- as.integer(match(schedule$pair_id, export_ids))
  schedule$access_order <- as.integer(match(schedule$pair_id, access_ids))
  schedule
}

bench_comparison_schedule <- function(tiers, backends, repeats = 3L) {
  bench_assert_comparison_tiers(tiers)
  if (!identical(backends, c("embedded", "bpcells", "h5"))) {
    stop("backends must be c('embedded', 'bpcells', 'h5')", call. = FALSE)
  }
  bench_assert_integer_scalar(repeats, "repeats", 1, .Machine$integer.max)
  schedule <- bench_schedule_frame(
    "comparison",
    tiers,
    backends,
    repeats,
    function(repeat_id) repeat_id - 1L,
    function(repeat_id) repeat_id,
    function(repeat_id) repeat_id - 1L,
    function(repeat_id) repeat_id + 1L
  )
  bench_validate_schedule(schedule, length(tiers) * length(backends) * repeats)
  schedule
}

bench_full_schedule <- function(tiers, repeats = 4L) {
  bench_assert_tiers(tiers, c("common", "tier_1m", "tier_2m", "full"))
  bench_assert_integer_scalar(repeats, "repeats", 1, .Machine$integer.max)
  schedule <- bench_schedule_frame(
    "full_scale",
    tiers,
    "bpcells",
    repeats,
    function(repeat_id) repeat_id - 1L,
    function(repeat_id) repeat_id + 1L,
    function(repeat_id) 0L,
    function(repeat_id) 0L
  )
  bench_validate_schedule(schedule, length(tiers) * repeats)
  schedule
}

bench_tier_order <- function(
  schedule,
  `repeat`,
  phase = c("export", "access")
) {
  phase <- match.arg(phase)
  bench_assert_integer_scalar(`repeat`, "repeat", 1, .Machine$integer.max)
  selected <- schedule[schedule[["repeat"]] == `repeat`, , drop = FALSE]
  if (nrow(selected) == 0L) {
    stop("repeat is absent from schedule", call. = FALSE)
  }
  unique(selected$tier_label[order(selected[[paste0(phase, "_order")]])])
}

bench_backend_order <- function(
  schedule,
  `repeat`,
  tier_label,
  phase = c("export", "access")
) {
  phase <- match.arg(phase)
  bench_assert_integer_scalar(`repeat`, "repeat", 1, .Machine$integer.max)
  if (
    length(tier_label) != 1L || !is.character(tier_label) || is.na(tier_label)
  ) {
    stop("tier_label must be one non-missing string", call. = FALSE)
  }
  selected <- schedule[
    schedule[["repeat"]] == `repeat` & schedule$tier_label == tier_label,
    ,
    drop = FALSE
  ]
  if (nrow(selected) == 0L) {
    stop("repeat and tier_label are absent from schedule", call. = FALSE)
  }
  unique(selected$backend[order(selected[[paste0(phase, "_order")]])])
}

bench_validate_schedule <- function(schedule, expected_rows) {
  schema <- c(
    "pair_id",
    "panel",
    "repeat",
    "tier_label",
    "n_cells",
    "backend",
    "export_order",
    "access_order"
  )
  bench_assert_integer_scalar(
    expected_rows,
    "expected_rows",
    1,
    .Machine$integer.max
  )
  if (!is.data.frame(schedule) || !identical(names(schedule), schema)) {
    stop("schedule schema is invalid", call. = FALSE)
  }
  if (nrow(schedule) != expected_rows) {
    stop("schedule row count is invalid", call. = FALSE)
  }

  panels <- unique(schedule$panel)
  if (
    !is.character(schedule$panel) ||
      anyNA(schedule$panel) ||
      length(panels) != 1L ||
      !panels %in% c("comparison", "full_scale")
  ) {
    stop(
      "schedule panel must be exactly one of 'comparison' or 'full_scale'",
      call. = FALSE
    )
  }
  expected_tiers <- if (panels == "comparison") {
    labels <- unique(schedule$tier_label)
    sizes <- vapply(
      labels,
      function(label) unique(schedule$n_cells[schedule$tier_label == label]),
      numeric(1L)
    )
    bench_assert_comparison_tiers(stats::setNames(sizes, labels))
    labels
  } else {
    c("common", "tier_1m", "tier_2m", "full")
  }
  expected_backends <- if (panels == "comparison") {
    c("embedded", "bpcells", "h5")
  } else {
    "bpcells"
  }
  if (
    !is.character(schedule$tier_label) ||
      anyNA(schedule$tier_label) ||
      !setequal(unique(schedule$tier_label), expected_tiers)
  ) {
    stop("schedule tier_label set is invalid for panel", call. = FALSE)
  }
  if (
    !is.character(schedule$backend) ||
      anyNA(schedule$backend) ||
      !setequal(unique(schedule$backend), expected_backends)
  ) {
    stop("schedule backend set is invalid for panel", call. = FALSE)
  }

  repeats <- schedule[["repeat"]]
  if (
    !is.numeric(repeats) ||
      any(!is.finite(repeats)) ||
      any(repeats != floor(repeats)) ||
      any(repeats < 1) ||
      any(repeats > .Machine$integer.max)
  ) {
    stop("repeat values must be positive numeric integers", call. = FALSE)
  }
  expected_repeat_order <- seq_len(length(unique(repeats)))
  if (!identical(sort(unique(as.integer(repeats))), expected_repeat_order)) {
    stop("repeat IDs must start at 1 and be consecutive", call. = FALSE)
  }
  repeats <- as.integer(repeats)

  n_cells <- schedule$n_cells
  if (
    !is.numeric(n_cells) ||
      any(!is.finite(n_cells)) ||
      any(n_cells != floor(n_cells)) ||
      any(n_cells < 1) ||
      any(n_cells > .Machine$integer.max)
  ) {
    stop("n_cells must contain positive integer-valued counts", call. = FALSE)
  }
  for (tier_label in expected_tiers) {
    if (length(unique(n_cells[schedule$tier_label == tier_label])) != 1L) {
      stop(
        "n_cells must map each tier_label consistently across repeats",
        call. = FALSE
      )
    }
  }

  expected_grid <- as.vector(outer(
    expected_tiers,
    expected_backends,
    paste,
    sep = ":"
  ))
  for (repeat_id in expected_repeat_order) {
    positions <- which(repeats == repeat_id)
    actual_grid <- paste(
      schedule$tier_label[positions],
      schedule$backend[positions],
      sep = ":"
    )
    if (
      length(actual_grid) != length(expected_grid) ||
        anyDuplicated(actual_grid) ||
        !setequal(actual_grid, expected_grid)
    ) {
      stop(
        "each repeat must contain the complete unique tier_label x backend grid",
        call. = FALSE
      )
    }
  }

  if (anyNA(schedule$pair_id) || anyDuplicated(schedule$pair_id)) {
    stop("schedule pair_id values must be unique", call. = FALSE)
  }
  expected_pair_id <- paste(
    schedule$panel,
    repeats,
    schedule$tier_label,
    schedule$backend,
    sep = ":"
  )
  if (!identical(schedule$pair_id, expected_pair_id)) {
    stop(
      "schedule pair_id must encode panel, repeat, tier_label, and backend",
      call. = FALSE
    )
  }
  expected_order <- seq_len(expected_rows)
  for (column in c("export_order", "access_order")) {
    value <- schedule[[column]]
    if (
      !is.numeric(value) ||
        anyNA(value) ||
        !identical(sort(as.integer(value)), expected_order) ||
        any(value != as.integer(value))
    ) {
      stop(column, " must be a global 1:n permutation", call. = FALSE)
    }
  }
  for (column in c("export_order", "access_order")) {
    execution_repeats <- repeats[order(schedule[[column]])]
    if (
      !identical(unique(execution_repeats), expected_repeat_order) ||
        anyDuplicated(rle(execution_repeats)$values)
    ) {
      stop(column, " must keep numeric repeat blocks contiguous", call. = FALSE)
    }
  }
  for (repeat_id in unique(repeats)) {
    positions <- which(repeats == repeat_id)
    if (length(positions) > 1L && any(diff(positions) != 1L)) {
      stop("numeric repeat blocks must be contiguous", call. = FALSE)
    }
    export_ids <- schedule$pair_id[positions][order(schedule$export_order[
      positions
    ])]
    access_ids <- schedule$pair_id[positions][order(schedule$access_order[
      positions
    ])]
    if (
      !setequal(export_ids, schedule$pair_id[positions]) ||
        !setequal(access_ids, schedule$pair_id[positions]) ||
        identical(export_ids, access_ids)
    ) {
      stop(
        "each repeat needs different complete export and access pair_id orders",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

bench_eligibility <- function(panel, tiers, exact_nnz, limit) {
  if (length(panel) != 1L || !panel %in% c("comparison", "full_scale")) {
    stop("panel must be 'comparison' or 'full_scale'", call. = FALSE)
  }
  expected_names <- if (panel == "comparison") {
    names(tiers)
  } else {
    c("common", "tier_1m", "tier_2m", "full")
  }
  if (panel == "comparison") {
    bench_assert_comparison_tiers(tiers)
  } else {
    bench_assert_tiers(tiers, expected_names)
  }
  if (
    !is.numeric(exact_nnz) ||
      !identical(names(exact_nnz), names(tiers)) ||
      any(!is.finite(exact_nnz)) ||
      any(exact_nnz < 0) ||
      any(exact_nnz != floor(exact_nnz)) ||
      any(exact_nnz > 2^53)
  ) {
    stop(
      "exact_nnz must be a named exact nonnegative integer-valued vector",
      call. = FALSE
    )
  }
  bench_assert_integer_scalar(limit, "limit", 0, 2^53)

  backends <- c("embedded", "bpcells", "h5")
  rows <- expand.grid(
    backend = backends,
    tier_label = names(tiers),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  rows <- rows[, c("tier_label", "backend")]
  rows$panel <- panel
  rows$n_cells <- as.integer(unname(tiers[rows$tier_label]))
  rows$exact_nnz <- unname(exact_nnz[rows$tier_label])
  rows$status <- "SCHEDULED"
  rows$reason <- NA_character_

  if (panel == "comparison") {
    unsupported <- rows$exact_nnz > limit
    rows$status[unsupported] <- "UNSUPPORTED_DGCMATRIX_INDEX"
    rows$reason[
      unsupported
    ] <- "shared dgCMatrix materialization exceeds sparse index limit"
  } else {
    non_bpcells <- rows$backend != "bpcells"
    unsupported <- non_bpcells &
      rows$tier_label != "common" &
      rows$exact_nnz > limit
    not_applicable <- non_bpcells & !unsupported
    rows$status[unsupported] <- "UNSUPPORTED_DGCMATRIX_INDEX"
    rows$reason[unsupported] <- "dgCMatrix backend exceeds sparse index limit"
    rows$status[not_applicable] <- "NOT_APPLICABLE_PROTOCOL"
    rows$reason[not_applicable] <- "backend is outside the full-scale protocol"
  }

  rows <- rows[, c(
    "panel",
    "tier_label",
    "n_cells",
    "backend",
    "exact_nnz",
    "status",
    "reason"
  )]
  rownames(rows) <- NULL
  rows
}

.bench_stage_vocabulary <- c(
  "startup",
  "source_open",
  "source_subset",
  "comparison_materialize",
  "seurat_shell",
  "export",
  "artifact_sizes",
  "crb_load",
  "backend_attach",
  "first_query",
  "warmed_queries",
  "block_prepare",
  "block_materialize",
  "correctness",
  "complete"
)
.bench_export_stages <- c(
  "startup",
  "source_open",
  "source_subset",
  "comparison_materialize",
  "seurat_shell",
  "export",
  "artifact_sizes",
  "complete"
)
.bench_access_stages <- c(
  "startup",
  "crb_load",
  "backend_attach",
  "first_query",
  "warmed_queries",
  "block_prepare",
  "block_materialize",
  "correctness",
  "complete"
)

.bench_scalar_string <- function(value, name, allow_empty = FALSE) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      (!allow_empty && !nzchar(value))
  ) {
    stop(name, " must be one non-missing string", call. = FALSE)
  }
  value
}

.bench_job_id <- function(job_id) {
  job_id <- .bench_scalar_string(job_id, "job_id")
  if (
    !identical(basename(job_id), job_id) ||
      job_id %in% c(".", "..") ||
      grepl("[/\\\\]", job_id)
  ) {
    stop("job_id must be a safe basename", call. = FALSE)
  }
  job_id
}

bench_write_stage <- function(job_dir, stage) {
  job_dir <- normalizePath(
    .bench_scalar_string(job_dir, "job_dir"),
    mustWork = TRUE
  )
  stage <- .bench_scalar_string(stage, "stage")
  if (!stage %in% .bench_stage_vocabulary) {
    stop("stage is outside the protocol", call. = FALSE)
  }
  marker <- file.path(job_dir, ".cerebro-benchmark-job")
  if (!file.exists(marker)) {
    stop("job marker is missing", call. = FALSE)
  }
  temporary <- tempfile(".stage-", tmpdir = job_dir)
  on.exit(unlink(temporary), add = TRUE)
  writeLines(stage, temporary, useBytes = TRUE)
  destination <- file.path(job_dir, "stage")
  if (!file.rename(temporary, destination)) {
    stop("could not atomically write stage", call. = FALSE)
  }
  invisible(stage)
}

bench_read_stage <- function(job_dir) {
  path <- file.path(.bench_scalar_string(job_dir, "job_dir"), "stage")
  value <- tryCatch(
    readLines(path, n = 2L, warn = FALSE),
    error = function(error) character()
  )
  if (length(value) != 1L || !value %in% .bench_stage_vocabulary) {
    "startup"
  } else {
    value
  }
}

bench_make_job_dir <- function(scratch_root, job_id) {
  scratch_root <- normalizePath(
    .bench_scalar_string(scratch_root, "scratch_root"),
    mustWork = TRUE
  )
  if (!file.exists(file.path(scratch_root, ".cerebro-benchmark-scratch"))) {
    stop("scratch marker is missing", call. = FALSE)
  }
  job_id <- .bench_job_id(job_id)
  job_dir <- file.path(scratch_root, job_id)
  if (file.exists(job_dir) || dir.exists(job_dir)) {
    stop("job directory already exists", call. = FALSE)
  }
  if (!dir.create(job_dir, mode = "0700")) {
    stop("could not create job directory", call. = FALSE)
  }
  marker <- file.path(job_dir, ".cerebro-benchmark-job")
  writeLines(job_id, marker, useBytes = TRUE)
  bench_write_stage(job_dir, "startup")
  job_dir
}

bench_remove_job_dir <- function(job_dir, scratch_root) {
  job_dir_input <- .bench_scalar_string(job_dir, "job_dir")
  scratch_root <- normalizePath(
    .bench_scalar_string(scratch_root, "scratch_root"),
    mustWork = TRUE
  )
  if (!file.exists(file.path(scratch_root, ".cerebro-benchmark-scratch"))) {
    stop("scratch marker is missing", call. = FALSE)
  }
  if (nzchar(Sys.readlink(job_dir_input))) {
    stop("job directory must not be a symlink", call. = FALSE)
  }
  job_dir <- normalizePath(job_dir_input, mustWork = TRUE)
  if (
    !identical(normalizePath(dirname(job_dir), mustWork = TRUE), scratch_root)
  ) {
    stop("job directory is not an exact child of scratch root", call. = FALSE)
  }
  marker <- file.path(job_dir, ".cerebro-benchmark-job")
  if (!file.exists(marker) || nzchar(Sys.readlink(marker))) {
    stop("job marker is missing or unsafe", call. = FALSE)
  }
  marker_id <- readLines(marker, n = 2L, warn = FALSE)
  expected_id <- basename(job_dir)
  if (
    length(marker_id) != 1L ||
      !identical(marker_id, expected_id) ||
      !identical(.bench_job_id(marker_id), expected_id)
  ) {
    stop("job marker job_id does not match directory", call. = FALSE)
  }
  unlink(job_dir, recursive = TRUE, force = FALSE)
  if (file.exists(job_dir) || dir.exists(job_dir)) {
    stop("job cleanup failed", call. = FALSE)
  }
  invisible(TRUE)
}

.bench_r_heap_peak_from_gc <- function(result) {
  if (
    !is.matrix(result) ||
      nrow(result) != 2L ||
      ncol(result) < 2L ||
      !identical(colnames(result)[[ncol(result) - 1L]], "max used")
  ) {
    stop(
      "gc result does not expose the expected max used column",
      call. = FALSE
    )
  }
  values <- as.double(result[, ncol(result)])
  if (length(values) != 2L || any(!is.finite(values))) {
    stop("gc peak MiB values must be finite", call. = FALSE)
  }
  as.double(sum(values))
}

bench_r_heap_peak_mb <- function() .bench_r_heap_peak_from_gc(gc())

bench_safe_r_heap_peak_mb <- function() {
  tryCatch(as.double(bench_r_heap_peak_mb()), error = function(error) NA_real_)
}

bench_rss_mb <- function(pid = Sys.getpid()) {
  if (
    !is.numeric(pid) ||
      length(pid) != 1L ||
      !is.finite(pid) ||
      pid != floor(pid) ||
      pid < 1 ||
      pid > .Machine$integer.max
  ) {
    return(NA_real_)
  }
  tryCatch(
    {
      output <- suppressWarnings(system2(
        "ps",
        c("-o", "rss=", "-p", as.character(as.integer(pid))),
        stdout = TRUE,
        stderr = FALSE
      ))
      command_status <- attr(output, "status", exact = TRUE)
      if (
        (!is.null(command_status) &&
          !identical(as.integer(command_status), 0L)) ||
          length(output) != 1L ||
          !grepl("^[[:space:]]*[0-9]+[[:space:]]*$", output)
      ) {
        return(NA_real_)
      }
      value <- as.double(trimws(output)) / 1024
      if (length(value) != 1L || !is.finite(value) || value < 0) {
        NA_real_
      } else {
        value
      }
    },
    error = function(error) NA_real_
  )
}

.bench_update_peak_rss <- function(peak, sample) {
  if (
    !is.numeric(sample) ||
      length(sample) != 1L ||
      !is.finite(sample) ||
      sample < 0
  ) {
    return(as.double(peak))
  }
  if (is.numeric(peak) && length(peak) == 1L && is.finite(peak) && peak >= 0) {
    return(as.double(max(peak, sample)))
  }
  as.double(sample)
}

bench_export_schema <- function() {
  c(
    "pair_id",
    "status",
    "failure_stage",
    "error",
    "exit_status",
    "log_path",
    "package_path",
    "peak_rss_mb",
    "r_heap_peak_mb",
    "elapsed_secs",
    "artifact_path",
    "source_open_secs",
    "comparison_materialize_secs",
    "seurat_shell_secs",
    "export_secs",
    "crb_bytes",
    "sidecar_path",
    "sidecar_bytes",
    "total_bytes",
    "sidecar_bytes_applicable",
    "sidecar_bytes_reason",
    "shell_sha256"
  )
}

bench_access_schema <- function() {
  c(
    "pair_id",
    "status",
    "failure_stage",
    "error",
    "exit_status",
    "log_path",
    "package_path",
    "peak_rss_mb",
    "r_heap_peak_mb",
    "elapsed_secs",
    "artifact_path",
    "crb_load_secs",
    "backend_attach_secs",
    "backend_attach_applicable",
    "backend_attach_reason",
    "first_query_secs",
    "warmed_query_1_secs",
    "warmed_query_2_secs",
    "warmed_query_3_secs",
    "warmed_query_4_secs",
    "warmed_query_5_secs",
    "warmed_median_secs",
    "block_prepare_secs",
    "block_materialize_secs",
    "block_ready_secs",
    "expected_row_fingerprint",
    "observed_row_fingerprint",
    "expected_block_fingerprint",
    "observed_block_fingerprint",
    "correctness"
  )
}

bench_empty_outcome <- function(phase, pair_id) {
  phase <- match.arg(phase, c("export", "access"))
  pair_id <- .bench_scalar_string(pair_id, "pair_id")
  row <- if (phase == "export") {
    data.frame(
      pair_id = pair_id,
      status = NA_character_,
      failure_stage = NA_character_,
      error = NA_character_,
      exit_status = NA_integer_,
      log_path = NA_character_,
      package_path = NA_character_,
      peak_rss_mb = NA_real_,
      r_heap_peak_mb = NA_real_,
      elapsed_secs = NA_real_,
      artifact_path = NA_character_,
      source_open_secs = NA_real_,
      comparison_materialize_secs = NA_real_,
      seurat_shell_secs = NA_real_,
      export_secs = NA_real_,
      crb_bytes = NA_real_,
      sidecar_path = NA_character_,
      sidecar_bytes = NA_real_,
      total_bytes = NA_real_,
      sidecar_bytes_applicable = NA,
      sidecar_bytes_reason = NA_character_,
      shell_sha256 = NA_character_,
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      pair_id = pair_id,
      status = NA_character_,
      failure_stage = NA_character_,
      error = NA_character_,
      exit_status = NA_integer_,
      log_path = NA_character_,
      package_path = NA_character_,
      peak_rss_mb = NA_real_,
      r_heap_peak_mb = NA_real_,
      elapsed_secs = NA_real_,
      artifact_path = NA_character_,
      crb_load_secs = NA_real_,
      backend_attach_secs = NA_real_,
      backend_attach_applicable = NA,
      backend_attach_reason = NA_character_,
      first_query_secs = NA_real_,
      warmed_query_1_secs = NA_real_,
      warmed_query_2_secs = NA_real_,
      warmed_query_3_secs = NA_real_,
      warmed_query_4_secs = NA_real_,
      warmed_query_5_secs = NA_real_,
      warmed_median_secs = NA_real_,
      block_prepare_secs = NA_real_,
      block_materialize_secs = NA_real_,
      block_ready_secs = NA_real_,
      expected_row_fingerprint = NA_character_,
      observed_row_fingerprint = NA_character_,
      expected_block_fingerprint = NA_character_,
      observed_block_fingerprint = NA_character_,
      correctness = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  stopifnot(identical(
    names(row),
    if (phase == "export") bench_export_schema() else bench_access_schema()
  ))
  row
}

.bench_require_nonnegative_metrics <- function(row, columns) {
  invalid <- columns[
    !vapply(
      columns,
      function(name) {
        value <- row[[name]]
        is.numeric(value) &&
          length(value) == 1L &&
          !is.na(value) &&
          is.finite(value) &&
          value >= 0
      },
      logical(1L)
    )
  ]
  if (length(invalid)) {
    stop(
      "OK outcome has missing, non-finite, or negative metrics: ",
      paste(invalid, collapse = ","),
      call. = FALSE
    )
  }
}

.bench_require_optional_nonnegative_metric <- function(row, column) {
  value <- row[[column]]
  missing <- is.numeric(value) &&
    length(value) == 1L &&
    is.na(value) &&
    !is.nan(value)
  observed <- is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.finite(value) &&
    value >= 0
  if (!missing && !observed) {
    stop(
      "OK outcome ",
      column,
      " must be NA or finite and nonnegative",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.bench_nonempty_absolute_path <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(value) &&
    startsWith(value, .Platform$file.sep) &&
    !grepl("[\r\n]", value)
}

.bench_validate_ok_outcome <- function(row, phase) {
  parts <- strsplit(row$pair_id, ":", fixed = TRUE)[[1L]]
  if (
    length(parts) != 4L ||
      !parts[[1L]] %in% c("comparison", "full_scale") ||
      !parts[[4L]] %in% c("embedded", "bpcells", "h5")
  ) {
    stop("OK outcome pair_id does not encode panel and backend", call. = FALSE)
  }
  panel <- parts[[1L]]
  backend <- parts[[4L]]
  if (!identical(row$exit_status, 0L)) {
    stop("OK outcome exit_status must be zero", call. = FALSE)
  }
  for (name in c("log_path", "package_path", "artifact_path")) {
    if (!.bench_nonempty_absolute_path(row[[name]])) {
      stop("OK outcome ", name, " must be an absolute path", call. = FALSE)
    }
  }
  if (
    !grepl("[.]log$", row$log_path) ||
      basename(row$package_path) != "CerebroNexus" ||
      !grepl("[.]crb$", row$artifact_path)
  ) {
    stop("OK outcome path structure is invalid", call. = FALSE)
  }
  .bench_require_optional_nonnegative_metric(row, "peak_rss_mb")
  .bench_require_nonnegative_metrics(row, c("r_heap_peak_mb", "elapsed_secs"))
  if (phase == "export") {
    .bench_require_nonnegative_metrics(
      row,
      c(
        "source_open_secs",
        "seurat_shell_secs",
        "export_secs",
        "crb_bytes",
        "total_bytes"
      )
    )
    if (panel == "comparison") {
      .bench_require_nonnegative_metrics(row, "comparison_materialize_secs")
    } else if (!is.na(row$comparison_materialize_secs)) {
      stop(
        "full-scale export must not report comparison materialization",
        call. = FALSE
      )
    }
    if (
      !is.character(row$shell_sha256) ||
        is.na(row$shell_sha256) ||
        !grepl(.bench_sha256_pattern, row$shell_sha256)
    ) {
      stop("OK export shell fingerprint is invalid", call. = FALSE)
    }
    if (backend == "embedded") {
      if (
        !identical(row$sidecar_bytes_applicable, FALSE) ||
          !is.na(row$sidecar_path) ||
          !is.na(row$sidecar_bytes) ||
          !identical(row$sidecar_bytes_reason, "embedded_has_no_sidecar") ||
          !isTRUE(all.equal(row$total_bytes, row$crb_bytes, tolerance = 0))
      ) {
        stop("embedded sidecar applicability is contradictory", call. = FALSE)
      }
    } else {
      .bench_require_nonnegative_metrics(row, "sidecar_bytes")
      if (
        !identical(row$sidecar_bytes_applicable, TRUE) ||
          !.bench_nonempty_absolute_path(row$sidecar_path) ||
          !is.na(row$sidecar_bytes_reason) ||
          (backend == "bpcells" && !grepl("[.]bpcells$", row$sidecar_path)) ||
          (backend == "h5" && !grepl("[.]h5$", row$sidecar_path)) ||
          !isTRUE(all.equal(
            row$total_bytes,
            row$crb_bytes + row$sidecar_bytes,
            tolerance = 0
          ))
      ) {
        stop("external sidecar applicability is contradictory", call. = FALSE)
      }
    }
  } else {
    timing <- c(
      "crb_load_secs",
      "first_query_secs",
      paste0("warmed_query_", 1:5, "_secs"),
      "warmed_median_secs",
      "block_prepare_secs",
      "block_materialize_secs",
      "block_ready_secs"
    )
    .bench_require_nonnegative_metrics(row, timing)
    warmed <- as.double(unlist(
      row[paste0("warmed_query_", 1:5, "_secs")],
      use.names = FALSE
    ))
    if (
      !isTRUE(all.equal(
        row$warmed_median_secs,
        stats::median(warmed),
        tolerance = 1e-9
      )) ||
        !isTRUE(all.equal(
          row$block_ready_secs,
          row$block_prepare_secs + row$block_materialize_secs,
          tolerance = 1e-9
        ))
    ) {
      stop("access derived timings are inconsistent", call. = FALSE)
    }
    hashes <- c(
      "expected_row_fingerprint",
      "observed_row_fingerprint",
      "expected_block_fingerprint",
      "observed_block_fingerprint"
    )
    if (
      any(
        !vapply(
          row[hashes],
          function(x) {
            is.character(x) &&
              length(x) == 1L &&
              !is.na(x) &&
              grepl(.bench_sha256_pattern, x)
          },
          logical(1L)
        )
      ) ||
        !identical(
          row$expected_row_fingerprint,
          row$observed_row_fingerprint
        ) ||
        !identical(
          row$expected_block_fingerprint,
          row$observed_block_fingerprint
        ) ||
        !identical(row$correctness, "PASS")
    ) {
      stop("OK access correctness fingerprints are invalid", call. = FALSE)
    }
    if (backend == "embedded") {
      if (
        !identical(row$backend_attach_applicable, FALSE) ||
          !is.na(row$backend_attach_secs) ||
          !identical(
            row$backend_attach_reason,
            "embedded_requires_no_external_attach"
          )
      ) {
        stop("embedded attach applicability is contradictory", call. = FALSE)
      }
    } else {
      .bench_require_nonnegative_metrics(row, "backend_attach_secs")
      if (
        !identical(row$backend_attach_applicable, TRUE) ||
          !is.na(row$backend_attach_reason)
      ) {
        stop("external attach applicability is contradictory", call. = FALSE)
      }
    }
  }
  invisible(TRUE)
}

.bench_validate_outcome_row <- function(row, expected_schema, pair_id = NULL) {
  if (!is.data.frame(row) || nrow(row) != 1L) {
    stop("outcome must contain exactly one row", call. = FALSE)
  }
  if (!identical(names(row), expected_schema)) {
    stop("outcome schema is invalid", call. = FALSE)
  }
  if (!is.null(pair_id) && !identical(row$pair_id, pair_id)) {
    stop("outcome pair_id is invalid", call. = FALSE)
  }
  template <- bench_empty_outcome(
    if (identical(expected_schema, bench_export_schema())) {
      "export"
    } else {
      "access"
    },
    row$pair_id[[1L]]
  )
  compatible <- vapply(
    expected_schema,
    function(name) identical(typeof(row[[name]]), typeof(template[[name]])),
    logical(1L)
  )
  if (!all(compatible)) {
    stop("outcome column types are invalid", call. = FALSE)
  }
  if (
    !is.character(row$pair_id) || is.na(row$pair_id) || !nzchar(row$pair_id)
  ) {
    stop("outcome pair_id is invalid", call. = FALSE)
  }
  phase <- if (identical(expected_schema, bench_export_schema())) {
    "export"
  } else {
    "access"
  }
  allowed_exact <- if (phase == "access") {
    c("OK", "NOT_RUN_EXPORT_FAILED")
  } else {
    "OK"
  }
  failure_stages <- if (phase == "export") {
    .bench_export_stages
  } else {
    .bench_access_stages
  }
  failed <- grepl(
    paste0("^FAILED_(", paste(failure_stages, collapse = "|"), ")$"),
    row$status
  )
  if (
    is.na(row$status) || (!row$status %in% allowed_exact && !isTRUE(failed))
  ) {
    stop("outcome status is outside the raw protocol vocabulary", call. = FALSE)
  }
  if (isTRUE(failed)) {
    expected_stage <- sub("^FAILED_", "", row$status)
    if (
      is.na(row$failure_stage) || !identical(row$failure_stage, expected_stage)
    ) {
      stop("outcome failure_stage does not match status", call. = FALSE)
    }
    if (is.na(row$error) || !nzchar(row$error)) {
      stop("failed outcome must contain a nonempty error", call. = FALSE)
    }
  } else if (!is.na(row$failure_stage)) {
    stop("non-failed outcome must not contain failure_stage", call. = FALSE)
  }
  if (identical(row$status, "OK") && !is.na(row$error)) {
    stop("OK outcome must not contain an error", call. = FALSE)
  }
  if (identical(row$status, "NOT_RUN_EXPORT_FAILED")) {
    if (is.na(row$error) || !nzchar(row$error)) {
      stop(
        "NOT_RUN_EXPORT_FAILED must contain a nonempty reason",
        call. = FALSE
      )
    }
    prohibited <- c(
      "crb_load_secs",
      "backend_attach_secs",
      "first_query_secs",
      paste0("warmed_query_", 1:5, "_secs"),
      "warmed_median_secs",
      "block_prepare_secs",
      "block_materialize_secs",
      "block_ready_secs",
      "observed_row_fingerprint",
      "observed_block_fingerprint",
      "correctness"
    )
    if (
      any(
        !vapply(
          row[prohibited],
          function(value) length(value) == 1L && is.na(value),
          logical(1L)
        )
      )
    ) {
      stop(
        "NOT_RUN_EXPORT_FAILED must not contain access measurements",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

bench_write_outcome_atomic <- function(path, row, expected_schema) {
  path <- .bench_scalar_string(path, "path")
  .bench_validate_outcome_row(row, expected_schema)
  parent <- dirname(path)
  if (!dir.exists(parent)) {
    stop("outcome parent directory is missing", call. = FALSE)
  }
  temporary <- tempfile(".outcome-", tmpdir = parent, fileext = ".rds")
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(row, temporary, version = 3)
  if (!file.rename(temporary, path)) {
    stop("could not atomically replace outcome", call. = FALSE)
  }
  invisible(path)
}

.bench_descendant <- function(path, parent) {
  path <- normalizePath(path, mustWork = TRUE)
  parent <- normalizePath(parent, mustWork = TRUE)
  path_parts <- strsplit(path, .Platform$file.sep, fixed = TRUE)[[1L]]
  parent_parts <- strsplit(parent, .Platform$file.sep, fixed = TRUE)[[1L]]
  length(path_parts) > length(parent_parts) &&
    identical(path_parts[seq_along(parent_parts)], parent_parts)
}

.bench_validate_package_origin <- function(
  run_context,
  package_path = find.package("CerebroNexus")
) {
  library <- normalizePath(
    .bench_scalar_string(run_context$library, "run_context$library"),
    mustWork = TRUE
  )
  marker <- file.path(library, ".cerebro-benchmark-library")
  if (!file.exists(marker)) {
    stop("run-local library marker is missing", call. = FALSE)
  }
  package_path <- normalizePath(package_path, mustWork = TRUE)
  if (!.bench_descendant(package_path, library)) {
    stop(
      "CerebroNexus package origin is outside the marked run-local library",
      call. = FALSE
    )
  }
  package_path
}

.bench_phase <- function(job) {
  phase <- job$phase
  if (is.null(phase)) {
    phase <- if (identical(job$panel, "access")) "access" else "export"
  }
  match.arg(phase, c("export", "access"))
}

bench_failure_row <- function(job, stage, error, r_heap_peak_mb = NA_real_) {
  phase <- .bench_phase(job)
  row <- bench_empty_outcome(phase, job$pair_id)
  stage <- if (length(stage) == 1L && stage %in% .bench_stage_vocabulary) {
    stage
  } else {
    "startup"
  }
  row$status <- paste0("FAILED_", stage)
  row$failure_stage <- stage
  row$error <- as.character(error)[[1L]]
  row$r_heap_peak_mb <- as.double(r_heap_peak_mb)
  if (
    !is.null(job$artifact_path) &&
      is.character(job$artifact_path) &&
      length(job$artifact_path) == 1L &&
      !is.na(job$artifact_path)
  ) {
    row$artifact_path <- job$artifact_path
  }
  if (
    !is.null(job$package_path) &&
      is.character(job$package_path) &&
      length(job$package_path) == 1L &&
      !is.na(job$package_path)
  ) {
    row$package_path <- job$package_path
  }
  row
}

bench_not_run_access_row <- function(
  job,
  reason = "paired export did not complete"
) {
  row <- bench_empty_outcome("access", job$pair_id)
  row$status <- "NOT_RUN_EXPORT_FAILED"
  row$error <- .bench_scalar_string(reason, "reason")
  if (
    !is.null(job$artifact_path) &&
      is.character(job$artifact_path) &&
      length(job$artifact_path) == 1L &&
      !is.na(job$artifact_path)
  ) {
    row$artifact_path <- job$artifact_path
  }
  row
}

bench_finalize_worker_row <- function(row, r_heap_peak_mb) {
  if (
    !is.data.frame(row) || nrow(row) != 1L || !"r_heap_peak_mb" %in% names(row)
  ) {
    stop("worker returned an invalid outcome row", call. = FALSE)
  }
  row$r_heap_peak_mb <- as.double(r_heap_peak_mb)
  row
}

.bench_artifact_sidecar <- function(crb_path, backend) {
  stem <- tools::file_path_sans_ext(crb_path)
  switch(
    backend,
    embedded = NA_character_,
    bpcells = paste0(stem, ".bpcells"),
    h5 = paste0(stem, ".h5"),
    stop("unsupported backend", call. = FALSE)
  )
}

.bench_path_bytes <- function(path) {
  if (is.na(path)) {
    return(NA_real_)
  }
  if (file.exists(path) && !dir.exists(path)) {
    return(as.double(file.info(path)$size))
  }
  if (dir.exists(path)) {
    files <- list.files(
      path,
      recursive = TRUE,
      all.files = TRUE,
      full.names = TRUE,
      include.dirs = FALSE,
      no.. = TRUE
    )
    if (!length(files)) {
      return(0)
    }
    sizes <- as.double(file.info(files)$size)
    if (any(!is.finite(sizes)) || any(sizes < 0)) {
      stop("artifact file sizes are invalid", call. = FALSE)
    }
    return(sum(sizes))
  }
  stop("expected artifact is missing: ", path, call. = FALSE)
}

.bench_job_source <- function(job) {
  path <- .bench_scalar_string(job$source_path, "job$source_path")
  group <- if (is.null(job$source_group)) "X" else job$source_group
  matrix <- bench_open_source(path, group)
  list(matrix = matrix)
}

.bench_job_indices <- function(job, matrix) {
  indices <- bench_stratified_indices(ncol(matrix), job$n_cells)
  if (
    !is.numeric(indices) ||
      anyNA(indices) ||
      any(indices != floor(indices)) ||
      any(indices < 1) ||
      any(indices > ncol(matrix)) ||
      anyDuplicated(indices)
  ) {
    stop("job source indices are invalid", call. = FALSE)
  }
  as.integer(indices)
}

bench_export_worker <- function(job, run_context) {
  started <- proc.time()[["elapsed"]]
  row <- bench_empty_outcome("export", job$pair_id)
  row$package_path <- .bench_validate_package_origin(run_context)
  bench_write_stage(job$job_dir, "source_open")
  opened <- bench_timed_value(.bench_job_source(job))
  source <- opened$value
  row$source_open_secs <- opened$seconds
  bench_write_stage(job$job_dir, "source_subset")
  source_indices <- .bench_job_indices(job, source$matrix)
  expression <- source$matrix[, source_indices, drop = FALSE]
  if (identical(job$panel, "comparison")) {
    bench_write_stage(job$job_dir, "comparison_materialize")
    .bench_require_namespace("Matrix")
    materialized <- bench_timed_value(methods::as(expression, "dgCMatrix"))
    expression <- materialized$value
    row$comparison_materialize_secs <- materialized$seconds
  } else if (!inherits(expression, "IterableMatrix")) {
    stop(
      "full-scale Panel B source subset must remain an IterableMatrix",
      call. = FALSE
    )
  }
  bench_write_stage(job$job_dir, "seurat_shell")
  prepared <- bench_timed_value(bench_make_seurat_shell(
    expression,
    source_indices
  ))
  shell <- prepared$value
  row$seurat_shell_secs <- prepared$seconds
  row$shell_sha256 <- bench_shell_fingerprint(shell, source_indices)
  bench_write_stage(job$job_dir, "export")
  .bench_require_namespace("CerebroNexus")
  artifact <- .bench_scalar_string(job$artifact_path, "job$artifact_path")
  backend <- match.arg(job$backend, c("embedded", "bpcells", "h5"))
  exported <- bench_timed_value(CerebroNexus::exportFromSeurat(
    object = shell,
    assay = "RNA",
    slot = "data",
    file = artifact,
    experiment_name = "CerebroNexus real-data benchmark",
    organism = "hg38",
    groups = c("sample", "cluster"),
    main_group = NULL,
    cell_cycle = NULL,
    nUMI = "nUMI",
    nGene = "nGene",
    add_all_meta_data = FALSE,
    use_delayed_array = FALSE,
    expression_matrix_mode = backend,
    verbose = FALSE
  ))
  row$export_secs <- exported$seconds
  bench_write_stage(job$job_dir, "artifact_sizes")
  sidecar <- .bench_artifact_sidecar(artifact, backend)
  row$artifact_path <- normalizePath(artifact, mustWork = TRUE)
  row$crb_bytes <- .bench_path_bytes(artifact)
  row$sidecar_path <- if (is.na(sidecar)) {
    NA_character_
  } else {
    normalizePath(sidecar, mustWork = TRUE)
  }
  row$sidecar_bytes <- .bench_path_bytes(sidecar)
  row$total_bytes <- row$crb_bytes +
    if (is.na(row$sidecar_bytes)) 0 else row$sidecar_bytes
  row$sidecar_bytes_applicable <- !identical(backend, "embedded")
  row$sidecar_bytes_reason <- if (row$sidecar_bytes_applicable) {
    NA_character_
  } else {
    "embedded_has_no_sidecar"
  }
  row$status <- "OK"
  row$elapsed_secs <- as.double(proc.time()[["elapsed"]] - started)
  bench_write_stage(job$job_dir, "complete")
  row
}

.bench_load_runtime_helpers <- function(package_path) {
  utility <- file.path(package_path, "viewer", "utility_functions.R")
  if (!file.exists(utility)) {
    stop("installed viewer attach helper is missing", call. = FALSE)
  }
  runtime <- new.env(parent = globalenv())
  sys.source(utility, envir = runtime)
  required <- c(".readRuntimeBackendDescriptor", ".attachExternalExpression")
  if (
    !all(vapply(
      required,
      function(name) {
        exists(name, envir = runtime, inherits = FALSE) &&
          is.function(get(name, envir = runtime, inherits = FALSE))
      },
      logical(1L)
    ))
  ) {
    stop("installed viewer runtime helpers are unavailable", call. = FALSE)
  }
  runtime
}

.bench_validate_artifact_backend <- function(
  runtime,
  object,
  artifact_path,
  expected_backend
) {
  expected_backend <- match.arg(
    expected_backend,
    c("embedded", "bpcells", "h5")
  )
  if (
    !is.environment(runtime) ||
      !is.function(runtime$.readRuntimeBackendDescriptor)
  ) {
    stop("runtime backend descriptor helper is unavailable", call. = FALSE)
  }
  descriptor <- runtime$.readRuntimeBackendDescriptor(object, artifact_path)
  if (
    !is.list(descriptor) ||
      !is.character(descriptor$type) ||
      length(descriptor$type) != 1L ||
      is.na(descriptor$type) ||
      !identical(descriptor$type, expected_backend)
  ) {
    observed <- if (
      is.list(descriptor) &&
        is.character(descriptor$type) &&
        length(descriptor$type) == 1L &&
        !is.na(descriptor$type)
    ) {
      descriptor$type
    } else {
      "invalid"
    }
    stop(
      "artifact backend '",
      observed,
      "' does not match scheduled backend '",
      expected_backend,
      "'",
      call. = FALSE
    )
  }
  descriptor
}

.bench_attach_artifact <- function(runtime, object, artifact_path) {
  if (
    !is.environment(runtime) || !is.function(runtime$.attachExternalExpression)
  ) {
    stop("runtime attach helper is unavailable", call. = FALSE)
  }
  runtime$.attachExternalExpression(object, artifact_path)
}

.bench_timed_external_attach <- function(
  runtime,
  object,
  artifact_path,
  timer = bench_timed_value
) {
  timer(.bench_attach_artifact(runtime, object, artifact_path))
}

bench_stage_timer <- function(write_stage, timer = bench_timed_value) {
  if (!is.function(write_stage) || !is.function(timer)) {
    stop("stage timer inputs must be functions", call. = FALSE)
  }
  call_index <- 0L
  stages <- c(
    "first_query",
    rep("warmed_queries", 5L),
    "block_prepare",
    "block_materialize"
  )
  function(expr) {
    call_index <<- call_index + 1L
    if (call_index > length(stages)) {
      stop("query timer exceeded eight protocol calls", call. = FALSE)
    }
    write_stage(stages[[call_index]])
    timer(expr)
  }
}

bench_access_worker <- function(job, run_context) {
  started <- proc.time()[["elapsed"]]
  row <- bench_empty_outcome("access", job$pair_id)
  row$package_path <- .bench_validate_package_origin(run_context)
  runtime <- .bench_load_runtime_helpers(row$package_path)
  artifact <- .bench_scalar_string(job$artifact_path, "job$artifact_path")
  row$artifact_path <- normalizePath(artifact, mustWork = TRUE)
  bench_write_stage(job$job_dir, "crb_load")
  loaded <- bench_timed_value(readRDS(artifact))
  object <- loaded$value
  row$crb_load_secs <- loaded$seconds
  bench_write_stage(job$job_dir, "backend_attach")
  backend <- match.arg(job$backend, c("embedded", "bpcells", "h5"))
  .bench_validate_artifact_backend(runtime, object, artifact, backend)
  row$backend_attach_applicable <- !identical(backend, "embedded")
  if (row$backend_attach_applicable) {
    attached <- .bench_timed_external_attach(runtime, object, artifact)
    object <- attached$value
    row$backend_attach_secs <- attached$seconds
    row$backend_attach_reason <- NA_character_
  } else {
    object <- .bench_attach_artifact(runtime, object, artifact)
    row$backend_attach_secs <- NA_real_
    row$backend_attach_reason <- "embedded_requires_no_external_attach"
  }
  timer <- bench_stage_timer(function(stage) {
    bench_write_stage(job$job_dir, stage)
  })
  measurement <- bench_measure_queries(object, job$query_plan, timer = timer)
  bench_write_stage(job$job_dir, "correctness")
  bench_validate_query_measurement(measurement, job$query_plan)
  row$first_query_secs <- measurement$first_query_secs
  row[paste0("warmed_query_", 1:5, "_secs")] <- as.list(as.double(
    measurement$warmed_secs
  ))
  row$warmed_median_secs <- measurement$warmed_median_secs
  row$block_prepare_secs <- measurement$block_prepare_secs
  row$block_materialize_secs <- measurement$block_materialize_secs
  row$block_ready_secs <- measurement$block_ready_secs
  row$expected_row_fingerprint <- job$query_plan$first_row_numeric_sha256
  row$observed_row_fingerprint <- measurement$row_fingerprint
  row$expected_block_fingerprint <- job$query_plan$block_numeric_sha256
  row$observed_block_fingerprint <- measurement$block_fingerprint
  row$correctness <- "PASS"
  row$status <- "OK"
  row$elapsed_secs <- as.double(proc.time()[["elapsed"]] - started)
  bench_write_stage(job$job_dir, "complete")
  row
}

bench_worker_entry <- function(worker_name, job, run_context) {
  gc(reset = TRUE)
  if (is.null(job$phase)) {
    job$phase <- if (identical(worker_name, "bench_access_worker")) {
      "access"
    } else {
      "export"
    }
  }
  tryCatch(
    {
      if (worker_name %in% c("bench_export_worker", "bench_access_worker")) {
        .bench_validate_package_origin(run_context)
      }
      worker <- get(
        .bench_scalar_string(worker_name, "worker_name"),
        mode = "function",
        inherits = TRUE
      )
      row <- worker(job, run_context)
      bench_finalize_worker_row(
        row,
        r_heap_peak_mb = bench_safe_r_heap_peak_mb()
      )
    },
    error = function(error) {
      row <- bench_failure_row(
        job = job,
        stage = bench_read_stage(job$job_dir),
        error = conditionMessage(error),
        r_heap_peak_mb = bench_safe_r_heap_peak_mb()
      )
      row$package_path <- tryCatch(
        .bench_validate_package_origin(run_context),
        error = function(origin_error) NA_character_
      )
      row
    }
  )
}

.bench_apply_parent_metrics <- function(
  row,
  exit_status,
  peak_rss_mb,
  log_path
) {
  row$exit_status <- as.integer(exit_status)
  row$peak_rss_mb <- as.double(peak_rss_mb)
  row$log_path <- as.character(log_path)
  row
}

bench_classify_worker_result <- function(
  job,
  result,
  exit_status,
  last_stage,
  peak_rss_mb,
  log_path
) {
  phase <- .bench_phase(job)
  schema <- if (phase == "export") {
    bench_export_schema()
  } else {
    bench_access_schema()
  }
  status <- if (
    is.numeric(exit_status) &&
      length(exit_status) == 1L &&
      is.finite(exit_status)
  ) {
    as.integer(exit_status)
  } else {
    NA_integer_
  }
  if (!identical(status, 0L)) {
    row <- bench_failure_row(
      job,
      last_stage,
      if (is.list(result) && !isTRUE(result$ok)) {
        result$error
      } else {
        "worker process exited without a result"
      },
      NA_real_
    )
    row <- .bench_apply_parent_metrics(row, status, peak_rss_mb, log_path)
    return(list(row = row, diagnostic = NA_character_))
  }
  if (!is.list(result) || !isTRUE(result$ok)) {
    return(list(
      row = NULL,
      diagnostic = paste0("collector could not recover result: ", result$error),
      exit_status = status,
      log_path = log_path,
      peak_rss_mb = peak_rss_mb
    ))
  }
  value <- result$value
  diagnostic <- tryCatch(
    {
      .bench_validate_outcome_row(value, schema, job$pair_id)
      NULL
    },
    error = function(error) conditionMessage(error)
  )
  if (!is.null(diagnostic)) {
    return(list(
      row = NULL,
      diagnostic = diagnostic,
      exit_status = status,
      log_path = log_path,
      peak_rss_mb = peak_rss_mb
    ))
  }
  value <- .bench_apply_parent_metrics(value, status, peak_rss_mb, log_path)
  list(row = value, diagnostic = NA_character_)
}

.bench_validate_worker_payload <- function(value, name, depth = 0L) {
  if (depth > 12L) {
    stop(name, " is nested too deeply", call. = FALSE)
  }
  if (
    is.environment(value) ||
      is.function(value) ||
      isS4(value) ||
      is.matrix(value) ||
      inherits(value, c("IterableMatrix", "Seurat"))
  ) {
    stop(
      name,
      " may contain only paths, scalars, and small data-frame/list manifests",
      call. = FALSE
    )
  }
  if (is.data.frame(value)) {
    if (nrow(value) > 10000L || ncol(value) > 100L) {
      stop(name, " data frame is too large", call. = FALSE)
    }
    invisible(lapply(
      value,
      .bench_validate_worker_payload,
      name = name,
      depth = depth + 1L
    ))
  } else if (is.list(value)) {
    if (length(value) > 1000L) {
      stop(name, " list is too large", call. = FALSE)
    }
    invisible(lapply(
      value,
      .bench_validate_worker_payload,
      name = name,
      depth = depth + 1L
    ))
  } else if (is.atomic(value)) {
    if (length(value) > 10000L) {
      stop(name, " atomic vector is too large", call. = FALSE)
    }
  } else if (!is.null(value)) {
    stop(name, " contains an unsupported object", call. = FALSE)
  }
  invisible(TRUE)
}

bench_run_worker <- function(worker, job, run_context, log_path, poll_ms) {
  .bench_require_namespace("callr")
  bench_assert_integer_scalar(poll_ms, "poll_ms", 1, .Machine$integer.max)
  if (is.null(job$phase)) {
    job$phase <- if (identical(worker, "bench_access_worker")) {
      "access"
    } else {
      "export"
    }
  }
  if (worker %in% c("bench_export_worker", "bench_access_worker")) {
    expected_package <- file.path(run_context$library, "CerebroNexus")
    if (dir.exists(expected_package)) {
      job$package_path <- .bench_validate_package_origin(
        run_context,
        expected_package
      )
    }
  }
  .bench_validate_worker_payload(job, "job")
  .bench_validate_worker_payload(run_context, "run_context")
  process <- callr::r_bg(
    func = function(root, worker_name, job, context) {
      assert_frozen <- function() {
        snapshot <- context$harness_snapshot
        if (is.null(snapshot)) {
          return(invisible(TRUE))
        }
        if (
          !requireNamespace("digest", quietly = TRUE) ||
            !is.list(snapshot) ||
            is.null(snapshot$root) ||
            is.null(snapshot$hashes) ||
            !identical(names(snapshot$hashes), c("config.R", "helpers.R"))
        ) {
          stop("frozen harness bootstrap contract is invalid", call. = FALSE)
        }
        root_normalized <- normalizePath(root, mustWork = TRUE)
        if (
          !identical(
            root_normalized,
            normalizePath(snapshot$root, mustWork = TRUE)
          ) ||
            nzchar(Sys.readlink(root_normalized))
        ) {
          stop("frozen harness bootstrap root changed", call. = FALSE)
        }
        paths <- setNames(
          file.path(root_normalized, c("config.R", "helpers.R")),
          c("config.R", "helpers.R")
        )
        info <- file.info(paths)
        unsafe <- any(!file.exists(paths)) ||
          any(is.na(info$isdir)) ||
          any(info$isdir) ||
          any(vapply(
            paths,
            function(path) nzchar(Sys.readlink(path)),
            logical(1L)
          ))
        if (unsafe) {
          stop("frozen harness bootstrap files are unsafe", call. = FALSE)
        }
        observed <- vapply(
          paths,
          function(path) {
            digest::digest(
              path,
              algo = "sha256",
              file = TRUE,
              serialize = FALSE
            )
          },
          character(1L)
        )
        if (!identical(observed, snapshot$hashes)) {
          stop("frozen harness changed across subprocess source", call. = FALSE)
        }
        invisible(TRUE)
      }
      if (!is.null(context$harness_snapshot)) {
        assert_frozen()
        source(file.path(root, "config.R"), local = TRUE)
        source(file.path(root, "helpers.R"), local = TRUE)
        assert_frozen()
      } else {
        source(file.path(root, "helpers.R"), local = TRUE)
      }
      value <- bench_worker_entry(worker_name, job, context)
      assert_frozen()
      value
    },
    args = list(run_context$bench_root, worker, job, run_context),
    libpath = c(run_context$library, .libPaths()),
    system_profile = FALSE,
    user_profile = FALSE,
    supervise = TRUE,
    stdout = log_path,
    stderr = "2>&1"
  )
  peak_rss <- NA_real_
  sample <- bench_rss_mb(process$get_pid())
  peak_rss <- .bench_update_peak_rss(peak_rss, sample)
  while (process$is_alive()) {
    Sys.sleep(poll_ms / 1000)
    sample <- bench_rss_mb(process$get_pid())
    peak_rss <- .bench_update_peak_rss(peak_rss, sample)
  }
  process$wait()
  exit_status <- process$get_exit_status()
  result <- tryCatch(
    list(ok = TRUE, value = process$get_result()),
    error = function(error) list(ok = FALSE, error = conditionMessage(error))
  )
  classified <- bench_classify_worker_result(
    job,
    result,
    exit_status,
    bench_read_stage(job$job_dir),
    peak_rss,
    log_path
  )
  if (!is.null(classified$row) && !is.null(job$outcome_path)) {
    schema <- if (.bench_phase(job) == "export") {
      bench_export_schema()
    } else {
      bench_access_schema()
    }
    bench_write_outcome_atomic(job$outcome_path, classified$row, schema)
  }
  classified
}

bench_validation_schema <- function() {
  c("check_id", "panel", "scope", "expected", "observed", "status", "detail")
}

bench_empty_validation <- function() {
  data.frame(
    check_id = character(),
    panel = character(),
    scope = character(),
    expected = character(),
    observed = character(),
    status = character(),
    detail = character(),
    stringsAsFactors = FALSE
  )
}

bench_validation_row <- function(
  check_id,
  panel,
  scope,
  expected = NA_character_,
  observed = NA_character_,
  status,
  detail = NA_character_
) {
  values <- list(
    check_id = check_id,
    panel = panel,
    scope = scope,
    expected = expected,
    observed = observed,
    status = status,
    detail = detail
  )
  scalar_character <- vapply(
    values,
    function(value) {
      is.character(value) && length(value) == 1L
    },
    logical(1L)
  )
  if (
    !all(scalar_character) ||
      is.na(check_id) ||
      !nzchar(check_id) ||
      is.na(panel) ||
      !panel %in% c("comparison", "full_scale") ||
      is.na(scope) ||
      !nzchar(scope) ||
      is.na(status) ||
      !nzchar(status)
  ) {
    stop("validation row fields are invalid", call. = FALSE)
  }
  row <- as.data.frame(values, stringsAsFactors = FALSE, optional = TRUE)
  row <- row[bench_validation_schema()]
  stopifnot(all(vapply(row, typeof, character(1L)) == "character"))
  row
}

.bench_gate_row <- function(rows, ...) {
  rows[[length(rows) + 1L]] <- bench_validation_row(...)
  rows
}

.bench_audit_outcome_table <- function(raw, phase) {
  schema <- if (phase == "export") {
    bench_export_schema()
  } else {
    bench_access_schema()
  }
  prototype <- bench_empty_outcome(phase, ".prototype")[FALSE, ]
  errors <- character()
  if (!is.data.frame(raw) || !identical(names(raw), schema)) {
    errors <- c(errors, "raw outcome schema is invalid")
    return(list(errors = errors, table = prototype))
  }
  type_ok <- vapply(
    schema,
    function(name) {
      identical(typeof(raw[[name]]), typeof(prototype[[name]]))
    },
    logical(1L)
  )
  if (!all(type_ok)) {
    errors <- c(
      errors,
      paste0("invalid column types: ", paste(schema[!type_ok], collapse = ","))
    )
  }
  if (nrow(raw)) {
    for (i in seq_len(nrow(raw))) {
      error <- tryCatch(
        {
          .bench_validate_outcome_row(raw[i, , drop = FALSE], schema)
          NULL
        },
        error = function(error) conditionMessage(error)
      )
      if (!is.null(error)) errors <- c(errors, paste0("row ", i, ": ", error))
    }
  }
  usable <- is.character(raw$pair_id) && is.character(raw$status)
  list(errors = unique(errors), table = if (usable) raw else prototype)
}

bench_raw_outcome_gate <- function(
  schedule,
  export_rows,
  access_rows,
  expected_fingerprints,
  panel
) {
  panel <- .bench_scalar_string(panel, "panel")
  if (!panel %in% c("comparison", "full_scale")) {
    stop("panel is invalid", call. = FALSE)
  }
  if (
    !is.data.frame(schedule) ||
      !all(c("pair_id", "panel") %in% names(schedule)) ||
      !is.character(schedule$pair_id) ||
      anyNA(schedule$pair_id) ||
      any(!nzchar(schedule$pair_id)) ||
      anyDuplicated(schedule$pair_id) ||
      !is.character(schedule$panel) ||
      any(schedule$panel != panel)
  ) {
    stop("schedule expected pair IDs are invalid", call. = FALSE)
  }
  fingerprint_schema <- c(
    "pair_id",
    "expected_row_fingerprint",
    "expected_block_fingerprint"
  )
  if (
    !is.data.frame(expected_fingerprints) ||
      !identical(names(expected_fingerprints), fingerprint_schema) ||
      !is.character(expected_fingerprints$pair_id) ||
      anyNA(expected_fingerprints$pair_id) ||
      anyDuplicated(expected_fingerprints$pair_id) ||
      !setequal(expected_fingerprints$pair_id, schedule$pair_id) ||
      !all(vapply(expected_fingerprints[-1L], is.character, logical(1L))) ||
      anyNA(expected_fingerprints[-1L]) ||
      any(
        !vapply(
          expected_fingerprints[-1L],
          function(values) {
            all(grepl(.bench_sha256_pattern, values))
          },
          logical(1L)
        )
      )
  ) {
    stop("expected fingerprints are invalid", call. = FALSE)
  }
  audits <- list(
    export = .bench_audit_outcome_table(export_rows, "export"),
    access = .bench_audit_outcome_table(access_rows, "access")
  )
  export_rows <- audits$export$table
  access_rows <- audits$access$table
  expected_ids <- schedule$pair_id
  for (phase in c("export", "access")) {
    raw <- if (phase == "export") export_rows else access_rows
    ok_rows <- which(!is.na(raw$status) & raw$status == "OK")
    for (i in ok_rows) {
      error <- tryCatch(
        {
          .bench_validate_ok_outcome(raw[i, , drop = FALSE], phase)
          NULL
        },
        error = function(error) conditionMessage(error)
      )
      if (!is.null(error)) {
        audits[[phase]]$errors <- c(
          audits[[phase]]$errors,
          paste0("row ", i, " OK evidence: ", error)
        )
      }
    }
  }
  for (pair_id in expected_ids) {
    export <- export_rows[
      export_rows$pair_id == pair_id & export_rows$status == "OK",
      ,
      drop = FALSE
    ]
    access <- access_rows[
      access_rows$pair_id == pair_id & access_rows$status == "OK",
      ,
      drop = FALSE
    ]
    if (
      nrow(export) == 1L &&
        nrow(access) == 1L &&
        (!identical(export$artifact_path, access$artifact_path) ||
          !identical(export$package_path, access$package_path))
    ) {
      audits$access$errors <- c(
        audits$access$errors,
        paste0(
          "pair ",
          pair_id,
          " export/access artifact or package paths differ"
        )
      )
    }
  }
  rows <- list()
  add <- function(...) rows <<- .bench_gate_row(rows, ...)
  for (phase in c("export", "access")) {
    raw <- if (phase == "export") export_rows else access_rows
    contract_errors <- audits[[phase]]$errors
    add(
      paste0(phase, "_raw_contract"),
      panel,
      phase,
      expected = "fixed typed and semantically valid raw rows",
      observed = if (length(contract_errors)) {
        paste(contract_errors, collapse = "; ")
      } else {
        "valid"
      },
      status = if (length(contract_errors)) "FAIL" else "PASS",
      detail = if (length(contract_errors)) {
        "raw outcome contract violation"
      } else {
        NA_character_
      }
    )
    unscheduled <- unique(raw$pair_id[!raw$pair_id %in% expected_ids])
    add(
      paste0(phase, "_scheduled_set"),
      panel,
      phase,
      expected = "no unscheduled pair_id",
      observed = if (length(unscheduled)) {
        paste(unscheduled, collapse = ",")
      } else {
        "none"
      },
      status = if (length(unscheduled)) "FAIL" else "PASS",
      detail = if (length(unscheduled)) {
        "raw outcomes contain unscheduled pair IDs"
      } else {
        NA_character_
      }
    )
    duplicates <- unique(raw$pair_id[
      duplicated(raw$pair_id) | duplicated(raw$pair_id, fromLast = TRUE)
    ])
    add(
      paste0(phase, "_unique"),
      panel,
      phase,
      expected = "one row per pair_id",
      observed = if (length(duplicates)) {
        paste(duplicates, collapse = ",")
      } else {
        "unique"
      },
      status = if (length(duplicates)) "FAIL" else "PASS",
      detail = if (length(duplicates)) {
        "duplicate raw outcome rows"
      } else {
        NA_character_
      }
    )
    for (pair_id in expected_ids) {
      found <- raw[raw$pair_id == pair_id, , drop = FALSE]
      if (nrow(found) == 0L) {
        add(
          paste0(phase, "_result:", pair_id),
          panel,
          phase,
          expected = "OK",
          observed = "absent",
          status = "MISSING_RESULT",
          detail = "expected pair_id is absent from raw outcomes"
        )
      } else if (nrow(found) != 1L) {
        add(
          paste0(phase, "_result:", pair_id),
          panel,
          phase,
          expected = "one OK row",
          observed = paste0(nrow(found), " rows"),
          status = "FAIL",
          detail = "pair_id does not have exactly one raw outcome"
        )
      } else {
        ok <- identical(found$status, "OK")
        add(
          paste0(phase, "_result:", pair_id),
          panel,
          phase,
          expected = "OK",
          observed = found$status,
          status = if (ok) "PASS" else "FAIL",
          detail = if (ok) {
            NA_character_
          } else {
            "scheduled worker outcome is not OK"
          }
        )
      }
    }
  }
  expected_by_id <- expected_fingerprints[
    match(expected_ids, expected_fingerprints$pair_id),
  ]
  for (i in seq_along(expected_ids)) {
    pair_id <- expected_ids[[i]]
    found <- access_rows[access_rows$pair_id == pair_id, , drop = FALSE]
    expected_row <- expected_by_id$expected_row_fingerprint[[i]]
    expected_block <- expected_by_id$expected_block_fingerprint[[i]]
    pass <- nrow(found) == 1L &&
      identical(found$status, "OK") &&
      identical(found$expected_row_fingerprint, expected_row) &&
      identical(found$observed_row_fingerprint, expected_row) &&
      identical(found$expected_block_fingerprint, expected_block) &&
      identical(found$observed_block_fingerprint, expected_block) &&
      identical(found$correctness, "PASS")
    observed <- if (nrow(found) == 1L) {
      paste(
        found$observed_row_fingerprint,
        found$observed_block_fingerprint,
        found$correctness,
        sep = "|"
      )
    } else {
      paste0(nrow(found), " rows")
    }
    add(
      paste0("access_correctness:", pair_id),
      panel,
      "access_correctness",
      expected = paste(expected_row, expected_block, "PASS", sep = "|"),
      observed = observed,
      status = if (pass) "PASS" else "FAIL",
      detail = if (pass) {
        NA_character_
      } else {
        "query fingerprints or correctness do not match"
      }
    )
  }
  checks <- if (length(rows)) do.call(rbind, rows) else bench_empty_validation()
  rownames(checks) <- NULL
  valid <- nrow(checks) > 0L && all(checks$status == "PASS")
  final <- bench_validation_row(
    "panel_valid",
    panel,
    "panel",
    expected = "all required gates PASS",
    observed = if (valid) {
      "all required gates PASS"
    } else {
      "one or more required gates failed"
    },
    status = if (valid) "VALID" else "INVALID",
    detail = if (valid) NA_character_ else "panel raw outcome gate is invalid"
  )
  result <- rbind(checks, final)
  rownames(result) <- NULL
  stopifnot(
    identical(names(result), bench_validation_schema()),
    identical(tail(result$check_id, 1L), "panel_valid"),
    sum(result$check_id == "panel_valid") == 1L
  )
  result
}

# ---- parent-runner protocol -------------------------------------------------

.bench_read_csv <- function(path, required = NULL) {
  path <- .bench_scalar_string(path, "path")
  if (!file.exists(path) || dir.exists(path) || .bench_is_symlink(path)) {
    stop(
      "required evidence file is missing or unsafe: ",
      basename(path),
      call. = FALSE
    )
  }
  rows <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = "NA"
  )
  if (!is.null(required) && !identical(names(rows), required)) {
    stop("evidence schema is invalid: ", basename(path), call. = FALSE)
  }
  rows
}

.bench_is_symlink <- function(path) {
  target <- Sys.readlink(path)
  is.character(target) &&
    length(target) == 1L &&
    !is.na(target) &&
    nzchar(target)
}

.bench_atomic_save_rds <- function(object, path) {
  parent <- dirname(path)
  if (!dir.exists(parent)) {
    stop("RDS parent directory is missing", call. = FALSE)
  }
  temporary <- tempfile(paste0(".", basename(path), "."), tmpdir = parent)
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(object, temporary, version = 3)
  if (!file.rename(temporary, path)) {
    stop("could not atomically install RDS", call. = FALSE)
  }
  invisible(path)
}

bench_parse_args <- function(args, panel) {
  panel <- match.arg(panel, c("comparison", "full_scale"))
  if (!is.character(args) || anyNA(args)) {
    stop("arguments must be character", call. = FALSE)
  }
  if (identical(args, "--dry-run")) {
    return(list(
      panel = panel,
      dry_run = TRUE,
      source_path = NULL,
      panel_a_dir = NULL,
      output_path = NULL
    ))
  }
  expected <- if (panel == "comparison") 2L else 3L
  if (
    length(args) != expected ||
      any(!nzchar(args)) ||
      any(startsWith(args, "--"))
  ) {
    syntax <- if (panel == "comparison") {
      "<local.h5ad> <new-output-dir>"
    } else {
      "<local.h5ad> <panel-a-dir> <new-output-dir>"
    }
    stop("usage: ", syntax, " or --dry-run", call. = FALSE)
  }
  list(
    panel = panel,
    dry_run = FALSE,
    source_path = args[[1L]],
    panel_a_dir = if (panel == "full_scale") args[[2L]] else NULL,
    output_path = args[[expected]]
  )
}

.bench_nearest_existing_parent <- function(path) {
  candidate <- path
  while (!file.exists(candidate) && !dir.exists(candidate)) {
    parent <- dirname(candidate)
    if (identical(parent, candidate)) {
      stop("output has no existing parent", call. = FALSE)
    }
    candidate <- parent
  }
  if (!dir.exists(candidate) || .bench_is_symlink(candidate)) {
    stop(
      "nearest existing output parent must be a real directory",
      call. = FALSE
    )
  }
  normalizePath(candidate, mustWork = TRUE)
}

bench_validate_output_candidate <- function(path, repo, panel_a_dir = NULL) {
  path <- .bench_scalar_string(path, "output path")
  repo <- normalizePath(.bench_scalar_string(repo, "repo"), mustWork = TRUE)
  expanded <- path.expand(path)
  if (!startsWith(expanded, .Platform$file.sep)) {
    expanded <- file.path(getwd(), expanded)
  }
  components <- strsplit(expanded, .Platform$file.sep, fixed = TRUE)[[1L]]
  if (any(components == "..")) {
    stop("output path must not contain parent traversal", call. = FALSE)
  }
  if (file.exists(path) || dir.exists(path)) {
    stop("output path must not exist", call. = FALSE)
  }
  if (.bench_is_symlink(path)) {
    stop("output path must not be a symlink", call. = FALSE)
  }
  cursor <- expanded
  suffix <- character()
  while (!file.exists(cursor) && !dir.exists(cursor)) {
    suffix <- c(basename(cursor), suffix)
    cursor <- dirname(cursor)
  }
  parent <- .bench_nearest_existing_parent(cursor)
  candidate <- do.call(file.path, as.list(c(parent, suffix)))
  if (
    .bench_descendant(parent, repo) ||
      identical(parent, repo) ||
      startsWith(candidate, paste0(repo, .Platform$file.sep))
  ) {
    stop("output path must be outside the Git worktree", call. = FALSE)
  }
  if (!is.null(panel_a_dir)) {
    panel_a <- normalizePath(
      .bench_scalar_string(panel_a_dir, "panel_a_dir"),
      mustWork = TRUE
    )
    if (!dir.exists(panel_a) || .bench_is_symlink(panel_a_dir)) {
      stop("Panel A input must be an existing real directory", call. = FALSE)
    }
    if (
      identical(parent, panel_a) ||
        .bench_descendant(parent, panel_a) ||
        startsWith(candidate, paste0(panel_a, .Platform$file.sep))
    ) {
      stop(
        "Panel B output must differ from and not be nested under Panel A",
        call. = FALSE
      )
    }
  }
  normalizePath(candidate, mustWork = FALSE)
}

bench_prepare_output <- function(path) {
  path <- .bench_scalar_string(path, "path")
  if (file.exists(path) || dir.exists(path)) {
    stop("output path must not exist", call. = FALSE)
  }
  expanded <- path.expand(path)
  if (!startsWith(expanded, .Platform$file.sep)) {
    expanded <- file.path(getwd(), expanded)
  }
  components <- strsplit(expanded, .Platform$file.sep, fixed = TRUE)[[1L]]
  if (any(components %in% c(".", ".."))) {
    stop("output path contains unsafe traversal", call. = FALSE)
  }
  cursor <- expanded
  suffix <- character()
  while (!file.exists(cursor) && !dir.exists(cursor)) {
    component <- basename(cursor)
    if (!nzchar(component) || component %in% c(".", "..")) {
      stop("output path contains an unsafe component", call. = FALSE)
    }
    suffix <- c(component, suffix)
    cursor <- dirname(cursor)
  }
  parent <- .bench_nearest_existing_parent(cursor)
  for (component in suffix) {
    if (.bench_is_symlink(parent) || !dir.exists(parent)) {
      stop("output parent changed or became unsafe", call. = FALSE)
    }
    parent <- normalizePath(parent, mustWork = TRUE)
    child <- file.path(parent, component)
    if (file.exists(child) || dir.exists(child) || .bench_is_symlink(child)) {
      stop("output suffix appeared during creation", call. = FALSE)
    }
    if (!dir.create(child, recursive = FALSE, mode = "0700")) {
      stop("could not create output path component", call. = FALSE)
    }
    if (.bench_is_symlink(child) || !dir.exists(child)) {
      stop("created output component is unsafe", call. = FALSE)
    }
    normalized_child <- normalizePath(child, mustWork = TRUE)
    if (
      !identical(dirname(normalized_child), parent) ||
        !identical(normalized_child, file.path(parent, component))
    ) {
      stop(
        "created output component escaped its validated parent",
        call. = FALSE
      )
    }
    parent <- normalized_child
  }
  path <- parent
  writeLines(
    "cerebro-benchmark-run-v1",
    file.path(path, ".cerebro-benchmark-run")
  )
  scratch <- file.path(path, "scratch")
  logs <- file.path(path, "logs")
  library <- file.path(scratch, "library")
  if (!dir.create(scratch, mode = "0700")) {
    stop("could not create scratch", call. = FALSE)
  }
  writeLines(
    "bench-scratch-v1",
    file.path(scratch, ".cerebro-benchmark-scratch")
  )
  if (!dir.create(logs, mode = "0700")) {
    stop("could not create logs", call. = FALSE)
  }
  if (!dir.create(library, mode = "0700")) {
    stop("could not create run-local library", call. = FALSE)
  }
  writeLines("run-library-v1", file.path(library, ".cerebro-benchmark-library"))
  list(output = path, scratch = scratch, logs = logs, library = library)
}

.bench_harness_payload <- function(config_sha256, helpers_sha256) {
  list(
    schema = "bench-frozen-harness-v1",
    config_sha256 = config_sha256,
    helpers_sha256 = helpers_sha256
  )
}

.bench_harness_after_copy <- function(source_paths, frozen_paths) {
  invisible(NULL)
}

bench_freeze_harness <- function(bench_root, scratch) {
  bench_root <- normalizePath(
    .bench_scalar_string(bench_root, "bench_root"),
    mustWork = TRUE
  )
  scratch <- normalizePath(
    .bench_scalar_string(scratch, "scratch"),
    mustWork = TRUE
  )
  if (
    .bench_is_symlink(bench_root) ||
      .bench_is_symlink(scratch) ||
      !file.exists(file.path(scratch, ".cerebro-benchmark-scratch"))
  ) {
    stop("harness source or scratch root is unsafe", call. = FALSE)
  }
  source_paths <- setNames(
    file.path(bench_root, c("config.R", "helpers.R")),
    c("config.R", "helpers.R")
  )
  if (
    any(!file.exists(source_paths)) ||
      any(vapply(source_paths, .bench_is_symlink, logical(1L))) ||
      any(file.info(source_paths)$isdir)
  ) {
    stop("harness source files are missing or unsafe", call. = FALSE)
  }
  source_hashes_before <- vapply(source_paths, bench_sha256_file, character(1L))
  frozen_root <- file.path(scratch, "harness")
  if (
    file.exists(frozen_root) ||
      dir.exists(frozen_root) ||
      .bench_is_symlink(frozen_root) ||
      !dir.create(frozen_root, recursive = FALSE, mode = "0700")
  ) {
    stop("could not create frozen harness directory", call. = FALSE)
  }
  if (!identical(dirname(normalizePath(frozen_root)), scratch)) {
    stop("frozen harness escaped scratch", call. = FALSE)
  }
  frozen_paths <- setNames(
    file.path(frozen_root, names(source_paths)),
    names(source_paths)
  )
  copied <- mapply(
    file.copy,
    source_paths,
    frozen_paths,
    MoreArgs = list(copy.mode = TRUE),
    USE.NAMES = FALSE
  )
  if (
    !all(copied) || any(vapply(frozen_paths, .bench_is_symlink, logical(1L)))
  ) {
    stop("could not safely copy frozen harness files", call. = FALSE)
  }
  frozen_hashes <- vapply(frozen_paths, bench_sha256_file, character(1L))
  .bench_harness_after_copy(source_paths, frozen_paths)
  if (
    any(!file.exists(source_paths)) ||
      any(vapply(source_paths, .bench_is_symlink, logical(1L))) ||
      any(file.info(source_paths)$isdir)
  ) {
    stop("harness source changed while freezing", call. = FALSE)
  }
  source_hashes_after <- vapply(source_paths, bench_sha256_file, character(1L))
  if (
    !identical(unname(source_hashes_before), unname(frozen_hashes)) ||
      !identical(source_hashes_after, source_hashes_before)
  ) {
    stop("harness source or copy changed while freezing", call. = FALSE)
  }
  payload <- .bench_harness_payload(
    frozen_hashes[["config.R"]],
    frozen_hashes[["helpers.R"]]
  )
  payload$harness_sha256 <- bench_sha256_object(payload)
  .bench_atomic_save_rds(
    payload,
    file.path(frozen_root, ".cerebro-benchmark-harness.rds")
  )
  snapshot <- list(
    root = normalizePath(frozen_root),
    paths = frozen_paths,
    hashes = frozen_hashes,
    payload = payload
  )
  bench_assert_frozen_harness(snapshot)
  snapshot
}

bench_assert_frozen_harness <- function(snapshot) {
  if (
    !is.list(snapshot) ||
      !identical(names(snapshot), c("root", "paths", "hashes", "payload")) ||
      .bench_is_symlink(snapshot$root) ||
      !dir.exists(snapshot$root)
  ) {
    stop("frozen harness snapshot is invalid", call. = FALSE)
  }
  marker <- file.path(snapshot$root, ".cerebro-benchmark-harness.rds")
  if (
    !file.exists(marker) ||
      .bench_is_symlink(marker) ||
      any(!file.exists(snapshot$paths)) ||
      any(vapply(snapshot$paths, .bench_is_symlink, logical(1L)))
  ) {
    stop("frozen harness files or marker are unsafe", call. = FALSE)
  }
  observed <- vapply(snapshot$paths, bench_sha256_file, character(1L))
  marker_payload <- readRDS(marker)
  expected_hash <- bench_sha256_object(snapshot$payload[
    names(snapshot$payload) != "harness_sha256"
  ])
  if (
    !identical(observed, snapshot$hashes) ||
      !identical(marker_payload, snapshot$payload) ||
      !identical(snapshot$payload$harness_sha256, expected_hash)
  ) {
    stop("frozen harness changed", call. = FALSE)
  }
  invisible(TRUE)
}

.bench_git <- function(repo, args) {
  output <- system2(
    "git",
    c("-C", shQuote(repo), args),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(output, "status", exact = TRUE)
  if (!is.null(status) && status != 0L) {
    stop("git command failed: ", paste(output, collapse = "\n"), call. = FALSE)
  }
  output
}

bench_record_environment <- function(repo, command) {
  repo <- normalizePath(.bench_scalar_string(repo, "repo"), mustWork = TRUE)
  status <- .bench_git(
    repo,
    c("status", "--porcelain=v1", "--untracked-files=normal")
  )
  if (length(status)) {
    stop("Git worktree must be clean before a real run", call. = FALSE)
  }
  sha <- trimws(.bench_git(repo, c("rev-parse", "HEAD"))[[1L]])
  if (!grepl("^[0-9a-f]{40}$", sha)) {
    stop("Git SHA is invalid", call. = FALSE)
  }
  system_value <- function(args) {
    value <- suppressWarnings(system2(
      "sysctl",
      args,
      stdout = TRUE,
      stderr = FALSE
    ))
    status <- attr(value, "status", exact = TRUE)
    if ((!is.null(status) && status != 0L) || length(value) != 1L) {
      NA_character_
    } else {
      value[[1L]]
    }
  }
  list(
    git_sha = sha,
    repo = repo,
    command = .bench_scalar_string(command, "command"),
    git_dirty = "false",
    started_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    r_version = R.version.string,
    platform = R.version$platform,
    os = paste(Sys.info()[c("sysname", "release", "machine")], collapse = "|"),
    cpu = system_value(c("-n", "machdep.cpu.brand_string")),
    physical_memory_bytes = system_value(c("-n", "hw.memsize"))
  )
}

bench_assert_environment_unchanged <- function(environment) {
  if (
    !is.list(environment) ||
      !is.character(environment$repo) ||
      length(environment$repo) != 1L ||
      !is.character(environment$git_sha) ||
      length(environment$git_sha) != 1L
  ) {
    stop("recorded Git environment is invalid", call. = FALSE)
  }
  repo <- normalizePath(environment$repo, mustWork = TRUE)
  status <- .bench_git(
    repo,
    c("status", "--porcelain=v1", "--untracked-files=normal")
  )
  sha <- trimws(.bench_git(repo, c("rev-parse", "HEAD"))[[1L]])
  if (length(status) || !identical(sha, environment$git_sha)) {
    stop(
      "Git worktree or HEAD changed during benchmark execution",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

bench_install_tree <- function(repo, library, log) {
  repo <- normalizePath(.bench_scalar_string(repo, "repo"), mustWork = TRUE)
  library <- normalizePath(
    .bench_scalar_string(library, "library"),
    mustWork = TRUE
  )
  log <- .bench_scalar_string(log, "log")
  expected_sha <- trimws(.bench_git(repo, c("rev-parse", "HEAD"))[[1L]])
  if (
    length(.bench_git(
      repo,
      c("status", "--porcelain=v1", "--untracked-files=normal")
    ))
  ) {
    stop("refusing to install a dirty Git tree", call. = FALSE)
  }
  output <- system2(
    file.path(R.home("bin"), "R"),
    c(
      "CMD",
      "INSTALL",
      "--no-multiarch",
      paste0("--library=", shQuote(library)),
      shQuote(repo)
    ),
    stdout = log,
    stderr = log
  )
  if (!identical(output, 0L)) {
    stop("R CMD INSTALL failed; see install log", call. = FALSE)
  }
  description <- read.dcf(file.path(library, "CerebroNexus", "DESCRIPTION"))
  marker <- list(
    schema = "bench-install-v1",
    git_sha = expected_sha,
    source_path = repo,
    package_version = unname(description[1L, "Version"]),
    install_log_sha256 = bench_sha256_file(log)
  )
  .bench_atomic_save_rds(
    marker,
    file.path(library, ".cerebro-benchmark-install.rds")
  )
  marker
}

bench_validate_installed_tree <- function(library, expected_git_sha) {
  library <- normalizePath(
    .bench_scalar_string(library, "library"),
    mustWork = TRUE
  )
  expected_git_sha <- .bench_scalar_string(expected_git_sha, "expected_git_sha")
  marker_path <- file.path(library, ".cerebro-benchmark-install.rds")
  if (!file.exists(marker_path) || .bench_is_symlink(marker_path)) {
    stop("install marker is missing", call. = FALSE)
  }
  marker <- readRDS(marker_path)
  required <- c(
    "schema",
    "git_sha",
    "source_path",
    "package_version",
    "install_log_sha256"
  )
  if (
    !is.list(marker) ||
      !identical(names(marker), required) ||
      !identical(marker$schema, "bench-install-v1") ||
      !identical(marker$git_sha, expected_git_sha)
  ) {
    stop("install marker provenance is invalid", call. = FALSE)
  }
  package_path <- file.path(library, "CerebroNexus")
  if (!dir.exists(package_path) || !.bench_descendant(package_path, library)) {
    stop("installed package origin is invalid", call. = FALSE)
  }
  resolved <- find.package("CerebroNexus", quiet = TRUE)
  if (
    !nzchar(resolved) ||
      !.bench_descendant(resolved, library) ||
      !identical(normalizePath(resolved), normalizePath(package_path))
  ) {
    stop(
      "find.package did not resolve the marked run-local package",
      call. = FALSE
    )
  }
  actual_version <- unname(read.dcf(file.path(package_path, "DESCRIPTION"))[
    1L,
    "Version"
  ])
  if (!identical(actual_version, marker$package_version)) {
    stop("installed package version differs from marker", call. = FALSE)
  }
  marker$package_path <- normalizePath(package_path, mustWork = TRUE)
  marker
}

.bench_runtime_manifest <- function(library) {
  packages <- c(
    "CerebroNexus",
    "Matrix",
    "SeuratObject",
    "Seurat",
    "BPCells",
    "HDF5Array",
    "rhdf5",
    "digest",
    "callr"
  )
  rows <- lapply(packages, function(package) {
    path <- find.package(package)
    data.frame(
      package = package,
      version = as.character(utils::packageVersion(package)),
      package_path = normalizePath(path),
      stringsAsFactors = FALSE
    )
  })
  rows <- do.call(rbind, rows[order(packages)])
  cerebro <- rows$package_path[rows$package == "CerebroNexus"]
  if (!.bench_descendant(cerebro, library)) {
    stop("setup loaded CerebroNexus outside run-local library", call. = FALSE)
  }
  payload <- list(
    r_version = R.version.string,
    platform = R.version$platform,
    packages = paste(rows$package, rows$version, sep = "=")
  )
  list(rows = rows, runtime_sha256 = bench_sha256_object(payload))
}

.bench_sampling_rows <- function(tiers, inventory, plans) {
  do.call(
    rbind,
    lapply(names(tiers), function(label) {
      n <- as.integer(tiers[[label]])
      indices <- bench_stratified_indices(inventory$n_cells, n)
      blocks <- bench_stratified_blocks(inventory$n_cells, n)
      plan <- plans[[label]]
      data.frame(
        tier_label = label,
        n_cells = n,
        stratum = as.integer(blocks$stratum),
        start = as.integer(blocks$start),
        end = as.integer(blocks$end),
        n = as.integer(blocks$n),
        exact_nnz = bench_exact_selected_nnz(
          inventory$n_cells,
          n,
          inventory$nnz_per_cell
        ),
        indices_sha256 = plan$ordered_indices_sha256,
        cell_identity_sha256 = plan$cell_identity_sha256,
        shell_sha256 = NA_character_,
        stringsAsFactors = FALSE
      )
    })
  )
}

bench_setup_worker <- function(
  panel,
  source_path,
  imported_panel_a,
  run_context
) {
  panel <- match.arg(panel, c("comparison", "full_scale"))
  if (!exists("BENCH_CONFIG", inherits = TRUE)) {
    stop("BENCH_CONFIG is required", call. = FALSE)
  }
  config <- get("BENCH_CONFIG", inherits = TRUE)
  output <- normalizePath(run_context$output, mustWork = TRUE)
  install <- bench_validate_installed_tree(
    run_context$library,
    run_context$git_sha
  )
  inventory <- bench_source_inventory(source_path, config$source)
  common <- bench_freeze_common_tier(
    config$common_target,
    config$common_min_exclusive,
    inventory$nnz_per_cell,
    config$sparse_index_limit
  )
  if (
    panel == "full_scale" &&
      !identical(
        common$common_target_actual,
        as.integer(imported_panel_a$common_target_actual)
      )
  ) {
    stop("locally recomputed common target differs from Panel A", call. = FALSE)
  }
  tiers <- if (panel == "comparison") {
    c(config$comparison_fixed_tiers, common = common$common_target_actual)
  } else {
    c(common = common$common_target_actual, config$full_scale_fixed_tiers)
  }
  genes <- if (panel == "comparison") {
    bench_select_query_genes(
      inventory$matrix,
      bench_stratified_indices(
        inventory$n_cells,
        config$comparison_fixed_tiers[[1L]]
      ),
      config$query_genes
    )
  } else {
    imported_panel_a$genes
  }
  plans <- setNames(
    lapply(tiers, function(n) {
      bench_build_query_plan(
        inventory$matrix,
        bench_stratified_indices(inventory$n_cells, n),
        genes
      )
    }),
    names(tiers)
  )
  sampling <- .bench_sampling_rows(tiers, inventory, plans)
  # The shell hash binds the real gene/cell dimensions but not matrix representation or values.
  for (label in names(tiers)) {
    indices <- bench_stratified_indices(inventory$n_cells, tiers[[label]])
    expression <- inventory$matrix[, indices, drop = FALSE]
    shell <- bench_make_seurat_shell(expression, indices)
    sampling$shell_sha256[
      sampling$tier_label == label
    ] <- bench_shell_fingerprint(shell, indices)
  }
  exact <- setNames(
    vapply(
      tiers,
      function(n) {
        bench_exact_selected_nnz(
          inventory$n_cells,
          n,
          inventory$nnz_per_cell
        )
      },
      numeric(1L)
    ),
    names(tiers)
  )
  eligibility <- bench_eligibility(
    panel,
    tiers,
    exact,
    config$sparse_index_limit
  )
  bench_write_sampling_manifest(file.path(output, "sampling.csv"), sampling)
  bench_write_query_manifest(file.path(output, "queries.csv"), plans)
  .bench_atomic_save_rds(plans, file.path(output, "query-plan.rds"))
  .bench_atomic_write_csv(file.path(output, "eligibility.csv"), eligibility)
  source_rows <- data.frame(
    source_key = config$source$key,
    source_url = config$source$url,
    expected_bytes = as.double(config$source$expected_bytes),
    actual_bytes = as.double(inventory$bytes),
    expected_sha256 = config$source$expected_sha256,
    actual_sha256 = inventory$sha256,
    n_cells = inventory$n_cells,
    n_genes = inventory$n_genes,
    exact_nnz = inventory$nnz,
    stringsAsFactors = FALSE
  )
  .bench_atomic_write_csv(file.path(output, "source.csv"), source_rows)
  runtime <- .bench_runtime_manifest(run_context$library)
  list(
    common_target_actual = common$common_target_actual,
    tiers = tiers,
    source_path = inventory$path,
    source_bytes = inventory$bytes,
    source_sha256 = inventory$sha256,
    source_identity = inventory$identity,
    sampling = sampling,
    genes = genes,
    plans = lapply(plans, function(x) {
      x[c(
        "query_plan_sha256",
        "ordered_indices_sha256",
        "cell_identity_sha256",
        "first_row_numeric_sha256",
        "block_numeric_sha256"
      )]
    }),
    runtime_sha256 = runtime$runtime_sha256,
    runtime = runtime$rows,
    package_path = install$package_path,
    package_version = install$package_version
  )
}

bench_run_setup_worker <- function(
  panel,
  source_path,
  imported_panel_a,
  run_context,
  log_path
) {
  .bench_require_namespace("callr")
  started <- proc.time()[["elapsed"]]
  process <- callr::r_bg(
    function(root, panel, source_path, imported, context) {
      gc(reset = TRUE)
      assert_frozen <- function() {
        snapshot <- context$harness_snapshot
        if (
          is.null(snapshot) ||
            !requireNamespace("digest", quietly = TRUE) ||
            !is.list(snapshot) ||
            is.null(snapshot$root) ||
            is.null(snapshot$hashes) ||
            !identical(names(snapshot$hashes), c("config.R", "helpers.R"))
        ) {
          stop("frozen harness bootstrap contract is invalid", call. = FALSE)
        }
        root_normalized <- normalizePath(root, mustWork = TRUE)
        if (
          !identical(
            root_normalized,
            normalizePath(snapshot$root, mustWork = TRUE)
          ) ||
            nzchar(Sys.readlink(root_normalized))
        ) {
          stop("frozen harness bootstrap root changed", call. = FALSE)
        }
        paths <- setNames(
          file.path(root_normalized, c("config.R", "helpers.R")),
          c("config.R", "helpers.R")
        )
        info <- file.info(paths)
        unsafe <- any(!file.exists(paths)) ||
          any(is.na(info$isdir)) ||
          any(info$isdir) ||
          any(vapply(
            paths,
            function(path) nzchar(Sys.readlink(path)),
            logical(1L)
          ))
        if (unsafe) {
          stop("frozen harness bootstrap files are unsafe", call. = FALSE)
        }
        observed <- vapply(
          paths,
          function(path) {
            digest::digest(
              path,
              algo = "sha256",
              file = TRUE,
              serialize = FALSE
            )
          },
          character(1L)
        )
        if (!identical(observed, snapshot$hashes)) {
          stop("frozen harness changed across subprocess source", call. = FALSE)
        }
        invisible(TRUE)
      }
      assert_frozen()
      source(file.path(root, "config.R"), local = TRUE)
      source(file.path(root, "helpers.R"), local = TRUE)
      assert_frozen()
      value <- bench_setup_worker(panel, source_path, imported, context)
      assert_frozen()
      list(value = value, r_heap_peak_mb = bench_safe_r_heap_peak_mb())
    },
    args = list(
      run_context$bench_root,
      panel,
      source_path,
      imported_panel_a,
      run_context
    ),
    libpath = c(run_context$library, .libPaths()),
    system_profile = FALSE,
    user_profile = FALSE,
    supervise = TRUE,
    stdout = log_path,
    stderr = "2>&1"
  )
  peak <- NA_real_
  while (process$is_alive()) {
    peak <- .bench_update_peak_rss(peak, bench_rss_mb(process$get_pid()))
    Sys.sleep(0.5)
  }
  process$wait()
  if (!identical(process$get_exit_status(), 0L)) {
    stop("setup worker failed; see setup log", call. = FALSE)
  }
  returned <- process$get_result()
  list(
    value = returned$value,
    elapsed_secs = as.double(proc.time()[["elapsed"]] - started),
    peak_rss_mb = peak,
    r_heap_peak_mb = returned$r_heap_peak_mb,
    log_path = normalizePath(log_path, mustWork = TRUE)
  )
}

bench_write_validation <- function(path, checks) {
  if (
    !is.data.frame(checks) ||
      !identical(names(checks), bench_validation_schema()) ||
      sum(checks$check_id == "panel_valid") != 1L ||
      !identical(tail(checks$check_id, 1L), "panel_valid")
  ) {
    stop("validation evidence is invalid", call. = FALSE)
  }
  .bench_atomic_write_csv(path, checks)
}

bench_validate_panel <- function(
  schedule,
  eligibility,
  exports,
  access,
  sampling,
  plans,
  linkage = NULL,
  evidence = NULL
) {
  panel <- unique(schedule$panel)
  if (length(panel) != 1L) {
    stop("schedule panel is invalid", call. = FALSE)
  }
  if (!panel %in% c("comparison", "full_scale")) {
    stop("schedule panel is invalid", call. = FALSE)
  }
  expected_rows <- nrow(schedule)
  bench_validate_schedule(schedule, expected_rows)
  if (
    !is.data.frame(eligibility) ||
      !identical(
        names(eligibility),
        c(
          "panel",
          "tier_label",
          "n_cells",
          "backend",
          "exact_nnz",
          "status",
          "reason"
        )
      )
  ) {
    stop("eligibility schema is invalid", call. = FALSE)
  }
  if (
    !is.data.frame(sampling) ||
      !all(
        c("tier_label", "indices_sha256", "shell_sha256") %in% names(sampling)
      )
  ) {
    stop("sampling evidence is invalid", call. = FALSE)
  }
  if (!is.list(plans) || !length(plans)) {
    stop("query plans are invalid", call. = FALSE)
  }
  tier_names <- unique(schedule$tier_label)
  if (
    !setequal(names(plans), tier_names) ||
      !identical(
        names(sampling),
        c(
          "tier_label",
          "n_cells",
          "stratum",
          "start",
          "end",
          "n",
          "exact_nnz",
          "indices_sha256",
          "cell_identity_sha256",
          "shell_sha256"
        )
      ) ||
      nrow(sampling) != 4L * length(tier_names)
  ) {
    stop("sampling/query-plan tier coverage is invalid", call. = FALSE)
  }
  for (tier in tier_names) {
    rows <- sampling[sampling$tier_label == tier, , drop = FALSE]
    if (
      nrow(rows) != 4L ||
        !setequal(as.integer(rows$stratum), 1:4) ||
        any(vapply(
          rows[c(
            "n_cells",
            "exact_nnz",
            "indices_sha256",
            "cell_identity_sha256",
            "shell_sha256"
          )],
          function(x) length(unique(x)) != 1L,
          logical(1L)
        ))
    ) {
      stop("sampling tier rows are incomplete or inconsistent", call. = FALSE)
    }
  }
  allowed_eligibility <- c(
    "SCHEDULED",
    "NOT_APPLICABLE_PROTOCOL",
    "UNSUPPORTED_DGCMATRIX_INDEX"
  )
  expected_eligibility <- expand.grid(
    tier_label = tier_names,
    backend = c("embedded", "bpcells", "h5"),
    stringsAsFactors = FALSE
  )
  actual_keys <- paste(eligibility$tier_label, eligibility$backend, sep = "\r")
  expected_keys_all <- paste(
    expected_eligibility$tier_label,
    expected_eligibility$backend,
    sep = "\r"
  )
  if (
    nrow(eligibility) != length(expected_keys_all) ||
      anyDuplicated(actual_keys) ||
      !setequal(actual_keys, expected_keys_all) ||
      any(!eligibility$status %in% allowed_eligibility)
  ) {
    stop("eligibility coverage or status vocabulary is invalid", call. = FALSE)
  }
  fingerprints <- do.call(
    rbind,
    lapply(seq_len(nrow(schedule)), function(i) {
      plan <- plans[[schedule$tier_label[[i]]]]
      data.frame(
        pair_id = schedule$pair_id[[i]],
        expected_row_fingerprint = plan$first_row_numeric_sha256,
        expected_block_fingerprint = plan$block_numeric_sha256,
        stringsAsFactors = FALSE
      )
    })
  )
  checks <- bench_raw_outcome_gate(
    schedule,
    exports,
    access,
    fingerprints,
    panel
  )
  expected_keys <- unique(schedule[c(
    "panel",
    "tier_label",
    "n_cells",
    "backend"
  )])
  scheduled_eligibility <- eligibility[
    eligibility$status == "SCHEDULED",
    c("panel", "tier_label", "n_cells", "backend"),
    drop = FALSE
  ]
  eligibility_ok <- nrow(scheduled_eligibility) == nrow(expected_keys) &&
    setequal(
      do.call(paste, c(scheduled_eligibility, sep = "\r")),
      do.call(paste, c(expected_keys, sep = "\r"))
    )
  package_paths <- c(exports$package_path, access$package_path)
  package_ok <- length(package_paths) == 2L * nrow(schedule) &&
    !anyNA(package_paths) &&
    all(nzchar(package_paths)) &&
    length(unique(package_paths)) == 1L
  shell_expected <- setNames(sampling$shell_sha256, sampling$tier_label)
  shell_expected <- shell_expected[!duplicated(names(shell_expected))]
  shell_observed <- shell_expected[match(
    schedule$tier_label,
    names(shell_expected)
  )]
  shell_ok <- length(shell_observed) == nrow(schedule) &&
    !anyNA(shell_observed) &&
    identical(
      as.character(exports$shell_sha256[match(
        schedule$pair_id,
        exports$pair_id
      )]),
      as.character(unname(shell_observed))
    )
  extra <- rbind(
    bench_validation_row(
      "eligibility_schedule_coverage",
      panel,
      "eligibility",
      "scheduled rows equal schedule conditions",
      if (eligibility_ok) "equal" else "mismatch",
      if (eligibility_ok) "PASS" else "FAIL",
      if (eligibility_ok) NA_character_ else "eligibility and schedule differ"
    ),
    bench_validation_row(
      "package_origin_consistency",
      panel,
      "package_origin",
      "one nonempty run-local package path",
      if (package_ok) unique(package_paths)[[1L]] else "mismatch",
      if (package_ok) "PASS" else "FAIL",
      if (package_ok) {
        NA_character_
      } else {
        "worker package origins differ or are missing"
      }
    ),
    bench_validation_row(
      "shell_fingerprint_coverage",
      panel,
      "shell",
      "every export matches frozen tier shell",
      if (shell_ok) "match" else "mismatch",
      if (shell_ok) "PASS" else "FAIL",
      if (shell_ok) {
        NA_character_
      } else {
        "worker shell fingerprints differ from sampling evidence"
      }
    )
  )
  final <- checks[nrow(checks), , drop = FALSE]
  checks <- rbind(checks[-nrow(checks), , drop = FALSE], extra, final)
  if (!is.null(evidence)) {
    config <- if (!is.null(evidence$config)) {
      evidence$config
    } else if (exists("BENCH_CONFIG", inherits = TRUE)) {
      get("BENCH_CONFIG", inherits = TRUE)
    } else {
      stop(
        "BENCH_CONFIG is required for frozen evidence validation",
        call. = FALSE
      )
    }
    frozen <- .bench_frozen_evidence_checks(
      evidence,
      panel,
      config,
      schedule = schedule,
      exports = exports,
      access = access
    )
    final <- checks[nrow(checks), , drop = FALSE]
    checks <- rbind(checks[-nrow(checks), , drop = FALSE], frozen, final)
  }
  if (!is.null(linkage)) {
    if (
      !is.data.frame(linkage) ||
        !identical(names(linkage), bench_validation_schema())
    ) {
      stop("linkage checks are invalid", call. = FALSE)
    }
    final <- checks[nrow(checks), , drop = FALSE]
    checks <- rbind(checks[-nrow(checks), , drop = FALSE], linkage, final)
    if (any(checks$status[-nrow(checks)] != "PASS")) {
      checks$status[nrow(checks)] <- "INVALID"
      checks$observed[nrow(checks)] <- "cross-panel linkage failed"
    }
  }
  if (any(checks$status[-nrow(checks)] != "PASS")) {
    checks$status[nrow(checks)] <- "INVALID"
    checks$observed[nrow(checks)] <- "one or more required gates failed"
  }
  checks
}

.bench_panel_a_required_files <- c(
  "manifest.csv",
  "source.csv",
  "sampling.csv",
  "eligibility.csv",
  "queries.csv",
  "query-plan.rds",
  "schedule.csv",
  "export.csv",
  "access.csv",
  "validation.csv"
)

.bench_panel_a_after_first_read <- function(paths) invisible(NULL)

.bench_hash_evidence_paths <- function(paths) {
  if (
    !is.character(paths) ||
      is.null(names(paths)) ||
      anyNA(paths) ||
      any(!file.exists(paths)) ||
      any(vapply(paths, .bench_is_symlink, logical(1L)))
  ) {
    stop("evidence paths are missing or unsafe", call. = FALSE)
  }
  vapply(paths, bench_sha256_file, character(1L))
}

.bench_read_typed_outcome <- function(path, phase) {
  prototype <- bench_empty_outcome(phase, ".prototype")[FALSE, ]
  classes <- vapply(
    prototype,
    function(column) {
      switch(
        typeof(column),
        character = "character",
        integer = "integer",
        double = "numeric",
        logical = "logical",
        stop("unsupported outcome prototype type", call. = FALSE)
      )
    },
    character(1L)
  )
  rows <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = "NA",
    colClasses = unname(classes)
  )
  audit <- .bench_audit_outcome_table(rows, phase)
  if (length(audit$errors)) {
    stop(
      "Panel A ",
      phase,
      " typed CSV is invalid: ",
      paste(audit$errors, collapse = "; "),
      call. = FALSE
    )
  }
  rows
}

.bench_read_typed_validation <- function(path) {
  rows <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = "NA",
    colClasses = rep("character", 7L)
  )
  if (!identical(names(rows), bench_validation_schema())) {
    stop("validation CSV schema is invalid", call. = FALSE)
  }
  rows
}

.bench_validate_frozen_plan <- function(
  plan,
  tier,
  source_sha256,
  source_dimensions = NULL
) {
  required <- c(
    "schema",
    "source_sha256",
    "sampling_sha256",
    "source_dimensions",
    "dimensions",
    "genes",
    "ordered_indices_sha256",
    "cell_identity_sha256",
    "first_row_numeric_sha256",
    "block_numeric_sha256",
    "boundaries",
    "query_plan_sha256"
  )
  gene_schema <- c("gene", "role", "density", "source_row", "tie_break_rank")
  if (
    !is.list(plan) ||
      !identical(names(plan), required) ||
      !identical(plan$schema, "bench-query-plan-v1") ||
      !identical(plan$source_sha256, source_sha256) ||
      !is.data.frame(plan$genes) ||
      !identical(names(plan$genes), gene_schema) ||
      nrow(plan$genes) != 5L ||
      !identical(plan$genes$role, c("first", rep("block", 4L))) ||
      !is.numeric(plan$genes$density) ||
      anyNA(plan$genes$density) ||
      any(!is.finite(plan$genes$density)) ||
      any(plan$genes$density < 0 | plan$genes$density > 1) ||
      !is.numeric(plan$genes$source_row) ||
      anyNA(plan$genes$source_row) ||
      any(plan$genes$source_row != floor(plan$genes$source_row)) ||
      anyDuplicated(plan$genes$source_row) ||
      any(plan$genes$source_row < 1) ||
      any(
        plan$genes$source_row > as.integer(plan$source_dimensions[["genes"]])
      ) ||
      !is.numeric(plan$genes$tie_break_rank) ||
      anyNA(plan$genes$tie_break_rank) ||
      any(plan$genes$tie_break_rank != floor(plan$genes$tie_break_rank)) ||
      any(plan$genes$tie_break_rank < 1) ||
      !identical(names(plan$dimensions), c("genes", "cells")) ||
      !identical(names(plan$source_dimensions), c("genes", "cells")) ||
      !identical(as.integer(plan$dimensions[["genes"]]), 5L) ||
      !identical(
        plan$boundaries,
        bench_stratified_blocks(
          as.integer(plan$source_dimensions[["cells"]]),
          as.integer(plan$dimensions[["cells"]])
        )
      )
  ) {
    stop("frozen query plan is invalid for tier ", tier, call. = FALSE)
  }
  .bench_canonical_ids(plan$genes$gene, "frozen query genes")
  if (
    !is.null(source_dimensions) &&
      !identical(
        as.integer(plan$source_dimensions),
        as.integer(source_dimensions)
      )
  ) {
    stop(
      "frozen query plan source dimensions differ for tier ",
      tier,
      call. = FALSE
    )
  }
  hashes <- c(
    "source_sha256",
    "sampling_sha256",
    "ordered_indices_sha256",
    "cell_identity_sha256",
    "first_row_numeric_sha256",
    "block_numeric_sha256",
    "query_plan_sha256"
  )
  if (
    any(
      !vapply(
        plan[hashes],
        function(x) {
          is.character(x) &&
            length(x) == 1L &&
            !is.na(x) &&
            grepl(.bench_sha256_pattern, x)
        },
        logical(1L)
      )
    ) ||
      !identical(
        plan$query_plan_sha256,
        bench_sha256_object(plan[names(plan) != "query_plan_sha256"])
      )
  ) {
    stop(
      "frozen query plan self-hash is invalid for tier ",
      tier,
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.bench_frame_values_equal <- function(actual, expected) {
  if (
    !is.data.frame(actual) ||
      !is.data.frame(expected) ||
      !identical(names(actual), names(expected)) ||
      nrow(actual) != nrow(expected)
  ) {
    return(FALSE)
  }
  all(vapply(
    names(expected),
    function(name) {
      x <- actual[[name]]
      y <- expected[[name]]
      if (is.numeric(y)) {
        identical(is.na(x), is.na(y)) &&
          isTRUE(all.equal(
            as.double(x[!is.na(y)]),
            as.double(y[!is.na(y)]),
            tolerance = 1e-9,
            check.attributes = FALSE
          ))
      } else if (is.logical(y)) {
        identical(as.logical(x), y)
      } else {
        identical(as.character(x), as.character(y))
      }
    },
    logical(1L)
  ))
}

.bench_frozen_evidence_checks <- function(
  evidence,
  panel,
  config,
  schedule = NULL,
  exports = NULL,
  access = NULL
) {
  panel <- match.arg(panel, c("comparison", "full_scale"))
  if (!is.list(evidence) || !is.list(config)) {
    stop("frozen evidence inputs are invalid", call. = FALSE)
  }
  required <- c(
    "manifest",
    "source",
    "sampling",
    "eligibility",
    "queries",
    "plans"
  )
  if (!all(required %in% names(evidence))) {
    stop("frozen evidence is incomplete", call. = FALSE)
  }
  result <- list()
  run_check <- function(id, scope, expression) {
    error <- tryCatch(
      {
        force(expression)
        NULL
      },
      error = function(error) conditionMessage(error)
    )
    result[[length(result) + 1L]] <<- bench_validation_row(
      id,
      panel,
      scope,
      expected = "canonical frozen evidence",
      observed = if (is.null(error)) "canonical frozen evidence" else error,
      status = if (is.null(error)) "PASS" else "FAIL",
      detail = if (is.null(error)) NA_character_ else error
    )
  }
  manifest <- evidence$manifest
  source <- evidence$source
  sampling <- evidence$sampling
  eligibility <- evidence$eligibility
  queries <- evidence$queries
  plans <- evidence$plans

  run_check("frozen_manifest_contract", "manifest", {
    if (
      !is.data.frame(manifest) ||
        !identical(names(manifest), c("key", "value")) ||
        anyNA(manifest$key) ||
        anyDuplicated(manifest$key)
    ) {
      stop("manifest schema or keys are invalid")
    }
    runtime_packages <- sort(c(
      "CerebroNexus",
      "Matrix",
      "SeuratObject",
      "Seurat",
      "BPCells",
      "HDF5Array",
      "rhdf5",
      "digest",
      "callr"
    ))
    keys <- c(
      "git_sha",
      "git_dirty",
      "schema_version",
      "config_sha256",
      "runtime_sha256",
      "common_target_actual",
      "package_version",
      "package_path",
      "source_path",
      "harness_config_sha256",
      "harness_helpers_sha256",
      "harness_sha256",
      "r_version",
      "platform",
      paste0("package.", runtime_packages)
    )
    invisible(lapply(keys, function(key) .bench_manifest_value(manifest, key)))
    runtime_records <- vapply(
      runtime_packages,
      function(package) {
        .bench_manifest_value(manifest, paste0("package.", package))
      },
      character(1L)
    )
    split_records <- strsplit(runtime_records, "|", fixed = TRUE)
    if (
      any(lengths(split_records) != 2L) ||
        any(
          !vapply(
            split_records,
            function(record) all(nzchar(record)),
            logical(1L)
          )
        ) ||
        any(
          !vapply(
            split_records,
            function(record) startsWith(record[[2L]], .Platform$file.sep),
            logical(1L)
          )
        )
    ) {
      stop("manifest runtime package records are invalid")
    }
    versions <- vapply(split_records, `[[`, character(1L), 1L)
    package_paths <- vapply(split_records, `[[`, character(1L), 2L)
    runtime_expected <- bench_sha256_object(list(
      r_version = .bench_manifest_value(manifest, "r_version"),
      platform = .bench_manifest_value(manifest, "platform"),
      packages = paste(runtime_packages, versions, sep = "=")
    ))
    harness_payload <- .bench_harness_payload(
      .bench_manifest_value(manifest, "harness_config_sha256"),
      .bench_manifest_value(manifest, "harness_helpers_sha256")
    )
    harness_expected <- bench_sha256_object(harness_payload)
    if (
      !grepl("^[0-9a-f]{40}$", .bench_manifest_value(manifest, "git_sha")) ||
        !identical(.bench_manifest_value(manifest, "git_dirty"), "false") ||
        !identical(
          .bench_manifest_value(manifest, "schema_version"),
          as.character(config$schema_version)
        ) ||
        !identical(
          .bench_manifest_value(manifest, "config_sha256"),
          bench_sha256_object(config)
        ) ||
        !identical(
          .bench_manifest_value(manifest, "runtime_sha256"),
          runtime_expected
        ) ||
        !grepl(
          .bench_sha256_pattern,
          .bench_manifest_value(manifest, "harness_config_sha256")
        ) ||
        !grepl(
          .bench_sha256_pattern,
          .bench_manifest_value(manifest, "harness_helpers_sha256")
        ) ||
        !identical(
          .bench_manifest_value(manifest, "harness_sha256"),
          harness_expected
        ) ||
        !.bench_nonempty_absolute_path(.bench_manifest_value(
          manifest,
          "source_path"
        )) ||
        !nzchar(.bench_manifest_value(manifest, "package_version")) ||
        !startsWith(
          .bench_manifest_value(manifest, "package_path"),
          .Platform$file.sep
        ) ||
        !identical(
          .bench_manifest_value(manifest, "package_path"),
          unname(package_paths[[which(runtime_packages == "CerebroNexus")]])
        ) ||
        !identical(
          .bench_manifest_value(manifest, "package_version"),
          unname(versions[[which(runtime_packages == "CerebroNexus")]])
        )
    ) {
      stop("manifest provenance values differ from the current protocol")
    }
  })
  run_check("frozen_source_contract", "source", {
    schema <- c(
      "source_key",
      "source_url",
      "expected_bytes",
      "actual_bytes",
      "expected_sha256",
      "actual_sha256",
      "n_cells",
      "n_genes",
      "exact_nnz"
    )
    if (
      !is.data.frame(source) ||
        !identical(names(source), schema) ||
        nrow(source) != 1L ||
        !identical(source$source_key, config$source$key) ||
        !identical(source$source_url, config$source$url) ||
        !identical(
          as.double(source$expected_bytes),
          as.double(config$source$expected_bytes)
        ) ||
        !identical(
          as.double(source$actual_bytes),
          as.double(config$source$expected_bytes)
        ) ||
        !identical(source$expected_sha256, config$source$expected_sha256) ||
        !identical(source$actual_sha256, config$source$expected_sha256) ||
        !identical(
          as.integer(source$n_cells),
          as.integer(config$source$n_cells)
        ) ||
        !is.finite(source$n_genes) ||
        source$n_genes < 5 ||
        source$n_genes != floor(source$n_genes) ||
        !is.finite(source$exact_nnz) ||
        source$exact_nnz < 0 ||
        source$exact_nnz != floor(source$exact_nnz)
    ) {
      stop("source evidence differs from the pinned source identity")
    }
  })
  run_check("frozen_query_plan_contract", "query_plan", {
    expected_tiers <- if (panel == "comparison") {
      c(names(config$comparison_fixed_tiers), "common")
    } else {
      c("common", "tier_1m", "tier_2m", "full")
    }
    if (!is.list(plans) || !identical(names(plans), expected_tiers)) {
      stop("query plan tier order is invalid")
    }
    source_dimensions <- c(
      genes = as.integer(source$n_genes),
      cells = as.integer(source$n_cells)
    )
    invisible(lapply(expected_tiers, function(tier) {
      .bench_validate_frozen_plan(
        plans[[tier]],
        tier,
        config$source$expected_sha256,
        source_dimensions
      )
    }))
    if (
      !all(vapply(
        plans,
        function(plan) identical(plan$genes$gene, plans[[1L]]$genes$gene),
        logical(1L)
      ))
    ) {
      stop("ordered query genes differ across tiers")
    }
    common <- as.integer(.bench_manifest_value(
      manifest,
      "common_target_actual"
    ))
    if (
      length(common) != 1L ||
        is.na(common) ||
        common <= config$common_min_exclusive ||
        common > config$common_target
    ) {
      stop("common target is outside the frozen protocol bounds")
    }
    expected_sizes <- if (panel == "comparison") {
      c(config$comparison_fixed_tiers, common = common)
    } else {
      c(common = common, config$full_scale_fixed_tiers)
    }
    actual_sizes <- vapply(
      plans,
      function(plan) as.integer(plan$dimensions[["cells"]]),
      integer(1L)
    )
    if (!identical(unname(actual_sizes), as.integer(expected_sizes))) {
      stop("query plan tier sizes differ from protocol")
    }
  })
  run_check("frozen_sampling_contract", "sampling", {
    schema <- c(
      "tier_label",
      "n_cells",
      "stratum",
      "start",
      "end",
      "n",
      "exact_nnz",
      "indices_sha256",
      "cell_identity_sha256",
      "shell_sha256"
    )
    if (
      !is.data.frame(sampling) ||
        !identical(names(sampling), schema) ||
        nrow(sampling) != 4L * length(plans) ||
        !identical(sampling$tier_label, rep(names(plans), each = 4L))
    ) {
      stop("sampling schema/order is invalid")
    }
    for (tier in names(plans)) {
      rows <- sampling[sampling$tier_label == tier, , drop = FALSE]
      plan <- plans[[tier]]
      n <- as.integer(plan$dimensions[["cells"]])
      blocks <- bench_stratified_blocks(as.integer(source$n_cells), n)
      if (
        !identical(as.integer(rows$stratum), 1:4) ||
          !identical(as.integer(rows$n_cells), rep(n, 4L)) ||
          !identical(as.integer(rows$start), blocks$start) ||
          !identical(as.integer(rows$end), blocks$end) ||
          !identical(as.integer(rows$n), blocks$n) ||
          sum(as.double(rows$n)) != n ||
          length(unique(rows$exact_nnz)) != 1L ||
          !identical(
            unique(rows$indices_sha256),
            plan$ordered_indices_sha256
          ) ||
          !identical(
            unique(rows$cell_identity_sha256),
            plan$cell_identity_sha256
          ) ||
          length(unique(rows$shell_sha256)) != 1L ||
          is.na(unique(rows$shell_sha256)) ||
          !grepl(.bench_sha256_pattern, unique(rows$shell_sha256))
      ) {
        stop("sampling rows differ from query plan for tier ", tier)
      }
    }
  })
  run_check("frozen_queries_contract", "queries", {
    schema <- c(
      "schema",
      "tier_label",
      "source_sha256",
      "sampling_sha256",
      "n_genes",
      "n_cells",
      "gene",
      "role",
      "density",
      "source_row",
      "tie_break_rank",
      "ordered_indices_sha256",
      "cell_identity_sha256",
      "first_row_numeric_sha256",
      "block_numeric_sha256",
      "query_plan_sha256"
    )
    if (
      !is.data.frame(queries) ||
        !identical(names(queries), schema) ||
        nrow(queries) != 5L * length(plans) ||
        !identical(queries$tier_label, rep(names(plans), each = 5L))
    ) {
      stop("queries schema/order is invalid")
    }
    expected <- do.call(
      rbind,
      lapply(names(plans), function(tier) {
        plan <- plans[[tier]]
        data.frame(
          schema = plan$schema,
          tier_label = tier,
          source_sha256 = plan$source_sha256,
          sampling_sha256 = plan$sampling_sha256,
          n_genes = as.integer(plan$dimensions[["genes"]]),
          n_cells = as.integer(plan$dimensions[["cells"]]),
          gene = plan$genes$gene,
          role = plan$genes$role,
          density = as.double(plan$genes$density),
          source_row = as.integer(plan$genes$source_row),
          tie_break_rank = as.integer(plan$genes$tie_break_rank),
          ordered_indices_sha256 = plan$ordered_indices_sha256,
          cell_identity_sha256 = plan$cell_identity_sha256,
          first_row_numeric_sha256 = plan$first_row_numeric_sha256,
          block_numeric_sha256 = plan$block_numeric_sha256,
          query_plan_sha256 = plan$query_plan_sha256,
          stringsAsFactors = FALSE
        )
      })
    )
    rownames(expected) <- NULL
    if (!.bench_frame_values_equal(queries, expected)) {
      stop("queries rows differ from frozen plans")
    }
  })
  run_check("frozen_eligibility_contract", "eligibility", {
    exact_nnz <- setNames(
      vapply(
        names(plans),
        function(tier) {
          values <- unique(sampling$exact_nnz[sampling$tier_label == tier])
          if (length(values) != 1L) {
            stop("sampling exact_nnz is inconsistent")
          }
          as.double(values)
        },
        numeric(1L)
      ),
      names(plans)
    )
    tiers <- setNames(
      vapply(
        plans,
        function(plan) as.integer(plan$dimensions[["cells"]]),
        integer(1L)
      ),
      names(plans)
    )
    expected <- bench_eligibility(
      panel,
      tiers,
      exact_nnz,
      config$sparse_index_limit
    )
    if (
      !.bench_frame_values_equal(eligibility, expected) ||
        (panel == "comparison" && any(eligibility$status != "SCHEDULED")) ||
        (panel == "full_scale" &&
          (nrow(eligibility) != 12L ||
            sum(eligibility$status == "SCHEDULED") != 4L))
    ) {
      stop("eligibility does not exactly match the reconstructed protocol")
    }
  })
  run_check("frozen_schedule_contract", "schedule", {
    if (!is.null(schedule)) {
      expected_rows <- nrow(schedule)
      bench_validate_schedule(schedule, expected_rows)
      sizes <- setNames(
        vapply(
          plans,
          function(plan) as.integer(plan$dimensions[["cells"]]),
          integer(1L)
        ),
        names(plans)
      )
      if (any(as.integer(schedule$n_cells) != sizes[schedule$tier_label])) {
        stop("schedule tier sizes differ from plans")
      }
    }
  })
  run_check("frozen_package_origin_contract", "package_origin", {
    if (!is.null(exports) || !is.null(access)) {
      expected_path <- .bench_manifest_value(manifest, "package_path")
      paths <- c(exports$package_path, access$package_path)
      expected_count <- 2L * nrow(schedule)
      if (
        length(paths) != expected_count ||
          anyNA(paths) ||
          any(paths != expected_path)
      ) {
        stop("worker package paths differ from manifest package_path")
      }
    }
  })
  rows <- do.call(rbind, result)
  rownames(rows) <- NULL
  rows
}

.bench_assert_frozen_evidence <- function(
  evidence,
  panel,
  config,
  schedule = NULL,
  exports = NULL,
  access = NULL
) {
  checks <- .bench_frozen_evidence_checks(
    evidence,
    panel,
    config,
    schedule,
    exports,
    access
  )
  failed <- checks[checks$status != "PASS", , drop = FALSE]
  if (nrow(failed)) {
    stop(paste(failed$detail, collapse = "; "), call. = FALSE)
  }
  invisible(checks)
}

bench_validate_panel_a_evidence <- function(panel_a_dir) {
  directory <- normalizePath(
    .bench_scalar_string(panel_a_dir, "panel_a_dir"),
    mustWork = TRUE
  )
  if (!dir.exists(directory) || .bench_is_symlink(panel_a_dir)) {
    stop("Panel A evidence directory is unsafe", call. = FALSE)
  }
  paths <- setNames(
    file.path(directory, .bench_panel_a_required_files),
    .bench_panel_a_required_files
  )
  if (
    any(!file.exists(paths)) ||
      any(vapply(paths, .bench_is_symlink, logical(1L)))
  ) {
    stop("Panel A evidence is incomplete or unsafe", call. = FALSE)
  }
  pre_hash <- .bench_hash_evidence_paths(paths)
  read_window_stable <- FALSE
  on.exit(
    {
      if (!read_window_stable) {
        changed <- tryCatch(
          !identical(.bench_hash_evidence_paths(paths), pre_hash),
          error = function(error) TRUE
        )
        if (changed) {
          stop("Panel A evidence changed while being read", call. = FALSE)
        }
      }
    },
    add = TRUE
  )
  schedule <- .bench_read_csv(
    paths[["schedule.csv"]],
    c(
      "pair_id",
      "panel",
      "repeat",
      "tier_label",
      "n_cells",
      "backend",
      "export_order",
      "access_order"
    )
  )
  .bench_panel_a_after_first_read(paths)
  schedule[["repeat"]] <- as.integer(schedule[["repeat"]])
  schedule$n_cells <- as.integer(schedule$n_cells)
  schedule$export_order <- as.integer(schedule$export_order)
  schedule$access_order <- as.integer(schedule$access_order)
  expected_pairs <- nrow(schedule)
  bench_validate_schedule(schedule, expected_pairs)
  if (!identical(unique(schedule$panel), "comparison")) {
    stop("Panel A schedule panel is invalid", call. = FALSE)
  }
  exports <- .bench_read_typed_outcome(paths[["export.csv"]], "export")
  accesses <- .bench_read_typed_outcome(paths[["access.csv"]], "access")
  if (
    !identical(names(exports), bench_export_schema()) ||
      !identical(names(accesses), bench_access_schema())
  ) {
    stop("Panel A raw outcome schema is invalid", call. = FALSE)
  }
  if (
    nrow(exports) != expected_pairs ||
      nrow(accesses) != expected_pairs ||
      anyDuplicated(exports$pair_id) ||
      anyDuplicated(accesses$pair_id) ||
      !setequal(exports$pair_id, schedule$pair_id) ||
      !setequal(accesses$pair_id, schedule$pair_id) ||
      any(exports$status != "OK") ||
      any(accesses$status != "OK") ||
      any(accesses$correctness != "PASS")
  ) {
    stop(
      "Panel A requires one successful export/access pair for every scheduled condition",
      call. = FALSE
    )
  }
  package_paths <- c(exports$package_path, accesses$package_path)
  if (
    anyNA(package_paths) ||
      any(!nzchar(package_paths)) ||
      length(unique(package_paths)) != 1L
  ) {
    stop(
      "Panel A worker package origins are missing or inconsistent",
      call. = FALSE
    )
  }
  validation <- .bench_read_typed_validation(paths[["validation.csv"]])
  if (
    sum(validation$check_id == "panel_valid") != 1L ||
      !identical(tail(validation$check_id, 1L), "panel_valid") ||
      !identical(tail(validation$status, 1L), "VALID") ||
      any(validation$status[-nrow(validation)] != "PASS")
  ) {
    stop("Panel A final validation is not uniquely VALID", call. = FALSE)
  }
  plans <- readRDS(paths[["query-plan.rds"]])
  queries <- .bench_read_csv(paths[["queries.csv"]])
  sampling <- .bench_read_csv(paths[["sampling.csv"]])
  eligibility <- .bench_read_csv(paths[["eligibility.csv"]])
  manifest <- .bench_read_csv(paths[["manifest.csv"]])
  source <- .bench_read_csv(paths[["source.csv"]])
  if (
    !identical(names(manifest), c("key", "value")) ||
      anyDuplicated(manifest$key) ||
      !all(
        c(
          "git_sha",
          "schema_version",
          "config_sha256",
          "runtime_sha256",
          "common_target_actual"
        ) %in%
          manifest$key
      )
  ) {
    stop("Panel A manifest schema is invalid", call. = FALSE)
  }
  if (
    !grepl("^[0-9a-f]{40}$", .bench_manifest_value(manifest, "git_sha")) ||
      !grepl(
        .bench_sha256_pattern,
        .bench_manifest_value(manifest, "config_sha256")
      ) ||
      !grepl(
        .bench_sha256_pattern,
        .bench_manifest_value(manifest, "runtime_sha256")
      )
  ) {
    stop("Panel A manifest provenance values are invalid", call. = FALSE)
  }
  if (
    !identical(
      names(source),
      c(
        "source_key",
        "source_url",
        "expected_bytes",
        "actual_bytes",
        "expected_sha256",
        "actual_sha256",
        "n_cells",
        "n_genes",
        "exact_nnz"
      )
    ) ||
      nrow(source) != 1L
  ) {
    stop("Panel A source schema is invalid", call. = FALSE)
  }
  if (
    !identical(source$expected_sha256, source$actual_sha256) ||
      !identical(
        as.double(source$expected_bytes),
        as.double(source$actual_bytes)
      )
  ) {
    stop(
      "Panel A source identity did not match the pinned source",
      call. = FALSE
    )
  }
  if (exists("BENCH_CONFIG", inherits = TRUE)) {
    pinned <- get("BENCH_CONFIG", inherits = TRUE)$source
    if (
      !identical(source$source_key, pinned$key) ||
        !identical(source$source_url, pinned$url) ||
        !identical(
          as.double(source$actual_bytes),
          as.double(pinned$expected_bytes)
        ) ||
        !identical(source$actual_sha256, pinned$expected_sha256) ||
        !identical(as.integer(source$n_cells), as.integer(pinned$n_cells))
    ) {
      stop(
        "Panel A source evidence differs from current BENCH_CONFIG",
        call. = FALSE
      )
    }
  }
  if (
    !identical(
      names(sampling),
      c(
        "tier_label",
        "n_cells",
        "stratum",
        "start",
        "end",
        "n",
        "exact_nnz",
        "indices_sha256",
        "cell_identity_sha256",
        "shell_sha256"
      )
    )
  ) {
    stop("Panel A sampling schema is invalid", call. = FALSE)
  }
  if (
    !identical(
      names(eligibility),
      c(
        "panel",
        "tier_label",
        "n_cells",
        "backend",
        "exact_nnz",
        "status",
        "reason"
      )
    ) ||
      any(eligibility$panel != "comparison") ||
      nrow(eligibility) !=
        length(unique(schedule$tier_label)) *
          length(unique(schedule$backend)) ||
      any(eligibility$status != "SCHEDULED")
  ) {
    stop("Panel A eligibility schema or gates are invalid", call. = FALSE)
  }
  if (
    !identical(
      names(queries),
      c(
        "schema",
        "tier_label",
        "source_sha256",
        "sampling_sha256",
        "n_genes",
        "n_cells",
        "gene",
        "role",
        "density",
        "source_row",
        "tie_break_rank",
        "ordered_indices_sha256",
        "cell_identity_sha256",
        "first_row_numeric_sha256",
        "block_numeric_sha256",
        "query_plan_sha256"
      )
    )
  ) {
    stop("Panel A query schema is invalid", call. = FALSE)
  }
  common <- plans[["common"]]
  if (is.null(common) || !is.list(common)) {
    stop("Panel A common query plan is missing", call. = FALSE)
  }
  if (
    !identical(
      as.character(common$dimensions[["cells"]]),
      .bench_manifest_value(manifest, "common_target_actual")
    )
  ) {
    stop(
      "Panel A common target differs between manifest and query plan",
      call. = FALSE
    )
  }
  plan_tiers <- unique(schedule$tier_label)
  if (!setequal(names(plans), plan_tiers)) {
    stop("Panel A query-plan tier set is invalid", call. = FALSE)
  }
  invisible(lapply(plan_tiers, function(tier) {
    .bench_validate_frozen_plan(plans[[tier]], tier, source$actual_sha256[[1L]])
  }))
  for (i in seq_len(nrow(schedule))) {
    id <- schedule$pair_id[[i]]
    plan <- plans[[schedule$tier_label[[i]]]]
    a <- accesses[accesses$pair_id == id, , drop = FALSE]
    e <- exports[exports$pair_id == id, , drop = FALSE]
    shell <- unique(sampling$shell_sha256[
      sampling$tier_label == schedule$tier_label[[i]]
    ])
    if (
      length(shell) != 1L ||
        !identical(e$shell_sha256, shell) ||
        !identical(a$expected_row_fingerprint, plan$first_row_numeric_sha256) ||
        !identical(a$observed_row_fingerprint, plan$first_row_numeric_sha256) ||
        !identical(a$expected_block_fingerprint, plan$block_numeric_sha256) ||
        !identical(a$observed_block_fingerprint, plan$block_numeric_sha256)
    ) {
      stop(
        "Panel A worker fingerprints do not match frozen evidence",
        call. = FALSE
      )
    }
  }
  for (tier in plan_tiers) {
    plan <- plans[[tier]]
    sample_rows <- sampling[sampling$tier_label == tier, , drop = FALSE]
    query_rows <- queries[queries$tier_label == tier, , drop = FALSE]
    if (
      !identical(
        as.character(plan$genes$gene),
        as.character(common$genes$gene)
      ) ||
        nrow(sample_rows) != 4L ||
        length(unique(sample_rows$indices_sha256)) != 1L ||
        !identical(
          unique(sample_rows$indices_sha256),
          plan$ordered_indices_sha256
        ) ||
        !identical(
          unique(sample_rows$cell_identity_sha256),
          plan$cell_identity_sha256
        ) ||
        nrow(query_rows) != 5L ||
        !identical(unique(query_rows$query_plan_sha256), plan$query_plan_sha256)
    ) {
      stop(
        "Panel A sampling/query fingerprints do not match frozen plans",
        call. = FALSE
      )
    }
  }
  frozen_evidence <- list(
    manifest = manifest,
    source = source,
    sampling = sampling,
    eligibility = eligibility,
    queries = queries,
    plans = plans,
    config = get("BENCH_CONFIG", inherits = TRUE)
  )
  .bench_assert_frozen_evidence(
    frozen_evidence,
    "comparison",
    frozen_evidence$config,
    schedule,
    exports,
    accesses
  )
  canonical_validation <- bench_validate_panel(
    schedule,
    eligibility,
    exports,
    accesses,
    sampling,
    plans,
    linkage = NULL,
    evidence = frozen_evidence
  )
  if (!.bench_frame_values_equal(validation, canonical_validation)) {
    stop(
      "saved Panel A validation does not exactly match recomputed canonical gates",
      call. = FALSE
    )
  }
  post_hash <- .bench_hash_evidence_paths(paths)
  if (!identical(post_hash, pre_hash)) {
    stop("Panel A evidence changed while being read", call. = FALSE)
  }
  read_window_stable <- TRUE
  frozen_hashes <- pre_hash
  list(
    directory = directory,
    paths = paths,
    frozen_hashes = frozen_hashes,
    schedule = schedule,
    exports = exports,
    access = accesses,
    validation = validation,
    manifest = manifest,
    source = source,
    sampling = sampling,
    queries = queries,
    plans = plans,
    genes = common$genes,
    canonical_validation = canonical_validation,
    common_target_actual = as.integer(common$dimensions[["cells"]])
  )
}

.bench_manifest_value <- function(rows, key) {
  if (all(c("key", "value") %in% names(rows))) {
    value <- rows$value[rows$key == key]
  } else if (key %in% names(rows) && nrow(rows) == 1L) {
    value <- rows[[key]]
  } else {
    value <- character()
  }
  if (length(value) != 1L || is.na(value)) {
    stop("manifest key is missing: ", key, call. = FALSE)
  }
  as.character(value)
}

bench_validate_panel_a_linkage <- function(panel_a_dir, current_context) {
  evidence <- if (is.list(panel_a_dir) && !is.null(panel_a_dir$frozen_hashes)) {
    panel_a_dir
  } else {
    bench_validate_panel_a_evidence(panel_a_dir)
  }
  if (!is.list(current_context)) {
    stop("current_context must be a list", call. = FALSE)
  }
  if (
    any(vapply(
      names(evidence$frozen_hashes),
      function(name) {
        !identical(
          bench_sha256_file(evidence$paths[[name]]),
          evidence$frozen_hashes[[name]]
        )
      },
      logical(1L)
    ))
  ) {
    stop("Panel A evidence changed during validation", call. = FALSE)
  }
  expected <- list(
    source_sha256 = if ("actual_sha256" %in% names(evidence$source)) {
      evidence$source$actual_sha256[[1L]]
    } else {
      .bench_manifest_value(evidence$source, "source_sha256")
    },
    git_sha = .bench_manifest_value(evidence$manifest, "git_sha"),
    schema_version = .bench_manifest_value(evidence$manifest, "schema_version"),
    config_sha256 = .bench_manifest_value(evidence$manifest, "config_sha256"),
    runtime_sha256 = .bench_manifest_value(evidence$manifest, "runtime_sha256"),
    harness_sha256 = .bench_manifest_value(evidence$manifest, "harness_sha256"),
    common_target_actual = as.character(evidence$common_target_actual),
    common_sampling_sha256 = unique(evidence$plans$common$sampling_sha256),
    common_shell_sha256 = unique(evidence$sampling$shell_sha256[
      evidence$sampling$tier_label == "common"
    ]),
    common_cell_identity_sha256 = evidence$plans$common$cell_identity_sha256,
    common_query_plan_sha256 = evidence$plans$common$query_plan_sha256
  )
  aliases <- list(
    common_cell_identity_sha256 = c(
      "common_cell_identity_sha256",
      "cell_identity_sha256"
    ),
    common_query_plan_sha256 = c(
      "common_query_plan_sha256",
      "query_plan_sha256"
    )
  )
  checks <- lapply(names(expected), function(key) {
    candidates <- c(key, aliases[[key]])
    observed <- NULL
    for (candidate in candidates) {
      if (!is.null(current_context[[candidate]])) {
        observed <- current_context[[candidate]]
        break
      }
    }
    pass <- length(observed) == 1L &&
      !is.na(observed) &&
      identical(as.character(observed), as.character(expected[[key]]))
    bench_validation_row(
      paste0("linkage_", key),
      "full_scale",
      "linkage",
      as.character(expected[[key]]),
      if (length(observed)) as.character(observed[[1L]]) else "absent",
      if (pass) "PASS" else "FAIL",
      if (pass) NA_character_ else "Panel A linkage mismatch"
    )
  })
  rows <- do.call(rbind, checks)
  if (any(rows$status != "PASS")) {
    stop("Panel A linkage validation failed", call. = FALSE)
  }
  rows
}

.bench_assert_frozen_snapshot <- function(evidence) {
  if (
    !is.list(evidence) ||
      is.null(evidence$frozen_hashes) ||
      is.null(evidence$paths) ||
      !identical(names(evidence$frozen_hashes), names(evidence$paths))
  ) {
    stop("frozen evidence snapshot is invalid", call. = FALSE)
  }
  changed <- names(evidence$frozen_hashes)[vapply(
    names(evidence$frozen_hashes),
    function(name) {
      path <- evidence$paths[[name]]
      !file.exists(path) ||
        .bench_is_symlink(path) ||
        !identical(bench_sha256_file(path), evidence$frozen_hashes[[name]])
    },
    logical(1L)
  )]
  if (length(changed)) {
    stop(
      "Panel A frozen evidence changed: ",
      paste(changed, collapse = ","),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.bench_local_evidence_names <- c(
  "manifest.csv",
  "source.csv",
  "sampling.csv",
  "eligibility.csv",
  "queries.csv",
  "query-plan.rds",
  "schedule.csv"
)

.bench_snapshot_local_evidence <- function(output) {
  output <- normalizePath(
    .bench_scalar_string(output, "output"),
    mustWork = TRUE
  )
  if (
    .bench_is_symlink(output) ||
      !file.exists(file.path(output, ".cerebro-benchmark-run"))
  ) {
    stop("local evidence output is unsafe", call. = FALSE)
  }
  paths <- setNames(
    file.path(output, .bench_local_evidence_names),
    .bench_local_evidence_names
  )
  hashes <- .bench_hash_evidence_paths(paths)
  list(output = output, paths = paths, hashes = hashes)
}

.bench_assert_local_evidence_snapshot <- function(snapshot) {
  if (
    !is.list(snapshot) ||
      !identical(names(snapshot), c("output", "paths", "hashes")) ||
      .bench_is_symlink(snapshot$output) ||
      !dir.exists(snapshot$output) ||
      !identical(names(snapshot$paths), .bench_local_evidence_names) ||
      !identical(names(snapshot$hashes), .bench_local_evidence_names) ||
      !file.exists(file.path(snapshot$output, ".cerebro-benchmark-run")) ||
      .bench_is_symlink(file.path(snapshot$output, ".cerebro-benchmark-run")) ||
      any(vapply(
        snapshot$paths,
        function(path) {
          !identical(
            dirname(normalizePath(path, mustWork = TRUE)),
            snapshot$output
          )
        },
        logical(1L)
      ))
  ) {
    stop("local evidence snapshot is invalid", call. = FALSE)
  }
  observed <- .bench_hash_evidence_paths(snapshot$paths)
  if (!identical(observed, snapshot$hashes)) {
    stop("local frozen evidence changed", call. = FALSE)
  }
  invisible(TRUE)
}

.bench_integrity_condition <- function(stage, error) {
  message <- paste0(stage, ": ", conditionMessage(error))
  structure(
    list(message = message, call = NULL, stage = stage, parent = error),
    class = c("bench_integrity_error", "error", "condition")
  )
}

.bench_integrity_guard <- function(stage, expression) {
  tryCatch(force(expression), error = function(error) {
    if (inherits(error, "bench_integrity_error")) {
      stop(error)
    }
    stop(.bench_integrity_condition(
      .bench_scalar_string(stage, "integrity stage"),
      error
    ))
  })
}

.bench_assert_measurement_integrity <- function(run_context, stage) {
  .bench_integrity_guard(stage, {
    if (!is.null(run_context$harness_snapshot)) {
      bench_assert_frozen_harness(run_context$harness_snapshot)
    }
    if (!is.null(run_context$source_snapshot)) {
      .bench_assert_source_identity(run_context$source_snapshot)
    }
    if (!is.null(run_context$local_evidence_snapshot)) {
      .bench_assert_local_evidence_snapshot(run_context$local_evidence_snapshot)
    }
    invisible(TRUE)
  })
}

.bench_write_integrity_failure_evidence <- function(output, panel, error) {
  integrity <- inherits(error, "bench_integrity_error")
  stage <- if (!is.null(error$stage)) {
    as.character(error$stage)
  } else {
    "runner_failure"
  }
  detail <- gsub("[\r\n]+", " ", conditionMessage(error))
  checks <- rbind(
    bench_validation_row(
      paste0(if (integrity) "integrity_failure:" else "runner_failure:", stage),
      panel,
      if (integrity) "integrity" else "runner",
      expected = if (integrity) {
        "frozen source, harness, and control evidence"
      } else {
        "measured runner and cleanup complete without error"
      },
      observed = detail,
      status = "FAIL",
      detail = detail
    ),
    bench_validation_row(
      "panel_valid",
      panel,
      "panel",
      expected = "all required gates PASS",
      observed = "measured runner or integrity failed",
      status = "INVALID",
      detail = detail
    )
  )
  tryCatch(
    {
      bench_write_validation(file.path(output, "validation.csv"), checks)
      TRUE
    },
    error = function(write_error) {
      message(
        "could not write integrity failure validation: ",
        conditionMessage(write_error)
      )
      FALSE
    }
  )
}

.bench_with_integrity_failure_evidence <- function(output, panel, expression) {
  tryCatch(force(expression), error = function(error) {
    .bench_write_integrity_failure_evidence(output, panel, error)
    stop(error)
  })
}

.bench_read_local_evidence_snapshot <- function(snapshot) {
  .bench_assert_local_evidence_snapshot(snapshot)
  paths <- snapshot$paths
  schedule <- .bench_read_csv(
    paths[["schedule.csv"]],
    c(
      "pair_id",
      "panel",
      "repeat",
      "tier_label",
      "n_cells",
      "backend",
      "export_order",
      "access_order"
    )
  )
  schedule[["repeat"]] <- as.integer(schedule[["repeat"]])
  schedule$n_cells <- as.integer(schedule$n_cells)
  schedule$export_order <- as.integer(schedule$export_order)
  schedule$access_order <- as.integer(schedule$access_order)
  evidence <- list(
    manifest = .bench_read_csv(paths[["manifest.csv"]]),
    source = .bench_read_csv(paths[["source.csv"]]),
    sampling = .bench_read_csv(paths[["sampling.csv"]]),
    eligibility = .bench_read_csv(paths[["eligibility.csv"]]),
    queries = .bench_read_csv(paths[["queries.csv"]]),
    plans = readRDS(paths[["query-plan.rds"]])
  )
  .bench_assert_local_evidence_snapshot(snapshot)
  list(schedule = schedule, evidence = evidence)
}

.bench_manifest_rows <- function(environment, setup) {
  config <- get("BENCH_CONFIG", inherits = TRUE)
  values <- c(
    schema_version = as.character(config$schema_version),
    config_sha256 = bench_sha256_object(config),
    git_sha = environment$git_sha,
    git_dirty = environment$git_dirty,
    command = environment$command,
    started_utc = environment$started_utc,
    r_version = environment$r_version,
    platform = environment$platform,
    os = environment$os,
    cpu = environment$cpu,
    physical_memory_bytes = environment$physical_memory_bytes,
    setup_completed_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    setup_elapsed_secs = as.character(setup$setup_elapsed_secs),
    setup_peak_rss_mb = as.character(setup$setup_peak_rss_mb),
    setup_r_heap_peak_mb = as.character(setup$setup_r_heap_peak_mb),
    runtime_sha256 = setup$runtime_sha256,
    common_target_actual = as.character(setup$common_target_actual),
    source_path = setup$source_path,
    harness_config_sha256 = setup$harness_config_sha256,
    harness_helpers_sha256 = setup$harness_helpers_sha256,
    harness_sha256 = setup$harness_sha256,
    package_version = setup$package_version,
    package_path = setup$package_path
  )
  base <- data.frame(
    key = names(values),
    value = unname(values),
    stringsAsFactors = FALSE
  )
  runtime <- data.frame(
    key = paste0("package.", setup$runtime$package),
    value = paste(setup$runtime$version, setup$runtime$package_path, sep = "|"),
    stringsAsFactors = FALSE
  )
  rbind(base, runtime)
}

.bench_remove_run_library <- function(paths) {
  library <- normalizePath(paths$library, mustWork = TRUE)
  scratch <- normalizePath(paths$scratch, mustWork = TRUE)
  if (
    !identical(dirname(library), scratch) ||
      !file.exists(file.path(library, ".cerebro-benchmark-library")) ||
      !file.exists(file.path(scratch, ".cerebro-benchmark-scratch"))
  ) {
    stop("refusing unsafe run-library cleanup", call. = FALSE)
  }
  unlink(library, recursive = TRUE, force = FALSE)
  if (dir.exists(library)) {
    stop("run-library cleanup failed", call. = FALSE)
  }
  invisible(TRUE)
}

.bench_outcome_bind <- function(rows, phase) {
  if (!length(rows)) {
    return(bench_empty_outcome(phase, ".prototype")[FALSE, ])
  }
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

.bench_append_parent_collector_diagnostic <- function(
  result,
  pair_id,
  log_path,
  logs_root
) {
  if (!is.list(result) || !is.null(result$row)) {
    return(invisible(FALSE))
  }
  pair_id <- .bench_scalar_string(pair_id, "pair_id")
  log_path <- .bench_scalar_string(log_path, "log_path")
  appended <- tryCatch(
    {
      raw_logs_root <- .bench_scalar_string(logs_root, "logs_root")
      raw_log_parent <- dirname(log_path)
      if (
        .bench_is_symlink(raw_logs_root) ||
          !dir.exists(raw_logs_root) ||
          .bench_is_symlink(raw_log_parent) ||
          !dir.exists(raw_log_parent)
      ) {
        stop(
          "collector logs root and log parent must be real directories",
          call. = FALSE
        )
      }
      logs_root <- normalizePath(raw_logs_root, mustWork = TRUE)
      log_parent <- normalizePath(raw_log_parent, mustWork = TRUE)
      if (
        !identical(log_parent, logs_root) ||
          .bench_is_symlink(log_path) ||
          dir.exists(log_path)
      ) {
        stop(
          "collector log path is outside the real logs directory",
          call. = FALSE
        )
      }
      diagnostic <- result$diagnostic
      if (
        !is.character(diagnostic) ||
          length(diagnostic) != 1L ||
          is.na(diagnostic) ||
          !nzchar(diagnostic)
      ) {
        diagnostic <- "collector returned NULL row without a diagnostic"
      }
      diagnostic <- gsub("\r\n?", "\n", diagnostic)
      diagnostic_lines <- strsplit(diagnostic, "\n", fixed = TRUE)[[1L]]
      exit_status <- result$exit_status
      if (
        !is.numeric(exit_status) ||
          length(exit_status) != 1L ||
          !is.finite(exit_status)
      ) {
        exit_status <- NA_integer_
      } else {
        exit_status <- as.integer(exit_status)
      }
      lines <- c(
        "=== PARENT COLLECTOR DIAGNOSTIC ===",
        paste0("pair_id: ", pair_id),
        paste0("exit_status: ", if (is.na(exit_status)) "NA" else exit_status),
        paste0("diagnostic: ", diagnostic_lines),
        "=== END PARENT COLLECTOR DIAGNOSTIC ==="
      )
      payload <- paste0("\n", paste(lines, collapse = "\n"), "\n")
      cat(payload, file = log_path, append = TRUE)
      TRUE
    },
    error = function(error) {
      message(
        "could not append parent collector diagnostic for ",
        pair_id,
        ": ",
        conditionMessage(error)
      )
      FALSE
    }
  )
  invisible(appended)
}

.bench_run_measured_schedule <- function(
  schedule,
  plans,
  source_path,
  paths,
  run_context
) {
  exports <- list()
  accesses <- list()
  job_dirs <- list()
  export_path <- file.path(paths$output, "export.csv")
  access_path <- file.path(paths$output, "access.csv")
  .bench_atomic_write_csv(export_path, .bench_outcome_bind(exports, "export"))
  .bench_atomic_write_csv(access_path, .bench_outcome_bind(accesses, "access"))
  for (repeat_id in sort(unique(schedule[["repeat"]]))) {
    block <- schedule[schedule[["repeat"]] == repeat_id, , drop = FALSE]
    for (index in order(block$export_order)) {
      row <- block[index, , drop = FALSE]
      .bench_assert_measurement_integrity(
        run_context,
        paste0("before_export:", row$pair_id)
      )
      id <- gsub("[^A-Za-z0-9_.-]", "_", row$pair_id)
      job_dir <- bench_make_job_dir(paths$scratch, id)
      job_dirs[[row$pair_id]] <- job_dir
      job <- list(
        pair_id = row$pair_id,
        phase = "export",
        panel = row$panel,
        n_cells = row$n_cells,
        backend = row$backend,
        source_path = source_path,
        source_group = BENCH_CONFIG$source$group,
        artifact_path = file.path(job_dir, "artifact.crb"),
        job_dir = job_dir
      )
      result <- bench_run_worker(
        "bench_export_worker",
        job,
        run_context,
        file.path(paths$logs, paste0(id, "-export.log")),
        BENCH_CONFIG$rss_poll_ms
      )
      .bench_append_parent_collector_diagnostic(
        result,
        row$pair_id,
        file.path(paths$logs, paste0(id, "-export.log")),
        paths$logs
      )
      exports[[row$pair_id]] <- result$row
      .bench_atomic_write_csv(
        export_path,
        .bench_outcome_bind(exports, "export")
      )
      .bench_assert_measurement_integrity(
        run_context,
        paste0("after_export:", row$pair_id)
      )
      if (!identical(result$row$status, "OK")) {
        bench_remove_job_dir(job_dir, paths$scratch)
        job_dirs[[row$pair_id]] <- NULL
      }
    }
    for (index in order(block$access_order)) {
      row <- block[index, , drop = FALSE]
      .bench_assert_measurement_integrity(
        run_context,
        paste0("before_access:", row$pair_id)
      )
      id <- gsub("[^A-Za-z0-9_.-]", "_", row$pair_id)
      export <- exports[[row$pair_id]]
      job <- list(
        pair_id = row$pair_id,
        phase = "access",
        panel = row$panel,
        backend = row$backend,
        artifact_path = file.path(job_dirs[[row$pair_id]], "artifact.crb"),
        job_dir = job_dirs[[row$pair_id]],
        query_plan = plans[[row$tier_label]]
      )
      if (!identical(export$status, "OK")) {
        result_row <- bench_not_run_access_row(job)
        result_row$log_path <- file.path(paths$logs, paste0(id, "-access.log"))
      } else {
        bench_write_stage(job$job_dir, "startup")
        access_result <- bench_run_worker(
          "bench_access_worker",
          job,
          run_context,
          file.path(paths$logs, paste0(id, "-access.log")),
          BENCH_CONFIG$rss_poll_ms
        )
        .bench_append_parent_collector_diagnostic(
          access_result,
          row$pair_id,
          file.path(paths$logs, paste0(id, "-access.log")),
          paths$logs
        )
        result_row <- access_result$row
      }
      accesses[[row$pair_id]] <- result_row
      .bench_atomic_write_csv(
        access_path,
        .bench_outcome_bind(accesses, "access")
      )
      .bench_assert_measurement_integrity(
        run_context,
        paste0("after_access:", row$pair_id)
      )
      if (!is.null(job_dirs[[row$pair_id]])) {
        bench_remove_job_dir(job_dirs[[row$pair_id]], paths$scratch)
      }
    }
  }
  list(
    exports = .bench_outcome_bind(exports, "export"),
    access = .bench_outcome_bind(accesses, "access")
  )
}

bench_run_panel <- function(panel, parsed, repo, bench_root, command) {
  panel <- match.arg(panel, c("comparison", "full_scale"))
  output <- bench_validate_output_candidate(
    parsed$output_path,
    repo,
    if (panel == "full_scale") parsed$panel_a_dir else NULL
  )
  environment <- bench_record_environment(repo, command)
  imported <- NULL
  if (panel == "full_scale") {
    # Static evidence is deserialized only after output and clean-tree gates, but before creation.
    imported <- bench_validate_panel_a_evidence(parsed$panel_a_dir)
    .bench_assert_frozen_snapshot(imported)
  }
  paths <- bench_prepare_output(output)
  on.exit(
    if (dir.exists(paths$library)) {
      try(.bench_remove_run_library(paths), silent = TRUE)
    },
    add = TRUE
  )
  harness_snapshot <- bench_freeze_harness(bench_root, paths$scratch)
  bench_assert_frozen_harness(harness_snapshot)
  bench_assert_environment_unchanged(environment)
  install_log <- file.path(paths$logs, "install.log")
  bench_install_tree(repo, paths$library, install_log)
  run_context <- list(
    bench_root = harness_snapshot$root,
    library = paths$library,
    output = paths$output,
    git_sha = environment$git_sha,
    harness_snapshot = harness_snapshot
  )
  if (panel == "full_scale") {
    .bench_assert_frozen_snapshot(imported)
  }
  bench_assert_frozen_harness(harness_snapshot)
  setup_run <- bench_run_setup_worker(
    panel,
    parsed$source_path,
    imported,
    run_context,
    file.path(paths$logs, "setup.log")
  )
  setup <- setup_run$value
  setup$setup_elapsed_secs <- setup_run$elapsed_secs
  setup$setup_peak_rss_mb <- setup_run$peak_rss_mb
  setup$setup_r_heap_peak_mb <- setup_run$r_heap_peak_mb
  setup$harness_config_sha256 <- harness_snapshot$payload$config_sha256
  setup$harness_helpers_sha256 <- harness_snapshot$payload$helpers_sha256
  setup$harness_sha256 <- harness_snapshot$payload$harness_sha256
  source_snapshot <- list(
    path = setup$source_path,
    bytes = as.double(setup$source_bytes),
    sha256 = setup$source_sha256,
    identity = setup$source_identity
  )
  run_context$source_snapshot <- source_snapshot
  bench_assert_frozen_harness(harness_snapshot)
  .bench_assert_source_identity(source_snapshot)
  eligibility <- .bench_read_csv(file.path(paths$output, "eligibility.csv"))
  if (panel == "comparison" && !all(eligibility$status == "SCHEDULED")) {
    stop("Panel A exact nnz gates rejected a scheduled tier", call. = FALSE)
  }
  .bench_atomic_write_csv(
    file.path(paths$output, "manifest.csv"),
    .bench_manifest_rows(environment, setup)
  )
  if (panel == "comparison") {
    tiers <- setup$tiers
    schedule <- bench_comparison_schedule(
      tiers,
      BENCH_CONFIG$comparison_backends,
      BENCH_CONFIG$comparison_repeats
    )
    linkage <- NULL
  } else {
    common_plan <- readRDS(file.path(paths$output, "query-plan.rds"))$common
    common_sampling <- setup$sampling[
      setup$sampling$tier_label == "common",
      ,
      drop = FALSE
    ]
    current <- list(
      source_sha256 = setup$source_sha256,
      git_sha = environment$git_sha,
      schema_version = as.character(BENCH_CONFIG$schema_version),
      config_sha256 = bench_sha256_object(BENCH_CONFIG),
      runtime_sha256 = setup$runtime_sha256,
      harness_sha256 = harness_snapshot$payload$harness_sha256,
      common_target_actual = setup$common_target_actual,
      common_sampling_sha256 = common_plan$sampling_sha256,
      common_shell_sha256 = unique(common_sampling$shell_sha256),
      common_cell_identity_sha256 = common_plan$cell_identity_sha256,
      common_query_plan_sha256 = common_plan$query_plan_sha256
    )
    .bench_assert_frozen_snapshot(imported)
    linkage <- bench_validate_panel_a_linkage(imported, current)
    tiers <- setup$tiers
    schedule <- bench_full_schedule(tiers, BENCH_CONFIG$full_scale_repeats)
  }
  .bench_atomic_write_csv(file.path(paths$output, "schedule.csv"), schedule)
  plans <- readRDS(file.path(paths$output, "query-plan.rds"))
  frozen_evidence <- list(
    manifest = .bench_read_csv(file.path(paths$output, "manifest.csv")),
    source = .bench_read_csv(file.path(paths$output, "source.csv")),
    sampling = .bench_read_csv(file.path(paths$output, "sampling.csv")),
    eligibility = eligibility,
    queries = .bench_read_csv(file.path(paths$output, "queries.csv")),
    plans = plans,
    config = BENCH_CONFIG
  )
  .bench_assert_frozen_evidence(frozen_evidence, panel, BENCH_CONFIG, schedule)
  local_evidence_snapshot <- .bench_snapshot_local_evidence(paths$output)
  run_context$local_evidence_snapshot <- local_evidence_snapshot
  completed <- .bench_with_integrity_failure_evidence(paths$output, panel, {
    .bench_assert_measurement_integrity(run_context, "before_measured_schedule")
    if (panel == "full_scale") {
      .bench_integrity_guard(
        "before_measured_panel_a",
        .bench_assert_frozen_snapshot(imported)
      )
    }
    measured <- .bench_run_measured_schedule(
      schedule,
      plans,
      source_snapshot$path,
      paths,
      run_context
    )
    if (panel == "full_scale") {
      .bench_integrity_guard(
        "after_measured_panel_a",
        .bench_assert_frozen_snapshot(imported)
      )
    }
    .bench_assert_measurement_integrity(run_context, "after_measured_schedule")
    .bench_integrity_guard(
      "final_source_sha256",
      .bench_assert_source_snapshot(source_snapshot)
    )
    final_disk <- .bench_integrity_guard(
      "final_control_evidence_read",
      .bench_read_local_evidence_snapshot(local_evidence_snapshot)
    )
    final_evidence <- final_disk$evidence
    final_evidence$config <- BENCH_CONFIG
    .bench_integrity_guard(
      "final_control_evidence_contract",
      .bench_assert_frozen_evidence(
        final_evidence,
        panel,
        BENCH_CONFIG,
        final_disk$schedule
      )
    )
    validation <- bench_validate_panel(
      final_disk$schedule,
      final_evidence$eligibility,
      measured$exports,
      measured$access,
      final_evidence$sampling,
      final_evidence$plans,
      linkage,
      evidence = final_evidence
    )
    .bench_remove_run_library(paths)
    .bench_integrity_guard(
      "final_git_state",
      bench_assert_environment_unchanged(environment)
    )
    if (panel == "full_scale") {
      .bench_integrity_guard(
        "before_validation_panel_a",
        .bench_assert_frozen_snapshot(imported)
      )
    }
    .bench_assert_measurement_integrity(run_context, "before_validation_write")
    bench_write_validation(
      file.path(paths$output, "validation.csv"),
      validation
    )
    .bench_assert_measurement_integrity(run_context, "after_validation_write")
    if (panel == "full_scale") {
      .bench_integrity_guard(
        "after_validation_panel_a",
        .bench_assert_frozen_snapshot(imported)
      )
    }
    list(measured = measured, validation = validation)
  })
  validation <- completed$validation
  if (!identical(tail(validation$status, 1L), "VALID")) {
    stop("panel validation is INVALID", call. = FALSE)
  }
  invisible(paths$output)
}
