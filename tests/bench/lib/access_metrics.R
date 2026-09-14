# Runtime access measurement helpers.

bench_build_query_plan <- function(m, n_genes = 50L) {
  genes <- rownames(m)
  if (is.null(genes) || anyNA(genes) || any(!nzchar(genes))) {
    stop("the benchmark matrix must have non-empty gene names", call. = FALSE)
  }
  counts <- tabulate(m@i + 1L, nbins = nrow(m))
  panel <- bench_stratified_gene_panel(genes, counts, n_genes = n_genes)

  first_values <- as.numeric(m[panel$gene[1], ])
  block_values <- as.matrix(m[panel$gene, , drop = FALSE])
  subset_cells <- bench_subset_cells(ncol(m))
  plan <- list(
    schema_version = 2L,
    n_cells = ncol(m),
    n_genes = nrow(m),
    nnz = sum(counts),
    panel = panel,
    subset_cells = subset_cells,
    subset_cells_fingerprint = bench_numeric_fingerprint(subset_cells),
    reference_row_fingerprint = bench_numeric_fingerprint(first_values),
    reference_block_fingerprint = bench_numeric_fingerprint(block_values),
    reference_subset_row_fingerprint = bench_numeric_fingerprint(
      first_values[subset_cells]
    ),
    reference_subset_block_fingerprint = bench_numeric_fingerprint(
      block_values[, subset_cells, drop = FALSE]
    )
  )
  plan$query_plan_fingerprint <- bench_serialized_fingerprint(plan)
  plan
}

bench_default_timer <- function(fn) {
  started <- proc.time()[["elapsed"]]
  value <- fn()
  list(
    seconds = unname(proc.time()[["elapsed"]] - started),
    value = value
  )
}

bench_measure_backend <- function(
  obj,
  plan,
  hot_iterations = 5L,
  timer = bench_default_timer
) {
  if (!identical(plan$schema_version, 2L)) {
    stop("unsupported query-plan schema", call. = FALSE)
  }
  panel <- plan$panel
  first_gene <- panel$gene[panel$role == "first"]
  if (length(first_gene) != 1L) {
    stop("query plan must contain exactly one first gene", call. = FALSE)
  }

  first <- timer(function() obj$getExpressionRow(first_gene))
  row_fingerprint <- bench_numeric_fingerprint(first$value)
  if (!identical(row_fingerprint, plan$reference_row_fingerprint)) {
    stop("first-query fingerprint mismatch", call. = FALSE)
  }

  hot_genes <- panel$gene[panel$role == "hot"]
  if (!length(hot_genes)) {
    hot_genes <- first_gene
  }
  invisible(lapply(hot_genes, obj$getExpressionRow))
  hot_order <- rep(hot_genes, times = max(1L, as.integer(hot_iterations)))
  hot_secs <- vapply(
    hot_order,
    function(gene) timer(function() obj$getExpressionRow(gene))$seconds,
    numeric(1)
  )

  block <- timer(function() obj$getExpressionBlock(panel$gene))
  block_fingerprint <- bench_numeric_fingerprint(block$value)
  if (!identical(block_fingerprint, plan$reference_block_fingerprint)) {
    stop("block fingerprint mismatch", call. = FALSE)
  }

  subset_row <- timer(function() {
    obj$getExpressionRow(first_gene, cells = plan$subset_cells)
  })
  subset_row_fingerprint <- bench_numeric_fingerprint(subset_row$value)
  if (
    !identical(
      subset_row_fingerprint,
      plan$reference_subset_row_fingerprint
    )
  ) {
    stop("subset-row fingerprint mismatch", call. = FALSE)
  }

  subset_block <- timer(function() {
    obj$getExpressionBlock(panel$gene, cells = plan$subset_cells)
  })
  subset_block_fingerprint <- bench_numeric_fingerprint(subset_block$value)
  if (
    !identical(
      subset_block_fingerprint,
      plan$reference_subset_block_fingerprint
    )
  ) {
    stop("subset-block fingerprint mismatch", call. = FALSE)
  }

  list(
    first_query_secs = first$seconds,
    hot_p50_secs = stats::quantile(hot_secs, 0.5, names = FALSE),
    hot_p95_secs = stats::quantile(hot_secs, 0.95, names = FALSE),
    block_secs = block$seconds,
    subset_row_secs = subset_row$seconds,
    subset_block_secs = subset_block$seconds,
    subset_n_cells = length(plan$subset_cells),
    n_hot = length(hot_secs),
    correctness = "OK",
    row_fingerprint = row_fingerprint,
    reference_row_fingerprint = plan$reference_row_fingerprint,
    block_fingerprint = block_fingerprint,
    reference_block_fingerprint = plan$reference_block_fingerprint,
    subset_row_fingerprint = subset_row_fingerprint,
    reference_subset_row_fingerprint = plan$reference_subset_row_fingerprint,
    subset_block_fingerprint = subset_block_fingerprint,
    reference_subset_block_fingerprint = plan$reference_subset_block_fingerprint,
    query_plan_fingerprint = plan$query_plan_fingerprint
  )
}
