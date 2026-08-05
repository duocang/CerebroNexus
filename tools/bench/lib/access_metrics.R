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
  plan <- list(
    schema_version = 1L,
    n_cells = ncol(m),
    n_genes = nrow(m),
    panel = panel,
    reference_row_fingerprint = bench_numeric_fingerprint(first_values),
    reference_block_fingerprint = bench_numeric_fingerprint(block_values)
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
  if (!identical(plan$schema_version, 1L)) {
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
  materialized_block <- timer(function() as.matrix(block$value))
  block_fingerprint <- bench_numeric_fingerprint(materialized_block$value)
  if (!identical(block_fingerprint, plan$reference_block_fingerprint)) {
    stop("block fingerprint mismatch", call. = FALSE)
  }

  list(
    first_query_secs = first$seconds,
    hot_p50_secs = stats::quantile(hot_secs, 0.5, names = FALSE),
    hot_p95_secs = stats::quantile(hot_secs, 0.95, names = FALSE),
    block_prepare_secs = block$seconds,
    block_materialize_secs = materialized_block$seconds,
    block_ready_secs = block$seconds + materialized_block$seconds,
    n_hot = length(hot_secs),
    correctness = "OK",
    row_fingerprint = row_fingerprint,
    reference_row_fingerprint = plan$reference_row_fingerprint,
    block_fingerprint = block_fingerprint,
    reference_block_fingerprint = plan$reference_block_fingerprint,
    query_plan_fingerprint = plan$query_plan_fingerprint
  )
}
