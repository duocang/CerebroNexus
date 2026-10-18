bench_protocol <- file.path(
  "..",
  "benchmarks",
  "expression-backends",
  "scripts",
  "benchmark.R"
)
bench_access_metrics <- bench_protocol

skip_unless_bench_access <- function() {
  testthat::skip_if_not(
    file.exists(bench_protocol) && file.exists(bench_access_metrics),
    "benchmark tree not present (expected when checking a built package)"
  )
  testthat::skip_if_not_installed("Matrix")
}

test_that("query plans stratify genes and fingerprint source values", {
  skip_unless_bench_access()
  source(bench_protocol, local = TRUE)
  source(bench_access_metrics, local = TRUE)

  m <- Matrix::sparseMatrix(
    i = c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4),
    j = c(1, 1, 2, 1, 2, 3, 1, 2, 3, 4),
    x = seq_len(10),
    dims = c(4, 4),
    dimnames = list(paste0("g", 1:4), paste0("c", 1:4))
  )
  plan <- bench_build_query_plan(m, n_genes = 3L)

  expect_equal(plan$schema_version, 2L)
  expect_equal(plan$n_cells, 4L)
  expect_equal(nrow(plan$panel), 3L)
  expect_equal(plan$panel$role[1], "first")
  expect_identical(plan$subset_cells, 4:1)
  expect_identical(
    plan$reference_row_fingerprint,
    bench_numeric_fingerprint(m[plan$panel$gene[1], ])
  )
  expect_identical(
    plan$reference_block_fingerprint,
    bench_numeric_fingerprint(as.matrix(m[plan$panel$gene, , drop = FALSE]))
  )
  expect_identical(
    plan$reference_subset_block_fingerprint,
    bench_numeric_fingerprint(
      as.matrix(m[plan$panel$gene, plan$subset_cells, drop = FALSE])
    )
  )
})

test_that("Linux peak RSS reads the process high-water mark", {
  skip_unless_bench_access()
  source(bench_protocol, local = TRUE)
  status <- tempfile("proc-status-")
  on.exit(unlink(status), add = TRUE)
  writeLines(c("Name:\tR", "VmRSS:\t1024 kB", "VmHWM:\t4096 kB"), status)

  expect_equal(bench_peak_rss_mb(status), 4)
  expect_true(is.na(bench_peak_rss_mb(paste0(status, "-missing"))))
})

test_that("the first backend call is the timed fresh-process query", {
  skip_unless_bench_access()
  source(bench_protocol, local = TRUE)
  source(bench_access_metrics, local = TRUE)

  rows <- list(
    median = c(0, 2, 0, 4),
    sparse = c(1, 0, 0, 0),
    dense = c(1, 2, 3, 4)
  )
  panel <- data.frame(
    gene = c("median", "sparse", "dense"),
    nnz = c(2, 1, 4),
    role = c("first", "hot", "hot"),
    stringsAsFactors = FALSE
  )
  plan <- list(
    schema_version = 2L,
    panel = panel,
    subset_cells = c(4L, 2L),
    reference_row_fingerprint = bench_numeric_fingerprint(rows$median),
    reference_block_fingerprint = bench_numeric_fingerprint(
      rbind(rows$median, rows$sparse, rows$dense)
    ),
    reference_subset_row_fingerprint = bench_numeric_fingerprint(
      rows$median[c(4L, 2L)]
    ),
    reference_subset_block_fingerprint = bench_numeric_fingerprint(
      rbind(rows$median, rows$sparse, rows$dense)[, c(4L, 2L), drop = FALSE]
    )
  )

  calls <- character()
  obj <- new.env(parent = emptyenv())
  obj$getExpressionRow <- function(gene, cells = NULL) {
    calls <<- c(
      calls,
      paste0("row:", gene, if (is.null(cells)) ":all" else ":subset")
    )
    values <- rows[[gene]]
    if (is.null(cells)) values else values[cells]
  }
  obj$getExpressionBlock <- function(genes, cells = NULL) {
    calls <<- c(calls, paste0("block:", paste(genes, collapse = ",")))
    values <- do.call(rbind, rows[genes])
    if (is.null(cells)) values else values[, cells, drop = FALSE]
  }
  timer <- function(fn) list(seconds = 0.01, value = fn())

  got <- bench_measure_backend(
    obj,
    plan,
    hot_iterations = 2L,
    timer = timer
  )

  expect_equal(calls[1], "row:median:all")
  expect_equal(got$first_query_secs, 0.01)
  expect_equal(got$n_hot, 4L)
  expect_equal(got$correctness, "OK")
  expect_identical(got$row_fingerprint, plan$reference_row_fingerprint)
  expect_identical(got$block_fingerprint, plan$reference_block_fingerprint)
  expect_equal(got$subset_row_secs, 0.01)
  expect_equal(got$subset_block_secs, 0.01)
  expect_identical(
    got$subset_block_fingerprint,
    plan$reference_subset_block_fingerprint
  )
})

test_that("a full collection settles the heap outside every timed read", {
  skip_unless_bench_access()
  source(bench_protocol, local = TRUE)

  rows <- list(a = c(1, 0, 2), b = c(0, 3, 0))
  plan <- list(
    schema_version = 2L,
    panel = data.frame(
      gene = c("a", "b"),
      nnz = c(2, 1),
      role = c("first", "hot"),
      stringsAsFactors = FALSE
    ),
    subset_cells = c(3L, 1L),
    reference_row_fingerprint = bench_numeric_fingerprint(rows$a),
    reference_block_fingerprint = bench_numeric_fingerprint(
      rbind(rows$a, rows$b)
    ),
    reference_subset_row_fingerprint = bench_numeric_fingerprint(
      rows$a[c(3L, 1L)]
    ),
    reference_subset_block_fingerprint = bench_numeric_fingerprint(
      rbind(rows$a, rows$b)[, c(3L, 1L), drop = FALSE]
    )
  )
  events <- character()
  obj <- list(
    getExpressionRow = function(gene, cells = NULL) {
      if (is.null(cells)) rows[[gene]] else rows[[gene]][cells]
    },
    getExpressionBlock = function(genes, cells = NULL) {
      values <- do.call(rbind, rows[genes])
      if (is.null(cells)) values else values[, cells, drop = FALSE]
    }
  )
  timer <- function(fn) {
    events <<- c(events, "start")
    value <- fn()
    events <<- c(events, "stop")
    list(seconds = 0.01, value = value)
  }
  record <- function(timing) {
    events[length(events)] <<- paste0("stop:", timing$phase)
  }

  bench_measure_backend(
    obj,
    plan,
    hot_iterations = 2L,
    timer = timer,
    record = record,
    settle = function() events <<- c(events, "settle")
  )

  # No collection is requested while a timer runs, the first read and every
  # one-off read follow a fresh collection, and the warmed loop gets one.
  expect_identical(
    events,
    c(
      "settle", "start", "stop:first",
      "settle", rep(c("start", "stop:hot"), 2L),
      "settle", "start", "stop:block",
      "settle", "start", "stop:subset_row",
      "settle", "start", "stop:subset_block"
    )
  )
  expect_identical(formals(bench_measure_backend)$settle, quote(bench_settle_heap))
  expect_invisible(bench_settle_heap())
})

test_that("backend measurements fail closed on wrong values", {
  skip_unless_bench_access()
  source(bench_protocol, local = TRUE)
  source(bench_access_metrics, local = TRUE)

  obj <- new.env(parent = emptyenv())
  obj$getExpressionRow <- function(gene, cells = NULL) c(1, 2)
  obj$getExpressionBlock <- function(genes, cells = NULL) {
    matrix(c(1, 2), nrow = 1)
  }
  plan <- list(
    schema_version = 2L,
    panel = data.frame(
      gene = "g1",
      nnz = 2,
      role = "first",
      stringsAsFactors = FALSE
    ),
    subset_cells = 2:1,
    reference_row_fingerprint = "wrong-row",
    reference_block_fingerprint = "wrong-block",
    reference_subset_row_fingerprint = "wrong-subset-row",
    reference_subset_block_fingerprint = "wrong-subset-block"
  )

  expect_error(
    bench_measure_backend(
      obj,
      plan,
      hot_iterations = 1L,
      timer = function(fn) list(seconds = 0, value = fn())
    ),
    "first-query fingerprint mismatch"
  )
})

test_that("fingerprints materialize BPCells blocks canonically", {
  skip_unless_bench_access()
  testthat::skip_if_not_installed("BPCells")
  source(bench_protocol, local = TRUE)

  matrix_dir <- tempfile("bench-bpcells-")
  on.exit(unlink(matrix_dir, recursive = TRUE), add = TRUE)
  sparse <- Matrix::Matrix(
    matrix(c(0, 1, 2, 0, 3, 4), nrow = 2),
    sparse = TRUE
  )
  dimnames(sparse) <- list(c("g1", "g2"), c("c1", "c2", "c3"))
  BPCells::write_matrix_dir(
    methods::as(sparse, "IterableMatrix"),
    matrix_dir
  )
  block <- BPCells::open_matrix_dir(matrix_dir)[1:2, , drop = FALSE]

  expect_identical(
    bench_numeric_fingerprint(block),
    bench_numeric_fingerprint(as.matrix(block))
  )
})

test_that("lazy block reads are materialized before the timer stops", {
  skip_unless_bench_access()
  testthat::skip_if_not_installed("BPCells")
  source(bench_protocol, local = TRUE)
  m <- Matrix::Matrix(matrix(c(1, 0, 3, 4, 0, 6), nrow = 2), sparse = TRUE)
  dimnames(m) <- list(c("g1", "g2"), paste0("c", 1:3))
  lazy <- BPCells::write_matrix_memory(m)
  plan <- bench_build_query_plan(m, n_genes = 2L)
  obj <- list(
    getExpressionRow = function(gene, cells = NULL) {
      if (is.null(cells)) {
        cells <- seq_len(ncol(m))
      }
      as.numeric(m[gene, cells])
    },
    getExpressionBlock = function(genes, cells = NULL) {
      if (is.null(cells)) {
        cells <- seq_len(ncol(m))
      }
      lazy[genes, cells, drop = FALSE]
    }
  )
  block_reads <- 0L
  timer <- function(fn) {
    value <- fn()
    expect_true(is.numeric(value))
    expect_false(inherits(value, "IterableMatrix"))
    if (is.matrix(value)) {
      block_reads <<- block_reads + 1L
    }
    list(seconds = 0.01, value = value)
  }
  result <- bench_measure_backend(obj, plan, 1L, timer)
  expect_equal(block_reads, 2L)
  expect_identical(result$access_contract, "materialized-v2")
  expect_identical(result$correctness, "OK")
})


test_that("monotonic timings survive wall-clock corrections and fail closed", {
  skip_unless_bench_access()
  source(bench_protocol, local = TRUE)
  readings <- list(
    c(monotonic = 10, wall = 100, resolution = 1e-9),
    c(monotonic = 10.25, wall = 90, resolution = 1e-9)
  )
  index <- 0L
  clock <- function() {
    index <<- index + 1L
    readings[[index]]
  }
  got <- bench_default_timer(function() 42, clock = clock)
  expect_equal(got$seconds, 0.25)
  expect_equal(got$value, 42)
  expect_lt(got$timing$wall_end, got$timing$wall_start)
  records <- list()
  for (bad in c(-1, NA_real_, Inf, NaN)) {
    expect_error(
      bench_timed(
        function() 42,
        timer = function(fn) list(seconds = bad, value = fn()),
        record = function(row) records[[length(records) + 1L]] <<- row
      ),
      "invalid elapsed time"
    )
  }
  expect_length(records, 4L)
})

test_that("native monotonic clock measures actual waiting", {
  skip_unless_bench_access()
  source(bench_protocol, local = TRUE)
  got <- bench_timed(function() {
    Sys.sleep(0.02)
    "done"
  })
  expect_equal(got$value, "done")
  expect_gte(got$seconds, 0.015)
  expect_identical(got$timing$clock, "CLOCK_MONOTONIC")
  expect_gt(got$timing$resolution_secs, 0)
})
