bench_protocol <- file.path("..", "..", "lib", "protocol.R")
bench_access_metrics <- file.path("..", "..", "lib", "access_metrics.R")

skip_unless_bench_access <- function() {
  testthat::skip_if_not(
    file.exists(bench_protocol) && file.exists(bench_access_metrics),
    "benchmark source tree is incomplete"
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

  expect_equal(plan$schema_version, 1L)
  expect_equal(plan$n_cells, 4L)
  expect_equal(nrow(plan$panel), 3L)
  expect_equal(plan$panel$role[1], "first")
  expect_identical(
    plan$reference_row_fingerprint,
    bench_numeric_fingerprint(m[plan$panel$gene[1], ])
  )
  expect_identical(
    plan$reference_block_fingerprint,
    bench_numeric_fingerprint(as.matrix(m[plan$panel$gene, , drop = FALSE]))
  )
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
    schema_version = 1L,
    panel = panel,
    reference_row_fingerprint = bench_numeric_fingerprint(rows$median),
    reference_block_fingerprint = bench_numeric_fingerprint(
      rbind(rows$median, rows$sparse, rows$dense)
    )
  )

  calls <- character()
  obj <- new.env(parent = emptyenv())
  obj$getExpressionRow <- function(gene) {
    calls <<- c(calls, paste0("row:", gene))
    rows[[gene]]
  }
  obj$getExpressionBlock <- function(genes) {
    calls <<- c(calls, paste0("block:", paste(genes, collapse = ",")))
    do.call(rbind, rows[genes])
  }
  timer <- function(fn) list(seconds = 0.01, value = fn())

  got <- bench_measure_backend(
    obj,
    plan,
    hot_iterations = 2L,
    timer = timer
  )

  expect_equal(calls[1], "row:median")
  expect_equal(got$first_query_secs, 0.01)
  expect_equal(got$n_hot, 4L)
  expect_equal(got$correctness, "OK")
  expect_identical(got$row_fingerprint, plan$reference_row_fingerprint)
  expect_identical(got$block_fingerprint, plan$reference_block_fingerprint)
})

test_that("backend measurements fail closed on wrong values", {
  skip_unless_bench_access()
  source(bench_protocol, local = TRUE)
  source(bench_access_metrics, local = TRUE)

  obj <- new.env(parent = emptyenv())
  obj$getExpressionRow <- function(gene) c(1, 2)
  obj$getExpressionBlock <- function(genes) matrix(c(1, 2), nrow = 1)
  plan <- list(
    schema_version = 1L,
    panel = data.frame(
      gene = "g1",
      nnz = 2,
      role = "first",
      stringsAsFactors = FALSE
    ),
    reference_row_fingerprint = "wrong-row",
    reference_block_fingerprint = "wrong-block"
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

test_that("block readiness times native preparation and materialization", {
  skip_unless_bench_access()
  source(bench_protocol, local = TRUE)
  source(bench_access_metrics, local = TRUE)

  timer_active <- FALSE
  materialization <- new.env(parent = emptyenv())
  materialization$count <- 0L
  as.matrix.bench_lazy_block <- function(x, ...) {
    if (!isTRUE(x$timer_active())) {
      stop("lazy block materialized outside the benchmark timer")
    }
    x$materialization$count <- x$materialization$count + 1L
    x$value
  }
  dim.bench_lazy_block <- function(x) dim(x$value)

  values <- list(
    first = c(0, 2, 0, 4),
    hot = c(1, 0, 3, 0)
  )
  block_values <- rbind(values$first, values$hot)
  lazy_block <- structure(
    list(
      value = block_values,
      materialization = materialization,
      timer_active = function() timer_active
    ),
    class = "bench_lazy_block"
  )
  obj <- new.env(parent = emptyenv())
  obj$getExpressionRow <- function(gene) values[[gene]]
  obj$getExpressionBlock <- function(genes) lazy_block
  panel <- data.frame(
    gene = c("first", "hot"),
    nnz = c(2, 2),
    role = c("first", "hot"),
    stringsAsFactors = FALSE
  )
  plan <- list(
    schema_version = 1L,
    panel = panel,
    reference_row_fingerprint = bench_numeric_fingerprint(values$first),
    reference_block_fingerprint = bench_numeric_fingerprint(block_values),
    query_plan_fingerprint = "plan"
  )
  timer_call <- 0L
  timer <- function(fn) {
    timer_call <<- timer_call + 1L
    timer_active <<- TRUE
    on.exit(timer_active <<- FALSE, add = TRUE)
    value <- fn()
    timer_active <<- FALSE
    list(seconds = timer_call / 100, value = value)
  }

  got <- bench_measure_backend(obj, plan, hot_iterations = 1L, timer = timer)

  expect_equal(materialization$count, 1L)
  expect_true(is.finite(got$block_prepare_secs))
  expect_true(is.finite(got$block_materialize_secs))
  expect_equal(
    got$block_ready_secs,
    got$block_prepare_secs + got$block_materialize_secs
  )
  expect_identical(got$block_fingerprint, plan$reference_block_fingerprint)
})
