test_that("Ren benchmark workers reject different values, shapes, and backends", {
  skip_if_not_installed("devtools")
  root <- withr::local_tempdir()
  script <- file.path(
    viewer_app_test_path(),
    "..",
    "tests",
    "bench",
    "benchmark_ren_expression_backends.R"
  )
  skip_if_not(
    file.exists(script),
    "benchmark workers require a source checkout"
  )
  script <- normalizePath(script)
  crb <- file.path(root, "tiny.crb")
  plan_path <- file.path(root, "plan.rds")
  result_path <- file.path(root, "result.rds")
  object <- Cerebro$new()
  values <- matrix(
    seq_len(60),
    nrow = 15,
    dimnames = list(paste0("g", 1:15), paste0("c", 1:4))
  )
  object$expression <- values
  saveRDS(object, crb)
  worker <- function(...) {
    log <- file.path(root, "worker.log")
    status <- system2(
      file.path(R.home("bin"), "Rscript"),
      shQuote(c(script, ...)),
      stdout = log,
      stderr = log
    )
    list(status = status, log = paste(readLines(log), collapse = "\n"))
  }
  planned <- worker("--plan", crb, plan_path)
  expect_identical(planned$status, 0L, info = planned$log)
  plan <- readRDS(plan_path)
  expect_identical(plan$cells, 4L)
  expect_identical(plan$genes, 15L)
  expect_length(plan$panel, 12L)
  expect_true(all(plan$panel %in% rownames(values)))
  measure <- function(backend = "embedded") {
    worker("--measure", crb, backend, plan_path, "1", result_path)
  }
  good <- measure()
  expect_identical(good$status, 0L, info = good$log)
  result <- readRDS(result_path)
  expect_identical(result$correctness, "OK")
  expect_true(all(unlist(result[grepl("seconds$", names(result))]) >= 0))
  # Change first-row and block-only values separately; either must fail.
  for (gene in plan$panel[c(1, 2)]) {
    object$expression <- values
    object$expression[gene, 1] <- -99
    saveRDS(object, crb)
    bad <- measure()
    expect_identical(bad$status, 2L, info = bad$log)
    expect_identical(readRDS(result_path)$correctness, "MISMATCH")
  }
  wrong_backend <- measure("h5")
  expect_gt(wrong_backend$status, 0L)
  expect_match(
    wrong_backend$log,
    "expected h5 CRB, found embedded",
    fixed = TRUE
  )
  object$expression <- values[, -1, drop = FALSE]
  saveRDS(object, crb)
  wrong_shape <- measure()
  expect_gt(wrong_shape$status, 0L)
  expect_match(wrong_shape$log, "different dimensions", fixed = TRUE)
})
