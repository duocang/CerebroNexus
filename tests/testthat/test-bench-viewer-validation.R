bench_viewer <- file.path("..", "bench", "lib", "viewer_validation.R")

test_that("Viewer validation drives a standalone App end to end", {
  skip_if_not(file.exists(bench_viewer), "benchmark tree not present")
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  chrome <- tryCatch(chromote::find_chrome(), error = function(error) "")
  skip_if_not(nzchar(chrome), "Chrome is unavailable")
  source(bench_viewer, local = TRUE)

  crb <- system.file(
    "extdata",
    "examples",
    "example.crb",
    package = "CerebroNexus"
  )
  skip_if_not(file.exists(crb), "installed example CRB is unavailable")
  root <- withr::local_tempdir()

  result <- bench_run_viewer_validation(
    crb = crb,
    app_dir = file.path(root, "app"),
    gene = "MS4A1",
    timeout = 60000
  )

  expect_identical(result$correctness, "OK")
  expect_true(is.character(result$browser) && nzchar(result$browser))
  timings <- c(
    "bundle_secs",
    "launch_secs",
    "hover_secs",
    "selection_secs",
    "zoom_secs",
    "gene_secs"
  )
  expect_true(all(timings %in% names(result)))
  expect_true(all(is.finite(unlist(result[timings]))))
  expect_true(all(unlist(result[timings]) >= 0))
})
