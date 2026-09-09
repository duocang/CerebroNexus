bench_viewer <- file.path("..", "bench", "lib", "viewer_validation.R")
bench_full_source <- file.path("..", "bench", "lib", "full_source.R")

viewer_ring_fixture <- function(
  root,
  write_backend,
  make_shell,
  n_cells = 7000L
) {
  matrix <- Matrix::sparseMatrix(
    i = rep(seq_len(3L), n_cells),
    j = rep(seq_len(n_cells), each = 3L),
    x = rep(c(1, 2, 3), n_cells),
    dims = c(3L, n_cells),
    dimnames = list(
      paste0("g", seq_len(3L)),
      paste0("c", seq_len(n_cells))
    )
  )
  matrix <- BPCells::write_matrix_memory(matrix)
  sibling <- file.path(root, "bench.bpcells")
  crb <- file.path(root, "bench.crb")
  write_backend(matrix, "bpcells", sibling)
  object <- make_shell(
    matrix,
    backend = "bpcells",
    location = basename(sibling),
    source_name = "ring fixture",
    organism = "mm10",
    run_id = "ring-fixture"
  )
  saveRDS(object, crb, version = 3)
  crb
}

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

test_that("Viewer selection follows rendered cells in the C2 ring projection", {
  skip_if_not(file.exists(bench_viewer), "benchmark tree not present")
  skip_if_not(file.exists(bench_full_source), "benchmark tree not present")
  skip_if_not_installed("BPCells")
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  chrome <- tryCatch(chromote::find_chrome(), error = function(error) "")
  skip_if_not(nzchar(chrome), "Chrome is unavailable")
  source(bench_viewer, local = TRUE)
  source(bench_full_source, local = TRUE)

  root <- withr::local_tempdir()
  crb <- viewer_ring_fixture(
    root,
    bench_write_full_backend,
    bench_make_full_shell
  )
  result <- bench_run_viewer_validation(
    crb = crb,
    app_dir = file.path(root, "app"),
    gene = "g1",
    timeout = 30000
  )

  expect_identical(result$correctness, "OK")
})

test_that("Viewer log validation catches standard Shiny errors", {
  skip_if_not(file.exists(bench_viewer), "benchmark tree not present")
  source(bench_viewer, local = TRUE)

  logs <- data.frame(
    location = "shiny",
    level = "stderr",
    message = "Warning: Error in renderText: fixture failure"
  )

  expect_true(bench_viewer_bad_logs(logs))
})
