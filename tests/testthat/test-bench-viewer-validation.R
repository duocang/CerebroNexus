bench_viewer <- file.path("..", "bench", "lib", "viewer_validation.R")
bench_full_source <- file.path("..", "bench", "lib", "full_source.R")

viewer_ring_fixture <- function(
  root,
  write_backend,
  make_shell,
  n_cells = 1000L
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
  CerebroNexus::saveCerebro(object, crb)
  crb
}

test_that("Chromium WebGPU flag is added without forcing Vulkan ANGLE", {
  skip_if_not(file.exists(bench_viewer), "benchmark tree not present")
  skip_if_not_installed("chromote")
  source(bench_viewer, local = TRUE)
  old <- chromote::get_chrome_args()
  on.exit(chromote::set_chrome_args(old), add = TRUE)

  chromote::set_chrome_args(setdiff(old, "--enable-unsafe-webgpu"))
  args <- bench_enable_chromium_webgpu()

  expect_true("--enable-unsafe-webgpu" %in% args)
  expect_setequal(setdiff(args, old), "--enable-unsafe-webgpu")
})

test_that("WebGPU preflight probes an independent local HTTP origin", {
  skip_if_not(file.exists(bench_viewer), "benchmark tree not present")
  source(bench_viewer, local = TRUE)
  body <- paste(deparse(body(bench_check_webgpu_adapter)), collapse = "\n")

  expect_match(body, "callr::r_bg", fixed = TRUE)
  expect_match(body, "httpuv::runServer", fixed = TRUE)
  expect_match(body, "http://127.0.0.1", fixed = TRUE)
  expect_match(body, "isSecureContext", fixed = TRUE)
})

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
    "gene_secs",
    "linked_secs"
  )
  expect_true(all(timings %in% names(result)))
  expect_true(all(is.finite(unlist(result[timings]))))
  expect_true(all(unlist(result[timings]) >= 0))
  expect_true(is.finite(result$rendered_point_count))
  expect_true(result$renderer_backend %in% c("canvas2d", "webgpu"))
  expect_false(result$renderer_context_lost)
  expect_identical(result$renderer_error, "")
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
    expected_cells = 1000L,
    timeout = 30000
  )

  expect_identical(result$correctness, "OK")
  expect_equal(result$rendered_point_count, 1000)
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
