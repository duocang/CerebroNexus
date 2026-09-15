# Run a disposable end-to-end Viewer smoke test before the full benchmark.

here <- Sys.getenv("BENCH_ROOT", "")
if (!nzchar(here)) {
  here <- normalizePath("tests/bench")
}
if (nzchar(Sys.getenv("BENCH_LIB"))) {
  .libPaths(c(Sys.getenv("BENCH_LIB"), .libPaths()))
}
source(file.path(here, "lib", "viewer_validation.R"))
suppressPackageStartupMessages(library(CerebroNexus))

crb <- system.file(
  "extdata", "examples", "example.crb", package = "CerebroNexus"
)
if (!file.exists(crb)) {
  stop("installed Viewer smoke-test fixture is unavailable", call. = FALSE)
}

smoke_root <- tempfile("cerebro-viewer-smoke-")
dir.create(smoke_root, recursive = TRUE)
on.exit(unlink(smoke_root, recursive = TRUE, force = TRUE), add = TRUE)

metrics <- bench_run_viewer_validation(
  crb = crb,
  app_dir = file.path(smoke_root, "app"),
  gene = "MS4A1",
  require_webgpu = FALSE,
  timeout = 60000
)
if (!identical(metrics$correctness, "OK")) {
  stop("Viewer smoke test did not complete correctly", call. = FALSE)
}
message(
  "Viewer smoke test OK: launch, render, hover, selection, zoom, gene, ",
  "and linked views succeeded (renderer=", metrics$renderer_backend, ")"
)
