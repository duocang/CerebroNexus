# Fail fast when a C2 publication run cannot obtain a WebGPU adapter.

here <- Sys.getenv("BENCH_ROOT", "")
if (!nzchar(here)) {
  here <- normalizePath("tests/bench")
}
if (nzchar(Sys.getenv("BENCH_LIB"))) {
  .libPaths(c(Sys.getenv("BENCH_LIB"), .libPaths()))
}
source(file.path(here, "lib", "viewer_validation.R"))

bench_check_webgpu_adapter()
message("WebGPU preflight OK: Chromium returned an adapter")
