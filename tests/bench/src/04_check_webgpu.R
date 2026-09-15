# Fail fast when a C2 publication run cannot obtain a WebGPU adapter.

here <- Sys.getenv("BENCH_ROOT", "")
if (!nzchar(here)) {
  here <- normalizePath("tests/bench")
}
if (nzchar(Sys.getenv("BENCH_LIB"))) {
  .libPaths(c(Sys.getenv("BENCH_LIB"), .libPaths()))
}
source(file.path(here, "lib", "viewer_validation.R"))

chrome <- chromote::find_chrome()
chrome_version <- tryCatch(
  system2(chrome, "--version", stdout = TRUE, stderr = TRUE),
  error = function(error) conditionMessage(error)
)
message("Viewer WebGPU preflight browser: ", chrome)
message("Viewer WebGPU preflight version: ", paste(chrome_version, collapse = " "))

probe <- bench_check_webgpu_adapter(
  file.path(dirname(here), "..", "inst", "viewer", "www", "cell_points_gpu.js")
)
message(
  "Viewer WebGPU preflight OK: canvas, shader pipeline, and render submission ",
  "succeeded",
  if (!is.null(probe$adapter) && nzchar(probe$adapter)) {
    paste0(" (", probe$adapter, ")")
  } else {
    ""
  }
)
