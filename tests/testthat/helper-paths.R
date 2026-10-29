## Paths into inst/. Helpers load alphabetically, so this file comes first and
## later helpers can source viewer files at load time.

## The source tree's copy when it exists (devtools::test(), test_local()),
## otherwise the installed package's (R CMD check).
inst_test_path <- function(...) {
  source_path <- testthat::test_path("..", "..", "inst", ...)
  if (file.exists(source_path)) {
    return(source_path)
  }

  system.file(..., package = "CerebroNexus")
}

viewer_test_path <- function(...) {
  inst_test_path("viewer", ...)
}

## Sources the Viewer runtime helpers into `envir` in the order shiny_server.R
## loads them, so a test does not have to know which files utility_functions.R
## depends on. `root` is a viewer directory, e.g. a generated app's. Returns
## `envir`.
viewer_runtime_env <- function(
  envir = new.env(parent = globalenv()),
  root = viewer_test_path()
) {
  for (file in c(
    "plotting_functions.R",
    "core/viewer_pack.R",
    "core/crb_contract.R",
    "msigdb.R",
    "crb_cache.R",
    "utility_functions.R"
  )) {
    sys.source(file.path(root, file), envir = envir)
  }
  invisible(envir)
}

viewer_app_test_path <- function() {
  source_path <- testthat::test_path("..", "..", "inst")
  if (file.exists(file.path(source_path, "app.R"))) {
    return(source_path)
  }

  system.file(package = "CerebroNexus")
}
