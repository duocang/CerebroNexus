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

viewer_app_test_path <- function() {
  source_path <- testthat::test_path("..", "..", "inst")
  if (file.exists(file.path(source_path, "app.R"))) {
    return(source_path)
  }

  system.file(package = "CerebroNexus")
}
