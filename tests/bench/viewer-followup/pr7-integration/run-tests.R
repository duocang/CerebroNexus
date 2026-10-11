pkgload::load_all(quiet = TRUE)
testthat::test_local(filter = paste(c(
  "gene-selection-canonical", "viewer-shared-selection", "cell-view-state",
  "viewer-dataset-context", "gene-hover-demand", "gene-progress-lifecycle",
  "gene-expression-color-transport", "expression-rgb-dataset-switch",
  "single-expression-incremental"
), collapse = "|"), reporter = "summary", stop_on_failure = TRUE)
