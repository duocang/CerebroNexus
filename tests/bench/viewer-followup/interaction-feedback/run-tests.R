testthat::test_local(filter = paste(c(
  "view-feedback", "gene-feedback", "gene-progress-lifecycle", "viewer-review-feedback",
  "dataset-loading-progress", "viewer-dataset-context", "cell-view-state",
  "single-expression-incremental", "app-trajectory", "trajectory-selection-demand",
  "spatial-image-payload", "spatial-missing-coordinates", "hla-app-contract"
), collapse = "|"), reporter = "summary", stop_on_failure = TRUE)
