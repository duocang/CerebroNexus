test_that("Groups reads only the active columns after its viewport gate", {
  composition <- paste(
    readLines(viewer_test_path("groups", "composition.R"), warn = FALSE),
    collapse = "\n"
  )
  metrics <- paste(
    readLines(viewer_test_path("groups", "expression_metrics.R"), warn = FALSE),
    collapse = "\n"
  )
  ui <- paste(
    readLines(viewer_test_path("groups", "UI.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_false(grepl("getMetaData()", composition, fixed = TRUE))
  expect_false(grepl("getMetaData()", metrics, fixed = TRUE))
  expect_match(composition, "groupsMetadataColumns", fixed = TRUE)
  expect_match(metrics, "groupsMetricData", fixed = TRUE)
  expect_match(
    metrics,
    "groups_expression_metrics_render_request",
    fixed = TRUE
  )
  expect_match(metrics, "groups_expression_metrics_tabs", fixed = TRUE)
  expect_match(ui, "IntersectionObserver", fixed = TRUE)
  expect_match(ui, "groups_expression_metrics_render_request", fixed = TRUE)
})
