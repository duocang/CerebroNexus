test_that("trajectory consumers reuse merged cells and structured hover", {
  server <- paste(
    readLines(viewer_test_path("trajectory", "server.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(
    server,
    "trajectory_row_index_reactive <- reactive",
    fixed = TRUE
  )
  expect_match(server, "trajectory_cells_reactive <- function", fixed = TRUE)
  expect_match(server, "viewerPackTrajectoryIndex", fixed = TRUE)
  expect_false(grepl(
    "mergeTrajectoryWithMetaData(",
    server,
    fixed = TRUE
  ))

  active_files <- c(
    "distribution_along_pseudotime.R",
    "expression_metrics.R",
    "projection_plot.R",
    "selected_cells_table.R",
    "states_by_group.R"
  )
  active_source <- paste(
    vapply(
      active_files,
      function(path) {
        paste(
          readLines(viewer_test_path("trajectory", path), warn = FALSE),
          collapse = "\n"
        )
      },
      character(1)
    ),
    collapse = "\n"
  )
  expect_false(grepl(
    "mergeTrajectoryWithMetaData(",
    active_source,
    fixed = TRUE
  ))
  expect_match(active_source, "trajectory_cells_reactive(", fixed = TRUE)

  projection <- paste(
    readLines(
      viewer_test_path("trajectory", "projection_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_false(grepl("buildHoverInfoForProjections", projection, fixed = TRUE))
  expect_match(projection, "hover_columns", fixed = TRUE)
})

test_that("supplementary trajectory panels wait for the main projection", {
  projection <- paste(
    readLines(
      viewer_test_path("trajectory", "projection_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(projection, "trajectory_projection_sent(TRUE)", fixed = TRUE)
  expect_match(projection, "session$onFlushed(", fixed = TRUE)

  for (path in c(
    "distribution_along_pseudotime.R",
    "states_by_group.R",
    "expression_metrics.R"
  )) {
    source <- paste(
      readLines(viewer_test_path("trajectory", path), warn = FALSE),
      collapse = "\n"
    )
    expect_match(
      source,
      "req(trajectory_projection_sent())",
      fixed = TRUE,
      info = path
    )
  }
})
