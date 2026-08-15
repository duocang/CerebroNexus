test_that("Spatial Builder uses a persistent Canvas live preview", {
  root <- testthat::test_path("..", "..", "inst", "builder")
  ui <- paste(
    readLines(file.path(root, "ui", "enhance_stage.R"), warn = FALSE),
    collapse = "\n"
  )
  app <- paste(
    readLines(file.path(root, "app.R"), warn = FALSE),
    collapse = "\n"
  )
  server <- paste(
    readLines(file.path(root, "spatial_alignment_server.R"), warn = FALSE),
    collapse = "\n"
  )
  canvas <- file.path(root, "www", "builder-spatial-canvas.js")

  expect_true(file.exists(canvas))
  expect_match(ui, 'tags$canvas(', fixed = TRUE)
  expect_match(ui, 'class = "builder-spatial-canvas"', fixed = TRUE)
  expect_match(ui, '`aria-label` = label', fixed = TRUE)
  expect_false(grepl("plotly::plotlyOutput(", ui, fixed = TRUE))
  expect_match(app, '"builder-spatial-canvas.js"', fixed = TRUE)
  expect_false(grepl('plotly::renderPlotly({', server, fixed = TRUE))
  expect_match(server, '"builder_spatial_canvas_scene"', fixed = TRUE)
  expect_match(server, "builder_spatial_canvas_scene(", fixed = TRUE)
  expect_false(grepl("encoded <- shiny::reactive({", server, fixed = TRUE))
})

test_that("Canvas renderer owns bounded raw points and latest-only controls", {
  root <- testthat::test_path("..", "..", "inst", "builder")
  js <- paste(
    readLines(
      file.path(root, "www", "builder-spatial-canvas.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(js, "requestAnimationFrame", fixed = TRUE)
  expect_match(js, "builder_spatial_canvas_scene", fixed = TRUE)
  expect_match(js, "builder_spatial_canvas_reset", fixed = TRUE)
  expect_match(js, "generation", fixed = TRUE)
  expect_match(js, "resetToken", fixed = TRUE)
  expect_match(js, "viewKey", fixed = TRUE)
  expect_match(js, "devicePixelRatio", fixed = TRUE)
  expect_match(js, "Math.min(window.devicePixelRatio || 1, 2)", fixed = TRUE)
  expect_match(js, "pointermove", fixed = TRUE)
  expect_false(grepl("Shiny.setInputValue", js, fixed = TRUE))
  expect_false(grepl("Plotly", js, fixed = TRUE))
})

test_that("Spatial preview worker contract does not include coordinate drafts", {
  root <- testthat::test_path("..", "..", "inst", "builder")
  server <- paste(
    readLines(file.path(root, "spatial_alignment_server.R"), warn = FALSE),
    collapse = "\n"
  )
  preview <- paste(
    readLines(file.path(root, "preview.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_false(grepl("coordinate_preview_transforms", server, fixed = TRUE))
  expect_false(grepl(
    "coordinate_transforms = coordinate_draft",
    server,
    fixed = TRUE
  ))
  expect_match(preview, "raw sampled spatial coordinates", fixed = TRUE)
})
