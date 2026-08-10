test_that("Linked views is the only projection, spatial, and Trekker workspace", {
  viewer_root <- system.file("viewer", package = "CerebroNexus")
  if (!nzchar(viewer_root)) {
    viewer_root <- testthat::test_path("../../inst/viewer")
  }

  ui <- paste(readLines(file.path(viewer_root, "shiny_UI.R")), collapse = "\n")
  server <- paste(
    readLines(file.path(viewer_root, "shiny_server.R")),
    collapse = "\n"
  )

  expect_match(ui, "/viewer/coordinated_views/UI.R", fixed = TRUE)
  expect_match(server, "/viewer/coordinated_views/server.R", fixed = TRUE)

  for (legacy in c("overview", "spatial", "trekker")) {
    expect_false(
      dir.exists(file.path(viewer_root, legacy)),
      info = paste("legacy module directory remains:", legacy)
    )
    expect_false(
      grepl(paste0("/viewer/", legacy, "/UI.R"), ui, fixed = TRUE),
      info = paste("legacy UI is still sourced:", legacy)
    )
    expect_false(
      grepl(paste0("/viewer/", legacy, "/server.R"), server, fixed = TRUE),
      info = paste("legacy server is still sourced:", legacy)
    )
  }

  expect_false(grepl('tabName = "overview"', ui, fixed = TRUE))
  expect_false(grepl("sidebar_item_spatial_placeholder", ui, fixed = TRUE))
  expect_false(grepl("sidebar_item_trekker_placeholder", ui, fixed = TRUE))
  expect_false(grepl('"Spatial",\n    "spatial"', server, fixed = TRUE))
  expect_false(grepl('"Trekker",\n    "trekker"', server, fixed = TRUE))
  expect_false(grepl("overview_cells_to_show", server, fixed = TRUE))
})
