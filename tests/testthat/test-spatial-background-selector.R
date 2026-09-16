test_that("All ROIs keeps the background selector for multiple images", {
  main_ui <- viewer_test_path(
    "spatial",
    "UI_projection_main_parameters.R"
  )
  spatial <- list(
    coordinates = data.frame(
      x = c(0, 1),
      y = c(0, 1),
      row.names = c("cell-1", "cell-2")
    )
  )
  server <- function(input, output, session) {
    data_set <- function() TRUE
    availableSpatial <- function() "fov"
    getSpatialData <- function(name) spatial
    getMetaData <- function() {
      data.frame(
        cell_barcode = c("cell-1", "cell-2"),
        roi = c("A", "B")
      )
    }
    getGroups <- function() character()
    serverSideGeneSelector <- function(...) invisible(NULL)
    Cerebro.options <- list(
      spatial_images = list(
        dataset = list(
          fov = c(
            `H&E` = "spatial-assets/he.png",
            DAPI = "spatial-assets/dapi.png"
          )
        )
      )
    )
    available_crb_files <- list(
      files = c(dataset = "dataset.crb"),
      selected = "dataset.crb"
    )
    sys.source(main_ui, envir = environment())
  }

  shiny::testServer(server, {
    session$setInputs(spatial_projection_roi = "__all__")
    session$flushReact()

    selector <- as.character(
      output$spatial_projection_background_selector_UI$html
    )
    expect_match(
      selector,
      'id="spatial_projection_background_image"',
      fixed = TRUE
    )
    expect_match(selector, "external::H&amp;E", fixed = TRUE)
    expect_match(selector, "external::DAPI", fixed = TRUE)
  })
})
