# Multi-panel Spatial view contracts.

spatial_test_file <- function(file) {
  installed <- system.file(
    "viewer",
    "spatial",
    file,
    package = "CerebroNexus"
  )
  if (nzchar(installed)) {
    return(installed)
  }
  testthat::test_path("../../inst/viewer/spatial", file)
}

test_that("spatial panel descriptors are stable and collision-safe", {
  names <- c("donor A tissue", "donor-A tissue", "donor/A tissue")

  panels <- spatial_panel_descriptors(names)

  expect_equal(vapply(panels, `[[`, character(1), "name"), names)
  expect_length(unique(vapply(panels, `[[`, character(1), "key")), 3)
  expect_length(unique(vapply(panels, `[[`, character(1), "plot_id")), 3)
  expect_length(unique(vapply(panels, `[[`, character(1), "background_id")), 3)
  expect_identical(spatial_panel_descriptors(names), panels)
})

test_that("spatial selection keeps the requested order and rejects stale names", {
  available <- c("donorA", "donorB", "donorC")

  expect_identical(
    normalize_spatial_panel_selection(
      c("donorC", "missing", "donorA", "donorC"),
      available
    ),
    c("donorC", "donorA")
  )
  expect_identical(
    normalize_spatial_panel_selection(NULL, available),
    character()
  )
})

test_that("Spatial background mode resolves automatic, none, and custom choices", {
  expect_true(
    exists("resolve_spatial_background_mode", mode = "function"),
    info = "the global background-mode resolver must exist"
  )
  if (!exists("resolve_spatial_background_mode", mode = "function")) {
    return(invisible())
  }

  external <- c("private-data/a.png", "private-data/b.png")
  expect_identical(
    resolve_spatial_background_mode(NULL, "auto", external, TRUE),
    "__embedded__"
  )
  expect_identical(
    resolve_spatial_background_mode(NULL, "auto", external, FALSE),
    external[[1L]]
  )
  expect_identical(
    resolve_spatial_background_mode("__embedded__", "none", external, TRUE),
    "No Background"
  )
  expect_identical(
    resolve_spatial_background_mode(FALSE, "custom", character(), TRUE),
    "No Background"
  )
  expect_identical(
    resolve_spatial_background_mode(TRUE, "custom", character(), TRUE),
    "__embedded__"
  )
  expect_identical(
    resolve_spatial_background_mode(external[[2L]], "custom", external, TRUE),
    external[[2L]]
  )
})

test_that("Spatial embedded backgrounds expose stable ids, labels, and bounds", {
  expect_true(
    exists("spatial_embedded_backgrounds", mode = "function"),
    info = "the embedded-background catalog helper must exist"
  )
  if (!exists("spatial_embedded_backgrounds", mode = "function")) {
    return(invisible())
  }

  bounds <- list(xmin = 0, xmax = 10, ymin = 20, ymax = 30)
  catalog <- spatial_embedded_backgrounds(list(
    histology_image = "data:image/png;base64,rose",
    histology_image_bounds = bounds,
    histology_images = list(
      "Rose H&E" = "data:image/png;base64,rose",
      "Blue H&E" = "data:image/png;base64,blue"
    )
  ))

  expect_identical(names(catalog), c("__embedded__", "__embedded__2"))
  expect_identical(
    unname(vapply(catalog, `[[`, character(1), "label")),
    c("Rose H&E", "Blue H&E")
  )
  expect_identical(catalog[[2L]]$image, "data:image/png;base64,blue")
  expect_identical(catalog[[2L]]$bounds, bounds)
  expect_identical(
    resolve_spatial_background_mode(
      "__embedded__2",
      "custom",
      character(),
      names(catalog)
    ),
    "__embedded__2"
  )
})

test_that("Spatial UI declares a wrapping grid and a multiple selector", {
  ui_path <- spatial_test_file("UI_projection.R")
  controls_path <- spatial_test_file("UI_projection_main_parameters.R")
  ui <- paste(readLines(ui_path), collapse = "\n")
  controls <- paste(readLines(controls_path), collapse = "\n")

  expect_match(ui, "spatial-projection-grid", fixed = TRUE)
  expect_match(
    ui,
    "repeat\\(auto-fit,[\\s\\S]{0,80}minmax\\(min\\(100%,[[:space:]]*20rem\\)",
    perl = TRUE
  )
  expect_match(
    controls,
    "spatial_projection_to_display[\\s\\S]{0,240}multiple[[:space:]]*=[[:space:]]*TRUE",
    perl = TRUE
  )
})

test_that("Spatial multi-panel cards use a compact horizontal gutter", {
  ui_path <- spatial_test_file("UI_projection.R")
  ui <- paste(readLines(ui_path), collapse = "\n")

  expect_match(ui, "gap: 8px", fixed = TRUE)
  expect_match(
    ui,
    "spatial-projection-item > .col-sm-12[\\s\\S]{0,160}padding-left: 8px",
    perl = TRUE
  )
  expect_match(ui, "padding-right: 8px", fixed = TRUE)
})

test_that("Spatial axes fit the currently visible finite coordinates", {
  expect_true(
    exists("spatial_projection_axis_ranges", mode = "function"),
    info = "the visible-coordinate range helper must exist"
  )
  if (!exists("spatial_projection_axis_ranges", mode = "function")) {
    return(invisible())
  }

  coordinates <- data.frame(
    x = c(0, 10, NA, Inf),
    y = c(50, 60, 999, 999)
  )
  ranges <- spatial_projection_axis_ranges(
    coordinates,
    fit_visible = TRUE,
    x_range = c(-100, 100),
    y_range = c(-100, 100)
  )

  expect_equal(ranges$x, c(-0.2, 10.2))
  expect_equal(ranges$y, c(49.8, 60.2))
})

test_that("Spatial axes retain manual ranges when visible fitting is off", {
  expect_true(
    exists("spatial_projection_axis_ranges", mode = "function"),
    info = "the visible-coordinate range helper must exist"
  )
  if (!exists("spatial_projection_axis_ranges", mode = "function")) {
    return(invisible())
  }

  ranges <- spatial_projection_axis_ranges(
    data.frame(x = 1:3, y = 11:13),
    fit_visible = FALSE,
    x_range = c(0, 20),
    y_range = c(10, 30)
  )

  expect_equal(ranges$x, c(0, 20))
  expect_equal(ranges$y, c(10, 30))
})

test_that("Spatial visible fitting accepts matrix coordinates", {
  coordinates <- cbind(x = c(0, 10), y = c(50, 60))

  ranges <- spatial_projection_axis_ranges(
    coordinates,
    fit_visible = TRUE
  )

  expect_equal(ranges$x, c(-0.2, 10.2))
  expect_equal(ranges$y, c(49.8, 60.2))
})

test_that("Spatial scale controls default to fitting visible cells", {
  scales <- paste(
    readLines(spatial_test_file("UI_projection_scales.R")),
    collapse = "\n"
  )
  parameters <- paste(
    readLines(spatial_test_file("obj_projection_parameters_plot.R")),
    collapse = "\n"
  )
  builder <- paste(
    readLines(spatial_test_file("obj_projection_data_to_plot.R")),
    collapse = "\n"
  )

  expect_match(
    scales,
    "spatial_projection_fit_visible_cells[\\s\\S]{0,160}value[[:space:]]*=[[:space:]]*TRUE",
    perl = TRUE
  )
  expect_match(parameters, "fit_visible_cells", fixed = TRUE)
  expect_match(builder, "spatial_projection_axis_ranges", fixed = TRUE)
})

test_that("the Spatial renderer routes every payload to its panel id", {
  renderer_path <- spatial_test_file("func_projection_update_plot.R")
  js_path <- spatial_test_file("js_projection_update_plot.js")
  renderer_env <- new.env(parent = globalenv())
  sys.source(renderer_path, envir = renderer_env)

  expect_true(
    "plot_id" %in%
      names(formals(
        renderer_env$spatial_projection_update_plot
      ))
  )
  renderer <- paste(readLines(renderer_path), collapse = "\n")
  js <- paste(readLines(js_path), collapse = "\n")
  expect_match(renderer, "getSpatialContainerDimensions\\(plot_id\\)")
  expect_match(renderer, "plot_id[[:space:]]*=[[:space:]]*plot_id")
  expect_match(
    js,
    "meta\\.plot_id[[:space:]]*\\|\\|[[:space:]]*SPATIAL_PLOT_ID"
  )
})

test_that("the Spatial grid renders one stable card per selected panel", {
  panel_path <- spatial_test_file("obj_projection_panels.R")
  expect_true(file.exists(panel_path))
  panel_src <- paste(readLines(panel_path), collapse = "\n")

  expect_match(panel_src, "spatial_projection_grid_UI", fixed = TRUE)
  expect_match(panel_src, "plotly::plotlyOutput", fixed = TRUE)
  expect_match(panel_src, "panel$plot_id", fixed = TRUE)
  expect_match(panel_src, "spatial_projection_parameters_for", fixed = TRUE)
  expect_match(panel_src, "spatial_projection_coordinates_for", fixed = TRUE)
  expect_match(panel_src, "build_spatial_projection_data", fixed = TRUE)
})

test_that("proxy-updated Spatial panels do not retain Shiny spinners", {
  panel_path <- spatial_test_file("obj_projection_panels.R")
  panel_src <- paste(readLines(panel_path), collapse = "\n")

  expect_false(
    grepl("shinycssloaders::withSpinner", panel_src, fixed = TRUE),
    info = paste(
      "plotlyProxy updates do not emit the completion event expected by",
      "withSpinner, so the loader would cover an already-rendered plot"
    )
  )
})
