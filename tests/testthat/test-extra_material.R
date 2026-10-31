# test-extra_material.R — Tests for extra material module

example_crb <- system.file(
  "extdata/examples/example.crb",
  package = "CerebroNexus"
)

fresh_extra_material_crb <- function(include_plots = FALSE) {
  crb <- Cerebro$new()
  crb$addExtraTable(
    "example_table",
    data.frame(cell = c("cell_1", "cell_2"), score = c(1, 2))
  )
  if (include_plots) {
    crb$addExtraPlot(
      "example_plot",
      ggplot2::ggplot(
        data.frame(x = c(1, 2), y = c(2, 1)),
        ggplot2::aes(x = x, y = y)
      ) +
        ggplot2::geom_point()
    )
  }

  path <- tempfile(fileext = ".crb")
  saveRDS(crb, path)
  readRDS(path)
}

## UI.R, content.R and select_content.R are sourced by the page tests.
test_that("extra_material server parses without errors", {
  expect_no_error(parse(file = viewer_test_path("extra_material", "server.R")))
})

test_that("example.crb extra material returns valid content", {
  crb <- readCerebro(example_crb)
  categories <- crb$getExtraMaterialCategories()
  expect_true(is.character(categories))
  expect_true("tables" %in% categories)
})

test_that("extra material tables are accessible", {
  crb <- readCerebro(example_crb)
  tables <- crb$getNamesOfExtraTables()
  expect_true(is.character(tables))
  expect_true(length(tables) > 0)
})

test_that("checkForExtraTables returns TRUE for example.crb", {
  crb <- readCerebro(example_crb)
  expect_true(crb$checkForExtraTables())
})

test_that("getExtraTable returns a data.frame", {
  crb <- readCerebro(example_crb)
  tables <- crb$getNamesOfExtraTables()
  skip_if(length(tables) == 0, "No tables in example.crb")
  tbl <- crb$getExtraTable(tables[1])
  expect_s3_class(tbl, "data.frame")
})

test_that("fresh serialized crb reports when no extra plots are present", {
  crb <- fresh_extra_material_crb()
  expect_false(crb$checkForExtraPlots())
})

test_that("fresh serialized crb lists extra plots when present", {
  crb <- fresh_extra_material_crb(include_plots = TRUE)
  expect_true(crb$checkForExtraPlots())
  expect_equal(crb$getNamesOfExtraPlots(), "example_plot")
})

test_that("fresh serialized crb returns stored extra plots", {
  crb <- fresh_extra_material_crb(include_plots = TRUE)
  expect_s3_class(crb$getExtraPlot("example_plot"), "ggplot")
  expect_null(crb$getExtraPlot("nonexistent"))
})
