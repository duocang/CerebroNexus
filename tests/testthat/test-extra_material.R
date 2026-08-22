# test-extra_material.R — Tests for extra material module

shiny_root <- system.file("viewer", package = "CerebroNexus")
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

extra_material_viewer_env <- function() {
  utility_file <- testthat::test_path(
    "..",
    "..",
    "inst",
    "viewer",
    "utility_functions.R"
  )
  env <- new.env(parent = globalenv())
  sys.source(utility_file, envir = env)
  env
}

external_extra_table_manifest <- list(
  files = list(
    "Clinical workbook" = list(
      sheets = list(
        list(
          key = "external:1:1",
          label = "Patients",
          path = "private-data/extra-tables/1-1.rds"
        ),
        list(
          key = "external:1:2",
          label = "Visits",
          path = "private-data/extra-tables/1-2.rds"
        )
      )
    ),
    "Marker workbook" = list(
      sheets = list(
        list(
          key = "external:2:1",
          label = "Patients",
          path = "private-data/extra-tables/2-1.rds"
        )
      )
    )
  )
)

test_that("extra table groups keep duplicate file labels separate", {
  viewer <- extra_material_viewer_env()
  groups <- viewer$extra_material_table_groups(
    external_manifest = external_extra_table_manifest,
    embedded = list(Score = data.frame(x = 1))
  )

  expect_named(
    groups,
    c("Embedded tables", "Clinical workbook", "Marker workbook")
  )
  expect_identical(groups[[2]]$sheet_specs[[1]]$label, "Patients")
  expect_identical(
    viewer$extra_material_table_selection(
      groups,
      file_key = "embedded",
      sheet_key = "embedded:1"
    )$sheet$table$x,
    1
  )
})

test_that("extra table groups retain legacy embedded tables without a manifest", {
  viewer <- extra_material_viewer_env()
  groups <- viewer$extra_material_table_groups(
    external_manifest = NULL,
    embedded = list("Legacy table" = data.frame(value = 42))
  )

  expect_named(groups, "Embedded tables")
  expect_identical(groups[[1]]$sheets[[1]]$label, "Legacy table")
  expect_identical(
    viewer$extra_material_table_selection(groups)$sheet$table$value,
    42
  )
})

test_that("extra table selection falls back to the first embedded table", {
  viewer <- extra_material_viewer_env()
  groups <- viewer$extra_material_table_groups(
    external_manifest = external_extra_table_manifest,
    embedded = list(Score = data.frame(x = 1))
  )

  expect_identical(
    viewer$extra_material_table_selection(
      groups,
      file_key = "missing-file",
      sheet_key = "missing-sheet"
    )$sheet$key,
    "embedded:1"
  )
})

test_that("external file labels stay unchanged when they match the embedded group", {
  viewer <- extra_material_viewer_env()
  manifest <- external_extra_table_manifest
  names(manifest$files)[[1]] <- "Embedded tables"
  groups <- viewer$extra_material_table_groups(
    external_manifest = manifest,
    embedded = list(Score = data.frame(x = 1))
  )

  expect_identical(groups[[1]]$label, "Embedded tables")
  expect_identical(groups[[2]]$label, "Embedded tables")
  expect_identical(groups[[2]]$key, "external-file:1")
  expect_identical(
    names(viewer$extra_material_table_file_choices(groups)),
    c("Embedded tables (from CRB)", "Embedded tables", "Marker workbook")
  )
})

test_that("external tables add the tables category without a CRB table", {
  viewer <- extra_material_viewer_env()
  viewer$Cerebro.options <- list(extra_tables = external_extra_table_manifest)
  viewer$data_set <- function() list()
  viewer$is_cerebro_dataset <- function(data) FALSE

  expect_identical(viewer$getExtraMaterialCategories(), "tables")
  expect_named(
    viewer$extra_material_table_groups(),
    c("Clinical workbook", "Marker workbook")
  )
})

test_that("extra_material module files parse without errors", {
  mod_files <- c("UI.R", "server.R", "content.R", "select_content.R")
  for (f in mod_files) {
    fpath <- file.path(shiny_root, "extra_material", f)
    skip_if_not(file.exists(fpath), message = paste("Missing:", f))
    expect_no_error(parse(file = fpath))
  }
})

test_that("extra_material UI defines correct tabName", {
  ui_file <- file.path(shiny_root, "extra_material", "UI.R")
  skip_if_not(file.exists(ui_file))
  content <- paste(readLines(ui_file), collapse = "\n")
  expect_match(content, 'tabName\\s*=\\s*"extra_material"', perl = TRUE)
})

test_that("extra-material selectors use shared compact fields", {
  selector_file <- testthat::test_path(
    "..",
    "..",
    "inst",
    "viewer",
    "extra_material",
    "select_content.R"
  )
  skip_if_not(file.exists(selector_file))
  selector <- paste(readLines(selector_file, warn = FALSE), collapse = "\n")

  expect_match(selector, "extra-material-selectors", fixed = TRUE)
  expect_match(selector, "result-selector-field", fixed = TRUE)
  expect_match(selector, "extra_material_selected_file", fixed = TRUE)
  expect_match(selector, 'label = "Material type:"', fixed = TRUE)
  expect_match(selector, 'label = "Choose a table:"', fixed = TRUE)
  expect_match(selector, 'width = "100%"', fixed = TRUE)
})

test_that("example.crb extra material returns valid content", {
  skip_if_not(file.exists(example_crb))
  crb <- readRDS(example_crb)
  categories <- crb$getExtraMaterialCategories()
  expect_true(is.character(categories))
  expect_true("tables" %in% categories)
})

test_that("extra material tables are accessible", {
  skip_if_not(file.exists(example_crb))
  crb <- readRDS(example_crb)
  tables <- crb$getNamesOfExtraTables()
  expect_true(is.character(tables))
  expect_true(length(tables) > 0)
})

test_that("checkForExtraTables returns TRUE for example.crb", {
  skip_if_not(file.exists(example_crb))
  crb <- readRDS(example_crb)
  expect_true(crb$checkForExtraTables())
})

test_that("getExtraTable returns a data.frame", {
  skip_if_not(file.exists(example_crb))
  crb <- readRDS(example_crb)
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
