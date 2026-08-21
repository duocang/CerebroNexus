# test-app-new-modules.R — shinytest2 integration tests for PR2 enhanced modules

library(shinytest2)

inst_dir <- system.file(package = "CerebroNexus")

## Booting the app dominates these recordings: every test below only reads from
## the DOM of the same freshly-loaded example.crb, so a driver per test spent
## almost all its time on startup. They share one driver instead, created on
## first use and stopped when the file finishes.
##
## Sharing is safe here because each test navigates to the tab it asserts on
## rather than relying on where a previous test left the app. Keep that property
## when adding a test: if a new test needs a pristine app (e.g. asserting that
## something has NOT been loaded yet), give it its own AppDriver instead.
shared_driver <- NULL
shared_driver_env <- environment()
shared_app <- function() {
  if (is.null(shared_driver)) {
    local_app_support(inst_dir, envir = shared_driver_env)
    shared_driver <<- AppDriver$new(
      inst_dir,
      name = "new_modules_shared",
      height = 950,
      width = 1619
    )
    withr::defer(shared_driver$stop(), envir = shared_driver_env)
    shared_driver$wait_for_idle(timeout = 20000)
  }
  shared_driver
}

test_that("most_expressed_genes tab navigates and renders table", {
  app <- shared_app()

  # Most expressed genes is now a conditionally + asynchronously inserted sidebar
  # item (insertConditionalTab). Wait for its menu link to appear, then click it,
  # so the tab activates on a slow CI runner instead of navigating too early.
  app$wait_for_js(
    "document.querySelector('a[href=\"#shiny-tab-mostExpressedGenes\"]') !== null",
    timeout = 20000
  )
  app$run_js(
    'document.querySelector(\'a[href="#shiny-tab-mostExpressedGenes"]\').click();'
  )
  app$wait_for_idle(timeout = 10000)

  # Group selector renders with expected options
  select_html <- app$get_value(
    output = "most_expressed_genes_select_group_UI"
  )$html
  expect_true(grepl("seurat_clusters", select_html))

  # Table renders without error
  table_html <- app$get_value(output = "most_expressed_genes_table_UI")$html
  expect_false(grepl("no.*available|not.*found|error", tolower(table_html)))
})


test_that("enriched_pathways tab content exists in DOM", {
  app <- shared_app()

  # Output container exists in the DOM
  has_div <- app$get_js(
    'document.getElementById("enriched_pathways_select_method_and_table_UI") !== null;'
  )
  expect_true(has_div)
})


test_that("extra_material tab content exists in DOM", {
  app <- shared_app()

  # Output container exists in the DOM
  has_div <- app$get_js(
    'document.getElementById("extra_material_select_category_and_content_UI") !== null;'
  )
  expect_true(has_div)
})

test_that("generated apps keep extra-table file and sheet choices separate", {
  skip_on_cran()
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  example <- system.file(
    "extdata/examples/example.crb",
    package = "CerebroNexus"
  )
  skip_if_not(nzchar(example), "example.crb not found")

  root <- withr::local_tempdir()
  crb <- file.path(root, "study.crb")
  study <- readRDS(example)
  study$extra_material <- list()
  study$addExtraTable("Legacy table", data.frame(source = "embedded"))
  saveRDS(study, crb)

  clinical <- file.path(root, "clinical.xlsx")
  markers <- file.path(root, "markers.xlsx")
  writexl::write_xlsx(
    list(
      Summary = data.frame(source = "clinical"),
      Details = data.frame(source = "clinical-details")
    ),
    clinical
  )
  writexl::write_xlsx(
    list(
      Summary = data.frame(source = "marker"),
      Details = data.frame(source = "marker-details")
    ),
    markers
  )

  app_dir <- file.path(root, "app")
  createShinyApp(
    cerebro_data = c(Study = crb),
    extra_tables = c(
      "Clinical workbook" = clinical,
      "Marker workbook" = markers
    ),
    result_dir = app_dir,
    launch_browser = FALSE,
    verbose = FALSE
  )

  config <- readRDS(file.path(app_dir, "cerebro_config.rds"))
  expect_named(
    config$extra_tables$files,
    c("Clinical workbook", "Marker workbook")
  )

  app <- AppDriver$new(
    app_dir,
    name = "extra_material_external_workbooks",
    height = 950,
    width = 1619,
    load_timeout = 30000
  )
  withr::defer(app$stop())
  app$wait_for_idle(timeout = 20000)
  app$wait_for_js(
    "document.querySelector('a[href=\"#shiny-tab-extra_material\"]') !== null",
    timeout = 20000
  )
  app$run_js(
    'document.querySelector(\'a[href="#shiny-tab-extra_material"]\').click();'
  )

  ## The default remains the CRB's historical table group.  Switching files
  ## must replace the sheet choices even when both workbooks use "Summary".
  app$wait_for_js(
    "document.getElementById('extra_material_selected_file') !== null",
    timeout = 20000
  )
  expect_identical(
    app$get_js("document.getElementById('extra_material_selected_file').value"),
    "embedded"
  )
  app$wait_for_js(
    "document.querySelector('#extra_material_table').innerText.includes('embedded')",
    timeout = 20000
  )

  app$run_js(
    "$('#extra_material_selected_file')[0].selectize.setValue('external-file:1');"
  )
  app$wait_for_js(
    "document.querySelector('#extra_material_selected_content option[value=\"external:1:1\"]') !== null",
    timeout = 20000
  )
  app$run_js(
    "$('#extra_material_selected_content')[0].selectize.setValue('external:1:1');"
  )
  app$wait_for_js(
    "document.querySelector('#extra_material_table').innerText.includes('clinical')",
    timeout = 20000
  )

  app$run_js(
    "$('#extra_material_selected_file')[0].selectize.setValue('external-file:2');"
  )
  app$wait_for_js(
    "document.querySelector('#extra_material_selected_content option[value=\"external:2:1\"]') !== null && document.querySelector('#extra_material_selected_content option[value=\"external:1:1\"]') === null",
    timeout = 20000
  )
  app$run_js(
    "$('#extra_material_selected_content')[0].selectize.setValue('external:2:1');"
  )
  app$wait_for_js(
    "document.querySelector('#extra_material_table').innerText.includes('marker')",
    timeout = 20000
  )
})


test_that("all three new tabs are visible in sidebar after data load", {
  app <- shared_app()

  most_expressed <- app$get_js(
    'document.querySelector(\'a[href="#shiny-tab-mostExpressedGenes"]\') !== null;'
  )
  expect_true(most_expressed)

  enriched_pw <- app$get_js(
    'document.querySelector(\'a[href="#shiny-tab-enrichedPathways"]\') !== null;'
  )
  expect_true(enriched_pw)

  extra_mat <- app$get_js(
    'document.querySelector(\'a[href="#shiny-tab-extra_material"]\') !== null;'
  )
  expect_true(extra_mat)
})

test_that("insertConditionalTab is defined and wired to conditional tabs", {
  server_file <- file.path(inst_dir, "viewer/shiny_server.R")
  skip_if_not(file.exists(server_file))
  content <- paste(readLines(server_file), collapse = "\n")

  # Function is defined with all required parameters
  expect_match(
    content,
    "insertConditionalTab\\s*<-\\s*function\\s*\\(\\s*tab_label\\s*,\\s*tab_name\\s*,\\s*icon_name\\s*,\\s*check_fn",
    perl = TRUE
  )

  # Calls are present for enriched pathways and extra material
  expect_match(
    content,
    "insertConditionalTab\\s*\\([\\s\\S]*?Enriched pathways",
    perl = TRUE
  )
  expect_match(
    content,
    "insertConditionalTab\\s*\\([\\s\\S]*?Extra material",
    perl = TRUE
  )
})

test_that("conditional tab placeholders exist in UI source", {
  ui_file <- file.path(inst_dir, "viewer/shiny_UI.R")
  skip_if_not(file.exists(ui_file))
  content <- paste(readLines(ui_file), collapse = "\n")

  # Placeholder divs for the two conditional tabs
  expect_match(
    content,
    'sidebar_item_enriched_pathways_placeholder',
    perl = TRUE
  )
  expect_match(content, 'sidebar_item_extra_material_placeholder', perl = TRUE)
})
