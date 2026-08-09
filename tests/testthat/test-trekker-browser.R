library(shinytest2)

trekker_browser_inst <- system.file(package = "CerebroNexus")
if (
  !nzchar(trekker_browser_inst) ||
    !file.exists(file.path(trekker_browser_inst, "app.R"))
) {
  trekker_browser_inst <- testthat::test_path("../../inst")
}

test_that("Trekker initial Cell type mode uses distinct categorical colours", {
  local_app_support(trekker_browser_inst)
  app <- AppDriver$new(
    trekker_browser_inst,
    name = "trekker-initial-celltype-colours",
    height = 950,
    width = 1500
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 20000)

  app$set_inputs(crb_file_selector = "extdata/examples/demo_omnibus.crb")
  app$wait_for_js(
    "document.querySelector('a[href=\"#shiny-tab-trekker\"]') !== null",
    timeout = 30000
  )
  app$run_js(
    "document.querySelector('a[href=\"#shiny-tab-trekker\"]').click();"
  )
  app$wait_for_js(
    "document.querySelectorAll('#tk-legend .tk-dot').length >= 2",
    timeout = 30000
  )

  colours <- unlist(app$get_js(paste0(
    "Array.from(document.querySelectorAll('#tk-legend .tk-dot'))",
    ".map(function(dot) { return getComputedStyle(dot).backgroundColor; })"
  )))
  expect_gt(length(unique(colours)), 1L)
  expect_false(all(colours == "rgb(153, 153, 153)"))
})
