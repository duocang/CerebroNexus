library(shinytest2)

inst_dir <- system.file(package = "CerebroNexus")
if (!nzchar(inst_dir) || !file.exists(file.path(inst_dir, "app.R"))) {
  inst_dir <- testthat::test_path("../../inst")
}

test_that("Gene expression progress waits for the browser render acknowledgement", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "gene_expression_progress",
    height = 950,
    width = 1619
  )
  withr::defer(app$stop())
  app$wait_for_idle(timeout = 20000)

  app$wait_for_js(
    "document.querySelector('a[href=\"#shiny-tab-geneExpression\"]') !== null",
    timeout = 10000
  )
  app$run_js(
    "document.querySelector('a[href=\"#shiny-tab-geneExpression\"]').click();"
  )
  app$wait_for_js(
    "document.getElementById('expression_genes_input') !== null",
    timeout = 10000
  )
  viewer_set_selectize(app, "expression_genes_input", "MS4A1")
  app$wait_for_js(
    paste0(
      "document.getElementById('cv-cbar')?.textContent.includes('MS4A1') && ",
      "document.querySelector('[id^=\"shiny-progress-\"]') === null"
    ),
    timeout = 20000
  )

  app$run_js(paste0(
    "window.__geneProgressSetInputValue=Shiny.setInputValue.bind(Shiny);",
    "window.__geneProgressAck=null;",
    "Shiny.setInputValue=function(name,value,options){",
    "if(name==='expression_projection_render_complete'){",
    "window.__geneProgressAck={name:name,value:value,options:options};return;}",
    "return window.__geneProgressSetInputValue(name,value,options);};"
  ))
  viewer_set_selectize(app, "expression_genes_input", "CD3D")
  app$wait_for_js(
    "window.__geneProgressAck !== null",
    timeout = 20000
  )
  expect_true(app$get_js(
    "document.querySelector('[id^=\"shiny-progress-\"]') !== null"
  ))

  app$run_js(paste0(
    "Shiny.setInputValue=window.__geneProgressSetInputValue;",
    "window.__geneProgressSetInputValue(",
    "window.__geneProgressAck.name,window.__geneProgressAck.value,",
    "window.__geneProgressAck.options);"
  ))
  app$wait_for_js(
    "document.querySelector('[id^=\"shiny-progress-\"]') === null",
    timeout = 10000
  )
})
