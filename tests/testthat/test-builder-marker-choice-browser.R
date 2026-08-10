library(shinytest2)

test_that("Marker genes keeps the calculated selection after confirmation", {
  builder_dir <- builder_profile_inst_path("builder")
  local_app_support(builder_dir)
  app <- AppDriver$new(
    builder_dir,
    name = "builder_marker_genes_choice",
    width = 1280,
    height = 900,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)

  app$click(selector = ".example-btn[data-ex=basic_pbmc]")
  app$wait_for_js(
    "document.getElementById('enhance-analysis_marker_genes') !== null",
    timeout = 60000
  )
  app$wait_for_idle(timeout = 30000)

  app$click(
    selector = paste0(
      ".enhance-module:has(#enhance-analysis_marker_genes) ",
      ".enhance-module-title"
    )
  )
  app$wait_for_js(
    paste0(
      "document.querySelector('.modal-title')?.textContent.trim() === ",
      "'Add Marker genes'"
    ),
    timeout = 10000
  )
  expect_false(app$get_js(
    "document.getElementById('enhance-analysis_marker_genes').checked"
  ))

  app$click("enhance-marker_genes_calculate")
  app$wait_for_idle(timeout = 30000)
  app$wait_for_js(
    paste0(
      "document.querySelector('.modal.show') === null && ",
      "document.getElementById('enhance-analysis_marker_genes').checked"
    ),
    timeout = 10000
  )
  expect_true(app$get_js(
    "document.getElementById('enhance-analysis_marker_genes').checked"
  ))
})
