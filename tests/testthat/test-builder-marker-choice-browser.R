library(shinytest2)

test_that("visible Marker genes card chooses and clears calculation", {
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
    "document.getElementById('enhance-analysis_marker_genes_action') !== null",
    timeout = 60000
  )
  app$wait_for_idle(timeout = 30000)

  app$click(selector = ".marker-genes-action .enhance-module-title")
  app$wait_for_js(
    paste0(
      "!document.getElementById('builder-marker-dialog-backdrop').hidden && ",
      "document.getElementById('builder-marker-dialog-title')?.textContent.trim() === ",
      "'Add Marker genes'"
    ),
    timeout = 10000
  )
  expect_identical(
    app$get_js(
      "document.querySelector('.marker-genes-action').getAttribute('aria-pressed')"
    ),
    "false"
  )

  app$click("enhance-marker_genes_calculate")
  app$wait_for_js(
    paste0(
      "document.getElementById('builder-marker-dialog-backdrop').hidden && ",
      "document.querySelector('.marker-genes-action')?.getAttribute('aria-pressed') === 'true'"
    ),
    timeout = 10000
  )

  app$click(selector = ".marker-genes-action .enhance-module-title")
  app$wait_for_js(
    "document.querySelector('.marker-genes-action')?.getAttribute('aria-pressed') === 'false'",
    timeout = 10000
  )
})

test_that("cancelling Marker genes choice leaves the card disabled", {
  builder_dir <- builder_profile_inst_path("builder")
  local_app_support(builder_dir)
  app <- AppDriver$new(
    builder_dir,
    name = "builder_marker_genes_cancel",
    width = 1280,
    height = 900,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)
  app$click(selector = ".example-btn[data-ex=basic_pbmc]")
  app$wait_for_js(
    "document.querySelector('.marker-genes-action') !== null",
    timeout = 60000
  )
  app$click(selector = ".marker-genes-action .enhance-module-title")
  app$wait_for_js(
    "!document.getElementById('builder-marker-dialog-backdrop').hidden"
  )
  app$click(selector = "#builder-marker-dialog-close")
  app$wait_for_js(
    "document.getElementById('builder-marker-dialog-backdrop').hidden"
  )
  expect_identical(
    app$get_js(
      "document.querySelector('.marker-genes-action').getAttribute('aria-pressed')"
    ),
    "false"
  )
})
