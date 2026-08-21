library(shinytest2)

builder_marker_browser_load_example <- function(app) {
  builder_browser_wait_for_example_ready(app)
  app$click(selector = ".example-btn[data-ex=all_content]")
  app$wait_for_js(
    "document.querySelector('.ds-pick[aria-current=true]') !== null",
    timeout = 60000
  )
  builder_browser_dismiss_project_offer(app)
  app$wait_for_idle(timeout = 30000)
  app$wait_for_js(
    "document.querySelector('.marker-genes-action') !== null",
    timeout = 60000
  )
}

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

  builder_marker_browser_load_example(app)

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

  app$wait_for_js(
    "document.getElementById('enhance-marker_genes_calculate') !== null",
    timeout = 10000
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
  builder_marker_browser_load_example(app)
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

test_that("Marker import confirms an inferred source before enabling Save", {
  marker_file_named <- file.path(tempdir(), "0.csv")
  utils::write.csv(
    data.frame(gene = c("CD3D", "IL7R"), score = c(4, 3)),
    marker_file_named,
    row.names = FALSE
  )

  builder_dir <- builder_profile_inst_path("builder")
  local_app_support(builder_dir)
  app <- AppDriver$new(
    builder_dir,
    name = "builder_marker_import_mapping",
    width = 1280,
    height = 900,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)
  builder_marker_browser_load_example(app)
  app$click(selector = ".marker-genes-action .enhance-module-title")
  app$wait_for_js(
    "document.getElementById('enhance-marker_genes_upload') !== null"
  )
  app$click("enhance-marker_genes_upload")
  app$wait_for_js(
    "document.getElementById('enhance-marker_import_method') !== null"
  )

  app$set_inputs(`enhance-marker_import_method` = "Scanpy Wilcoxon")
  app$upload_file(`enhance-marker_import_files` = marker_file_named)
  app$wait_for_js(
    "document.querySelector('.marker-import-source.is-unresolved') !== null",
    timeout = 10000
  )
  expect_true(app$get_js(
    "document.getElementById('enhance-marker_import_save').disabled"
  ))
  app$click(selector = ".marker-source-confirm[data-source-id='source-001']")
  app$wait_for_js(
    paste0(
      "document.querySelector('.marker-import-source.is-ready') !== null && ",
      "!document.getElementById('enhance-marker_import_save').disabled"
    ),
    timeout = 10000
  )
  app$click("enhance-marker_import_save")
  app$wait_for_js(
    paste0(
      "document.getElementById('builder-marker-dialog-backdrop').hidden && ",
      "document.querySelector('.marker-genes-action')?.getAttribute('aria-pressed') === 'true'"
    ),
    timeout = 10000
  )
})
