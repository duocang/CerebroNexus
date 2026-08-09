library(shinytest2)

spatial_browser_inst <- system.file(package = "CerebroNexus")
if (
  !nzchar(spatial_browser_inst) ||
    !file.exists(file.path(spatial_browser_inst, "app.R"))
) {
  spatial_browser_inst <- testthat::test_path("../../inst")
}

test_that("Spatial renders selected tissues as independent responsive cards", {
  local_app_support(spatial_browser_inst)
  app <- AppDriver$new(
    spatial_browser_inst,
    name = "spatial-multi-panel",
    height = 950,
    width = 1619
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 20000)

  app$wait_for_js(
    "document.getElementById('crb_file_selector') !== null",
    timeout = 20000
  )
  app$set_inputs(
    crb_file_selector = "extdata/examples/demo_omnibus.crb"
  )
  app$wait_for_js(
    "document.querySelector('a[href=\"#shiny-tab-spatial\"]') !== null",
    timeout = 30000
  )
  app$run_js(
    "document.querySelector('a[href=\"#shiny-tab-spatial\"]').click();"
  )
  app$wait_for_js(
    "document.getElementById('spatial_projection_to_display') !== null",
    timeout = 20000
  )

  app$set_inputs(
    spatial_projection_to_display = c(
      "donorA tissue",
      "donorB tissue",
      "donorC tissue"
    )
  )
  app$wait_for_js(
    "document.querySelectorAll('.spatial-projection-item').length === 3",
    timeout = 30000
  )
  app$wait_for_js(
    "document.querySelectorAll('.spatial-background-row').length === 3",
    timeout = 30000
  )
  app$wait_for_js(
    paste0(
      "['spatial_projection','spatial_projection_002',",
      "'spatial_projection_003'].every(function(id) { ",
      "return document.getElementById(id) && ",
      "document.getElementById(id).classList.contains('js-plotly-plot'); })"
    ),
    timeout = 30000
  )

  card_names <- unlist(app$get_js(
    paste0(
      "Array.from(document.querySelectorAll(",
      "'[data-spatial-panel] .box-title'))",
      ".map(function(el) { return el.textContent.trim(); })"
    )
  ))
  expect_equal(
    card_names,
    c("donorA tissue", "donorB tissue", "donorC tissue")
  )
  background_labels <- unlist(app$get_js(paste0(
    "Array.from(document.querySelectorAll(",
    "'.spatial-background-row-label'))",
    ".map(function(el) { return el.textContent.trim(); })"
  )))
  expect_equal(background_labels, card_names)
  ## Only donorA has multiple images. The common one-image case is a labelled
  ## switch; a select appears only for the genuine multi-image row.
  expect_equal(
    app$get_js(
      "document.querySelectorAll('.spatial-background-row select').length"
    ),
    1
  )
  expect_equal(
    app$get_js(
      "document.querySelectorAll('.spatial-background-row input[type=checkbox]').length"
    ),
    2
  )
  expect_equal(
    unlist(app$get_js(paste0(
      "Object.values(document.getElementById(",
      "'spatial_projection_background_image').selectize.options)",
      ".map(function(option) { return option.label; })"
    ))),
    c("None", "Rose H&E", "Blue H&E")
  )
  expect_equal(
    unlist(app$get_js(paste0(
      "Array.from(document.querySelectorAll(",
      "'.spatial-background-single-name'))",
      ".map(function(el) { return el.textContent.trim(); })"
    ))),
    rep("Embedded histology", 2)
  )
  expect_true(app$get_js(paste0(
    "['auto','none'].every(function(mode) { return !!document.querySelector(",
    "'[data-spatial-background-mode=\"' + mode + '\"]'); })"
  )))
  expect_true(app$get_js(
    "!!document.getElementById('spatial_projection_background_customize')"
  ))

  ## The second donorA option is a visibly different image, not a duplicate
  ## label pointing at the opening background.
  app$run_js(
    "document.getElementById('spatial_projection_background_customize').click();"
  )
  app$run_js(paste0(
    "window.__spatialRoseImage = document.getElementById(",
    "'spatial_projection_background').dataset.backgroundImage;"
  ))
  app$set_inputs(spatial_projection_background_image = "__embedded__2")
  app$wait_for_js(
    paste0(
      "document.getElementById('spatial_projection_background')",
      ".dataset.backgroundImage !== window.__spatialRoseImage"
    ),
    timeout = 30000
  )

  ## Auto is the opening mode and displays each section's sole image.
  app$wait_for_js(
    paste0(
      "['spatial_projection','spatial_projection_002',",
      "'spatial_projection_003'].every(function(id) { var bg = ",
      "document.getElementById(id + '_background'); return bg && ",
      "bg.dataset.backgroundImage.indexOf('data:image/') === 0; })"
    ),
    timeout = 30000
  )

  widths <- unlist(app$get_js(
    paste0(
      "Array.from(document.querySelectorAll('.spatial-projection-item'))",
      ".map(function(el) { return el.getBoundingClientRect().width; })"
    )
  ))
  expect_true(all(widths >= 300))

  ## None temporarily hides every image without presenting per-panel dropdowns.
  app$run_js(
    "document.querySelector('[data-spatial-background-mode=\"none\"]').click();"
  )
  app$wait_for_js(
    paste0(
      "['spatial_projection','spatial_projection_002',",
      "'spatial_projection_003'].every(function(id) { var bg = ",
      "document.getElementById(id + '_background'); return bg && ",
      "!bg.dataset.backgroundImage; })"
    ),
    timeout = 30000
  )
  app$set_inputs(
    spatial_projection_to_display = c("donorA tissue", "donorB tissue")
  )
  app$wait_for_js(
    paste0(
      "document.querySelectorAll('.spatial-background-row').length === 2 && ",
      "document.querySelector('[data-spatial-background-mode=\"none\"]')",
      ".classList.contains('is-active')"
    ),
    timeout = 30000
  )
  app$set_inputs(
    spatial_projection_to_display = c(
      "donorA tissue",
      "donorB tissue",
      "donorC tissue"
    )
  )
  app$wait_for_js(
    "document.querySelectorAll('.spatial-background-row').length === 3",
    timeout = 30000
  )

  ## Customize restores the per-panel choices. Turning B off leaves A and C on.
  app$run_js(
    "document.getElementById('spatial_projection_background_customize').click();"
  )
  app$set_inputs(spatial_projection_002_background_image = FALSE)
  app$wait_for_js(
    paste0(
      "(function(){var a=document.getElementById(",
      "'spatial_projection_background').dataset.backgroundImage;",
      "var b=document.getElementById(",
      "'spatial_projection_002_background').dataset.backgroundImage;",
      "var c=document.getElementById(",
      "'spatial_projection_003_background').dataset.backgroundImage;",
      "return a.indexOf('data:image/')===0 && !b && ",
      "c.indexOf('data:image/')===0;})()"
    ),
    timeout = 30000
  )
  expect_true(app$get_js(paste0(
    "!document.getElementById('spatial_projection_background')",
    ".dataset.backgroundImage === false"
  )))

  app$set_window_size(width = 520, height = 900)
  app$wait_for_js(
    paste0(
      "(function(){var c=Array.from(document.querySelectorAll(",
      "'.spatial-projection-item'));return c.length===3 && ",
      "new Set(c.map(function(el){return Math.round(el.getBoundingClientRect()",
      ".left);})).size===1;})()"
    ),
    timeout = 10000
  )
})
