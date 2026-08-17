library(shinytest2)

test_that("long queued upload rows keep Cancel inside the dataset rail", {
  skip_if_not(identical(Sys.getenv("CEREBRO_RUN_BROWSER_TESTS"), "true"))
  app_dir <- builder_profile_inst_path("builder")
  local_app_support(app_dir)
  app <- AppDriver$new(
    app_dir,
    name = "builder_upload_row_geometry",
    width = 942,
    height = 850,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)

  app$run_js(paste0(
    "const queue = document.getElementById('ds_client_import_queue');",
    "const row = document.createElement('div');",
    "row.className = 'ds ds--import ds--client-upload geometry-fixture';",
    "row.dataset.loadState = 'awaiting_dispatch';",
    "const body = document.createElement('span'); body.className = 'ds-body';",
    "const name = document.createElement('span'); name.className = 'nm';",
    "name.textContent = '17.0_duraFibro_sample_roi_name_that_must_truncate';",
    "const status = document.createElement('span');",
    "status.className = 'builder-import-status';",
    "status.textContent = 'Preparing dataset settings…';",
    "body.append(name, status);",
    "const dot = document.createElement('span'); dot.className = 'ds-state-dot';",
    "const cancel = document.createElement('button');",
    "cancel.className = 'btn btn-remove-soft builder-cancel-client-import';",
    "cancel.textContent = 'Cancel';",
    "row.append(body, dot, cancel); queue.appendChild(row);"
  ))
  geometry <- app$get_js(paste0(
    "(() => {",
    "const rail = document.querySelector('.rail').getBoundingClientRect();",
    "const row = document.querySelector('.geometry-fixture').getBoundingClientRect();",
    "const cancel = document.querySelector('.geometry-fixture ",
    ".builder-cancel-client-import');",
    "const button = cancel.getBoundingClientRect();",
    "return {rowRight:row.right, railRight:rail.right, ",
    "buttonRight:button.right, position:getComputedStyle(cancel).position};",
    "})()"
  ))

  expect_lte(geometry$rowRight, geometry$railRight)
  expect_lte(geometry$buttonRight, geometry$railRight)
  expect_identical(geometry$position, "static")
})
