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
    "row.append(body, dot, cancel); queue.appendChild(row);",
    "const activeHost = document.getElementById('ds_import_list');",
    "const activeRow = document.createElement('div');",
    "activeRow.className = 'ds ds--import geometry-active-fixture';",
    "const activeActions = document.createElement('div');",
    "activeActions.className = 'ds-actions';",
    "const activeCancel = document.createElement('button');",
    "activeCancel.className = ",
    "'ds-del btn btn-remove-soft builder-cancel-import builder-remove-import';",
    "activeCancel.textContent = 'Cancel';",
    "activeActions.appendChild(activeCancel);",
    "activeRow.appendChild(activeActions); activeHost.appendChild(activeRow);"
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

  styles <- app$get_js(paste0(
    "(() => {",
    "const props = ['backgroundColor', 'color', 'borderRadius', 'minHeight', ",
    "'paddingTop', 'paddingRight', 'paddingBottom', 'paddingLeft', ",
    "'fontSize', 'fontWeight'];",
    "const read = selector => { const style = getComputedStyle(",
    "document.querySelector(selector)); return Object.fromEntries(",
    "props.map(prop => [prop, style[prop]])); };",
    "return {",
    "client: read('.geometry-fixture .builder-cancel-client-import'),",
    "active: read('.geometry-active-fixture .builder-cancel-import')",
    "};",
    "})()"
  ))

  expect_identical(styles$active, styles$client)
})
