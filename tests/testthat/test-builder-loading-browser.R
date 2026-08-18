library(shinytest2)

test_that("Builder stays visible while a dataset loads", {
  skip_if_not(identical(Sys.getenv("CEREBRO_RUN_BROWSER_TESTS"), "true"))
  app_dir <- builder_profile_inst_path("builder")
  local_app_support(app_dir)
  app <- AppDriver$new(
    app_dir,
    name = "builder_loading_non_blank",
    width = 1920,
    height = 850,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)

  app$get_chromote_session()$set_viewport_size(width = 1920, height = 850)
  app$wait_for_js(
    "window.innerWidth === 1920 && window.innerHeight === 850",
    timeout = 10000
  )

  expect_true(app$get_js(paste0(
    "document.querySelector('.builder-shell') !== null && ",
    "document.querySelector('.example-btn[data-ex=all_content]') !== null && ",
    "document.querySelector('.builder-empty-state') !== null && ",
    "document.getElementById('workbench').textContent.trim().length > 0"
  )))

  geometry <- app$get_js(paste0(
    "(() => {",
    "const rail = document.querySelector('.rail').getBoundingClientRect();",
    "const pane = document.getElementById('pane').getBoundingClientRect();",
    "const viewport = document.documentElement.clientWidth;",
    "return {",
    "left: rail.left, right: viewport - pane.right, ",
    "paneWidth: pane.width, viewport: viewport, ",
    "documentWidth: document.documentElement.scrollWidth",
    "};",
    "})()"
  ))
  expect_lte(abs(geometry$left - 26), 2)
  expect_lte(abs(geometry$right - 26), 2)
  expect_gt(geometry$paneWidth, 1200)
  expect_lte(geometry$documentWidth, geometry$viewport + 1)

  builder_browser_wait_for_example_ready(app)
  app$click(selector = ".example-btn[data-ex=all_content]")
  app$wait_for_js(
    paste0(
      "document.querySelector('.builder-loading-stage') !== null && ",
      "document.querySelector('.ds--import') !== null && ",
      "document.querySelector('.builder-shell').getClientRects().length > 0 && ",
      "document.getElementById('workbench').textContent.trim().length > 0 && ",
      "document.querySelector('.builder-loading-status')",
      ".textContent.trim().length > 0 && ",
      "document.querySelector('.actionbar') === null && ",
      "document.getElementById('build') === null && ",
      "document.getElementById('make_app') === null && ",
      "document.getElementById('continue_to_review') === null"
    ),
    timeout = 10000
  )
  loading_colours <- app$get_js(paste0(
    "(() => {",
    "const row = document.querySelector('.ds--import');",
    "const dot = row.querySelector('.ds-state-dot');",
    "return {background:getComputedStyle(row).backgroundColor, ",
    "dot:getComputedStyle(dot).color, ",
    "dotWidth:getComputedStyle(dot).width, dotHeight:getComputedStyle(dot).height};",
    "})()"
  ))
  expect_identical(loading_colours$background, "rgb(238, 244, 251)")
  expect_identical(loading_colours$dot, "rgb(47, 111, 214)")

  app$wait_for_js(
    paste0(
      "document.querySelector('.ds-pick[aria-current=true]') !== null && ",
      "document.querySelector('#core-stage') !== null && ",
      "document.querySelector('#inspect-stage') !== null && ",
      "document.querySelector('.builder-loading-stage') === null && ",
      "document.querySelector('.builder-stage-footer') !== null && ",
      "document.getElementById('make_app') === null && ",
      "document.querySelectorAll('#continue_to_review').length === 1"
    ),
    timeout = 60000
  )
  builder_browser_dismiss_project_offer(app)
  expect_true(app$get_js(paste0(
    "document.querySelector('.example-btn[data-ex=all_content]').classList",
    ".contains('is-taken') && ",
    "document.getElementById('workbench').textContent.trim().length > 0 && ",
    "window.Shiny.shinyapp.$socket.readyState === WebSocket.OPEN"
  )))
  ready_colours <- app$get_js(paste0(
    "(() => {",
    "const row = document.querySelector('.ds.ds--ready.is-active');",
    "const stamp = row.querySelector('.ds-ready-stamp');",
    "return {background:getComputedStyle(row).backgroundColor, ",
    "marker:getComputedStyle(row, '::before').backgroundColor, ",
    "stampText:stamp.textContent.trim().replace(/\\s+/g, ' '), ",
    "stampColor:getComputedStyle(stamp).color, ",
    "stampBorder:getComputedStyle(stamp).borderColor, ",
    "stampTransform:getComputedStyle(stamp).transform, ",
    "readyDot:row.querySelector('.ds-ready-dot')};",
    "})()"
  ))
  expect_identical(ready_colours$background, "rgb(255, 244, 236)")
  expect_identical(ready_colours$marker, "rgb(249, 115, 22)")
  expect_identical(ready_colours$stampText, "NEEDS CHECK")
  expect_identical(ready_colours$stampColor, "rgb(198, 40, 40)")
  expect_identical(ready_colours$stampBorder, "rgb(198, 40, 40)")
  expect_false(identical(ready_colours$stampTransform, "none"))
  expect_null(ready_colours$readyDot)

  for (layout in list(
    list(width = 1920L, height = 850L, gutter = 26),
    list(width = 768L, height = 800L, gutter = 24),
    list(width = 390L, height = 844L, gutter = 16)
  )) {
    app$get_chromote_session()$set_viewport_size(
      width = layout$width,
      height = layout$height
    )
    app$wait_for_js(
      sprintf(
        "window.innerWidth === %d && window.innerHeight === %d",
        layout$width,
        layout$height
      ),
      timeout = 10000
    )
    aligned <- app$get_js(paste0(
      "(() => {",
      "const shell = document.querySelector('.builder-shell');",
      "const viewport = document.documentElement.clientWidth;",
      "const shellStyle = getComputedStyle(shell);",
      "return {",
      "shellLeft: parseFloat(shellStyle.paddingLeft), ",
      "shellRight: parseFloat(shellStyle.paddingRight), ",
      "viewport: viewport, ",
      "documentWidth: document.documentElement.scrollWidth",
      "};",
      "})()"
    ))
    expect_lte(abs(aligned$shellLeft - layout$gutter), 2)
    expect_lte(abs(aligned$shellRight - layout$gutter), 2)
    expect_lte(aligned$documentWidth, aligned$viewport + 1)
  }

  logs <- app$get_logs()
  browser_failures <- logs[
    as.character(logs$location) == "chromote" &
      tolower(as.character(logs$level)) %in%
        c("error", "warning", "assert", "throw"),
    ,
    drop = FALSE
  ]
  expect_identical(
    nrow(browser_failures),
    0L,
    info = paste(browser_failures$message, collapse = "\n")
  )
})

builder_browser_choose_files <- function(app, files) {
  builder_browser_wait_for_worker_ready(app)
  chromote <- app$get_chromote_session()
  chromote$Page$enable()
  chromote$Page$setInterceptFileChooserDialog(enabled = TRUE)
  chooser <- chromote$Page$fileChooserOpened(wait_ = FALSE)
  chromote$Runtime$evaluate(
    expression = "document.getElementById('builder_add_datasets').click()",
    userGesture = TRUE,
    awaitPromise = FALSE,
    wait_ = FALSE
  )
  opened <- chromote$wait_for(chooser)
  expect_true(isTRUE(opened$mode %in% c("selectMultiple", "select")))
  chromote$DOM$setFileInputFiles(
    files = as.list(normalizePath(files)),
    backendNodeId = opened$backendNodeId
  )
  invisible(opened)
}

test_that("multi-file selection stays FIFO through a single transport", {
  skip_if_not(identical(Sys.getenv("CEREBRO_RUN_BROWSER_TESTS"), "true"))
  app_dir <- builder_profile_inst_path("builder")
  local_app_support(app_dir)
  app <- AppDriver$new(
    app_dir,
    name = "builder_loading_serial_files",
    width = 390,
    height = 844,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)

  app$get_js(paste0(
    "window.__builderFirstReadyRow = null;",
    "window.__builderFirstReadyRowDetached = false;",
    "window.__builderRailIdentityObserver = new MutationObserver(() => {",
    "const ready = document.querySelectorAll('#ds_ready_list > .ds[data-ds]');",
    "const first = ready.item(0);",
    "if (!window.__builderFirstReadyRow && ready.length >= 2 && first) ",
    "window.__builderFirstReadyRow = first;",
    "if (window.__builderFirstReadyRow && !window.__builderFirstReadyRow.isConnected) ",
    "window.__builderFirstReadyRowDetached = true;",
    "});",
    "window.__builderRailIdentityObserver.observe(",
    "document.getElementById('ds_ready_list'), {childList:true});",
    "true;"
  ))

  fixture_dir <- withr::local_tempdir()
  fixture_source <- builder_profile_inst_path(
    "extdata",
    "examples",
    "pbmc_seurat.rds"
  )
  fixture_a <- file.path(fixture_dir, "pbmc-a.rds")
  fixture_b <- file.path(fixture_dir, "pbmc-b.rds")
  fixture_c <- file.path(fixture_dir, "pbmc-c.rds")
  expect_true(file.copy(fixture_source, fixture_a))
  expect_true(file.copy(fixture_source, fixture_b))
  expect_true(file.copy(fixture_source, fixture_c))

  app$click(selector = ".rail-summary")
  app$wait_for_js(
    paste0(
      "document.querySelector('.rail').classList.contains('is-manager-open') && ",
      "document.querySelector('.rail').classList.contains('is-manager-visible')"
    ),
    timeout = 10000
  )
  builder_browser_choose_files(app, c(fixture_a, fixture_b, fixture_c))

  app$wait_for_js(
    paste0(
      "document.getElementById('dataset_files').files.length === 1 && ",
      "document.querySelectorAll('#ds_client_import_queue ",
      ".ds--client-upload').length >= 1"
    ),
    timeout = 10000
  )
  queue <- app$get_js(paste0(
    "Array.from(document.querySelectorAll('#ds_client_import_queue ",
    ".ds--client-upload')).map(row => ({",
    "name: row.querySelector('.nm').textContent, ",
    "state: row.dataset.loadState, ",
    "status: row.querySelector('.builder-import-status').textContent",
    "}))"
  ))
  queued_tail <- queue[[length(queue)]]
  expect_identical(queued_tail$name, "pbmc-c.rds")
  expect_true(queued_tail$state %in% c("queued", "awaiting_accept"))
  expect_match(queued_tail$status, "Waiting", fixed = TRUE)

  app$wait_for_js(
    paste0(
      "document.querySelector('#ds_ready_list .builder-pick') !== null && ",
      "document.querySelector('#ds_import_list .builder-pick-import') !== null"
    ),
    timeout = 120000
  )
  builder_browser_dismiss_project_offer(app)
  app$run_js(
    "document.querySelector('#ds_ready_list .builder-pick').click();"
  )
  app$wait_for_js(
    paste0(
      "document.querySelector('.builder-loading-stage') === null && ",
      "document.querySelector('#core-stage') !== null"
    ),
    timeout = 30000
  )
  app$run_js(
    "document.querySelector('#ds_import_list .builder-pick-import').click();"
  )
  app$wait_for_js(
    paste0(
      "document.querySelector('#ds_import_list .builder-pick-import') === null || ",
      "(document.querySelector('.builder-loading-stage') !== null && ",
      "document.querySelector('#ds_import_list ",
      ".builder-pick-import[aria-current=true]') !== null)"
    ),
    timeout = 30000
  )
  app$wait_for_js(
    paste0(
      "!document.querySelector('.rail').classList.contains('is-manager-open') && ",
      "!document.querySelector('.rail-manager-backdrop')",
      ".classList.contains('is-open')"
    ),
    timeout = 10000
  )
  expect_false(app$get_js(
    "document.body.classList.contains('builder-dialog-open')"
  ))

  builder_with_browser_diagnostics(
    app,
    "builder-loading-serial-files",
    app$wait_for_js(
      paste0(
        "document.querySelectorAll('#ds_ready_list .ds--ready').length === 3 && ",
        "document.getElementById('ds_client_import_queue').children.length === 0"
      ),
      timeout = 120000
    )
  )
  expect_true(app$get_js(paste0(
    "Boolean(window.__builderFirstReadyRow) && ",
    "window.__builderFirstReadyRow.isConnected && ",
    "window.__builderFirstReadyRowDetached === false"
  )))
  app$get_js("window.__builderRailIdentityObserver.disconnect(); true;")
})

test_that("a failed import automatically continues to the next file", {
  skip_if_not(identical(Sys.getenv("CEREBRO_RUN_BROWSER_TESTS"), "true"))
  app_dir <- builder_profile_inst_path("builder")
  local_app_support(app_dir)
  app <- AppDriver$new(
    app_dir,
    name = "builder_loading_failure_continues",
    width = 768,
    height = 800,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)

  fixture_dir <- withr::local_tempdir()
  invalid <- file.path(fixture_dir, "invalid-sce.rds")
  valid <- file.path(fixture_dir, "valid-seurat.rds")
  expect_true(file.copy(
    builder_profile_inst_path("extdata", "examples", "pbmc_SCE.rds"),
    invalid
  ))
  expect_true(file.copy(
    builder_profile_inst_path("extdata", "examples", "pbmc_seurat.rds"),
    valid
  ))

  builder_browser_choose_files(app, c(invalid, valid))
  builder_with_browser_diagnostics(
    app,
    "builder-loading-failure-continues",
    app$wait_for_js(
      paste0(
        "document.getElementById('ds_client_import_queue').children.length === 0 && ",
        "document.getElementById('builder-live-status').textContent",
        ".includes('valid-seurat is ready')"
      ),
      timeout = 120000
    )
  )
})

test_that("a waiting file can be cancelled without cancelling the active file", {
  skip_if_not(identical(Sys.getenv("CEREBRO_RUN_BROWSER_TESTS"), "true"))
  app_dir <- builder_profile_inst_path("builder")
  local_app_support(app_dir)
  app <- AppDriver$new(
    app_dir,
    name = "builder_loading_cancel_waiting",
    width = 390,
    height = 844,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)

  fixture_dir <- withr::local_tempdir()
  fixture <- builder_profile_inst_path(
    "extdata",
    "examples",
    "pbmc_seurat.rds"
  )
  first <- file.path(fixture_dir, "first.rds")
  second <- file.path(fixture_dir, "second.rds")
  expect_true(file.copy(fixture, first))
  expect_true(file.copy(fixture, second))

  builder_browser_choose_files(app, c(first, second))
  app$wait_for_js(
    "document.querySelector('.builder-cancel-client-import') !== null",
    timeout = 10000
  )
  app$run_js(paste0(
    "document.querySelectorAll('.builder-cancel-client-import')",
    ".item(document.querySelectorAll('.builder-cancel-client-import').length - 1)",
    ".click();"
  ))
  builder_with_browser_diagnostics(
    app,
    "builder-loading-cancel-waiting",
    app$wait_for_js(
      paste0(
        "document.querySelectorAll('#ds_ready_list .ds--ready').length === 1 && ",
        "document.getElementById('ds_client_import_queue').children.length === 0"
      ),
      timeout = 120000
    )
  )
})

test_that("rapid repeated drops preserve every file in FIFO order", {
  skip_if_not(identical(Sys.getenv("CEREBRO_RUN_BROWSER_TESTS"), "true"))
  app_dir <- builder_profile_inst_path("builder")
  local_app_support(app_dir)
  app <- AppDriver$new(
    app_dir,
    name = "builder_loading_rapid_drop",
    width = 768,
    height = 800,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)

  app$run_js(paste0(
    "(() => {",
    "const trigger = document.getElementById('builder_add_datasets');",
    "const drop = names => {",
    "const transfer = new DataTransfer();",
    "names.forEach(name => transfer.items.add(new File(",
    "[new Uint8Array([98,114,111,107,101,110])], name, ",
    "{type:'application/octet-stream'})));",
    "trigger.dispatchEvent(new DragEvent('drop', {",
    "bubbles:true, cancelable:true, dataTransfer:transfer",
    "}));",
    "};",
    "const started = performance.now();",
    "drop(['rapid-a.rds','rapid-b.rds']);",
    "drop(['rapid-c.rds','rapid-d.rds']);",
    "window.__builderRapidDropElapsed = performance.now() - started;",
    "})();"
  ))
  app$wait_for_js(
    paste0(
      "document.querySelectorAll('#ds_client_import_queue .nm').length >= 3 && ",
      "window.__builderRapidDropElapsed < 100"
    ),
    timeout = 10000
  )
  expect_identical(
    unlist(app$get_js(paste0(
      "Array.from(document.querySelectorAll('#ds_list .nm'))",
      ".map(node => node.textContent.trim())"
    ))),
    c("rapid-a.rds", "rapid-b.rds", "rapid-c.rds", "rapid-d.rds")
  )
  app$wait_for_js(
    "document.getElementById('ds_client_import_queue').children.length === 0",
    timeout = 120000
  )
})

test_that("disconnect pauses the queue until server state is synchronized", {
  skip_if_not(identical(Sys.getenv("CEREBRO_RUN_BROWSER_TESTS"), "true"))
  app_dir <- builder_profile_inst_path("builder")
  local_app_support(app_dir)
  app <- AppDriver$new(
    app_dir,
    name = "builder_loading_disconnect_sync",
    width = 768,
    height = 800,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)

  fixture_dir <- withr::local_tempdir()
  fixture <- builder_profile_inst_path(
    "builder",
    "fixtures",
    "all_content.rds"
  )
  first <- file.path(fixture_dir, "sync-a.rds")
  second <- file.path(fixture_dir, "sync-b.rds")
  expect_true(file.copy(fixture, first))
  expect_true(file.copy(fixture, second))
  builder_browser_choose_files(app, c(first, second))
  app$wait_for_js(
    paste0(
      "document.getElementById('ds_client_import_queue').children.length === 1 && ",
      "document.querySelector('#ds_client_import_queue .nm')",
      ".textContent.trim() === 'sync-b.rds'"
    ),
    timeout = 30000
  )

  app$run_js("document.dispatchEvent(new Event('shiny:disconnected')); ")
  app$wait_for_js(
    paste0(
      "document.querySelector('#ds_client_import_queue [data-load-state=paused]')",
      " !== null && ",
      "document.getElementById('ds_client_import_queue').textContent",
      ".includes('Connection lost')"
    ),
    timeout = 10000
  )
  app$run_js("document.dispatchEvent(new Event('shiny:connected')); ")
  builder_with_browser_diagnostics(
    app,
    "builder-loading-disconnect-sync",
    app$wait_for_js(
      paste0(
        "document.querySelectorAll('#ds_ready_list .ds--ready').length === 2 && ",
        "document.getElementById('ds_client_import_queue').children.length === 0"
      ),
      timeout = 120000
    )
  )
})
