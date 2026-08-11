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
  expect_true(app$get_js(paste0(
    "document.querySelector('.example-btn[data-ex=all_content]').classList",
    ".contains('is-taken') && ",
    "document.getElementById('workbench').textContent.trim().length > 0 && ",
    "window.Shiny.shinyapp.$socket.readyState === WebSocket.OPEN"
  )))

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
