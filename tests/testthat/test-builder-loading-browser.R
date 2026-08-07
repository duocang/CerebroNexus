library(shinytest2)

test_that("Builder stays visible while a dataset loads", {
  skip_if_not(identical(Sys.getenv("CEREBRO_RUN_BROWSER_TESTS"), "true"))
  app_dir <- builder_profile_inst_path("builder")
  local_app_support(app_dir)
  app <- AppDriver$new(
    app_dir,
    name = "builder_loading_non_blank",
    width = 1280,
    height = 850,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)

  expect_true(app$get_js(paste0(
    "document.querySelector('.builder-shell') !== null && ",
    "document.querySelector('.example-btn[data-ex=basic_pbmc]') !== null && ",
    "document.querySelector('.builder-empty-state') !== null && ",
    "document.getElementById('workbench').textContent.trim().length > 0"
  )))

  app$click(selector = ".example-btn[data-ex=basic_pbmc]")
  app$wait_for_js(
    paste0(
      "document.querySelector('.builder-loading-stage') !== null && ",
      "document.querySelector('.ds--import') !== null"
    ),
    timeout = 10000
  )
  expect_true(app$get_js(paste0(
    "document.querySelector('.builder-shell').getClientRects().length > 0 && ",
    "document.getElementById('workbench').textContent.trim().length > 0 && ",
    "document.querySelector('.builder-loading-status').textContent.trim().length > 0 && ",
    "document.documentElement.scrollHeight > document.documentElement.clientHeight && ",
    "document.getElementById('build').disabled === true"
  )))

  app$wait_for_js(
    paste0(
      "document.querySelector('.ds-pick[aria-current=true]') !== null && ",
      "document.querySelector('#core-stage') !== null && ",
      "document.querySelector('#inspect-stage') !== null && ",
      "document.querySelector('.builder-loading-stage') === null"
    ),
    timeout = 60000
  )
  expect_true(app$get_js(paste0(
    "document.querySelector('.example-btn[data-ex=basic_pbmc]').classList",
    ".contains('is-taken') && ",
    "document.getElementById('workbench').textContent.trim().length > 0 && ",
    "window.Shiny.shinyapp.$socket.readyState === WebSocket.OPEN"
  )))

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
