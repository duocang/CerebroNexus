viewer_benchmark_scope <- function() {
  file <- test_path("..", "bench", "benchmark_viewer_1m_pages.R")
  skip_if_not(file.exists(file), "benchmark tree not present")
  scope <- new.env(parent = globalenv())
  sys.source(test_path("..", "bench", "viewer_1m_page_protocol.R"), scope)
  for (expression in parse(file)) {
    if (
      is.call(expression) &&
        as.character(expression[[1]])[1] %in% c("<-", "=") &&
        is.call(expression[[3]]) &&
        identical(expression[[3]][[1]], as.name("function"))
    ) {
      eval(expression, scope)
    }
  }
  scope
}

test_that("page benchmarks reject stale readiness and count actual websocket bytes", {
  scope <- viewer_benchmark_scope()
  skip_if_not_installed("chromote")
  browser <- chromote::ChromoteSession$new()
  on.exit(browser$close(), add = TRUE)
  js <- function(code) {
    result <- browser$Runtime$evaluate(code, returnByValue = TRUE)
    if (!is.null(result$exceptionDetails)) {
      stop(result$exceptionDetails$exception$description)
    }
    result$result$value
  }
  app <- list(run_js = js, get_js = js)
  js(
    "document.body.innerHTML='<button id=go>Open</button><div id=shiny-tab-overview class=active><div id=host><canvas data-point-count=2></canvas></div></div>';window.__cerebroPageBenchExpectedFingerprint='fp';window.emitReady=detail=>window.dispatchEvent(new CustomEvent('cerebro:specialist-state',{detail}));"
  )
  page <- scope$canvas_page(
    "overview",
    "#host",
    "overview_projection",
    expected_points = 2
  )
  scope$arm_and_click_page(app, page, "#go")
  expect_false(js(scope$page_active_js(page)))
  for (detail in list(
    list(
      viewId = "overview_projection",
      benchmarkGeneration = 0,
      datasetFingerprint = "fp",
      renderedPointCount = 2
    ),
    list(
      viewId = "overview_projection",
      benchmarkGeneration = 1,
      datasetFingerprint = "old",
      renderedPointCount = 2
    ),
    list(
      viewId = "overview_projection",
      benchmarkGeneration = 1,
      datasetFingerprint = "fp",
      renderedPointCount = 1
    )
  )) {
    js(sprintf("emitReady(%s)", jsonlite::toJSON(detail, auto_unbox = TRUE)))
    expect_false(js(scope$page_active_js(page)))
  }
  js(
    "emitReady({viewId:'overview_projection',benchmarkGeneration:1,datasetFingerprint:'fp',renderedPointCount:2})"
  )
  expect_true(js(scope$page_active_js(page)))
  expect_true(js(page$correctness))
  js("document.querySelector('canvas').dataset.pointCount='3'")
  expect_false(js(page$correctness))
  scope$arm_and_click_page(app, page, "#go")
  js(
    "emitReady({viewId:'overview_projection',benchmarkGeneration:1,datasetFingerprint:'fp',renderedPointCount:2})"
  )
  expect_false(js(scope$page_active_js(page)))

  js(
    "window.socket=new EventTarget();socket.sent=[];socket.send=function(data){this.sent.push(data)};window.originalSend=socket.send;window.Shiny={shinyapp:{$socket:socket}};"
  )
  scope$skip_socket_meter <- FALSE
  scope$start_socket_meter(app)
  expect_error(scope$start_socket_meter(app), "already active")
  js(
    "socket.send('汉字');socket.send(new Uint8Array([1,2,3]));socket.dispatchEvent(new MessageEvent('message',{data:'é'}));socket.dispatchEvent(new MessageEvent('message',{data:new Blob(['abcd'])}));"
  )
  expect_equal(scope$stop_socket_meter(app), list(sent = 9, received = 6))
  expect_true(js("socket.send===originalSend && socket.sent.length===2"))
  expect_error(scope$stop_socket_meter(app), "not active")
  scope$start_socket_meter(app)
  scope$cleanup_socket_meter(app)
  expect_true(js(
    "socket.send===originalSend && window.__cerebroPageBenchSocketMeter===null"
  ))
  scope$skip_socket_meter <- TRUE
  scope$start_socket_meter(app)
  expect_equal(scope$stop_socket_meter(app), list(sent = 0, received = 0))
})

test_that("benchmark browser retries clean up failures and preserve real errors", {
  scope <- viewer_benchmark_scope()
  scope$browser_start_attempts <- 3L
  starts <- stops <- 0L
  error_message <- "Chrome debugging port not open after 10 seconds"
  ready <- list(ready = TRUE)
  testthat::local_mocked_bindings(
    AppDriver = list(new = function(...) {
      starts <<- starts + 1L
      if (starts == 1L) {
        error <- simpleError(error_message)
        error$app <- list(stop = function() stops <<- stops + 1L)
        stop(error)
      }
      ready
    }),
    .package = "shinytest2"
  )
  result <- scope$start_app_driver("app", "test")
  expect_identical(result$app, ready)
  expect_equal(result$attempts, 2)
  expect_equal(stops, 1)
  expect_length(result$errors, 1)
  starts <- stops <- 0L
  error_message <- "application failed to initialize"
  expect_error(
    scope$start_app_driver("app", "test"),
    error_message,
    fixed = TRUE
  )
  expect_equal(starts, 1)
  expect_equal(stops, 1)
})
