async_source_viewer_root <- testthat::test_path("..", "..", "inst", "viewer")
async_viewer_root <- if (
  file.exists(file.path(
    async_source_viewer_root,
    "async_runtime.R"
  ))
) {
  async_source_viewer_root
} else {
  system.file("viewer", package = "CerebroNexus")
}
async_runtime_file <- file.path(async_viewer_root, "async_runtime.R")

test_that("the standalone viewer carries its async runtime", {
  runtime <- async_runtime_file
  expect_true(file.exists(runtime))
})

source(async_runtime_file, local = TRUE)

test_that("async configuration is bounded and overrideable", {
  config <- cerebro_async_config()

  expect_true(config$enabled)
  expect_gte(config$workers, 1L)
  expect_lte(config$workers, 2L)
  expect_identical(config$compute, "cerebro")
  expect_gt(config$queue_memory_mb, 0)
  expect_gt(config$timeout_ms, 0L)

  override <- cerebro_async_config(list(
    enabled = FALSE,
    workers = 7L,
    queue_memory_mb = 512,
    timeout_ms = 9000L,
    compute = "viewer"
  ))
  expect_false(override$enabled)
  expect_identical(override$workers, 7L)
  expect_identical(override$queue_memory_mb, 512)
  expect_identical(override$timeout_ms, 9000L)
  expect_identical(override$compute, "viewer")
})

test_that("invalid async configuration fails before daemon startup", {
  expect_error(cerebro_async_config(list(workers = 0L)), "workers")
  expect_error(
    cerebro_async_config(list(queue_memory_mb = -1)),
    "queue_memory_mb"
  )
  expect_error(cerebro_async_config(list(timeout_ms = 0L)), "timeout_ms")
  expect_error(cerebro_async_config(list(compute = "")), "compute")
})

test_that("latest-value cache bounds and worker errors are validated", {
  expect_error(
    cerebro_async_latest_value(list(), identity, max_entries = 0L),
    "max_entries"
  )
  expect_identical(cerebro_async_error_message(simpleError("failed")), "failed")
  expect_identical(
    cerebro_async_error_message("remote failed"),
    "remote failed"
  )
})

test_that("generation tokens reject stale results", {
  generation <- cerebro_async_generation()
  first <- generation[["next"]]()
  second <- generation[["next"]]()

  expect_false(generation$current(first))
  expect_true(generation$current(second))
  expect_identical(generation$value(), second)
})

test_that("cache keys stay bounded for large cell selections", {
  cells <- sprintf("cell-%08d", seq_len(20000L))
  first <- cerebro_async_cache_key("dataset", cells)
  second <- cerebro_async_cache_key("dataset", rev(cells))

  expect_lt(nchar(first, type = "bytes"), 100L)
  expect_false(identical(first, second))
  expect_identical(first, cerebro_async_cache_key("dataset", cells))
})

test_that("disabled execution uses the same worker locally", {
  value <- cerebro_async_execute(
    function(x, y) x + y,
    list(x = 2, y = 5)
  )

  expect_identical(value, 7)
})

test_that("source-call workers load standalone helpers in an isolated environment", {
  helper <- tempfile(fileext = ".R")
  on.exit(unlink(helper), add = TRUE)
  writeLines("triple <- function(x) x * 3", helper)

  value <- cerebro_async_source_call(
    root = dirname(helper),
    files = basename(helper),
    function_name = "triple",
    args = list(x = 4)
  )

  expect_identical(value, 12)
  expect_identical(environment(cerebro_async_source_call), baseenv())
})

test_that("namespace-call workers do not capture the calling session", {
  value <- cerebro_async_namespace_call(
    package = "stats",
    function_name = "median",
    args = list(x = c(1, 3, 8))
  )

  expect_identical(value, 3)
  expect_identical(environment(cerebro_async_namespace_call), baseenv())
})

test_that("runtime owns and releases only daemons it starts", {
  calls <- new.env(parent = emptyenv())
  calls$start <- list()
  calls$stop <- list()
  backend <- list(
    daemons_set = function(compute) FALSE,
    start = function(config) {
      calls$start[[length(calls$start) + 1L]] <- config
      invisible(TRUE)
    },
    stop = function(compute) {
      calls$stop[[length(calls$stop) + 1L]] <- compute
      invisible(TRUE)
    },
    submit = function(worker, args, config) list(worker = worker, args = args)
  )
  config <- cerebro_async_config(list(workers = 3L, compute = "test-profile"))

  state <- cerebro_async_init(config, backend = backend)
  expect_true(state$active)
  expect_true(state$owned)
  expect_length(calls$start, 0L)
  cerebro_async_submit(function() NULL)
  expect_length(calls$start, 1L)
  expect_identical(calls$start[[1L]], config)

  cerebro_async_shutdown()
  expect_identical(calls$stop, list("test-profile"))
  expect_false(cerebro_async_status()$active)
})

test_that("owned daemons start lazily on the first submitted task", {
  starts <- 0L
  backend <- list(
    daemons_set = function(compute) FALSE,
    start = function(config) starts <<- starts + 1L,
    stop = function(compute) invisible(TRUE),
    submit = function(worker, args, config) do.call(worker, args)
  )
  cerebro_async_init(cerebro_async_config(), backend = backend)
  withr::defer(cerebro_async_shutdown())

  expect_identical(starts, 0L)
  expect_identical(cerebro_async_submit(function() 7L), 7L)
  expect_identical(starts, 1L)
})

test_that("runtime preserves an existing daemon profile", {
  stopped <- FALSE
  backend <- list(
    daemons_set = function(compute) TRUE,
    start = function(config) stop("must not replace an existing profile"),
    stop = function(compute) stopped <<- TRUE,
    submit = function(worker, args, config) list(worker = worker, args = args)
  )

  state <- cerebro_async_init(cerebro_async_config(), backend = backend)
  expect_true(state$active)
  expect_false(state$owned)
  cerebro_async_shutdown()
  expect_false(stopped)
})

test_that("disabled runtime executes locally and never starts daemons", {
  backend <- list(
    daemons_set = function(compute) FALSE,
    start = function(config) stop("disabled runtime must not start"),
    stop = function(compute) stop("disabled runtime must not stop"),
    submit = function(worker, args, config) {
      stop("disabled runtime must not submit")
    }
  )
  config <- cerebro_async_config(list(enabled = FALSE))
  state <- cerebro_async_init(config, backend = backend)

  expect_false(state$active)
  expect_identical(
    cerebro_async_submit(function(x) x * 2, list(x = 4)),
    8
  )
  cerebro_async_shutdown()
})

test_that("missing async packages fall back to synchronous execution", {
  runtime <- new.env(parent = globalenv())
  sys.source(async_runtime_file, envir = runtime)
  runtime$requireNamespace <- function(package, quietly = TRUE) {
    package != "mirai"
  }

  expect_warning(
    state <- runtime$cerebro_async_init(runtime$cerebro_async_config()),
    "using synchronous execution"
  )
  expect_false(state$active)
  expect_false(state$config$enabled)
  expect_identical(
    runtime$cerebro_async_submit(function(x) x + 1L, list(x = 2L)),
    3L
  )
})

test_that("latest task cancels its predecessor and ignores stale completion", {
  callbacks <- list()
  cancelled <- integer()
  published <- integer()
  failed <- character()
  next_id <- 0L
  controller <- cerebro_async_latest(
    worker = function(x) x,
    on_value = function(value) published <<- c(published, value),
    on_error = function(error) failed <<- c(failed, conditionMessage(error)),
    submit = function(worker, args) {
      next_id <<- next_id + 1L
      structure(list(id = next_id, args = args), class = "fake_async")
    },
    cancel = function(task) cancelled <<- c(cancelled, task$id),
    chain = function(task, on_value, on_error) {
      callbacks[[task$id]] <<- list(value = on_value, error = on_error)
      invisible(task)
    }
  )

  first <- controller$invoke(list(x = 1L))
  second <- controller$invoke(list(x = 2L))
  callbacks[[first$id]]$value(1L)
  callbacks[[second$id]]$value(2L)

  expect_identical(cancelled, 1L)
  expect_identical(published, 2L)
  expect_length(failed, 0L)
  expect_identical(controller$generation(), 2L)
})

test_that("latest task reports only the current error and cancels on cleanup", {
  callbacks <- list()
  cancelled <- integer()
  failed <- character()
  next_id <- 0L
  controller <- cerebro_async_latest(
    worker = identity,
    on_value = function(value) NULL,
    on_error = function(error) failed <<- conditionMessage(error),
    submit = function(worker, args) {
      next_id <<- next_id + 1L
      structure(list(id = next_id), class = "fake_async")
    },
    cancel = function(task) cancelled <<- c(cancelled, task$id),
    chain = function(task, on_value, on_error) {
      callbacks[[task$id]] <<- list(value = on_value, error = on_error)
    }
  )

  task <- controller$invoke(list("x"))
  callbacks[[task$id]]$error(simpleError("worker failed"))
  expect_identical(failed, "worker failed")

  current <- controller$invoke(list("y"))
  controller$cancel()
  expect_true(current$id %in% cancelled)
})

test_that("latest-value adapter publishes and caches session-safe results", {
  on.exit(cerebro_async_shutdown(), add = TRUE)
  cerebro_async_init(cerebro_async_config(list(enabled = FALSE)))

  server <- function(input, output, session) {
    calls <- 0L
    job <- cerebro_async_latest_value(
      session,
      function(x) {
        calls <<- calls + 1L
        x * 2
      }
    )
    observeEvent(input$go, {
      job$invoke(paste0("key-", input$go), list(x = input$go))
    })
    output$value <- renderText(job$result())
  }

  shiny::testServer(server, {
    session$setInputs(go = 3L)
    session$flushReact()
    expect_identical(output$value, "6")

    job$invoke("key-3", list(x = 99L))
    session$flushReact()
    expect_identical(output$value, "6")
  })
})

test_that("inline reactive jobs do not republish the same cached key", {
  on.exit(cerebro_async_shutdown(), add = TRUE)
  invisible(cerebro_async_init(cerebro_async_config(list(enabled = FALSE))))

  server <- function(input, output, session) {
    calls <- 0L
    job <- cerebro_async_latest_value(session, function(x) {
      calls <<- calls + 1L
      x * 2
    })
    value <- reactive({
      req(input$go)
      job$invoke(paste0("key-", input$go), list(x = input$go))
      job$result()
    })
    output$value <- renderText(value())
    output$calls <- renderText(calls)
  }

  shiny::testServer(server, {
    session$setInputs(go = 3L)
    session$flushReact()
    session$flushReact()
    expect_identical(output$value, "6")
    expect_identical(output$calls, "1")
  })
})

test_that("real mirai execution is out of process", {
  skip_if_not_installed("mirai")
  profile <- paste0("cerebro-test-", Sys.getpid())
  config <- cerebro_async_config(list(workers = 1L, compute = profile))
  on.exit(cerebro_async_shutdown(), add = TRUE)

  cerebro_async_init(config)
  task <- cerebro_async_submit(function() Sys.getpid())

  expect_s3_class(task, "mirai")
  expect_false(identical(task[], Sys.getpid()))
})

test_that("real mirai completion returns through the promises event loop", {
  skip_if_not_installed("mirai")
  profile <- paste0("cerebro-callback-test-", Sys.getpid())
  config <- cerebro_async_config(list(workers = 1L, compute = profile))
  on.exit(cerebro_async_shutdown(), add = TRUE)
  invisible(cerebro_async_init(config))
  value <- failure <- NULL

  task <- cerebro_async_submit(function(x) x * 2, list(x = 21))
  cerebro_async_chain(
    task,
    function(result) value <<- result,
    function(error) failure <<- error
  )
  deadline <- Sys.time() + 5
  while (is.null(value) && is.null(failure) && Sys.time() < deadline) {
    later::run_now(0.05)
  }

  expect_null(failure)
  expect_identical(value, 42)
})

test_that("real mirai source-call worker can execute a bundled helper", {
  skip_if_not_installed("mirai")
  root <- normalizePath(async_viewer_root)
  profile <- paste0("cerebro-source-test-", Sys.getpid())
  config <- cerebro_async_config(list(workers = 1L, compute = profile))
  on.exit(cerebro_async_shutdown(), add = TRUE)

  cerebro_async_init(config)
  task <- cerebro_async_submit(
    cerebro_async_source_call,
    list(
      root = root,
      files = "spatial/func_spatial_helpers.R",
      function_name = "morans_i",
      args = list(x = 1:8, y = c(1:4, 1:4), values = 1:8, k = 2L)
    )
  )

  expect_s3_class(task, "mirai")
  expect_true(is.finite(task[]))
})
