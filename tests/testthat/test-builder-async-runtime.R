test_that("Builder async runtime owns one bounded callr pool", {
  path <- builder_profile_inst_path("builder", "async.R")
  expect_true(file.exists(path))

  runtime <- new.env(parent = globalenv())
  sys.source(path, envir = runtime)
  on.exit(runtime$builder_async_stop(), add = TRUE)

  task <- runtime$builder_async_submit(quote(value + 1L), list(value = 1L))
  resolved <- NULL
  runtime$builder_async_then(
    task,
    session = NULL,
    on_fulfilled = function(value) resolved <<- value,
    on_rejected = function(error) stop(error)
  )
  deadline <- Sys.time() + 5
  while (is.null(resolved) && Sys.time() < deadline) {
    later::run_now(0.1)
  }

  expect_identical(resolved, 2L)
  expect_identical(runtime$.builder_async$n, 2L)
  expect_identical(runtime$.builder_async$memory, 64L)
})

test_that("the three read-only jobs use event-driven async callbacks", {
  source_text <- function(...) {
    paste(
      readLines(builder_profile_inst_path("builder", ...), warn = FALSE),
      collapse = "\n"
    )
  }
  function_text <- function(text, start, end) {
    start_at <- regexpr(start, text, fixed = TRUE)[[1L]]
    expect_gt(start_at, 0L)
    remainder <- substr(text, start_at, nchar(text))
    end_at <- regexpr(end, remainder, perl = TRUE)[[1L]]
    expect_gt(end_at, 0L)
    substr(remainder, 1L, end_at - 1L)
  }

  enhancements <- function_text(
    source_text("server", "enhancements.R"),
    "start_enhance_table_inventory <- function",
    "observeEvent\\(input\\[\\[\"enhance-table_files"
  )
  project <- function_text(
    source_text("server", "project.R"),
    "builder_project_start_open <- function",
    "invalidate_builder_project_source_sync <- function"
  )
  build <- function_text(
    source_text("server", "build.R"),
    "builder_start_build_output_preflight_process <- function",
    "show_builder_build_server_path <- function"
  )

  for (text in list(enhancements, project, build)) {
    expect_match(text, "builder_async_submit", fixed = TRUE)
    expect_match(text, "builder_async_then", fixed = TRUE)
  }
  expect_false(grepl("callr::r_bg", enhancements, fixed = TRUE))
  expect_false(grepl(
    "builder_project_schedule_open_poll",
    project,
    fixed = TRUE
  ))
  expect_false(grepl("builder_project_poll_open", project, fixed = TRUE))
  expect_false(grepl(
    "builder_poll_build_output_preflight",
    build,
    fixed = TRUE
  ))
})

test_that("async callback failures use the task rejection path", {
  runtime <- new.env(parent = globalenv())
  sys.source(
    builder_profile_inst_path("builder", "async.R"),
    envir = runtime
  )
  on.exit(runtime$builder_async_stop(), add = TRUE)
  rejected <- NULL
  task <- runtime$builder_async_submit(quote(1L))
  runtime$builder_async_then(
    task,
    session = NULL,
    on_fulfilled = function(value) stop("callback failed"),
    on_rejected = function(error) rejected <<- conditionMessage(error)
  )
  deadline <- Sys.time() + 2
  while (is.null(rejected) && Sys.time() < deadline) {
    later::run_now(0.05)
  }

  expect_identical(rejected, "callback failed")
})

test_that("Builder declares only its callr async runtime", {
  description_path <- file.path(
    dirname(dirname(builder_profile_inst_path("builder"))),
    "DESCRIPTION"
  )
  description <- read.dcf(description_path)[1L, ]
  expect_match(description[["Suggests"]], "callr")
  expect_false(grepl("mirai|promises", paste(description, collapse = "\n")))

  app <- paste(
    readLines(builder_profile_inst_path("builder", "app.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(app, 'source("async.R", local = TRUE)', fixed = TRUE)
})
