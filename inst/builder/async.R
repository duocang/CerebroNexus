## Shared asynchronous runtime for short, read-only Builder tasks.

.builder_async <- new.env(parent = emptyenv())
.builder_async$n <- 2L
.builder_async$memory <- 64L
.builder_async$tasks <- list()
.builder_async$started <- FALSE
.builder_async$stopping <- FALSE

builder_async_evaluate <- function(.expr, .args) {
  list2env(.args, envir = environment())
  eval(.expr, envir = environment())
}

builder_async_active <- function() {
  Filter(
    function(task) task$state %in% c("queued", "running"),
    .builder_async$tasks
  )
}

builder_async_schedule <- function() {
  if (isTRUE(.builder_async$stopping)) {
    return(invisible(FALSE))
  }
  active <- builder_async_active()
  running <- sum(vapply(
    active,
    function(task) identical(task$state, "running"),
    logical(1)
  ))
  queued <- Filter(function(task) identical(task$state, "queued"), active)
  for (task in head(queued, max(0L, .builder_async$n - running))) {
    task$process <- callr::r_bg(
      builder_async_evaluate,
      list(task$expr, task$args),
      supervise = TRUE,
      stdout = "|",
      stderr = "|"
    )
    task$state <- "running"
  }
  invisible(TRUE)
}

builder_async_submit <- function(.expr, .args = list()) {
  .builder_async$tasks <- builder_async_active()
  if (
    length(.builder_async$tasks) >= .builder_async$n + .builder_async$memory
  ) {
    stop("The Builder background queue is full.", call. = FALSE)
  }
  task <- new.env(parent = emptyenv())
  task$expr <- .expr
  task$args <- .args
  task$process <- NULL
  task$state <- "queued"
  class(task) <- "builder_async_task"
  .builder_async$tasks[[length(.builder_async$tasks) + 1L]] <- task
  .builder_async$started <- TRUE
  builder_async_schedule()
  task
}

builder_async_then <- function(
  task,
  session,
  on_fulfilled,
  on_rejected
) {
  run <- function(callback, value) {
    if (!is.null(session)) {
      closed <- tryCatch(
        is.function(session$isClosed) && isTRUE(session$isClosed()),
        error = function(error) FALSE
      )
      if (closed) {
        return(invisible(FALSE))
      }
      return(shiny::withReactiveDomain(session, callback(value)))
    }
    callback(value)
  }
  poll <- function() {
    if (
      identical(task$state, "queued") ||
        (identical(task$state, "running") && task$process$is_alive())
    ) {
      later::later(poll, 0.01)
      return(invisible(FALSE))
    }
    if (!identical(task$state, "running")) {
      return(invisible(FALSE))
    }
    task$state <- "completed"
    .builder_async$tasks <- Filter(
      function(candidate) !identical(candidate, task),
      .builder_async$tasks
    )
    builder_async_schedule()
    value <- tryCatch(task$process$get_result(), error = identity)
    if (inherits(value, "condition")) {
      run(on_rejected, value)
    } else {
      tryCatch(
        run(on_fulfilled, value),
        error = function(error) run(on_rejected, error)
      )
    }
    invisible(TRUE)
  }
  later::later(poll, 0)
  invisible(task)
}

builder_async_cancel <- function(task) {
  if (
    !inherits(task, "builder_async_task") ||
      !task$state %in% c("queued", "running")
  ) {
    return(invisible(FALSE))
  }
  if (identical(task$state, "running") && task$process$is_alive()) {
    task$process$kill_tree()
    task$process$wait(1000)
  }
  task$state <- "cancelled"
  .builder_async$tasks <- Filter(
    function(candidate) !identical(candidate, task),
    .builder_async$tasks
  )
  builder_async_schedule()
  invisible(TRUE)
}

builder_async_stop <- function() {
  active <- builder_async_active()
  if (!length(active)) {
    .builder_async$tasks <- list()
    .builder_async$started <- FALSE
    return(invisible(FALSE))
  }
  .builder_async$stopping <- TRUE
  on.exit(.builder_async$stopping <- FALSE, add = TRUE)
  lapply(active, builder_async_cancel)
  .builder_async$tasks <- list()
  .builder_async$started <- FALSE
  invisible(TRUE)
}

shiny::onStop(builder_async_stop)
