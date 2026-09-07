## Shared asynchronous runtime for short, read-only Builder tasks.

.builder_async <- new.env(parent = emptyenv())
.builder_async$n <- 2L
.builder_async$memory <- 64L
.builder_async$compute <- paste0("cerebro-builder-", Sys.getpid())
.builder_async$started <- FALSE

builder_async_start <- function() {
  if (!isTRUE(.builder_async$started)) {
    mirai::daemons(
      .builder_async$n,
      dispatcher = TRUE,
      memory = .builder_async$memory,
      .compute = .builder_async$compute
    )
    .builder_async$started <- TRUE
  }
  .builder_async$compute
}

builder_async_submit <- function(.expr, .args = list()) {
  task <- do.call(
    mirai::try_mirai,
    list(
      .expr = .expr,
      .args = .args,
      .compute = builder_async_start()
    )
  )
  if (is.null(task)) {
    stop("The Builder background queue is full.", call. = FALSE)
  }
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
  handled <- promises::then(
    task,
    onFulfilled = function(value) run(on_fulfilled, value)
  )
  promises::catch(
    handled,
    onRejected = function(error) run(on_rejected, error)
  )
  invisible(task)
}

builder_async_cancel <- function(task) {
  if (!inherits(task, "mirai")) {
    return(invisible(FALSE))
  }
  tryCatch(
    {
      mirai::stop_mirai(task)
      invisible(TRUE)
    },
    error = function(error) invisible(FALSE)
  )
}

builder_async_stop <- function() {
  if (!isTRUE(.builder_async$started)) {
    return(invisible(FALSE))
  }
  mirai::daemons(0L, .compute = .builder_async$compute)
  .builder_async$started <- FALSE
  invisible(TRUE)
}

shiny::onStop(builder_async_stop)
