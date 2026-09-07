## Bounded asynchronous execution shared by installed and standalone viewers.

cerebro_async_config <- function(options = NULL) {
  cores <- suppressWarnings(parallel::detectCores(logical = TRUE))
  if (is.na(cores) || cores < 2L) {
    cores <- 2L
  }
  defaults <- list(
    enabled = TRUE,
    workers = min(2L, cores - 1L),
    queue_memory_mb = 256,
    timeout_ms = 300000L,
    compute = "cerebro"
  )
  if (is.null(options)) {
    options <- list()
  }
  if (!is.list(options)) {
    stop("Cerebro.options[['mirai']] must be a list.", call. = FALSE)
  }
  unknown <- setdiff(names(options), names(defaults))
  if (length(unknown)) {
    stop("Unknown mirai option: ", unknown[[1L]], ".", call. = FALSE)
  }
  config <- utils::modifyList(defaults, options)
  is_scalar <- function(value) length(value) == 1L && !is.na(value)
  is_positive <- function(value) {
    is.numeric(value) && is_scalar(value) && is.finite(value) && value > 0
  }
  is_positive_integer <- function(value) {
    is_positive(value) && value == as.integer(value)
  }
  if (!is.logical(config$enabled) || !is_scalar(config$enabled)) {
    stop("mirai enabled must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is_positive_integer(config$workers)) {
    stop("mirai workers must be a positive integer.", call. = FALSE)
  }
  if (!is_positive(config$queue_memory_mb)) {
    stop("mirai queue_memory_mb must be positive.", call. = FALSE)
  }
  if (!is_positive_integer(config$timeout_ms)) {
    stop("mirai timeout_ms must be a positive integer.", call. = FALSE)
  }
  if (
    !is.character(config$compute) ||
      !is_scalar(config$compute) ||
      !nzchar(config$compute)
  ) {
    stop("mirai compute must be a non-empty string.", call. = FALSE)
  }
  config$workers <- as.integer(config$workers)
  config$timeout_ms <- as.integer(config$timeout_ms)
  config
}

## Environment names are limited to 10,000 bytes, so large cell selections
## need a compact cache identity rather than a pasted barcode vector.
cerebro_async_cache_key <- function(...) {
  bytes <- as.double(as.integer(serialize(list(...), NULL, version = 2L)))
  index <- seq_along(bytes)
  modulus <- 2147483629
  first <- sum(bytes * ((index %% 65521) + 1)) %% modulus
  second <- sum(bytes * (((index * 131) %% 65519) + 1)) %% modulus
  paste(
    length(bytes),
    format(first, scientific = FALSE),
    format(second, scientific = FALSE),
    sep = "-"
  )
}

## Run a pure helper from the standalone viewer bundle without serialising a
## Shiny session, reactive graph, R6 object, or file-backed matrix handle.
cerebro_async_source_call <- function(
  root,
  files,
  function_name,
  args = list(),
  function_args = character(0)
) {
  worker_env <- new.env(parent = baseenv())
  for (file in files) {
    sys.source(file.path(root, file), envir = worker_env)
  }
  args[names(function_args)] <- unname(mget(
    function_args,
    envir = worker_env,
    inherits = FALSE
  ))
  do.call(get(function_name, envir = worker_env, inherits = FALSE), args)
}
environment(cerebro_async_source_call) <- baseenv()

## Invoke one exported package function in a daemon without capturing the
## caller's environment. Useful for heavy, pure package calls such as
## scRepertoire plots.
cerebro_async_namespace_call <- function(
  package,
  function_name,
  args = list(),
  quiet_warnings = character(0)
) {
  if (!length(quiet_warnings)) {
    return(do.call(getExportedValue(package, function_name), args))
  }
  withCallingHandlers(
    do.call(getExportedValue(package, function_name), args),
    warning = function(condition) {
      if (any(grepl(quiet_warnings, conditionMessage(condition)))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
environment(cerebro_async_namespace_call) <- baseenv()

cerebro_async_ggsave <- function(path, plot, width, height) {
  getExportedValue("ggplot2", "ggsave")(
    filename = path,
    plot = plot,
    width = width,
    height = height
  )
  path
}
environment(cerebro_async_ggsave) <- baseenv()

cerebro_async_backend <- function() {
  list(
    daemons_set = function(compute) mirai::daemons_set(.compute = compute),
    start = function(config) {
      mirai::daemons(
        n = config$workers,
        dispatcher = TRUE,
        memory = config$queue_memory_mb,
        .compute = config$compute
      )
    },
    stop = function(compute) mirai::daemons(0L, .compute = compute),
    submit = function(worker, args, config) {
      mirai::mirai(
        do.call(worker, args),
        worker = worker,
        args = args,
        .timeout = config$timeout_ms,
        .compute = config$compute
      )
    }
  )
}

.cerebro_async_state <- new.env(parent = emptyenv())
.cerebro_async_state$owned <- FALSE
.cerebro_async_state$started <- FALSE
.cerebro_async_state$config <- cerebro_async_config(list(enabled = FALSE))
.cerebro_async_state$backend <- NULL

cerebro_async_init <- function(
  config = cerebro_async_config(),
  backend = NULL
) {
  if (!is.list(config) || is.null(config$enabled)) {
    config <- cerebro_async_config(config)
  }
  if (!is.null(.cerebro_async_state$backend)) {
    cerebro_async_shutdown()
  }
  .cerebro_async_state$config <- config
  .cerebro_async_state$backend <- NULL
  .cerebro_async_state$owned <- FALSE
  .cerebro_async_state$started <- FALSE
  if (!isTRUE(config$enabled)) {
    return(invisible(FALSE))
  }
  if (is.null(backend)) {
    required <- c("mirai", "promises")
    missing <- required[
      !vapply(
        required,
        requireNamespace,
        logical(1),
        quietly = TRUE
      )
    ]
    if (length(missing)) {
      config$enabled <- FALSE
      .cerebro_async_state$config <- config
      warning(
        "Asynchronous execution unavailable (missing: ",
        paste(missing, collapse = ", "),
        "); using synchronous execution.",
        call. = FALSE
      )
      return(invisible(FALSE))
    }
    backend <- cerebro_async_backend()
  }
  existing <- isTRUE(backend$daemons_set(config$compute))
  .cerebro_async_state$backend <- backend
  .cerebro_async_state$owned <- !existing
  .cerebro_async_state$started <- existing
  invisible(TRUE)
}

cerebro_async_start <- function() {
  if (
    !is.null(.cerebro_async_state$backend) &&
      !isTRUE(.cerebro_async_state$started)
  ) {
    .cerebro_async_state$backend$start(.cerebro_async_state$config)
    .cerebro_async_state$started <- TRUE
  }
  invisible(TRUE)
}

cerebro_async_shutdown <- function() {
  if (
    !is.null(.cerebro_async_state$backend) &&
      isTRUE(.cerebro_async_state$owned) &&
      isTRUE(.cerebro_async_state$started)
  ) {
    .cerebro_async_state$backend$stop(.cerebro_async_state$config$compute)
  }
  .cerebro_async_state$backend <- NULL
  .cerebro_async_state$owned <- FALSE
  .cerebro_async_state$started <- FALSE
  invisible(TRUE)
}

cerebro_async_submit <- function(worker, args = list()) {
  if (is.null(.cerebro_async_state$backend)) {
    return(do.call(worker, args))
  }
  cerebro_async_start()
  .cerebro_async_state$backend$submit(
    worker,
    args,
    .cerebro_async_state$config
  )
}

cerebro_async_cancel <- function(task) {
  if (inherits(task, "mirai") && requireNamespace("mirai", quietly = TRUE)) {
    try(mirai::stop_mirai(task), silent = TRUE)
  }
  invisible(NULL)
}

cerebro_async_chain <- function(task, on_value, on_error) {
  async <- inherits(task, "mirai") ||
    (requireNamespace("promises", quietly = TRUE) && promises::is.promise(task))
  if (!async) {
    return(on_value(task))
  }
  if (!requireNamespace("promises", quietly = TRUE)) {
    stop(
      "The promises package is required for asynchronous callbacks.",
      call. = FALSE
    )
  }
  promises::then(task, onFulfilled = on_value, onRejected = on_error)
}

cerebro_async_error_message <- function(error) {
  if (inherits(error, "condition")) {
    return(conditionMessage(error))
  }
  message <- paste(as.character(error), collapse = " ")
  if (nzchar(message)) message else "Asynchronous computation failed."
}

cerebro_async_latest <- function(
  worker,
  on_value,
  on_error,
  submit = cerebro_async_submit,
  cancel = cerebro_async_cancel,
  chain = cerebro_async_chain
) {
  generation <- 0L
  current <- NULL

  cancel_current <- function() {
    generation <<- generation + 1L
    if (!is.null(current)) {
      cancel(current)
      current <<- NULL
    }
    invisible(NULL)
  }

  invoke <- function(args = list()) {
    if (!is.null(current)) {
      cancel(current)
    }
    generation <<- generation + 1L
    token <- generation
    task <- submit(worker, args)
    current <<- task
    chain(
      task,
      function(value) {
        if (identical(token, generation)) {
          current <<- NULL
          on_value(value)
        }
        invisible(value)
      },
      function(error) {
        if (identical(token, generation)) {
          current <<- NULL
          on_error(error)
        }
        invisible(NULL)
      }
    )
    task
  }

  list(
    invoke = invoke,
    cancel = cancel_current
  )
}

## Adapt latest-wins tasks to a Shiny reactive value with a small per-session
## cache. Callers pass only immutable, serialisable arguments to invoke().
cerebro_async_latest_value <- function(
  session,
  worker = cerebro_async_source_call,
  max_entries = 8L
) {
  if (!is.function(worker)) {
    stop("worker must be a function.", call. = FALSE)
  }
  if (
    !is.numeric(max_entries) ||
      length(max_entries) != 1L ||
      is.na(max_entries) ||
      max_entries < 1 ||
      max_entries != as.integer(max_entries)
  ) {
    stop("max_entries must be a positive integer.", call. = FALSE)
  }
  max_entries <- as.integer(max_entries)
  cache <- new.env(parent = emptyenv())
  cache_order <- character(0)
  active_key <- NULL
  pending_key <- NULL
  published_key <- NULL
  state <- shiny::reactiveVal(list(ready = FALSE, value = NULL))
  error <- shiny::reactiveVal(NULL)

  remember <- function(key, value) {
    assign(key, value, envir = cache)
    cache_order <<- c(setdiff(cache_order, key), key)
    if (length(cache_order) > max_entries) {
      remove(list = cache_order[[1L]], envir = cache)
      cache_order <<- cache_order[-1L]
    }
  }

  controller <- cerebro_async_latest(
    worker = worker,
    on_value = function(value) {
      remember(active_key, value)
      pending_key <<- NULL
      published_key <<- active_key
      error(NULL)
      state(list(ready = TRUE, value = value))
    },
    on_error = function(condition) {
      pending_key <<- NULL
      error(condition)
    }
  )

  invoke <- function(key, args = list()) {
    if (!is.character(key) || length(key) != 1L || is.na(key) || !nzchar(key)) {
      stop("async cache key must be one non-empty string.", call. = FALSE)
    }
    active_key <<- key
    error(NULL)
    if (exists(key, envir = cache, inherits = FALSE)) {
      if (identical(published_key, key)) {
        return(invisible(NULL))
      }
      controller$cancel()
      pending_key <<- NULL
      published_key <<- key
      state(list(
        ready = TRUE,
        value = get(key, envir = cache, inherits = FALSE)
      ))
      return(invisible(NULL))
    }
    if (identical(pending_key, key)) {
      return(invisible(NULL))
    }
    pending_key <<- key
    published_key <<- NULL
    state(list(ready = FALSE, value = NULL))
    controller$invoke(args)
  }

  result <- shiny::reactive({
    failure <- error()
    shiny::validate(shiny::need(
      is.null(failure),
      if (is.null(failure)) "" else cerebro_async_error_message(failure)
    ))
    current <- state()
    shiny::req(isTRUE(current$ready))
    current$value
  })

  session$onSessionEnded(function() controller$cancel())
  list(
    invoke = invoke,
    result = result
  )
}
