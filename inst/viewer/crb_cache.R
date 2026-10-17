## Process-owned CRB prototypes and in-flight reads. Load once per app process.
## Sessions supply an immutable backend identity and clone published prototypes.
.crb_process_cache <- new.env(parent = emptyenv())
.crb_backend_identities <- new.env(parent = emptyenv())
crb_prefetch_tasks <- new.env(parent = emptyenv())

.touchCrbPrototype <- function(key) {
  cached <- .crb_process_cache[[key]]
  if (!is.null(cached)) {
    cached$last_used <- as.numeric(Sys.time())
    .crb_process_cache[[key]] <- cached
  }
  cached
}

.cacheCrbPrototype <- function(path, prototype, backend_identity) {
  cache_key <- normalizePath(path, winslash = "/", mustWork = FALSE)
  .checkCrbBackendIdentity(cache_key, backend_identity)
  cached <- .touchCrbPrototype(cache_key)
  if (!is.null(cached)) {
    return(invisible(cached))
  }
  cached <- list(
    object = prototype,
    backend_identity = backend_identity,
    last_used = as.numeric(Sys.time())
  )
  .crb_process_cache[[cache_key]] <- cached
  ## Bound retained prototypes by count: object.size(R6) cannot measure their
  ## referenced payload reliably. Active session clones remain valid on eviction.
  keys <- ls(.crb_process_cache, all.names = TRUE)
  if (length(keys) > 2L) {
    older <- setdiff(keys, cache_key)
    ages <- vapply(
      older,
      function(key) .crb_process_cache[[key]]$last_used %||% 0,
      numeric(1)
    )
    rm(
      list = older[order(ages)][seq_len(length(keys) - 2L)],
      envir = .crb_process_cache
    )
  }
  invisible(cached)
}

.checkCrbBackendIdentity <- function(key, identity) {
  previous <- .crb_backend_identities[[key]]
  cached <- .crb_process_cache[[key]]
  if (
    (!is.null(previous) && !identical(previous$identity, identity)) ||
      (!is.null(cached) && !identical(cached$backend_identity, identity))
  ) {
    stop(
      "The cached CRB backend configuration changed after it was loaded. Start a new app session.",
      call. = FALSE
    )
  }
  .crb_backend_identities[[key]] <- list(identity = identity)
}

## A mirai daemon is a fresh Rscript: it sees only the library paths R derives
## from its environment, not the ones the Viewer process added at run time
## (.libPaths(), renv, a launcher script). Without them the daemon dies on
## loadNamespace("mirai") and mirai discards its stderr, leaving the request
## unresolved forever. R_LIBS is read when the child starts, so set it only
## around the call that spawns the daemon.
.withDaemonLibPaths <- function(code) {
  previous <- Sys.getenv("R_LIBS", unset = NA_character_)
  Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  on.exit(
    if (is.na(previous)) {
      Sys.unsetenv("R_LIBS")
    } else {
      Sys.setenv(R_LIBS = previous)
    },
    add = TRUE
  )
  code
}

## Safety net for a daemon that never connects or dies mid-read (out of memory,
## killed): the task then errors and the dataset loads synchronously instead
## of waiting on a reply that cannot arrive. Scales with file size so a slow
## read of a large CRB is not cut short; option is in seconds.
.crbPrefetchTimeoutMs <- function(path) {
  seconds <- getOption("cerebro.crb_prefetch_timeout")
  if (is.null(seconds)) {
    size_mb <- sum(file.size(path), na.rm = TRUE) / 1024^2
    seconds <- 60 + size_mb / 2
  }
  as.integer(min(max(seconds, 1) * 1000, .Machine$integer.max))
}

start_crb_prefetch <- function(path, backend_identity) {
  path <- as.character(path[[1L]])
  if (!file.exists(path)) {
    return(invisible(NULL))
  }
  cache_key <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (
    !is.null(.crb_process_cache[[cache_key]]) ||
      !is.null(crb_prefetch_tasks[[cache_key]]) ||
      length(crb_prefetch_tasks) >= 2L
  ) {
    return(invisible(NULL))
  }
  # Snapshot the caller's identity before asynchronous completion; retain no
  # unevaluated session expression that could observe a later dataset switch.
  force(backend_identity)
  prefetch_task <- shiny::withReactiveDomain(
    NULL,
    shiny::ExtendedTask$new(function(path, timeout) {
      .withDaemonLibPaths(
        mirai::mirai(
          {
            connection <- file(path, open = "rb")
            on.exit(close(connection), add = TRUE)
            magic <- readBin(connection, "raw", n = 4L)
            prototype <- if (
              identical(magic, as.raw(c(0x0b, 0x0e, 0x0a, 0xc1)))
            ) {
              qs2::qs_read(path)
            } else {
              readRDS(path)
            }
            list(path = path, prototype = prototype)
          },
          path = path,
          .timeout = timeout
        )
      )
    })
  )
  crb_prefetch_tasks[[cache_key]] <- prefetch_task
  print(glue::glue(
    "[{Sys.time()}] CRB background prefetch started: {basename(path)}"
  ))
  shiny::withReactiveDomain(
    NULL,
    prefetch_task$invoke(path, .crbPrefetchTimeoutMs(path))
  )
  ## Completion belongs to the process, not the first requesting session.
  ## Publish once, then release the task's retained result even when the
  ## initiating user has left the dataset or disconnected.
  completion <- shiny::observe(
    {
      status <- prefetch_task$status()
      if (!status %in% c("success", "error")) {
        return()
      }
      on.exit(
        {
          rm(list = cache_key, envir = crb_prefetch_tasks)
          completion$destroy()
        },
        add = TRUE
      )
      if (identical(status, "success")) {
        .cacheCrbPrototype(
          path,
          prefetch_task$result()$prototype,
          backend_identity
        )
      } else {
        tryCatch(prefetch_task$result(), error = function(error) {
          warning(
            "CRB background prefetch failed: ",
            conditionMessage(error),
            call. = FALSE
          )
        })
      }
    },
    domain = NULL
  )
  invisible(NULL)
}
