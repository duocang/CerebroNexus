## Process-owned CRB prototypes and in-flight reads. Load once per app process.
## Sessions supply an immutable backend identity and clone published prototypes.
.crb_process_cache <- new.env(parent = emptyenv())
.crb_backend_identities <- new.env(parent = emptyenv())
crb_prefetch_tasks <- new.env(parent = emptyenv())
crb_prefetch_started <- new.env(parent = emptyenv())
crb_prefetch_seen <- new.env(parent = emptyenv())

## A background read that a dataset switch has waited this long for is abandoned
## and the dataset is read in the foreground instead. The clock starts when the
## switch begins to wait (crb_prefetch_begin_wait), not when the task started: a
## read that began earlier, while the user was elsewhere, is not penalised.
## Without a limit a daemon that cannot start (a library path the child R does
## not see, a blocked socket) would leave the dataset switch waiting forever.
crb_prefetch_timeout_seconds <- 30
## The loader looks again every second while it waits. A longer gap means the
## user left and came back, which starts a new wait.
crb_prefetch_wait_gap_seconds <- 5

crb_prefetch_begin_wait <- function(key) {
  now <- as.numeric(Sys.time())
  seen <- crb_prefetch_seen[[key]]
  if (
    is.null(crb_prefetch_started[[key]]) ||
      (!is.null(seen) && now - seen > crb_prefetch_wait_gap_seconds)
  ) {
    crb_prefetch_started[[key]] <- now
  }
  crb_prefetch_seen[[key]] <- now
  invisible(NULL)
}

crb_prefetch_waited <- function(key) {
  started <- crb_prefetch_started[[key]]
  if (is.null(started)) 0 else as.numeric(Sys.time()) - started
}

crb_prefetch_abandon <- function(key) {
  if (exists(key, envir = crb_prefetch_tasks, inherits = FALSE)) {
    rm(list = key, envir = crb_prefetch_tasks)
  }
  for (clock in list(crb_prefetch_started, crb_prefetch_seen)) {
    if (exists(key, envir = clock, inherits = FALSE)) {
      rm(list = key, envir = clock)
    }
  }
}

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
    shiny::ExtendedTask$new(function(path) {
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
        path = path
      )
    })
  )
  crb_prefetch_tasks[[cache_key]] <- prefetch_task
  print(glue::glue(
    "[{Sys.time()}] CRB background prefetch started: {basename(path)}"
  ))
  shiny::withReactiveDomain(NULL, prefetch_task$invoke(path))
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
          ## An abandoned task must not remove the entries of a newer one.
          if (identical(crb_prefetch_tasks[[cache_key]], prefetch_task)) {
            crb_prefetch_abandon(cache_key)
          }
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

## The Viewer process holds millions of R objects from its package imports, so
## every full garbage collection costs about a second. Opening a large CRB
## allocates more than R's default free heap, which forced one or two of them
## while Data info waited. Raise the heap trigger once at startup instead:
## requesting a block larger than the free space makes R collect once and grow
## its trigger, and the unused block is released by the next young collection.
.reserveCrbHeap <- function(paths) {
  sizes <- suppressWarnings(file.size(as.character(paths)))
  largest <- max(c(0, sizes), na.rm = TRUE)
  ## A qs2 CRB expands to about five times its file size in memory; leave room
  ## for that and its transient copies.
  bytes <- min(16 * largest, 4 * 1024^3)
  if (bytes < 256 * 1024^2) {
    return(invisible(FALSE))
  }
  invisible(tryCatch(
    {
      block <- raw(bytes)
      rm(block)
      TRUE
    },
    error = function(error) FALSE
  ))
}
