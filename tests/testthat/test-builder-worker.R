## -------------------------------------------------------------------------
## Builder worker protocol and restart contracts.
## -------------------------------------------------------------------------

builder_profile_source_runtime(globalenv())
builder_repo_source("io.R", local = globalenv())
builder_repo_source("profile.R", local = globalenv())
builder_repo_source("inspect.R", local = globalenv())
builder_repo_source("adapters.R", local = globalenv())
builder_repo_source("state.R", local = globalenv())

builder_worker_path <- builder_profile_inst_path("builder", "worker.R")
if (nzchar(builder_worker_path) && file.exists(builder_worker_path)) {
  sys.source(builder_worker_path, envir = globalenv())
}
builder_repo_source("session.R", local = globalenv())

builder_protocol_api <- c(
  "builder_request_protocol",
  "builder_query",
  "builder_command",
  "builder_enqueue",
  "builder_pending_ids",
  "builder_protocol_dispatch",
  "builder_protocol_complete",
  "builder_protocol_acknowledge",
  "builder_protocol_cancel"
)
builder_protocol_api_available <- all(vapply(
  builder_protocol_api,
  exists,
  logical(1),
  mode = "function",
  inherits = TRUE
))
builder_protocol_recovery_api_available <- exists(
  "builder_protocol_recover",
  mode = "function",
  inherits = TRUE
)
builder_protocol_terminal_api <- c(
  "builder_protocol_fail_pending",
  "builder_protocol_forget_dataset"
)
builder_protocol_terminal_api_available <- all(vapply(
  builder_protocol_terminal_api,
  exists,
  logical(1),
  mode = "function",
  inherits = TRUE
))

builder_lifecycle_api <- c(
  "builder_worker_start",
  "builder_worker_restart",
  "builder_worker_interrupt",
  "builder_worker_poll",
  "builder_worker_cell_count",
  "builder_worker_register_snapshot",
  "builder_worker_release_snapshot"
)
builder_lifecycle_api_available <- all(vapply(
  builder_lifecycle_api,
  exists,
  logical(1),
  mode = "function",
  inherits = TRUE
))
builder_worker_stop_api_available <- exists(
  "builder_worker_stop",
  mode = "function",
  inherits = TRUE
)

test_that("the isolated worker API is available", {
  expect_true(builder_protocol_api_available)
  expect_true(builder_lifecycle_api_available)
  expect_true(builder_protocol_recovery_api_available)
  expect_true(builder_protocol_terminal_api_available)
  expect_true(builder_worker_stop_api_available)
})

test_that("the real worker loads Marker import support before Build", {
  worker <- readLines(builder_worker_path, warn = FALSE)
  marker_line <- grep(
    'source(file.path(dir, "marker_import.R"))',
    worker,
    fixed = TRUE
  )
  build_line <- grep('source(file.path(dir, "build.R"))', worker, fixed = TRUE)
  expect_length(marker_line, 1L)
  expect_length(build_line, 1L)
  expect_lt(marker_line, build_line)

  expect_true(callr::r(
    function(marker_path) {
      `%||%` <- function(left, right) if (is.null(left)) right else left
      source(marker_path, local = globalenv())
      exists("builder_attach_marker_imports", mode = "function")
    },
    args = list(
      marker_path = builder_profile_inst_path("builder", "marker_import.R")
    )
  ))
})

test_that("authentication accounts are redacted from every protocol owner", {
  raw_contains <- function(value, text) {
    bytes <- serialize(value, NULL)
    needle <- charToRaw(text)
    any(vapply(
      seq_len(max(0L, length(bytes) - length(needle) + 1L)),
      function(index) identical(bytes[index + seq_along(needle) - 1L], needle),
      logical(1)
    ))
  }
  sentinel <- "protocol-password-8b21"
  request <- builder_command(
    "build",
    "session",
    payload = list(
      kind = "build",
      auth_accounts = list(list(
        id = "auth-account-1",
        username = "protocol-user-8b21",
        password = sentinel
      ))
    )
  )
  protocol <- builder_enqueue(builder_request_protocol("epoch-auth"), request)
  queued <- protocol$queue[[1L]]
  protocol$pending <- queued
  protocol$awaiting_ack[[queued$request_id]] <- queued
  redacted <- builder_protocol_redact_auth(protocol)

  expect_s3_class(redacted, "builder_request_protocol")
  expect_false(raw_contains(redacted, sentinel))

  terminal <- .builder_protocol_terminal_request(queued, "stopped")
  expect_false(raw_contains(terminal, sentinel))
})

test_that("build recovery never retries or returns authentication accounts", {
  raw_contains <- function(value, text) {
    bytes <- serialize(value, NULL)
    needle <- charToRaw(text)
    any(vapply(
      seq_len(max(0L, length(bytes) - length(needle) + 1L)),
      function(index) identical(bytes[index + seq_along(needle) - 1L], needle),
      logical(1)
    ))
  }
  sentinel <- "recovery-password-1d73"
  request <- builder_command(
    "build",
    "session",
    payload = list(
      kind = "build",
      auth_accounts = list(list(password = sentinel))
    )
  )
  protocol <- builder_enqueue(builder_request_protocol("epoch-old"), request)
  protocol <- builder_protocol_dispatch(protocol)$protocol
  recovered <- builder_protocol_recover(protocol, epoch = "epoch-new")

  expect_length(recovered$retried, 0L)
  expect_false(raw_contains(recovered, sentinel))
})

if (builder_protocol_api_available) {
  test_that("build protocol redaction removes login accounts everywhere", {
    accounts <- list(list(
      id = "auth-account-1",
      username = "auth-user-a-7f31",
      password = "auth-password-a-7f31"
    ))
    request <- builder_command(
      "build",
      "session",
      payload = list(kind = "build", auth_accounts = accounts)
    )
    protocol <- builder_enqueue(
      builder_request_protocol("worker-auth"),
      request
    )
    dispatched <- builder_protocol_dispatch(protocol)
    redacted <- builder_protocol_redact_auth(dispatched$protocol)

    expect_null(
      builder_request_redact_auth(dispatched$request)$payload$auth_accounts
    )
    expect_null(redacted$pending$payload$auth_accounts)
    expect_false(builder_auth_value_contains(redacted, "auth-user-a-7f31"))
    expect_false(builder_auth_value_contains(redacted, "auth-password-a-7f31"))
  })

  test_that("persistent commands are FIFO and queries replace only their slot", {
    protocol <- builder_request_protocol(epoch = "worker-1")
    protocol <- builder_enqueue(
      protocol,
      builder_query("preview", "a", 1L, slot = "projection")
    )
    protocol <- builder_enqueue(
      protocol,
      builder_query("preview", "a", 1L, slot = "spatial")
    )
    protocol <- builder_enqueue(
      protocol,
      builder_query("preview", "a", 2L, slot = "projection")
    )
    protocol <- builder_enqueue(
      protocol,
      builder_command("alignment", "a")
    )
    protocol <- builder_enqueue(
      protocol,
      builder_command("settings", "a")
    )

    expect_identical(
      builder_pending_ids(protocol),
      c(
        "preview:a:spatial:1",
        "preview:a:projection:2",
        "alignment:a",
        "settings:a"
      )
    )
  })

  test_that("query replacement never crosses dataset or logical slot", {
    protocol <- builder_request_protocol(epoch = "worker-1")
    protocol <- builder_enqueue(
      protocol,
      builder_query("preview", "a", 1L, slot = "projection")
    )
    protocol <- builder_enqueue(
      protocol,
      builder_query("preview", "b", 1L, slot = "projection")
    )
    protocol <- builder_enqueue(
      protocol,
      builder_query("preview", "a", 1L, slot = "spatial")
    )
    protocol <- builder_enqueue(
      protocol,
      builder_query("preview", "a", 2L, slot = "projection")
    )

    expect_identical(
      builder_pending_ids(protocol),
      c(
        "preview:b:projection:1",
        "preview:a:spatial:1",
        "preview:a:projection:2"
      )
    )
  })

  test_that("worker protocol can be sourced without application globals", {
    isolated <- new.env(parent = baseenv())
    sys.source(builder_worker_path, envir = isolated)
    protocol <- isolated$builder_enqueue(
      isolated$builder_request_protocol(epoch = "worker-1"),
      isolated$builder_command(
        "build",
        "session",
        payload = list(id = "build-1")
      )
    )
    expect_identical(protocol$build_status, "queued")
  })

  test_that("persistent completion waits for owner acknowledgement", {
    protocol <- builder_request_protocol(epoch = "worker-1")
    protocol <- builder_enqueue(
      protocol,
      builder_command(
        "alignment",
        "a",
        revision = 4L,
        snapshot_identity = "snapshot-a"
      )
    )
    sent <- builder_protocol_dispatch(protocol)
    protocol <- sent$protocol
    request <- sent$request

    completed <- builder_protocol_complete(
      protocol,
      list(
        epoch = request$epoch,
        token = request$token,
        generation = request$generation,
        dataset_revision = request$dataset_revision,
        snapshot_identity = request$snapshot_identity,
        value = TRUE
      )
    )

    expect_null(completed$protocol$pending)
    expect_identical(length(completed$protocol$awaiting_ack), 1L)
    expect_identical(
      builder_protocol_dispatch(completed$protocol)$blocked,
      "awaiting_ack"
    )
    acknowledged <- builder_protocol_acknowledge(
      completed$protocol,
      request$token
    )
    expect_length(acknowledged$awaiting_ack, 0L)
  })

  test_that("responses must match every request identity before pending clears", {
    protocol <- builder_enqueue(
      builder_request_protocol(epoch = "worker-1"),
      builder_query(
        "preview",
        "a",
        7L,
        slot = "projection",
        revision = 12L,
        snapshot_identity = "snapshot-a"
      )
    )
    sent <- builder_protocol_dispatch(protocol)
    request <- sent$request

    fields <- c(
      "epoch",
      "token",
      "generation",
      "dataset_revision",
      "snapshot_identity"
    )
    for (field in fields) {
      stale <- list(
        epoch = request$epoch,
        token = request$token,
        generation = request$generation,
        dataset_revision = request$dataset_revision,
        snapshot_identity = request$snapshot_identity,
        value = "stale"
      )
      stale[[field]] <- if (is.integer(stale[[field]])) {
        stale[[field]] + 1L
      } else {
        paste0(stale[[field]], "-old")
      }
      rejected <- builder_protocol_complete(sent$protocol, stale)
      expect_false(rejected$accepted, info = field)
      expect_false(is.null(rejected$protocol$pending), info = field)
    }
  })

  test_that("Review and Build wait behind the acknowledged command cutoff", {
    protocol <- builder_request_protocol(epoch = "worker-1")
    protocol <- builder_enqueue(
      protocol,
      builder_command("settings", "a")
    )
    protocol <- builder_enqueue(
      protocol,
      builder_command("review", "session")
    )
    first <- builder_protocol_dispatch(protocol)
    request <- first$request
    completed <- builder_protocol_complete(
      first$protocol,
      list(
        epoch = request$epoch,
        token = request$token,
        generation = request$generation,
        dataset_revision = request$dataset_revision,
        snapshot_identity = request$snapshot_identity,
        value = TRUE
      )
    )

    expect_identical(
      builder_protocol_dispatch(completed$protocol)$blocked,
      "awaiting_ack"
    )
    acknowledged <- builder_protocol_acknowledge(
      completed$protocol,
      request$token
    )
    expect_identical(
      builder_protocol_dispatch(acknowledged)$request$kind,
      "review"
    )
  })

  test_that("Review and Build barriers discard queued replaceable queries", {
    protocol <- builder_request_protocol(epoch = "worker-1")
    protocol <- builder_enqueue(
      protocol,
      builder_query("preview", "a", 1L, slot = "projection")
    )
    protocol <- builder_enqueue(protocol, builder_command("settings", "a"))
    protocol <- builder_enqueue(
      protocol,
      builder_query("coords", "a", 1L, slot = "section-a")
    )
    protocol <- builder_enqueue(protocol, builder_command("review", "session"))

    expect_identical(
      builder_pending_ids(protocol),
      c("settings:a", "review:session")
    )

    protocol <- builder_enqueue(
      protocol,
      builder_query("preview", "a", 2L, slot = "projection")
    )
    protocol <- builder_enqueue(protocol, builder_command("build", "session"))
    expect_identical(
      builder_pending_ids(protocol),
      c("settings:a", "review:session", "build:session")
    )
  })

  test_that("a barrier invalidates a pending query without consuming its generation", {
    protocol <- builder_enqueue(
      builder_request_protocol(epoch = "worker-1"),
      builder_query("preview", "a", 1L, slot = "projection")
    )
    dispatched <- builder_protocol_dispatch(protocol)
    protocol <- builder_enqueue(
      dispatched$protocol,
      builder_command("build", "session")
    )

    completed <- builder_protocol_complete(
      protocol,
      builder_worker_response(
        dispatched$request,
        error = "obsolete query failed"
      )
    )

    expect_false(completed$accepted)
    expect_null(completed$protocol$pending)
    expect_null(completed$value)
    expect_no_error(
      builder_enqueue(
        completed$protocol,
        builder_query("preview", "a", 2L, slot = "projection")
      )
    )
    expect_identical(
      builder_protocol_dispatch(completed$protocol)$request$kind,
      "build"
    )
  })

  test_that("snapshot-bound previews survive settings-only revision changes", {
    protocol <- builder_protocol_dataset(
      builder_request_protocol(epoch = "worker-1"),
      "a",
      revision = 1L,
      snapshot_identity = "snapshot-a"
    )
    protocol <- builder_enqueue(
      protocol,
      builder_query(
        "projection_previews",
        "a",
        1L,
        slot = "viewer-projection-gallery",
        payload = list(revision_independent = TRUE),
        revision = 1L,
        snapshot_identity = "snapshot-a"
      )
    )
    dispatched <- builder_protocol_dispatch(protocol)
    revised <- builder_protocol_dataset(
      dispatched$protocol,
      "a",
      revision = 2L,
      snapshot_identity = "snapshot-a"
    )

    completed <- builder_protocol_complete(
      revised,
      builder_worker_response(dispatched$request, list(umap = "preview"))
    )

    expect_true(completed$accepted)
    expect_identical(completed$value, list(umap = "preview"))
  })

  test_that("persistent dispatch rebinds the latest dataset identity in FIFO order", {
    protocol <- builder_protocol_dataset(
      builder_request_protocol(epoch = "worker-1"),
      "a",
      revision = 0L,
      snapshot_identity = "snapshot-0"
    )
    protocol <- builder_enqueue(
      protocol,
      builder_command(
        "settings",
        "a",
        revision = 0L,
        snapshot_identity = "snapshot-0"
      )
    )
    protocol <- builder_enqueue(
      protocol,
      builder_command(
        "alignment",
        "a",
        revision = 0L,
        snapshot_identity = "snapshot-0"
      )
    )

    first <- builder_protocol_dispatch(protocol)
    completed <- builder_protocol_complete(
      first$protocol,
      builder_worker_response(first$request, TRUE)
    )
    acknowledged <- builder_protocol_acknowledge(
      completed$protocol,
      first$request$request_id
    )
    acknowledged <- builder_protocol_dataset(
      acknowledged,
      "a",
      revision = 1L,
      snapshot_identity = "snapshot-1"
    )

    second <- builder_protocol_dispatch(acknowledged)
    expect_identical(second$request$kind, "alignment")
    expect_identical(second$request$dataset_revision, 1L)
    expect_identical(second$request$snapshot_identity, "snapshot-1")
  })

  test_that("Build is single-flight while queued running or cancelling", {
    queued <- builder_enqueue(
      builder_request_protocol(epoch = "worker-1"),
      builder_command("build", "session")
    )
    expect_error(
      builder_enqueue(queued, builder_command("build", "session")),
      "already"
    )

    running <- builder_protocol_dispatch(queued)$protocol
    expect_error(
      builder_enqueue(running, builder_command("build", "session")),
      "already"
    )

    cancelling <- builder_protocol_cancel(running)
    expect_identical(cancelling$build_status, "cancelling")
    expect_error(
      builder_enqueue(cancelling, builder_command("build", "session")),
      "already"
    )
  })

  test_that("build terminal events and reset cannot target another build", {
    state <- builder_reduce_build(
      builder_build_state(),
      list(type = "start", id = "build-1", revision = 1L)
    )
    for (type in c("succeed", "fail", "cancel")) {
      expect_error(
        builder_reduce_build(
          state,
          list(type = type, id = "build-2", result = TRUE, error = "x")
        ),
        "build"
      )
    }
    expect_error(
      builder_reduce_build(state, list(type = "reset", id = "build-1")),
      "running"
    )
  })
}

if (builder_protocol_recovery_api_available) {
  test_that("protocol recovery retries FIFO commands but never a Build", {
    protocol <- builder_request_protocol(epoch = "worker-1")
    protocol <- builder_enqueue(protocol, builder_command("settings", "a"))
    protocol <- builder_enqueue(protocol, builder_command("alignment", "a"))
    protocol <- builder_enqueue(
      protocol,
      builder_command("build", "session", payload = list(id = "build-1"))
    )
    dispatched <- builder_protocol_dispatch(protocol)

    recovered <- builder_protocol_recover(
      dispatched$protocol,
      epoch = "worker-2",
      reason = "worker_restarted"
    )

    expect_identical(recovered$protocol$epoch, "worker-2")
    expect_identical(
      builder_pending_ids(recovered$protocol),
      c("settings:a", "alignment:a")
    )
    expect_identical(
      vapply(recovered$retried, `[[`, character(1), "kind"),
      c("settings", "alignment")
    )
    expect_identical(
      vapply(recovered$failed, `[[`, character(1), "kind"),
      "build"
    )
    expect_identical(recovered$failed[[1L]]$build_id, "build-1")
    expect_identical(
      recovered$failed[[1L]]$terminal_reason,
      "worker_restarted"
    )
    expect_identical(recovered$protocol$build_status, "idle")
  })

  test_that("protocol recovery reports queries and terminal ACK ownership", {
    query_protocol <- builder_enqueue(
      builder_request_protocol(epoch = "worker-1"),
      builder_query("preview", "a", 1L, slot = "projection")
    )
    query_protocol <- builder_enqueue(
      query_protocol,
      builder_command("settings", "a")
    )
    query_recovery <- builder_protocol_recover(
      query_protocol,
      epoch = "worker-2"
    )
    expect_identical(
      vapply(query_recovery$discarded, `[[`, character(1), "kind"),
      "preview"
    )
    expect_identical(builder_pending_ids(query_recovery$protocol), "settings:a")

    command_protocol <- builder_enqueue(
      builder_request_protocol(epoch = "worker-1"),
      builder_command("settings", "a")
    )
    dispatched <- builder_protocol_dispatch(command_protocol)
    completed <- builder_protocol_complete(
      dispatched$protocol,
      builder_worker_response(dispatched$request, TRUE)
    )
    acknowledged_recovery <- builder_protocol_recover(
      completed$protocol,
      epoch = "worker-2"
    )
    expect_length(acknowledged_recovery$retried, 0L)
    expect_identical(
      vapply(acknowledged_recovery$failed, `[[`, character(1), "kind"),
      "settings"
    )
    expect_length(acknowledged_recovery$protocol$awaiting_ack, 0L)
  })

  test_that("failed restart can explicitly terminate every outstanding request", {
    protocol <- builder_request_protocol(epoch = "worker-1")
    protocol <- builder_enqueue(protocol, builder_command("settings", "a"))
    protocol <- builder_enqueue(
      protocol,
      builder_command("build", "session", payload = list(id = "build-1"))
    )
    dispatched <- builder_protocol_dispatch(protocol)

    terminal <- builder_protocol_recover(
      dispatched$protocol,
      epoch = "worker-1",
      reason = "restart_failed",
      retry_persistent = FALSE
    )

    expect_length(terminal$retried, 0L)
    expect_identical(
      vapply(terminal$failed, `[[`, character(1), "kind"),
      c("settings", "build")
    )
    expect_length(builder_pending_ids(terminal$protocol), 0L)
    expect_null(terminal$protocol$pending)
    expect_length(terminal$protocol$awaiting_ack, 0L)
    expect_identical(terminal$protocol$build_status, "idle")
  })

  test_that("only a pending command consumes one transport retry", {
    protocol <- builder_request_protocol(epoch = "worker-1")
    protocol <- builder_enqueue(protocol, builder_command("settings", "a"))
    protocol <- builder_enqueue(protocol, builder_command("alignment", "a"))
    first_dispatch <- builder_protocol_dispatch(protocol)

    first_recovery <- builder_protocol_recover(
      first_dispatch$protocol,
      epoch = "worker-2"
    )
    expect_identical(first_recovery$retried[[1L]]$kind, "settings")
    expect_identical(first_recovery$retried[[1L]]$transport_retries, 1L)
    expect_identical(first_recovery$retried[[2L]]$kind, "alignment")
    expect_identical(first_recovery$retried[[2L]]$transport_retries, 0L)

    second_dispatch <- builder_protocol_dispatch(first_recovery$protocol)
    second_recovery <- builder_protocol_recover(
      second_dispatch$protocol,
      epoch = "worker-3"
    )
    expect_identical(
      vapply(second_recovery$failed, `[[`, character(1), "kind"),
      "settings"
    )
    expect_identical(
      builder_pending_ids(second_recovery$protocol),
      "alignment:a"
    )
    expect_identical(second_recovery$retried[[1L]]$transport_retries, 0L)

    third_dispatch <- builder_protocol_dispatch(second_recovery$protocol)
    third_recovery <- builder_protocol_recover(
      third_dispatch$protocol,
      epoch = "worker-4"
    )
    expect_length(third_recovery$failed, 0L)
    expect_identical(third_recovery$retried[[1L]]$kind, "alignment")
    expect_identical(third_recovery$retried[[1L]]$transport_retries, 1L)
  })
}

test_that("a deterministic command failure advances without clearing FIFO", {
  protocol <- builder_request_protocol(epoch = "worker-1")
  protocol <- builder_enqueue(protocol, builder_command("settings", "a"))
  protocol <- builder_enqueue(protocol, builder_command("review", "session"))
  dispatched <- builder_protocol_dispatch(protocol)
  completed <- builder_protocol_complete(
    dispatched$protocol,
    builder_worker_response(dispatched$request, error = "invalid settings")
  )

  terminal <- builder_protocol_fail_pending(
    completed$protocol,
    reason = "command_error"
  )

  expect_identical(terminal$failed$kind, "settings")
  expect_identical(terminal$failed$terminal_reason, "command_error")
  expect_identical(terminal$failed$terminal_status, "failed")
  expect_length(terminal$protocol$awaiting_ack, 0L)
  expect_identical(
    builder_protocol_dispatch(terminal$protocol)$request$kind,
    "review"
  )
})

test_that("failing a pending Build opens a new single-flight", {
  protocol <- builder_enqueue(
    builder_request_protocol(epoch = "worker-1"),
    builder_command("build", "session", payload = list(id = "build-1"))
  )
  dispatched <- builder_protocol_dispatch(protocol)

  terminal <- builder_protocol_fail_pending(
    dispatched$protocol,
    reason = "build_error"
  )

  expect_identical(terminal$failed$build_id, "build-1")
  expect_identical(terminal$protocol$build_status, "idle")
  expect_no_error(
    builder_enqueue(
      terminal$protocol,
      builder_command("build", "session", payload = list(id = "build-2"))
    )
  )
})

test_that("forgetting a dataset drops only its work and rebases barriers", {
  protocol <- builder_request_protocol(epoch = "worker-1")
  protocol <- builder_protocol_dataset(protocol, "a", 2L, "snapshot-a")
  protocol <- builder_protocol_dataset(protocol, "b", 3L, "snapshot-b")
  protocol <- builder_enqueue(protocol, builder_command("settings", "a"))
  protocol <- builder_enqueue(protocol, builder_command("settings", "b"))
  protocol <- builder_enqueue(protocol, builder_command("review", "session"))
  protocol <- builder_enqueue(
    protocol,
    builder_command("build", "session", payload = list(id = "build-1"))
  )

  forgotten <- builder_protocol_forget_dataset(
    protocol,
    "a",
    reason = "drop_succeeded"
  )

  expect_null(forgotten$protocol$datasets[["a"]])
  expect_identical(forgotten$protocol$datasets[["b"]]$revision, 3L)
  expect_identical(
    vapply(forgotten$failed, `[[`, character(1), "kind"),
    "settings"
  )
  expect_identical(
    builder_pending_ids(forgotten$protocol),
    c("settings:b", "review:session", "build:session")
  )
  persistent <- Filter(
    function(request) isTRUE(request$persistent),
    forgotten$protocol$queue
  )
  expect_identical(
    vapply(persistent, `[[`, integer(1), "persistent_sequence"),
    1:3
  )
  expect_identical(vapply(persistent, `[[`, integer(1), "cutoff"), 0:2)

  settings <- builder_protocol_dispatch(forgotten$protocol)
  settings_done <- builder_protocol_complete(
    settings$protocol,
    builder_worker_response(settings$request, TRUE)
  )
  protocol <- builder_protocol_acknowledge(
    settings_done$protocol,
    settings$request$request_id
  )
  review <- builder_protocol_dispatch(protocol)
  expect_identical(review$request$kind, "review")
  review_done <- builder_protocol_complete(
    review$protocol,
    builder_worker_response(review$request, TRUE)
  )
  protocol <- builder_protocol_acknowledge(
    review_done$protocol,
    review$request$request_id
  )
  expect_identical(builder_protocol_dispatch(protocol)$request$kind, "build")
})

test_that("forgetting a dataset discards its queries and awaiting drop", {
  protocol <- builder_protocol_dataset(
    builder_request_protocol(epoch = "worker-1"),
    "a",
    1L,
    "snapshot-a"
  )
  protocol <- builder_enqueue(
    protocol,
    builder_query("preview", "a", 1L, slot = "projection")
  )
  protocol <- builder_enqueue(
    protocol,
    builder_query("preview", "b", 1L, slot = "projection")
  )
  forgotten_queries <- builder_protocol_forget_dataset(protocol, "a")
  expect_identical(
    vapply(forgotten_queries$discarded, `[[`, character(1), "dataset"),
    "a"
  )
  expect_identical(
    builder_pending_ids(forgotten_queries$protocol),
    "preview:b:projection:1"
  )

  drop_protocol <- builder_protocol_dataset(
    builder_request_protocol(epoch = "worker-1"),
    "a",
    1L,
    "snapshot-a"
  )
  drop_protocol <- builder_enqueue(drop_protocol, builder_command("drop", "a"))
  drop_protocol <- builder_enqueue(
    drop_protocol,
    builder_command("settings", "a")
  )
  drop_protocol <- builder_enqueue(
    drop_protocol,
    builder_command("review", "session")
  )
  dispatched <- builder_protocol_dispatch(drop_protocol)
  completed <- builder_protocol_complete(
    dispatched$protocol,
    builder_worker_response(dispatched$request, TRUE)
  )

  forgotten_drop <- builder_protocol_forget_dataset(completed$protocol, "a")
  expect_identical(
    vapply(forgotten_drop$failed, `[[`, character(1), "kind"),
    c("drop", "settings")
  )
  expect_length(forgotten_drop$protocol$awaiting_ack, 0L)
  expect_identical(
    builder_protocol_dispatch(forgotten_drop$protocol)$request$kind,
    "review"
  )
})

if (builder_lifecycle_api_available) {
  .builder_wait_for_process_state <- function(
    process,
    expected,
    timeout = 10
  ) {
    deadline <- Sys.time() + timeout
    repeat {
      state <- .builder_worker_process_state(process, 0)
      if (identical(state, expected)) {
        return(invisible(state))
      }
      if (Sys.time() >= deadline) {
        stop("Timed out waiting for the Builder worker state.")
      }
      Sys.sleep(0.01)
    }
  }

  .builder_fake_process <- function(state, message = NULL) {
    process <- new.env(parent = emptyenv())
    process$poll_process <- function(timeout) state
    process$read <- function() message
    process$interrupt <- function() invisible(TRUE)
    process
  }

  .builder_fake_worker <- function(process, interrupt = NULL) {
    structure(
      list(process = process, interrupt = interrupt),
      class = c("builder_worker", "list")
    )
  }

  .builder_test_pid_alive <- function(pid) {
    tryCatch(
      ps::ps_is_running(ps::ps_handle(as.integer(pid))),
      error = function(error) FALSE
    )
  }

  .builder_wait_for_pid_file <- function(path, timeout = 10) {
    deadline <- Sys.time() + timeout
    repeat {
      if (file.exists(path)) {
        value <- suppressWarnings(as.integer(readLines(path, warn = FALSE)[1L]))
        if (length(value) == 1L && !is.na(value)) {
          return(value)
        }
      }
      if (Sys.time() >= deadline) {
        stop("Timed out waiting for the Builder child process.")
      }
      Sys.sleep(0.01)
    }
  }

  builder_worker_fixture <- function() {
    root <- tempfile("builder-main-snapshots-")
    if (!dir.create(root, mode = "0700")) {
      stop("Could not create the worker fixture root.")
    }
    snapshot <- builder_snapshot_seurat(
      SeuratObject::pbmc_small,
      file.path(root, "dataset-a"),
      available_bytes = 2^40
    )
    list(
      root = root,
      registry = list("dataset-a" = snapshot),
      snapshot = snapshot
    )
  }

  test_that("a fresh worker accepts an empty main-owned registry", {
    skip_if_not_installed("callr")
    worker <- builder_worker_start(builder_profile_inst_path("builder"))
    withr::defer({
      try(worker$process$close(), silent = TRUE)
      if (isTRUE(worker$owns_root)) {
        unlink(worker$snapshot_root, recursive = TRUE, force = TRUE)
      }
    })

    expect_null(worker$error)
    expect_true(worker$ready)
    expect_identical(worker$snapshot_registry, list())
  })

  test_that("session startup returns before a slow worker bootstrap completes", {
    skip_if_not_installed("callr")
    gate <- tempfile("builder-worker-startup-gate-")
    withr::defer(unlink(gate, force = TRUE))

    started_at <- system.time({
      started <- builder_session_start(
        builder_profile_inst_path("builder"),
        .async = TRUE,
        .bootstrap = local({
          gate_path <- gate
          function(...) {
            while (!file.exists(gate_path)) {
              Sys.sleep(0.01)
            }
            character()
          }
        })
      )
    })[["elapsed"]]
    worker <- started$worker
    withr::defer(try(builder_worker_stop(worker), silent = TRUE))

    expect_null(started$error)
    expect_lt(started_at, 1)
    expect_identical(worker$state, "starting")
    expect_false(worker$ready)

    file.create(gate)
    deadline <- Sys.time() + 5
    repeat {
      startup <- builder_session_poll_startup(worker)
      worker <- startup$worker
      if (!identical(startup$state, "starting")) {
        break
      }
      if (Sys.time() >= deadline) {
        fail("The gated Builder worker did not finish startup.")
      }
      Sys.sleep(0.01)
    }

    expect_identical(startup$state, "ready")
    expect_true(worker$ready)
  })

  test_that("a session can stop while worker bootstrap is still running", {
    skip_if_not_installed("callr")
    gate <- tempfile("builder-worker-stop-startup-gate-")
    withr::defer(unlink(gate, force = TRUE))
    worker <- builder_worker_start(
      builder_profile_inst_path("builder"),
      .async = TRUE,
      .bootstrap = local({
        gate_path <- gate
        function(...) {
          while (!file.exists(gate_path)) {
            Sys.sleep(0.01)
          }
          character()
        }
      })
    )
    process <- worker$process

    expect_identical(worker$state, "starting")
    expect_true(process$is_alive())

    stopped <- builder_worker_stop(worker, grace_ms = 1000L)

    expect_true(stopped$stopped)
    expect_false(process$is_alive())
    expect_true(stopped$tree_verified)
  })

  test_that("worker startup tolerates a cold CI process launch", {
    observed_timeout <- NULL
    failed <- builder_worker_start(
      builder_profile_inst_path("builder"),
      .new_session = function(wait_timeout) {
        observed_timeout <<- wait_timeout
        stop("synthetic startup failure")
      }
    )

    expect_identical(observed_timeout, 30000L)
    expect_match(failed$error, "synthetic startup failure", fixed = TRUE)
  })

  test_that("copied Builder runtimes reuse the loaded development source", {
    copied_runtime <- withr::local_tempdir()
    copied_builder <- file.path(copied_runtime, "builder")
    fs::dir_copy(builder_profile_inst_path("builder"), copied_builder)

    expect_identical(
      .builder_worker_package_source(copied_builder),
      normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
    )
  })

  test_that("copied Builder runtimes accept an explicit development source", {
    copied_runtime <- withr::local_tempdir()
    copied_builder <- file.path(copied_runtime, "builder")
    fs::dir_copy(builder_profile_inst_path("builder"), copied_builder)
    source_root <- withr::local_tempdir()
    file.copy(test_path("..", "..", "DESCRIPTION"), source_root)
    fs::dir_copy(test_path("..", "..", "R"), file.path(source_root, "R"))
    source_root <- normalizePath(source_root, winslash = "/", mustWork = TRUE)
    withr::local_envvar(CEREBRO_PACKAGE_SOURCE = source_root)

    expect_identical(
      .builder_worker_package_source(copied_builder),
      source_root
    )
  })

  test_that("main registry rejects an owned snapshot from another root", {
    skip_if_not_installed("callr")
    foreign <- builder_worker_fixture()
    withr::defer(.builder_snapshot_release(foreign$snapshot))
    local_root <- tempfile("builder-local-snapshots-")
    worker <- builder_worker_start(
      builder_profile_inst_path("builder"),
      snapshot_root = local_root,
      snapshot_registry = list()
    )
    withr::defer({
      try(worker$process$close(), silent = TRUE)
      unlink(local_root, recursive = TRUE, force = TRUE)
    })

    expect_error(
      builder_worker_register_snapshot(
        worker,
        "foreign",
        foreign$snapshot
      ),
      "snapshot root"
    )
    expect_length(worker$snapshot_registry, 0L)
  })

  test_that("dead workers reopen main-owned immutable snapshots", {
    skip_if_not_installed("callr")
    fixture <- builder_worker_fixture()
    withr::defer(.builder_snapshot_release(fixture$snapshot))
    worker <- builder_worker_start(
      builder_profile_inst_path("builder"),
      snapshot_root = fixture$root,
      snapshot_registry = fixture$registry
    )
    withr::defer(try(worker$process$close(), silent = TRUE))
    expect_true(worker$ready)
    expect_equal(builder_worker_cell_count(worker, "dataset-a"), 80L)
    old_epoch <- worker$epoch
    old_process <- worker$process

    restarted <- builder_worker_restart(worker)
    withr::defer(try(restarted$process$close(), silent = TRUE))

    expect_false(old_process$is_alive())
    expect_true(restarted$ready)
    expect_false(identical(restarted$epoch, old_epoch))
    expect_identical(
      restarted$snapshot_root,
      normalizePath(fixture$root, mustWork = TRUE)
    )
    expect_identical(restarted$snapshot_registry, fixture$registry)
    expect_equal(builder_worker_cell_count(restarted, "dataset-a"), 80L)
    expect_true(.builder_snapshot_owned(fixture$snapshot))
  })

  test_that("restart removes only verified unregistered snapshot artifacts", {
    skip_if_not_installed("callr")
    root <- withr::local_tempdir()
    registered <- builder_snapshot_seurat(
      SeuratObject::pbmc_small,
      file.path(root, "dataset-registered"),
      available_bytes = 2^40
    )
    orphan <- builder_snapshot_seurat(
      SeuratObject::pbmc_small,
      file.path(root, "dataset-orphan"),
      available_bytes = 2^40
    )
    stage <- file.path(root, "dataset-stage-incomplete")
    expect_true(dir.create(stage, mode = "0700"))
    .builder_stage_owner(stage)
    writeLines("partial", file.path(stage, "partial.txt"))

    unknown <- file.path(root, "foreign-directory")
    expect_true(dir.create(unknown))
    writeLines("keep", file.path(unknown, "foreign.txt"))

    tampered <- builder_snapshot_seurat(
      SeuratObject::pbmc_small,
      file.path(root, "dataset-tampered"),
      available_bytes = 2^40
    )
    tampered_marker <- readRDS(.builder_snapshot_marker_path(tampered$path))
    tampered_marker$owner_token <- paste0(tampered_marker$owner_token, "-other")
    saveRDS(tampered_marker, .builder_snapshot_marker_path(tampered$path))

    worker <- builder_worker_start(
      builder_profile_inst_path("builder"),
      snapshot_root = root,
      snapshot_registry = list("dataset-a" = registered)
    )
    old_process <- worker$process
    restarted <- builder_worker_restart(worker)
    withr::defer(try(restarted$process$close(), silent = TRUE))

    expect_false(old_process$is_alive())
    expect_true(.builder_snapshot_owned(registered))
    expect_false(dir.exists(orphan$path))
    expect_false(dir.exists(stage))
    expect_true(dir.exists(unknown))
    expect_true(file.exists(file.path(unknown, "foreign.txt")))
    expect_true(dir.exists(tampered$path))
  })

  test_that("restart failure is typed and leaves no running old process", {
    skip_if_not_installed("callr")
    root <- withr::local_tempdir()
    snapshot <- builder_snapshot_seurat(
      SeuratObject::pbmc_small,
      file.path(root, "dataset-a"),
      available_bytes = 2^40
    )
    worker <- builder_worker_start(
      builder_profile_inst_path("builder"),
      snapshot_root = root,
      snapshot_registry = list("dataset-a" = snapshot)
    )
    old_process <- worker$process
    unlink(snapshot$object_file)

    failed <- builder_worker_restart(worker)

    expect_s3_class(failed, "builder_worker_restart_failed")
    expect_identical(failed$event, "restart_failed")
    expect_match(
      failed$error,
      "snapshot is no longer owned",
      ignore.case = TRUE
    )
    expect_false(failed$worker$ready)
    expect_false(old_process$is_alive())
  })

  test_that("poll restarts a closed worker without reading its closed pipe", {
    skip_if_not_installed("callr")
    fixture <- builder_worker_fixture()
    withr::defer(.builder_snapshot_release(fixture$snapshot))
    worker <- builder_worker_start(
      builder_profile_inst_path("builder"),
      snapshot_root = fixture$root,
      snapshot_registry = fixture$registry
    )
    worker$process$close()

    polled <- builder_worker_poll(worker, timeout = 0)
    withr::defer(try(polled$worker$process$close(), silent = TRUE))

    expect_identical(polled$event, "restarted")
    expect_true(polled$worker$ready)
    expect_equal(
      builder_worker_cell_count(polled$worker, "dataset-a"),
      80L
    )
  })

  test_that("poll returns a typed event when snapshot restore fails", {
    skip_if_not_installed("callr")
    root <- withr::local_tempdir()
    snapshot <- builder_snapshot_seurat(
      SeuratObject::pbmc_small,
      file.path(root, "dataset-a"),
      available_bytes = 2^40
    )
    worker <- builder_worker_start(
      builder_profile_inst_path("builder"),
      snapshot_root = root,
      snapshot_registry = list("dataset-a" = snapshot)
    )
    unlink(snapshot$object_file)
    worker$process$close()

    polled <- builder_worker_poll(worker, timeout = 0)

    expect_identical(polled$event, "restart_failed")
    expect_match(
      polled$error,
      "snapshot is no longer owned",
      ignore.case = TRUE
    )
    expect_null(polled$result)
    expect_false(polled$worker$ready)
    expect_false(polled$worker$process$is_alive())
  })

  test_that("interrupt reports a closed-worker restart transition", {
    skip_if_not_installed("callr")
    worker <- builder_worker_start(builder_profile_inst_path("builder"))
    old_epoch <- worker$epoch
    root <- worker$snapshot_root
    worker$process$close()

    interrupted <- builder_worker_interrupt(worker)
    withr::defer({
      try(interrupted$worker$process$close(), silent = TRUE)
      unlink(root, recursive = TRUE, force = TRUE)
    })

    expect_identical(interrupted$event, "restarted")
    expect_true(interrupted$worker$ready)
    expect_false(identical(interrupted$worker$epoch, old_epoch))
  })

  test_that("interrupt returns an already-ready result exactly once", {
    skip_if_not_installed("callr")
    worker <- builder_worker_start(builder_profile_inst_path("builder"))
    withr::defer({
      try(worker$process$close(), silent = TRUE)
      if (isTRUE(worker$owns_root)) {
        unlink(worker$snapshot_root, recursive = TRUE, force = TRUE)
      }
    })
    worker$process$call(function() 42L)
    .builder_wait_for_process_state(worker$process, "ready")

    interrupted <- builder_worker_interrupt(worker)
    first <- builder_worker_poll(interrupted, timeout = 0)
    second <- builder_worker_poll(first$worker, timeout = 0)

    expect_identical(first$event, "completed")
    expect_identical(first$result$value, 42L)
    expect_true(first$result$done)
    expect_null(second$result)
    expect_null(second$event)
  })

  test_that("interrupt has one non-extensible five-second deadline", {
    process <- .builder_fake_process("working")
    worker <- .builder_fake_worker(process)
    started <- as.POSIXct("2026-08-04 10:00:00", tz = "UTC")

    first <- builder_worker_interrupt(
      worker,
      grace_ms = 9000L,
      now = started
    )
    repeated <- builder_worker_interrupt(
      first,
      grace_ms = 5000L,
      now = started + 1
    )

    expect_equal(as.numeric(first$interrupt$deadline - started), 5)
    expect_identical(repeated$interrupt$deadline, first$interrupt$deadline)
  })

  test_that("interim callr messages do not clear an interrupt deadline", {
    deadline <- as.POSIXct("2026-08-04 10:00:05", tz = "UTC")
    process <- .builder_fake_process("ready", list(code = 301L))
    worker <- .builder_fake_worker(
      process,
      interrupt = list(status = "waiting", deadline = deadline)
    )

    polled <- builder_worker_poll(
      worker,
      timeout = 0,
      now = deadline - 1
    )

    expect_null(polled$event)
    expect_null(polled$result)
    expect_identical(polled$worker$interrupt$status, "waiting")
    expect_identical(polled$worker$interrupt$deadline, deadline)
  })

  test_that("a callr read failure is one completed typed error", {
    process <- .builder_fake_process("ready")
    process$read <- function() stop("closed response channel")
    worker <- .builder_fake_worker(process)

    first <- builder_worker_poll(worker, timeout = 0)

    expect_identical(first$event, "completed")
    expect_true(first$result$done)
    expect_match(first$result$error, "could not be read")
  })

  test_that("interrupt is non-blocking then kills and restores an uncooperative worker", {
    skip_if_not_installed("callr")
    fixture <- builder_worker_fixture()
    withr::defer(.builder_snapshot_release(fixture$snapshot))
    worker <- builder_worker_start(
      builder_profile_inst_path("builder"),
      snapshot_root = fixture$root,
      snapshot_registry = fixture$registry
    )
    worker$process$call(function() {
      repeat {
        try(Sys.sleep(0.05), silent = TRUE)
      }
    })
    started <- Sys.time()
    interrupted <- builder_worker_interrupt(
      worker,
      grace_ms = 20L,
      now = started
    )
    expect_lt(as.numeric(difftime(Sys.time(), started, units = "secs")), 1)
    expect_identical(interrupted$epoch, worker$epoch)
    expect_identical(interrupted$interrupt$status, "waiting")

    polled <- builder_worker_poll(
      interrupted,
      timeout = 0,
      now = started + 1
    )
    withr::defer(try(polled$worker$process$close(), silent = TRUE))

    expect_identical(polled$event, "restarted")
    expect_false(identical(polled$worker$epoch, worker$epoch))
    expect_equal(
      builder_worker_cell_count(polled$worker, "dataset-a"),
      80L
    )
  })

  test_that("worker drop only evicts and never releases main-owned snapshots", {
    skip_if_not_installed("callr")
    fixture <- builder_worker_fixture()
    withr::defer(.builder_snapshot_release(fixture$snapshot))
    worker <- builder_worker_start(
      builder_profile_inst_path("builder"),
      snapshot_root = fixture$root,
      snapshot_registry = fixture$registry
    )
    withr::defer(try(worker$process$close(), silent = TRUE))

    worker$process$run(function() {
      store <- get(".builder_objects", envir = globalenv())
      rm(list = "dataset-a", envir = store)
      gc(FALSE)
      TRUE
    })

    expect_true(.builder_snapshot_owned(fixture$snapshot))
    expect_true(dir.exists(fixture$snapshot$path))
  })
}

if (builder_lifecycle_api_available && builder_worker_stop_api_available) {
  test_that("worker stop confirms process death before returning", {
    skip_if_not_installed("callr")
    worker <- builder_worker_start(builder_profile_inst_path("builder"))
    root <- worker$snapshot_root
    old_process <- worker$process

    stopped <- builder_worker_stop(worker, grace_ms = 9000L)
    withr::defer(unlink(root, recursive = TRUE, force = TRUE))

    expect_true(stopped$stopped)
    expect_null(stopped$error)
    expect_false(old_process$is_alive())
  })

  test_that("worker stop lets an active call unwind before force kill", {
    skip_if_not_installed("callr")
    worker <- builder_worker_start(builder_profile_inst_path("builder"))
    root <- worker$snapshot_root
    rollback_file <- tempfile("builder-stop-rollback-")
    started_file <- tempfile("builder-stop-started-")
    withr::defer({
      if (worker$process$is_alive()) {
        try(worker$process$kill_tree(), silent = TRUE)
      }
      unlink(root, recursive = TRUE, force = TRUE)
      unlink(rollback_file, force = TRUE)
      unlink(started_file, force = TRUE)
    })
    worker$process$call(
      function(rollback_path, started_path) {
        on.exit(writeLines("rolled back", rollback_path), add = TRUE)
        writeLines("started", started_path)
        repeat {
          Sys.sleep(0.05)
        }
      },
      args = list(
        rollback_path = rollback_file,
        started_path = started_file
      )
    )
    deadline <- Sys.time() + 10
    while (!file.exists(started_file) && Sys.time() < deadline) {
      Sys.sleep(0.01)
    }
    expect_true(file.exists(started_file))

    stopped <- builder_worker_stop(worker, grace_ms = 1000L)

    expect_true(stopped$stopped)
    expect_false(worker$process$is_alive())
    expect_true(file.exists(rollback_file))
    if (file.exists(rollback_file)) {
      expect_identical(readLines(rollback_file, warn = FALSE), "rolled back")
    }
  })

  test_that("dead-parent restart quarantines artifacts from a lost child", {
    skip_if_not_installed("callr")
    skip_if_not_installed("processx")
    skip_if_not_installed("ps")
    root <- withr::local_tempdir()
    pid_file <- tempfile("builder-orphan-child-pid-")
    withr::defer(unlink(pid_file, force = TRUE))
    registered <- builder_snapshot_seurat(
      SeuratObject::pbmc_small,
      file.path(root, "dataset-registered"),
      available_bytes = 2^40
    )
    orphan <- builder_snapshot_seurat(
      SeuratObject::pbmc_small,
      file.path(root, "dataset-orphan"),
      available_bytes = 2^40
    )
    worker <- builder_worker_start(
      builder_profile_inst_path("builder"),
      snapshot_root = root,
      snapshot_registry = list("dataset-a" = registered)
    )
    worker$process$call(
      function(path) {
        r_binary <- file.path(
          R.home("bin"),
          if (.Platform$OS.type == "windows") "R.exe" else "R"
        )
        child <- processx::process$new(
          r_binary,
          c("--vanilla", "--slave", "-e", "repeat { Sys.sleep(1) }"),
          cleanup = FALSE,
          cleanup_tree = FALSE
        )
        writeLines(as.character(child$get_pid()), path)
        quit(save = "no", status = 17L, runLast = FALSE)
      },
      args = list(path = pid_file)
    )
    child_pid <- .builder_wait_for_pid_file(pid_file)
    child_handle <- ps::ps_handle(child_pid)
    withr::defer({
      if (
        tryCatch(ps::ps_is_running(child_handle), error = function(error) FALSE)
      ) {
        try(tools::pskill(child_pid, signal = 9L), silent = TRUE)
      }
    })
    deadline <- Sys.time() + 10
    while (worker$process$is_alive() && Sys.time() < deadline) {
      Sys.sleep(0.01)
    }
    expect_false(worker$process$is_alive())
    expect_true(ps::ps_is_running(child_handle))

    restarted <- builder_worker_restart(worker)
    withr::defer({
      if (restarted$process$is_alive()) {
        try(restarted$process$kill_tree(), silent = TRUE)
      }
    })

    expect_s3_class(restarted, "builder_worker")
    expect_false(restarted$cleanup_safe)
    expect_null(restarted$cleanup)
    expect_true(ps::ps_is_running(child_handle))
    expect_true(dir.exists(registered$path))
    expect_true(dir.exists(orphan$path))
    expect_equal(builder_worker_cell_count(restarted, "dataset-a"), 80L)

    identity <- .builder_worker_identity(registered)
    restarted <- builder_worker_release_snapshot(
      restarted,
      "dataset-a",
      expected_identity = identity
    )
    expect_null(restarted$snapshot_registry[["dataset-a"]])
    expect_true(dir.exists(registered$path))

    stopped <- builder_worker_stop(restarted)

    expect_true(stopped$stopped)
    expect_false(stopped$worker$cleanup_safe)
    expect_null(stopped$cleanup)
    expect_true(dir.exists(registered$path))
    expect_true(dir.exists(orphan$path))
  })

  test_that("worker stop kills descendants before snapshot cleanup", {
    skip_if_not_installed("callr")
    skip_if_not_installed("processx")
    skip_if_not_installed("ps")
    root <- withr::local_tempdir()
    pid_file <- tempfile("builder-child-pid-")
    withr::defer(unlink(pid_file, force = TRUE))
    orphan <- builder_snapshot_seurat(
      SeuratObject::pbmc_small,
      file.path(root, "dataset-orphan"),
      available_bytes = 2^40
    )
    worker <- builder_worker_start(
      builder_profile_inst_path("builder"),
      snapshot_root = root,
      snapshot_registry = list()
    )
    parent_process <- worker$process
    worker$process$call(
      function(path) {
        r_binary <- file.path(
          R.home("bin"),
          if (.Platform$OS.type == "windows") "R.exe" else "R"
        )
        child <- processx::process$new(
          r_binary,
          c("--vanilla", "--slave", "-e", "repeat { Sys.sleep(1) }"),
          stdout = "|",
          stderr = "|",
          cleanup = FALSE,
          cleanup_tree = FALSE
        )
        writeLines(as.character(child$get_pid()), path)
        repeat {
          Sys.sleep(0.05)
        }
      },
      args = list(path = pid_file)
    )
    child_pid <- .builder_wait_for_pid_file(pid_file)
    withr::defer({
      if (.builder_test_pid_alive(child_pid)) {
        try(tools::pskill(child_pid, signal = 9L), silent = TRUE)
      }
    })
    expect_true(.builder_test_pid_alive(child_pid))

    cleanup_observation <- new.env(parent = emptyenv())
    cleanup_observation$child_alive <- NA
    original_cleanup_hook <- .builder_cleanup_after_check
    assign(
      ".builder_cleanup_after_check",
      function(path) {
        cleanup_observation$child_alive <- .builder_test_pid_alive(child_pid)
        invisible(path)
      },
      envir = globalenv()
    )
    withr::defer(assign(
      ".builder_cleanup_after_check",
      original_cleanup_hook,
      envir = globalenv()
    ))

    stopped <- builder_worker_stop(worker)

    expect_true(stopped$stopped)
    expect_false(parent_process$is_alive())
    expect_false(.builder_test_pid_alive(child_pid))
    expect_identical(cleanup_observation$child_alive, FALSE)
    expect_false(dir.exists(orphan$path))
  })
}

builder_session_api_available <- all(vapply(
  c(
    "builder_session_start",
    "builder_session_load",
    "builder_session_drop",
    "builder_session_poll"
  ),
  exists,
  logical(1),
  mode = "function",
  inherits = TRUE
)) &&
  identical(
    names(formals(builder_session_start)),
    c("builder_dir", "snapshot_root", "snapshot_registry")
  )

test_that("session calls expose the main-owned snapshot boundary", {
  expect_true(builder_session_api_available)
})

test_that("the Builder app has one protocol authority for worker requests", {
  app <- readLines(builder_profile_inst_path("builder", "app.R"), warn = FALSE)
  server <- unlist(lapply(
    c("foundation.R", "imports.R"),
    function(file) {
      readLines(
        builder_profile_inst_path("builder", "server", file),
        warn = FALSE
      )
    }
  ))
  text <- paste(c(app, server), collapse = "\n")
  worker_source <- grep('source("worker.R"', app, fixed = TRUE)[1L]
  session_source <- grep('source("session.R"', app, fixed = TRUE)[1L]

  expect_true(worker_source < session_source)
  expect_false(grepl("pending <- reactiveVal", text, fixed = TRUE))
  expect_false(grepl("queue <- reactiveVal", text, fixed = TRUE))
  expect_false(grepl("latest_request <- reactiveVal", text, fixed = TRUE))
  expect_match(text, "protocol <- reactiveVal", fixed = TRUE)
  expect_match(text, "builder_protocol_dispatch", fixed = TRUE)
  expect_match(text, "builder_protocol_complete", fixed = TRUE)
  expect_match(text, "builder_protocol_acknowledge", fixed = TRUE)
})

test_that("the app applies accepted snapshot ownership before ACK", {
  app <- readLines(
    builder_profile_inst_path("builder", "server", "imports.R"),
    warn = FALSE
  )
  completed <- grep(
    "completed <- builder_protocol_complete",
    app,
    fixed = TRUE
  )[1L]
  registered <- grep("builder_worker_register_snapshot", app, fixed = TRUE)[1L]
  released <- grep("builder_worker_release_snapshot", app, fixed = TRUE)[1L]
  acknowledged <- tail(
    grep("builder_protocol_acknowledge", app, fixed = TRUE),
    1L
  )

  expect_true(completed < registered)
  expect_true(completed < released)
  expect_true(registered < acknowledged)
  expect_true(released < acknowledged)
})

test_that("build dispatch scrubs protocol before worker availability can exit", {
  imports <- readLines(
    builder_profile_inst_path("builder", "server", "imports.R"),
    warn = FALSE
  )
  dispatch <- grep(
    "dispatched <- builder_protocol_dispatch",
    imports,
    fixed = TRUE
  )[1L]
  next_poller <- grep("## -- one poller", imports, fixed = TRUE)[1L]
  block <- imports[dispatch:(next_poller - 1L)]
  cleanup <- grep("on.exit(", block, fixed = TRUE)[1L]
  save_redacted <- grep("protocol(dispatched$protocol)", block, fixed = TRUE)[
    1L
  ]
  worker_lookup <- grep("current_worker <- worker()", block, fixed = TRUE)[1L]
  worker_req <- grep("req(current_worker)", block, fixed = TRUE)[1L]

  expect_true(cleanup < save_redacted)
  expect_true(save_redacted < worker_lookup)
  expect_true(worker_lookup < worker_req)
})

test_that("authentication material failure terminally acknowledges the build", {
  imports <- readLines(
    builder_profile_inst_path("builder", "server", "imports.R"),
    warn = FALSE
  )
  created <- grep("builder_auth_create_material", imports, fixed = TRUE)[1L]
  failed <- grep('inherits(auth_material, "try-error")', imports, fixed = TRUE)[
    1L
  ]
  next_switch <- grep("started_call <- try", imports, fixed = TRUE)[1L]
  branch <- imports[failed:(next_switch - 1L)]

  expect_true(created < failed)
  expect_match(
    paste(branch, collapse = "\n"),
    "builder_protocol_complete",
    fixed = TRUE
  )
  expect_match(
    paste(branch, collapse = "\n"),
    "builder_protocol_acknowledge",
    fixed = TRUE
  )
})

test_that("authentication creation failure leaves protocol ready for another build", {
  first <- builder_command(
    "build",
    "session",
    payload = list(
      kind = "build",
      auth_accounts = list(list(password = "lifecycle-secret-4f61"))
    )
  )
  protocol <- builder_enqueue(builder_request_protocol("auth-lifecycle"), first)
  sent <- builder_protocol_dispatch(protocol)
  safe_protocol <- builder_protocol_redact_auth(sent$protocol)
  safe_request <- builder_request_redact_auth(sent$request)
  completed <- builder_protocol_complete(
    safe_protocol,
    builder_worker_response(
      safe_request,
      value = list(error = "Authentication setup failed.")
    )
  )
  terminal <- builder_protocol_acknowledge(
    completed$protocol,
    safe_request$request_id
  )

  expect_identical(terminal$build_status, "idle")
  expect_null(terminal$pending)
  expect_length(terminal$awaiting_ack, 0L)
  second <- builder_command(
    "build",
    "session",
    payload = list(kind = "build", auth_accounts = list())
  )
  requeued <- builder_enqueue(terminal, second)
  expect_identical(requeued$build_status, "queued")
  expect_false(builder_auth_raw_contains(
    serialize(requeued, NULL),
    "lifecycle-secret-4f61"
  ))
})

test_that("alignment is persistent and Build freezes only after its barrier", {
  foundation <- readLines(
    builder_profile_inst_path("builder", "server", "foundation.R"),
    warn = FALSE
  )
  imports <- readLines(
    builder_profile_inst_path("builder", "server", "imports.R"),
    warn = FALSE
  )
  text <- paste(c(foundation, imports), collapse = "\n")
  replaceable <- grep("replaceable <-", foundation, fixed = TRUE)[1L]
  dispatch <- grep(
    "dispatched <- builder_protocol_dispatch",
    imports,
    fixed = TRUE
  )[1L]
  freeze <- grep("plan <- builder_make_plan", imports, fixed = TRUE)
  build_call <- grep("builder_session_build", imports, fixed = TRUE)[1L]

  replaceable_block <- paste(
    foundation[replaceable:(replaceable + 8L)],
    collapse = "\n"
  )
  expect_match(replaceable_block, '"preview"', fixed = TRUE)
  expect_match(replaceable_block, '"coords"', fixed = TRUE)
  expect_length(freeze, 1L)
  expect_true(dispatch < build_call)
  expect_false(grepl(
    'req$kind %in% c("preview", "coords", "align_all")',
    text,
    fixed = TRUE
  ))
})

test_that("a stale persistent response has an explicit acknowledged terminal", {
  protocol <- builder_request_protocol(epoch = "worker-1")
  protocol <- builder_protocol_dataset(
    protocol,
    "a",
    1L,
    "snapshot-a"
  )
  protocol <- builder_enqueue(
    protocol,
    builder_command(
      "alignment",
      "a",
      revision = 1L,
      snapshot_identity = "snapshot-a"
    )
  )
  sent <- builder_protocol_dispatch(protocol)
  protocol <- builder_protocol_dataset(
    sent$protocol,
    "a",
    2L,
    "snapshot-a"
  )
  response <- builder_worker_response(sent$request, TRUE)
  completed <- builder_protocol_complete(protocol, response)

  expect_false(completed$accepted)
  expect_null(completed$protocol$pending)
  expect_length(completed$protocol$awaiting_ack, 1L)
  terminal <- builder_protocol_acknowledge(
    completed$protocol,
    sent$request$request_id
  )
  expect_length(terminal$awaiting_ack, 0L)
  expect_identical(builder_protocol_dispatch(terminal)$blocked, "empty")
})

test_that("drop removes UI state only after owner release and protocol ACK", {
  app <- readLines(
    builder_profile_inst_path("builder", "server", "imports.R"),
    warn = FALSE
  )
  drop_branch <- grep(
    'identical(p$kind, "drop")',
    app,
    fixed = TRUE
  )[1L]
  release <- grep("builder_worker_release_snapshot", app, fixed = TRUE)
  release <- release[release > drop_branch][1L]
  acknowledge <- grep("builder_protocol_acknowledge", app, fixed = TRUE)
  remove_state <- grep(
    "Filter(function(e) !identical(e$id, p$id)",
    app,
    fixed = TRUE
  )
  remove_state <- remove_state[remove_state > release][1L]
  acknowledge <- acknowledge[acknowledge > remove_state][1L]

  expect_true(drop_branch < release)
  expect_true(release < acknowledge)
  expect_true(remove_state < acknowledge)
})

if (builder_session_api_available && builder_lifecycle_api_available) {
  .builder_wait_for_session <- function(worker, timeout = 30) {
    deadline <- Sys.time() + timeout
    repeat {
      polled <- builder_session_poll(worker, timeout = 100)
      worker <- polled$worker
      if (!is.null(polled$result)) {
        return(list(worker = worker, result = polled$result))
      }
      if (Sys.time() >= deadline) {
        stop("Timed out waiting for the Builder worker.")
      }
    }
  }

  test_that("load returns a descriptor and drop releases only after main ACK", {
    skip_if_not_installed("callr")
    root <- withr::local_tempdir()
    source <- file.path(root, "pbmc.rds")
    saveRDS(SeuratObject::pbmc_small, source)
    started <- builder_session_start(
      builder_profile_inst_path("builder"),
      snapshot_root = file.path(root, "snapshots"),
      snapshot_registry = list()
    )
    worker <- started$worker
    withr::defer(try(worker$process$close(), silent = TRUE))

    load_protocol <- builder_request_protocol(epoch = worker$epoch)
    load_protocol <- builder_enqueue(
      load_protocol,
      builder_command("load", "dataset-a")
    )
    sent <- builder_protocol_dispatch(load_protocol)
    builder_session_load(worker, "dataset-a", source, sent$request)
    loaded <- .builder_wait_for_session(worker)
    worker <- loaded$worker
    completed <- builder_protocol_complete(
      sent$protocol,
      loaded$result$value
    )
    expect_true(completed$accepted)
    snapshot <- completed$value$snapshot
    expect_true(.builder_snapshot_owned(snapshot))
    worker <- builder_worker_register_snapshot(worker, "dataset-a", snapshot)
    completed$protocol <- builder_protocol_acknowledge(
      completed$protocol,
      sent$request$request_id
    )
    unlink(source)

    drop_protocol <- builder_request_protocol(epoch = worker$epoch)
    identity <- .builder_worker_identity(snapshot)
    drop_protocol <- builder_enqueue(
      drop_protocol,
      builder_command(
        "drop",
        "dataset-a",
        snapshot_identity = identity
      )
    )
    dropped_request <- builder_protocol_dispatch(drop_protocol)
    builder_session_drop(worker, "dataset-a", dropped_request$request)
    dropped <- .builder_wait_for_session(worker)
    worker <- dropped$worker
    completed_drop <- builder_protocol_complete(
      dropped_request$protocol,
      dropped$result$value
    )

    expect_true(completed_drop$accepted)
    expect_true(.builder_snapshot_owned(snapshot))
    expect_true(dir.exists(snapshot$path))
    worker <- builder_worker_release_snapshot(
      worker,
      "dataset-a",
      expected_identity = identity
    )
    completed_drop$protocol <- builder_protocol_acknowledge(
      completed_drop$protocol,
      dropped_request$request$request_id
    )

    expect_false(dir.exists(snapshot$path))
    expect_null(worker$snapshot_registry[["dataset-a"]])
    expect_length(completed_drop$protocol$awaiting_ack, 0L)
  })
}
