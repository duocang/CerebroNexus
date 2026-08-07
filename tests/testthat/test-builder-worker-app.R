## -------------------------------------------------------------------------
## Builder worker integration contracts owned by the Shiny main process.
## -------------------------------------------------------------------------

builder_profile_source_runtime(globalenv())
builder_repo_source("io.R", local = globalenv())
builder_repo_source("adapters.R", local = globalenv())
builder_repo_source("state.R", local = globalenv())

builder_app_worker_path <- builder_profile_inst_path("builder", "worker.R")
if (nzchar(builder_app_worker_path) && file.exists(builder_app_worker_path)) {
  sys.source(builder_app_worker_path, envir = globalenv())
}
builder_repo_source("session.R", local = globalenv())

builder_app_lines <- function() {
  readLines(
    builder_profile_inst_path("builder", "app.R"),
    warn = FALSE
  )
}

builder_session_lines <- function() {
  readLines(
    builder_profile_inst_path("builder", "session.R"),
    warn = FALSE
  )
}

builder_app_block <- function(lines, start, finish) {
  first <- grep(start, lines, fixed = TRUE)[1L]
  last <- grep(finish, lines, fixed = TRUE)
  last <- last[last > first][1L]
  expect_false(is.na(first), info = paste("Missing App marker:", start))
  expect_false(is.na(last), info = paste("Missing App marker:", finish))
  paste(lines[first:(last - 1L)], collapse = "\n")
}

test_that("parent and worker load App assembly before build authorities", {
  app <- paste(builder_app_lines(), collapse = "\n")
  worker <- paste(
    readLines(builder_app_worker_path, warn = FALSE),
    collapse = "\n"
  )
  app_bundle_parent <- regexpr(
    'source("app_bundle.R", local = TRUE)',
    app,
    fixed = TRUE
  )[1L]
  coordinator_parent <- regexpr(
    'source("coordinator.R", local = TRUE)',
    app,
    fixed = TRUE
  )[1L]
  build_parent <- regexpr('source("build.R", local = TRUE)', app, fixed = TRUE)[
    1L
  ]
  app_bundle_worker <- regexpr(
    'source(file.path(dir, "app_bundle.R"))',
    worker,
    fixed = TRUE
  )[1L]
  build_worker <- regexpr(
    'source(file.path(dir, "build.R"))',
    worker,
    fixed = TRUE
  )[1L]

  expect_gt(app_bundle_parent, 0L)
  expect_lt(app_bundle_parent, coordinator_parent)
  expect_lt(app_bundle_parent, build_parent)
  expect_gt(app_bundle_worker, 0L)
  expect_lt(app_bundle_worker, build_worker)
  expect_false(grepl(
    'source(file.path(dir, "publish.R"))',
    worker,
    fixed = TRUE
  ))
})

test_that("preview and coordinates apply only to the visible dataset section", {
  lines <- builder_app_lines()
  preview <- builder_app_block(
    lines,
    'identical(p$kind, "preview")',
    'identical(p$kind, "coords")'
  )
  coords <- builder_app_block(
    lines,
    'identical(p$kind, "coords")',
    'identical(p$kind, "align_all")'
  )

  expect_match(preview, "identical(current(), p$id)", fixed = TRUE)
  expect_match(coords, "identical(current(), p$id)", fixed = TRUE)
  expect_match(coords, "identical(active_slice(), p$image)", fixed = TRUE)
  expect_true(grepl("preview_frame(value)", preview, fixed = TRUE))
  expect_true(grepl("spatial_coords(value)", coords, fixed = TRUE))
})

test_that("the App polls replaceable worker results every 100 milliseconds", {
  lines <- builder_app_lines()
  poller <- builder_app_block(
    lines,
    "## -- one poller drains",
    "## Take the per-section extents"
  )

  expect_match(poller, "invalidateLater(100, session)", fixed = TRUE)
  expect_false(grepl("invalidateLater(300, session)", poller, fixed = TRUE))
})

test_that("session calls return deterministic failures as typed responses", {
  session <- paste(builder_session_lines(), collapse = "\n")

  expect_gte(
    lengths(regmatches(
      session,
      gregexpr(
        "builder_worker_response\\(request, error = conditionMessage\\(error\\)\\)",
        session,
        perl = TRUE
      )
    )),
    7L
  )
  expect_match(
    session,
    "builder_worker_response(request, error = error)",
    fixed = TRUE
  )
  expect_false(grepl(
    "builder_worker_response(request, list(error = error))",
    session,
    fixed = TRUE
  ))
})

test_that("a missing align-all fails once and later FIFO work proceeds", {
  skip_if_not_installed("callr")
  root <- withr::local_tempdir()
  worker <- builder_worker_start(
    builder_profile_inst_path("builder"),
    snapshot_root = root,
    snapshot_registry = list()
  )
  withr::defer(try(builder_worker_stop(worker), silent = TRUE))

  protocol <- builder_request_protocol(worker$epoch)
  protocol <- builder_enqueue(
    protocol,
    builder_command(
      "align_all",
      "missing",
      payload = list(kind = "align_all")
    )
  )
  protocol <- builder_enqueue(
    protocol,
    builder_command("drop", "missing", payload = list(kind = "drop"))
  )
  dispatched <- builder_protocol_dispatch(protocol)
  expect_identical(dispatched$request$kind, "align_all")

  builder_session_section_bounds(
    worker,
    "missing",
    sections = "section-a",
    mode = "pixels",
    extent_width = 100,
    extent_height = 100,
    request = dispatched$request
  )
  deadline <- Sys.time() + 10
  repeat {
    polled <- builder_session_poll(worker, timeout = 100)
    worker <- polled$worker
    if (!is.null(polled$result)) {
      break
    }
    if (Sys.time() >= deadline) fail("Timed out waiting for align-all failure.")
  }

  expect_null(polled$result$error)
  for (field in c(
    "epoch",
    "request_id",
    "token",
    "generation",
    "dataset_revision",
    "snapshot_identity"
  )) {
    expect_identical(
      polled$result$value[[field]],
      dispatched$request[[field]],
      info = paste("Typed failure identity field:", field)
    )
  }
  expect_match(polled$result$value$error, "not found", ignore.case = TRUE)
  completed <- builder_protocol_complete(
    dispatched$protocol,
    polled$result$value
  )
  expect_true(completed$accepted)
  expect_match(completed$error, "not found", ignore.case = TRUE)
  protocol <- builder_protocol_acknowledge(
    completed$protocol,
    dispatched$request$request_id
  )

  following <- builder_protocol_dispatch(protocol)
  expect_identical(following$request$kind, "drop")
  expect_identical(following$request$dataset, "missing")
  expect_identical(following$request$epoch, worker$epoch)
})

test_that("an invalidated query failure leaves a queued Build runnable", {
  protocol <- builder_request_protocol("worker-a")
  protocol <- builder_enqueue(
    protocol,
    builder_query("preview", "dataset-a", generation = 1L)
  )
  dispatched <- builder_protocol_dispatch(protocol)
  protocol <- builder_enqueue(
    dispatched$protocol,
    builder_command("build", "session", payload = list(id = "build-a"))
  )

  completed <- builder_protocol_complete(
    protocol,
    builder_worker_response(
      dispatched$request,
      error = "The old preview no longer exists."
    )
  )
  expect_false(completed$accepted)
  expect_null(completed$protocol$pending)
  expect_identical(builder_pending_ids(completed$protocol), "build:session")

  build <- builder_protocol_dispatch(completed$protocol)
  expect_identical(build$request$kind, "build")
  expect_identical(build$protocol$build_status, "running")
})

test_that("business errors are terminalized without worker recovery", {
  lines <- builder_app_lines()
  poller <- builder_app_block(
    lines,
    "## -- one poller drains",
    "## Take the per-section extents"
  )
  typed_error <- builder_app_block(
    strsplit(poller, "\n", fixed = TRUE)[[1L]],
    "if (!is.null(completed$error))",
    'if (identical(p$kind, "load"))'
  )

  expect_match(typed_error, "builder_protocol_acknowledge", fixed = TRUE)
  expect_false(grepl("restart_worker_protocol", typed_error, fixed = TRUE))
})

test_that("a successful drop forgets all stale protocol state", {
  lines <- builder_app_lines()
  drop <- builder_app_block(
    lines,
    '} else if (identical(p$kind, "drop")) {',
    "## Take the per-section extents"
  )

  released <- regexpr("builder_worker_release_snapshot", drop, fixed = TRUE)[1L]
  acknowledged <- regexpr("builder_protocol_acknowledge", drop, fixed = TRUE)[
    1L
  ]
  forgotten <- regexpr("builder_protocol_forget_dataset", drop, fixed = TRUE)[
    1L
  ]

  expect_gt(released, 0L)
  expect_gt(acknowledged, released)
  expect_gt(forgotten, acknowledged)
  expect_match(drop, "failed", fixed = TRUE)
  expect_match(drop, "discarded", fixed = TRUE)
})

test_that("Build actions rerender without rebuilding persistent output inputs", {
  lines <- builder_app_lines()
  actionbar <- builder_app_block(
    lines,
    "output$actionbar <- renderUI({",
    "output$build_actions <- renderUI({"
  )
  actions <- builder_app_block(
    lines,
    "output$build_actions <- renderUI({",
    "## -- build"
  )

  expect_match(actionbar, '"build_actions"', fixed = TRUE)
  expect_false(grepl("protocol()", actionbar, fixed = TRUE))
  expect_false(grepl(
    'textInput(\n          "out_dir"',
    actionbar,
    fixed = TRUE
  ))
  expect_false(grepl('"Replace existing outputs"', actionbar, fixed = TRUE))
  expect_false(grepl("input$out_dir", actionbar, fixed = TRUE))
  expect_false(grepl("input$overwrite", actionbar, fixed = TRUE))
  expect_match(actions, "protocol()", fixed = TRUE)
  expect_match(actions, '"build"', fixed = TRUE)
  expect_match(actions, '"Choose a folder…"', fixed = TRUE)
  expect_match(actions, '"Building…"', fixed = TRUE)
  expect_false(grepl('"cancel_build"', actions, fixed = TRUE))
})

test_that("native output directory selection normalizes selection and preserves cancellation", {
  root <- withr::local_tempdir()
  nested <- file.path(root, "nested", "..", "output")
  dir.create(file.path(root, "nested"))
  dir.create(file.path(root, "output"))

  selected <- builder_choose_output_directory(.select = function() nested)
  cancelled <- builder_choose_output_directory(.select = function() NULL)
  failed <- builder_choose_output_directory(.select = function() {
    stop("picker unavailable")
  })

  expect_identical(selected$status, "selected")
  expect_identical(selected$path, normalizePath(file.path(root, "output")))
  expect_identical(cancelled, list(status = "cancelled", path = NULL))
  expect_identical(failed$status, "error")
  expect_match(failed$error, "picker unavailable", fixed = TRUE)
})

test_that("Build flow confirms multiple datasets and handles real conflicts", {
  app <- paste(builder_app_lines(), collapse = "\n")

  expect_match(app, '"builder_build_dialog"', fixed = TRUE)
  expect_match(app, '"Ready to build all datasets?"', fixed = TRUE)
  expect_match(app, '"Files already exist"', fixed = TRUE)
  expect_match(app, 'identical(action, "replace")', fixed = TRUE)
  expect_match(app, 'identical(action, "choose_another")', fixed = TRUE)
  expect_match(app, "builder_choose_output_directory()", fixed = TRUE)
  expect_false(grepl("isolate(input$out_dir)", app, fixed = TRUE))
  expect_false(grepl("isolate(input$overwrite)", app, fixed = TRUE))
})

test_that("Build flow requires current review revisions before final confirmation", {
  app <- paste(builder_app_lines(), collapse = "\n")

  expect_match(app, "reviewed_revision", fixed = TRUE)
  expect_match(app, '"Some datasets have not been reviewed"', fixed = TRUE)
  expect_match(app, '"Some datasets still need attention"', fixed = TRUE)
  expect_match(app, 'identical(action, "review_now")', fixed = TRUE)
  expect_match(app, 'identical(action, "fix_issues")', fixed = TRUE)
  expect_match(app, 'input$review_current_dataset', fixed = TRUE)
  expect_match(app, "next_unreviewed", fixed = TRUE)
  expect_match(app, '"builder_focus_review"', fixed = TRUE)
})

test_that("group color changes use the existing settings revision path", {
  app <- paste(builder_app_lines(), collapse = "\n")

  expect_match(app, 'input[["core-group_color"]]', fixed = TRUE)
  expect_match(app, "builder_update_color_override", fixed = TRUE)
  expect_match(app, 'input[["core-reset_colors"]]', fixed = TRUE)
  expect_match(app, "builder_reset_color_overrides", fixed = TRUE)
  expect_match(app, "replace_entry(entry)", fixed = TRUE)
  expect_false(grepl("umap_palette|pca_palette|tsne_palette", app))
})

test_that("the App describes object isolation accurately", {
  app <- paste(builder_app_lines(), collapse = "\n")

  expect_false(grepl("Objects are read into this process", app, fixed = TRUE))
  expect_match(app, "isolated worker process", fixed = TRUE)
})

test_that("unchanged settings do not write state or advance revisions", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  settings_observer <- builder_app_block(
    builder_app_lines(),
    "## -- keep the current entry's settings",
    "## Assay-dependent controls"
  )
  expect_match(
    settings_observer,
    "e <- isolate(entry_of(id))",
    fixed = TRUE
  )
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }

  shiny::testServer(app_env$server, {
    protocol(builder_request_protocol("worker-a"))
    entry <- list(
      id = "dataset-a",
      revision = 0L,
      snapshot = list(
        path = "/private/dataset-a",
        owner_token = "owner-a",
        object_md5 = strrep("a", 32L)
      ),
      profile = list(marker = "current"),
      settings = list(name = "Dataset A")
    )
    use_state_only_fixture(list(entry))

    expect_false(replace_entry(entry))
    expect_identical(sets()[[1L]]$revision, 0L)
    expect_null(protocol()$datasets[["dataset-a"]])

    changed <- entry
    changed$settings$name <- "Dataset A renamed"
    changed$profile$marker <- "stale candidate"
    expect_true(replace_entry(changed))
    expect_identical(sets()[[1L]]$revision, 1L)
    expect_identical(sets()[[1L]]$profile$marker, "current")
    expect_identical(
      protocol()$datasets[["dataset-a"]]$revision,
      1L
    )

    expect_false(replace_entry(sets()[[1L]]))
    expect_identical(sets()[[1L]]$revision, 1L)
  })
})

test_that("BuildPlan snapshots must match the worker registry exactly", {
  expect_true(exists(
    ".builder_session_plan_snapshot_error",
    mode = "function",
    inherits = TRUE
  ))
  if (
    !exists(
      ".builder_session_plan_snapshot_error",
      mode = "function",
      inherits = TRUE
    )
  ) {
    return(invisible(NULL))
  }

  snapshot <- list(
    path = "/private/snapshot-a",
    object_file = "/private/snapshot-a/object.rds",
    owner_token = "owner-a",
    created_at = as.POSIXct("2026-08-04 12:00:00", tz = "UTC"),
    object_md5 = strrep("a", 32L),
    closure_bytes = 1024
  )
  identity <- c(
    list(available = TRUE, snapshot = snapshot, source = list(id = "a")),
    snapshot
  )
  plan <- list(
    items = list(list(
      id = "dataset-a",
      name = "Dataset A",
      source_snapshot_identity = identity
    ))
  )

  expect_null(.builder_session_plan_snapshot_error(
    plan,
    list(`dataset-a` = snapshot)
  ))

  replaced <- snapshot
  replaced$object_md5 <- strrep("b", 32L)
  expect_match(
    .builder_session_plan_snapshot_error(
      plan,
      list(`dataset-a` = replaced)
    ),
    "does not match the frozen BuildPlan",
    fixed = TRUE
  )
  expect_match(
    .builder_session_plan_snapshot_error(plan, list()),
    "missing",
    fixed = TRUE
  )

  unavailable <- plan
  unavailable$items[[1L]]$source_snapshot_identity$available <- FALSE
  expect_match(
    .builder_session_plan_snapshot_error(
      unavailable,
      list(`dataset-a` = snapshot)
    ),
    "does not contain an owned frozen snapshot",
    fixed = TRUE
  )
})

test_that("direct and request-bearing Builds reject a replaced same-id snapshot", {
  skip_if_not_installed("callr")
  root <- withr::local_tempdir()
  frozen <- builder_snapshot_seurat(
    SeuratObject::pbmc_small,
    file.path(root, "dataset-frozen"),
    available_bytes = 2^40
  )
  replacement <- builder_snapshot_seurat(
    SeuratObject::pbmc_small,
    file.path(root, "dataset-replacement"),
    available_bytes = 2^40
  )
  worker <- builder_worker_start(
    builder_profile_inst_path("builder"),
    snapshot_root = root,
    snapshot_registry = list(`dataset-a` = replacement)
  )
  withr::defer({
    try(builder_worker_stop(worker), silent = TRUE)
    if (isTRUE(.builder_snapshot_owned(frozen))) {
      .builder_snapshot_release(frozen)
    }
    if (isTRUE(.builder_snapshot_owned(replacement))) {
      .builder_snapshot_release(replacement)
    }
  })

  identity <- c(
    list(available = TRUE, snapshot = frozen, source = list(id = "a")),
    frozen
  )
  plan <- list(
    items = list(list(
      id = "dataset-a",
      name = "Dataset A",
      source_snapshot_identity = identity
    ))
  )
  wait_for_result <- function(worker) {
    deadline <- Sys.time() + 10
    repeat {
      polled <- builder_session_poll(worker, timeout = 100)
      worker <- polled$worker
      if (!is.null(polled$result)) {
        return(list(worker = worker, result = polled$result))
      }
      if (Sys.time() >= deadline) {
        fail("Timed out waiting for the frozen-snapshot rejection.")
      }
    }
  }

  builder_session_build(worker, plan)
  direct <- wait_for_result(worker)
  worker <- direct$worker
  expect_true(direct$result$done)
  expect_match(
    direct$result$value$error,
    "does not match the frozen BuildPlan",
    fixed = TRUE
  )

  protocol <- builder_enqueue(
    builder_request_protocol(worker$epoch),
    builder_command("build", "session", payload = list(id = "build-1"))
  )
  dispatched <- builder_protocol_dispatch(protocol)

  builder_session_build(worker, plan, dispatched$request)
  requested <- wait_for_result(worker)
  polled <- list(worker = requested$worker, result = requested$result)
  worker <- polled$worker

  expect_true(polled$result$done)
  expect_match(
    polled$result$value$error,
    "does not match the frozen BuildPlan",
    fixed = TRUE
  )
  expect_identical(polled$result$value$build_id, "build-1")
})

test_that("worker failures recover or terminate every protocol barrier", {
  app <- paste(builder_app_lines(), collapse = "\n")

  expect_match(app, "builder_protocol_recover", fixed = TRUE)
  expect_match(app, 'identical(got$event, "restart_failed")', fixed = TRUE)
  expect_match(app, "retry_persistent = FALSE", fixed = TRUE)
  expect_match(app, "restart_worker_protocol <- function", fixed = TRUE)
  expect_gte(
    lengths(regmatches(
      app,
      gregexpr("restart_worker_protocol\\(", app, perl = TRUE)
    )),
    4L
  )

  register_failure <- builder_app_block(
    builder_app_lines(),
    "if (inherits(updated_worker, \"try-error\"))",
    "worker(updated_worker)"
  )
  release_failure <- builder_app_block(
    builder_app_lines(),
    "if (inherits(released, \"try-error\"))",
    "worker(released$worker)"
  )
  expect_match(
    app,
    "builder_snapshot_release_transition(",
    fixed = TRUE
  )
  expect_match(register_failure, "restart_worker_protocol(", fixed = TRUE)
  expect_match(release_failure, "restart_worker_protocol(", fixed = TRUE)
})

test_that("the App exposes one Build flight without unsafe hard cancellation", {
  lines <- builder_app_lines()
  app <- paste(lines, collapse = "\n")
  actions <- builder_app_block(
    lines,
    "output$build_actions <- renderUI({",
    "## -- build"
  )

  expect_match(
    app,
    "build_state <- reactiveVal(builder_build_state())",
    fixed = TRUE
  )
  expect_match(app, "builder_reduce_build", fixed = TRUE)
  expect_false(grepl('observeEvent(input$cancel_build, {', app, fixed = TRUE))
  expect_false(grepl("builder_protocol_cancel", app, fixed = TRUE))
  expect_false(grepl("builder_worker_interrupt", app, fixed = TRUE))
  expect_false(grepl('"cancel_build"', actions, fixed = TRUE))
  expect_match(actions, 'c("queued", "running", "cancelling")', fixed = TRUE)
  expect_match(actions, "build_in_flight", fixed = TRUE)
})

test_that("session shutdown stops the worker before releasing snapshots", {
  lines <- builder_app_lines()
  shutdown <- builder_app_block(
    lines,
    "session$onSessionEnded(function() {",
    "## -- native file picker and examples"
  )
  stopped <- regexpr("builder_worker_stop", shutdown, fixed = TRUE)[1L]
  confirmed <- regexpr("isTRUE(stopped$stopped)", shutdown, fixed = TRUE)[1L]
  cleanup_safe <- regexpr(
    "isTRUE(stopped$worker$cleanup_safe)",
    shutdown,
    fixed = TRUE
  )[1L]
  released <- regexpr(".builder_snapshot_release", shutdown, fixed = TRUE)[1L]

  expect_gt(stopped, 0L)
  expect_gt(confirmed, stopped)
  expect_gt(cleanup_safe, confirmed)
  expect_gt(released, cleanup_safe)
  expect_match(
    shutdown,
    "unlink(current_worker$snapshot_root, recursive = TRUE, force = TRUE)",
    fixed = TRUE
  )
  expect_false(grepl("process$close()", shutdown, fixed = TRUE))
})

test_that("session build validates frozen snapshots before execution", {
  session <- paste(
    readLines(
      builder_profile_inst_path("builder", "session.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  validate <- regexpr(
    ".builder_session_plan_snapshot_error",
    session,
    fixed = TRUE
  )[1L]
  execute <- regexpr(
    "builder_execute_plan(plan, stage, registry)",
    session,
    fixed = TRUE
  )[1L]

  expect_gt(validate, 0L)
  expect_gt(execute, validate)
  expect_match(session, "snapshot_validator", fixed = TRUE)
})
