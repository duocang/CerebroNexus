builder_build_session_app <- function(scope = parent.frame()) {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"), .local_envir = scope)
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this Build session test.")
  }
  app_env$builder_session_example <- function(...) invisible(TRUE)
  app_env$builder_session_poll <- function(worker, ...) {
    list(worker = worker, event = NULL, result = NULL)
  }
  app_env
}

builder_build_session_plan <- function(make_app = TRUE) {
  item <- function(id) {
    list(
      id = id,
      name = toupper(id),
      filename = paste0(id, ".crb"),
      sidecars = character(),
      source_snapshot_identity = list(
        available = TRUE,
        object_md5 = strrep(substr(id, 1L, 1L), 32L)
      )
    )
  }
  structure(
    list(
      make_app = make_app,
      dataset_order = c("a", "b"),
      items = list(item("a"), item("b"))
    ),
    class = c("builder_build_plan", "list")
  )
}

test_that("a published Build records its CRBs for the next Build", {
  app_env <- builder_build_session_app()
  release <- withr::local_tempdir()
  private <- file.path(release, "cerebro_app", "private-data")
  dir.create(private, recursive = TRUE)
  for (id in c("a", "b")) {
    writeLines(id, file.path(private, paste0(id, ".crb")))
  }
  entry <- function(id) {
    path <- file.path(private, paste0(id, ".crb"))
    list(
      path = paste0("cerebro_app/private-data/", id, ".crb"),
      type = "file",
      size = file.size(path),
      md5 = unname(tools::md5sum(path))
    )
  }
  plan <- builder_build_session_plan()
  value <- app_env$builder_result_success(
    published = TRUE,
    built = c(
      a = file.path(private, "a.crb"),
      b = file.path(private, "b.crb")
    ),
    app_dir = file.path(release, "cerebro_app"),
    app_verified = TRUE,
    verifications = list(
      a = list(valid = TRUE, path = file.path(private, "a.crb")),
      b = list(valid = TRUE, path = file.path(private, "b.crb"))
    ),
    release = list(
      target = release,
      identity = list(entries = list(entry("a"), entry("b")))
    )
  )

  shiny::testServer(app_env$server, {
    session$flushReact()
    builder_build_remember_plan(plan)
    result(value)
    session$flushReact()

    keys <- ls(builder_session_reuse_cache)
    expect_length(keys, 2L)
    expect_null(builder_build_reuse_state$plan)
    record <- app_env$builder_plan_reuse_lookup(
      builder_session_reuse_cache,
      app_env$builder_plan_reuse_key(plan$items[[1L]])
    )
    expect_identical(record$path, file.path(private, "a.crb"))

    # A failed Build that reused a cached CRB forgets that entry.
    failed_plan <- plan
    failed_plan$items[[1L]]$reused_artifact <- list(path = record$path)
    builder_build_remember_plan(failed_plan)
    result(app_env$builder_result_failure("The reusable CRB could not be staged."))
    session$flushReact()
    remaining <- ls(builder_session_reuse_cache)
    expect_length(remaining, 1L)
    expect_true(startsWith(remaining, "b::"))

    # Retry optional work: the datasets that finished before the failed
    # analysis are retained and offered to the retry.
    kept <- file.path(release, "kept-a.crb")
    writeLines("kept", kept)
    builder_build_remember_plan(plan)
    result(app_env$.builder_result(
      "needs_decision",
      "Dataset B: analysis `marker_genes` failed.",
      list(
        failed_dataset_id = "b",
        retained_artifacts = list(a = list(
          path = kept,
          fingerprint = list(md5 = unname(tools::md5sum(kept))),
          members = list()
        ))
      )
    ))
    session$flushReact()
    record <- app_env$builder_plan_reuse_lookup(
      builder_session_reuse_cache,
      app_env$builder_plan_reuse_key(plan$items[[1L]])
    )
    expect_identical(record$path, kept)
  })
})

test_that("the Build panel shows dataset, step and elapsed time", {
  app_env <- builder_build_session_app()
  now <- as.POSIXct("2026-10-17 12:00:00", tz = "UTC")
  message <- app_env$builder_build_progress_message(
    list(
      phase = "datasets",
      dataset_index = 2L,
      dataset_count = 5L,
      dataset_name = "PBMC 3k",
      step_label = "Marker genes",
      started_at = as.numeric(now) - 83
    ),
    now = now
  )
  expect_identical(message$text, "Dataset 2/5 · PBMC 3k · Marker genes")
  expect_identical(message$elapsed_seconds, 83)
  expect_identical(
    app_env$builder_build_progress_message(list(phase = "publish"))$text,
    "Publishing release"
  )
  expect_false(app_env$builder_build_progress_message(NULL)$active)

  html <- as.character(app_env$builder_build_stage_status_body_ui(list(
    state = "building",
    message = "Building 5 datasets…"
  )))
  expect_match(html, "data-build-progress-detail", fixed = TRUE)

  progress <- withr::local_tempfile(fileext = ".rds")
  shiny::testServer(app_env$server, {
    session$flushReact()
    app_env$builder_build_progress_write(
      progress,
      "datasets",
      list(
        dataset_index = 1L,
        dataset_count = 2L,
        dataset_name = "Lung",
        step_label = "Writing CRB",
        started_at = as.numeric(Sys.time())
      )
    )
    release_settlement_context(list(progress_path = progress))
    session$flushReact()
    sent <- builder_build_progress_sent$message
    expect_true(sent$active)
    expect_identical(sent$text, "Dataset 1/2 · Lung · Writing CRB")

    release_settlement_context(NULL)
    session$flushReact()
    expect_identical(builder_build_progress_sent$message, list(active = FALSE))
  })
})

test_that("runtime identity reuses MD5s of files whose stats are unchanged", {
  app_env <- builder_build_session_app()
  root <- withr::local_tempdir()
  files <- file.path(root, c("a.R", "b.R"))
  writeLines("a <- 1", files[[1L]])
  writeLines("b <- 2", files[[2L]])
  stats <- new.env(parent = emptyenv())
  first <- app_env$builder_runtime_file_identity(files, .stats = stats)
  previous <- list(identity = first, stats = stats$stats)
  hashed <- character()
  counting <- function(paths) {
    hashed <<- c(hashed, basename(paths))
    tools::md5sum(paths)
  }

  again <- app_env$builder_runtime_file_identity(
    files,
    .md5 = counting,
    .previous = previous
  )
  expect_identical(again, first)
  expect_identical(hashed, character())

  writeLines("b <- 3 # changed", files[[2L]])
  changed <- app_env$builder_runtime_file_identity(
    files,
    .md5 = counting,
    .previous = previous
  )
  expect_identical(hashed, "b.R")
  expect_identical(changed, app_env$builder_runtime_file_identity(files))
  expect_false(identical(changed, first))
})

test_that("a prewarmed coordinator runs the release preparation", {
  app_env <- builder_build_session_app()
  target <- file.path(withr::local_tempdir(), "release")
  plan <- structure(
    list(
      revision = 1L,
      readiness = "ready",
      out_dir = target,
      overwrite = FALSE,
      targets = file.path(target, "dataset.crb"),
      output_release = list(targets = file.path(target, "dataset.crb")),
      make_app = FALSE,
      app_contract_version = 0L,
      dataset_order = "dataset-1",
      items = list(list(
        id = "dataset-1",
        name = "Dataset 1",
        filename = "dataset.crb",
        organism = "hg",
        analyses = character(),
        included_groups = character(),
        included_projections = character(),
        metadata_policy = list(included = character()),
        expression_backend = "embedded",
        sidecars = character(),
        viewer_page_expectations = list(visible_conditional = character())
      )),
      manifest = list(),
      acknowledgements = list(),
      viewer_bundle_assets = character(),
      private_assets = "dataset.crb",
      app_options = list(enabled = FALSE),
      app_auth = list(enabled = FALSE, account_count = 0L, timeout_minutes = 15L)
    ),
    class = c("builder_build_plan", "list")
  )
  shiny::testServer(app_env$server, {
    on.exit(builder_coordinator_stop_warm(), add = TRUE)
    builder_prewarm_release_processes("preparation")
    warm <- builder_coordinator_process_state$warm$preparation
    expect_true(warm$process$is_alive())

    process <- builder_start_release_preparation_process(plan, "prewarmed")
    expect_identical(process$get_pid(), warm$process$get_pid())
    expect_null(builder_coordinator_process_state$warm$preparation)
    # Settlement loads while the worker would build.
    expect_true(builder_coordinator_process_state$warm$settlement$process$is_alive())
    process$wait(120000)
    prepared <- process$get_result()
    on.exit(app_env$builder_coordinator_abort(prepared$coordinator), add = TRUE)
    expect_identical(prepared$coordinator$pid, as.integer(Sys.getpid()))
    expect_true(dir.exists(prepared$coordinator$stage))
    expect_identical(
      prepared$runtime$identity,
      app_env$builder_runtime_file_identity(prepared$runtime$files)
    )
    cached <- builder_coordinator_runtime_previous()
    expect_identical(cached$identity, prepared$runtime$identity)
  })
})

test_that("a coordinator refuses a job if its runtime changed after loading", {
  app_env <- builder_build_session_app()
  root <- withr::local_tempdir()
  runtime_file <- file.path(root, "runtime.R")
  writeLines("loaded_marker <- TRUE", runtime_file)
  inbox <- file.path(root, "inbox.rds")
  saveRDS(
    list(
      run = function(runtime, identity, args) list(ran = TRUE),
      args = list()
    ),
    inbox
  )
  calls <- 0L
  identity <- function(paths, .previous = NULL, .stats = NULL) {
    calls <<- calls + 1L
    list(schema_version = 1L, files = list(list(path = "x", md5 = calls)))
  }
  process_main <- NULL
  shiny::testServer(app_env$server, {
    process_main <<- builder_coordinator_process_main
  })
  main <- function(runtime_identity) {
    process_main(
      inbox = inbox,
      runtime_files = list(root = root, marker = runtime_file),
      package_files = character(),
      package_source = NULL,
      execution_files = runtime_file,
      previous = NULL,
      cache_path = file.path(root, "runtime-identity.rds"),
      parent_pid = Sys.getpid(),
      idle_seconds = 5,
      runtime_identity = runtime_identity,
      source_utf8 = app_env$builder_source_utf8
    )
  }

  expect_error(main(identity), "runtime changed while its coordinator")
  saveRDS(
    list(run = function(runtime, identity, args) {
      list(ran = isTRUE(runtime$loaded_marker))
    }, args = list()),
    inbox
  )
  stable <- function(paths, .previous = NULL, .stats = NULL) {
    list(schema_version = 1L, files = list(list(path = "x", md5 = "same")))
  }
  expect_identical(main(stable), list(ran = TRUE))
  expect_false(file.exists(inbox))
})
