## Project open/save responsiveness: background copies, queued writes and
## work skipped when nothing changed.

builder_responsiveness_runtime <- function() {
  runtime <- new.env(parent = globalenv())
  sys.source(
    builder_test_inst_path("builder", "prerequisite.R"),
    envir = runtime
  )
  for (file in c("core/bundle_path_contract.R", "io.R", "project.R")) {
    runtime$builder_source_utf8(
      builder_test_inst_path("builder", file),
      envir = runtime
    )
  }
  runtime
}

builder_responsiveness_app <- function(env = parent.frame()) {
  withr::local_options(
    CerebroNexus.builder.project_worker_prewarm = FALSE,
    .local_envir = env
  )
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_test_inst_path("builder"), .local_envir = env)
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this responsiveness test.")
  }
  app_env
}

builder_responsiveness_run_later <- function(done, timeout = 10) {
  deadline <- Sys.time() + timeout
  while (!isTRUE(done()) && Sys.time() < deadline) {
    later::run_now(0.05)
  }
  isTRUE(done())
}

test_that("a source job skips copying when its blob already verifies", {
  runtime <- builder_responsiveness_runtime()
  root <- withr::local_tempdir()
  project <- file.path(root, "project")
  dir.create(project)
  source <- file.path(root, "session-sources", "ds1", "source.rds")
  dir.create(dirname(source), recursive = TRUE)
  writeBin(charToRaw("source-bytes"), source)
  md5 <- unname(tools::md5sum(source))
  entry <- list(
    id = "ds1",
    path = source,
    filename = "sample.rds",
    snapshot = list(source_fingerprint = paste0("builder-snapshot-v2:", md5))
  )
  job <- runtime$builder_project_source_job(entry, project)
  expect_true(runtime$builder_project_content_addressed_source(
    job$relative,
    "ds1"
  ))

  first <- runtime$builder_project_copy_source_job(job)
  expect_identical(first$status, "ready")
  expect_identical(first$planned, job$relative)

  ## A copy queued again for the same bytes (for example after an earlier
  ## run was cut short) publishes nothing new and needs no source.
  unlink(source)
  info_before <- file.info(first$path)$mtime
  second <- runtime$builder_project_copy_source_job(job)
  expect_identical(second$status, "ready")
  expect_identical(second$path, first$path)
  expect_identical(second$fingerprint$md5, md5)
  expect_identical(file.info(second$path)$mtime, info_before)
  expect_false(file.exists(job$part))

  ## A blob that no longer matches is never reused.
  writeBin(charToRaw("tampered"), first$path)
  third <- runtime$builder_project_copy_source_job(job)
  expect_identical(third$status, "failed")
})

test_that("finished copies merge only into records still waiting for them", {
  runtime <- builder_responsiveness_runtime()
  root <- withr::local_tempdir()
  target <- file.path(root, "sources", "ds1", "blobs", strrep("a", 32), "source.rds")
  dir.create(dirname(target), recursive = TRUE)
  writeBin(charToRaw("ready"), target)
  planned <- paste("sources", "ds1", "blobs", strrep("a", 32), "source.rds", sep = "/")
  record <- function(id, status, path) {
    list(
      id = id,
      source = list(kind = "managed", path = path, status = status)
    )
  }
  manifest <- list(
    project = list(id = "p1", revision = 7L),
    datasets = list(
      record("ds1", "pending", planned),
      record("ds2", "pending", "sources/ds2/blobs/other/source.rds"),
      record("ds3", "ready", "sources/ds3/source.rds")
    )
  )
  result <- function(id) {
    list(
      id = id,
      status = "ready",
      path = target,
      planned = planned,
      fingerprint = list(md5 = strrep("a", 32))
    )
  }
  updated <- runtime$builder_project_apply_source_results(
    manifest,
    list(result("ds1"), result("ds2"), result("ds3")),
    root,
    only_pending = TRUE
  )
  expect_identical(updated$datasets[[1L]]$source$status, "ready")
  expect_identical(updated$datasets[[1L]]$source$path, planned)
  ## ds2 now waits for a different source; ds3 was already saved meanwhile.
  expect_identical(updated$datasets[[2L]], manifest$datasets[[2L]])
  expect_identical(updated$datasets[[3L]], manifest$datasets[[3L]])
})

test_that("a source job identical to the running copy is not queued again", {
  runtime <- builder_responsiveness_runtime()
  job <- function(id, md5, source = paste0("/s/", id)) {
    list(
      id = id,
      source = source,
      source_md5 = md5,
      relative = paste("sources", id, "blobs", md5, "source.rds", sep = "/")
    )
  }
  running <- list(job("ds1", strrep("a", 32)))
  queued <- runtime$builder_project_source_queue_add(
    list(ds1 = job("ds1", strrep("0", 32))),
    list(job("ds1", strrep("a", 32)), job("ds2", strrep("b", 32))),
    running
  )
  expect_identical(names(queued), "ds2")

  ## New bytes for the same dataset are queued behind the running copy.
  queued <- runtime$builder_project_source_queue_add(
    list(),
    list(job("ds1", strrep("c", 32))),
    running
  )
  expect_identical(queued$ds1$source_md5, strrep("c", 32))
})

test_that("queued manifest writes start from the latest Project revision", {
  skip_if_not_installed("shiny")
  app_env <- builder_responsiveness_app()

  shiny::testServer(app_env$server, {
    server_env <- environment(start_builder_project_manifest_write)
    calls <- new.env(parent = emptyenv())
    calls$revisions <- integer()
    calls$manifests <- list()
    server_env$builder_project_write_process <- function(
      manifest,
      root,
      expected_revision,
      ...
    ) {
      calls$revisions <- c(calls$revisions, as.integer(expected_revision))
      calls$manifests[[length(calls$manifests) + 1L]] <- manifest
      manifest$project$revision <- as.integer(expected_revision) + 1L
      list(
        is_alive = function() FALSE,
        get_result = function() {
          list(manifest = manifest, path = file.path(root, "builder-project.json"))
        }
      )
    }
    project <- list(
      root = "project-root",
      path = "project-root/builder-project.json",
      name = "P",
      manifest = list(
        project = list(id = "p1", name = "P", revision = 3L),
        datasets = list()
      )
    )
    builder_project(project)
    apply_written <- function(written) {
      current <- isolate(builder_project())
      current$manifest <- written$manifest
      builder_project(current)
    }

    start_builder_project_manifest_write(project$manifest, project, apply_written)
    start_builder_project_manifest_write(
      function(current) {
        manifest <- current$manifest
        manifest$derived_from <- current$manifest$project$revision
        manifest
      },
      project,
      apply_written
    )
    expect_true(builder_project_manifest_write_pending())
    expect_true(builder_responsiveness_run_later(
      function() !builder_project_manifest_write_pending()
    ))
    ## The second write waited for the first and built on its revision
    ## instead of failing the revision check.
    expect_identical(calls$revisions, c(3L, 4L))
    expect_identical(calls$manifests[[2L]]$derived_from, 4L)
    expect_identical(isolate(builder_project())$manifest$project$revision, 5L)

    ## A queued write never lands on a different Project.
    other <- project
    other$root <- "another-root"
    outcome <- new.env(parent = emptyenv())
    start_builder_project_manifest_write(
      other$manifest,
      other,
      function(written) outcome$value <- written
    )
    expect_true(builder_responsiveness_run_later(
      function() !is.null(outcome$value)
    ))
    expect_s3_class(outcome$value, "error")
    expect_length(calls$revisions, 2L)
  })
})

test_that("an unchanged Save is recognised without a manifest transaction", {
  runtime <- builder_responsiveness_runtime()
  configuration <- runtime$builder_project_configuration()
  last_ui <- list(stage = "configure", selected_dataset = "ds1")
  record <- list(
    id = "ds1",
    configuration = list(path = "datasets/ds1/config.json", checked = TRUE),
    source = list(kind = "managed", status = "ready", path = "sources/ds1/source.rds")
  )
  manifest <- list(
    schema_version = 3L,
    project = list(id = "p1", revision = 4L),
    datasets = list(record),
    configuration = configuration,
    last_ui = last_ui
  )
  entries <- list(list(id = "ds1", revision = 2L))
  context <- list(configuration = configuration, last_ui = last_ui)
  base_manifest <- manifest
  base_entries <- entries
  base_context <- context
  unchanged <- function(
    manifest = base_manifest,
    entries = base_entries,
    context = base_context,
    jobs = list()
  ) {
    runtime$builder_project_save_unchanged(manifest, entries, context, jobs)
  }

  expect_true(unchanged())
  ## A JSON round trip of the saved manifest still compares equal.
  reread <- jsonlite::fromJSON(
    jsonlite::toJSON(manifest, auto_unbox = TRUE, null = "null", digits = NA),
    simplifyVector = FALSE
  )
  expect_true(unchanged(manifest = reread))

  pending <- manifest
  pending$datasets[[1L]]$source$status <- "pending"
  expect_false(unchanged(manifest = pending))
  ## An artifact-backed dataset needs no source copy.
  expect_true(unchanged(
    manifest = pending,
    entries = list(list(id = "ds1", load_state = "artifact_ready"))
  ))
  expect_false(unchanged(entries = list(list(id = "ds2"))))
  expect_false(unchanged(jobs = list(list(kind = "spatial"))))
  moved <- context
  moved$last_ui$stage <- "review"
  expect_false(unchanged(context = moved))
  options <- context
  options$configuration$build_mode <- !isTRUE(configuration$build_mode)
  expect_false(unchanged(context = options))
  legacy <- manifest
  legacy$datasets[[1L]]$configuration$payload <- "{}"
  expect_false(unchanged(manifest = legacy))
  migrated <- manifest
  migrated$migrated_from_schema <- 2L
  expect_false(unchanged(manifest = migrated))
  interrupted <- manifest
  interrupted$pending_build <- list(status = "interrupted")
  expect_false(unchanged(manifest = interrupted))
})

test_that("Save with nothing to write reports Saved immediately", {
  skip_if_not_installed("shiny")
  app_env <- builder_responsiveness_app()

  shiny::testServer(app_env$server, {
    server_env <- environment(builder_project_save_is_noop)
    calls <- new.env(parent = emptyenv())
    calls$saves <- 0L
    calls$reports <- 0L
    server_env$request_builder_project_save <- function(...) {
      calls$saves <- calls$saves + 1L
      invisible(TRUE)
    }
    server_env$report_builder_project_unchanged_save <- function() {
      calls$reports <- calls$reports + 1L
      invisible(TRUE)
    }
    server_env$builder_operation_allowed <- function(...) TRUE
    context <- builder_project_manifest_context()
    builder_project(list(
      root = withr::local_tempdir(),
      path = "builder-project.json",
      name = "P",
      manifest = list(
        schema_version = 3L,
        project = list(id = "p1", name = "P", revision = 2L),
        datasets = list(),
        configuration = context$configuration,
        last_ui = context$last_ui
      )
    ))
    session$flushReact()
    expect_true(builder_project_save_is_noop())

    session$setInputs(save_builder_project = 1)
    expect_identical(calls$reports, 1L)
    expect_identical(calls$saves, 0L)

    ## Pending source work keeps the ordinary Save.
    builder_project_source_queue(list(ds1 = list(id = "ds1")))
    expect_false(builder_project_save_is_noop())
    session$setInputs(save_builder_project = 2)
    expect_identical(calls$saves, 1L)
    builder_project_source_queue(list())

    builder_project_operation_phase("save_failed")
    expect_false(builder_project_save_is_noop())
  })
})
