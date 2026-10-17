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
