builder_task9_source <- function(local = parent.frame()) {
  root <- testthat::test_path("..", "..", "inst", "builder")
  if (!dir.exists(root)) {
    root <- system.file("builder", package = "CerebroNexus")
  }
  source(file.path(root, "core", "bundle_path_contract.R"), local = local)
  source(file.path(root, "publish.R"), local = local)
  source(file.path(root, "coordinator.R"), local = local)
  invisible(root)
}

test_that("the coordinator preregisters an owner-only assigned stage", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")

    handle <- builder_coordinator_prepare(
      list(out_dir = target, expected_prior_identity = NULL),
      build_id = "build-1"
    )

    expect_s3_class(handle, "builder_release_coordinator")
    expect_true(dir.exists(handle$stage))
    expect_true(.pathWithin(handle$stage, handle$control))
    expect_identical(
      as.octmode(file.info(handle$control)$mode),
      as.octmode("700")
    )
    journal <- readRDS(handle$journal)
    expect_identical(journal$phase, "prepared")
    expect_identical(journal$build_id, "build-1")
    expect_identical(journal$stage, handle$stage)
    expect_true(dir.exists(handle$lock))

    expect_true(builder_coordinator_abort(handle)$aborted)
    expect_false(dir.exists(handle$stage))
    expect_false(dir.exists(handle$lock))
  })
})

test_that("only one process can coordinate one release", {
  skip_if_not_installed("callr")
  local({
    builder_root <- builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    barrier <- file.path(root, "go")
    runner <- function(id, builder_root, target, barrier) {
      source(file.path(builder_root, "core", "bundle_path_contract.R"))
      source(file.path(builder_root, "publish.R"))
      source(file.path(builder_root, "coordinator.R"))
      while (!file.exists(barrier)) {
        Sys.sleep(0.01)
      }
      tryCatch(
        {
          handle <- builder_coordinator_prepare(
            list(out_dir = target, expected_prior_identity = NULL),
            build_id = id
          )
          writeLines(id, file.path(handle$stage, "dataset.crb"))
          Sys.sleep(0.25)
          result <- builder_coordinator_publish(
            handle,
            list(
              state = "success",
              publishable = TRUE,
              stage = handle$stage,
              built = file.path(handle$stage, "dataset.crb"),
              labels = id
            )
          )
          list(published = isTRUE(result$published), error = result$error)
        },
        error = function(error) {
          list(published = FALSE, error = conditionMessage(error))
        }
      )
    }
    first <- callr::r_bg(runner, list("one", builder_root, target, barrier))
    second <- callr::r_bg(runner, list("two", builder_root, target, barrier))
    writeLines("go", barrier)
    first$wait(timeout = 10000)
    second$wait(timeout = 10000)
    results <- list(first$get_result(), second$get_result())

    expect_identical(
      sum(vapply(results, `[[`, logical(1), "published")),
      1L
    )
    expect_true(file.exists(file.path(target, "dataset.crb")))
  })
})

test_that("coordinator publishes only its verified assigned stage", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    handle <- builder_coordinator_prepare(
      list(out_dir = file.path(root, "release")),
      "build-verified"
    )
    writeLines("new", file.path(handle$stage, "dataset.crb"))

    expect_error(
      builder_coordinator_publish(
        handle,
        list(state = "failure", publishable = FALSE, stage = handle$stage)
      ),
      "verified"
    )
    expect_error(
      builder_coordinator_publish(
        handle,
        list(
          state = "success",
          publishable = TRUE,
          stage = root,
          built = file.path(root, "foreign.crb")
        )
      ),
      "assigned stage"
    )
    expect_false(dir.exists(handle$target))
    builder_coordinator_abort(handle)
  })
})

test_that("publication stays in the parent and uses the registered stage", {
  root <- testthat::test_path("..", "..", "inst", "builder")
  if (!dir.exists(root)) {
    root <- system.file("builder", package = "CerebroNexus")
  }
  app <- readLines(file.path(root, "app.R"), warn = FALSE)
  worker <- readLines(file.path(root, "worker.R"), warn = FALSE)
  session <- readLines(file.path(root, "session.R"), warn = FALSE)

  core_source <- grep("bundle_path_contract.R", app, fixed = TRUE)
  publish_source <- grep('source("publish.R", local = TRUE)', app, fixed = TRUE)
  coordinator_source <- grep(
    'source("coordinator.R", local = TRUE)',
    app,
    fixed = TRUE
  )
  expect_length(core_source, 1L)
  expect_length(publish_source, 1L)
  expect_length(coordinator_source, 1L)
  expect_lt(core_source, publish_source)
  expect_lt(publish_source, coordinator_source)
  expect_false(any(grepl(
    'source(file.path(dir, "publish.R"))',
    worker,
    fixed = TRUE
  )))
  expect_false(any(grepl("builder_publish_release", worker, fixed = TRUE)))
  expect_true(any(grepl("builder_coordinator_prepare", app, fixed = TRUE)))
  expect_true(any(grepl("builder_coordinator_publish", app, fixed = TRUE)))
  expect_true(any(grepl("coordinator$stage", session, fixed = TRUE)))
})

test_that("a parent-published result is a successful build transition", {
  local({
    builder_repo_source("state.R")
    action <- builder_build_action(
      list(
        state = "success",
        publishable = FALSE,
        published = TRUE,
        built = "/release/dataset.crb"
      ),
      "build-parent"
    )
    expect_identical(action$type, "succeed")
    expect_true(action$result$published)
  })
})

test_that("whole-release publication preserves foreign output occupants", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    foreign <- file.path(target, "notes.txt")
    writeLines("keep me", foreign)
    plan <- list(
      out_dir = target,
      overwrite = TRUE,
      targets = file.path(target, "dataset.crb"),
      output_release = list(targets = file.path(target, "dataset.crb"))
    )

    expect_error(
      builder_coordinator_prepare(plan, "build-foreign-release"),
      "foreign release"
    )
    expect_identical(readLines(foreign), "keep me")
    expect_false(dir.exists(builder_release_control_path(target)))
  })
})

test_that("known prior outputs still require explicit replacement", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old", file.path(target, "dataset.crb"))
    plan <- list(
      out_dir = target,
      overwrite = FALSE,
      targets = file.path(target, "dataset.crb"),
      output_release = list(targets = file.path(target, "dataset.crb"))
    )

    expect_error(
      builder_coordinator_prepare(plan, "build-no-replace"),
      "Replace existing outputs"
    )
    expect_identical(readLines(file.path(target, "dataset.crb")), "old")
  })
})

test_that("unplanned staged artifacts cannot enter the release", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    planned <- file.path(target, "dataset.crb")
    handle <- builder_coordinator_prepare(
      list(
        out_dir = target,
        overwrite = FALSE,
        targets = planned,
        output_release = list(targets = planned)
      ),
      "build-unplanned"
    )
    artifact <- file.path(handle$stage, "dataset.crb")
    writeLines("planned", artifact)
    writeLines("unexpected", file.path(handle$stage, "surprise.txt"))

    expect_error(
      builder_coordinator_publish(
        handle,
        list(
          state = "success",
          publishable = TRUE,
          stage = handle$stage,
          built = artifact,
          labels = "Dataset"
        )
      ),
      "unplanned"
    )
    expect_false(dir.exists(target))
    expect_true(builder_coordinator_abort(handle)$aborted)
  })
})
