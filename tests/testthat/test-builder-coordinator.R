builder_task9_source <- function(local = parent.frame()) {
  root <- testthat::test_path("..", "..", "inst", "builder")
  if (!dir.exists(root)) {
    root <- system.file("builder", package = "CerebroNexus")
  }
  source(file.path(root, "core", "bundle_path_contract.R"), local = local)
  source(file.path(root, "publish.R"), local = local)
  source(file.path(root, "app_bundle.R"), local = local)
  source(file.path(root, "coordinator.R"), local = local)
  invisible(root)
}

builder_app_coordinator_plan_fixture <- function(
  target,
  make_app = TRUE,
  backend = "embedded"
) {
  filenames <- c("dataset-a.crb", "dataset-b.crb")
  labels <- c("Dataset A", "Dataset B")
  items <- list(
    list(
      id = "dataset-a",
      name = labels[[1L]],
      filename = filenames[[1L]],
      colors = list(cluster = c(A = "#000000")),
      expression_backend = "embedded",
      sidecars = character()
    ),
    list(
      id = "dataset-b",
      name = labels[[2L]],
      filename = filenames[[2L]],
      colors = list(cluster = c(B = "#ffffff")),
      expression_backend = "embedded",
      sidecars = character()
    )
  )
  if (!identical(backend, "embedded")) {
    items[[1L]]$expression_backend <- backend
    items[[1L]]$sidecars <- paste0(
      tools::file_path_sans_ext(filenames[[1L]]),
      if (identical(backend, "h5")) ".h5" else ".bpcells"
    )
  }
  targets <- file.path(target, filenames)
  if (length(items[[1L]]$sidecars)) {
    targets <- c(targets, file.path(target, items[[1L]]$sidecars))
  }
  if (isTRUE(make_app)) {
    targets <- c(targets, file.path(target, "cerebro_app"))
  }
  structure(
    list(
      out_dir = target,
      overwrite = FALSE,
      targets = targets,
      output_release = list(targets = targets),
      make_app = isTRUE(make_app),
      app_contract_version = if (isTRUE(make_app)) 1L else 0L,
      dataset_order = c("dataset-a", "dataset-b"),
      items = items,
      app_options = list(
        enabled = isTRUE(make_app),
        show_upload_ui = FALSE,
        initial_dataset = "dataset-b",
        initial_dataset_mode = "explicit"
      )
    ),
    class = c("builder_build_plan", "list")
  )
}

test_that("coordinator contract inspection never dispatches plan methods", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    plan <- builder_app_coordinator_plan_fixture(file.path(root, "release"))
    calls <- 0L
    `$.builder_build_plan` <- function(value, ...) {
      calls <<- calls + 1L
      stop("executed hostile method")
    }

    contract <- .builder_coordinator_app_contract(plan)

    expect_true(contract$expectation$expected)
    expect_identical(calls, 0L)
  })
})

builder_app_coordinator_fake_app <- function(request, app_dir) {
  dir.create(app_dir)
  file.copy(
    builder_profile_inst_path("shiny"),
    app_dir,
    recursive = TRUE
  )
  file.copy(
    builder_profile_inst_path("extdata"),
    app_dir,
    recursive = TRUE
  )
  dir.create(file.path(app_dir, "private-data"))
  relative_crbs <- file.path(
    "private-data",
    basename(request$cerebro_data)
  )
  for (index in seq_along(relative_crbs)) {
    file.copy(
      request$cerebro_data[[index]],
      file.path(app_dir, relative_crbs[[index]])
    )
  }
  for (index in seq_along(request$backend_plan$entries)) {
    entry <- request$backend_plan$entries[[index]]
    if (!identical(entry$mode, "bundled")) {
      next
    }
    source <- file.path(request$stage, entry$location)
    target <- file.path(app_dir, "private-data", entry$location)
    if (dir.exists(source)) {
      dir.create(target)
      children <- list.files(
        source,
        all.files = TRUE,
        no.. = TRUE,
        full.names = TRUE
      )
      if (length(children)) {
        file.copy(children, target, recursive = TRUE)
      }
    } else {
      file.copy(source, target)
    }
  }
  file.copy(
    builder_profile_inst_path("shiny", "v1.4", "_bundle_app.R"),
    file.path(app_dir, "app.R")
  )
  saveRDS(
    list(
      crb_file_to_load = stats::setNames(
        relative_crbs,
        request$selector_order
      ),
      initial_dataset = request$initial_dataset,
      show_upload_ui = request$show_upload_ui,
      colors = request$colors,
      crb_pick_smallest_file = request$crb_pick_smallest_file,
      .bundle_backend_plan = request$backend_plan
    ),
    file.path(app_dir, "cerebro_config.rds")
  )
  app_dir
}

builder_app_coordinator_fixture <- function(
  make_app = TRUE,
  backend = "embedded",
  .local_envir = parent.frame(),
  coordinator_prepare,
  bundle_request,
  verify_app
) {
  root <- withr::local_tempdir(.local_envir = .local_envir)
  target <- file.path(root, "release")
  plan <- builder_app_coordinator_plan_fixture(target, make_app, backend)
  handle <- coordinator_prepare(plan, "build-app")
  built <- file.path(
    handle$stage,
    vapply(plan$items, `[[`, character(1), "filename")
  )
  labels <- vapply(plan$items, `[[`, character(1), "name")
  names(built) <- labels
  lapply(seq_along(built), function(index) {
    saveRDS(list(dataset = index), built[[index]])
  })
  if (identical(backend, "h5")) {
    writeBin(as.raw(1:32), file.path(handle$stage, plan$items[[1L]]$sidecars))
  } else if (identical(backend, "bpcells")) {
    dir.create(file.path(handle$stage, plan$items[[1L]]$sidecars))
  }
  result <- list(
    state = "success",
    publishable = TRUE,
    stage = handle$stage,
    build_id = "build-app",
    built = built,
    labels = labels,
    verifications = lapply(built, function(path) {
      list(valid = TRUE, path = unname(path))
    }),
    app_dir = NULL,
    app_verification = NULL
  )
  if (isTRUE(make_app)) {
    request <- bundle_request(plan, built, labels)
    result$app_dir <- builder_app_coordinator_fake_app(
      request,
      file.path(handle$stage, "cerebro_app")
    )
    result$app_verification <- verify_app(result$app_dir, request)
  }
  list(
    root = root,
    target = target,
    plan = plan,
    handle = handle,
    result = result
  )
}

write_builder_coordinator_record_fixture <- function(root, members) {
  lines <- c(
    "CEREBRO_BUILDER_RELEASE_V1",
    vapply(
      members,
      function(member) {
        paste(member$type, member$path, sep = "\t")
      },
      ""
    )
  )
  writeLines(
    lines,
    file.path(root, ".cerebro-builder-release-v1"),
    useBytes = TRUE
  )
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

test_that("coordinator rejects a dangling release-root link before prepare", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    outside <- file.path(root, "outside", "release")
    dir.create(dirname(outside))
    linked <- tryCatch(
      file.symlink(outside, target),
      error = function(error) FALSE
    )
    skip_if_not(isTRUE(linked), "Symbolic links are unavailable")

    attempt <- tryCatch(
      builder_coordinator_prepare(
        list(
          out_dir = target,
          overwrite = TRUE,
          targets = file.path(target, "dataset.crb")
        ),
        "build-dangling-root"
      ),
      error = function(error) error
    )
    if (inherits(attempt, "builder_release_coordinator")) {
      artifact <- file.path(attempt$stage, "dataset.crb")
      writeLines("new", artifact)
      builder_coordinator_publish(
        attempt,
        list(
          state = "success",
          publishable = TRUE,
          stage = attempt$stage,
          built = artifact
        )
      )
    }

    expect_s3_class(attempt, "error")
    if (inherits(attempt, "error")) {
      expect_match(conditionMessage(attempt), "symbolic link")
    }
    expect_false(dir.exists(outside))
    expect_identical(Sys.readlink(target), outside)
  })
})

test_that("coordinator rejects an existing release-root link", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    outside <- file.path(root, "outside")
    target <- file.path(root, "release")
    dir.create(outside)
    linked <- tryCatch(
      file.symlink(outside, target),
      error = function(error) FALSE
    )
    skip_if_not(isTRUE(linked), "Symbolic links are unavailable")

    expect_error(
      builder_coordinator_prepare(
        list(
          out_dir = target,
          overwrite = TRUE,
          targets = file.path(target, "dataset.crb")
        ),
        "build-existing-root-link"
      ),
      "symbolic link"
    )
    expect_true(dir.exists(outside))
    expect_identical(Sys.readlink(target), outside)
  })
})

test_that("coordinator rejects an unreadable prior payload", {
  skip_if(identical(.Platform$OS.type, "windows"))
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    payload <- file.path(target, "dataset.crb")
    dir.create(target)
    writeLines("prior", payload)
    Sys.chmod(payload, mode = "0000")
    withr::defer(Sys.chmod(payload, mode = "0600"))
    skip_if(file.access(payload, mode = 4L) == 0L)

    attempt <- tryCatch(
      builder_coordinator_prepare(
        list(
          out_dir = target,
          overwrite = TRUE,
          targets = payload,
          output_release = list(targets = payload)
        ),
        "build-unreadable-prior"
      ),
      error = function(error) error
    )
    if (inherits(attempt, "builder_release_coordinator")) {
      artifact <- file.path(attempt$stage, "dataset.crb")
      writeLines("new", artifact)
      builder_coordinator_publish(
        attempt,
        list(
          state = "success",
          publishable = TRUE,
          stage = attempt$stage,
          built = artifact
        )
      )
    }

    expect_s3_class(attempt, "error")
    if (inherits(attempt, "error")) {
      expect_match(conditionMessage(attempt), "read")
    }
    expect_true(dir.exists(target))
    expect_true(file.exists(payload))
    Sys.chmod(payload, mode = "0600")
    expect_identical(readLines(payload), "prior")
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
      source(file.path(builder_root, "app_bundle.R"))
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
  expect_false(any(grepl(
    ".cerebro-builder-release-v1",
    worker,
    fixed = TRUE
  )))
  expect_true(any(grepl("builder_coordinator_prepare", app, fixed = TRUE)))
  expect_true(any(grepl("builder_coordinator_publish", app, fixed = TRUE)))
  expect_true(any(grepl("coordinator$stage", session, fixed = TRUE)))
})

test_that("parent ownership permits safe release shrinkage", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(file.path(target, "cerebro_app"), recursive = TRUE)
    writeLines("old-a", file.path(target, "01-a.crb"))
    writeLines("old-b", file.path(target, "02-b.crb"))
    writeLines("old-app", file.path(target, "cerebro_app", "app.R"))
    write_builder_coordinator_record_fixture(
      target,
      list(
        list(type = "D", path = "cerebro_app"),
        list(type = "F", path = "01-a.crb"),
        list(type = "F", path = "02-b.crb"),
        list(type = "F", path = "cerebro_app/app.R")
      )
    )
    planned <- file.path(target, "01-a.crb")
    handle <- builder_coordinator_prepare(
      list(
        out_dir = target,
        overwrite = TRUE,
        targets = planned,
        output_release = list(targets = planned)
      ),
      "build-shrink"
    )
    artifact <- file.path(handle$stage, "01-a.crb")
    writeLines("new-a", artifact)

    result <- builder_coordinator_publish(
      handle,
      list(
        state = "success",
        publishable = TRUE,
        stage = handle$stage,
        built = artifact,
        labels = "A"
      )
    )

    expect_true(result$published)
    expect_identical(readLines(file.path(target, "01-a.crb")), "new-a")
    expect_false(file.exists(file.path(target, "02-b.crb")))
    expect_false(dir.exists(file.path(target, "cerebro_app")))
    expect_true(file.exists(file.path(
      target,
      ".cerebro-builder-release-v1"
    )))
  })
})

test_that("unrecorded nested release members remain foreign and untouched", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(file.path(target, "cerebro_app", "private"), recursive = TRUE)
    writeLines("app", file.path(target, "cerebro_app", "app.R"))
    foreign <- file.path(target, "cerebro_app", "private", "notes.txt")
    writeLines("keep me", foreign)
    write_builder_coordinator_record_fixture(
      target,
      list(
        list(type = "D", path = "cerebro_app"),
        list(type = "D", path = "cerebro_app/private"),
        list(type = "F", path = "cerebro_app/app.R")
      )
    )
    planned <- file.path(target, "dataset.crb")

    expect_error(
      builder_coordinator_prepare(
        list(
          out_dir = target,
          overwrite = TRUE,
          targets = planned,
          output_release = list(targets = planned)
        ),
        "build-nested-foreign"
      ),
      "foreign release entr"
    )
    expect_identical(readLines(foreign), "keep me")
    expect_identical(
      readLines(file.path(target, "cerebro_app", "app.R")),
      "app"
    )
  })
})

test_that("one prior snapshot prevents record ABA from claiming foreign files", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old-a", file.path(target, "a.crb"))
    foreign <- file.path(target, "notes.txt")
    writeLines("keep me", foreign)
    narrow <- list(list(type = "F", path = "a.crb"))
    broad <- list(
      list(type = "F", path = "a.crb"),
      list(type = "F", path = "notes.txt")
    )
    write_builder_coordinator_record_fixture(target, narrow)
    expected_prior <- builder_release_identity(target)
    original_read <- .builder_release_read_record
    reads <- 0L
    .builder_release_read_record <- function(...) {
      record <- original_read(...)
      reads <<- reads + 1L
      if (identical(reads, 1L)) {
        write_builder_coordinator_record_fixture(target, broad)
      } else if (identical(reads, 2L)) {
        write_builder_coordinator_record_fixture(target, narrow)
      }
      record
    }
    planned <- file.path(target, "a.crb")
    result <- tryCatch(
      {
        handle <- builder_coordinator_prepare(
          list(
            out_dir = target,
            overwrite = TRUE,
            targets = planned,
            output_release = list(targets = planned),
            expected_prior_identity = expected_prior
          ),
          "build-record-aba"
        )
        artifact <- file.path(handle$stage, "a.crb")
        writeLines("new-a", artifact)
        builder_coordinator_publish(
          handle,
          list(
            state = "success",
            publishable = TRUE,
            stage = handle$stage,
            built = artifact,
            labels = "A"
          )
        )
      },
      error = function(error) error
    )

    expect_s3_class(result, "error")
    expect_true(file.exists(foreign))
    expect_identical(readLines(foreign), "keep me")
  })
})

test_that("legacy releases cannot silently shrink", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old-a", file.path(target, "01-a.crb"))
    writeLines("old-b", file.path(target, "02-b.crb"))

    expect_error(
      builder_coordinator_prepare(
        list(
          out_dir = target,
          overwrite = TRUE,
          targets = file.path(target, "01-a.crb"),
          output_release = list(targets = file.path(target, "01-a.crb"))
        ),
        "build-legacy-shrink"
      ),
      "foreign release entr"
    )
    expect_identical(readLines(file.path(target, "02-b.crb")), "old-b")

    expected <- file.path(target, c("01-a.crb", "02-b.crb"))
    handle <- builder_coordinator_prepare(
      list(
        out_dir = target,
        overwrite = TRUE,
        targets = expected,
        output_release = list(targets = expected)
      ),
      "build-legacy-same-topology"
    )
    staged <- file.path(handle$stage, c("01-a.crb", "02-b.crb"))
    writeLines("new-a", staged[[1L]])
    writeLines("new-b", staged[[2L]])
    result <- builder_coordinator_publish(
      handle,
      list(
        state = "success",
        publishable = TRUE,
        stage = handle$stage,
        built = staged,
        labels = c("A", "B")
      )
    )
    expect_true(result$published)
    expect_true(file.exists(file.path(
      target,
      ".cerebro-builder-release-v1"
    )))
  })
})

test_that("legacy nested topology cannot silently shrink", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    app <- file.path(target, "nested-output")
    dir.create(app, recursive = TRUE)
    writeLines("old-a", file.path(app, "a.txt"))
    writeLines("old-b", file.path(app, "b.txt"))
    planned <- file.path(target, "nested-output")
    handle <- builder_coordinator_prepare(
      list(
        out_dir = target,
        overwrite = TRUE,
        targets = planned,
        output_release = list(targets = planned)
      ),
      "build-legacy-nested-shrink"
    )
    staged_app <- file.path(handle$stage, "nested-output")
    dir.create(staged_app)
    writeLines("new-a", file.path(staged_app, "a.txt"))

    expect_error(
      builder_coordinator_publish(
        handle,
        list(
          state = "success",
          publishable = TRUE,
          stage = handle$stage,
          built = staged_app,
          labels = "App"
        )
      ),
      "legacy release topology"
    )
    expect_identical(readLines(file.path(app, "a.txt")), "old-a")
    expect_identical(readLines(file.path(app, "b.txt")), "old-b")
    expect_true(builder_coordinator_abort(handle)$aborted)
  })
})

test_that("record write failure leaves the verified stage unpublished", {
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
      "build-record-failure"
    )
    artifact <- file.path(handle$stage, "dataset.crb")
    writeLines("new", artifact)

    expect_error(
      builder_coordinator_publish(
        handle,
        list(
          state = "success",
          publishable = TRUE,
          stage = handle$stage,
          built = artifact,
          labels = "Dataset"
        ),
        .record_move = function(from, to) FALSE
      ),
      "atomically"
    )
    expect_false(dir.exists(target))
    expect_identical(readLines(artifact), "new")
    expect_false(file.exists(file.path(
      handle$stage,
      ".cerebro-builder-release-v1"
    )))
    expect_true(builder_coordinator_abort(handle)$aborted)
  })
})

test_that("payload changes during record commit cannot be published", {
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
      "build-record-race"
    )
    artifact <- file.path(handle$stage, "dataset.crb")
    writeLines("verified", artifact)

    result <- tryCatch(
      builder_coordinator_publish(
        handle,
        list(
          state = "success",
          publishable = TRUE,
          stage = handle$stage,
          built = artifact,
          labels = "Dataset"
        ),
        .record_move = function(from, to) {
          writeLines("tampered", artifact)
          file.rename(from, to)
        }
      ),
      error = function(error) error
    )

    expect_s3_class(result, "error")
    expect_match(conditionMessage(result), "changed during ownership")
    expect_false(dir.exists(target))
    expect_identical(readLines(artifact), "tampered")
    expect_true(builder_coordinator_abort(handle)$aborted)
  })
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

test_that("coordinator freezes the complete App publication expectation", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    plan <- builder_app_coordinator_plan_fixture(target)
    handle <- builder_coordinator_prepare(plan, "build-frozen-app")

    expect_true(handle$app_expectation$expected)
    expect_identical(
      names(handle$app_expectation),
      c(
        "expected",
        "contract_version",
        "dataset_ids",
        "labels",
        "filenames",
        "initial_dataset",
        "initial_dataset_mode",
        "show_upload_ui",
        "colors",
        "backend_plan",
        "app_dir"
      )
    )
    frozen <- unserialize(serialize(handle$app_expectation, NULL))
    plan$items[[1L]]$name <- "Mutated"
    plan$items[[1L]]$colors$cluster[["A"]] <- "#ff0000"
    plan$app_options$initial_dataset <- "dataset-a"
    expect_identical(handle$app_expectation, frozen)
    expect_identical(
      handle$app_expectation$app_dir,
      file.path(handle$stage, "cerebro_app")
    )
    expect_true(builder_coordinator_abort(handle)$aborted)
  })
})

test_that("App publication requires exact parent-bound evidence", {
  local({
    builder_task9_source()

    missing <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    missing$result$app_verification <- NULL
    expect_error(
      builder_coordinator_publish(missing$handle, missing$result),
      "App verification"
    )
    expect_true(builder_coordinator_abort(missing$handle)$aborted)

    forged <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    forged$result$app_verification <- unclass(forged$result$app_verification)
    expect_error(
      builder_coordinator_publish(forged$handle, forged$result),
      "App verification"
    )
    expect_true(builder_coordinator_abort(forged$handle)$aborted)

    forged_field <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    forged_field$result$app_verification$private_files <- character()
    expect_error(
      builder_coordinator_publish(
        forged_field$handle,
        forged_field$result
      ),
      "App verification"
    )
    expect_true(builder_coordinator_abort(forged_field$handle)$aborted)

    wrong_build <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    wrong_build$result$build_id <- "another-build"
    expect_error(
      builder_coordinator_publish(wrong_build$handle, wrong_build$result),
      "build identity"
    )
    expect_true(builder_coordinator_abort(wrong_build$handle)$aborted)

    escaped <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    escaped$result$app_dir <- escaped$root
    expect_error(
      builder_coordinator_publish(escaped$handle, escaped$result),
      "assigned App directory"
    )
    expect_true(builder_coordinator_abort(escaped$handle)$aborted)

    missing_dir <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    unlink(missing_dir$result$app_dir, recursive = TRUE)
    expect_error(
      builder_coordinator_publish(missing_dir$handle, missing_dir$result),
      "assigned App directory"
    )
    expect_true(builder_coordinator_abort(missing_dir$handle)$aborted)
  })
})

test_that("CRB-only publication rejects an unexpected staged App", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      make_app = FALSE,
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    fixture$result$app_dir <- file.path(fixture$handle$stage, "cerebro_app")
    dir.create(fixture$result$app_dir)
    fixture$result$app_verification <- structure(
      list(valid = TRUE),
      class = c("builder_app_verification", "list")
    )

    expect_error(
      builder_coordinator_publish(fixture$handle, fixture$result),
      "unexpected App"
    )
    expect_true(builder_coordinator_abort(fixture$handle)$aborted)
  })
})

test_that("parent rejects an App changed after worker verification", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    staged_crb <- unname(fixture$result$built[[1L]])
    app_crb <- file.path(
      fixture$result$app_dir,
      "private-data",
      basename(staged_crb)
    )
    saveRDS(list(dataset = "changed"), staged_crb)
    file.copy(staged_crb, app_crb, overwrite = TRUE)

    expect_error(
      builder_coordinator_publish(fixture$handle, fixture$result),
      "changed after worker verification"
    )
    expect_false(dir.exists(fixture$target))
    expect_true(builder_coordinator_abort(fixture$handle)$aborted)
  })
})

test_that("parent binds its App verification to final payload identity", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    original_identity <- .builder_coordinator_stage_identity
    injected <- FALSE
    .builder_coordinator_stage_identity <- function(...) {
      if (!injected) {
        injected <<- TRUE
        saveRDS(
          list(dataset = "post-parent-change"),
          file.path(
            fixture$result$app_dir,
            "private-data",
            basename(fixture$result$built[[1L]])
          )
        )
      }
      original_identity(...)
    }

    expect_error(
      builder_coordinator_publish(fixture$handle, fixture$result),
      "changed after parent verification"
    )
    expect_false(dir.exists(fixture$target))
    expect_true(builder_coordinator_abort(fixture$handle)$aborted)
  })
})

test_that("parent binds staged CRB closure to final payload identity", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    original_identity <- .builder_coordinator_stage_identity
    injected <- FALSE
    .builder_coordinator_stage_identity <- function(...) {
      if (!injected) {
        injected <<- TRUE
        saveRDS(
          list(dataset = "post-parent-source-change"),
          unname(fixture$result$built[[1L]])
        )
      }
      original_identity(...)
    }

    expect_error(
      builder_coordinator_publish(fixture$handle, fixture$result),
      "input closure changed after parent verification"
    )
    expect_false(dir.exists(fixture$target))
    expect_true(builder_coordinator_abort(fixture$handle)$aborted)
  })
})

test_that("same-byte staged CRB hard-link swaps stay unpublished", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    staged_crb <- unname(fixture$result$built[[1L]])
    outside_copy <- file.path(fixture$root, "same-byte-source.crb")
    copied <- file.copy(staged_crb, outside_copy)
    skip_if_not(isTRUE(copied), "A same-byte fixture could not be created")
    original_identity <- .builder_coordinator_stage_identity
    injected <- FALSE
    .builder_coordinator_stage_identity <- function(...) {
      if (!injected) {
        injected <<- TRUE
        unlink(staged_crb)
        stopifnot(file.link(outside_copy, staged_crb))
      }
      original_identity(...)
    }

    expect_error(
      builder_coordinator_publish(fixture$handle, fixture$result),
      "input closure changed after parent verification"
    )
    expect_false(dir.exists(fixture$target))
    expect_true(builder_coordinator_abort(fixture$handle)$aborted)
  })
})

test_that("empty BPCells root type remains bound after parent verification", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      backend = "bpcells",
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    sidecar <- file.path(fixture$handle$stage, "dataset-a.bpcells")
    original_identity <- .builder_coordinator_stage_identity
    injected <- FALSE
    .builder_coordinator_stage_identity <- function(...) {
      if (!injected) {
        injected <<- TRUE
        unlink(sidecar, recursive = TRUE)
        writeBin(raw(), sidecar)
      }
      original_identity(...)
    }

    expect_error(
      builder_coordinator_publish(fixture$handle, fixture$result),
      "input closure changed after parent verification"
    )
    expect_false(dir.exists(fixture$target))
    expect_true(builder_coordinator_abort(fixture$handle)$aborted)
  })
})

test_that("same-byte App hard-link swaps fail after parent verification", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    app_crb <- file.path(
      fixture$result$app_dir,
      "private-data",
      basename(fixture$result$built[[1L]])
    )
    outside_link <- file.path(fixture$root, "same-bytes.crb")
    copied <- file.copy(app_crb, outside_link)
    skip_if_not(isTRUE(copied), "A same-byte fixture could not be created")
    original_identity <- .builder_coordinator_stage_identity
    injected <- FALSE
    .builder_coordinator_stage_identity <- function(...) {
      if (!injected) {
        injected <<- TRUE
        unlink(app_crb)
        stopifnot(file.link(outside_link, app_crb))
      }
      original_identity(...)
    }

    expect_error(
      builder_coordinator_publish(fixture$handle, fixture$result),
      "changed after parent verification"
    )
    expect_false(dir.exists(fixture$target))
    expect_true(builder_coordinator_abort(fixture$handle)$aborted)
  })
})

test_that("App metadata races during ownership commit stay unpublished", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    app_crb <- file.path(
      fixture$result$app_dir,
      "private-data",
      basename(fixture$result$built[[1L]])
    )
    outside_link <- file.path(fixture$root, "ownership-race.crb")
    copied <- file.copy(app_crb, outside_link)
    skip_if_not(isTRUE(copied), "A same-byte fixture could not be created")

    expect_error(
      builder_coordinator_publish(
        fixture$handle,
        fixture$result,
        .record_move = function(from, to) {
          unlink(app_crb)
          stopifnot(file.link(outside_link, app_crb))
          file.rename(from, to)
        }
      ),
      "changed during ownership"
    )
    expect_false(dir.exists(fixture$target))
    expect_true(builder_coordinator_abort(fixture$handle)$aborted)
  })
})

test_that("input closure races during ownership commit stay unpublished", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    staged_crb <- unname(fixture$result$built[[1L]])
    outside_copy <- file.path(fixture$root, "ownership-source-race.crb")
    copied <- file.copy(staged_crb, outside_copy)
    skip_if_not(isTRUE(copied), "A same-byte fixture could not be created")

    expect_error(
      builder_coordinator_publish(
        fixture$handle,
        fixture$result,
        .record_move = function(from, to) {
          unlink(staged_crb)
          stopifnot(file.link(outside_copy, staged_crb))
          file.rename(from, to)
        }
      ),
      "input closure changed during ownership"
    )
    expect_false(dir.exists(fixture$target))
    expect_true(builder_coordinator_abort(fixture$handle)$aborted)
  })
})

test_that("verified Apps publish with final paths and parent ownership", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    stage <- fixture$handle$stage

    published <- builder_coordinator_publish(
      fixture$handle,
      fixture$result
    )

    expect_true(published$published)
    expect_identical(
      published$app_dir,
      file.path(published$release$target, "cerebro_app")
    )
    expect_identical(
      unname(published$built),
      file.path(published$release$target, basename(fixture$result$built))
    )
    expect_identical(
      unname(vapply(
        published$verifications,
        `[[`,
        character(1),
        "path"
      )),
      unname(published$built)
    )
    expect_null(published$app_verification)
    expect_null(published$stage)
    expect_false(any(grepl(
      stage,
      unlist(published, recursive = TRUE, use.names = FALSE),
      fixed = TRUE
    )))
    record <- .builder_release_read_record(
      published$release$target,
      exact = TRUE
    )
    member_paths <- vapply(record$members, `[[`, character(1), "path")
    expect_true(all(
      c(
        "cerebro_app",
        "cerebro_app/app.R",
        "cerebro_app/private-data/dataset-a.crb",
        "cerebro_app/private-data/dataset-b.crb"
      ) %in%
        member_paths
    ))
  })
})
