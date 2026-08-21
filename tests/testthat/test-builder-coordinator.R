builder_task9_source <- function(local = parent.frame()) {
  root <- testthat::test_path("..", "..", "inst", "builder")
  if (!dir.exists(root)) {
    root <- system.file("builder", package = "CerebroNexus")
  }
  source(file.path(root, "core", "bundle_path_contract.R"), local = local)
  source(file.path(root, "publish.R"), local = local)
  source(file.path(root, "app_bundle.R"), local = local)
  source(file.path(root, "report.R"), local = local)
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
      default_projection = "umap",
      default_trajectory = NULL,
      overview_point_size = 5,
      expression_backend = "embedded",
      sidecars = character()
    ),
    list(
      id = "dataset-b",
      name = labels[[2L]],
      filename = filenames[[2L]],
      colors = list(cluster = c(B = "#ffffff")),
      default_projection = "pca",
      default_trajectory = list(method = "slingshot", name = "lineage"),
      overview_point_size = 7,
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
  targets <- if (isTRUE(make_app)) {
    file.path(target, c("cerebro_app", "viewer-auth.env"))
  } else {
    values <- c(filenames, items[[1L]]$sidecars %||% character())
    file.path(target, values)
  }
  structure(
    list(
      revision = 1L,
      readiness = "ready",
      out_dir = target,
      overwrite = FALSE,
      targets = targets,
      output_release = list(targets = targets),
      make_app = isTRUE(make_app),
      app_contract_version = if (isTRUE(make_app)) 1L else 0L,
      dataset_order = c("dataset-a", "dataset-b"),
      items = items,
      manifest = list(),
      acknowledgements = list(),
      viewer_bundle_assets = character(),
      private_assets = filenames,
      app_options = list(
        enabled = isTRUE(make_app),
        show_upload_ui = FALSE,
        initial_dataset = "dataset-b",
        initial_dataset_mode = "explicit",
        initial_page = "data_info",
        welcome_message = "Welcome, team!",
        point_size = list(overview_projection_point_size = 6),
        variable_to_compare = FALSE,
        host = "127.0.0.1",
        port = 4242L,
        max_request_size = 512,
        display_mode = "showcase",
        launch_browser = FALSE
      ),
      app_auth = list(
        enabled = isTRUE(make_app),
        account_count = if (isTRUE(make_app)) 2L else 0L,
        timeout_minutes = 15L
      )
    ),
    class = c("builder_build_plan", "list")
  )
}

builder_crb_coordinator_plan <- function(
  target,
  filenames,
  overwrite = FALSE,
  expected_prior_identity = NULL
) {
  items <- lapply(seq_along(filenames), function(index) {
    list(
      id = paste0("dataset-", index),
      name = paste0("Dataset ", index),
      filename = filenames[[index]],
      organism = "hg",
      analyses = character(),
      included_groups = character(),
      included_projections = character(),
      metadata_policy = list(included = character()),
      expression_backend = "embedded",
      sidecars = character(),
      viewer_page_expectations = list(visible_conditional = character())
    )
  })
  targets <- file.path(target, filenames)
  structure(
    list(
      revision = 1L,
      readiness = "ready",
      out_dir = target,
      overwrite = overwrite,
      targets = targets,
      output_release = list(targets = targets),
      expected_prior_identity = expected_prior_identity,
      make_app = FALSE,
      dataset_order = vapply(items, `[[`, character(1), "id"),
      items = items,
      manifest = list(),
      acknowledgements = list(),
      viewer_bundle_assets = character(),
      private_assets = filenames,
      app_options = list(enabled = FALSE),
      app_auth = list(
        enabled = FALSE,
        account_count = 0L,
        timeout_minutes = 15L
      )
    ),
    class = c("builder_build_plan", "list")
  )
}

builder_crb_coordinator_result <- function(handle, artifacts, labels = NULL) {
  if (is.null(labels)) {
    labels <- paste0("Dataset ", seq_along(artifacts))
  }
  list(
    state = "success",
    publishable = TRUE,
    stage = handle$stage,
    built = artifacts,
    labels = labels,
    verifications = lapply(artifacts, function(path) {
      list(valid = TRUE, path = path, metadata = character())
    }),
    app_dir = NULL,
    app_verification = NULL,
    auth_enabled = FALSE,
    auth_env_file = NULL
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

test_that("coordinator prepare never dispatches hostile plan methods", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    calls <- character()
    `$.builder_build_plan` <- function(value, name) {
      calls <<- c(calls, name)
      if (identical(name, "out_dir")) {
        return(target)
      }
      stop("hostile-plan-method-sentinel-5e82")
    }
    plan <- structure(
      list(
        out_dir = target,
        make_app = FALSE,
        app_auth = list(
          enabled = FALSE,
          account_count = 0L,
          timeout_minutes = 15L,
          accounts = "forged-auth-sentinel-5e82"
        )
      ),
      class = c("builder_build_plan", "list")
    )

    error <- tryCatch(
      builder_coordinator_prepare(plan, "hostile-plan"),
      error = identity
    )

    expect_s3_class(error, "error")
    expect_identical(
      conditionMessage(error),
      "The App publication expectation is invalid."
    )
    expect_identical(calls, character())
    expect_false(builder_auth_value_contains(
      error,
      "hostile-plan-method-sentinel-5e82"
    ))
    expect_false(builder_auth_value_contains(
      error,
      "forged-auth-sentinel-5e82"
    ))
  })
})

builder_app_coordinator_fake_app <- function(
  request,
  app_dir,
  auth_material = NULL
) {
  dir.create(app_dir)
  file.copy(
    builder_profile_inst_path("viewer"),
    app_dir,
    recursive = TRUE
  )
  file.copy(
    builder_profile_inst_path("extdata"),
    app_dir,
    recursive = TRUE
  )
  .removeBundleSystemMetadata(app_dir)
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
    builder_profile_inst_path("viewer", "_bundle_app.R"),
    file.path(app_dir, "app.R")
  )
  config <- list(
    crb_file_to_load = stats::setNames(
      relative_crbs,
      request$selector_order
    ),
    initial_dataset = request$initial_dataset,
    initial_page = request$initial_page,
    show_upload_ui = request$show_upload_ui,
    welcome_message = request$welcome_message,
    point_size = request$point_size,
    viewer_content = request$viewer_content,
    variable_to_compare = request$variable_to_compare,
    .bundle_run_options = list(
      schema_version = 1L,
      max_request_size_bytes = request$max_request_size * 1024^2,
      shiny_app_options = list(
        port = as.integer(request$port),
        host = request$host,
        launch.browser = request$launch_browser,
        quiet = TRUE,
        display.mode = request$display_mode
      )
    ),
    colors = request$colors,
    crb_pick_smallest_file = request$crb_pick_smallest_file,
    .bundle_backend_plan = request$backend_plan
  )
  if (isTRUE(request$auth$enabled)) {
    auth_dir <- file.path(app_dir, "private-data", "auth")
    dir.create(auth_dir)
    file.copy(
      auth_material$credentials,
      file.path(auth_dir, "credentials.sqlite")
    )
    config$.viewer_auth <- list(
      credentials_path = "private-data/auth/credentials.sqlite",
      passphrase_env = "CEREBRO_AUTH_PASSPHRASE",
      timeout_minutes = 15L
    )
  }
  saveRDS(config, file.path(app_dir, "cerebro_config.rds"))
  app_dir
}

builder_app_coordinator_fixture <- function(
  make_app = TRUE,
  backend = "embedded",
  root = NULL,
  target = NULL,
  plan = NULL,
  .local_envir = parent.frame(),
  coordinator_prepare,
  bundle_request,
  verify_app
) {
  if (is.null(root)) {
    root <- withr::local_tempdir(.local_envir = .local_envir)
  }
  if (is.null(target)) {
    target <- file.path(root, "release")
  }
  if (is.null(plan)) {
    plan <- builder_app_coordinator_plan_fixture(target, make_app, backend)
  }
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
    app_verification = NULL,
    auth_enabled = FALSE,
    auth_env_file = NULL
  )
  if (isTRUE(make_app)) {
    request <- bundle_request(plan, built, labels)
    auth_runtime <- environment(bundle_request)
    auth_material <- if (isTRUE(request$auth$enabled)) {
      get("builder_auth_create_material", envir = auth_runtime)(
        get("builder_auth_validate_payload", envir = auth_runtime)(
          TRUE,
          list(
            list(
              id = "auth-account-1",
              username = "fixture-a",
              password = "fixture-password-a"
            ),
            list(
              id = "auth-account-2",
              username = "fixture-b",
              password = "fixture-password-b"
            )
          )
        )$accounts,
        handle$stage,
        .capability = function() list(available = TRUE, reason = NULL)
      )
    } else {
      NULL
    }
    result$app_dir <- builder_app_coordinator_fake_app(
      request,
      file.path(handle$stage, "cerebro_app"),
      auth_material = auth_material
    )
    result$app_verification <- verify_app(
      result$app_dir,
      request,
      auth_env_file = if (is.null(auth_material)) {
        NULL
      } else {
        auth_material$env_file
      }
    )
    if (!is.null(auth_material)) {
      get("builder_auth_cleanup_material", envir = auth_runtime)(
        auth_material,
        handle$stage,
        keep_env = TRUE
      )
      result$auth_enabled <- TRUE
      result$auth_env_file <- auth_material$env_file
    } else {
      result$auth_enabled <- FALSE
      result$auth_env_file <- NULL
    }
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
      list(
        out_dir = target,
        expected_prior_identity = NULL,
        app_auth = list(
          enabled = FALSE,
          account_count = 0L,
          timeout_minutes = 15L
        )
      ),
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
          targets = file.path(target, "dataset.crb"),
          app_auth = list(
            enabled = FALSE,
            account_count = 0L,
            timeout_minutes = 15L
          )
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
          auth_enabled = FALSE,
          auth_env_file = NULL,
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
          targets = file.path(target, "dataset.crb"),
          app_auth = list(
            enabled = FALSE,
            account_count = 0L,
            timeout_minutes = 15L
          )
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
          output_release = list(targets = payload),
          app_auth = list(
            enabled = FALSE,
            account_count = 0L,
            timeout_minutes = 15L
          )
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
          auth_enabled = FALSE,
          auth_env_file = NULL,
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
      source(file.path(builder_root, "report.R"))
      source(file.path(builder_root, "coordinator.R"))
      while (!file.exists(barrier)) {
        Sys.sleep(0.01)
      }
      tryCatch(
        {
          item <- list(
            id = "dataset-1",
            name = id,
            filename = "dataset.crb",
            analyses = character(),
            included_groups = character(),
            included_projections = character(),
            metadata_policy = list(included = character()),
            expression_backend = "embedded",
            sidecars = character(),
            viewer_page_expectations = list(visible_conditional = character())
          )
          plan <- structure(
            list(
              revision = 1L,
              readiness = "ready",
              out_dir = target,
              overwrite = FALSE,
              targets = file.path(target, "dataset.crb"),
              output_release = list(targets = file.path(target, "dataset.crb")),
              expected_prior_identity = NULL,
              make_app = FALSE,
              dataset_order = "dataset-1",
              items = list(item),
              manifest = list(),
              acknowledgements = list(),
              viewer_bundle_assets = character(),
              private_assets = "dataset.crb",
              app_options = list(enabled = FALSE),
              app_auth = list(
                enabled = FALSE,
                account_count = 0L,
                timeout_minutes = 15L
              )
            ),
            class = c("builder_build_plan", "list")
          )
          handle <- builder_coordinator_prepare(plan, build_id = id)
          writeLines(id, file.path(handle$stage, "dataset.crb"))
          Sys.sleep(0.25)
          result <- builder_coordinator_publish(
            handle,
            list(
              state = "success",
              publishable = TRUE,
              auth_enabled = FALSE,
              auth_env_file = NULL,
              stage = handle$stage,
              built = file.path(handle$stage, "dataset.crb"),
              labels = id,
              verifications = list(list(
                valid = TRUE,
                path = file.path(handle$stage, "dataset.crb"),
                metadata = character()
              )),
              app_dir = NULL,
              app_verification = NULL
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
      1L,
      info = paste(
        vapply(results, function(value) value$error %||% "", ""),
        collapse = " | "
      )
    )
    expect_true(file.exists(file.path(target, "dataset.crb")))
  })
})

test_that("coordinator publishes only its verified assigned stage", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    handle <- builder_coordinator_prepare(
      list(
        out_dir = file.path(root, "release"),
        app_auth = list(
          enabled = FALSE,
          account_count = 0L,
          timeout_minutes = 15L
        )
      ),
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
          auth_enabled = FALSE,
          auth_env_file = NULL,
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
  app <- builder_app_source_lines()
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
    plan <- builder_crb_coordinator_plan(target, "01-a.crb", overwrite = TRUE)
    handle <- builder_coordinator_prepare(
      plan,
      "build-shrink"
    )
    artifact <- file.path(handle$stage, "01-a.crb")
    writeLines("new-a", artifact)

    result <- builder_coordinator_publish(
      handle,
      builder_crb_coordinator_result(handle, artifact, "Dataset 1")
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
          output_release = list(targets = planned),
          app_auth = list(
            enabled = FALSE,
            account_count = 0L,
            timeout_minutes = 15L
          )
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
            expected_prior_identity = expected_prior,
            app_auth = list(
              enabled = FALSE,
              account_count = 0L,
              timeout_minutes = 15L
            )
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
            auth_enabled = FALSE,
            auth_env_file = NULL,
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
          output_release = list(targets = file.path(target, "01-a.crb")),
          app_auth = list(
            enabled = FALSE,
            account_count = 0L,
            timeout_minutes = 15L
          )
        ),
        "build-legacy-shrink"
      ),
      "foreign release entr"
    )
    expect_identical(readLines(file.path(target, "02-b.crb")), "old-b")

    plan <- builder_crb_coordinator_plan(
      target,
      c("01-a.crb", "02-b.crb"),
      overwrite = TRUE
    )
    handle <- builder_coordinator_prepare(
      plan,
      "build-legacy-same-topology"
    )
    staged <- file.path(handle$stage, c("01-a.crb", "02-b.crb"))
    writeLines("new-a", staged[[1L]])
    writeLines("new-b", staged[[2L]])
    result <- builder_coordinator_publish(
      handle,
      builder_crb_coordinator_result(handle, staged)
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
        output_release = list(targets = planned),
        app_auth = list(
          enabled = FALSE,
          account_count = 0L,
          timeout_minutes = 15L
        )
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
          auth_enabled = FALSE,
          auth_env_file = NULL,
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
    plan <- builder_crb_coordinator_plan(target, "dataset.crb")
    handle <- builder_coordinator_prepare(
      plan,
      "build-record-failure"
    )
    artifact <- file.path(handle$stage, "dataset.crb")
    writeLines("new", artifact)

    expect_error(
      builder_coordinator_publish(
        handle,
        builder_crb_coordinator_result(handle, artifact),
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
    plan <- builder_crb_coordinator_plan(target, "dataset.crb")
    handle <- builder_coordinator_prepare(
      plan,
      "build-record-race"
    )
    artifact <- file.path(handle$stage, "dataset.crb")
    writeLines("verified", artifact)

    result <- tryCatch(
      builder_coordinator_publish(
        handle,
        builder_crb_coordinator_result(handle, artifact),
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
      output_release = list(targets = file.path(target, "dataset.crb")),
      app_auth = list(
        enabled = FALSE,
        account_count = 0L,
        timeout_minutes = 15L
      )
    )

    expect_error(
      builder_coordinator_prepare(plan, "build-foreign-release"),
      "foreign release"
    )
    expect_identical(readLines(foreign), "keep me")
    expect_false(dir.exists(builder_release_control_path(target)))
  })
})

test_that("output preflight reports foreign occupants before staging", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    foreign <- c("builder-project.json", "datasets", "sources")
    writeLines("project", file.path(target, foreign[[1L]]))
    dir.create(file.path(target, foreign[[2L]]))
    dir.create(file.path(target, foreign[[3L]]))
    plan <- builder_crb_coordinator_plan(target, "dataset.crb")

    preflight <- builder_coordinator_output_preflight(plan)

    expect_setequal(preflight$foreign, foreign)
    expect_false(dir.exists(builder_release_control_path(target)))
  })
})

test_that("output preflight ignores Finder metadata only", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("finder", file.path(target, ".DS_Store"))
    plan <- builder_crb_coordinator_plan(target, "dataset.crb")

    clean <- builder_coordinator_output_preflight(plan)
    expect_length(clean$foreign, 0L)

    writeLines("foreign", file.path(target, ".unknown"))
    foreign <- builder_coordinator_output_preflight(plan)
    expect_identical(foreign$foreign, ".unknown")
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
      output_release = list(targets = file.path(target, "dataset.crb")),
      app_auth = list(
        enabled = FALSE,
        account_count = 0L,
        timeout_minutes = 15L
      )
    )

    expect_error(
      builder_coordinator_prepare(plan, "build-no-replace"),
      "Replace existing outputs"
    )
    expect_identical(readLines(file.path(target, "dataset.crb")), "old")
  })
})

test_that("an abandoned Builder release shell can be reused without replacement", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines(
      c(
        "CEREBRO_BUILDER_RELEASE_V1",
        "D\tcerebro_app",
        "F\tcerebro_app/app.R",
        "F\tdataset.crb"
      ),
      file.path(target, ".cerebro-builder-release-v1")
    )
    writeLines("finder", file.path(target, ".DS_Store"))
    planned <- file.path(target, "dataset.crb")

    handle <- builder_coordinator_prepare(
      list(
        out_dir = target,
        overwrite = FALSE,
        targets = planned,
        output_release = list(targets = planned),
        app_auth = list(
          enabled = FALSE,
          account_count = 0L,
          timeout_minutes = 15L
        )
      ),
      "build-abandoned-shell"
    )

    expect_true(handle$expected_prior_state$record$abandoned)
    expect_true(file.exists(file.path(target, ".cerebro-builder-release-v1")))
    expect_true(builder_coordinator_abort(handle)$aborted)
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
        output_release = list(targets = planned),
        app_auth = list(
          enabled = FALSE,
          account_count = 0L,
          timeout_minutes = 15L
        )
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
          auth_enabled = FALSE,
          auth_env_file = NULL,
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
        "initial_page",
        "show_upload_ui",
        "welcome_message",
        "point_size",
        "variable_to_compare",
        "host",
        "port",
        "max_request_size",
        "display_mode",
        "launch_browser",
        "auth",
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

test_that("coordinator preserves per-dataset Viewer defaults for parent verification", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    plan <- builder_app_coordinator_plan_fixture(file.path(root, "release"))
    contract <- .builder_coordinator_app_contract(plan)

    expect_identical(
      contract$plan$items[[1L]]$default_projection,
      "umap"
    )
    expect_null(contract$plan$items[[1L]]$default_trajectory)
    expect_identical(
      contract$plan$items[[1L]]$overview_point_size,
      5
    )
    expect_identical(
      contract$plan$items[[2L]]$default_projection,
      "pca"
    )
    expect_identical(
      contract$plan$items[[2L]]$default_trajectory,
      list(method = "slingshot", name = "lineage")
    )
    expect_identical(
      contract$plan$items[[2L]]$overview_point_size,
      7
    )
  })
})

test_that("coordinator exact targets allow declared App tree members", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    plan <- builder_app_coordinator_plan_fixture(file.path(root, "release"))
    handle <- builder_coordinator_prepare(plan, "app-tree-members")
    dir.create(
      file.path(handle$stage, "cerebro_app", "viewer"),
      recursive = TRUE
    )
    writeLines("app", file.path(handle$stage, "cerebro_app", "app.R"))
    writeLines(
      "viewer",
      file.path(handle$stage, "cerebro_app", "viewer", "ui.R")
    )
    writeLines("report", file.path(handle$stage, "build-report.json"))

    identity <- .builder_coordinator_stage_identity(
      handle,
      expected = c("cerebro_app", "build-report.json"),
      exact = TRUE
    )
    expect_true(any(vapply(
      identity$entries,
      function(entry) identical(entry$path, "cerebro_app/viewer/ui.R"),
      logical(1)
    )))
    expect_true(builder_coordinator_abort(handle)$aborted)
  })
})

test_that("parent and worker requests retain identical Viewer defaults", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    plan <- builder_app_coordinator_plan_fixture(file.path(root, "release"))
    plan$items[[1L]]$overview_percentage_cells_to_show <- 65
    plan$items[[2L]]$overview_percentage_cells_to_show <- 80
    handle <- builder_coordinator_prepare(plan, "viewer-defaults")
    artifacts <- file.path(
      handle$stage,
      vapply(
        plan$items,
        `[[`,
        character(1),
        "filename"
      )
    )
    lapply(artifacts, function(path) writeBin(raw(0), path))
    names(artifacts) <- vapply(plan$items, `[[`, character(1), "name")

    worker_request <- builder_app_bundle_request(
      plan,
      artifacts,
      names(artifacts)
    )
    parent_request <- builder_app_bundle_request(
      handle$app_plan,
      artifacts,
      names(artifacts)
    )

    expect_identical(
      parent_request$viewer_content,
      worker_request$viewer_content
    )
    expect_identical(
      parent_request$viewer_content[["Dataset A"]][[
        "overview_percentage_cells_to_show"
      ]],
      65
    )
    expect_identical(
      parent_request$viewer_content[["Dataset B"]][[
        "overview_percentage_cells_to_show"
      ]],
      80
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

test_that("coordinator requires exact scalar authentication evidence", {
  local({
    builder_task9_source()
    login <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    for (value in list(NULL, NA, "TRUE", FALSE)) {
      forged <- login$result$app_verification
      forged["auth_enabled"] <- list(value)
      expect_error(
        .builder_coordinator_app_verification(
          forged,
          login$handle$app_expectation
        ),
        "differs from the frozen plan"
      )
    }
    expect_true(builder_coordinator_abort(login$handle)$aborted)

    root <- withr::local_tempdir()
    plan <- builder_app_coordinator_plan_fixture(file.path(root, "release"))
    plan$app_auth <- list(
      enabled = FALSE,
      account_count = 0L,
      timeout_minutes = 15L
    )
    public <- builder_app_coordinator_fixture(
      root = root,
      plan = plan,
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    for (value in list(NULL, NA, "FALSE", TRUE)) {
      forged <- public$result$app_verification
      forged["auth_enabled"] <- list(value)
      expect_error(
        .builder_coordinator_app_verification(
          forged,
          public$handle$app_expectation
        ),
        "differs from the frozen plan"
      )
    }
    expect_true(builder_coordinator_abort(public$handle)$aborted)
  })
})

test_that("coordinator binds top-level build authentication to all plan modes", {
  local({
    builder_task9_source()
    check_forged <- function(fixture, enabled, env_file) {
      valid <- fixture$result
      expect_silent(.builder_coordinator_validate_build_auth(
        fixture$handle,
        valid
      ))
      for (value in list(NULL, NA, "TRUE", 0, !enabled)) {
        forged <- valid
        forged["auth_enabled"] <- list(value)
        expect_error(
          .builder_coordinator_validate_build_auth(fixture$handle, forged),
          "authentication evidence"
        )
      }
      forged_env <- valid
      forged_env["auth_env_file"] <- list(env_file)
      expect_error(
        .builder_coordinator_validate_build_auth(fixture$handle, forged_env),
        "authentication evidence"
      )
      expect_true(builder_coordinator_abort(fixture$handle)$aborted)
    }

    login <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    check_forged(login, TRUE, file.path(login$root, "wrong.env"))

    public_root <- withr::local_tempdir()
    public_plan <- builder_app_coordinator_plan_fixture(
      file.path(public_root, "release")
    )
    public_plan$app_auth <- list(
      enabled = FALSE,
      account_count = 0L,
      timeout_minutes = 15L
    )
    public_plan$targets <- file.path(public_root, "release", "cerebro_app")
    public_plan$output_release$targets <- public_plan$targets
    public <- builder_app_coordinator_fixture(
      root = public_root,
      plan = public_plan,
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    check_forged(
      public,
      FALSE,
      file.path(public$handle$stage, "viewer-auth.env")
    )

    crb_root <- withr::local_tempdir()
    crb_plan <- builder_crb_coordinator_plan(
      file.path(crb_root, "release"),
      "dataset.crb"
    )
    crb <- builder_coordinator_prepare(crb_plan, "crb-auth")
    artifact <- file.path(crb$stage, "dataset.crb")
    saveRDS(list(ok = TRUE), artifact)
    crb_fixture <- list(
      root = crb_root,
      handle = crb,
      result = builder_crb_coordinator_result(crb, artifact)
    )
    check_forged(
      crb_fixture,
      FALSE,
      file.path(crb$stage, "viewer-auth.env")
    )
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

test_that("input closure races fail the ownership record guard", {
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
      "release ownership record does not match the complete release"
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
    expect_true(file.exists(published$report_path))
    expect_identical(
      published$app_dir,
      file.path(published$release$target, "cerebro_app")
    )
    expect_identical(
      unname(published$built),
      file.path(
        published$release$target,
        "cerebro_app",
        "private-data",
        basename(fixture$result$built)
      )
    )
    expect_identical(names(published$built), names(fixture$result$built))
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
        "build-report.json",
        "cerebro_app",
        "cerebro_app/app.R",
        "cerebro_app/private-data/dataset-a.crb",
        "cerebro_app/private-data/dataset-b.crb"
      ) %in%
        member_paths
    ))
  })
})

test_that("coordinator reports after App verification and before ownership", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    events <- character()

    published <- builder_coordinator_publish(
      fixture$handle,
      fixture$result,
      .verify_app = function(...) {
        events <<- c(events, "app_verify")
        builder_verify_app(...)
      },
      .write_report = function(stage, report) {
        events <<- c(events, "report")
        builder_write_build_report(stage, report)
      },
      .record_move = function(from, to) {
        events <<- c(events, "ownership")
        file.rename(from, to)
      }
    )

    expect_identical(events, c("app_verify", "report", "ownership"))
    expect_true(published$published)
    expect_true(file.exists(published$report_path))
  })
})

test_that("coordinator freezes only the portable report-plan projection", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    plan <- builder_app_coordinator_plan_fixture(file.path(root, "release"))
    plan$runtime_only <- new.env(parent = emptyenv())

    handle <- builder_coordinator_prepare(plan, "portable-report-plan")

    expect_s3_class(handle$report_plan, "builder_build_plan")
    expect_false(.builder_app_has_reference(handle$report_plan))
    expect_false("runtime_only" %in% names(handle$report_plan))
    expect_identical(handle$report_plan$dataset_order, plan$dataset_order)
    expect_identical(
      handle$report_plan$output_release$targets,
      plan$output_release$targets
    )
    expect_identical(
      handle$report_plan$items[[1L]]$spatial_image_storage,
      plan$items[[1L]]$spatial_image_storage %||% "embedded"
    )
    expect_identical(
      handle$report_plan$items[[1L]]$spatial_alignment,
      list(
        section_count = as.integer(
          plan$items[[1L]]$spatial_alignment$section_count %||% 0L
        ),
        image_count = as.integer(
          plan$items[[1L]]$spatial_alignment$image_count %||% 0L
        )
      )
    )
    expect_true(builder_coordinator_abort(handle)$aborted)
  })
})

test_that("coordinator report projection rebuilds only safe auth fields", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    plan <- builder_crb_coordinator_plan(
      file.path(root, "release"),
      "dataset-a.crb"
    )
    plan$app_auth <- list(
      enabled = FALSE,
      account_count = 0L,
      timeout_minutes = 15L,
      accounts = "auth-report-account-sentinel-91c4",
      passphrase = "auth-report-passphrase-sentinel-91c4"
    )

    report_plan <- .builder_coordinator_report_plan(plan)

    expect_identical(
      report_plan$app_auth,
      list(enabled = FALSE, account_count = 0L, timeout_minutes = 15L)
    )
    expect_false(builder_auth_value_contains(
      report_plan,
      "auth-report-account-sentinel-91c4"
    ))
    expect_false(builder_auth_value_contains(
      report_plan,
      "auth-report-passphrase-sentinel-91c4"
    ))
  })
})

test_that("coordinator rejects forged auth on CRB-only plans", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    plan <- builder_crb_coordinator_plan(
      file.path(root, "release"),
      "dataset-a.crb"
    )
    cases <- list(
      extra_fields = list(
        enabled = FALSE,
        account_count = 0L,
        timeout_minutes = 15L,
        accounts = "auth-forged-account-sentinel-91c4",
        passphrase = "auth-forged-passphrase-sentinel-91c4"
      ),
      login_without_app = list(
        enabled = TRUE,
        account_count = 1L,
        timeout_minutes = 15L
      )
    )
    for (name in names(cases)) {
      plan$app_auth <- cases[[name]]
      error <- tryCatch(
        builder_coordinator_prepare(plan, paste0("forged-crb-auth-", name)),
        error = identity
      )

      expect_s3_class(error, "error")
      expect_match(
        conditionMessage(error),
        "invalid",
        ignore.case = TRUE,
        info = name
      )
      expect_false(builder_auth_value_contains(
        error,
        "auth-forged-account-sentinel-91c4"
      ))
      expect_false(builder_auth_value_contains(
        error,
        "auth-forged-passphrase-sentinel-91c4"
      ))
    }
  })
})

test_that("report failure preserves the prior release unpublished", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    saveRDS("prior", file.path(target, "dataset-a.crb"))
    prior <- builder_release_identity(target)
    plan <- builder_app_coordinator_plan_fixture(target, make_app = FALSE)
    plan$overwrite <- TRUE
    plan$expected_prior_identity <- prior
    fixture <- builder_app_coordinator_fixture(
      root = root,
      target = target,
      make_app = FALSE,
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app,
      plan = plan
    )

    expect_error(
      builder_coordinator_publish(
        fixture$handle,
        fixture$result,
        .write_report = function(...) stop("injected report failure")
      ),
      "report failure"
    )
    expect_identical(readRDS(file.path(target, "dataset-a.crb")), "prior")
    expect_false(file.exists(file.path(target, "build-report.json")))
    expect_true(dir.exists(fixture$handle$stage))
  })
})

test_that("public Apps reach publication guards without authentication material", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    plan <- builder_app_coordinator_plan_fixture(target)
    plan$app_auth <- list(
      enabled = FALSE,
      account_count = 0L,
      timeout_minutes = 15L
    )
    plan$targets <- file.path(target, "cerebro_app")
    plan$output_release$targets <- plan$targets
    fixture <- builder_app_coordinator_fixture(
      root = root,
      target = target,
      plan = plan,
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    published <- builder_coordinator_publish(fixture$handle, fixture$result)
    expect_true(published$published)
    expect_false(file.exists(file.path(target, "viewer-auth.env")))
    expect_true(dir.exists(file.path(target, "cerebro_app")))
  })
})

test_that("login publication removes only transient root inputs after reporting", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    stage <- fixture$handle$stage
    published <- builder_coordinator_publish(fixture$handle, fixture$result)
    target <- published$release$target
    root_inputs <- basename(unname(fixture$result$built))
    expect_false(any(file.exists(file.path(target, root_inputs))))
    expect_true(all(file.exists(file.path(
      target,
      "cerebro_app",
      "private-data",
      root_inputs
    ))))
    expect_true(file.exists(file.path(target, "viewer-auth.env")))
    expect_true(isTRUE(builder_auth_verify_database_pair(
      file.path(
        target,
        "cerebro_app",
        "private-data",
        "auth",
        "credentials.sqlite"
      ),
      file.path(target, "viewer-auth.env")
    )))
    expect_false(dir.exists(stage))
  })
})

test_that("login publication rejects an environment changed after parent verification", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    env_file <- file.path(fixture$handle$stage, "viewer-auth.env")
    expect_error(
      builder_coordinator_publish(
        fixture$handle,
        fixture$result,
        .write_report = function(stage, report) {
          path <- builder_write_build_report(stage, report)
          Sys.chmod(env_file, mode = "0644")
          path
        }
      ),
      "authentication secret file is not private"
    )
    expect_true(dir.exists(fixture$handle$stage))
    expect_false(dir.exists(fixture$handle$target))
  })
})

test_that("publication rejects a mapped verification path that is absent", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    fixture$result$verifications[[1L]]$path <- file.path(
      fixture$root,
      "missing-verification.crb"
    )
    expect_error(
      builder_coordinator_publish(fixture$handle, fixture$result),
      "Publication verification failed"
    )
  })
})

test_that("login publication detects same-content inode and hard-link env replacement", {
  local({
    builder_task9_source()
    run_case <- function(name, mutate) {
      fixture <- builder_app_coordinator_fixture(
        .local_envir = environment(),
        coordinator_prepare = builder_coordinator_prepare,
        bundle_request = builder_app_bundle_request,
        verify_app = builder_verify_app
      )
      env_file <- file.path(fixture$handle$stage, "viewer-auth.env")
      expect_error(
        builder_coordinator_publish(
          fixture$handle,
          fixture$result,
          .write_report = function(stage, report) {
            path <- builder_write_build_report(stage, report)
            mutate(env_file, stage)
            path
          }
        ),
        "staged App changed after parent verification",
        info = name
      )
    }
    run_case("inode", function(env_file, stage) {
      value <- readLines(env_file, warn = FALSE)
      unlink(env_file)
      writeLines(value, env_file, useBytes = TRUE)
      Sys.chmod(env_file, mode = "0600")
    })
    probe <- withr::local_tempfile()
    writeLines("probe", probe)
    skip_if_not(
      isTRUE(file.link(probe, paste0(probe, "-link"))),
      "Hard links are unavailable"
    )
    run_case("hard-link", function(env_file, stage) {
      source <- file.path(stage, "same-content-env")
      copied <- file.copy(env_file, source)
      stopifnot(isTRUE(copied))
      unlink(env_file)
      stopifnot(isTRUE(file.link(source, env_file)))
    })
  })
})

test_that("login publisher guard rejects env changes in both rename windows", {
  local({
    builder_task9_source()
    run_case <- function(window, mutate) {
      fixture <- builder_app_coordinator_fixture(
        .local_envir = environment(),
        coordinator_prepare = builder_coordinator_prepare,
        bundle_request = builder_app_bundle_request,
        verify_app = builder_verify_app
      )
      publish <- function(handle, .verify_payload) {
        builder_publish_release(
          handle,
          .verify_payload = .verify_payload,
          .after_phase = function(phase) {
            if (identical(window, "old_moved") && identical(phase, window)) {
              mutate(file.path(handle$stage, "viewer-auth.env"), handle$stage)
            }
          },
          .after_move = function(move) {
            if (identical(window, "new_to_target") && identical(move, window)) {
              mutate(file.path(handle$target, "viewer-auth.env"), handle$target)
            }
          }
        )
      }
      expect_error(
        builder_coordinator_publish(
          fixture$handle,
          fixture$result,
          .publish = publish
        ),
        "Publication verification failed"
      )
      expect_true(dir.exists(fixture$handle$stage))
      expect_false(dir.exists(fixture$handle$target))
    }
    run_case("old_moved", function(env_file, root) Sys.chmod(env_file, "0644"))
    run_case("new_to_target", function(env_file, root) {
      value <- readLines(env_file, warn = FALSE)
      unlink(env_file)
      writeLines(value, env_file, useBytes = TRUE)
      Sys.chmod(env_file, mode = "0600")
    })
  })
})
