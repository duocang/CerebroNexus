if (!exists("viewer_auth_fixture", mode = "function")) {
  source(testthat::test_path("helper-viewer-auth.R"), local = TRUE)
}

auth_bundle_artifacts <- function(root) {
  list.files(
    root,
    pattern = "^\\.app-(stage-|backup-|build\\.lock)",
    all.files = TRUE,
    full.names = TRUE
  )
}

auth_build_app <- function(root, auth = NULL, ...) {
  result <- file.path(root, "app")
  crb <- viewer_auth_test_crb(file.path(root, "source"))
  createShinyApp(
    cerebro_data = c("Dataset" = crb),
    result_dir = result,
    auth = auth,
    launch_browser = FALSE,
    verbose = FALSE,
    ...
  )
  result
}

auth_bundle_build_ops <- function(...) {
  utils::modifyList(.bundleBuildOps(), list(...))
}

capture_auth_bundle_conditions <- function(expr) {
  warnings <- character()
  messages <- character()
  value <- NULL
  stderr <- capture.output(
    stdout <- capture.output(
      value <- withCallingHandlers(
        tryCatch(
          eval(substitute(expr), envir = parent.frame()),
          error = identity
        ),
        warning = function(condition) {
          warnings <<- c(warnings, conditionMessage(condition))
          invokeRestart("muffleWarning")
        },
        message = function(condition) {
          messages <<- c(messages, conditionMessage(condition))
          invokeRestart("muffleMessage")
        }
      ),
      type = "output"
    ),
    type = "message"
  )
  list(
    value = value,
    warnings = warnings,
    messages = messages,
    stdout = stdout,
    stderr = stderr
  )
}

expect_auth_bundle_rollback <- function(root, result, old_mode) {
  expect_identical(readLines(file.path(result, "marker.txt")), "OLD")
  expect_equal(
    as.integer(file.info(result)$mode[[1L]]),
    as.integer(old_mode)
  )
  expect_length(auth_bundle_artifacts(root), 0L)
}

test_that("invalid authentication fails before result target preparation", {
  root <- withr::local_tempdir()
  crb <- viewer_auth_test_crb(file.path(root, "source"))
  result <- file.path(root, "new-parent", "app")
  prepare_calls <- 0L
  testthat::local_mocked_bindings(
    .prepareBundleResultTarget = function(result_dir) {
      prepare_calls <<- prepare_calls + 1L
      stop("target preparation reached", call. = FALSE)
    },
    .package = "CerebroNexus"
  )

  expect_error(
    createShinyApp(
      cerebro_data = c("Dataset" = crb),
      result_dir = result,
      auth = list(
        provider = "shinymanager",
        credentials = "relative.sqlite",
        passphrase_env = "CEREBRO_TEST_AUTH_PASSPHRASE"
      ),
      launch_browser = FALSE,
      verbose = FALSE
    ),
    "absolute path",
    fixed = TRUE
  )
  expect_identical(prepare_calls, 0L)
  expect_false(file.exists(result))
  expect_false(dir.exists(dirname(result)))
  expect_length(auth_bundle_artifacts(root), 0L)
})

test_that("matrix authentication fails before mutating an existing target", {
  fixture <- viewer_auth_fixture()
  crb <- viewer_auth_test_crb(file.path(fixture$root, "source"))
  result <- file.path(fixture$root, "app")
  dir.create(result)
  marker <- file.path(result, "marker.txt")
  writeLines("OLD", marker)
  credentials <- shinymanager::read_db_decrypt(
    conn = fixture$database,
    name = "credentials",
    passphrase = fixture$passphrase
  )
  credentials$user <- matrix(credentials$user, ncol = 1L)
  shinymanager::write_db_encrypt(
    conn = fixture$database,
    value = credentials,
    name = "credentials",
    passphrase = fixture$passphrase
  )
  prepare_calls <- 0L
  testthat::local_mocked_bindings(
    .prepareBundleResultTarget = function(result_dir) {
      prepare_calls <<- prepare_calls + 1L
      stop("target preparation reached", call. = FALSE)
    },
    .package = "CerebroNexus"
  )

  expect_error(
    createShinyApp(
      cerebro_data = c("Dataset" = crb),
      result_dir = result,
      auth = fixture$descriptor,
      overwrite = TRUE,
      launch_browser = FALSE,
      verbose = FALSE
    ),
    "auth$credentials",
    fixed = TRUE
  )
  expect_identical(prepare_calls, 0L)
  expect_identical(readLines(marker), "OLD")
  expect_length(auth_bundle_artifacts(fixture$root), 0L)
})

test_that("NULL authentication omits private auth data and manifest", {
  root <- withr::local_tempdir()
  result <- auth_build_app(
    root,
    cerebro_options = list(.viewer_auth = list(forged = TRUE))
  )
  config <- readRDS(file.path(result, "cerebro_config.rds"))

  expect_false(".viewer_auth" %in% names(config))
  expect_false(dir.exists(file.path(result, "private-data", "auth")))
})

test_that("enabled authentication publishes one frozen private manifest", {
  fixture <- viewer_auth_fixture()
  crb_root <- file.path(fixture$root, "crb")
  crb <- viewer_auth_test_crb(crb_root)
  result <- file.path(fixture$root, "app")
  sentinel <- "AUTH_SOURCE_PATH_SENTINEL_7319"
  source_with_sentinel <- file.path(
    dirname(fixture$database),
    paste0("source-", sentinel, ".sqlite")
  )
  file.copy(fixture$database, source_with_sentinel)
  descriptor <- fixture$descriptor
  descriptor$credentials <- normalizePath(source_with_sentinel, winslash = "/")
  expected <- .compileViewerAuth(descriptor, scope = "bundle")$config

  captured <- capture_auth_bundle_conditions(createShinyApp(
    cerebro_data = c("Dataset" = crb),
    result_dir = result,
    auth = descriptor,
    cerebro_options = list(.viewer_auth = list(forged = TRUE)),
    launch_browser = FALSE,
    verbose = TRUE
  ))
  expect_false(inherits(captured$value, "error"))
  config <- readRDS(file.path(result, "cerebro_config.rds"))
  auth_path <- file.path(
    result,
    "private-data",
    "auth",
    "credentials.sqlite"
  )

  expect_identical(config[[".viewer_auth"]], expected)
  expect_identical(
    names(config)[names(config) == ".viewer_auth"],
    ".viewer_auth"
  )
  expect_identical(
    config[[".viewer_auth"]]$credentials_path,
    "private-data/auth/credentials.sqlite"
  )
  expect_true(file.exists(auth_path))
  expect_false(file.exists(file.path(
    result,
    "private-data",
    "auth",
    basename(source_with_sentinel)
  )))
  serialized <- paste(capture.output(str(config)), collapse = "\n")
  app_source <- paste(readLines(file.path(result, "app.R")), collapse = "\n")
  expect_false(grepl(descriptor$credentials, serialized, fixed = TRUE))
  expect_false(grepl(fixture$passphrase, serialized, fixed = TRUE))
  expect_false("passphrase" %in% names(config[[".viewer_auth"]]))
  expect_false(grepl(descriptor$credentials, app_source, fixed = TRUE))
  expect_false(grepl(fixture$passphrase, app_source, fixed = TRUE))
  expect_match(
    app_source,
    'source(file.path(cerebro_root, "viewer/auth.R"), local = TRUE)',
    fixed = TRUE
  )
  expect_match(app_source, "viewer_auth_apply", fixed = TRUE)
  expect_silent(parse(file = file.path(result, "app.R")))
  expect_false(any(grepl(
    sentinel,
    unlist(captured, use.names = FALSE),
    fixed = TRUE
  )))
  expect_false(any(grepl(
    fixture$passphrase,
    unlist(captured, use.names = FALSE),
    fixed = TRUE
  )))

  token <- getFromNamespace(".tok", "shinymanager")
  old_path <- token$get_sqlite_path()
  old_passphrase <- token$get_passphrase()
  withr::defer({
    token$set_sqlite_path(old_path)
    token$set_passphrase(old_passphrase)
  })
  checker <- shinymanager::check_credentials(
    db = auth_path,
    passphrase = fixture$passphrase
  )
  expect_true(isTRUE(checker("viewer", "correct horse 47")$result))

  if (.Platform$OS.type != "windows") {
    expect_identical(
      bitwAnd(as.integer(file.info(dirname(auth_path))$mode[[1L]]), 511L),
      strtoi("700", base = 8L)
    )
    expect_identical(
      bitwAnd(as.integer(file.info(auth_path)$mode[[1L]]), 511L),
      strtoi("600", base = 8L)
    )
  }
  expect_identical(unname(file.access(auth_path, mode = 6L)), 0L)
})

test_that("authentication target uses the existing collision registry", {
  fixture <- viewer_auth_fixture()
  source_dir <- file.path(fixture$root, "source")
  dir.create(file.path(source_dir, "auth"), recursive = TRUE)
  backend <- file.path(source_dir, "auth", "credentials.sqlite")
  writeLines("backend", backend)
  crb <- file.path(source_dir, "dataset.crb")
  object <- Cerebro_v1.3$new()
  object$setExpressionBackend("h5", "auth/credentials.sqlite")
  saveRDS(object, crb)

  expect_error(
    createShinyApp(
      cerebro_data = c("Dataset" = crb),
      result_dir = file.path(fixture$root, "app"),
      auth = fixture$descriptor,
      launch_browser = FALSE,
      verbose = FALSE
    ),
    "same bundle target",
    fixed = TRUE
  )
})

test_that("authentication chmod errors roll back without leaking details", {
  skip_on_os("windows")
  fixture <- viewer_auth_fixture()
  crb <- viewer_auth_test_crb(file.path(fixture$root, "source"))
  result <- file.path(fixture$root, "app")
  dir.create(result)
  writeLines("OLD", file.path(result, "marker.txt"))
  Sys.chmod(result, "0711")
  old_mode <- file.info(result)$mode[[1L]]
  sentinel <- paste0("chmod-detail-", fixture$passphrase)
  ops <- auth_bundle_build_ops(chmod = function(path, mode) stop(sentinel))
  testthat::local_mocked_bindings(
    .bundleBuildOps = function() ops,
    .package = "CerebroNexus"
  )

  captured <- capture_auth_bundle_conditions(createShinyApp(
    cerebro_data = c("Dataset" = crb),
    result_dir = result,
    auth = fixture$descriptor,
    overwrite = TRUE,
    launch_browser = FALSE,
    verbose = TRUE
  ))
  expect_s3_class(captured$value, "error")
  expect_identical(
    conditionMessage(captured$value),
    "Failed to harden the authentication database."
  )
  expect_false(any(grepl(sentinel, unlist(captured), fixed = TRUE)))
  expect_auth_bundle_rollback(fixture$root, result, old_mode)
})

test_that("authentication mode verification rejects ineffective chmod", {
  skip_on_os("windows")
  fixture <- viewer_auth_fixture()
  crb <- viewer_auth_test_crb(file.path(fixture$root, "source"))
  result <- file.path(fixture$root, "app")
  dir.create(result)
  writeLines("OLD", file.path(result, "marker.txt"))
  old_mode <- file.info(result)$mode[[1L]]
  ops <- auth_bundle_build_ops(chmod = function(path, mode) {
    Sys.chmod(path, as.octmode("0777"))
    TRUE
  })
  testthat::local_mocked_bindings(
    .bundleBuildOps = function() ops,
    .package = "CerebroNexus"
  )

  expect_error(
    createShinyApp(
      cerebro_data = c("Dataset" = crb),
      result_dir = result,
      auth = fixture$descriptor,
      overwrite = TRUE,
      launch_browser = FALSE,
      verbose = FALSE
    ),
    "Failed to harden the authentication database.",
    fixed = TRUE
  )
  expect_auth_bundle_rollback(fixture$root, result, old_mode)
})

test_that("authentication copy failure rolls back without leaking source", {
  fixture <- viewer_auth_fixture()
  crb <- viewer_auth_test_crb(file.path(fixture$root, "source"))
  result <- file.path(fixture$root, "app")
  dir.create(result)
  writeLines("OLD", file.path(result, "marker.txt"))
  old_mode <- file.info(result)$mode[[1L]]
  original_copy <- auth_bundle_build_ops()$copy
  ops <- auth_bundle_build_ops(copy = function(from, to, ...) {
    normalized <- normalizePath(from, winslash = "/", mustWork = TRUE)
    if (identical(normalized, fixture$database)) {
      return(FALSE)
    }
    original_copy(from, to, ...)
  })
  testthat::local_mocked_bindings(
    .bundleBuildOps = function() ops,
    .package = "CerebroNexus"
  )

  error <- expect_error(createShinyApp(
    cerebro_data = c("Dataset" = crb),
    result_dir = result,
    auth = fixture$descriptor,
    overwrite = TRUE,
    launch_browser = FALSE,
    verbose = FALSE
  ))
  expect_match(
    conditionMessage(error),
    "Failed to copy authentication database"
  )
  expect_false(grepl(fixture$database, conditionMessage(error), fixed = TRUE))
  expect_false(grepl(fixture$passphrase, conditionMessage(error), fixed = TRUE))
  expect_auth_bundle_rollback(fixture$root, result, old_mode)
})

test_that("staged authentication is revalidated after the source changes", {
  fixture <- viewer_auth_fixture()
  crb <- viewer_auth_test_crb(file.path(fixture$root, "source"))
  result <- file.path(fixture$root, "app")
  dir.create(result)
  writeLines("OLD", file.path(result, "marker.txt"))
  old_mode <- file.info(result)$mode[[1L]]
  sentinel <- paste0("corrupt-detail-", fixture$passphrase)
  original_copy <- auth_bundle_build_ops()$copy
  ops <- auth_bundle_build_ops(copy = function(from, to, ...) {
    normalized <- normalizePath(from, winslash = "/", mustWork = TRUE)
    if (identical(normalized, fixture$database)) {
      writeLines(sentinel, to)
      writeLines(sentinel, from)
      return(TRUE)
    }
    original_copy(from, to, ...)
  })
  testthat::local_mocked_bindings(
    .bundleBuildOps = function() ops,
    .package = "CerebroNexus"
  )

  captured <- capture_auth_bundle_conditions(createShinyApp(
    cerebro_data = c("Dataset" = crb),
    result_dir = result,
    auth = fixture$descriptor,
    overwrite = TRUE,
    launch_browser = FALSE,
    verbose = TRUE
  ))
  expect_s3_class(captured$value, "error")
  expect_identical(
    conditionMessage(captured$value),
    "Failed to validate the staged authentication database."
  )
  expect_false(any(grepl(sentinel, unlist(captured), fixed = TRUE)))
  expect_auth_bundle_rollback(fixture$root, result, old_mode)
})
