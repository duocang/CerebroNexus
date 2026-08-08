auth_test_database <- function() {
  path <- withr::local_tempfile(
    fileext = ".sqlite",
    .local_envir = parent.frame()
  )
  writeBin(as.raw(c(0x53, 0x51, 0x4c)), path)
  path
}

auth_test_descriptor <- function(path = auth_test_database()) {
  list(
    credentials = path,
    passphrase_env = "CEREBRO_AUTH_TEST_KEY",
    timeout_minutes = 20
  )
}

test_that("NULL keeps Viewer authentication disabled", {
  expect_null(CerebroNexus:::.compileViewerAuth(NULL))
})

test_that("authentication accepts an existing encrypted database", {
  path <- auth_test_database()
  withr::local_envvar(CEREBRO_AUTH_TEST_KEY = "test database passphrase")
  testthat::local_mocked_bindings(
    .viewerAuthRequireProvider = function() invisible(TRUE),
    .viewerAuthValidateDatabase = function(path, passphrase) invisible(TRUE),
    .package = "CerebroNexus"
  )

  config <- CerebroNexus:::.compileViewerAuth(auth_test_descriptor(path))

  expect_identical(
    config$credentials_path,
    "private-data/auth/credentials.sqlite"
  )
  expect_identical(config$passphrase_env, "CEREBRO_AUTH_TEST_KEY")
  expect_identical(config$timeout_minutes, 20L)
  expect_identical(config$source, normalizePath(path, winslash = "/"))
  expect_false(any(grepl("test database passphrase", config, fixed = TRUE)))
})

test_that("authentication descriptor has one strict shape", {
  invalid <- list(
    FALSE,
    "credentials.sqlite",
    list(),
    list(credentials = "credentials.sqlite"),
    list(
      credentials = "credentials.sqlite",
      passphrase_env = "CEREBRO_AUTH_TEST_KEY",
      provider = "shinymanager"
    )
  )
  for (value in invalid) {
    expect_error(
      CerebroNexus:::.compileViewerAuth(value),
      "auth must be a named list"
    )
  }
})

test_that("authentication validates path and environment before provider", {
  withr::local_envvar(CEREBRO_AUTH_TEST_KEY = NA)
  expect_error(
    CerebroNexus:::.compileViewerAuth(auth_test_descriptor("missing.sqlite")),
    "auth\\$credentials"
  )

  path <- auth_test_database()
  descriptor <- auth_test_descriptor(path)
  descriptor$passphrase_env <- "not-valid"
  expect_error(
    CerebroNexus:::.compileViewerAuth(descriptor),
    "auth\\$passphrase_env"
  )

  expect_error(
    CerebroNexus:::.compileViewerAuth(auth_test_descriptor(path)),
    "CEREBRO_AUTH_TEST_KEY is not set"
  )
})

test_that("authentication timeout is a whole minute in range", {
  path <- auth_test_database()
  withr::local_envvar(CEREBRO_AUTH_TEST_KEY = "test database passphrase")
  for (value in list(0, 1441, 1.5, NA_real_, Inf, TRUE, "15")) {
    descriptor <- auth_test_descriptor(path)
    descriptor$timeout_minutes <- value
    expect_error(
      CerebroNexus:::.compileViewerAuth(descriptor),
      "auth\\$timeout_minutes"
    )
  }
})

test_that("authentication reports unavailable provider and invalid database", {
  path <- auth_test_database()
  withr::local_envvar(CEREBRO_AUTH_TEST_KEY = "test database passphrase")

  testthat::local_mocked_bindings(
    .viewerAuthRequireProvider = function() {
      stop("Authentication requires shinymanager (>= 1.1.0).", call. = FALSE)
    },
    .package = "CerebroNexus"
  )
  expect_error(
    CerebroNexus:::.compileViewerAuth(auth_test_descriptor(path)),
    "requires shinymanager"
  )

  testthat::local_mocked_bindings(
    .viewerAuthRequireProvider = function() invisible(TRUE),
    .viewerAuthValidateDatabase = function(path, passphrase) {
      stop("auth$credentials and its passphrase do not match.", call. = FALSE)
    },
    .package = "CerebroNexus"
  )
  expect_error(
    CerebroNexus:::.compileViewerAuth(auth_test_descriptor(path)),
    "do not match"
  )
})

auth_test_build_fixture <- function() {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  crb <- file.path(root, "dataset.crb")
  saveRDS(Cerebro_v1.3$new(), crb)
  credentials <- file.path(root, "credentials.sqlite")
  writeBin(as.raw(c(0x53, 0x51, 0x4c)), credentials)
  list(root = root, crb = crb, credentials = credentials)
}

auth_test_compiled_config <- function(source) {
  list(
    credentials_path = "private-data/auth/credentials.sqlite",
    passphrase_env = "CEREBRO_AUTH_TEST_KEY",
    timeout_minutes = 15L,
    source = normalizePath(source, winslash = "/")
  )
}

auth_test_file_contains <- function(path, value) {
  size <- file.info(path)$size[[1L]]
  bytes <- readBin(path, "raw", n = size)
  haystack <- paste(format(bytes), collapse = "")
  needle <- paste(format(charToRaw(value)), collapse = "")
  grepl(needle, haystack, fixed = TRUE)
}

test_that("createShinyApp bundles only encrypted authentication configuration", {
  fixture <- auth_test_build_fixture()
  app <- file.path(fixture$root, "app")
  passphrase <- "secret-that-must-not-enter-the-app"
  withr::local_envvar(CEREBRO_AUTH_TEST_KEY = passphrase)
  testthat::local_mocked_bindings(
    .compileViewerAuth = function(auth) {
      auth_test_compiled_config(auth$credentials)
    },
    .package = "CerebroNexus"
  )

  createShinyApp(
    cerebro_data = c(Dataset = fixture$crb),
    result_dir = app,
    auth = list(
      credentials = fixture$credentials,
      passphrase_env = "CEREBRO_AUTH_TEST_KEY"
    ),
    launch_browser = FALSE,
    verbose = FALSE
  )

  bundled <- file.path(
    app,
    "private-data",
    "auth",
    "credentials.sqlite"
  )
  expect_true(file.exists(bundled))
  expect_identical(readBin(bundled, "raw", n = 3L), as.raw(c(0x53, 0x51, 0x4c)))

  config <- readRDS(file.path(app, "cerebro_config.rds"))$.viewer_auth
  expect_identical(
    config,
    list(
      credentials_path = "private-data/auth/credentials.sqlite",
      passphrase_env = "CEREBRO_AUTH_TEST_KEY",
      timeout_minutes = 15L
    )
  )
  artifacts <- list.files(app, recursive = TRUE, full.names = TRUE)
  expect_false(any(vapply(
    artifacts,
    auth_test_file_contains,
    logical(1),
    value = passphrase
  )))
  expect_false(any(vapply(
    artifacts,
    auth_test_file_contains,
    logical(1),
    value = normalizePath(fixture$credentials, winslash = "/")
  )))
})

test_that("createShinyApp keeps unauthenticated output unchanged", {
  fixture <- auth_test_build_fixture()
  app <- file.path(fixture$root, "public-app")

  createShinyApp(
    cerebro_data = c(Dataset = fixture$crb),
    result_dir = app,
    launch_browser = FALSE,
    verbose = FALSE
  )

  config <- readRDS(file.path(app, "cerebro_config.rds"))
  expect_false(".viewer_auth" %in% names(config))
  expect_false(dir.exists(file.path(app, "private-data", "auth")))
})

test_that("authentication preflight fails before output mutation", {
  fixture <- auth_test_build_fixture()
  app <- file.path(fixture$root, "existing-app")
  dir.create(app)
  marker <- file.path(app, "marker.txt")
  writeLines("KEEP", marker)
  compile_calls <- 0L
  testthat::local_mocked_bindings(
    .compileViewerAuth = function(auth) {
      compile_calls <<- compile_calls + 1L
      stop("database and passphrase do not match", call. = FALSE)
    },
    .package = "CerebroNexus"
  )

  expect_error(
    createShinyApp(
      cerebro_data = c(Dataset = fixture$crb),
      result_dir = app,
      auth = list(
        credentials = fixture$credentials,
        passphrase_env = "CEREBRO_AUTH_TEST_KEY"
      ),
      launch_browser = FALSE,
      verbose = FALSE
    ),
    "database and passphrase do not match"
  )
  expect_identical(compile_calls, 1L)
  expect_identical(readLines(marker), "KEEP")
  expect_setequal(list.files(app), "marker.txt")
})
