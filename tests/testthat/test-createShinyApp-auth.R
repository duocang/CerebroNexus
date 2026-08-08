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
