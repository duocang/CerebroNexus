viewer_auth_runtime_environment <- function() {
  runtime <- new.env(parent = globalenv())
  source_file <- file.path("inst", "viewer", "auth.R")
  if (!file.exists(source_file)) {
    source_file <- system.file(
      "viewer/auth.R",
      package = "CerebroNexus"
    )
  }
  sys.source(source_file, envir = runtime)
  runtime
}

viewer_auth_runtime_config <- function() {
  list(
    credentials_path = "private-data/auth/credentials.sqlite",
    passphrase_env = "CEREBRO_AUTH_TEST_KEY",
    timeout_minutes = 15L
  )
}

test_that("Viewer authentication runtime is a no-op when disabled", {
  runtime <- viewer_auth_runtime_environment()
  ui <- shiny::fluidPage("viewer")
  server <- function(input, output, session) NULL

  app <- runtime$viewer_auth_apply(ui, server, NULL, ".")

  expect_identical(app$ui, ui)
  expect_identical(app$server, server)
})

test_that("Viewer authentication runtime requires its deployed secret", {
  runtime <- viewer_auth_runtime_environment()
  root <- withr::local_tempdir()
  database <- file.path(root, "private-data", "auth", "credentials.sqlite")
  dir.create(dirname(database), recursive = TRUE)
  writeBin(as.raw(c(0x53, 0x51, 0x4c)), database)
  withr::local_envvar(CEREBRO_AUTH_TEST_KEY = NA)

  expect_error(
    runtime$viewer_auth_apply(
      shiny::fluidPage("viewer"),
      function(input, output, session) NULL,
      viewer_auth_runtime_config(),
      root
    ),
    "CEREBRO_AUTH_TEST_KEY is not set"
  )
})

test_that("Viewer server starts once after authentication", {
  skip_if_not_installed("shinymanager", minimum_version = "1.1.0")
  runtime <- viewer_auth_runtime_environment()
  root <- withr::local_tempdir()
  database <- file.path(root, "private-data", "auth", "credentials.sqlite")
  dir.create(dirname(database), recursive = TRUE)
  writeBin(as.raw(c(0x53, 0x51, 0x4c)), database)
  passphrase <- "runtime test passphrase"
  withr::local_envvar(CEREBRO_AUTH_TEST_KEY = passphrase)
  auth_state <- shiny::reactiveValues(user = NULL)
  checker_calls <- 0L
  testthat::local_mocked_bindings(
    check_credentials = function(db, passphrase) {
      checker_calls <<- checker_calls + 1L
      expect_identical(
        db,
        normalizePath(database, winslash = "/", mustWork = TRUE)
      )
      expect_identical(passphrase, "runtime test passphrase")
      function(user, password) list(result = TRUE)
    },
    secure_app = function(ui, ...) structure(ui, viewer_auth_secured = TRUE),
    secure_server = function(...) auth_state,
    .package = "shinymanager"
  )
  starts <- 0L
  viewer_server <- function(input, output, session) {
    starts <<- starts + 1L
  }

  app <- runtime$viewer_auth_apply(
    shiny::fluidPage("viewer"),
    viewer_server,
    viewer_auth_runtime_config(),
    root
  )

  expect_identical(attr(app$ui, "viewer_auth_secured"), TRUE)
  expect_identical(checker_calls, 1L)
  shiny::testServer(app$server, {
    session$flushReact()
    expect_identical(starts, 0L)
    auth_state$user <- "alice"
    session$flushReact()
    expect_identical(starts, 1L)
    auth_state$user <- "bob"
    session$flushReact()
    expect_identical(starts, 1L)
  })
})
