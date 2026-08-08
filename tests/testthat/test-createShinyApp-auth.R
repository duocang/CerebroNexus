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

test_that("database preflight decrypts without creating a credential checker", {
  skip_if_not_installed("shinymanager", minimum_version = "1.1.0")
  path <- auth_test_database()
  credentials <- data.frame(
    user = "alice",
    password = "hashed-password",
    is_hashed_password = TRUE,
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    read_db_decrypt = function(conn, name, passphrase) {
      expect_identical(conn, path)
      expect_identical(name, "credentials")
      expect_identical(passphrase, "test database passphrase")
      credentials
    },
    check_credentials = function(...) {
      stop("preflight must not create a credential checker", call. = FALSE)
    },
    .package = "shinymanager"
  )

  expect_invisible(CerebroNexus:::.viewerAuthValidateDatabase(
    path,
    "test database passphrase"
  ))
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
  app_source <- readLines(file.path(app, "app.R"), warn = FALSE)
  expect_true(any(grepl("viewer/auth.R", app_source, fixed = TRUE)))
  expect_true(any(grepl("viewer_auth_apply", app_source, fixed = TRUE)))
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

test_that("createShinyApp makes the bundled database writable at runtime", {
  skip_on_os("windows")
  fixture <- auth_test_build_fixture()
  Sys.chmod(fixture$credentials, mode = "0400")
  app <- file.path(fixture$root, "writable-auth-app")
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
  expect_identical(unname(file.access(bundled, mode = 6L)), 0L)
  expect_identical(unname(file.access(dirname(bundled), mode = 3L)), 0L)
  expect_identical(as.integer(file.info(bundled)$mode), 384L)
  expect_identical(as.integer(file.info(dirname(bundled))$mode), 448L)
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

test_that("a real encrypted multi-user database survives app generation", {
  skip_if_not_installed("shinymanager", minimum_version = "1.1.0")
  fixture <- auth_test_build_fixture()
  database <- file.path(fixture$root, "real-credentials.sqlite")
  passphrase <- "independent database passphrase"
  withr::local_envvar(CEREBRO_AUTH_TEST_KEY = passphrase)
  shinymanager::create_db(
    credentials_data = data.frame(
      user = c("alice", "bob"),
      password = c("alice-login-password", "bob-login-password"),
      stringsAsFactors = FALSE
    ),
    sqlite_path = database,
    passphrase = passphrase
  )
  app <- file.path(fixture$root, "real-auth-app")

  createShinyApp(
    cerebro_data = c(Dataset = fixture$crb),
    result_dir = app,
    auth = list(
      credentials = database,
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
  checker <- shinymanager::check_credentials(
    db = bundled,
    passphrase = passphrase
  )
  expect_true(checker("alice", "alice-login-password")$result)
  expect_true(checker("bob", "bob-login-password")$result)
  expect_false(checker("alice", "wrong-password")$result)
  artifacts <- list.files(app, recursive = TRUE, full.names = TRUE)
  for (secret in c(
    passphrase,
    "alice-login-password",
    "bob-login-password"
  )) {
    expect_false(any(vapply(
      artifacts,
      auth_test_file_contains,
      logical(1),
      value = secret
    )))
  }
})

auth_test_package_file <- function(path) {
  if (file.exists(path)) {
    return(path)
  }
  file.path("..", "..", path)
}

test_that("authentication deployment documentation is runnable and credited", {
  vignette_path <- auth_test_package_file(file.path(
    "vignettes",
    "control_access_to_cerebro_with_a_login_page.Rmd"
  ))
  skip_if_not(
    file.exists(vignette_path),
    "static source-tree documentation contract"
  )
  vignette <- readLines(vignette_path, warn = FALSE)
  required <- c(
    "Roman Hillje",
    "Xuesong Wang",
    "shinymanager::create_db",
    "createShinyApp(",
    "readRenviron(",
    "Shiny Server",
    "systemd",
    "Docker Compose",
    "another machine"
  )
  for (term in required) {
    expect_true(any(grepl(term, vignette, fixed = TRUE)), info = term)
  }
  expect_true(any(grepl('Sys.umask("077")', vignette, fixed = TRUE)))
  expect_false(any(grepl(
    "file.create(secret_file, mode",
    vignette,
    fixed = TRUE
  )))

  description <- read.dcf(auth_test_package_file("DESCRIPTION"))
  expect_identical(description[[1L, "Version"]], "4.1")
  suggests <- strsplit(description[[1L, "Suggests"]], ",")[[1L]]
  expect_true(any(grepl("shinymanager", suggests, fixed = TRUE)))
  expect_false(any(grepl("askpass|chromote", suggests)))

  readme <- readLines(auth_test_package_file("README.md"), warn = FALSE)
  news <- readLines(auth_test_package_file("NEWS.md"), warn = FALSE)
  expect_true(any(grepl("auth = list", readme, fixed = TRUE)))
  expect_true(any(grepl("encrypted shinymanager", news, fixed = TRUE)))
})
