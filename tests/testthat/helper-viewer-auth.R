viewer_auth_fixture <- function(envir = parent.frame()) {
  testthat::skip_if_not_installed(
    "shinymanager",
    minimum_version = "1.1.0"
  )
  root <- withr::local_tempdir(.local_envir = envir)
  database <- file.path(root, "credentials.sqlite")
  env_name <- "CEREBRO_TEST_AUTH_PASSPHRASE"
  passphrase <- paste0(
    "test-secret-",
    strrep("x", 32L),
    basename(root)
  )
  withr::local_envvar(
    stats::setNames(passphrase, env_name),
    .local_envir = envir
  )
  suppressMessages(shinymanager::create_db(
    credentials_data = data.frame(
      user = c("viewer", "reviewer"),
      password = c("correct horse 47", "review only 83"),
      admin = c(FALSE, TRUE),
      stringsAsFactors = FALSE
    ),
    sqlite_path = database,
    passphrase = passphrase
  ))
  database <- normalizePath(database, winslash = "/", mustWork = TRUE)

  list(
    root = root,
    database = database,
    env_name = env_name,
    passphrase = passphrase,
    descriptor = list(
      provider = "shinymanager",
      credentials = database,
      passphrase_env = env_name,
      timeout_minutes = 15
    )
  )
}

viewer_auth_test_crb <- function(root) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(root, "dataset.crb")
  saveRDS(Cerebro_v1.3$new(), path)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}
