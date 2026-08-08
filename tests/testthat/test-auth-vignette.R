auth_vignette_path <- function() {
  source_path <- testthat::test_path(
    "..",
    "..",
    "vignettes",
    "control_access_to_cerebro_with_a_login_page.Rmd"
  )
  if (file.exists(source_path)) {
    return(source_path)
  }

  installed_path <- system.file(
    "doc",
    "control_access_to_cerebro_with_a_login_page.Rmd",
    package = "CerebroNexus"
  )
  if (!nzchar(installed_path) || !file.exists(installed_path)) {
    stop("Cannot locate the authentication guide.", call. = FALSE)
  }
  installed_path
}

auth_vignette_chunk <- function(lines, label) {
  header <- grep(
    paste0("^```\\{[[:alnum:]_+-]+[[:space:]]+", label, "([,}])"),
    lines
  )
  if (length(header) != 1L) {
    stop("Expected exactly one '", label, "' chunk.", call. = FALSE)
  }
  remainder <- lines[seq.int(header + 1L, length(lines))]
  closing <- which(trimws(remainder) == "```")
  if (!length(closing)) {
    stop("Chunk '", label, "' is not closed.", call. = FALSE)
  }
  list(
    header = lines[[header]],
    code = paste(remainder[seq_len(closing[[1L]] - 1L)], collapse = "\n")
  )
}

vignette_auth_ops <- function() {
  reads <- c("alice", "y", "bob", "n")
  passwords <- list(
    "alice pass 47",
    "alice pass 47",
    "bob pass 83",
    "bob pass 83"
  )
  utils::modifyList(
    CerebroNexus:::.viewerAuthSetupOps(),
    list(
      is_interactive = function() TRUE,
      read_input = function(prompt) {
        value <- reads[[1L]]
        reads <<- reads[-1L]
        value
      },
      read_password = function(prompt) {
        value <- passwords[[1L]]
        passwords <<- passwords[-1L]
        value
      },
      random_bytes = function(size) as.raw(seq_len(size))
    )
  )
}

test_that("authentication guide quick start is exact and executable", {
  skip_if_not_installed("shinymanager", minimum_version = "1.1.0")
  skip_if_not_installed("askpass")
  skip_if_not_installed("openssl")
  skip_if_not_installed("callr")
  skip_if_not_installed("httpuv")

  env_name <- "CEREBRO_AUTH_PASSPHRASE_0102030405060708"
  withr::local_envvar(stats::setNames(NA_character_, env_name))

  lines <- readLines(auth_vignette_path(), warn = FALSE)
  chunk <- auth_vignette_chunk(lines, "interactive-quick-start")
  expected <- paste(
    c(
      "example_crb <- system.file(",
      "  \"extdata/examples/example.crb\",",
      "  package = \"CerebroNexus\",",
      "  mustWork = TRUE",
      ")",
      "CerebroNexus::createShinyApp(",
      "  cerebro_data = c(example = example_crb),",
      "  result_dir = \"my_app\",",
      "  auth = TRUE",
      ")"
    ),
    collapse = "\n"
  )
  expect_identical(chunk$code, expected)
  expect_match(chunk$header, "eval=FALSE", fixed = TRUE)
  expect_match(chunk$header, "purl=FALSE", fixed = TRUE)

  root <- withr::local_tempdir()
  withr::local_dir(root)
  ops <- vignette_auth_ops()
  value <- NULL
  invisible(capture.output(
    value <- testthat::with_mocked_bindings(
      eval(parse(text = chunk$code), envir = new.env(parent = globalenv())),
      .viewerAuthSetupOps = function() ops,
      .package = "CerebroNexus"
    )
  ))
  app_dir <- file.path(root, "my_app")
  secret_path <- paste0(app_dir, ".auth.env")
  config <- readRDS(file.path(app_dir, "cerebro_config.rds"))$.viewer_auth
  database <- file.path(app_dir, config$credentials_path)
  expect_identical(value, "my_app")
  expect_true(file.exists(secret_path))
  expect_true(file.exists(database))

  secret <- CerebroNexus:::.viewerAuthReadSecretFile(secret_path)
  token <- getFromNamespace(".tok", "shinymanager")
  old_path <- token$get_sqlite_path()
  old_passphrase <- token$get_passphrase()
  withr::defer({
    token$set_sqlite_path(old_path)
    token$set_passphrase(old_passphrase)
  })
  checker <- shinymanager::check_credentials(
    db = database,
    passphrase = secret$passphrase
  )
  expect_true(isTRUE(checker("alice", "alice pass 47")$result))
  expect_true(isTRUE(checker("bob", "bob pass 83")$result))

  runtime <- callr::r(
    function(app_dir, secret_path, env_name) {
      Sys.unsetenv(env_name)
      readRenviron(secret_path)
      setwd(app_dir)
      app <- source("app.R", local = new.env(parent = globalenv()))$value
      later::later(shiny::stopApp, delay = 0.1)
      shiny::runApp(
        app,
        host = "127.0.0.1",
        port = httpuv::randomPort(),
        launch.browser = FALSE,
        quiet = TRUE
      )
      list(
        is_app = inherits(app, "shiny.appobj"),
        secret_loaded = nzchar(Sys.getenv(env_name))
      )
    },
    args = list(
      app_dir = app_dir,
      secret_path = secret_path,
      env_name = secret$env_name
    ),
    timeout = 30
  )
  expect_true(runtime$is_app)
  expect_true(runtime$secret_loaded)
})

test_that("authentication guide covers local and remote deployment paths", {
  lines <- readLines(auth_vignette_path(), warn = FALSE)
  labels <- c(
    "interactive-quick-start",
    "local-run-now",
    "local-run-later",
    "own-crb",
    "advanced-descriptor",
    "shiny-server-systemd",
    "docker-run",
    "docker-compose"
  )
  chunks <- stats::setNames(
    lapply(labels, function(label) auth_vignette_chunk(lines, label)),
    labels
  )
  for (label in labels) {
    expect_true(nzchar(chunks[[label]]$code), info = label)
  }

  text <- paste(lines, collapse = "\n")
  expect_match(text, "private-data/auth/credentials.sqlite", fixed = TRUE)
  expect_match(text, "my_app.auth.env", fixed = TRUE)
  expect_match(text, "readRenviron", fixed = TRUE)
  expect_match(text, "another machine", ignore.case = TRUE)
  expect_match(text, "TLS", fixed = TRUE)
  expect_match(text, "rate limit", ignore.case = TRUE)
  expect_match(text, "backup", ignore.case = TRUE)
  expect_match(text, "SSO", fixed = TRUE)

  expect_match(chunks$`local-run-now`$code, "shiny::runApp", fixed = TRUE)
  expect_match(chunks$`local-run-later`$code, "readRenviron", fixed = TRUE)
  expect_match(chunks$`own-crb`$code, "auth = TRUE", fixed = TRUE)
  expect_match(
    chunks$`advanced-descriptor`$code,
    "provider = \"shinymanager\"",
    fixed = TRUE
  )
})

test_that("documented shell and container configurations are safe to copy", {
  skip_on_os("windows")
  lines <- readLines(auth_vignette_path(), warn = FALSE)
  shell_labels <- c(
    "copy-to-another-machine",
    "shiny-server-systemd",
    "docker-run",
    "docker-compose",
    "prepare-key-rotation"
  )
  for (label in shell_labels) {
    chunk <- auth_vignette_chunk(lines, label)
    script <- withr::local_tempfile(fileext = ".sh")
    writeLines(chunk$code, script)
    output <- system2(
      "bash",
      c("-n", shQuote(script)),
      stdout = TRUE,
      stderr = TRUE
    )
    status <- attr(output, "status")
    expect_true(is.null(status) || identical(status, 0L), info = label)
  }

  docker <- auth_vignette_chunk(lines, "docker-run")$code
  compose <- auth_vignette_chunk(lines, "docker-compose")$code
  expect_match(docker, "--env-file", fixed = TRUE)
  expect_match(compose, "env_file:", fixed = TRUE)
  expect_match(compose, "my_app.auth.env", fixed = TRUE)
  expect_false(any(grepl(
    "^COPY[[:space:]]+.*\\.auth\\.env",
    lines
  )))

  rotation <- auth_vignette_chunk(lines, "prepare-key-rotation")$code
  rotation_root <- withr::local_tempdir()
  dir.create(file.path(rotation_root, "my_app"))
  writeLines("old app", file.path(rotation_root, "my_app", "marker"))
  secret <- file.path(rotation_root, "my_app.auth.env")
  writeLines("CEREBRO_AUTH_PASSPHRASE_TEST=value", secret)
  Sys.chmod(secret, "0600", use_umask = FALSE)
  rotation_script <- withr::local_tempfile(fileext = ".sh")
  writeLines(rotation, rotation_script)
  withr::local_dir(rotation_root)
  rotation_backups <- function() {
    paths <- list.dirs(rotation_root, recursive = FALSE, full.names = TRUE)
    paths[grepl("^my_app-key-rotation\\.", basename(paths))]
  }

  first <- system2(
    "bash",
    shQuote(rotation_script),
    stdout = TRUE,
    stderr = TRUE
  )
  first_status <- attr(first, "status")
  expect_true(is.null(first_status) || identical(first_status, 0L))
  backups <- rotation_backups()
  expect_length(backups, 1L)
  expect_identical(
    readLines(file.path(backups, "my_app", "marker")),
    "old app"
  )
  expect_identical(
    readLines(file.path(backups, "my_app.auth.env")),
    "CEREBRO_AUTH_PASSPHRASE_TEST=value"
  )
  expect_false(file.exists(secret))

  second <- suppressWarnings(system2(
    "bash",
    shQuote(rotation_script),
    stdout = TRUE,
    stderr = TRUE
  ))
  expect_identical(attr(second, "status"), 1L)
  expect_identical(rotation_backups(), backups)
  expect_identical(
    readLines(file.path(backups, "my_app.auth.env")),
    "CEREBRO_AUTH_PASSPHRASE_TEST=value"
  )
})

test_that("authentication guide renders", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "Pandoc is not available")
  output_dir <- withr::local_tempdir()
  output <- rmarkdown::render(
    auth_vignette_path(),
    output_dir = output_dir,
    envir = new.env(parent = globalenv()),
    quiet = TRUE,
    clean = TRUE
  )
  expect_true(file.exists(output))
})
