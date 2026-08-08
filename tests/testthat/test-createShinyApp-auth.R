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

default_viewer_auth_setup_ops <- .viewerAuthSetupOps
default_viewer_auth_create_database <- default_viewer_auth_setup_ops()$create_db
default_viewer_auth_validate_database <- .viewerAuthValidateDatabase
default_viewer_auth_provider_available <- .viewerAuthProviderAvailable
simple_auth_environment <- "CEREBRO_AUTH_PASSPHRASE_0102030405060708"

mock_viewer_auth_provider_available <- function() invisible(TRUE)

mock_viewer_auth_validate_database <- function(...) invisible(TRUE)

mock_viewer_auth_create_database <- function(
  credentials_data,
  sqlite_path,
  passphrase
) {
  writeBin(charToRaw("mock authentication database"), sqlite_path)
  TRUE
}

auth_test_setup_ops <- function(...) {
  utils::modifyList(
    utils::modifyList(
      default_viewer_auth_setup_ops(),
      list(
        namespace_available = function(package) TRUE,
        create_db = mock_viewer_auth_create_database
      )
    ),
    list(...)
  )
}

foreign_auth_secret <- function() {
  charToRaw(paste0(
    "CEREBRO_AUTH_PASSPHRASE_FFFFFFFFFFFFFFFF=",
    strrep("b", 64L),
    "\n"
  ))
}

local_simple_auth_environment <- function(envir = parent.frame()) {
  withr::local_envvar(
    stats::setNames(NA_character_, simple_auth_environment),
    .local_envir = envir
  )
}

simple_auth_ops <- function(
  read_values = c("alice", "y", "bob", "n"),
  password_values = list(
    "alice pass 47",
    "alice pass 47",
    "bob pass 83",
    "bob pass 83"
  ),
  ...
) {
  reads <- read_values
  passwords <- password_values
  auth_test_setup_ops(
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
    random_bytes = function(size) as.raw(seq_len(size)),
    ...
  )
}

real_simple_auth_ops <- function(...) {
  simple_auth_ops(
    ...,
    create_db = default_viewer_auth_create_database
  )
}

single_user_auth_ops <- function(
  user = "alice",
  password = "alice pass 47",
  ...
) {
  simple_auth_ops(
    read_values = c(user, "n"),
    password_values = list(password, password),
    ...
  )
}

real_single_user_auth_ops <- function(
  user = "alice",
  password = "alice pass 47",
  ...
) {
  real_simple_auth_ops(
    read_values = c(user, "n"),
    password_values = list(password, password),
    ...
  )
}

simple_auth_case <- function(root, old_app = TRUE) {
  result <- file.path(root, "app")
  old_mode <- NULL
  if (isTRUE(old_app)) {
    dir.create(result)
    writeLines("OLD", file.path(result, "marker.txt"))
    old_mode <- file.info(result)$mode[[1L]]
  }
  list(
    result = result,
    crb = viewer_auth_test_crb(file.path(root, "source")),
    old_mode = old_mode
  )
}

logical_viewer_auth_fixture <- function(envir = parent.frame()) {
  testthat::local_mocked_bindings(
    .viewerAuthProviderAvailable = mock_viewer_auth_provider_available,
    .viewerAuthValidateDatabase = mock_viewer_auth_validate_database,
    .package = "CerebroNexus",
    .env = envir
  )
  root <- withr::local_tempdir(.local_envir = envir)
  database <- file.path(root, "credentials.sqlite")
  writeBin(charToRaw("logical authentication database"), database)
  database <- normalizePath(database, winslash = "/", mustWork = TRUE)
  env_name <- "CEREBRO_TEST_AUTH_PASSPHRASE"
  passphrase <- paste0("logical-secret-", strrep("x", 32L))
  withr::local_envvar(
    stats::setNames(passphrase, env_name),
    .local_envir = envir
  )

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

run_simple_auth_build <- function(
  crb,
  result,
  ops,
  overwrite = TRUE,
  verbose = FALSE,
  publication_ops = .bundlePublicationOps(),
  validate_database = mock_viewer_auth_validate_database,
  provider_available = mock_viewer_auth_provider_available,
  ...
) {
  force(ops)
  force(publication_ops)
  force(validate_database)
  force(provider_available)
  testthat::with_mocked_bindings(
    createShinyApp(
      cerebro_data = c(Dataset = crb),
      result_dir = result,
      overwrite = overwrite,
      auth = TRUE,
      launch_browser = FALSE,
      verbose = verbose,
      ...
    ),
    .viewerAuthSetupOps = function() ops,
    .viewerAuthProviderAvailable = provider_available,
    .bundlePublicationOps = function() publication_ops,
    .viewerAuthValidateDatabase = validate_database,
    .package = "CerebroNexus"
  )
}

simple_auth_transaction_artifacts <- function(root) {
  c(
    auth_bundle_artifacts(root),
    list.files(
      root,
      pattern = "^\\.app\\.auth\\.env-(candidate|scratch)-",
      all.files = TRUE,
      full.names = TRUE
    )
  )
}

expect_simple_auth_rollback <- function(root, result, old_mode) {
  expect_auth_bundle_rollback(root, result, old_mode)
  expect_false(.bundlePathExists(paste0(result, ".auth.env")))
  expect_true(is.na(Sys.getenv(
    simple_auth_environment,
    unset = NA_character_
  )))
  expect_length(simple_auth_transaction_artifacts(root), 0L)
}

test_that("auth TRUE rejects non-interactive calls before target preparation", {
  root <- withr::local_tempdir()
  crb <- viewer_auth_test_crb(file.path(root, "source"))
  prepare_calls <- 0L
  ops <- utils::modifyList(
    .viewerAuthSetupOps(),
    list(is_interactive = function() FALSE)
  )
  testthat::local_mocked_bindings(
    .viewerAuthSetupOps = function() ops,
    .prepareBundleResultTarget = function(result_dir) {
      prepare_calls <<- prepare_calls + 1L
      stop("target preparation reached", call. = FALSE)
    },
    .package = "CerebroNexus"
  )

  expect_error(
    createShinyApp(
      cerebro_data = c(Dataset = crb),
      result_dir = file.path(root, "app"),
      auth = TRUE,
      launch_browser = FALSE,
      verbose = FALSE
    ),
    "requires an interactive R session",
    fixed = TRUE
  )
  expect_identical(prepare_calls, 0L)
})

test_that("auth TRUE creates a real two-user app and sibling secret", {
  skip_if_not_installed("shinymanager", minimum_version = "1.1.0")

  local_simple_auth_environment()
  root <- withr::local_tempdir()
  result <- file.path(root, "generated", "app")
  crb <- viewer_auth_test_crb(file.path(root, "source"))

  expect_identical(
    run_simple_auth_build(
      crb,
      result,
      real_simple_auth_ops(),
      validate_database = default_viewer_auth_validate_database,
      provider_available = default_viewer_auth_provider_available
    ),
    result
  )

  secret <- paste0(result, ".auth.env")
  config <- readRDS(file.path(result, "cerebro_config.rds"))$.viewer_auth
  database <- file.path(result, config$credentials_path)
  expect_true(file.exists(secret))
  expect_identical(config$passphrase_env, simple_auth_environment)
  expect_identical(
    config$credentials_path,
    "private-data/auth/credentials.sqlite"
  )
  expect_identical(config$timeout_minutes, 15L)
  expect_false("passphrase" %in% names(config))
  expect_true(nzchar(Sys.getenv(config$passphrase_env)))
  expect_false(any(vapply(
    paste0(database, c("-journal", "-wal", "-shm")),
    .bundlePathExists,
    logical(1)
  )))

  token <- getFromNamespace(".tok", "shinymanager")
  old_path <- token$get_sqlite_path()
  old_passphrase <- token$get_passphrase()
  withr::defer({
    token$set_sqlite_path(old_path)
    token$set_passphrase(old_passphrase)
  })
  checker <- shinymanager::check_credentials(
    db = database,
    passphrase = Sys.getenv(config$passphrase_env)
  )
  expect_true(isTRUE(checker("alice", "alice pass 47")$result))
  expect_true(isTRUE(checker("bob", "bob pass 83")$result))

  visible <- c(
    readLines(file.path(result, "app.R"), warn = FALSE),
    capture.output(str(readRDS(file.path(result, "cerebro_config.rds"))))
  )
  expect_false(any(grepl(
    "alice|bob|alice pass 47|bob pass 83",
    visible
  )))
  expect_false(any(grepl(
    Sys.getenv(config$passphrase_env),
    visible,
    fixed = TRUE
  )))

  if (.Platform$OS.type != "windows") {
    expected_uid <- as.numeric(system2("id", "-u", stdout = TRUE))
    auth_dir <- dirname(database)
    expect_identical(
      as.integer(file.info(secret)$mode[[1L]]),
      strtoi("600", 8L)
    )
    expect_identical(
      as.integer(file.info(database)$mode[[1L]]),
      strtoi("600", 8L)
    )
    expect_identical(
      as.integer(file.info(auth_dir)$mode[[1L]]),
      strtoi("700", 8L)
    )
    expect_identical(as.numeric(file.info(secret)$uid[[1L]]), expected_uid)
    expect_identical(as.numeric(file.info(database)$uid[[1L]]), expected_uid)
    expect_identical(as.numeric(file.info(auth_dir)$uid[[1L]]), expected_uid)
  }
})

test_that("late ordinary validation errors occur before account prompts", {
  root <- withr::local_tempdir()
  crb <- viewer_auth_test_crb(file.path(root, "source"))
  prompts <- 0L
  ops <- auth_test_setup_ops(
    is_interactive = function() TRUE,
    read_input = function(prompt) {
      prompts <<- prompts + 1L
      stop("unexpected prompt", call. = FALSE)
    }
  )
  expect_error(
    run_simple_auth_build(
      crb,
      file.path(root, "new-parent", "app"),
      ops,
      colors = list("#ffffff")
    ),
    "colors must be a named list",
    fixed = TRUE
  )
  expect_identical(prompts, 0L)
  expect_false(dir.exists(file.path(root, "new-parent")))
})

test_that("simple auth dependencies and FALSE fail before prompt or mutation", {
  root <- withr::local_tempdir()
  crb <- viewer_auth_test_crb(file.path(root, "source"))
  for (missing in c("shinymanager", "askpass", "openssl")) {
    prompts <- 0L
    prepare_calls <- 0L
    result <- file.path(root, paste0("missing-", missing), "app")
    ops <- utils::modifyList(
      default_viewer_auth_setup_ops(),
      list(
        is_interactive = function() TRUE,
        namespace_available = function(package) !identical(package, missing),
        read_input = function(prompt) {
          prompts <<- prompts + 1L
          stop("unexpected prompt", call. = FALSE)
        }
      )
    )
    expect_error(
      testthat::with_mocked_bindings(
        createShinyApp(
          cerebro_data = c(Dataset = crb),
          result_dir = result,
          auth = TRUE,
          launch_browser = FALSE,
          verbose = FALSE
        ),
        .viewerAuthSetupOps = function() ops,
        .prepareBundleResultTarget = function(result_dir) {
          prepare_calls <<- prepare_calls + 1L
          stop("unexpected target preparation", call. = FALSE)
        },
        .package = "CerebroNexus"
      ),
      missing,
      fixed = TRUE
    )
    expect_identical(prompts, 0L)
    expect_identical(prepare_calls, 0L)
    expect_false(dir.exists(dirname(result)))
  }

  prepare_calls <- 0L
  expect_error(
    testthat::with_mocked_bindings(
      createShinyApp(
        cerebro_data = c(Dataset = crb),
        result_dir = file.path(root, "false-app"),
        auth = FALSE,
        launch_browser = FALSE,
        verbose = FALSE
      ),
      .prepareBundleResultTarget = function(result_dir) {
        prepare_calls <<- prepare_calls + 1L
        stop("unexpected target preparation", call. = FALSE)
      },
      .package = "CerebroNexus"
    ),
    "auth must be a named list",
    fixed = TRUE
  )
  expect_identical(prepare_calls, 0L)
})

test_that("prompt cancellation leaves app secret and environment unchanged", {
  local_simple_auth_environment()
  root <- withr::local_tempdir()
  crb <- viewer_auth_test_crb(file.path(root, "source"))
  cases <- list(
    list(reads = "", passwords = list()),
    list(reads = "alice", passwords = list(NULL)),
    list(reads = c("alice", "y", ""), passwords = list("a", "a"))
  )
  for (index in seq_along(cases)) {
    result <- file.path(root, paste0("cancel-", index), "app")
    ops <- simple_auth_ops(cases[[index]]$reads, cases[[index]]$passwords)
    expect_error(
      run_simple_auth_build(crb, result, ops),
      "cancelled",
      fixed = TRUE
    )
    expect_false(dir.exists(dirname(result)))
    expect_false(.bundlePathExists(paste0(result, ".auth.env")))
    expect_true(is.na(Sys.getenv(
      simple_auth_environment,
      unset = NA_character_
    )))
  }
})

test_that("destination and resource errors occur before account prompts", {
  root <- withr::local_tempdir()
  crb <- viewer_auth_test_crb(file.path(root, "source"))
  file_target <- file.path(root, "file-app")
  writeLines("not a directory", file_target)
  nonempty_target <- file.path(root, "nonempty-app")
  dir.create(nonempty_target)
  writeLines("old", file.path(nonempty_target, "marker"))
  resource_root <- file.path(root, "public")
  dir.create(resource_root)
  shiny::addResourcePath("auth-simple-public", resource_root)
  withr::defer(shiny::removeResourcePath("auth-simple-public"))
  cases <- list(
    list(result = file_target, overwrite = TRUE, error = "not a directory"),
    list(
      result = nonempty_target,
      overwrite = FALSE,
      error = "overwrite = FALSE"
    ),
    list(
      result = file.path(resource_root, "app"),
      overwrite = TRUE,
      error = "HTTP resource"
    )
  )

  for (case in cases) {
    prompts <- 0L
    ops <- auth_test_setup_ops(
      is_interactive = function() TRUE,
      read_input = function(prompt) {
        prompts <<- prompts + 1L
        stop("unexpected prompt", call. = FALSE)
      }
    )
    expect_error(
      run_simple_auth_build(
        crb,
        case$result,
        ops,
        overwrite = case$overwrite
      ),
      case$error,
      fixed = TRUE
    )
    expect_identical(prompts, 0L)
    expect_false(.bundlePathExists(paste0(case$result, ".auth.env")))
  }
  expect_identical(readLines(file_target), "not a directory")
  expect_identical(readLines(file.path(nonempty_target, "marker")), "old")
})

test_that("simple auth rebuild reuses the secret and replaces accounts", {
  skip_if_not_installed("shinymanager", minimum_version = "1.1.0")

  local_simple_auth_environment()
  root <- withr::local_tempdir()
  result <- file.path(root, "app")
  crb <- viewer_auth_test_crb(file.path(root, "source"))
  build <- function(ops) {
    run_simple_auth_build(
      crb,
      result,
      ops,
      validate_database = default_viewer_auth_validate_database,
      provider_available = default_viewer_auth_provider_available
    )
  }
  build(real_single_user_auth_ops())
  secret <- paste0(result, ".auth.env")
  before <- .viewerAuthReadSecretFile(secret)
  prior_value <- Sys.getenv(before$env_name)

  build(real_single_user_auth_ops("charlie", "charlie pass 91"))
  after <- .viewerAuthReadSecretFile(secret)
  expect_true(.viewerAuthSameArtifact(before, after))
  expect_identical(Sys.getenv(after$env_name), prior_value)

  database <- file.path(result, "private-data", "auth", "credentials.sqlite")
  token <- getFromNamespace(".tok", "shinymanager")
  old_path <- token$get_sqlite_path()
  old_passphrase <- token$get_passphrase()
  withr::defer({
    token$set_sqlite_path(old_path)
    token$set_passphrase(old_passphrase)
  })
  checker <- shinymanager::check_credentials(
    db = database,
    passphrase = prior_value
  )
  expect_false(isTRUE(checker("alice", "alice pass 47")$result))
  expect_true(isTRUE(checker("charlie", "charlie pass 91")$result))
})

test_that("existing secret replacement aborts a rebuild without old-app loss", {
  local_simple_auth_environment()
  root <- withr::local_tempdir()
  result <- file.path(root, "app")
  crb <- viewer_auth_test_crb(file.path(root, "source"))
  run_simple_auth_build(crb, result, single_user_auth_ops())
  writeLines("OLD", file.path(result, "marker.txt"))
  secret <- paste0(result, ".auth.env")
  original_secret <- .viewerAuthReadSecretFile(secret)
  original_env <- Sys.getenv(original_secret$env_name)
  replacement <- charToRaw(paste0(
    "CEREBRO_AUTH_PASSPHRASE_FFFFFFFFFFFFFFFF=",
    strrep("b", 64L),
    "\n"
  ))
  second_ops <- single_user_auth_ops("charlie", "charlie pass 91")
  original_create <- second_ops$create_db
  second_ops$create_db <- function(credentials_data, sqlite_path, passphrase) {
    created <- original_create(credentials_data, sqlite_path, passphrase)
    unlink(secret)
    writeBin(replacement, secret, useBytes = TRUE)
    Sys.chmod(secret, "0600", use_umask = FALSE)
    created
  }

  expect_error(
    run_simple_auth_build(crb, result, second_ops),
    "changed",
    fixed = TRUE
  )
  expect_identical(readLines(file.path(result, "marker.txt")), "OLD")
  expect_identical(readBin(secret, "raw", n = length(replacement)), replacement)
  expect_identical(Sys.getenv(original_secret$env_name), original_env)
  expect_length(simple_auth_transaction_artifacts(root), 0L)
})

test_that("simple authentication prints runnable redacted instructions", {
  local_simple_auth_environment()
  root <- withr::local_tempdir()
  result <- file.path(root, "app")
  crb <- viewer_auth_test_crb(file.path(root, "source"))
  captured <- capture_auth_bundle_conditions(
    run_simple_auth_build(crb, result, simple_auth_ops(), verbose = TRUE)
  )
  expect_false(inherits(captured$value, "error"))
  text <- paste(captured$stdout, collapse = "\n")
  expect_match(text, "Authentication enabled for 2 users.", fixed = TRUE)
  normalized_result <- normalizePath(result, winslash = "/")
  expect_match(text, normalized_result, fixed = TRUE)
  expect_match(text, paste0(normalized_result, ".auth.env"), fixed = TRUE)
  expect_match(text, "readRenviron(", fixed = TRUE)
  expect_match(text, "shiny::runApp(", fixed = TRUE)
  expect_match(text, "Shiny Server or Docker", fixed = TRUE)
  all_channels <- unlist(captured, use.names = FALSE)
  expect_false(any(grepl(
    "alice|bob|alice pass|bob pass",
    all_channels
  )))
  expect_false(any(grepl(
    Sys.getenv(simple_auth_environment),
    all_channels,
    fixed = TRUE
  )))
})

test_that("database failures occur after the candidate and fully roll back", {
  local_simple_auth_environment()
  for (kind in c("create", "validate")) {
    root <- withr::local_tempdir()
    case <- simple_auth_case(root)
    ops <- single_user_auth_ops()
    candidate_seen <- FALSE
    ops$create_db <- function(credentials_data, sqlite_path, passphrase) {
      candidate_seen <<- any(grepl(
        "candidate",
        simple_auth_transaction_artifacts(root),
        fixed = TRUE
      ))
      if (identical(kind, "create")) {
        return(FALSE)
      }
      mock_viewer_auth_create_database(
        credentials_data,
        sqlite_path,
        passphrase
      )
    }
    validate_database <- if (identical(kind, "validate")) {
      function(...) stop("injected database validation failure")
    } else {
      mock_viewer_auth_validate_database
    }

    expected <- if (identical(kind, "create")) {
      "Failed to create the staged authentication database."
    } else {
      "Failed to validate the staged authentication database."
    }
    expect_error(
      run_simple_auth_build(
        case$crb,
        case$result,
        ops,
        validate_database = validate_database
      ),
      expected,
      fixed = TRUE
    )
    expect_true(candidate_seen)
    expect_simple_auth_rollback(root, case$result, case$old_mode)
  }
})

test_that("staged SQLite sidecars are deleted and deletion failures abort", {
  local_simple_auth_environment()
  for (unlink_fails in c(FALSE, TRUE)) {
    Sys.unsetenv(simple_auth_environment)
    root <- withr::local_tempdir()
    case <- simple_auth_case(root)
    ops <- single_user_auth_ops()
    original_unlink <- ops$unlink_file
    sidecar_unlinks <- 0L
    ops$unlink_file <- function(path) {
      if (endsWith(path, "-wal")) {
        sidecar_unlinks <<- sidecar_unlinks + 1L
        if (unlink_fails) {
          return(FALSE)
        }
      }
      original_unlink(path)
    }
    validate_with_sidecar <- function(path, passphrase, passphrase_env) {
      sidecar <- paste0(path, "-wal")
      writeBin(charToRaw("owned-sidecar"), sidecar, useBytes = TRUE)
      Sys.chmod(sidecar, "0600", use_umask = FALSE)
      invisible(TRUE)
    }

    build <- function() {
      run_simple_auth_build(
        case$crb,
        case$result,
        ops,
        validate_database = validate_with_sidecar
      )
    }
    if (unlink_fails) {
      expect_error(
        build(),
        "Failed to finalize the staged authentication database.",
        fixed = TRUE
      )
      expect_identical(
        readLines(file.path(case$result, "marker.txt")),
        "OLD"
      )
      expect_equal(
        as.integer(file.info(case$result)$mode[[1L]]),
        as.integer(case$old_mode)
      )
      expect_false(.bundlePathExists(paste0(case$result, ".auth.env")))
      expect_true(is.na(Sys.getenv(
        simple_auth_environment,
        unset = NA_character_
      )))
    } else {
      expect_identical(build(), case$result)
      expect_false(.bundlePathExists(file.path(
        case$result,
        "private-data",
        "auth",
        "credentials.sqlite-wal"
      )))
    }
    expect_identical(sidecar_unlinks, 1L)
    expect_length(simple_auth_transaction_artifacts(root), 0L)
  }
  Sys.unsetenv(simple_auth_environment)
})

test_that("a raced final secret link preserves the foreign entry and old app", {
  local_simple_auth_environment()
  root <- withr::local_tempdir()
  case <- simple_auth_case(root)
  secret <- paste0(case$result, ".auth.env")
  replacement <- foreign_auth_secret()
  ops <- single_user_auth_ops()
  original_link <- ops$link
  link_calls <- 0L
  ops$link <- function(from, to) {
    link_calls <<- link_calls + 1L
    if (identical(link_calls, 2L)) {
      writeBin(replacement, to, useBytes = TRUE)
      Sys.chmod(to, "0600", use_umask = FALSE)
      return(FALSE)
    }
    original_link(from, to)
  }

  expect_error(
    run_simple_auth_build(case$crb, case$result, ops),
    "without clobbering target",
    fixed = TRUE
  )
  expect_identical(link_calls, 2L)
  expect_auth_bundle_rollback(root, case$result, case$old_mode)
  expect_identical(readBin(secret, "raw", n = length(replacement)), replacement)
  expect_true(is.na(Sys.getenv(
    simple_auth_environment,
    unset = NA_character_
  )))
  expect_length(simple_auth_transaction_artifacts(root), 0L)
})

test_that("candidate unlink refusal aborts without publishing the app", {
  local_simple_auth_environment()
  root <- withr::local_tempdir()
  case <- simple_auth_case(root)
  ops <- single_user_auth_ops()
  original_unlink <- ops$unlink_file
  ops$unlink_file <- function(path) {
    if (grepl("-candidate-", path, fixed = TRUE)) {
      return(FALSE)
    }
    original_unlink(path)
  }

  captured <- capture_auth_bundle_conditions(
    run_simple_auth_build(case$crb, case$result, ops)
  )
  expect_s3_class(captured$value, "error")
  expect_match(
    conditionMessage(captured$value),
    "Failed to remove the authentication secret candidate.",
    fixed = TRUE
  )
  expect_auth_bundle_rollback(root, case$result, case$old_mode)
  expect_false(.bundlePathExists(paste0(case$result, ".auth.env")))
  expect_true(is.na(Sys.getenv(
    simple_auth_environment,
    unset = NA_character_
  )))
  residues <- simple_auth_transaction_artifacts(root)
  expect_length(residues, 1L)
  expect_match(basename(residues), "-candidate-", fixed = TRUE)
  expect_true(any(grepl(
    normalizePath(residues, winslash = "/"),
    captured$warnings,
    fixed = TRUE
  )))
  unlink(residues)
})

test_that("environment and app publication failures restore the old app", {
  local_simple_auth_environment()
  for (kind in c("environment", "publication")) {
    root <- withr::local_tempdir()
    case <- simple_auth_case(root)
    ops <- single_user_auth_ops()
    if (identical(kind, "environment")) {
      original_setenv <- ops$setenv
      ops$setenv <- function(name, value) {
        original_setenv(name, value)
        FALSE
      }
    }
    publication_ops <- .bundlePublicationOps()
    if (identical(kind, "publication")) {
      original_rename <- publication_ops$rename
      publication_ops$rename <- function(from, to) {
        if (grepl("-stage-", basename(from), fixed = TRUE)) {
          return(FALSE)
        }
        original_rename(from, to)
      }
    }

    expected <- if (identical(kind, "environment")) {
      "Failed to install authentication environment variable"
    } else {
      "Failed to publish the staged app bundle"
    }
    expect_error(
      run_simple_auth_build(
        case$crb,
        case$result,
        ops,
        publication_ops = publication_ops
      ),
      expected,
      fixed = TRUE
    )
    expect_simple_auth_rollback(root, case$result, case$old_mode)
  }
})

test_that("environment rollback preserves absent empty and valued states", {
  local_simple_auth_environment()
  prior_states <- list(
    absent = NA_character_,
    empty = "",
    value = "prior-auth-value"
  )
  for (prior in prior_states) {
    Sys.unsetenv(simple_auth_environment)
    root <- withr::local_tempdir()
    case <- simple_auth_case(root)
    ops <- single_user_auth_ops()
    original_create <- ops$create_db
    original_setenv <- ops$setenv
    generated_passphrase <- NULL
    ops$create_db <- function(credentials_data, sqlite_path, passphrase) {
      generated_passphrase <<- passphrase
      created <- original_create(credentials_data, sqlite_path, passphrase)
      if (!is.na(prior)) {
        original_setenv(simple_auth_environment, prior)
      }
      created
    }
    ops$setenv <- function(name, value) {
      installed <- original_setenv(name, value)
      if (identical(value, generated_passphrase)) FALSE else installed
    }

    expect_error(
      run_simple_auth_build(case$crb, case$result, ops),
      "Failed to install authentication environment variable",
      fixed = TRUE
    )
    expect_identical(
      Sys.getenv(simple_auth_environment, unset = NA_character_),
      prior
    )
    expect_auth_bundle_rollback(root, case$result, case$old_mode)
    expect_false(.bundlePathExists(paste0(case$result, ".auth.env")))
    expect_length(simple_auth_transaction_artifacts(root), 0L)
  }
  Sys.unsetenv(simple_auth_environment)
})

test_that("secret replacement before app commit is preserved and reported", {
  local_simple_auth_environment()
  root <- withr::local_tempdir()
  case <- simple_auth_case(root)
  secret <- paste0(case$result, ".auth.env")
  replacement <- foreign_auth_secret()
  ops <- single_user_auth_ops()
  original_setenv <- ops$setenv
  ops$setenv <- function(name, value) {
    installed <- original_setenv(name, value)
    unlink(secret)
    writeBin(replacement, secret, useBytes = TRUE)
    Sys.chmod(secret, "0600", use_umask = FALSE)
    installed
  }

  captured <- capture_auth_bundle_conditions(
    run_simple_auth_build(case$crb, case$result, ops)
  )
  expect_s3_class(captured$value, "error")
  expect_match(conditionMessage(captured$value), "changed", fixed = TRUE)
  expect_true(
    any(grepl(
      normalizePath(secret, winslash = "/"),
      captured$warnings,
      fixed = TRUE
    )),
    info = paste(captured$warnings, collapse = " | ")
  )
  expect_auth_bundle_rollback(root, case$result, case$old_mode)
  expect_identical(readBin(secret, "raw", n = length(replacement)), replacement)
  expect_true(is.na(Sys.getenv(
    simple_auth_environment,
    unset = NA_character_
  )))
  expect_length(auth_bundle_artifacts(root), 0L)
})

test_that("secret replacement during the app swap aborts and restores", {
  local_simple_auth_environment()
  root <- withr::local_tempdir()
  case <- simple_auth_case(root)
  secret <- paste0(case$result, ".auth.env")
  replacement <- foreign_auth_secret()
  ops <- single_user_auth_ops()
  publication_ops <- .bundlePublicationOps()
  original_rename <- publication_ops$rename
  publication_ops$rename <- function(from, to) {
    moved <- original_rename(from, to)
    if (isTRUE(moved) && grepl("-stage-", basename(from), fixed = TRUE)) {
      unlink(secret)
      writeBin(replacement, secret, useBytes = TRUE)
      Sys.chmod(secret, "0600", use_umask = FALSE)
    }
    moved
  }

  captured <- capture_auth_bundle_conditions(run_simple_auth_build(
    case$crb,
    case$result,
    ops,
    publication_ops = publication_ops
  ))
  expect_s3_class(captured$value, "error")
  expect_match(conditionMessage(captured$value), "changed", fixed = TRUE)
  expect_auth_bundle_rollback(root, case$result, case$old_mode)
  expect_identical(readBin(secret, "raw", n = length(replacement)), replacement)
  expect_true(is.na(Sys.getenv(
    simple_auth_environment,
    unset = NA_character_
  )))
  expect_true(any(grepl(
    normalizePath(secret, winslash = "/"),
    captured$warnings,
    fixed = TRUE
  )))
  expect_length(simple_auth_transaction_artifacts(root), 0L)
})

test_that("commit survives old-backup cleanup warnings", {
  local_simple_auth_environment()
  root <- withr::local_tempdir()
  case <- simple_auth_case(root)
  ops <- single_user_auth_ops()
  publication_ops <- utils::modifyList(
    .bundlePublicationOps(),
    list(unlink = function(path, recursive, force) 1L)
  )
  withr::local_options(warn = 2)

  expect_error(
    run_simple_auth_build(
      case$crb,
      case$result,
      ops,
      publication_ops = publication_ops
    ),
    "old backup remains",
    fixed = TRUE
  )

  expect_true(file.exists(file.path(case$result, "app.R")))
  expect_false(file.exists(file.path(case$result, "marker.txt")))
  expect_true(file.exists(paste0(case$result, ".auth.env")))
  expect_identical(
    Sys.getenv(simple_auth_environment),
    paste(sprintf("%02x", 1:32), collapse = "")
  )
  residues <- simple_auth_transaction_artifacts(root)
  expect_true(any(grepl("-backup-", residues, fixed = TRUE)))
  expect_false(any(grepl(
    "candidate|scratch|stage|build\\.lock",
    residues
  )))
})

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

test_that("simple authentication reserves its database before prompting", {
  local_simple_auth_environment()
  root <- withr::local_tempdir()
  source_dir <- file.path(root, "source")
  dir.create(file.path(source_dir, "auth"), recursive = TRUE)
  writeLines("backend", file.path(source_dir, "auth", "credentials.sqlite"))
  crb <- file.path(source_dir, "dataset.crb")
  object <- Cerebro_v1.3$new()
  object$setExpressionBackend("h5", "auth/credentials.sqlite")
  saveRDS(object, crb)
  result <- file.path(root, "app")
  prompts <- 0L
  ops <- auth_test_setup_ops(
    is_interactive = function() TRUE,
    read_input = function(prompt) {
      prompts <<- prompts + 1L
      stop("unexpected prompt", call. = FALSE)
    }
  )

  expect_error(
    testthat::with_mocked_bindings(
      createShinyApp(
        cerebro_data = c(Dataset = crb),
        result_dir = result,
        auth = TRUE,
        launch_browser = FALSE,
        verbose = FALSE
      ),
      .viewerAuthSetupOps = function() ops,
      .viewerAuthProviderAvailable = mock_viewer_auth_provider_available,
      .package = "CerebroNexus"
    ),
    "same bundle target",
    fixed = TRUE
  )
  expect_identical(prompts, 0L)
  expect_false(.bundlePathExists(result))
  expect_false(.bundlePathExists(paste0(result, ".auth.env")))
  expect_length(simple_auth_transaction_artifacts(root), 0L)
})

test_that("authentication target uses the existing collision registry", {
  fixture <- logical_viewer_auth_fixture()
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
  fixture <- logical_viewer_auth_fixture()
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
  fixture <- logical_viewer_auth_fixture()
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
  fixture <- logical_viewer_auth_fixture()
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
