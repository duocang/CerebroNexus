test_that("provider R channels are redacted and all sinks are restored", {
  sentinel <- "PROVIDER-SECRET-SENTINEL"
  state <- viewer_auth_provision_staged_state()
  provider <- function() {
    message(sentinel)
    warning(sentinel, call. = FALSE)
    cat(sentinel, "\n", sep = "")
    cat(sentinel, "\n", sep = "", file = stderr())
    stop(sentinel, call. = FALSE)
  }
  output_before <- sink.number(type = "output")
  message_before <- sink.number(type = "message")
  outer_output <- capture.output(
    {
      outer_message <- capture.output(
        {
          result <- CerebroNexus:::.viewerAuthRunProvider(state, provider)
        },
        type = "message"
      )
    },
    type = "output"
  )
  expect_false(result$ok)
  expect_null(result$value)
  expect_false(any(grepl(
    sentinel,
    c(outer_output, outer_message),
    fixed = TRUE
  )))
  expect_identical(sink.number(type = "output"), output_before)
  expect_identical(sink.number(type = "message"), message_before)
})

test_that("sensitive artifact scanning is bounded and detects chunk boundaries", {
  path <- withr::local_tempfile(fileext = ".bin")
  writeBin(charToRaw("prefix-SECRET-suffix"), path)
  expect_true(viewer_auth_file_contains(path, "SECRET", chunk_size = 8L))
  expect_false(viewer_auth_file_contains(path, "missing", chunk_size = 8L))
})

test_that("public provisioning returns a strict secret-free result", {
  fixture <- viewer_auth_provision_public_fixture()
  fixture$ops$setenv <- function(name, value) {
    stop("must not install", call. = FALSE)
  }
  testthat::local_mocked_bindings(
    .viewerAuthProvisionOps = function() fixture$ops,
    .package = "CerebroNexus"
  )
  provision <- provisionViewerAuthentication(fixture$accounts, fixture$target)
  expect_s3_class(provision, "cerebro_viewer_auth_provision")
  expect_identical(
    names(provision$auth),
    c("credentials", "passphrase_env", "timeout_minutes")
  )
  secret <- sub("^[^=]+=", "", readLines(provision$secret_file, warn = FALSE))
  result_file <- withr::local_tempfile(fileext = ".rds")
  saveRDS(provision, result_file)
  for (payload in list(
    serialize(provision, NULL),
    readBin(result_file, "raw", n = file.info(result_file)$size),
    serialize(readRDS(provision$manifest_file), NULL)
  )) {
    expect_false(viewer_auth_raw_contains(payload, secret))
    for (password in fixture$passwords) {
      expect_false(viewer_auth_raw_contains(payload, password))
    }
  }
  rendered <- c(
    capture.output(print(provision)),
    capture.output(str(provision)),
    capture.output(str(readRDS(provision$manifest_file)))
  )
  for (sensitive in c(secret, fixture$passwords)) {
    expect_false(any(grepl(sensitive, rendered, fixed = TRUE)))
  }
  expect_false(provision$environment_installed)
})

test_that("partial setenv failures are rolled back", {
  for (mode in c("throw", "false", "wrong_readback")) {
    env_name <- paste0("CEREBRO_AUTH_PARTIAL_", toupper(mode))
    Sys.unsetenv(env_name)
    fixture <- viewer_auth_provision_public_fixture(env_name)
    fixture$ops$setenv <- local({
      selected <- mode
      function(name, value) {
        do.call(
          Sys.setenv,
          stats::setNames(
            list(if (selected == "wrong_readback") "wrong" else value),
            name
          )
        )
        if (selected == "throw") {
          stop("sentinel", call. = FALSE)
        }
        selected != "false"
      }
    })
    testthat::local_mocked_bindings(
      .viewerAuthProvisionOps = function() fixture$ops,
      .package = "CerebroNexus"
    )
    condition <- tryCatch(
      provisionViewerAuthentication(
        fixture$accounts,
        fixture$target,
        passphrase_env = env_name,
        install_env = TRUE
      ),
      error = identity
    )
    expect_identical(condition$code, "environment_install_failed", info = mode)
    expect_true(is.na(Sys.getenv(env_name, unset = NA_character_)), info = mode)
  }
  for (malformed in list(
    function() stop("sentinel", call. = FALSE),
    function() character(),
    function() c("one", "two"),
    function() TRUE
  )) {
    local({
      env_name <- "CEREBRO_AUTH_MALFORMED_READBACK"
      Sys.unsetenv(env_name)
      fixture <- viewer_auth_provision_public_fixture(env_name)
      real <- fixture$ops$getenv
      pending <- FALSE
      fixture$ops$setenv <- function(name, value) {
        do.call(Sys.setenv, stats::setNames(list(value), name))
        pending <<- TRUE
        TRUE
      }
      fixture$ops$getenv <- function(name) {
        if (pending) {
          pending <<- FALSE
          malformed()
        } else {
          real(name)
        }
      }
      testthat::local_mocked_bindings(
        .viewerAuthProvisionOps = function() fixture$ops,
        .package = "CerebroNexus"
      )
      condition <- tryCatch(
        provisionViewerAuthentication(
          fixture$accounts,
          fixture$target,
          passphrase_env = env_name,
          install_env = TRUE
        ),
        error = identity
      )
      expect_identical(condition$code, "environment_install_failed")
      expect_false(grepl("sentinel", conditionMessage(condition), fixed = TRUE))
      expect_true(is.na(Sys.getenv(env_name, unset = NA_character_)))
    })
  }
})

test_that("throwing or malformed environment readback is rolled back", {
  env_name <- "CEREBRO_AUTH_MALFORMED_READBACK_NAMED"
  Sys.unsetenv(env_name)
  fixture <- viewer_auth_provision_public_fixture(env_name)
  real <- fixture$ops$getenv
  pending <- FALSE
  fixture$ops$setenv <- function(name, value) {
    do.call(Sys.setenv, stats::setNames(list(value), name))
    pending <<- TRUE
    TRUE
  }
  fixture$ops$getenv <- function(name) {
    if (pending) {
      pending <<- FALSE
      stop("sentinel", call. = FALSE)
    }
    real(name)
  }
  testthat::local_mocked_bindings(
    .viewerAuthProvisionOps = function() fixture$ops,
    .package = "CerebroNexus"
  )
  condition <- tryCatch(
    provisionViewerAuthentication(
      fixture$accounts,
      fixture$target,
      passphrase_env = env_name,
      install_env = TRUE
    ),
    error = identity
  )
  expect_identical(condition$code, "environment_install_failed")
  expect_false(grepl("sentinel", conditionMessage(condition), fixed = TRUE))
  expect_true(is.na(Sys.getenv(env_name, unset = NA_character_)))
})

test_that("unset failure is cleanup_incomplete and keeps recovery lock", {
  fixture <- viewer_auth_provision_public_fixture()
  fixture$ops$create_db <- function(...) stop("primary sentinel", call. = FALSE)
  testthat::local_mocked_bindings(
    .viewerAuthProvisionOps = function() fixture$ops,
    .package = "CerebroNexus"
  )
  condition <- tryCatch(
    provisionViewerAuthentication(fixture$accounts, fixture$target),
    error = identity
  )
  expect_identical(condition$code, "database_create_failed")
  expect_false(dir.exists(fixture$target))

  env_name <- "CEREBRO_AUTH_UNSET_FAILURE"
  withr::local_envvar(.new = stats::setNames(NA_character_, env_name))
  fixture <- viewer_auth_provision_public_fixture(env_name)
  fixture$ops$setenv <- function(name, value) {
    do.call(Sys.setenv, stats::setNames(list(value), name))
    FALSE
  }
  fixture$ops$unsetenv <- function(name) FALSE
  testthat::local_mocked_bindings(
    .viewerAuthProvisionOps = function() fixture$ops,
    .package = "CerebroNexus"
  )
  condition <- tryCatch(
    provisionViewerAuthentication(
      fixture$accounts,
      fixture$target,
      passphrase_env = env_name,
      install_env = TRUE
    ),
    error = identity
  )
  expect_identical(condition$code, "cleanup_incomplete")
  expect_identical(condition$cause_code, "environment_install_failed")
  expect_true(dir.exists(condition$recovery_path))
})

test_that("a downstream lock-release failure restores only the owned env", {
  env_name <- "CEREBRO_AUTH_RELEASE_FAILURE"
  other_name <- "CEREBRO_AUTH_UNRELATED"
  Sys.unsetenv(env_name)
  Sys.setenv(CEREBRO_AUTH_UNRELATED = "keep")
  withr::defer({
    Sys.unsetenv(env_name)
    Sys.unsetenv(other_name)
  })
  fixture <- viewer_auth_provision_public_fixture(env_name)
  remove_dir <- fixture$ops$remove_dir
  fixture$ops$remove_dir <- function(path) {
    if (grepl("\\.lock$", path)) FALSE else remove_dir(path)
  }
  testthat::local_mocked_bindings(
    .viewerAuthProvisionOps = function() fixture$ops,
    .package = "CerebroNexus"
  )
  condition <- tryCatch(
    provisionViewerAuthentication(
      fixture$accounts,
      fixture$target,
      passphrase_env = env_name,
      install_env = TRUE
    ),
    error = identity
  )
  expect_identical(condition$code, "cleanup_incomplete")
  expect_identical(condition$cause_code, "artifact_publish_failed")
  expect_true(is.na(Sys.getenv(env_name, unset = NA_character_)))
  expect_identical(Sys.getenv(other_name), "keep")
  expect_true(file.exists(condition$recovery_path))
  receipt <- readRDS(condition$recovery_path)
  expect_true(CerebroNexus:::.viewerAuthValidOwnerManifest(receipt))
  expect_identical(receipt$operation_id, fixture$operation_id)
})

test_that("cleanup and finish-hook exceptions never replace stable outcomes", {
  fixture <- viewer_auth_provision_public_fixture()
  fixture$ops$create_db <- function(...) stop("primary sentinel", call. = FALSE)
  fixture$ops$list_files <- function(...) {
    stop("cleanup sentinel", call. = FALSE)
  }
  fixture$ops$finish_hook <- function(...) {
    stop("finish sentinel", call. = FALSE)
  }
  testthat::local_mocked_bindings(
    .viewerAuthProvisionOps = function() fixture$ops,
    .package = "CerebroNexus"
  )
  condition <- tryCatch(
    provisionViewerAuthentication(fixture$accounts, fixture$target),
    error = identity
  )
  expect_identical(condition$code, "cleanup_incomplete")
  expect_identical(condition$cause_code, "database_create_failed")
  expect_true(file.exists(condition$recovery_path))
  expect_false(grepl("sentinel", conditionMessage(condition), fixed = TRUE))
})

test_that("finish hook observes all state-held secrets scrubbed on both paths", {
  observed <- list()
  run_case <- function(fail) {
    local({
      fixture <- viewer_auth_provision_public_fixture()
      if (fail) {
        fixture$ops$create_db <- function(...) stop("sentinel", call. = FALSE)
      }
      fixture$ops$finish_hook <- function(state) {
        observed[[length(observed) + 1L]] <<- c(
          accounts = is.null(state$accounts),
          passphrase = is.null(state$passphrase),
          provider_capture = is.null(state$provider_capture),
          secret_bytes = is.null(state$secret_bytes)
        )
        invisible(NULL)
      }
      testthat::local_mocked_bindings(
        .viewerAuthProvisionOps = function() fixture$ops,
        .package = "CerebroNexus"
      )
      try(
        provisionViewerAuthentication(fixture$accounts, fixture$target),
        silent = TRUE
      )
    })
  }
  run_case(FALSE)
  run_case(TRUE)
  expect_length(observed, 2L)
  expect_true(all(vapply(observed, all, logical(1))))
})

test_that("opt-in environment installation has ownership and rollback", {
  env_name <- "CEREBRO_AUTH_PUBLIC_TEST_KEY"
  withr::local_envvar(.new = stats::setNames(NA_character_, env_name))
  fixture <- viewer_auth_provision_public_fixture(env_name)
  testthat::local_mocked_bindings(
    .viewerAuthProvisionOps = function() fixture$ops,
    .package = "CerebroNexus"
  )
  provision <- provisionViewerAuthentication(
    fixture$accounts,
    fixture$target,
    passphrase_env = env_name,
    install_env = TRUE
  )
  expect_true(nzchar(Sys.getenv(env_name)))
  expect_true(provision$environment_installed)
})

test_that("an explicitly occupied empty environment variable is never overwritten", {
  env_name <- "CEREBRO_AUTH_OCCUPIED_EMPTY"
  withr::local_envvar(.new = stats::setNames("", env_name))
  fixture <- viewer_auth_provision_public_fixture(env_name)
  testthat::local_mocked_bindings(
    .viewerAuthProvisionOps = function() fixture$ops,
    .package = "CerebroNexus"
  )
  condition <- tryCatch(
    provisionViewerAuthentication(
      fixture$accounts,
      fixture$target,
      passphrase_env = env_name,
      install_env = TRUE
    ),
    error = identity
  )
  expect_identical(condition$code, "environment_conflict")
  expect_identical(Sys.getenv(env_name, unset = NA_character_), "")
})

test_that("successful provider calls also redact channels and restore sinks", {
  sentinel <- "PROVIDER-SUCCESS-SENTINEL"
  state <- viewer_auth_provision_staged_state()
  provider <- function() {
    message(sentinel)
    warning(sentinel, call. = FALSE)
    cat(sentinel, "\n", sep = "")
    cat(sentinel, "\n", sep = "", file = stderr())
    TRUE
  }
  output_before <- sink.number(type = "output")
  message_before <- sink.number(type = "message")
  outer_output <- capture.output(
    {
      outer_message <- capture.output(
        {
          result <- CerebroNexus:::.viewerAuthRunProvider(state, provider)
        },
        type = "message"
      )
    },
    type = "output"
  )
  expect_true(result$ok)
  expect_true(result$value)
  expect_false(any(grepl(
    sentinel,
    c(outer_output, outer_message),
    fixed = TRUE
  )))
  expect_identical(sink.number(type = "output"), output_before)
  expect_identical(sink.number(type = "message"), message_before)
})

test_that("dependency failure precedes every filesystem claim", {
  state <- viewer_auth_provision_test_state(namespace_available = function(
    package
  ) {
    FALSE
  })
  before <- list.files(dirname(state$options$target_dir), all.files = TRUE)
  expect_provision_error(
    CerebroNexus:::.viewerAuthRequireProvisionDependencies(state),
    "missing_dependency",
    "dependency"
  )
  expect_identical(
    list.files(dirname(state$options$target_dir), all.files = TRUE),
    before
  )
})

test_that("database creation clears accounts and validates one private file", {
  seen <- new.env(parent = emptyenv())
  state <- viewer_auth_provision_staged_state(
    create_db = function(credentials_data, sqlite_path, passphrase) {
      seen$names <- names(credentials_data)
      seen$passphrase_length <- nchar(passphrase)
      writeBin(as.raw(c(0x53, 0x51, 0x4c)), sqlite_path)
      TRUE
    },
    validate_db = function(path, passphrase) {
      expect_true(file.exists(path))
      expect_identical(nchar(passphrase), 64L)
      TRUE
    }
  )
  CerebroNexus:::.viewerAuthCreateProvisionDatabase(state)
  expect_identical(seen$names, c("user", "password", "admin"))
  expect_identical(seen$passphrase_length, 64L)
  expect_null(state$accounts)
  expect_true(CerebroNexus:::.viewerAuthIdentityMatches(
    state,
    "credentials",
    state$paths$credentials
  ))
})

test_that("database providers must return literal TRUE", {
  for (value in list(FALSE, NULL, "TRUE", 1L)) {
    create_state <- viewer_auth_provision_staged_state(create_db = function(
      ...
    ) {
      writeBin(as.raw(1L), list(...)[[2L]])
      value
    })
    expect_identical(
      tryCatch(
        CerebroNexus:::.viewerAuthCreateProvisionDatabase(create_state),
        error = identity
      )$code,
      "database_create_failed"
    )
    validate_state <- viewer_auth_provision_staged_state(validate_db = function(
      ...
    ) {
      value
    })
    expect_identical(
      tryCatch(
        CerebroNexus:::.viewerAuthCreateProvisionDatabase(validate_state),
        error = identity
      )$code,
      "database_validation_failed"
    )
  }
})

test_that("secret artifact is strict, private, and clears in-memory passphrase", {
  state <- viewer_auth_provision_staged_state()
  CerebroNexus:::.viewerAuthCreateProvisionDatabase(state)
  expected <- paste0(state$identity$passphrase_env, "=", state$passphrase, "\n")
  CerebroNexus:::.viewerAuthWriteProvisionSecret(state)
  expect_identical(
    rawToChar(readBin(state$paths$secret, "raw", n = 4097L)),
    expected
  )
  expect_null(state$passphrase)
  expect_null(state$secret_bytes)
  expect_false(dir.exists(file.path(state$paths$stage, "www")))
})

test_that("secret reader rejects appended text after a maximal environment name", {
  state <- viewer_auth_provision_staged_state()
  state$identity$passphrase_env <- paste0("A", strrep("B", 4030L))
  passphrase <- state$passphrase
  writeLines(
    c(
      paste0(state$identity$passphrase_env, "=", passphrase),
      "INJECTED=not-a-passphrase"
    ),
    state$paths$secret,
    useBytes = TRUE
  )
  expect_provision_error(
    CerebroNexus:::.viewerAuthReadProvisionSecret(state, state$paths$stage),
    "artifact_publish_failed",
    "publish"
  )
})

test_that("failed secret writes remain owned and are removable during cleanup", {
  for (fault in c("partial", "chmod")) {
    state <- viewer_auth_provision_staged_state()
    CerebroNexus:::.viewerAuthCreateProvisionDatabase(state)
    if (identical(fault, "partial")) {
      state$ops$write_raw <- function(bytes, path) {
        writeBin(bytes[1L], path)
        FALSE
      }
    } else {
      chmod <- state$ops$chmod
      state$ops$chmod <- function(...) FALSE
    }
    expect_provision_error(
      CerebroNexus:::.viewerAuthWriteProvisionSecret(state),
      "artifact_publish_failed",
      "artifact_stage"
    )
    expect_true(CerebroNexus:::.viewerAuthIdentityMatches(
      state,
      "secret_tmp",
      state$paths$secret_tmp
    ))
    if (identical(fault, "chmod")) {
      state$ops$chmod <- chmod
    }
    expect_true(CerebroNexus:::.viewerAuthCleanupProvision(state))
    expect_true(CerebroNexus:::.viewerAuthReleaseProvisionLock(state))
  }
})

test_that("secret rename only rebinds the frozen temporary generation", {
  state <- viewer_auth_provision_staged_state()
  CerebroNexus:::.viewerAuthCreateProvisionDatabase(state)
  inspect <- state$ops$inspect_path
  state$ops$inspect_path <- function(path) {
    info <- inspect(path)
    if (identical(path, state$paths$secret) && isTRUE(info$exists)) {
      info$inode <- "foreign-inode"
    }
    info
  }
  expect_provision_error(
    CerebroNexus:::.viewerAuthWriteProvisionSecret(state),
    "artifact_publish_failed",
    "artifact_stage"
  )
  expect_false(CerebroNexus:::.viewerAuthIdentityMatches(
    state,
    "secret",
    state$paths$secret
  ))
  expect_false(is.null(state$identities$secret_tmp))
})

test_that("owner uses publishing around rename and published after validation", {
  state <- viewer_auth_provision_staged_state()
  CerebroNexus:::.viewerAuthCreateProvisionDatabase(state)
  CerebroNexus:::.viewerAuthWriteProvisionSecret(state)
  real_rename <- state$ops$rename
  state$ops$rename <- function(from, to) {
    if (identical(from, state$paths$stage)) {
      expect_identical(readRDS(state$paths$owner)$state, "publishing")
      expect_false(file.exists(state$paths$target))
    }
    real_rename(from, to)
  }
  CerebroNexus:::.viewerAuthPublishProvision(state)
  expect_false(dir.exists(state$paths$stage))
  expect_true(dir.exists(state$paths$target))
  expect_identical(readRDS(state$paths$owner)$state, "published")
})

test_that("postvalidation rejects replaced published artifacts before DB access", {
  calls <- 0L
  state <- viewer_auth_provision_staged_state(
    validate_db = function(...) {
      calls <<- calls + 1L
      TRUE
    }
  )
  CerebroNexus:::.viewerAuthCreateProvisionDatabase(state)
  CerebroNexus:::.viewerAuthWriteProvisionSecret(state)
  calls <- 0L
  rename <- state$ops$rename
  state$ops$rename <- function(from, to) {
    ok <- rename(from, to)
    if (ok && identical(from, state$paths$stage)) {
      credentials <- file.path(state$paths$target, "credentials.sqlite")
      unlink(credentials)
      writeBin(as.raw(c(0x66, 0x61, 0x6b, 0x65)), credentials)
      Sys.chmod(credentials, "0600")
    }
    ok
  }
  expect_provision_error(
    CerebroNexus:::.viewerAuthPublishProvision(state),
    "artifact_publish_failed",
    "publish"
  )
  expect_identical(calls, 0L)
})

test_that("stage publication only rebinds the frozen stage generation", {
  state <- viewer_auth_provision_staged_state()
  CerebroNexus:::.viewerAuthCreateProvisionDatabase(state)
  CerebroNexus:::.viewerAuthWriteProvisionSecret(state)
  rename <- state$ops$rename
  state$ops$rename <- function(from, to) {
    ok <- rename(from, to)
    if (ok && identical(from, state$paths$stage)) {
      unlink(to, recursive = TRUE)
      dir.create(to, mode = "0700")
    }
    ok
  }
  expect_provision_error(
    CerebroNexus:::.viewerAuthPublishProvision(state),
    "artifact_publish_failed",
    "publish"
  )
  expect_null(state$identities$target)
  expect_false(is.null(state$identities$stage))
})

test_that("publish-side inspection failures are redacted to stable conditions", {
  state <- viewer_auth_provision_staged_state()
  CerebroNexus:::.viewerAuthCreateProvisionDatabase(state)
  CerebroNexus:::.viewerAuthWriteProvisionSecret(state)
  state$ops$list_files <- function(...) stop("INSPECT-SENTINEL", call. = FALSE)
  condition <- tryCatch(
    CerebroNexus:::.viewerAuthPublishProvision(state),
    error = identity
  )
  expect_s3_class(condition, "cerebro_viewer_auth_provision_error")
  expect_identical(condition$code, "artifact_publish_failed")
  expect_identical(condition$stage, "publish")
  expect_false(grepl(
    "INSPECT-SENTINEL",
    conditionMessage(condition),
    fixed = TRUE
  ))
})

test_that("sidecar identity inspection exceptions do not escape publish", {
  state <- viewer_auth_provision_staged_state()
  CerebroNexus:::.viewerAuthCreateProvisionDatabase(state)
  CerebroNexus:::.viewerAuthWriteProvisionSecret(state)
  sidecar <- paste0(state$paths$credentials, "-wal")
  writeBin(as.raw(1L), sidecar)
  inspect <- state$ops$inspect_path
  calls <- 0L
  state$ops$inspect_path <- function(path) {
    if (identical(path, sidecar)) {
      calls <<- calls + 1L
      if (calls >= 3L) stop("SIDECAR-SENTINEL", call. = FALSE)
    }
    inspect(path)
  }
  condition <- tryCatch(
    CerebroNexus:::.viewerAuthPublishProvision(state),
    error = identity
  )
  expect_s3_class(condition, "cerebro_viewer_auth_provision_error")
  expect_identical(condition$code, "artifact_publish_failed")
  expect_identical(condition$stage, "publish")
  expect_false(grepl(
    "SIDECAR-SENTINEL",
    conditionMessage(condition),
    fixed = TRUE
  ))
})

test_that("artifact fault matrix is stable and scrubs all held secrets", {
  faults <- list(
    provider = list(
      setup = function(s) {
        s$ops$create_db <- function(...) stop("sentinel", call. = FALSE)
      },
      action = CerebroNexus:::.viewerAuthCreateProvisionDatabase,
      code = "database_create_failed",
      stage = "database"
    ),
    validator = list(
      setup = function(s) {
        s$ops$validate_db <- function(...) stop("sentinel", call. = FALSE)
      },
      action = CerebroNexus:::.viewerAuthCreateProvisionDatabase,
      code = "database_validation_failed",
      stage = "database"
    ),
    secret = list(
      setup = function(s) {
        CerebroNexus:::.viewerAuthCreateProvisionDatabase(s)
        s$ops$write_raw <- function(bytes, path) FALSE
      },
      action = CerebroNexus:::.viewerAuthWriteProvisionSecret,
      code = "artifact_publish_failed",
      stage = "artifact_stage"
    ),
    secret_chmod = list(
      setup = function(s) {
        CerebroNexus:::.viewerAuthCreateProvisionDatabase(s)
        s$ops$chmod <- function(...) FALSE
      },
      action = CerebroNexus:::.viewerAuthWriteProvisionSecret,
      code = "artifact_publish_failed",
      stage = "artifact_stage"
    ),
    ready_manifest = list(
      setup = function(s) {
        CerebroNexus:::.viewerAuthCreateProvisionDatabase(s)
        CerebroNexus:::.viewerAuthWriteProvisionSecret(s)
        s$ops$save_rds <- function(...) FALSE
      },
      action = CerebroNexus:::.viewerAuthPublishProvision,
      code = "artifact_publish_failed",
      stage = "manifest"
    ),
    final_rename = list(
      setup = function(s) {
        CerebroNexus:::.viewerAuthCreateProvisionDatabase(s)
        CerebroNexus:::.viewerAuthWriteProvisionSecret(s)
        rename <- s$ops$rename
        s$ops$rename <- function(from, to) {
          if (identical(from, s$paths$stage)) FALSE else rename(from, to)
        }
      },
      action = CerebroNexus:::.viewerAuthPublishProvision,
      code = "artifact_publish_failed",
      stage = "publish"
    ),
    postvalidation = list(
      setup = function(s) {
        CerebroNexus:::.viewerAuthCreateProvisionDatabase(s)
        CerebroNexus:::.viewerAuthWriteProvisionSecret(s)
        s$ops$read_raw <- function(...) stop("sentinel", call. = FALSE)
      },
      action = CerebroNexus:::.viewerAuthPublishProvision,
      code = "artifact_publish_failed",
      stage = "publish"
    ),
    sqlite_sidecar = list(
      setup = function(s) {
        CerebroNexus:::.viewerAuthCreateProvisionDatabase(s)
        CerebroNexus:::.viewerAuthWriteProvisionSecret(s)
        sidecar <- paste0(s$paths$credentials, "-wal")
        writeBin(as.raw(1L), sidecar)
        remove <- s$ops$remove_file
        s$ops$remove_file <- function(path) {
          if (identical(path, sidecar)) FALSE else remove(path)
        }
      },
      action = CerebroNexus:::.viewerAuthPublishProvision,
      code = "artifact_publish_failed",
      stage = "publish"
    )
  )
  for (name in names(faults)) {
    fault <- faults[[name]]
    state <- viewer_auth_provision_staged_state()
    fault$setup(state)
    condition <- tryCatch(fault$action(state), error = identity)
    expect_identical(condition$code, fault$code)
    expect_identical(condition$stage, fault$stage)
    expect_false(grepl("sentinel", conditionMessage(condition), fixed = TRUE))
    if (name %in% c("final_rename", "postvalidation")) {
      # A rename failure retains staging; a postvalidation failure retains the
      # published directory under its still-publishing owner receipt.
      expect_identical(readRDS(state$paths$owner)$state, "publishing")
    }
    CerebroNexus:::.viewerAuthScrubProvisionState(state)
    expect_null(state$accounts)
    expect_null(state$passphrase)
    expect_null(state$provider_capture)
    expect_null(state$secret_bytes)
  }
})
test_that("4.2 metadata and deployment handoff stay synchronized", {
  description <- read.dcf(test_path("..", "..", "DESCRIPTION"))
  expect_identical(unname(description[1L, "Version"]), "4.2")
  suggests <- trimws(strsplit(description[1L, "Suggests"], ",")[[1L]])
  expect_true(any(grepl("^openssl($| \\()", suggests)))

  news <- readLines(test_path("..", "..", "NEWS.md"), warn = FALSE)
  expect_identical(news[[1L]], "# CerebroNexus 4.2")
  app <- readLines(test_path("..", "..", "inst", "app.R"), warn = FALSE)
  expect_true(any(grepl('"cerebro_version" = "4.2"', app, fixed = TRUE)))

  vignette <- paste(
    readLines(
      test_path(
        "..",
        "..",
        "vignettes",
        "control_access_to_cerebro_with_a_login_page.Rmd"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  required <- c(
    "provisionViewerAuthentication",
    "# One-minute setup",
    "# What provisioning creates",
    "# How this maps to Builder",
    "# Troubleshooting",
    "readRenviron",
    "EnvironmentFile=/absolute/private/viewer-auth.env",
    "--env-file /absolute/private/viewer-auth.env",
    "Sys.unsetenv",
    "rollback window",
    "secret store",
    "outside the App tree"
  )
  for (token in required) {
    expect_true(grepl(token, vignette, fixed = TRUE))
  }
  expect_true(grepl(
    "Builder passes only `provision$auth` to `createShinyApp()`",
    vignette,
    fixed = TRUE
  ))
  expect_true(grepl(
    "Builder must never persist `accounts`, login passwords, or the generated passphrase",
    vignette,
    fixed = TRUE
  ))
  diagrams <- c(
    "img/auth-provisioning-validation.png",
    "img/auth-builder-boundary.png",
    "img/auth-deployment-boundary.png"
  )
  for (diagram in diagrams) {
    expect_true(grepl(diagram, vignette, fixed = TRUE), info = diagram)
    expect_true(
      file.exists(test_path("..", "..", "vignettes", diagram)),
      info = diagram
    )
  }
})

test_that("real provisioning builds a lite-compatible authenticated App", {
  skip_on_os("windows")
  skip_if_not_installed("openssl")
  skip_if_not_installed("shinymanager", minimum_version = "1.1.0")
  root <- withr::local_tempdir()
  Sys.chmod(root, "0700")
  env_name <- "CEREBRO_AUTH_REAL_PROVISION_TEST"
  Sys.unsetenv(env_name)
  on.exit(Sys.unsetenv(env_name), add = TRUE)
  provision <- provisionViewerAuthentication(
    accounts = data.frame(
      user = c("alice", "bob"),
      password = c("alice-password", "bob-password-12"),
      admin = c(TRUE, FALSE),
      stringsAsFactors = FALSE
    ),
    target_dir = file.path(root, "viewer-auth"),
    passphrase_env = env_name,
    install_env = TRUE
  )
  crb <- file.path(root, "dataset.crb")
  saveRDS(Cerebro_v1.3$new(), crb)
  app <- file.path(root, "app")
  createShinyApp(
    cerebro_data = c(Dataset = crb),
    result_dir = app,
    auth = provision$auth,
    launch_browser = FALSE,
    verbose = FALSE
  )
  config <- readRDS(file.path(app, "cerebro_config.rds"))$.viewer_auth
  expect_identical(config$passphrase_env, env_name)
  bundled <- file.path(app, "private-data", "auth", "credentials.sqlite")
  expect_true(file.exists(bundled))
  expect_false(any(
    basename(list.files(
      app,
      recursive = TRUE,
      all.files = TRUE
    )) ==
      "viewer-auth.env"
  ))
  secret <- sub("^[^=]+=", "", readLines(provision$secret_file, warn = FALSE))
  checker <- shinymanager::check_credentials(
    db = bundled,
    passphrase = secret
  )
  expect_true(checker("alice", "alice-password")$result)
  expect_true(checker("bob", "bob-password-12")$result)
  expect_false(checker("alice", "wrong-password")$result)
  for (path in list.files(
    app,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE
  )) {
    if (!dir.exists(path)) {
      for (sensitive in c(secret, "alice-password", "bob-password-12")) {
        expect_false(viewer_auth_file_contains(path, sensitive))
      }
    }
  }
})
