if (!exists("viewer_auth_fixture", mode = "function")) {
  source(testthat::test_path("helper-viewer-auth.R"), local = TRUE)
}

auth_setup_ops <- function(
  read_values = character(),
  password_values = list()
) {
  reads <- read_values
  passwords <- password_values
  utils::modifyList(
    .viewerAuthSetupOps(),
    list(
      is_interactive = function() TRUE,
      read_input = function(prompt) {
        if (!length(reads)) {
          return("")
        }
        value <- reads[[1L]]
        reads <<- reads[-1L]
        value
      },
      read_password = function(prompt) {
        if (!length(passwords)) {
          return(NULL)
        }
        value <- passwords[[1L]]
        passwords <<- passwords[-1L]
        value
      },
      random_bytes = function(size) as.raw(seq_len(size))
    )
  )
}

capture_auth_setup_conditions <- function(expr) {
  messages <- character()
  stdout <- capture.output(
    value <- withCallingHandlers(
      tryCatch(expr, error = identity),
      message = function(condition) {
        messages <<- c(messages, conditionMessage(condition))
        invokeRestart("muffleMessage")
      }
    )
  )
  list(value = value, messages = messages, stdout = stdout)
}

test_that("database validation accepts a direct passphrase without env mutation", {
  fixture <- viewer_auth_fixture()
  states <- list(
    absent = NA_character_,
    empty = "",
    present = "unrelated-process-secret"
  )

  for (state in states) {
    withr::local_envvar(stats::setNames(state, fixture$env_name))
    prior <- Sys.getenv(fixture$env_name, unset = NA_character_)
    expect_invisible(.viewerAuthValidateDatabase(
      fixture$database,
      fixture$passphrase,
      fixture$env_name
    ))
    expect_identical(
      Sys.getenv(fixture$env_name, unset = NA_character_),
      prior
    )
  }
})

test_that("direct database validation redacts an incorrect passphrase", {
  fixture <- viewer_auth_fixture()
  wrong <- paste0("wrong-", strrep("z", 64L))
  captured <- capture_auth_setup_conditions(
    .viewerAuthValidateDatabase(fixture$database, wrong, fixture$env_name)
  )

  expect_s3_class(captured$value, "error")
  expect_identical(
    conditionMessage(captured$value),
    paste0(
      "auth$credentials must be a readable shinymanager database using ",
      fixture$env_name,
      "."
    )
  )
  channels <- c(
    conditionMessage(captured$value),
    captured$messages,
    captured$stdout
  )
  expect_false(any(grepl(wrong, channels, fixed = TRUE)))
})

test_that("authentication manifests have one canonical ordered shape", {
  expect_identical(
    .viewerAuthManifest("bundle", "private/credentials.sqlite", "AUTH_SECRET"),
    list(
      schema_version = 1L,
      provider = "shinymanager",
      credentials_scope = "bundle",
      credentials_path = "private/credentials.sqlite",
      passphrase_env = "AUTH_SECRET",
      timeout_minutes = 15L
    )
  )
  expect_identical(
    .viewerAuthManifest("host", "/credentials.sqlite", "AUTH_SECRET", 42),
    list(
      schema_version = 1L,
      provider = "shinymanager",
      credentials_scope = "host",
      credentials_path = "/credentials.sqlite",
      passphrase_env = "AUTH_SECRET",
      timeout_minutes = 42L
    )
  )
})

test_that("setup operations expose the production dependency seam", {
  expect_identical(
    names(.viewerAuthSetupOps()),
    c(
      "is_interactive",
      "read_input",
      "read_password",
      "random_bytes",
      "namespace_available",
      "fs_info",
      "base_info",
      "effective_uid",
      "read_raw",
      "write_raw",
      "link",
      "unlink_file",
      "claim_dir",
      "remove_dir",
      "create_private_file",
      "chmod",
      "access",
      "list_dir",
      "getenv",
      "setenv",
      "unsetenv",
      "resource_paths",
      "entry_exists"
    )
  )
})

auth_secret_raw <- function(
  env_name = "CEREBRO_AUTH_PASSPHRASE_0102030405060708",
  passphrase = strrep("a", 64L)
) {
  charToRaw(paste0(env_name, "=", passphrase, "\n"))
}

write_auth_secret <- function(path, bytes = auth_secret_raw()) {
  writeBin(bytes, path, useBytes = TRUE)
  if (.Platform$OS.type != "windows") {
    Sys.chmod(path, "0600", use_umask = FALSE)
  }
  invisible(path)
}

test_that("secret parser accepts only the exact generated grammar", {
  root <- withr::local_tempdir()
  path <- file.path(root, "app.auth.env")
  write_auth_secret(path)

  snapshot <- .viewerAuthReadSecretFile(path, .viewerAuthSetupOps())
  expect_identical(
    snapshot$env_name,
    "CEREBRO_AUTH_PASSPHRASE_0102030405060708"
  )
  expect_identical(snapshot$passphrase, strrep("a", 64L))
  expect_identical(snapshot$raw, auth_secret_raw())

  invalid <- list(
    crlf = sub(
      "\n$",
      "\r\n",
      rawToChar(auth_secret_raw()),
      perl = TRUE
    ),
    multiline = paste0(rawToChar(auth_secret_raw()), "SECOND=value\n"),
    trailing = paste0(rawToChar(auth_secret_raw()), "x")
  )
  for (name in names(invalid)) {
    write_auth_secret(path, charToRaw(invalid[[name]]))
    expect_error(
      .viewerAuthReadSecretFile(path, .viewerAuthSetupOps()),
      "invalid or unsafe",
      fixed = TRUE,
      info = name
    )
  }
  write_auth_secret(path, c(auth_secret_raw(), as.raw(0L)))
  expect_error(
    .viewerAuthReadSecretFile(path, .viewerAuthSetupOps()),
    "invalid or unsafe",
    fixed = TRUE
  )
})

test_that("secret parser rejects symlinks and permissive POSIX modes", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  target <- write_auth_secret(file.path(root, "target.env"))
  link <- file.path(root, "app.auth.env")

  expect_true(file.symlink(target, link))
  expect_error(
    .viewerAuthReadSecretFile(link, .viewerAuthSetupOps()),
    "invalid or unsafe",
    fixed = TRUE
  )
  unlink(link)
  expect_true(file.copy(target, link))
  Sys.chmod(link, "0644", use_umask = FALSE)
  expect_error(
    .viewerAuthReadSecretFile(link, .viewerAuthSetupOps()),
    "invalid or unsafe",
    fixed = TRUE
  )
})

test_that("secret parser rejects dangling symlink entries", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  link <- file.path(root, "app.auth.env")
  expect_true(file.symlink(file.path(root, "missing.env"), link))
  expect_true(.bundlePathExists(link))
  expect_error(
    .viewerAuthReadSecretFile(link, .viewerAuthSetupOps()),
    "invalid or unsafe",
    fixed = TRUE
  )
})

test_that("preflight rejects a dangling sibling before prompting", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  result <- file.path(root, "app")
  secret <- paste0(result, ".auth.env")
  missing <- file.path(root, "missing.env")
  expect_true(file.symlink(missing, secret))
  prompts <- 0L
  ops <- auth_setup_ops()
  ops$read_input <- function(prompt) {
    prompts <<- prompts + 1L
    "should-not-be-read"
  }
  ops$read_password <- function(prompt) {
    prompts <<- prompts + 1L
    "should-not-be-read"
  }

  expect_error(
    .viewerAuthPreflightSimple(result, ops),
    "invalid or unsafe",
    fixed = TRUE
  )
  expect_identical(prompts, 0L)
  expect_true(.bundlePathExists(secret))
  expect_identical(Sys.readlink(secret), missing)
})

test_that("hex encoding is deterministic and honors requested case", {
  bytes <- as.raw(c(0L, 1L, 15L, 16L, 127L, 128L, 255L))
  expect_identical(.viewerAuthHex(bytes, FALSE), "00010f107f80ff")
  expect_identical(.viewerAuthHex(bytes, TRUE), "00010F107F80FF")
})

test_that("account collector accepts two confirmed trimmed users", {
  ops <- auth_setup_ops(
    c(" alice ", "yes", " bob ", "n"),
    list(
      "alice-password",
      "alice-password",
      "bob-password",
      "bob-password"
    )
  )
  accounts <- .viewerAuthCollectAccounts(ops)

  expect_identical(
    accounts,
    data.frame(
      user = c("alice", "bob"),
      password = c("alice-password", "bob-password"),
      admin = c(FALSE, FALSE),
      stringsAsFactors = FALSE
    )
  )
})

test_that("account collector retries duplicate users and mismatched passwords", {
  ops <- auth_setup_ops(
    c("alice", "y", "alice", "bob", "n"),
    list("a", "not-a", "a", "a", "b", "b")
  )
  captured <- capture_auth_setup_conditions(.viewerAuthCollectAccounts(ops))

  expect_false(inherits(captured$value, "error"))
  expect_identical(captured$value$user, c("alice", "bob"))
  expect_true(any(grepl("do not match", captured$messages, fixed = TRUE)))
  expect_true(any(grepl("already exists", captured$messages, fixed = TRUE)))
  channels <- c(captured$messages, captured$stdout)
  expect_false(any(grepl("not-a", channels, fixed = TRUE)))
})

test_that("blank usernames cancel the entire account build", {
  cases <- list(
    first_user_blank = auth_setup_ops("", list()),
    first_user_trimmed_blank = auth_setup_ops("   ", list()),
    later_user_blank = auth_setup_ops(
      c("alice", "y", ""),
      list("a", "a")
    )
  )

  for (name in names(cases)) {
    expect_error(
      .viewerAuthCollectAccounts(cases[[name]]),
      "cancelled",
      fixed = TRUE,
      info = name
    )
  }
})

test_that("cancelling either password prompt cancels the account build", {
  cases <- list(
    password = auth_setup_ops("alice", list(NULL)),
    confirmation = auth_setup_ops("alice", list("a", NULL))
  )

  for (name in names(cases)) {
    expect_error(
      .viewerAuthCollectAccounts(cases[[name]]),
      "cancelled",
      fixed = TRUE,
      info = name
    )
  }
})

test_that("empty passwords are rejected and the current user is retried", {
  ops <- auth_setup_ops(
    c("alice", "n"),
    list("", "alice-password", "alice-password")
  )
  captured <- capture_auth_setup_conditions(.viewerAuthCollectAccounts(ops))

  expect_identical(captured$value$user, "alice")
  expect_true("Password must not be empty.\n" %in% captured$messages)
  expect_false(any(grepl(
    "alice-password",
    c(captured$messages, captured$stdout),
    fixed = TRUE
  )))
})

test_that("continuation accepts y yes n no empty and end of input", {
  cases <- list(
    y_n = c("alice", "y", "bob", "n"),
    yes_no = c("alice", " YES ", "bob", " NO "),
    empty = c("alice", ""),
    eof = "alice"
  )

  for (name in names(cases)) {
    users <- if (name %in% c("y_n", "yes_no")) c("alice", "bob") else "alice"
    passwords <- rep(list("password", "password"), length(users))
    accounts <- .viewerAuthCollectAccounts(auth_setup_ops(
      cases[[name]],
      passwords
    ))
    expect_identical(accounts$user, users, info = name)
  }
})

test_that("continuation rejects unknown answers without losing accounts", {
  ops <- auth_setup_ops(
    c("alice", "maybe", " y ", "bob", "no"),
    list("a", "a", "b", "b")
  )
  captured <- capture_auth_setup_conditions(.viewerAuthCollectAccounts(ops))

  expect_identical(captured$value$user, c("alice", "bob"))
  expect_true("Please enter y or n.\n" %in% captured$messages)
})

test_that("dependency preflight reports every missing helper at once", {
  ops <- .viewerAuthSetupOps()
  checked <- character()
  ops$namespace_available <- function(package) {
    checked <<- c(checked, package)
    FALSE
  }

  expect_error(
    .viewerAuthRequireDependencies(ops),
    "shinymanager, askpass, openssl",
    fixed = TRUE
  )
  expect_identical(checked, c("shinymanager", "askpass", "openssl"))
})

test_that("dependency preflight delegates provider version validation last", {
  ops <- .viewerAuthSetupOps()
  ops$namespace_available <- function(package) TRUE
  called <- FALSE
  testthat::local_mocked_bindings(
    .viewerAuthProviderAvailable = function() {
      called <<- TRUE
      invisible(TRUE)
    },
    .package = "CerebroNexus"
  )

  expect_invisible(.viewerAuthRequireDependencies(ops))
  expect_true(called)
})

viewer_auth_setup_fixture <- function(
  existing_secret = FALSE,
  ops = NULL,
  envir = parent.frame()
) {
  root <- withr::local_tempdir(.local_envir = envir)
  result <- file.path(root, "app")
  if (existing_secret) {
    write_auth_secret(paste0(result, ".auth.env"))
  }
  if (is.null(ops)) {
    ops <- auth_setup_ops(
      c("fixture-user", "n"),
      list("fixture-password", "fixture-password")
    )
  }
  state <- .viewerAuthPreflightSimple(result, ops)
  .viewerAuthCompleteSimple(state)
  if (is.null(state$existing_snapshot)) {
    .viewerAuthCreateSecretCandidate(state)
  }
  state
}

capture_all_auth_conditions <- function(expr) {
  messages <- character()
  warnings <- character()
  stdout <- capture.output(
    value <- withCallingHandlers(
      tryCatch(expr, error = identity),
      message = function(condition) {
        messages <<- c(messages, conditionMessage(condition))
        invokeRestart("muffleMessage")
      },
      warning = function(condition) {
        warnings <<- c(warnings, conditionMessage(condition))
        invokeRestart("muffleWarning")
      }
    )
  )
  list(value = value, messages = messages, warnings = warnings, stdout = stdout)
}

test_that("secret identity rejects wrong owner nonregular and access failure", {
  root <- withr::local_tempdir()
  path <- write_auth_secret(file.path(root, "app.auth.env"))
  ops <- .viewerAuthSetupOps()

  if (.Platform$OS.type != "windows") {
    wrong_owner <- ops
    wrong_owner$effective_uid <- function() ops$effective_uid() + 1
    expect_error(
      .viewerAuthReadSecretFile(path, wrong_owner),
      "invalid or unsafe",
      fixed = TRUE
    )
  }

  denied <- ops
  denied$access <- function(path, mode) FALSE
  expect_error(
    .viewerAuthReadSecretFile(path, denied),
    "invalid or unsafe",
    fixed = TRUE
  )

  directory <- file.path(root, "directory.env")
  dir.create(directory)
  expect_error(
    .viewerAuthReadSecretFile(directory, ops),
    "invalid or unsafe",
    fixed = TRUE
  )
})

test_that("filesystem identity rejects non-finite and non-scalar metadata", {
  root <- withr::local_tempdir()
  path <- write_auth_secret(file.path(root, "app.auth.env"))
  original <- .viewerAuthSetupOps()

  for (field in c("device_id", "inode", "size")) {
    for (invalid in list(NA_real_, Inf, -1)) {
      ops <- original
      ops$fs_info <- function(candidate) {
        info <- original$fs_info(candidate)
        info[[field]][[1L]] <- invalid
        info
      }
      expect_error(
        .viewerAuthReadFileIdentity(path, "0600", ops),
        "invalid or unsafe",
        fixed = TRUE,
        info = paste(field, invalid)
      )
    }

    ops <- original
    ops$fs_info <- function(candidate) {
      info <- original$fs_info(candidate)
      info[[field]] <- list(c(1, 2))
      info
    }
    expect_error(
      .viewerAuthReadFileIdentity(path, "0600", ops),
      "invalid or unsafe",
      fixed = TRUE,
      info = paste(field, "non-scalar")
    )
  }
})

test_that("secret parser detects replacement during raw read", {
  root <- withr::local_tempdir()
  path <- write_auth_secret(file.path(root, "app.auth.env"))
  ops <- .viewerAuthSetupOps()
  original_read <- ops$read_raw
  ops$read_raw <- function(candidate) {
    bytes <- original_read(candidate)
    unlink(candidate)
    write_auth_secret(
      candidate,
      auth_secret_raw(
        "CEREBRO_AUTH_PASSPHRASE_FFFFFFFFFFFFFFFF",
        strrep("b", 64L)
      )
    )
    bytes
  }

  expect_error(
    .viewerAuthReadSecretFile(path, ops),
    "changed",
    fixed = TRUE
  )
})

test_that("preflight freezes parent identity and rejects resource roots", {
  root <- withr::local_tempdir()
  result <- file.path(root, "app")
  ops <- auth_setup_ops()
  ops$resource_paths <- function() root

  expect_error(
    .viewerAuthPreflightSimple(result, ops),
    "HTTP resource",
    fixed = TRUE
  )

  ops$resource_paths <- function() character()
  state <- .viewerAuthPreflightSimple(result, ops)
  expect_identical(state$result_parent, normalizePath(root, winslash = "/"))
  expect_false(is.null(state$result_parent_snapshot))
  expect_true(.viewerAuthSameDirectory(
    state$result_parent_snapshot,
    .viewerAuthReadDirectoryIdentity(root, "0700", ops, exact_mode = FALSE)
  ))
})

test_that("preflight records a nearest existing anchor for missing parents", {
  root <- withr::local_tempdir()
  result <- file.path(root, "missing", "nested", "app")
  state <- .viewerAuthPreflightSimple(result, auth_setup_ops())

  expect_null(state$result_parent_snapshot)
  expect_identical(
    state$parent_anchor_path,
    normalizePath(root, winslash = "/")
  )
  expect_false(is.null(state$parent_anchor_snapshot))
})

test_that("missing parent revalidation transitions to a frozen identity", {
  root <- withr::local_tempdir()
  result <- file.path(root, "missing", "nested", "app")
  state <- .viewerAuthPreflightSimple(result, auth_setup_ops())

  expect_invisible(.viewerAuthRevalidateParent(state))
  expect_null(state$result_parent_snapshot)

  expect_true(dir.create(state$result_parent, recursive = TRUE, mode = "0700"))
  if (.Platform$OS.type != "windows") {
    Sys.chmod(state$result_parent, "0700", use_umask = FALSE)
  }
  expect_invisible(.viewerAuthRevalidateParent(state))
  expect_false(is.null(state$result_parent_snapshot))
  frozen <- state$result_parent_snapshot
  expect_invisible(.viewerAuthRevalidateParent(state))
  expect_true(.viewerAuthSameDirectory(frozen, state$result_parent_snapshot))
})

test_that("environment names and passphrases use independent exact byte counts", {
  requested <- integer()
  calls <- 0L
  ops <- auth_setup_ops(
    c("fixture-user", "n"),
    list("fixture-password", "fixture-password")
  )
  ops$random_bytes <- function(size) {
    requested <<- c(requested, size)
    calls <<- calls + 1L
    if (calls == 1L) as.raw(rep(0xab, size)) else as.raw(rep(0xcd, size))
  }
  root <- withr::local_tempdir()
  state <- .viewerAuthPreflightSimple(file.path(root, "app"), ops)
  .viewerAuthCompleteSimple(state)

  expect_identical(requested, c(8L, 32L))
  expect_match(state$env_name, "^CEREBRO_AUTH_PASSPHRASE_[A-F0-9]{16}$")
  expect_match(state$passphrase, "^[a-f0-9]{64}$")
  expect_identical(
    state$env_name,
    paste0(
      "CEREBRO_AUTH_PASSPHRASE_",
      strrep("AB", 8L)
    )
  )
  expect_identical(state$passphrase, strrep("cd", 32L))
})

test_that("random byte length and environment-name collision bounds fail closed", {
  short <- auth_setup_ops(
    c("fixture-user", "n"),
    list("fixture-password", "fixture-password")
  )
  short$random_bytes <- function(size) as.raw(1L)
  root <- withr::local_tempdir()
  state <- .viewerAuthPreflightSimple(file.path(root, "app"), short)
  expect_error(.viewerAuthCompleteSimple(state), "random bytes", fixed = TRUE)

  calls <- 0L
  colliding <- auth_setup_ops()
  colliding$random_bytes <- function(size) {
    calls <<- calls + 1L
    as.raw(rep(1L, size))
  }
  colliding$getenv <- function(name, unset = NA_character_) "occupied"
  expect_error(
    .viewerAuthGenerateEnvironmentName(colliding, max_attempts = 100L),
    "100",
    fixed = TRUE
  )
  expect_identical(calls, 100L)
})

test_that("existing secret values are reused without random generation", {
  ops <- auth_setup_ops(
    c("fixture-user", "n"),
    list("fixture-password", "fixture-password")
  )
  ops$random_bytes <- function(size) stop("random generation was used")
  state <- viewer_auth_setup_fixture(TRUE, ops)

  expect_identical(
    state$env_name,
    "CEREBRO_AUTH_PASSPHRASE_0102030405060708"
  )
  expect_identical(state$passphrase, strrep("a", 64L))
  expect_null(state$candidate_path)
})

test_that("partial write failures remove the owned payload and scratch", {
  for (failure in c("false", "throw")) {
    root <- withr::local_tempdir()
    ops <- auth_setup_ops(
      c("fixture-user", "n"),
      list("fixture-password", "fixture-password")
    )
    ops$write_raw <- function(bytes, path) {
      writeBin(bytes[seq_len(17L)], path, useBytes = TRUE)
      if (identical(failure, "throw")) {
        stop("partial write fault")
      }
      FALSE
    }
    state <- .viewerAuthPreflightSimple(file.path(root, "app"), ops)
    .viewerAuthCompleteSimple(state)

    expect_error(
      .viewerAuthCreateSecretCandidate(state),
      "scratch payload",
      fixed = TRUE,
      info = failure
    )
    payload <- state$scratch_payload
    scratch <- state$scratch_dir
    expect_false(is.null(state$scratch_payload_snapshot), info = failure)
    expect_null(state$scratch_payload_snapshot$raw, info = failure)
    expect_gt(file.info(payload)$size[[1L]], 0)

    captured <- capture_all_auth_conditions(.viewerAuthFinishSimple(state))
    expect_false(.bundlePathExists(payload), info = failure)
    expect_false(.bundlePathExists(scratch), info = failure)
    expect_length(c(captured$warnings, captured$messages), 0L)
  }
})

test_that("partial write cleanup preserves a same-content foreign inode", {
  root <- withr::local_tempdir()
  ops <- auth_setup_ops(
    c("fixture-user", "n"),
    list("fixture-password", "fixture-password")
  )
  ops$write_raw <- function(bytes, path) {
    writeBin(bytes, path, useBytes = TRUE)
    FALSE
  }
  state <- .viewerAuthPreflightSimple(file.path(root, "app"), ops)
  .viewerAuthCompleteSimple(state)
  expect_error(.viewerAuthCreateSecretCandidate(state), "scratch payload")
  payload <- state$scratch_payload
  expected_inode <- state$scratch_payload_snapshot$inode
  bytes <- readBin(payload, "raw", n = 107L)

  foreign <- file.path(state$scratch_dir, "foreign")
  writeBin(bytes, foreign, useBytes = TRUE)
  if (.Platform$OS.type != "windows") {
    Sys.chmod(foreign, "0600", use_umask = FALSE)
  }
  foreign_inode <- as.numeric(fs::file_info(
    foreign,
    follow = FALSE
  )$inode[[1L]])
  expect_false(identical(expected_inode, foreign_inode))
  expect_true(unlink(payload, recursive = FALSE, force = FALSE) == 0L)
  expect_true(file.rename(foreign, payload))

  captured <- capture_all_auth_conditions(.viewerAuthFinishSimple(state))
  expect_true(.bundlePathExists(payload))
  residue <- c(captured$warnings, captured$messages)
  expect_true(any(grepl(payload, residue, fixed = TRUE)))
})

test_that("candidate preparation uses hard links and leaves an exact artifact", {
  state <- viewer_auth_setup_fixture()
  snapshot <- .viewerAuthReadSecretFile(state$candidate_path, state$ops)

  expect_identical(dirname(state$candidate_path), state$result_parent)
  expect_true(startsWith(
    basename(state$candidate_path),
    paste0(".", basename(state$secret_path), "-candidate-")
  ))
  expect_true(.viewerAuthSameArtifact(snapshot, state$candidate_snapshot))
  expect_identical(
    snapshot$raw,
    auth_secret_raw(
      state$env_name,
      state$passphrase
    )
  )
  expect_null(state$scratch_dir)
  expect_null(state$scratch_payload)
})

test_that("candidate hard-link failure has no rename or copy fallback", {
  root <- withr::local_tempdir()
  ops <- auth_setup_ops(
    c("fixture-user", "n"),
    list("fixture-password", "fixture-password")
  )
  ops$link <- function(from, to) FALSE
  state <- .viewerAuthPreflightSimple(file.path(root, "app"), ops)
  .viewerAuthCompleteSimple(state)

  expect_error(
    .viewerAuthCreateSecretCandidate(state),
    "candidate",
    fixed = TRUE
  )
  .viewerAuthFinishSimple(state)
  source <- readLines(testthat::test_path(
    "..",
    "..",
    "R",
    "viewer-auth-setup.R"
  ))
  expect_false(any(grepl("file.rename|file.copy", source)))
})

test_that("production hard links suppress an occupied-target warning", {
  root <- withr::local_tempdir()
  source <- file.path(root, "source")
  target <- file.path(root, "target")
  writeLines("source", source)
  writeLines("target", target)

  expect_false(withr::with_options(
    list(warn = 2),
    .viewerAuthSetupOps()$link(source, target)
  ))
  expect_identical(readLines(target), "target")
})

test_that("candidate hard-link race preserves the competing entry", {
  root <- withr::local_tempdir()
  ops <- auth_setup_ops(
    c("fixture-user", "n"),
    list("fixture-password", "fixture-password")
  )
  original_link <- ops$link
  raced_candidate <- NULL
  ops$link <- function(from, to) {
    raced_candidate <<- to
    writeBin(charToRaw("RACED"), to)
    original_link(from, to)
  }
  state <- .viewerAuthPreflightSimple(file.path(root, "app"), ops)
  .viewerAuthCompleteSimple(state)

  expect_error(
    .viewerAuthCreateSecretCandidate(state),
    "candidate",
    fixed = TRUE
  )
  expect_identical(readBin(raced_candidate, "raw", n = 5L), charToRaw("RACED"))
  expect_null(state$candidate_path)
  .viewerAuthFinishSimple(state)
  expect_true(.bundlePathExists(raced_candidate))
})

test_that("candidate provisional snapshot survives an immediate read failure", {
  root <- withr::local_tempdir()
  ops <- auth_setup_ops(
    c("fixture-user", "n"),
    list("fixture-password", "fixture-password")
  )
  original_link <- ops$link
  original_read <- ops$read_raw
  linked_source <- NULL
  linked_candidate <- NULL
  candidate_read_fault_reached <- FALSE
  ops$link <- function(from, to) {
    linked <- original_link(from, to)
    if (isTRUE(linked)) {
      linked_source <<- from
      linked_candidate <<- to
    }
    linked
  }
  ops$read_raw <- function(path) {
    if (!is.null(linked_candidate) && identical(path, linked_candidate)) {
      candidate_read_fault_reached <<- TRUE
      stop("candidate read fault")
    }
    original_read(path)
  }
  state <- .viewerAuthPreflightSimple(file.path(root, "app"), ops)
  .viewerAuthCompleteSimple(state)

  expect_error(
    .viewerAuthCreateSecretCandidate(state),
    "invalid or unsafe",
    fixed = TRUE
  )
  expect_identical(state$candidate_path, linked_candidate)
  expect_false(is.null(state$candidate_snapshot))
  expect_true(candidate_read_fault_reached)
  expect_true(.bundlePathExists(linked_candidate))
  source_info <- fs::file_info(linked_source, follow = FALSE)
  target_info <- fs::file_info(linked_candidate, follow = FALSE)
  expect_identical(
    as.numeric(source_info$device_id[[1L]]),
    as.numeric(target_info$device_id[[1L]])
  )
  expect_identical(
    as.numeric(source_info$inode[[1L]]),
    as.numeric(target_info$inode[[1L]])
  )
  state$ops$read_raw <- original_read
  .viewerAuthFinishSimple(state)
  expect_false(.bundlePathExists(linked_candidate))
})

test_that("new secret publication never clobbers a raced final path", {
  state <- viewer_auth_setup_fixture()
  writeBin(charToRaw("RACED"), state$secret_path)

  expect_error(
    .viewerAuthPublishSecret(state),
    "target changed",
    fixed = TRUE
  )
  expect_identical(
    readBin(state$secret_path, "raw", n = 5L),
    charToRaw("RACED")
  )
  .viewerAuthFinishSimple(state)
  expect_true(.bundlePathExists(state$secret_path))
})

test_that("final hard-link race preserves the competing entry", {
  state <- viewer_auth_setup_fixture()
  original_link <- state$ops$link
  state$ops$link <- function(from, to) {
    if (identical(to, state$secret_path)) {
      writeBin(charToRaw("RACED"), to)
    }
    original_link(from, to)
  }

  expect_error(
    .viewerAuthPublishSecret(state),
    "publish authentication secret",
    fixed = TRUE
  )
  expect_identical(
    readBin(state$secret_path, "raw", n = 5L),
    charToRaw("RACED")
  )
  .viewerAuthFinishSimple(state)
  expect_true(.bundlePathExists(state$secret_path))
})

test_that("published provisional snapshot survives an immediate read failure", {
  state <- viewer_auth_setup_fixture()
  candidate <- state$candidate_path
  original_read <- state$ops$read_raw
  original_link <- state$ops$link
  final_link_created <- FALSE
  final_link_identity <- NULL
  final_read_fault_reached <- FALSE
  state$ops$link <- function(from, to) {
    linked <- original_link(from, to)
    if (isTRUE(linked) && identical(to, state$secret_path)) {
      source_info <- fs::file_info(from, follow = FALSE)
      target_info <- fs::file_info(to, follow = FALSE)
      final_link_created <<- TRUE
      final_link_identity <<- list(
        source_device = as.numeric(source_info$device_id[[1L]]),
        source_inode = as.numeric(source_info$inode[[1L]]),
        target_device = as.numeric(target_info$device_id[[1L]]),
        target_inode = as.numeric(target_info$inode[[1L]])
      )
    }
    linked
  }
  state$ops$read_raw <- function(path) {
    if (identical(path, state$secret_path)) {
      final_read_fault_reached <<- TRUE
      stop("final read fault")
    }
    original_read(path)
  }

  captured <- capture_all_auth_conditions(.viewerAuthPublishSecret(state))
  expect_s3_class(captured$value, "error")
  expect_match(
    conditionMessage(captured$value),
    "invalid or unsafe",
    fixed = TRUE
  )
  expect_false(is.null(state$published_snapshot))
  expect_true(final_link_created)
  expect_true(final_read_fault_reached)
  expect_identical(
    final_link_identity$source_device,
    final_link_identity$target_device
  )
  expect_identical(
    final_link_identity$source_inode,
    final_link_identity$target_inode
  )
  state$ops$read_raw <- original_read
  .viewerAuthFinishSimple(state)
  expect_false(.bundlePathExists(state$secret_path))
  expect_false(.bundlePathExists(candidate))
})

test_that("payload cleanup refusal and foreign scratch children fail closed", {
  root <- withr::local_tempdir()
  ops <- auth_setup_ops(
    c("fixture-user", "n"),
    list("fixture-password", "fixture-password")
  )
  base_unlink <- ops$unlink_file
  ops$unlink_file <- function(path) {
    if (identical(basename(path), "payload")) {
      return(FALSE)
    }
    base_unlink(path)
  }
  state <- .viewerAuthPreflightSimple(file.path(root, "app"), ops)
  .viewerAuthCompleteSimple(state)
  expect_error(.viewerAuthCreateSecretCandidate(state), "scratch payload")
  expect_true(.bundlePathExists(state$scratch_payload))

  state$ops$unlink_file <- base_unlink
  writeLines("foreign", file.path(state$scratch_dir, "foreign-child"))
  captured <- capture_all_auth_conditions(.viewerAuthFinishSimple(state))
  expect_true(dir.exists(state$scratch_dir))
  expect_true(file.exists(file.path(state$scratch_dir, "foreign-child")))
  expect_true(length(c(captured$warnings, captured$messages)) > 0L)
})

test_that("candidate unlink refusal rolls final back before commit", {
  state <- viewer_auth_setup_fixture()
  original_unlink <- state$ops$unlink_file
  state$ops$unlink_file <- function(path) {
    if (identical(path, state$candidate_path)) {
      return(FALSE)
    }
    original_unlink(path)
  }

  captured <- capture_all_auth_conditions(.viewerAuthPublishSecret(state))
  expect_s3_class(captured$value, "error")
  expect_match(conditionMessage(captured$value), "candidate", fixed = TRUE)
  expect_false(state$committed)
  expect_false(state$ops$entry_exists(state$secret_path))
  expect_true(state$ops$entry_exists(state$candidate_path))
})

test_that("cleanup preserves foreign file and parent identities", {
  state <- viewer_auth_setup_fixture()
  candidate <- state$candidate_path
  expected_inode <- state$candidate_snapshot$inode
  bytes <- readBin(candidate, "raw", n = 107L)
  foreign <- file.path(state$result_parent, "foreign-candidate")
  write_auth_secret(foreign, bytes)
  foreign_inode <- as.numeric(fs::file_info(
    foreign,
    follow = FALSE
  )$inode[[1L]])
  expect_false(identical(expected_inode, foreign_inode))
  expect_true(unlink(candidate, recursive = FALSE, force = FALSE) == 0L)
  expect_true(file.rename(foreign, candidate))
  captured <- capture_all_auth_conditions(.viewerAuthFinishSimple(state))
  expect_true(.bundlePathExists(candidate))
  residue <- c(captured$warnings, captured$messages)
  expect_true(any(grepl(candidate, residue, fixed = TRUE)))

  root <- withr::local_tempdir()
  state <- viewer_auth_setup_fixture(envir = environment())
  candidate <- state$candidate_path
  moved <- paste0(state$result_parent, "-moved")
  expect_true(file.rename(state$result_parent, moved))
  dir.create(state$result_parent)
  on.exit(unlink(moved, recursive = TRUE, force = TRUE), add = TRUE)
  captured <- capture_all_auth_conditions(.viewerAuthFinishSimple(state))
  expect_true(.bundlePathExists(file.path(moved, basename(candidate))))
  expect_true(length(c(captured$warnings, captured$messages)) > 0L)
})

test_that("environment rollback preserves absent empty and valued states", {
  for (prior in list(NA_character_, "", "prior-value")) {
    Sys.unsetenv("CEREBRO_AUTH_PASSPHRASE_0102030405060708")
    state <- viewer_auth_setup_fixture()
    name <- state$env_name
    if (is.na(prior)) {
      Sys.unsetenv(name)
    } else {
      do.call(Sys.setenv, stats::setNames(list(prior), name))
    }
    .viewerAuthInstallEnvironment(state)
    expect_identical(Sys.getenv(name, unset = NA_character_), state$passphrase)
    .viewerAuthRollbackSimple(state)
    expect_identical(Sys.getenv(name, unset = NA_character_), prior)
  }
})

test_that("environment installation rolls back false throw and readback failure", {
  failures <- c("false", "throw", "mismatch")
  for (failure in failures) {
    Sys.unsetenv("CEREBRO_AUTH_PASSPHRASE_0102030405060708")
    state <- viewer_auth_setup_fixture()
    name <- state$env_name
    do.call(Sys.setenv, stats::setNames(list("prior-value"), name))
    real_set <- state$ops$setenv
    if (identical(failure, "false")) {
      failed <- FALSE
      state$ops$setenv <- function(name, value) {
        if (!failed) {
          failed <<- TRUE
          return(FALSE)
        }
        real_set(name, value)
      }
    } else if (identical(failure, "throw")) {
      failed <- FALSE
      state$ops$setenv <- function(name, value) {
        result <- real_set(name, value)
        if (!failed) {
          failed <<- TRUE
          stop("setenv fault")
        }
        result
      }
    } else {
      mismatch_reached <- FALSE
      getenv_calls <- 0L
      real_get <- state$ops$getenv
      state$ops$getenv <- function(name, unset = NA_character_) {
        getenv_calls <<- getenv_calls + 1L
        if (getenv_calls == 1L) {
          return(real_get(name, unset = unset))
        }
        mismatch_reached <<- TRUE
        "readback-mismatch"
      }
    }

    expect_error(.viewerAuthInstallEnvironment(state), "environment")
    if (identical(failure, "mismatch")) {
      expect_true(mismatch_reached)
    }
    state$ops$getenv <- .viewerAuthSetupOps()$getenv
    .viewerAuthRollbackSimple(state)
    expect_identical(Sys.getenv(name, unset = NA_character_), "prior-value")
  }
})

test_that("commit retains environment and artifacts while finish clears secrets", {
  Sys.unsetenv("CEREBRO_AUTH_PASSPHRASE_0102030405060708")
  state <- viewer_auth_setup_fixture()
  .viewerAuthPublishSecret(state)
  .viewerAuthInstallEnvironment(state)
  name <- state$env_name
  passphrase <- state$passphrase
  secret <- state$secret_path
  .viewerAuthCommitSimple(state)
  .viewerAuthFinishSimple(state)

  expect_true(.bundlePathExists(secret))
  expect_identical(Sys.getenv(name), passphrase)
  expect_null(state$accounts)
  expect_null(state$passphrase)
  expect_null(state$existing_snapshot)
  expect_null(state$published_snapshot)
  expect_null(state$prior_env)
  Sys.unsetenv(name)
})

test_that("cleanup under warn equals two does not mask the original error", {
  Sys.unsetenv("CEREBRO_AUTH_PASSPHRASE_0102030405060708")
  state <- viewer_auth_setup_fixture()
  original <- simpleError("original setup fault")
  unlink(state$candidate_path)
  write_auth_secret(
    state$candidate_path,
    auth_secret_raw(
      "CEREBRO_AUTH_PASSPHRASE_FFFFFFFFFFFFFFFF",
      strrep("b", 64L)
    )
  )

  messages <- character()
  stdout <- capture.output(
    returned <- withCallingHandlers(
      withr::with_options(
        list(warn = 2),
        tryCatch(
          stop(original),
          error = function(condition) {
            .viewerAuthFinishSimple(state)
            condition
          }
        )
      ),
      message = function(condition) {
        messages <<- c(messages, conditionMessage(condition))
        invokeRestart("muffleMessage")
      }
    )
  )
  expect_identical(returned, original)
  expect_identical(conditionMessage(returned), "original setup fault")
  expect_true(any(startsWith(messages, "Warning: ")))
  expect_true(any(grepl(state$candidate_path, messages, fixed = TRUE)))
  expect_length(stdout, 0L)
})

test_that("authentication setup conditions redact all supplied secrets", {
  leaked <- c("fixture-user", "fixture-password", strrep("a", 64L))
  root <- withr::local_tempdir()
  path <- write_auth_secret(file.path(root, "app.auth.env"))
  ops <- .viewerAuthSetupOps()
  ops$read_raw <- function(path) stop("raw read fault")
  captured <- capture_all_auth_conditions(.viewerAuthReadSecretFile(path, ops))
  channels <- c(
    if (inherits(captured$value, "error")) {
      conditionMessage(captured$value)
    } else {
      ""
    },
    captured$messages,
    captured$warnings,
    captured$stdout
  )
  for (secret in leaked) {
    expect_false(any(grepl(secret, channels, fixed = TRUE)))
  }
})
