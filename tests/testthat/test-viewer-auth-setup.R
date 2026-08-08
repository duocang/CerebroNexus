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
      "namespace_available"
    )
  )
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
