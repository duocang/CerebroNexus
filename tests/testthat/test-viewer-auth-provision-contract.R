provision_accounts <- function(
  n = 1L,
  user = "alice",
  password = "secret-password"
) {
  data.frame(
    user = rep(user, n),
    password = rep(password, n),
    stringsAsFactors = FALSE
  )
}

test_that("accounts normalize to one strict provider table", {
  accounts <- provision_accounts(user = " alice ")
  got <- CerebroNexus:::.viewerAuthNormalizeAccounts(accounts)
  expect_identical(class(got), "data.frame")
  expect_identical(got$user, "alice")
  expect_identical(got$password, "secret-password")
  expect_identical(got$admin, FALSE)
  bads <- list(
    accounts[0, ],
    provision_accounts(1001L),
    data.frame(
      user = "a",
      password = "secret-password",
      extra = "x",
      stringsAsFactors = FALSE
    ),
    data.frame(user = "a", stringsAsFactors = FALSE),
    structure(accounts, names = c("user", "user")),
    structure(accounts, class = c("tbl_df", "tbl", "data.frame")),
    data.frame(user = factor("a"), password = "secret-password"),
    data.frame(user = I(list("a")), password = "secret-password"),
    data.frame(user = NA_character_, password = "secret-password"),
    data.frame(user = "a", password = NA_character_)
  )
  for (bad in bads) {
    expect_provision_error(
      CerebroNexus:::.viewerAuthNormalizeAccounts(bad),
      "invalid_accounts",
      "input"
    )
  }
  expect_provision_error(
    CerebroNexus:::.viewerAuthNormalizeAccounts(data.frame(
      user = c(" a", "a "),
      password = rep("secret-password", 2)
    )),
    "invalid_accounts",
    "input"
  )
  expect_provision_error(
    CerebroNexus:::.viewerAuthNormalizeAccounts(data.frame(
      user = "a",
      password = "secret-password",
      admin = 1
    )),
    "invalid_accounts",
    "input"
  )
  expect_provision_error(
    CerebroNexus:::.viewerAuthNormalizeAccounts(data.frame(
      user = "a",
      password = "secret-password",
      admin = NA
    )),
    "invalid_accounts",
    "input"
  )
})

test_that("provision identifiers are deterministic under injected randomness", {
  ops <- viewer_auth_provision_test_ops(
    random_values = list(as.raw(0:15), as.raw(16:47), as.raw(48:55))
  )
  state <- CerebroNexus:::.viewerAuthNewProvisionState(
    accounts = provision_accounts(),
    options = list(
      target_dir = "/private/auth",
      passphrase_env = NULL,
      timeout_minutes = 15L,
      install_env = FALSE
    ),
    ops = ops
  )
  state$preflight <- list(target_path = "/private/auth")
  CerebroNexus:::.viewerAuthPrepareProvisionIdentity(state)
  expect_identical(
    state$identity$operation_id,
    paste(sprintf("%02x", 0:15), collapse = "")
  )
  expect_identical(
    state$identity$passphrase_env,
    "CEREBRO_AUTH_PASSPHRASE_3031323334353637"
  )
  expect_identical(nchar(state$passphrase), 64L)
})

test_that("every random draw has one exact raw length", {
  cases <- list(
    character(8L),
    as.raw(1:7),
    as.raw(1:9),
    character(16L),
    as.raw(1:15),
    as.raw(c(1:16, 17)),
    character(32L),
    as.raw(1:31)
  )
  sizes <- c(8L, 8L, 8L, 16L, 16L, 16L, 32L, 32L)
  for (i in seq_along(cases)) {
    expect_provision_error(
      CerebroNexus:::.viewerAuthRandomRaw(
        viewer_auth_provision_test_ops(random_bytes = function(size) {
          cases[[i]]
        }),
        sizes[[i]],
        "test"
      ),
      "secret_generation_failed",
      "preflight"
    )
  }
})

test_that("environment name generation is fail closed", {
  calls <- 0L
  ops <- viewer_auth_provision_test_ops(
    random_bytes = function(size) {
      calls <<- calls + 1L
      as.raw(rep(calls %% 255L, size))
    },
    getenv = function(name) ""
  )
  expect_provision_error(
    CerebroNexus:::.viewerAuthResolveEnvironmentName(NULL, ops),
    "environment_conflict",
    "environment"
  )
  expect_identical(calls, 100L)
  for (reader in list(
    function(name) stop("sentinel", call. = FALSE),
    function(name) character(),
    function(name) FALSE
  )) {
    condition <- expect_provision_error(
      CerebroNexus:::.viewerAuthResolveEnvironmentName(
        "CEREBRO_AUTH_KEY",
        viewer_auth_provision_test_ops(getenv = reader)
      ),
      "environment_conflict",
      "environment"
    )
    expect_false(grepl("sentinel", conditionMessage(condition), fixed = TRUE))
  }
})

test_that("recovery metadata has one exact schema", {
  operation_id <- paste(rep("a", 32L), collapse = "")
  owner <- CerebroNexus:::.viewerAuthOwnerManifest(
    operation_id,
    "/private/auth",
    paste0(".cerebro-auth-0123456789abcdef-", operation_id, ".stage"),
    "2026-08-08T21:50:46Z",
    "claimed"
  )
  expect_true(CerebroNexus:::.viewerAuthValidOwnerManifest(owner))
  owner$extra <- TRUE
  expect_false(CerebroNexus:::.viewerAuthValidOwnerManifest(owner))
  provision <- CerebroNexus:::.viewerAuthProvisionManifest(
    operation_id,
    "ready",
    "2026-08-08T21:50:46Z",
    "CEREBRO_AUTH_KEY",
    15L,
    2L
  )
  expect_true(CerebroNexus:::.viewerAuthValidProvisionManifest(provision))
  provision$artifacts[["secret"]] <- "/absolute/viewer-auth.env"
  expect_false(CerebroNexus:::.viewerAuthValidProvisionManifest(provision))
})

test_that("transaction state is a private environment with exact fields", {
  state <- CerebroNexus:::.viewerAuthNewProvisionState(
    provision_accounts(),
    list(
      target_dir = "/private/auth",
      passphrase_env = NULL,
      timeout_minutes = 15L,
      install_env = FALSE
    ),
    viewer_auth_provision_test_ops()
  )
  expect_true(is.environment(state))
  expect_identical(parent.env(state), emptyenv())
  expect_identical(
    sort(ls(state, all.names = TRUE)),
    sort(CerebroNexus:::.viewerAuthProvisionStateFields)
  )
  expect_false(state$lock_claimed)
  expect_false(state$committed)
})

test_that("account scalar, row-count, and byte limits are exact", {
  for (control in c(1:31, 127)) {
    for (field in c("user", "password")) {
      if (identical(field, "user") && control %in% c(9L, 10L, 11L, 12L, 13L)) {
        next
      }
      bad <- provision_accounts()
      bad[[field]] <- paste0(
        if (field == "user") "a" else "secret-password",
        rawToChar(as.raw(control))
      )
      expect_provision_error(
        CerebroNexus:::.viewerAuthNormalizeAccounts(bad),
        "invalid_accounts",
        "input"
      )
    }
  }
  for (user in c(
    paste(rep("a", 128), collapse = ""),
    paste(rep("é", 64), collapse = "")
  )) {
    expect_s3_class(
      CerebroNexus:::.viewerAuthNormalizeAccounts(provision_accounts(
        user = user
      )),
      "data.frame"
    )
  }
  for (user in c(
    paste(rep("a", 129), collapse = ""),
    rawToChar(as.raw(c(0xc3, 0x28)))
  )) {
    expect_provision_error(
      CerebroNexus:::.viewerAuthNormalizeAccounts(provision_accounts(
        user = user
      )),
      "invalid_accounts",
      "input"
    )
  }
  for (n in c(11L, 1025L)) {
    expect_provision_error(
      CerebroNexus:::.viewerAuthNormalizeAccounts(provision_accounts(
        password = paste(rep("x", n), collapse = "")
      )),
      "invalid_accounts",
      "input"
    )
  }
  for (n in c(12L, 1024L)) {
    expect_s3_class(
      CerebroNexus:::.viewerAuthNormalizeAccounts(provision_accounts(
        password = paste(rep("x", n), collapse = "")
      )),
      "data.frame"
    )
  }
  thousand <- data.frame(
    user = sprintf("u%04d", 1:1000),
    password = rep("secret-password", 1000),
    stringsAsFactors = FALSE
  )
  expect_s3_class(
    CerebroNexus:::.viewerAuthNormalizeAccounts(thousand),
    "data.frame"
  )
})

test_that("provision options normalize to a secret-free exact list", {
  got <- CerebroNexus:::.viewerAuthNormalizeProvisionOptions(
    "/tmp/a",
    NULL,
    30,
    TRUE
  )
  expect_identical(
    names(got),
    c("target_dir", "passphrase_env", "timeout_minutes", "install_env")
  )
  expect_identical(got$timeout_minutes, 30L)
  expect_null(got$passphrase_env)
  expect_true(got$install_env)
  bads <- list(
    list(c("/tmp/a", "/tmp/b"), NULL, 1, TRUE),
    list("\t/tmp/a", NULL, 1, TRUE),
    list("/tmp/a", "1BAD", 1, TRUE),
    list("/tmp/a", c("A", "B"), 1, TRUE),
    list("/tmp/a", NULL, TRUE, TRUE),
    list("/tmp/a", NULL, 1.5, TRUE),
    list("/tmp/a", NULL, 0, TRUE),
    list("/tmp/a", NULL, 1441, TRUE),
    list("/tmp/a", NULL, 1, NA),
    list("/tmp/a", NULL, 1, c(TRUE, FALSE))
  )
  for (args in bads) {
    expect_provision_error(
      do.call(CerebroNexus:::.viewerAuthNormalizeProvisionOptions, args),
      "invalid_options",
      "input"
    )
  }
})

test_that("provision conditions expose stable machine fields", {
  err <- CerebroNexus:::.viewerAuthProvisionCondition(
    "cleanup_incomplete",
    "cleanup",
    "cleanup",
    recovery_path = tempdir()
  )
  expect_s3_class(err, "cerebro_viewer_auth_cleanup_incomplete")
  expect_s3_class(err, "cerebro_viewer_auth_provision_error")
  expect_null(err$call)
  expect_error(CerebroNexus:::.viewerAuthProvisionCondition(
    "cleanup_incomplete",
    "cleanup",
    "cleanup"
  ))
  expect_error(CerebroNexus:::.viewerAuthProvisionCondition(
    "invalid_accounts",
    "input",
    "x",
    cause_code = "bad"
  ))
  expect_error(CerebroNexus:::.viewerAuthProvisionCondition(
    "invalid_accounts",
    "input",
    "x",
    recovery_path = "relative"
  ))
  auth <- list(
    credentials = "/tmp/credentials",
    passphrase_env = "AUTH_SECRET",
    timeout_minutes = 30L
  )
  result <- CerebroNexus:::.viewerAuthProvisionResult(
    auth,
    "/tmp/secret",
    "/tmp/manifest",
    1L,
    TRUE
  )
  expect_identical(
    names(result),
    c(
      "schema_version",
      "auth",
      "secret_file",
      "manifest_file",
      "user_count",
      "environment_installed"
    )
  )
  output <- capture.output(print(result))
  expect_false(any(grepl(
    "alice|secret-password|/tmp/secret",
    output,
    ignore.case = TRUE
  )))
  expect_true(any(grepl("AUTH_SECRET", output, fixed = TRUE)))
})

test_that("UTF-8 and ASCII-control boundaries are exact", {
  expect_provision_error(
    CerebroNexus:::.viewerAuthNormalizeAccounts(provision_accounts(
      user = paste0("alice", rawToChar(as.raw(1L)))
    )),
    "invalid_accounts",
    "input"
  )
  expect_s3_class(
    CerebroNexus:::.viewerAuthNormalizeAccounts(provision_accounts(
      user = paste(rep("é", 64L), collapse = "")
    )),
    "data.frame"
  )
  expect_provision_error(
    CerebroNexus:::.viewerAuthNormalizeAccounts(provision_accounts(
      user = rawToChar(as.raw(c(0xc3, 0x28)))
    )),
    "invalid_accounts",
    "input"
  )
})

test_that("provision error and warning factories preserve condition semantics", {
  expect_true(is.function(CerebroNexus:::.viewerAuthProvisionError))
  expect_true(is.function(CerebroNexus:::.viewerAuthProvisionWarning))
  expect_true(is.function(CerebroNexus:::.viewerAuthProvisionCleanupWarning))
  error <- CerebroNexus:::.viewerAuthProvisionError(
    "invalid_accounts",
    "input",
    "x"
  )
  warning <- CerebroNexus:::.viewerAuthProvisionWarning(
    "missing_dependency",
    "dependency",
    "x"
  )
  cleanup <- CerebroNexus:::.viewerAuthProvisionCleanupWarning("x", tempdir())
  expect_s3_class(error, "error")
  expect_s3_class(warning, "warning")
  expect_s3_class(cleanup, "warning")
  expect_s3_class(cleanup, "cerebro_viewer_auth_cleanup_incomplete")
  expect_identical(error$code, "invalid_accounts")
  expect_identical(warning$stage, "dependency")
})

test_that("user is trimmed and password whitespace is retained", {
  got <- CerebroNexus:::.viewerAuthNormalizeAccounts(provision_accounts(
    user = " alice ",
    password = " password-123 "
  ))
  expect_identical(got$user, "alice")
  expect_identical(got$password, " password-123 ")
  expect_identical(
    CerebroNexus:::.viewerAuthNormalizeAccounts(provision_accounts(
      user = "\talice\n"
    ))$user,
    "alice"
  )
  expect_s3_class(
    CerebroNexus:::.viewerAuthNormalizeAccounts(provision_accounts(
      password = paste(rep("x", 1024), collapse = "")
    )),
    "data.frame"
  )
  expect_provision_error(
    CerebroNexus:::.viewerAuthNormalizeAccounts(provision_accounts(
      password = paste(rep("x", 1025), collapse = "")
    )),
    "invalid_accounts",
    "input"
  )
})

test_that("accounts accept required columns in any order", {
  accounts <- data.frame(
    admin = TRUE,
    password = "secret-password",
    user = "alice",
    stringsAsFactors = FALSE
  )
  got <- CerebroNexus:::.viewerAuthNormalizeAccounts(accounts)
  expect_identical(names(got), c("user", "password", "admin"))
})

test_that("provision warnings do not inherit the error-only class", {
  warning <- CerebroNexus:::.viewerAuthProvisionWarning(
    "missing_dependency",
    "dependency",
    "x"
  )
  cleanup <- CerebroNexus:::.viewerAuthProvisionCleanupWarning("x", tempdir())
  expect_false(inherits(warning, "cerebro_viewer_auth_provision_error"))
  expect_false(inherits(cleanup, "cerebro_viewer_auth_provision_error"))
})

test_that("result constructor copies only the safe auth contract", {
  auth <- list(
    credentials = "/tmp/credentials",
    passphrase_env = "AUTH_SECRET",
    timeout_minutes = 30L
  )
  result <- CerebroNexus:::.viewerAuthProvisionResult(
    auth,
    "/tmp/secret",
    "/tmp/manifest",
    1L,
    TRUE
  )
  expect_identical(
    names(result$auth),
    c("credentials", "passphrase_env", "timeout_minutes")
  )
  poisoned <- c(
    auth,
    list(
      user = "ACCOUNT-SENTINEL",
      password = "PASSWORD-SENTINEL",
      passphrase = "PASSPHRASE-SENTINEL"
    )
  )
  err <- tryCatch(
    CerebroNexus:::.viewerAuthProvisionResult(
      poisoned,
      "/tmp/secret",
      "/tmp/manifest",
      1L,
      TRUE
    ),
    error = identity
  )
  expect_s3_class(err, "error")
  expect_false(grepl(
    "ACCOUNT-SENTINEL|PASSWORD-SENTINEL|PASSPHRASE-SENTINEL",
    conditionMessage(err)
  ))
  expect_false(any(grepl(
    "ACCOUNT-SENTINEL|PASSWORD-SENTINEL|PASSPHRASE-SENTINEL",
    capture.output(print(result))
  )))
})
