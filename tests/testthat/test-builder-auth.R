builder_repo_source("prerequisite.R")
builder_repo_source("app_bundle.R")

test_that("Builder App bundle loads the auth contract before request helpers", {
  path <- testthat::test_path("..", "..", "inst", "builder", "app_bundle.R")
  lines <- readLines(path, warn = FALSE)
  auth_source <- grep('"auth.R"', lines, fixed = TRUE)
  contract_source <- grep('"contract.R"', lines, fixed = TRUE)

  expect_length(auth_source, 1L)
  expect_length(contract_source, 1L)
  expect_lt(auth_source, contract_source)
})

test_that("Builder auth normalizes usernames but never passwords", {
  parsed <- builder_auth_validate_payload(
    enabled = TRUE,
    accounts = builder_auth_test_accounts()
  )

  expect_true(parsed$ok)
  expect_s3_class(parsed$accounts, "builder_auth_accounts")
  expect_identical(parsed$accounts[[1L]]$username, "auth-user-a-7f31")
  expect_identical(
    parsed$accounts[[1L]]$password,
    "auth-password-a-7f31"
  )
  expect_identical(
    builder_auth_summary(TRUE, parsed$accounts),
    list(enabled = TRUE, account_count = 2L, timeout_minutes = 15L)
  )
})

test_that("Builder auth rejects incomplete or duplicate accounts safely", {
  cases <- list(
    empty = list(),
    blank_user = list(list(
      id = "auth-account-1",
      username = "  ",
      password = "auth-password-blank-13a9"
    )),
    short_password = list(list(
      id = "auth-account-1",
      username = "auth-user-short-24b8",
      password = "p24b8"
    )),
    duplicate_user = list(
      list(
        id = "auth-account-1",
        username = "auth-user-duplicate-35c7",
        password = "auth-password-first-35c7"
      ),
      list(
        id = "auth-account-2",
        username = " auth-user-duplicate-35c7 ",
        password = "auth-password-second-46d6"
      )
    )
  )
  forbidden <- c(
    "auth-password-blank-13a9",
    "auth-user-short-24b8",
    "p24b8",
    "auth-user-duplicate-35c7",
    "auth-password-first-35c7",
    "auth-password-second-46d6"
  )

  for (name in names(cases)) {
    parsed <- builder_auth_validate_payload(TRUE, cases[[name]])
    expect_false(parsed$ok, info = name)
    expect_null(parsed$accounts, info = name)
    expect_false(
      any(vapply(forbidden, grepl, logical(1), x = parsed$error, fixed = TRUE)),
      info = name
    )
  }
  expect_identical(
    builder_auth_summary(FALSE, builder_auth_empty_accounts()),
    list(enabled = FALSE, account_count = 0L, timeout_minutes = 15L)
  )
})

test_that("Builder auth accepts only the strict account payload boundary", {
  valid_password <- "password"
  cases <- list(
    invalid_enabled = list(
      enabled = NA,
      accounts = builder_auth_test_accounts()
    ),
    non_list = list(enabled = TRUE, accounts = "not-a-list"),
    missing_field = list(
      enabled = TRUE,
      accounts = list(list(
        id = "auth-account-1",
        username = "auth-user-missing-52e1"
      ))
    ),
    duplicate_id = list(
      enabled = TRUE,
      accounts = list(
        list(
          id = "auth-account-1",
          username = "auth-user-id-a-62f1",
          password = valid_password
        ),
        list(
          id = "auth-account-1",
          username = "auth-user-id-b-73a2",
          password = valid_password
        )
      )
    ),
    too_many = list(
      enabled = TRUE,
      accounts = lapply(seq_len(51L), function(i) {
        list(
          id = paste0("auth-account-", i),
          username = paste0("auth-user-many-", i),
          password = valid_password
        )
      })
    )
  )

  for (name in names(cases)) {
    parsed <- builder_auth_validate_payload(
      cases[[name]]$enabled,
      cases[[name]]$accounts
    )
    expect_false(parsed$ok, info = name)
    expect_null(parsed$accounts, info = name)
  }
})

test_that("Builder auth preserves an exactly eight-character password", {
  parsed <- builder_auth_validate_payload(
    TRUE,
    list(list(
      id = "auth-account-1",
      username = " auth-user-eight-84b3 ",
      password = "eight888"
    ))
  )

  expect_true(parsed$ok)
  expect_identical(parsed$accounts[[1L]]$username, "auth-user-eight-84b3")
  expect_identical(parsed$accounts[[1L]]$password, "eight888")
})

test_that("disabled Builder auth discards browser account residue", {
  parsed <- builder_auth_validate_payload(TRUE, builder_auth_test_accounts())
  disabled <- builder_auth_validate_payload(FALSE, parsed$accounts)

  expect_true(disabled$ok)
  expect_s3_class(disabled$accounts, "builder_auth_accounts")
  expect_length(disabled$accounts, 0L)
  expect_false(builder_auth_value_contains(disabled, "auth-user-a-7f31"))
  expect_false(builder_auth_value_contains(disabled, "auth-password-a-7f31"))
})
