.builder_auth_env_name <- "CEREBRO_AUTH_PASSPHRASE"
.builder_auth_timeout_minutes <- 15L
.builder_auth_max_accounts <- 50L

builder_auth_empty_accounts <- function() {
  structure(list(), class = c("builder_auth_accounts", "list"))
}

builder_auth_validate_payload <- function(enabled, accounts) {
  invalid <- function(message) {
    list(ok = FALSE, error = message, accounts = NULL)
  }
  if (!is.logical(enabled) || length(enabled) != 1L || is.na(enabled)) {
    return(invalid("The login setting is invalid."))
  }
  if (!isTRUE(enabled)) {
    return(list(
      ok = TRUE,
      error = NULL,
      accounts = builder_auth_empty_accounts()
    ))
  }
  if (!is.list(accounts) || !length(accounts)) {
    return(invalid("Add at least one login account."))
  }
  if (is.object(accounts)) {
    if (
      !identical(
        attr(accounts, "class", exact = TRUE),
        c("builder_auth_accounts", "list")
      )
    ) {
      return(invalid("The login accounts are invalid."))
    }
    accounts <- unclass(accounts)
  }
  if (length(accounts) > .builder_auth_max_accounts) {
    return(invalid("Login supports at most 50 accounts."))
  }
  normalized <- vector("list", length(accounts))
  for (index in seq_along(accounts)) {
    account <- accounts[[index]]
    expected <- c("id", "username", "password")
    if (
      !is.list(account) ||
        is.object(account) ||
        !identical(sort(names(account)), sort(expected))
    ) {
      return(invalid(paste0("Account ", index, " is incomplete.")))
    }
    scalar_text <- function(value) {
      is.character(value) &&
        length(value) == 1L &&
        !is.na(value)
    }
    if (
      !scalar_text(account$id) ||
        !grepl("^auth-account-[1-9][0-9]*$", account$id)
    ) {
      return(invalid(paste0("Account ", index, " has an invalid row.")))
    }
    username <- if (scalar_text(account$username)) {
      trimws(account$username)
    } else {
      ""
    }
    if (!nzchar(username)) {
      return(invalid(paste0("Account ", index, " needs a username.")))
    }
    password <- account$password
    if (!scalar_text(password) || !nzchar(password) || nchar(password) < 8L) {
      return(invalid(paste0(
        "Account ",
        index,
        " needs a password of at least 8 characters."
      )))
    }
    normalized[[index]] <- list(
      id = account$id,
      username = username,
      password = password
    )
  }
  ids <- vapply(normalized, `[[`, character(1), "id")
  users <- vapply(normalized, `[[`, character(1), "username")
  if (anyDuplicated(ids)) {
    return(invalid("Each login row must be unique."))
  }
  if (anyDuplicated(users)) {
    return(invalid("Usernames must be unique."))
  }
  list(
    ok = TRUE,
    error = NULL,
    accounts = structure(
      normalized,
      class = c("builder_auth_accounts", "list")
    )
  )
}

builder_auth_summary <- function(enabled, accounts) {
  parsed <- builder_auth_validate_payload(enabled, accounts)
  if (!isTRUE(parsed$ok)) {
    stop(parsed$error, call. = FALSE)
  }
  list(
    enabled = isTRUE(enabled),
    account_count = as.integer(length(parsed$accounts)),
    timeout_minutes = .builder_auth_timeout_minutes
  )
}
