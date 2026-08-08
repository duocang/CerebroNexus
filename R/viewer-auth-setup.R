.viewerAuthSetupOps <- function() {
  list(
    is_interactive = function() base::interactive(),
    read_input = function(prompt) base::readline(prompt),
    read_password = function(prompt) askpass::askpass(prompt),
    random_bytes = function(size) openssl::rand_bytes(size),
    namespace_available = function(package) {
      requireNamespace(package, quietly = TRUE)
    }
  )
}

.viewerAuthRequireDependencies <- function(ops) {
  packages <- c("shinymanager", "askpass", "openssl")
  missing <- packages[
    !vapply(packages, ops$namespace_available, logical(1))
  ]
  if (length(missing) > 0L) {
    stop(
      "Interactive authentication requires: ",
      paste(missing, collapse = ", "),
      ". Install the missing package(s) and retry.",
      call. = FALSE
    )
  }
  .viewerAuthProviderAvailable()
  invisible(TRUE)
}

.viewerAuthHex <- function(bytes, uppercase) {
  value <- paste(sprintf("%02x", as.integer(bytes)), collapse = "")
  if (isTRUE(uppercase)) {
    toupper(value)
  } else {
    value
  }
}

.viewerAuthCollectAccounts <- function(ops) {
  cancel <- function() {
    stop("Interactive authentication setup was cancelled.", call. = FALSE)
  }
  accounts <- data.frame(
    user = character(),
    password = character(),
    admin = logical(),
    stringsAsFactors = FALSE
  )

  repeat {
    username <- ops$read_input("Username: ")
    if (
      is.null(username) ||
        length(username) != 1L ||
        is.na(username)
    ) {
      cancel()
    }
    username <- trimws(username)
    if (!nzchar(username)) {
      cancel()
    }
    if (username %in% accounts$user) {
      message("Username already exists.")
      next
    }

    repeat {
      password <- ops$read_password("Password: ")
      if (is.null(password)) {
        cancel()
      }
      if (
        length(password) != 1L ||
          is.na(password) ||
          !nzchar(password)
      ) {
        message("Password must not be empty.")
        next
      }
      confirmation <- ops$read_password("Confirm password: ")
      if (is.null(confirmation)) {
        cancel()
      }
      if (!identical(password, confirmation)) {
        message("Passwords do not match.")
        next
      }
      break
    }

    accounts <- rbind(
      accounts,
      data.frame(
        user = username,
        password = password,
        admin = FALSE,
        stringsAsFactors = FALSE
      )
    )

    repeat {
      continuation <- ops$read_input("Add another user? [y/N]: ")
      if (
        is.null(continuation) ||
          length(continuation) != 1L ||
          is.na(continuation)
      ) {
        continuation <- ""
      }
      continuation <- tolower(trimws(continuation))
      if (continuation %in% c("y", "yes")) {
        break
      }
      if (continuation %in% c("", "n", "no")) {
        return(accounts)
      }
      message("Please enter y or n.")
    }
  }
}
