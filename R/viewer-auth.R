.viewerAuthScalarCharacter <- function(value, field) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(value)
  ) {
    stop(
      paste0("auth$", field, " must be one non-empty string."),
      call. = FALSE
    )
  }
  attributes(value) <- NULL
  value
}

.viewerAuthProviderAvailable <- function() {
  available <- requireNamespace("shinymanager", quietly = TRUE)
  current <- if (available) {
    tryCatch(
      utils::packageVersion("shinymanager"),
      error = function(condition) NULL
    )
  } else {
    NULL
  }
  if (is.null(current) || current < "1.1.0") {
    stop(
      "Authentication requires shinymanager (>= 1.1.0).",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.viewerAuthDatabaseError <- function(passphrase_env) {
  stop(
    paste0(
      "auth$credentials must be a readable shinymanager database using ",
      passphrase_env,
      "."
    ),
    call. = FALSE
  )
}

.viewerAuthReadSecret <- function(passphrase_env) {
  passphrase <- Sys.getenv(passphrase_env, unset = NA_character_)
  if (
    length(passphrase) != 1L ||
      is.na(passphrase) ||
      nchar(passphrase, type = "bytes") < 32L
  ) {
    stop(
      paste0(
        "Authentication passphrase environment variable ",
        passphrase_env,
        " must contain at least 32 bytes."
      ),
      call. = FALSE
    )
  }
  passphrase
}

.viewerAuthPreflightDatabase <- function(path, passphrase_env) {
  .viewerAuthProviderAvailable()
  passphrase <- .viewerAuthReadSecret(passphrase_env)
  credentials <- NULL
  pwd_mngt <- NULL
  logs <- NULL
  on.exit(
    {
      passphrase <- NULL
      credentials <- NULL
      pwd_mngt <- NULL
      logs <- NULL
    },
    add = TRUE
  )

  read_table <- function(name) {
    suppressWarnings(suppressMessages(tryCatch(
      shinymanager::read_db_decrypt(
        conn = path,
        name = name,
        passphrase = passphrase
      ),
      error = function(condition) NULL
    )))
  }
  credentials <- read_table("credentials")
  pwd_mngt <- read_table("pwd_mngt")
  logs <- read_table("logs")

  required <- list(
    credentials = c(
      "user",
      "password",
      "start",
      "expire",
      "admin",
      "is_hashed_password"
    ),
    pwd_mngt = c(
      "user",
      "must_change",
      "have_changed",
      "date_change",
      "n_wrong_pwd"
    ),
    logs = c("user", "server_connected", "token", "logout", "app")
  )
  tables <- list(
    credentials = credentials,
    pwd_mngt = pwd_mngt,
    logs = logs
  )
  valid_tables <- vapply(
    names(required),
    function(name) {
      value <- tables[[name]]
      is.data.frame(value) &&
        !anyDuplicated(names(value)) &&
        all(required[[name]] %in% names(value))
    },
    logical(1)
  )
  if (!all(valid_tables)) {
    .viewerAuthDatabaseError(passphrase_env)
  }

  valid_users <- function(value, rows, require_rows) {
    is.character(value) &&
      is.null(attr(value, "class", exact = TRUE)) &&
      is.null(dim(value)) &&
      length(value) == rows &&
      (!require_rows || rows > 0L) &&
      !anyNA(value) &&
      all(nzchar(value)) &&
      !anyDuplicated(value)
  }
  credential_users <- credentials$user
  pwd_mngt_users <- pwd_mngt$user
  valid_credential_users <- valid_users(
    credential_users,
    nrow(credentials),
    TRUE
  )
  valid_pwd_mngt_users <- valid_users(
    pwd_mngt_users,
    nrow(pwd_mngt),
    FALSE
  )
  hashed <- credentials$is_hashed_password
  hash_type <- is.logical(hashed) ||
    typeof(hashed) %in% c("integer", "double")
  valid_hashes <- is.null(attr(hashed, "class", exact = TRUE)) &&
    is.null(dim(hashed)) &&
    hash_type &&
    length(hashed) == nrow(credentials) &&
    !anyNA(hashed) &&
    all(hashed == 1)
  same_users <- isTRUE(valid_credential_users) &&
    isTRUE(valid_pwd_mngt_users) &&
    identical(sort(credential_users), sort(pwd_mngt_users))
  if (
    !isTRUE(valid_hashes) ||
      !isTRUE(same_users)
  ) {
    .viewerAuthDatabaseError(passphrase_env)
  }
  invisible(TRUE)
}

.viewerAuthPathWithin <- function(path, root) {
  normalize <- function(value) {
    tryCatch(
      normalizePath(value, winslash = "/", mustWork = FALSE),
      error = function(condition) NA_character_
    )
  }
  path <- normalize(path)
  root <- normalize(root)
  if (is.na(path) || is.na(root) || !nzchar(path) || !nzchar(root)) {
    return(FALSE)
  }
  if (.Platform$OS.type == "windows") {
    path <- tolower(path)
    root <- tolower(root)
  }
  root <- sub("/+$", "", root)
  if (!nzchar(root)) {
    root <- "/"
  }
  identical(path, root) ||
    startsWith(
      path,
      if (identical(root, "/")) root else paste0(root, "/")
    )
}

.viewerAuthRejectResourcePath <- function(path, cerebro_root = NULL) {
  resource_error <- function() {
    stop(
      "auth$credentials must not be located in an HTTP resource directory.",
      call. = FALSE
    )
  }
  normalize_root <- function(root) {
    if (
      !is.character(root) ||
        length(root) != 1L ||
        is.na(root) ||
        !nzchar(root)
    ) {
      resource_error()
    }
    normalized <- tryCatch(
      normalizePath(root, winslash = "/", mustWork = TRUE),
      error = function(condition) NULL
    )
    if (
      !is.character(normalized) ||
        length(normalized) != 1L ||
        is.na(normalized) ||
        !nzchar(normalized)
    ) {
      resource_error()
    }
    normalized
  }
  path <- normalize_root(path)
  roots <- tryCatch(
    unname(shiny::resourcePaths()),
    error = function(condition) {
      resource_error()
    }
  )
  if (!is.character(roots) || anyNA(roots) || any(!nzchar(roots))) {
    resource_error()
  }
  if (!is.null(cerebro_root)) {
    cerebro_root <- normalize_root(cerebro_root)
    roots <- c(roots, file.path(cerebro_root, "viewer", "www"))
  }
  roots <- unname(vapply(roots, normalize_root, character(1)))
  if (any(vapply(roots, .viewerAuthPathWithin, logical(1), path = path))) {
    resource_error()
  }
  invisible(TRUE)
}

.compileViewerAuth <- function(
  auth,
  scope = c("host", "bundle"),
  cerebro_root = NULL
) {
  scope <- match.arg(scope)
  if (is.null(auth)) {
    return(list(config = NULL, source = NULL))
  }

  auth_names <- names(auth)
  if (
    !is.list(auth) ||
      is.data.frame(auth) ||
      is.null(auth_names) ||
      length(auth_names) != length(auth) ||
      anyNA(auth_names) ||
      any(!nzchar(auth_names)) ||
      anyDuplicated(auth_names)
  ) {
    stop(
      "auth must be a named list with unique non-empty names.",
      call. = FALSE
    )
  }
  required <- c("provider", "credentials", "passphrase_env")
  optional <- "timeout_minutes"
  missing <- setdiff(required, auth_names)
  if (length(missing) > 0L) {
    stop(paste0("auth$", missing[[1L]], " is required."), call. = FALSE)
  }
  unknown <- setdiff(auth_names, c(required, optional))
  if (length(unknown) > 0L) {
    stop(paste0("auth$", unknown[[1L]], " is not supported."), call. = FALSE)
  }

  provider <- .viewerAuthScalarCharacter(auth$provider, "provider")
  if (!identical(provider, "shinymanager")) {
    stop("auth$provider must be \"shinymanager\".", call. = FALSE)
  }
  credentials <- .viewerAuthScalarCharacter(
    auth$credentials,
    "credentials"
  )
  is_absolute <- grepl(
    "^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)",
    credentials
  )
  if (!is_absolute) {
    stop("auth$credentials must be an absolute path.", call. = FALSE)
  }
  passphrase_env <- .viewerAuthScalarCharacter(
    auth$passphrase_env,
    "passphrase_env"
  )
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", passphrase_env)) {
    stop(
      "auth$passphrase_env must be a valid environment variable name.",
      call. = FALSE
    )
  }

  timeout_minutes <- if ("timeout_minutes" %in% auth_names) {
    auth$timeout_minutes
  } else {
    15
  }
  if (
    !is.numeric(timeout_minutes) ||
      is.logical(timeout_minutes) ||
      length(timeout_minutes) != 1L ||
      is.na(timeout_minutes) ||
      !is.finite(timeout_minutes) ||
      timeout_minutes != floor(timeout_minutes) ||
      timeout_minutes < 1 ||
      timeout_minutes > 1440
  ) {
    stop(
      "auth$timeout_minutes must be one whole number from 1 to 1440.",
      call. = FALSE
    )
  }

  regular <- isTRUE(file.exists(credentials)) &&
    isTRUE(utils::file_test("-f", credentials)) &&
    isTRUE(file.access(credentials, mode = 4L) == 0L)
  if (!regular) {
    .viewerAuthDatabaseError(passphrase_env)
  }
  credentials <- tryCatch(
    normalizePath(credentials, winslash = "/", mustWork = TRUE),
    error = function(condition) NULL
  )
  if (is.null(credentials)) {
    .viewerAuthDatabaseError(passphrase_env)
  }
  if (identical(scope, "host")) {
    .viewerAuthRejectResourcePath(credentials, cerebro_root = cerebro_root)
  }
  .viewerAuthPreflightDatabase(credentials, passphrase_env)

  runtime_credentials <- if (identical(scope, "bundle")) {
    "private-data/auth/credentials.sqlite"
  } else {
    credentials
  }
  list(
    config = list(
      schema_version = 1L,
      provider = provider,
      credentials_scope = scope,
      credentials_path = runtime_credentials,
      passphrase_env = passphrase_env,
      timeout_minutes = as.integer(timeout_minutes)
    ),
    source = credentials
  )
}
