.viewer_auth_error <- function(message) {
  stop(message, call. = FALSE)
}

.viewer_auth_validate_config <- function(config) {
  expected <- c(
    "credentials_path",
    "passphrase_env",
    "timeout_minutes"
  )
  valid <- is.list(config) &&
    identical(names(config), expected) &&
    identical(
      config$credentials_path,
      "private-data/auth/credentials.sqlite"
    ) &&
    is.character(config$passphrase_env) &&
    length(config$passphrase_env) == 1L &&
    !is.na(config$passphrase_env) &&
    grepl("^[A-Za-z_][A-Za-z0-9_]*$", config$passphrase_env) &&
    is.integer(config$timeout_minutes) &&
    length(config$timeout_minutes) == 1L &&
    !is.na(config$timeout_minutes) &&
    config$timeout_minutes >= 1L &&
    config$timeout_minutes <= 1440L
  if (!valid) {
    .viewer_auth_error("Invalid Viewer authentication configuration.")
  }
  config
}

.viewer_auth_require_provider <- function() {
  available <- requireNamespace("shinymanager", quietly = TRUE)
  version <- if (available) {
    tryCatch(
      utils::packageVersion("shinymanager"),
      error = function(condition) NULL
    )
  } else {
    NULL
  }
  if (is.null(version) || version < "1.1.0") {
    .viewer_auth_error(
      "Authentication requires shinymanager (>= 1.1.0)."
    )
  }
}

viewer_auth_apply <- function(ui, server, config, cerebro_root = ".") {
  if (is.null(config)) {
    return(list(ui = ui, server = server))
  }

  config <- .viewer_auth_validate_config(config)
  root <- tryCatch(
    normalizePath(cerebro_root, winslash = "/", mustWork = TRUE),
    error = function(condition) NULL
  )
  database <- if (is.null(root)) {
    NULL
  } else {
    tryCatch(
      normalizePath(
        file.path(root, config$credentials_path),
        winslash = "/",
        mustWork = TRUE
      ),
      error = function(condition) NULL
    )
  }
  accessible <- !is.null(database) &&
    isTRUE(utils::file_test("-f", database)) &&
    isTRUE(file.access(database, mode = 6L) == 0L) &&
    isTRUE(file.access(dirname(database), mode = 3L) == 0L)
  if (!accessible) {
    .viewer_auth_error(
      "Authentication credentials database is not accessible."
    )
  }

  passphrase <- Sys.getenv(config$passphrase_env, unset = NA_character_)
  on.exit(passphrase <- NULL, add = TRUE)
  if (
    length(passphrase) != 1L ||
      is.na(passphrase) ||
      !nzchar(passphrase)
  ) {
    .viewer_auth_error(paste0(config$passphrase_env, " is not set."))
  }

  .viewer_auth_require_provider()
  checker <- suppressWarnings(suppressMessages(tryCatch(
    shinymanager::check_credentials(
      db = database,
      passphrase = passphrase
    ),
    error = function(condition) NULL
  )))
  passphrase <- NULL
  if (!is.function(checker)) {
    .viewer_auth_error(
      "Authentication database or passphrase is invalid."
    )
  }

  secured_server <- function(input, output, session) {
    auth <- shinymanager::secure_server(
      check_credentials = checker,
      timeout = config$timeout_minutes,
      keep_token = FALSE,
      session = session
    )
    started <- shiny::reactiveVal(FALSE)
    shiny::observe({
      user <- auth$user
      shiny::req(
        is.character(user),
        length(user) == 1L,
        !is.na(user),
        nzchar(user)
      )
      if (!started()) {
        started(TRUE)
        server(input, output, session)
      }
    })
    invisible(auth)
  }

  list(
    ui = shinymanager::secure_app(ui, enable_admin = FALSE),
    server = secured_server
  )
}
