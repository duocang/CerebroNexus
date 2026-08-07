.viewer_auth_manifest_error <- function() {
  stop("Invalid viewer authentication manifest.", call. = FALSE)
}

.viewer_auth_plain_scalar <- function(value, type) {
  identical(typeof(value), type) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.null(attributes(value))
}

.viewer_auth_is_absolute <- function(path) {
  if (.Platform$OS.type == "windows") {
    return(grepl("^([A-Za-z]:[/\\\\]|\\\\\\\\|//)", path))
  }
  startsWith(path, "/") && !startsWith(path, "//")
}

.viewer_auth_path_within <- function(path, root) {
  if (.Platform$OS.type == "windows") {
    path <- tolower(path)
    root <- tolower(root)
  }
  root <- sub("/+$", "", root)
  if (!nzchar(root)) {
    root <- "/"
  }
  identical(path, root) ||
    startsWith(path, if (identical(root, "/")) root else paste0(root, "/"))
}

.viewer_auth_normalize_root <- function(path, error) {
  valid <- .viewer_auth_plain_scalar(path, "character") && nzchar(path)
  if (!valid) {
    stop(error, call. = FALSE)
  }
  result <- tryCatch(
    normalizePath(path, winslash = "/", mustWork = TRUE),
    error = function(condition) NULL
  )
  if (is.null(result) || !isTRUE(file.info(result)$isdir)) {
    stop(error, call. = FALSE)
  }
  result
}

.viewer_auth_regular_file <- function(path) {
  warned <- FALSE
  connection <- tryCatch(
    withCallingHandlers(
      file(path, open = "r+b", blocking = FALSE),
      warning = function(condition) {
        warned <<- TRUE
        invokeRestart("muffleWarning")
      }
    ),
    error = function(condition) NULL
  )
  if (inherits(connection, "connection")) {
    close(connection)
  }
  !warned && inherits(connection, "connection")
}

viewer_auth_validate_manifest <- function(config) {
  expected <- c(
    "schema_version",
    "provider",
    "credentials_scope",
    "credentials_path",
    "passphrase_env",
    "timeout_minutes"
  )
  if (
    !identical(typeof(config), "list") ||
      !identical(attributes(config), list(names = expected)) ||
      !.viewer_auth_plain_scalar(config$schema_version, "integer") ||
      !identical(config$schema_version, 1L) ||
      !.viewer_auth_plain_scalar(config$provider, "character") ||
      !identical(config$provider, "shinymanager") ||
      !.viewer_auth_plain_scalar(config$credentials_scope, "character") ||
      !config$credentials_scope %in% c("host", "bundle") ||
      !.viewer_auth_plain_scalar(config$credentials_path, "character") ||
      !nzchar(config$credentials_path) ||
      !.viewer_auth_plain_scalar(config$passphrase_env, "character") ||
      !grepl("^[A-Za-z_][A-Za-z0-9_]*$", config$passphrase_env) ||
      !.viewer_auth_plain_scalar(config$timeout_minutes, "integer") ||
      config$timeout_minutes < 1L ||
      config$timeout_minutes > 1440L
  ) {
    .viewer_auth_manifest_error()
  }
  if (identical(config$credentials_scope, "bundle")) {
    if (
      !identical(
        config$credentials_path,
        "private-data/auth/credentials.sqlite"
      )
    ) {
      .viewer_auth_manifest_error()
    }
  } else if (!.viewer_auth_is_absolute(config$credentials_path)) {
    .viewer_auth_manifest_error()
  }
  config
}

viewer_auth_resolve_credentials <- function(config, cerebro_root) {
  config <- viewer_auth_validate_manifest(config)
  path_error <- "Invalid viewer authentication credentials path."
  access_error <- "Authentication credentials database is not accessible."
  resource_error <- paste(
    "Authentication credentials must not be in an HTTP resource directory."
  )
  root <- .viewer_auth_normalize_root(cerebro_root, path_error)

  if (identical(config$credentials_scope, "bundle")) {
    candidate <- file.path(root, config$credentials_path)
    path <- tryCatch(
      normalizePath(candidate, winslash = "/", mustWork = TRUE),
      error = function(condition) NULL
    )
    if (is.null(path) || !.viewer_auth_path_within(path, root)) {
      stop(path_error, call. = FALSE)
    }
  } else {
    path <- tryCatch(
      normalizePath(config$credentials_path, winslash = "/", mustWork = TRUE),
      error = function(condition) NULL
    )
    if (is.null(path)) {
      stop(access_error, call. = FALSE)
    }

    roots <- tryCatch(
      unname(shiny::resourcePaths()),
      error = function(condition) NULL
    )
    if (
      is.null(roots) ||
        !is.character(roots) ||
        anyNA(roots) ||
        any(!nzchar(roots))
    ) {
      stop(resource_error, call. = FALSE)
    }
    roots <- c(roots, file.path(root, "viewer", "www"))
    normalized_roots <- vapply(
      roots,
      function(resource_root) {
        tryCatch(
          normalizePath(resource_root, winslash = "/", mustWork = TRUE),
          error = function(condition) NA_character_
        )
      },
      character(1)
    )
    if (anyNA(normalized_roots) || any(!nzchar(normalized_roots))) {
      stop(resource_error, call. = FALSE)
    }
    if (
      any(vapply(
        normalized_roots,
        function(resource_root) .viewer_auth_path_within(path, resource_root),
        logical(1)
      ))
    ) {
      stop(resource_error, call. = FALSE)
    }
  }

  info <- file.info(path)
  parent <- dirname(path)
  accessible <- isTRUE(file.exists(path)) &&
    isTRUE(utils::file_test("-f", path)) &&
    isTRUE(!info$isdir) &&
    isTRUE(file.access(path, mode = 6L) == 0L) &&
    isTRUE(file.access(parent, mode = 3L) == 0L) &&
    .viewer_auth_regular_file(path)
  if (!accessible) {
    stop(access_error, call. = FALSE)
  }
  path
}

viewer_auth_read_secret <- function(passphrase_env) {
  if (
    !.viewer_auth_plain_scalar(passphrase_env, "character") ||
      !grepl("^[A-Za-z_][A-Za-z0-9_]*$", passphrase_env)
  ) {
    stop("Invalid authentication passphrase environment name.", call. = FALSE)
  }
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

.viewer_auth_database_error <- function(passphrase_env) {
  stop(
    paste0(
      "Authentication credentials database is invalid; check ",
      passphrase_env,
      "."
    ),
    call. = FALSE
  )
}

viewer_auth_validate_database <- function(
  credentials,
  passphrase,
  passphrase_env
) {
  credentials_table <- NULL
  pwd_mngt <- NULL
  logs <- NULL
  on.exit(
    {
      credentials_table <- NULL
      pwd_mngt <- NULL
      logs <- NULL
    },
    add = TRUE
  )

  read_table <- function(name) {
    suppressWarnings(suppressMessages(tryCatch(
      shinymanager::read_db_decrypt(
        conn = credentials,
        name = name,
        passphrase = passphrase
      ),
      error = function(condition) NULL
    )))
  }
  credentials_table <- read_table("credentials")
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
    credentials = credentials_table,
    pwd_mngt = pwd_mngt,
    logs = logs
  )
  valid_tables <- vapply(
    names(required),
    function(name) {
      table <- tables[[name]]
      is.data.frame(table) &&
        !anyDuplicated(names(table)) &&
        all(required[[name]] %in% names(table))
    },
    logical(1)
  )
  if (!all(valid_tables)) {
    .viewer_auth_database_error(passphrase_env)
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
  credential_users <- credentials_table$user
  pwd_users <- pwd_mngt$user
  credentials_ok <- valid_users(
    credential_users,
    nrow(credentials_table),
    TRUE
  )
  pwd_ok <- valid_users(pwd_users, nrow(pwd_mngt), FALSE)

  hashed <- credentials_table$is_hashed_password
  hash_type <- is.logical(hashed) ||
    typeof(hashed) %in% c("integer", "double")
  hashes_ok <- is.null(attr(hashed, "class", exact = TRUE)) &&
    is.null(dim(hashed)) &&
    hash_type &&
    length(hashed) == nrow(credentials_table) &&
    !anyNA(hashed) &&
    all(hashed == 1)
  users_match <- isTRUE(credentials_ok) &&
    isTRUE(pwd_ok) &&
    identical(sort(credential_users), sort(pwd_users))
  if (
    !isTRUE(hashes_ok) ||
      !isTRUE(users_match)
  ) {
    .viewer_auth_database_error(passphrase_env)
  }
  invisible(TRUE)
}

viewer_auth_brand <- function(cerebro_root) {
  root <- .viewer_auth_normalize_root(
    cerebro_root,
    "Authentication branding assets are unavailable."
  )
  www <- file.path(root, "viewer", "www")
  css <- file.path(www, "auth.css")
  logo <- file.path(www, "cerebronexus.svg")
  if (
    !isTRUE(utils::file_test("-f", css)) ||
      !isTRUE(utils::file_test("-f", logo))
  ) {
    stop("Authentication branding assets are unavailable.", call. = FALSE)
  }
  svg <- paste(readLines(logo, warn = FALSE), collapse = "\n")
  list(
    head = shiny::includeCSS(css),
    top = shiny::tags$div(
      class = "cerebro-auth-brand",
      shiny::HTML(svg)
    ),
    bottom = shiny::tags$div(
      class = "cerebro-auth-brand cerebro-auth-brand-bottom",
      "CerebroNexus"
    )
  )
}

.viewer_auth_provider_failure <- function(passphrase_env) {
  stop(
    paste0("Authentication provider failed; check ", passphrase_env, "."),
    call. = FALSE
  )
}

.viewer_auth_provider_call <- function(
  expr,
  passphrase_env,
  fail_on_condition = FALSE
) {
  failed <- function() .viewer_auth_provider_failure(passphrase_env)
  tryCatch(
    withCallingHandlers(
      force(expr),
      warning = function(condition) {
        if (fail_on_condition) {
          failed()
        }
        invokeRestart("muffleWarning")
      },
      message = function(condition) {
        if (fail_on_condition) {
          failed()
        }
        invokeRestart("muffleMessage")
      }
    ),
    error = function(condition) failed()
  )
}

.viewer_auth_provider_version <- function() {
  if (!base::requireNamespace("shinymanager", quietly = TRUE)) {
    return(NULL)
  }
  tryCatch(
    utils::packageVersion("shinymanager"),
    error = function(condition) NULL
  )
}

viewer_auth_shinymanager_provider <- function(config, credentials, brand) {
  version <- .viewer_auth_provider_version()
  if (is.null(version) || version < "1.1.0") {
    stop("Authentication requires shinymanager (>= 1.1.0).", call. = FALSE)
  }

  make_checker <- function(passphrase) {
    checker <- .viewer_auth_provider_call(
      shinymanager::check_credentials(
        db = credentials,
        passphrase = passphrase
      ),
      config$passphrase_env,
      fail_on_condition = TRUE
    )
    if (!is.function(checker)) {
      .viewer_auth_provider_failure(config$passphrase_env)
    }
    checker
  }

  list(
    preflight = function(passphrase) {
      viewer_auth_validate_database(
        credentials,
        passphrase,
        config$passphrase_env
      )
    },
    secure_ui = function(viewer_ui) {
      shinymanager::secure_app(
        viewer_ui,
        enable_admin = FALSE,
        head_auth = brand$head,
        tags_top = brand$top,
        tags_bottom = brand$bottom,
        status = "primary",
        language = "en"
      )
    },
    secure_server = function(session) {
      passphrase <- viewer_auth_read_secret(config$passphrase_env)
      on.exit(
        {
          if (exists("passphrase", inherits = FALSE)) {
            rm(passphrase)
          }
        },
        add = TRUE
      )
      checker <- make_checker(passphrase)
      rm(passphrase)
      state <- .viewer_auth_provider_call(
        shinymanager::secure_server(
          check_credentials = checker,
          timeout = config$timeout_minutes,
          keep_token = FALSE,
          session = session
        ),
        config$passphrase_env
      )
      invisible(state)
    }
  )
}

viewer_auth_gate_server <- function(viewer_server, auth_server) {
  force(viewer_server)
  force(auth_server)
  function(input, output, session) {
    auth_state <- auth_server(session)
    started <- shiny::reactiveVal(FALSE)
    shiny::observe({
      user <- auth_state$user
      shiny::req(
        is.character(user),
        length(user) == 1L,
        !is.na(user),
        nzchar(user)
      )
      if (!started()) {
        started(TRUE)
        viewer_server(input, output, session)
      }
    })
    invisible(auth_state)
  }
}

viewer_auth_apply <- function(
  ui,
  server,
  config,
  cerebro_root,
  provider_factory = viewer_auth_shinymanager_provider
) {
  if (is.null(config)) {
    return(list(ui = ui, server = server))
  }
  credentials <- viewer_auth_resolve_credentials(config, cerebro_root)
  brand <- viewer_auth_brand(cerebro_root)
  provider <- provider_factory(config, credentials, brand)
  if (
    !is.list(provider) ||
      !identical(
        names(provider),
        c("preflight", "secure_ui", "secure_server")
      ) ||
      !all(vapply(provider, is.function, logical(1)))
  ) {
    stop(
      "Authentication provider factory returned invalid adapters.",
      call. = FALSE
    )
  }
  passphrase <- viewer_auth_read_secret(config$passphrase_env)
  on.exit(
    {
      if (exists("passphrase", inherits = FALSE)) {
        rm(passphrase)
      }
    },
    add = TRUE
  )
  provider$preflight(passphrase)
  rm(passphrase)
  list(
    ui = provider$secure_ui(ui),
    server = viewer_auth_gate_server(server, provider$secure_server)
  )
}
