if (!exists("viewer_auth_fixture", mode = "function")) {
  source(testthat::test_path("helper-viewer-auth.R"), local = TRUE)
}

expect_auth_field_error <- function(descriptor, field) {
  expect_error(
    .compileViewerAuth(descriptor, "host"),
    paste0("auth$", field),
    fixed = TRUE
  )
}

capture_auth_conditions <- function(expr) {
  warnings <- character()
  messages <- character()
  value <- NULL
  stderr <- capture.output(
    stdout <- capture.output(
      value <- withCallingHandlers(
        tryCatch(
          eval(substitute(expr), envir = parent.frame()),
          error = identity
        ),
        warning = function(condition) {
          warnings <<- c(warnings, conditionMessage(condition))
          invokeRestart("muffleWarning")
        },
        message = function(condition) {
          messages <<- c(messages, conditionMessage(condition))
          invokeRestart("muffleMessage")
        }
      ),
      type = "output"
    ),
    type = "message"
  )
  list(
    value = value,
    warnings = warnings,
    messages = messages,
    stdout = stdout,
    stderr = stderr
  )
}

snapshot_shinymanager_token <- function(envir = parent.frame()) {
  token <- getFromNamespace(".tok", "shinymanager")
  sqlite_path <- token$get_sqlite_path()
  passphrase <- token$get_passphrase()
  withr::defer(
    {
      token$set_sqlite_path(sqlite_path)
      token$set_passphrase(passphrase)
    },
    envir = envir
  )
  list(sqlite_path = sqlite_path, passphrase = passphrase)
}

test_that("NULL authentication compiles to no manifest", {
  expect_identical(
    .compileViewerAuth(NULL, "host"),
    list(config = NULL, source = NULL)
  )
})

test_that("authentication descriptors have a strict named shape", {
  fixture <- viewer_auth_fixture()
  invalid <- list(
    list(value = TRUE, field = "auth"),
    list(value = "shinymanager", field = "auth"),
    list(value = as.data.frame(fixture$descriptor), field = "auth"),
    list(value = unname(fixture$descriptor), field = "auth"),
    list(
      value = structure(
        fixture$descriptor,
        names = c(
          "provider",
          "credentials",
          "passphrase_env",
          ""
        )
      ),
      field = "auth"
    ),
    list(
      value = structure(
        c(fixture$descriptor, list("shinymanager")),
        names = c(names(fixture$descriptor), "provider")
      ),
      field = "auth"
    ),
    list(
      value = c(fixture$descriptor, list(surprise = TRUE)),
      field = "auth$surprise"
    ),
    list(
      value = fixture$descriptor[names(fixture$descriptor) != "credentials"],
      field = "auth$credentials"
    )
  )

  for (case in invalid) {
    expect_error(
      .compileViewerAuth(case$value, "host"),
      case$field,
      fixed = TRUE,
      info = paste("invalid descriptor:", case$field)
    )
  }
})

test_that("authentication scalar fields reject malformed values", {
  fixture <- viewer_auth_fixture()
  cases <- list(
    provider = list(NULL, NA_character_, "custom", c("shinymanager", "custom")),
    credentials = list(
      NULL,
      NA_character_,
      "credentials.sqlite",
      fixture$root
    ),
    passphrase_env = list(
      NULL,
      "",
      "9BAD",
      "BAD-NAME",
      NA_character_
    ),
    timeout_minutes = list(0, 1441, 1.5, Inf, NA_real_, "15", TRUE)
  )

  for (field in names(cases)) {
    for (value in cases[[field]]) {
      descriptor <- fixture$descriptor
      descriptor[field] <- list(value)
      expect_auth_field_error(descriptor, field)
    }
  }
})

test_that("authentication fails closed when the secret is absent", {
  fixture <- viewer_auth_fixture()
  withr::local_envvar(stats::setNames(NA_character_, fixture$env_name))
  expect_error(
    .compileViewerAuth(fixture$descriptor, "host"),
    fixture$env_name,
    fixed = TRUE
  )
})

test_that("authentication rejects secrets shorter than 32 bytes without echoing them", {
  fixture <- viewer_auth_fixture()
  sentinel <- "short-secret-sentinel"
  withr::local_envvar(stats::setNames(sentinel, fixture$env_name))
  condition <- tryCatch(
    .compileViewerAuth(fixture$descriptor, "host"),
    error = identity
  )
  expect_s3_class(condition, "error")
  expect_match(conditionMessage(condition), fixture$env_name, fixed = TRUE)
  expect_false(grepl(sentinel, conditionMessage(condition), fixed = TRUE))
})

test_that("wrong passphrases are not disclosed on any condition or output channel", {
  fixture <- viewer_auth_fixture()
  sentinel <- paste0("wrong-passphrase-sentinel-", strrep("z", 32L))
  withr::local_envvar(stats::setNames(sentinel, fixture$env_name))
  captured <- capture_auth_conditions(
    .compileViewerAuth(fixture$descriptor, "host")
  )

  expect_s3_class(captured$value, "error")
  expect_match(
    conditionMessage(captured$value),
    fixture$env_name,
    fixed = TRUE
  )
  channels <- c(
    conditionMessage(captured$value),
    captured$warnings,
    captured$messages,
    captured$stdout,
    captured$stderr
  )
  expect_false(any(grepl(sentinel, channels, fixed = TRUE)))
})

test_that("missing malformed and non-regular databases share a stable error", {
  fixture <- viewer_auth_fixture()
  malformed <- file.path(fixture$root, "malformed.sqlite")
  writeLines("not a sqlite database", malformed)
  missing <- file.path(fixture$root, "missing.sqlite")
  expected <- paste0(
    "auth$credentials must be a readable shinymanager database using ",
    fixture$env_name,
    "."
  )

  for (path in c(missing, malformed)) {
    descriptor <- fixture$descriptor
    descriptor$credentials <- path
    expect_error(
      .compileViewerAuth(descriptor, "host"),
      expected,
      fixed = TRUE
    )
  }

  if (.Platform$OS.type != "windows") {
    unreadable <- file.path(fixture$root, "unreadable.sqlite")
    expect_true(file.copy(fixture$database, unreadable))
    Sys.chmod(unreadable, mode = "0000")
    withr::defer(Sys.chmod(unreadable, mode = "0600"))
    if (file.access(unreadable, mode = 4L) != 0L) {
      descriptor <- fixture$descriptor
      descriptor$credentials <- unreadable
      expect_error(
        .compileViewerAuth(descriptor, "host"),
        expected,
        fixed = TRUE
      )
    }
  }

  skip_on_os("windows")
  fifo <- file.path(fixture$root, "credentials.fifo")
  status <- system2("mkfifo", fifo)
  skip_if(status != 0L, "mkfifo is unavailable")
  descriptor <- fixture$descriptor
  descriptor$credentials <- fifo
  expect_error(
    .compileViewerAuth(descriptor, "host"),
    expected,
    fixed = TRUE
  )
})

test_that("host and bundle manifests contain no secret", {
  fixture <- viewer_auth_fixture()
  host <- .compileViewerAuth(fixture$descriptor, "host")
  bundle <- .compileViewerAuth(fixture$descriptor, "bundle")
  default_timeout <- fixture$descriptor
  default_timeout$timeout_minutes <- NULL

  expect_identical(
    host,
    list(
      config = list(
        schema_version = 1L,
        provider = "shinymanager",
        credentials_scope = "host",
        credentials_path = fixture$database,
        passphrase_env = fixture$env_name,
        timeout_minutes = 15L
      ),
      source = fixture$database
    )
  )
  expect_identical(
    bundle,
    list(
      config = list(
        schema_version = 1L,
        provider = "shinymanager",
        credentials_scope = "bundle",
        credentials_path = "private-data/auth/credentials.sqlite",
        passphrase_env = fixture$env_name,
        timeout_minutes = 15L
      ),
      source = fixture$database
    )
  )
  expect_identical(
    .compileViewerAuth(default_timeout, "host")$config$timeout_minutes,
    15L
  )
  for (boundary in c(1, 1440)) {
    descriptor <- fixture$descriptor
    descriptor$timeout_minutes <- boundary
    expect_identical(
      .compileViewerAuth(descriptor, "host")$config$timeout_minutes,
      as.integer(boundary)
    )
  }
  expect_false(grepl(
    fixture$passphrase,
    paste(capture.output(str(host)), collapse = "\n"),
    fixed = TRUE
  ))
  expect_false(grepl(
    fixture$passphrase,
    paste(capture.output(str(bundle)), collapse = "\n"),
    fixed = TRUE
  ))
  captured <- capture_auth_conditions(
    .compileViewerAuth(fixture$descriptor, "host")
  )
  expect_false(inherits(captured$value, "error"))
  expect_false(any(grepl(
    fixture$passphrase,
    c(captured$warnings, captured$messages, captured$stdout, captured$stderr),
    fixed = TRUE
  )))
})

test_that("all required shinymanager tables and columns are preflighted", {
  fixture <- viewer_auth_fixture()
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
  expected <- paste0(
    "auth$credentials must be a readable shinymanager database using ",
    fixture$env_name,
    "."
  )

  for (table in names(required)) {
    database <- file.path(
      fixture$root,
      paste0(table, "-missing-column.sqlite")
    )
    expect_true(file.copy(fixture$database, database))
    value <- shinymanager::read_db_decrypt(
      conn = database,
      name = table,
      passphrase = fixture$passphrase
    )
    value[[required[[table]][1L]]] <- NULL
    shinymanager::write_db_encrypt(
      conn = database,
      value = value,
      name = table,
      passphrase = fixture$passphrase
    )
    descriptor <- fixture$descriptor
    descriptor$credentials <- database
    expect_error(
      .compileViewerAuth(descriptor, "host"),
      expected,
      fixed = TRUE,
      info = paste("missing required column in", table)
    )
  }
})

test_that("shinymanager tables reject duplicate column names", {
  fixture <- viewer_auth_fixture()
  required <- list(
    credentials = "user",
    pwd_mngt = "user",
    logs = "token"
  )
  expected <- paste0(
    "auth$credentials must be a readable shinymanager database using ",
    fixture$env_name,
    "."
  )

  for (table in names(required)) {
    database <- file.path(
      fixture$root,
      paste0(table, "-duplicate-column.sqlite")
    )
    expect_true(file.copy(fixture$database, database))
    value <- shinymanager::read_db_decrypt(
      conn = database,
      name = table,
      passphrase = fixture$passphrase
    )
    duplicate <- required[[table]]
    value[[ncol(value) + 1L]] <- value[[duplicate]]
    names(value)[[ncol(value)]] <- duplicate
    shinymanager::write_db_encrypt(
      conn = database,
      value = value,
      name = table,
      passphrase = fixture$passphrase
    )
    persisted <- shinymanager::read_db_decrypt(
      conn = database,
      name = table,
      passphrase = fixture$passphrase
    )
    expect_gt(anyDuplicated(names(persisted)), 0L)
    descriptor <- fixture$descriptor
    descriptor$credentials <- database
    expect_error(
      .compileViewerAuth(descriptor, "host"),
      expected,
      fixed = TRUE,
      info = paste("duplicate required column in", table)
    )
  }
})

test_that("shinymanager user tables require one-to-one plain character users", {
  fixture <- viewer_auth_fixture()
  expected <- paste0(
    "auth$credentials must be a readable shinymanager database using ",
    fixture$env_name,
    "."
  )
  expect_invalid_database <- function(database, info) {
    descriptor <- fixture$descriptor
    descriptor$credentials <- database
    expect_error(
      .compileViewerAuth(descriptor, "host"),
      expected,
      fixed = TRUE,
      info = info
    )
  }

  empty <- file.path(fixture$root, "empty-users.sqlite")
  expect_true(file.copy(fixture$database, empty))
  for (table in c("credentials", "pwd_mngt")) {
    value <- shinymanager::read_db_decrypt(
      conn = empty,
      name = table,
      passphrase = fixture$passphrase
    )
    shinymanager::write_db_encrypt(
      conn = empty,
      value = value[0, , drop = FALSE],
      name = table,
      passphrase = fixture$passphrase
    )
  }
  expect_invalid_database(empty, "empty credential and password tables")

  duplicate <- file.path(fixture$root, "duplicate-password-users.sqlite")
  expect_true(file.copy(fixture$database, duplicate))
  pwd_mngt <- shinymanager::read_db_decrypt(
    conn = duplicate,
    name = "pwd_mngt",
    passphrase = fixture$passphrase
  )
  pwd_mngt <- rbind(pwd_mngt, pwd_mngt[1L, , drop = FALSE])
  shinymanager::write_db_encrypt(
    conn = duplicate,
    value = pwd_mngt,
    name = "pwd_mngt",
    passphrase = fixture$passphrase
  )
  expect_invalid_database(duplicate, "duplicate password-management user")

  malformed <- list(
    credentials_list = list(table = "credentials", mutate = function(value) {
      value$user <- I(as.list(value$user))
      value
    }),
    pwd_mngt_data_frame = list(table = "pwd_mngt", mutate = function(value) {
      value$user <- I(data.frame(value = value$user))
      value
    })
  )
  for (name in names(malformed)) {
    case <- malformed[[name]]
    database <- file.path(fixture$root, paste0(name, ".sqlite"))
    expect_true(file.copy(fixture$database, database))
    value <- shinymanager::read_db_decrypt(
      conn = database,
      name = case$table,
      passphrase = fixture$passphrase
    )
    shinymanager::write_db_encrypt(
      conn = database,
      value = case$mutate(value),
      name = case$table,
      passphrase = fixture$passphrase
    )
    expect_invalid_database(database, paste("malformed user column:", name))
  }
})

test_that("shinymanager credential semantics are preflighted", {
  fixture <- viewer_auth_fixture()
  mutations <- list(
    user_na = list(table = "credentials", mutate = function(value) {
      value$user[[1L]] <- NA_character_
      value
    }),
    user_empty = list(table = "credentials", mutate = function(value) {
      value$user[[1L]] <- ""
      value
    }),
    user_duplicate = list(table = "credentials", mutate = function(value) {
      value$user[[2L]] <- value$user[[1L]]
      value
    }),
    hash_na = list(table = "credentials", mutate = function(value) {
      value$is_hashed_password[[1L]] <- NA
      value
    }),
    hash_two = list(table = "credentials", mutate = function(value) {
      value$is_hashed_password <- c(2, 2)
      value
    }),
    hash_string = list(table = "credentials", mutate = function(value) {
      value$is_hashed_password <- c("true", "true")
      value
    }),
    users_differ = list(table = "pwd_mngt", mutate = function(value) {
      value$user[[1L]] <- "someone-else"
      value
    })
  )
  expected <- paste0(
    "auth$credentials must be a readable shinymanager database using ",
    fixture$env_name,
    "."
  )

  for (name in names(mutations)) {
    mutation <- mutations[[name]]
    database <- file.path(fixture$root, paste0(name, ".sqlite"))
    expect_true(file.copy(fixture$database, database))
    value <- shinymanager::read_db_decrypt(
      conn = database,
      name = mutation$table,
      passphrase = fixture$passphrase
    )
    shinymanager::write_db_encrypt(
      conn = database,
      value = mutation$mutate(value),
      name = mutation$table,
      passphrase = fixture$passphrase
    )
    descriptor <- fixture$descriptor
    descriptor$credentials <- database
    expect_error(
      .compileViewerAuth(descriptor, "host"),
      expected,
      fixed = TRUE,
      info = paste("invalid credential semantics:", name)
    )
  }

  integer_hashes <- file.path(fixture$root, "integer-hashes.sqlite")
  expect_true(file.copy(fixture$database, integer_hashes))
  credentials <- shinymanager::read_db_decrypt(
    conn = integer_hashes,
    name = "credentials",
    passphrase = fixture$passphrase
  )
  credentials$is_hashed_password <- c(1L, 1L)
  shinymanager::write_db_encrypt(
    conn = integer_hashes,
    value = credentials,
    name = "credentials",
    passphrase = fixture$passphrase
  )
  descriptor <- fixture$descriptor
  descriptor$credentials <- integer_hashes
  expect_identical(
    .compileViewerAuth(descriptor, "host")$source,
    normalizePath(integer_hashes, winslash = "/")
  )
})

test_that("compiler rejects matrix user and hash columns", {
  fixture <- viewer_auth_fixture()
  mutations <- list(
    credentials_user_matrix = list(
      table = "credentials",
      column = "user",
      mutate = function(value) matrix(value$user, ncol = 1L)
    ),
    logical_hash_matrix = list(
      table = "credentials",
      column = "is_hashed_password",
      mutate = function(value) {
        matrix(
          value$is_hashed_password,
          ncol = 1L
        )
      }
    )
  )
  expected <- paste0(
    "auth$credentials must be a readable shinymanager database using ",
    fixture$env_name,
    "."
  )

  for (name in names(mutations)) {
    mutation <- mutations[[name]]
    database <- file.path(fixture$root, paste0(name, ".sqlite"))
    expect_true(file.copy(fixture$database, database))
    value <- shinymanager::read_db_decrypt(
      conn = database,
      name = mutation$table,
      passphrase = fixture$passphrase
    )
    value[[mutation$column]] <- mutation$mutate(value)
    shinymanager::write_db_encrypt(
      conn = database,
      value = value,
      name = mutation$table,
      passphrase = fixture$passphrase
    )
    persisted <- shinymanager::read_db_decrypt(
      conn = database,
      name = mutation$table,
      passphrase = fixture$passphrase
    )
    expect_false(is.null(dim(persisted[[mutation$column]])), info = name)
    descriptor <- fixture$descriptor
    descriptor$credentials <- database
    expect_error(
      .compileViewerAuth(descriptor, "host"),
      expected,
      fixed = TRUE,
      info = paste("matrix credential column:", name)
    )
  }
})

test_that("database preflight never mutates shinymanager token state", {
  fixture <- viewer_auth_fixture()
  token <- getFromNamespace(".tok", "shinymanager")
  before <- snapshot_shinymanager_token()

  .compileViewerAuth(fixture$descriptor, "host")
  expect_identical(token$get_sqlite_path(), before$sqlite_path)
  expect_identical(token$get_passphrase(), before$passphrase)

  sentinel <- paste0("wrong-passphrase-sentinel-", strrep("q", 32L))
  withr::local_envvar(stats::setNames(sentinel, fixture$env_name))
  expect_error(.compileViewerAuth(fixture$descriptor, "host"))
  expect_identical(token$get_sqlite_path(), before$sqlite_path)
  expect_identical(token$get_passphrase(), before$passphrase)
})

test_that("character descriptor fields compile to canonical base strings", {
  fixture <- viewer_auth_fixture()
  descriptor <- fixture$descriptor
  descriptor$provider <- structure("shinymanager", names = "provider")
  descriptor$credentials <- structure(
    fixture$database,
    class = "viewer_auth_path"
  )
  descriptor$passphrase_env <- structure(
    fixture$env_name,
    note = "must not reach the manifest"
  )

  compiled <- .compileViewerAuth(descriptor, "host")
  values <- list(
    compiled$config$provider,
    compiled$config$credentials_path,
    compiled$config$passphrase_env,
    compiled$source
  )
  expect_true(all(vapply(values, is.character, logical(1))))
  expect_true(all(vapply(
    values,
    function(value) is.null(attributes(value)),
    logical(1)
  )))
  expect_identical(compiled$config$provider, "shinymanager")
  expect_identical(compiled$config$credentials_path, fixture$database)
  expect_identical(compiled$config$passphrase_env, fixture$env_name)
  expect_identical(compiled$source, fixture$database)
})

test_that("provider dependency failures propagate before returning a manifest", {
  fixture <- viewer_auth_fixture()
  testthat::local_mocked_bindings(
    .viewerAuthProviderAvailable = function() {
      stop("Authentication requires mocked provider.", call. = FALSE)
    },
    .package = "CerebroNexus"
  )
  expect_error(
    .compileViewerAuth(fixture$descriptor, "host"),
    "Authentication requires mocked provider.",
    fixed = TRUE
  )
})

test_that("host credentials cannot be served as an HTTP resource", {
  fixture <- viewer_auth_fixture()
  prefix <- paste0("viewer-auth-", as.integer(stats::runif(1L, 1, 1e8)))
  shiny::addResourcePath(prefix, fixture$root)
  withr::defer(shiny::removeResourcePath(prefix))

  expect_error(
    .compileViewerAuth(fixture$descriptor, "host"),
    "auth$credentials must not be located in an HTTP resource directory.",
    fixed = TRUE
  )
})

test_that("host resource discovery fails closed", {
  fixture <- viewer_auth_fixture()
  testthat::local_mocked_bindings(
    resourcePaths = function() stop("resource registry unavailable"),
    .package = "shiny"
  )
  expect_error(
    .compileViewerAuth(fixture$descriptor, "host"),
    "auth$credentials must not be located in an HTTP resource directory.",
    fixed = TRUE
  )
})

test_that("host resource roots must be valid and normalizable", {
  fixture <- viewer_auth_fixture()
  expected <- "auth$credentials must not be located in an HTTP resource directory."

  for (cerebro_root in list(
    NA_character_,
    "",
    c(fixture$root, fixture$root),
    file.path(fixture$root, "missing-cerebro-root")
  )) {
    expect_error(
      .compileViewerAuth(
        fixture$descriptor,
        "host",
        cerebro_root = cerebro_root
      ),
      expected,
      fixed = TRUE,
      info = paste("invalid cerebro_root:", paste(cerebro_root, collapse = ","))
    )
  }

  missing_resource_root <- file.path(fixture$root, "missing-resource-root")
  testthat::local_mocked_bindings(
    resourcePaths = function() c(missing = missing_resource_root),
    .package = "shiny"
  )
  expect_error(
    .compileViewerAuth(fixture$descriptor, "host"),
    expected,
    fixed = TRUE
  )
})

test_that("host credentials reject viewer www with directory-safe boundaries", {
  fixture <- viewer_auth_fixture()
  cerebro_root <- file.path(fixture$root, "cerebro")
  viewer_www <- file.path(cerebro_root, "viewer", "www")
  viewer_www2 <- file.path(cerebro_root, "viewer", "www2")
  dir.create(viewer_www, recursive = TRUE)
  dir.create(viewer_www2)
  database_in_www <- file.path(viewer_www, "credentials.sqlite")
  database_in_www2 <- file.path(viewer_www2, "credentials.sqlite")
  expect_true(file.copy(fixture$database, database_in_www))
  expect_true(file.copy(fixture$database, database_in_www2))

  descriptor <- fixture$descriptor
  descriptor$credentials <- database_in_www
  expect_error(
    .compileViewerAuth(descriptor, "host", cerebro_root = cerebro_root),
    "auth$credentials must not be located in an HTTP resource directory.",
    fixed = TRUE
  )

  descriptor$credentials <- database_in_www2
  compiled <- .compileViewerAuth(
    descriptor,
    "host",
    cerebro_root = cerebro_root
  )
  expect_identical(
    compiled$source,
    normalizePath(database_in_www2, winslash = "/")
  )
})
