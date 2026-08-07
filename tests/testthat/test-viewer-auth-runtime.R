if (!exists("viewer_auth_fixture", mode = "function")) {
  source(testthat::test_path("helper-viewer-auth.R"), local = TRUE)
}

viewer_auth_runtime_path <- function() {
  file.path(viewer_auth_package_root(), "viewer", "auth.R")
}

viewer_auth_package_root <- function() {
  source_root <- testthat::test_path("..", "..", "inst")
  if (dir.exists(source_root)) {
    return(normalizePath(source_root, winslash = "/", mustWork = TRUE))
  }

  package_root <- system.file(package = "CerebroNexus")
  if (!nzchar(package_root) || !dir.exists(package_root)) {
    stop(
      "Cannot locate the installed CerebroNexus package root.",
      call. = FALSE
    )
  }
  normalizePath(package_root, winslash = "/", mustWork = TRUE)
}

viewer_auth_compiler_path <- function() {
  source_path <- testthat::test_path("..", "..", "R", "viewer-auth.R")
  if (!file.exists(source_path)) {
    return(NULL)
  }
  normalizePath(source_path, winslash = "/", mustWork = TRUE)
}

source_viewer_auth <- function(parent = baseenv()) {
  environment <- new.env(parent = parent)
  sys.source(viewer_auth_runtime_path(), envir = environment)
  environment
}

viewer_auth_manifest <- function(fixture, scope = "host", path = NULL) {
  if (is.null(path)) {
    path <- if (identical(scope, "bundle")) {
      "private-data/auth/credentials.sqlite"
    } else {
      fixture$database
    }
  }
  list(
    schema_version = 1L,
    provider = "shinymanager",
    credentials_scope = scope,
    credentials_path = path,
    passphrase_env = fixture$env_name,
    timeout_minutes = 15L
  )
}

capture_viewer_auth_conditions <- function(expr) {
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

viewer_auth_package_environment_contains <- function(
  value,
  sentinel,
  runtime_environment,
  seen = new.env(parent = emptyenv())
) {
  package_environment <- function(environment) {
    cursor <- environment
    while (!identical(cursor, emptyenv())) {
      if (identical(cursor, runtime_environment)) {
        return(TRUE)
      }
      cursor <- parent.env(cursor)
    }
    FALSE
  }
  visit <- function(object, depth = 0L) {
    if (is.character(object) && any(grepl(sentinel, object, fixed = TRUE))) {
      return(TRUE)
    }
    if (is.function(object)) {
      environment <- environment(object)
      if (!is.null(environment) && package_environment(environment)) {
        key <- format(environment)
        if (!exists(key, envir = seen, inherits = FALSE)) {
          assign(key, TRUE, envir = seen)
          return(any(vapply(
            as.list(environment, all.names = TRUE),
            function(binding) visit(binding, depth + 1L),
            logical(1)
          )))
        }
      }
    }
    if (is.list(object) && depth < 6L) {
      return(any(vapply(
        object,
        function(element) visit(element, depth + 1L),
        logical(1)
      )))
    }
    FALSE
  }
  visit(value)
}

test_that("runtime is standalone plain R and NULL authentication is identity", {
  expect_true(file.exists(viewer_auth_runtime_path()))
  expect_silent(parse(viewer_auth_runtime_path()))
  text <- paste(
    readLines(viewer_auth_runtime_path(), warn = FALSE),
    collapse = "\n"
  )
  expect_false(grepl(
    "CerebroNexus::|cerebroAppLite::|getFromNamespace|system\\.file\\(",
    text
  ))

  runtime <- source_viewer_auth(baseenv())
  ui <- structure(list("ui"), class = "sentinel-ui")
  server <- function(input, output, session) "server"
  expect_identical(
    runtime$viewer_auth_apply(ui, server, NULL, tempdir()),
    list(ui = ui, server = server)
  )
})

test_that("frozen runtime manifest has exact ordered names and types", {
  runtime <- source_viewer_auth(baseenv())
  fixture <- viewer_auth_fixture()
  valid <- viewer_auth_manifest(fixture)
  expect_identical(runtime$viewer_auth_validate_manifest(valid), valid)

  invalid <- list(
    unname(valid),
    as.pairlist(valid),
    valid[c(2:6, 1)],
    c(valid, list(extra = TRUE)),
    structure(valid, class = "manifest"),
    within(valid, schema_version <- 2L),
    within(valid, schema_version <- 1),
    within(valid, provider <- "custom"),
    within(valid, credentials_scope <- "other"),
    within(
      valid,
      credentials_path <- structure(credentials_path, class = "path")
    ),
    within(valid, passphrase_env <- "9-BAD"),
    within(valid, timeout_minutes <- 0L),
    within(valid, timeout_minutes <- 15)
  )
  for (value in invalid) {
    expect_error(
      runtime$viewer_auth_validate_manifest(value),
      "Invalid viewer authentication manifest.",
      fixed = TRUE
    )
  }

  mismatch <- viewer_auth_manifest(fixture, "bundle", fixture$database)
  expect_error(
    runtime$viewer_auth_validate_manifest(mismatch),
    "Invalid viewer authentication manifest.",
    fixed = TRUE
  )
  mismatch <- viewer_auth_manifest(
    fixture,
    "host",
    "private-data/auth/credentials.sqlite"
  )
  expect_error(
    runtime$viewer_auth_validate_manifest(mismatch),
    "Invalid viewer authentication manifest.",
    fixed = TRUE
  )
})

test_that("runtime public surface has frozen argument order", {
  runtime <- source_viewer_auth(baseenv())
  expected <- list(
    viewer_auth_apply = c(
      "ui",
      "server",
      "config",
      "cerebro_root",
      "provider_factory"
    )
  )
  for (name in names(expected)) {
    expect_identical(
      names(formals(runtime[[name]])),
      expected[[name]],
      info = name
    )
  }
})

test_that("host absolute path syntax follows the runtime platform", {
  runtime <- source_viewer_auth(baseenv())
  fixture <- viewer_auth_fixture()
  windows_paths <- c(
    "C:/credentials.sqlite",
    "C:\\credentials.sqlite",
    "\\\\server\\share\\credentials.sqlite",
    "//server/share/credentials.sqlite"
  )
  invalid <- if (.Platform$OS.type == "windows") {
    c("/posix-only.sqlite", "relative.sqlite")
  } else {
    windows_paths
  }
  for (path in invalid) {
    config <- viewer_auth_manifest(fixture, path = path)
    expect_error(
      runtime$viewer_auth_validate_manifest(config),
      "Invalid viewer authentication manifest.",
      fixed = TRUE
    )
  }
  if (.Platform$OS.type == "windows") {
    for (path in windows_paths) {
      config <- viewer_auth_manifest(fixture, path = path)
      expect_identical(runtime$viewer_auth_validate_manifest(config), config)
    }
  }
})

test_that("credential resolution enforces bundle containment and file access", {
  runtime <- source_viewer_auth(globalenv())
  fixture <- viewer_auth_fixture()
  root <- file.path(fixture$root, "bundle")
  private <- file.path(root, "private-data", "auth")
  dir.create(private, recursive = TRUE)
  dir.create(file.path(root, "viewer", "www"), recursive = TRUE)
  bundled <- file.path(private, "credentials.sqlite")
  expect_true(file.copy(fixture$database, bundled))

  host <- viewer_auth_manifest(fixture)
  expect_identical(
    runtime$viewer_auth_resolve_credentials(host, root),
    fixture$database
  )
  bundle <- viewer_auth_manifest(fixture, "bundle")
  expect_identical(
    runtime$viewer_auth_resolve_credentials(bundle, root),
    normalizePath(bundled, winslash = "/", mustWork = TRUE)
  )

  for (bad in c(
    "private-data/auth/../credentials.sqlite"
  )) {
    config <- bundle
    config$credentials_path <- bad
    expect_error(
      runtime$viewer_auth_resolve_credentials(config, root),
      "Invalid viewer authentication manifest.",
      fixed = TRUE
    )
  }

  missing <- host
  missing$credentials_path <- file.path(fixture$root, "missing.sqlite")
  expect_error(
    runtime$viewer_auth_resolve_credentials(missing, root),
    "Authentication credentials database is not accessible.",
    fixed = TRUE
  )
})

test_that("bundle symlinks cannot escape and host resource checks use boundaries", {
  skip_on_os("windows")
  runtime <- source_viewer_auth(globalenv())
  fixture <- viewer_auth_fixture()
  root <- file.path(fixture$root, "bundle")
  private <- file.path(root, "private-data", "auth")
  dir.create(private, recursive = TRUE)
  link <- file.path(private, "credentials.sqlite")
  expect_true(file.symlink(fixture$database, link))
  bundle <- viewer_auth_manifest(fixture, "bundle")
  expect_error(
    runtime$viewer_auth_resolve_credentials(bundle, root),
    "Invalid viewer authentication credentials path.",
    fixed = TRUE
  )

  viewer_www <- file.path(root, "viewer", "www")
  viewer_www2 <- file.path(root, "viewer", "www2")
  dir.create(viewer_www, recursive = TRUE)
  dir.create(viewer_www2)
  inside <- file.path(viewer_www, "credentials.sqlite")
  sibling <- file.path(viewer_www2, "credentials.sqlite")
  expect_true(file.copy(fixture$database, inside))
  expect_true(file.copy(fixture$database, sibling))

  config <- viewer_auth_manifest(fixture, path = inside)
  expect_error(
    runtime$viewer_auth_resolve_credentials(config, root),
    "Authentication credentials must not be in an HTTP resource directory.",
    fixed = TRUE
  )
  config$credentials_path <- sibling
  expect_identical(
    runtime$viewer_auth_resolve_credentials(config, root),
    normalizePath(sibling, winslash = "/", mustWork = TRUE)
  )

  prefix <- paste0("runtime-auth-", sample.int(1e8, 1L))
  shiny::addResourcePath(prefix, fixture$root)
  withr::defer(shiny::removeResourcePath(prefix))
  config$credentials_path <- fixture$database
  expect_error(
    runtime$viewer_auth_resolve_credentials(config, root),
    "Authentication credentials must not be in an HTTP resource directory.",
    fixed = TRUE
  )
})

test_that("host resource discovery fails closed and non-regular files fail", {
  runtime <- source_viewer_auth(globalenv())
  fixture <- viewer_auth_fixture()
  root <- file.path(fixture$root, "runtime-root")
  dir.create(file.path(root, "viewer", "www"), recursive = TRUE)
  config <- viewer_auth_manifest(fixture)
  expected <- "Authentication credentials must not be in an HTTP resource directory."

  testthat::with_mocked_bindings(
    expect_error(
      runtime$viewer_auth_resolve_credentials(config, root),
      expected,
      fixed = TRUE
    ),
    resourcePaths = function() c(missing = file.path(root, "absent")),
    .package = "shiny"
  )

  skip_on_os("windows")
  fifo <- file.path(fixture$root, "runtime-credentials.fifo")
  skip_if(system2("mkfifo", fifo) != 0L, "mkfifo is unavailable")
  config$credentials_path <- fifo
  expect_error(
    runtime$viewer_auth_resolve_credentials(config, root),
    "Authentication credentials database is not accessible.",
    fixed = TRUE
  )
})

test_that("credential access checks honor demonstrable POSIX permissions", {
  skip_on_os("windows")
  runtime <- source_viewer_auth(globalenv())
  fixture <- viewer_auth_fixture()
  root <- file.path(fixture$root, "permission-root")
  dir.create(file.path(root, "viewer", "www"), recursive = TRUE)

  readonly <- file.path(fixture$root, "runtime-readonly.sqlite")
  expect_true(file.copy(fixture$database, readonly))
  Sys.chmod(readonly, mode = "0400")
  withr::defer(Sys.chmod(readonly, mode = "0600"))
  if (file.access(readonly, mode = 6L) != 0L) {
    config <- viewer_auth_manifest(fixture, path = readonly)
    expect_error(
      runtime$viewer_auth_resolve_credentials(config, root),
      "Authentication credentials database is not accessible.",
      fixed = TRUE
    )
  }

  locked_parent <- file.path(fixture$root, "runtime-locked-parent")
  dir.create(locked_parent)
  locked <- file.path(locked_parent, "credentials.sqlite")
  expect_true(file.copy(fixture$database, locked))
  Sys.chmod(locked_parent, mode = "0500")
  withr::defer(Sys.chmod(locked_parent, mode = "0700"))
  if (file.access(locked_parent, mode = 3L) != 0L) {
    config <- viewer_auth_manifest(fixture, path = locked)
    expect_error(
      runtime$viewer_auth_resolve_credentials(config, root),
      "Authentication credentials database is not accessible.",
      fixed = TRUE
    )
  }
})

test_that("secret reading is environment-only and redacted", {
  runtime <- source_viewer_auth(baseenv())
  env <- "CEREBRO_RUNTIME_SECRET_TEST"
  secret <- paste0("runtime-sentinel-", strrep("s", 32L))
  withr::local_envvar(stats::setNames(secret, env))
  expect_identical(runtime$viewer_auth_read_secret(env), secret)

  for (value in c(NA_character_, "", "too-short")) {
    withr::local_envvar(stats::setNames(value, env))
    condition <- tryCatch(
      runtime$viewer_auth_read_secret(env),
      error = identity
    )
    expect_s3_class(condition, "error")
    expect_match(conditionMessage(condition), env, fixed = TRUE)
    if (!is.na(value) && nzchar(value)) {
      expect_false(grepl(value, conditionMessage(condition), fixed = TRUE))
    }
  }
})

test_that("database preflight validates real encrypted tables without leakage", {
  runtime <- source_viewer_auth(baseenv())
  fixture <- viewer_auth_fixture()
  token <- getFromNamespace(".tok", "shinymanager")
  before <- list(
    sqlite_path = token$get_sqlite_path(),
    passphrase = token$get_passphrase()
  )
  withr::defer({
    token$set_sqlite_path(before$sqlite_path)
    token$set_passphrase(before$passphrase)
  })
  success <- capture_viewer_auth_conditions(
    runtime$viewer_auth_validate_database(
      fixture$database,
      fixture$passphrase,
      fixture$env_name
    )
  )
  expect_identical(success$value, TRUE)
  expect_false(any(grepl(
    fixture$passphrase,
    c(success$warnings, success$messages, success$stdout, success$stderr),
    fixed = TRUE
  )))
  expect_identical(token$get_sqlite_path(), before$sqlite_path)
  expect_identical(token$get_passphrase(), before$passphrase)

  sentinel <- paste0("wrong-runtime-secret-", strrep("q", 32L))
  captured <- capture_viewer_auth_conditions(
    runtime$viewer_auth_validate_database(
      fixture$database,
      sentinel,
      fixture$env_name
    )
  )
  expect_s3_class(captured$value, "error")
  expect_match(conditionMessage(captured$value), fixture$env_name, fixed = TRUE)
  expect_false(any(grepl(
    sentinel,
    c(
      conditionMessage(captured$value),
      captured$warnings,
      captured$messages,
      captured$stdout,
      captured$stderr
    ),
    fixed = TRUE
  )))
  expect_identical(token$get_sqlite_path(), before$sqlite_path)
  expect_identical(token$get_passphrase(), before$passphrase)

  malformed <- file.path(fixture$root, "runtime-bad-hash.sqlite")
  expect_true(file.copy(fixture$database, malformed))
  credentials <- shinymanager::read_db_decrypt(
    conn = malformed,
    name = "credentials",
    passphrase = fixture$passphrase
  )
  credentials$is_hashed_password <- rep(2, nrow(credentials))
  shinymanager::write_db_encrypt(
    conn = malformed,
    value = credentials,
    name = "credentials",
    passphrase = fixture$passphrase
  )
  expect_error(
    runtime$viewer_auth_validate_database(
      malformed,
      fixture$passphrase,
      fixture$env_name
    ),
    paste0(
      "Authentication credentials database is invalid; check ",
      fixture$env_name,
      "."
    ),
    fixed = TRUE
  )

  required <- list(
    credentials = "password",
    pwd_mngt = "must_change",
    logs = "token"
  )
  for (table in names(required)) {
    database <- file.path(
      fixture$root,
      paste0("runtime-missing-", table, ".sqlite")
    )
    expect_true(file.copy(fixture$database, database))
    value <- shinymanager::read_db_decrypt(
      conn = database,
      name = table,
      passphrase = fixture$passphrase
    )
    value[[required[[table]]]] <- NULL
    shinymanager::write_db_encrypt(
      conn = database,
      value = value,
      name = table,
      passphrase = fixture$passphrase
    )
    expect_error(
      runtime$viewer_auth_validate_database(
        database,
        fixture$passphrase,
        fixture$env_name
      ),
      fixture$env_name,
      fixed = TRUE,
      info = paste("missing runtime column in", table)
    )
  }

  duplicated <- file.path(fixture$root, "runtime-duplicate-column.sqlite")
  expect_true(file.copy(fixture$database, duplicated))
  value <- shinymanager::read_db_decrypt(
    conn = duplicated,
    name = "credentials",
    passphrase = fixture$passphrase
  )
  value[[ncol(value) + 1L]] <- value$user
  names(value)[ncol(value)] <- "user"
  shinymanager::write_db_encrypt(
    conn = duplicated,
    value = value,
    name = "credentials",
    passphrase = fixture$passphrase
  )
  expect_error(
    runtime$viewer_auth_validate_database(
      duplicated,
      fixture$passphrase,
      fixture$env_name
    ),
    fixture$env_name,
    fixed = TRUE
  )

  user_mutations <- list(
    duplicate_credentials = list(
      table = "credentials",
      mutate = function(value) {
        value$user[[2L]] <- value$user[[1L]]
        value
      }
    ),
    mismatched_pwd_users = list(table = "pwd_mngt", mutate = function(value) {
      value$user[[1L]] <- "not-a-credential-user"
      value
    })
  )
  for (name in names(user_mutations)) {
    mutation <- user_mutations[[name]]
    database <- file.path(fixture$root, paste0("runtime-", name, ".sqlite"))
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
    expect_error(
      runtime$viewer_auth_validate_database(
        database,
        fixture$passphrase,
        fixture$env_name
      ),
      fixture$env_name,
      fixed = TRUE,
      info = paste("invalid user mapping:", name)
    )
  }

  matrix_mutations <- list(
    user_matrix = list(
      column = "user",
      mutate = function(value) matrix(value$user, ncol = 1L)
    ),
    hash_matrix = list(
      column = "is_hashed_password",
      mutate = function(value) {
        matrix(value$is_hashed_password, ncol = 1L)
      }
    )
  )
  for (name in names(matrix_mutations)) {
    mutation <- matrix_mutations[[name]]
    database <- file.path(fixture$root, paste0("runtime-", name, ".sqlite"))
    expect_true(file.copy(fixture$database, database))
    value <- shinymanager::read_db_decrypt(
      conn = database,
      name = "credentials",
      passphrase = fixture$passphrase
    )
    value[[mutation$column]] <- mutation$mutate(value)
    shinymanager::write_db_encrypt(
      conn = database,
      value = value,
      name = "credentials",
      passphrase = fixture$passphrase
    )
    persisted <- shinymanager::read_db_decrypt(
      conn = database,
      name = "credentials",
      passphrase = fixture$passphrase
    )
    expect_false(is.null(dim(persisted[[mutation$column]])), info = name)
    expect_error(
      runtime$viewer_auth_validate_database(
        database,
        fixture$passphrase,
        fixture$env_name
      ),
      fixture$env_name,
      fixed = TRUE,
      info = paste("matrix credential column:", name)
    )
  }
})

test_that("auth helpers work without default attached packages", {
  fixture <- viewer_auth_fixture()
  cerebro_root <- file.path(fixture$root, "minimal-default-packages")
  dir.create(file.path(cerebro_root, "viewer", "www"), recursive = TRUE)
  compiler_path <- viewer_auth_compiler_path()
  runtime_path <- normalizePath(
    viewer_auth_runtime_path(),
    winslash = "/",
    mustWork = TRUE
  )
  package_root <- viewer_auth_package_root()

  result <- callr::r(
    function(
      compiler_path,
      runtime_path,
      database,
      passphrase,
      env_name,
      cerebro_root,
      package_root,
      library_paths
    ) {
      base::.libPaths(library_paths)
      base::loadNamespace("utils")
      base::loadNamespace("stats")
      base::loadNamespace("shiny")
      base::loadNamespace("shinymanager")
      base::do.call(
        base::Sys.setenv,
        stats::setNames(base::list(passphrase), env_name)
      )
      compile_auth <- if (!base::is.null(compiler_path)) {
        compiler <- base::new.env(parent = baseenv())
        base::sys.source(compiler_path, envir = compiler)
        compiler$.compileViewerAuth
      } else {
        base::loadNamespace("CerebroNexus")
        loaded_root <- base::system.file(package = "CerebroNexus")
        if (
          !base::identical(
            base::normalizePath(loaded_root, winslash = "/", mustWork = TRUE),
            package_root
          )
        ) {
          base::stop("Loaded an unexpected CerebroNexus installation.")
        }
        base::get(
          ".compileViewerAuth",
          envir = base::asNamespace("CerebroNexus"),
          inherits = FALSE
        )
      }
      descriptor <- base::list(
        provider = "shinymanager",
        credentials = database,
        passphrase_env = env_name,
        timeout_minutes = 15
      )
      compiled <- compile_auth(
        descriptor,
        "host",
        cerebro_root = cerebro_root
      )

      runtime <- base::new.env(parent = baseenv())
      base::sys.source(runtime_path, envir = runtime)
      resolved <- runtime$viewer_auth_resolve_credentials(
        compiled$config,
        cerebro_root
      )
      runtime$viewer_auth_validate_database(
        resolved,
        passphrase,
        env_name
      )
      base::list(
        default_packages = base::Sys.getenv("R_DEFAULT_PACKAGES"),
        compiled = compiled$source,
        resolved = resolved
      )
    },
    args = list(
      compiler_path = compiler_path,
      runtime_path = runtime_path,
      database = fixture$database,
      passphrase = fixture$passphrase,
      env_name = fixture$env_name,
      cerebro_root = cerebro_root,
      package_root = package_root,
      library_paths = unique(c(dirname(package_root), .libPaths()))
    ),
    env = c(R_DEFAULT_PACKAGES = "NULL")
  )
  expect_identical(result$default_packages, "NULL")
  expect_identical(result$compiled, fixture$database)
  expect_identical(result$resolved, fixture$database)
})

test_that("brand reads only fixed runtime assets", {
  runtime <- source_viewer_auth(globalenv())
  root <- withr::local_tempdir()
  www <- file.path(root, "viewer", "www")
  dir.create(www, recursive = TRUE)
  css <- "/* fixed auth css */"
  svg <- "<svg xmlns='http://www.w3.org/2000/svg'><text>Fixed</text></svg>"
  writeLines(css, file.path(www, "auth.css"))
  writeLines(svg, file.path(www, "cerebronexus.svg"))

  brand <- runtime$viewer_auth_brand(root)
  expect_s3_class(brand$head, "shiny.tag")
  expect_identical(
    brand$head,
    shiny::includeCSS(file.path(www, "auth.css"))
  )
  expect_match(as.character(brand$top), "cerebro-auth-brand", fixed = TRUE)
  expect_match(as.character(brand$top), "Fixed", fixed = TRUE)
  expect_match(as.character(brand$bottom), "CerebroNexus", fixed = TRUE)
})

test_that("provider exposes preflight and callable secure adapters", {
  runtime <- source_viewer_auth(globalenv())
  fixture <- viewer_auth_fixture()
  config <- viewer_auth_manifest(fixture)
  brand <- runtime$viewer_auth_brand(
    viewer_auth_package_root()
  )
  provider <- runtime$viewer_auth_shinymanager_provider(
    config,
    fixture$database,
    brand
  )
  expect_named(provider, c("preflight", "secure_ui", "secure_server"))
  expect_true(all(vapply(provider, is.function, logical(1))))
  expect_invisible(provider$preflight(fixture$passphrase))
})

test_that("provider dependency is checked before adapters are returned", {
  runtime <- source_viewer_auth(globalenv())
  fixture <- viewer_auth_fixture()
  config <- viewer_auth_manifest(fixture)
  brand <- list(head = NULL, top = NULL, bottom = NULL)

  runtime$.viewer_auth_provider_version <- function() NULL
  expect_error(
    runtime$viewer_auth_shinymanager_provider(config, fixture$database, brand),
    "Authentication requires shinymanager (>= 1.1.0).",
    fixed = TRUE
  )
  runtime$.viewer_auth_provider_version <- function() numeric_version("1.0.9")
  expect_error(
    runtime$viewer_auth_shinymanager_provider(config, fixture$database, brand),
    "Authentication requires shinymanager (>= 1.1.0).",
    fixed = TRUE
  )
})

test_that("provider UI adapter supplies the frozen secure_app arguments", {
  runtime <- source_viewer_auth(globalenv())
  fixture <- viewer_auth_fixture()
  config <- viewer_auth_manifest(fixture)
  brand <- list(head = "head", top = "top", bottom = "bottom")
  provider <- runtime$viewer_auth_shinymanager_provider(
    config,
    fixture$database,
    brand
  )
  captured <- testthat::with_mocked_bindings(
    provider$secure_ui("viewer-ui"),
    secure_app = function(...) list(...),
    .package = "shinymanager"
  )
  expect_identical(
    captured,
    list(
      "viewer-ui",
      enable_admin = FALSE,
      head_auth = "head",
      tags_top = "top",
      tags_bottom = "bottom",
      status = "primary",
      language = "en"
    )
  )
})

test_that("session checker failures are stable and never disclose secrets", {
  runtime <- source_viewer_auth(globalenv())
  fixture <- viewer_auth_fixture()
  config <- viewer_auth_manifest(fixture)
  provider <- runtime$viewer_auth_shinymanager_provider(
    config,
    fixture$database,
    list(head = NULL, top = NULL, bottom = NULL)
  )
  sentinel <- paste0("session-only-secret-", strrep("v", 32L))
  withr::local_envvar(stats::setNames(sentinel, fixture$env_name))
  captured <- testthat::with_mocked_bindings(
    capture_viewer_auth_conditions(provider$secure_server(list())),
    check_credentials = function(...) {
      warning(paste("upstream exposed", sentinel))
      function(...) TRUE
    },
    .package = "shinymanager"
  )
  expect_s3_class(captured$value, "error")
  expect_identical(
    conditionMessage(captured$value),
    paste0("Authentication provider failed; check ", fixture$env_name, ".")
  )
  expect_false(any(grepl(
    sentinel,
    c(
      conditionMessage(captured$value),
      captured$warnings,
      captured$messages,
      captured$stdout,
      captured$stderr
    ),
    fixed = TRUE
  )))
})

test_that("successful session adapter passes the frozen secure_server arguments", {
  runtime <- source_viewer_auth(globalenv())
  fixture <- viewer_auth_fixture()
  config <- viewer_auth_manifest(fixture)
  config$timeout_minutes <- 37L
  provider <- runtime$viewer_auth_shinymanager_provider(
    config,
    fixture$database,
    list(head = NULL, top = NULL, bottom = NULL)
  )
  checker <- function(user, password) TRUE
  session <- new.env(parent = emptyenv())
  state <- structure(list(user = "viewer"), class = "auth-state")
  captured <- new.env(parent = emptyenv())
  result <- testthat::with_mocked_bindings(
    provider$secure_server(session),
    check_credentials = function(db, passphrase) {
      expect_identical(db, fixture$database)
      expect_identical(passphrase, fixture$passphrase)
      checker
    },
    secure_server = function(check_credentials, timeout, keep_token, session) {
      captured$arguments <- list(
        check_credentials = check_credentials,
        timeout = timeout,
        keep_token = keep_token,
        session = session
      )
      state
    },
    .package = "shinymanager"
  )
  expect_identical(result, state)
  expect_identical(
    captured$arguments,
    list(
      check_credentials = checker,
      timeout = 37L,
      keep_token = FALSE,
      session = session
    )
  )
})

test_that("secure_server construction conditions are synchronously redacted", {
  runtime <- source_viewer_auth(globalenv())
  fixture <- viewer_auth_fixture()
  config <- viewer_auth_manifest(fixture)
  provider <- runtime$viewer_auth_shinymanager_provider(
    config,
    fixture$database,
    list(head = NULL, top = NULL, bottom = NULL)
  )
  sentinel <- paste0("secure-server-upstream-", strrep("r", 32L))
  state <- structure(list(user = "viewer"), class = "auth-state")

  cases <- list(
    warning = function(...) {
      warning(sentinel)
      state
    },
    message = function(...) {
      message(sentinel)
      state
    },
    error = function(...) stop(sentinel, call. = FALSE)
  )
  for (name in names(cases)) {
    captured <- testthat::with_mocked_bindings(
      capture_viewer_auth_conditions(provider$secure_server(list())),
      check_credentials = function(...) function(...) TRUE,
      secure_server = cases[[name]],
      .package = "shinymanager"
    )
    if (identical(name, "error")) {
      expect_s3_class(captured$value, "error")
      expect_identical(
        conditionMessage(captured$value),
        paste0("Authentication provider failed; check ", fixture$env_name, ".")
      )
    } else {
      expect_identical(captured$value, state, info = name)
    }
    channels <- c(
      if (inherits(captured$value, "error")) conditionMessage(captured$value),
      captured$warnings,
      captured$messages,
      captured$stdout,
      captured$stderr
    )
    expect_false(any(grepl(sentinel, channels, fixed = TRUE)), info = name)
  }
})

test_that("server gate starts viewer exactly once after a valid user", {
  runtime <- source_viewer_auth(globalenv())
  starts <- 0L
  auth_server <- function(session) shiny::reactiveValues(user = NULL)
  viewer_server <- function(input, output, session) starts <<- starts + 1L
  gated <- runtime$viewer_auth_gate_server(viewer_server, auth_server)

  shiny::testServer(gated, {
    session$flushReact()
    expect_identical(starts, 0L)
    auth_state$user <- "viewer"
    session$flushReact()
    expect_identical(starts, 1L)
    auth_state$user <- "reviewer"
    session$flushReact()
    expect_identical(starts, 1L)
  })
})

test_that("server gate freezes both server adapters when it is constructed", {
  runtime <- source_viewer_auth(globalenv())
  selected <- character()
  viewer_server <- function(input, output, session) selected <<- "original"
  auth_server <- function(session) shiny::reactiveValues(user = "viewer")
  gated <- runtime$viewer_auth_gate_server(viewer_server, auth_server)
  viewer_server <- function(input, output, session) selected <<- "replacement"
  auth_server <- function(session) shiny::reactiveValues(user = NULL)

  shiny::testServer(gated, {
    session$flushReact()
    expect_identical(selected, "original")
  })
})

test_that("apply preflights once, wraps UI, and returns a gated server", {
  runtime <- source_viewer_auth(globalenv())
  fixture <- viewer_auth_fixture()
  config <- viewer_auth_manifest(fixture)
  calls <- new.env(parent = emptyenv())
  calls$preflight <- 0L
  calls$starts <- 0L
  auth_state <- shiny::reactiveValues(user = NULL)
  provider_factory <- function(received_config, credentials, brand) {
    expect_identical(received_config, config)
    expect_identical(credentials, fixture$database)
    expect_named(brand, c("head", "top", "bottom"))
    list(
      preflight = function(secret) {
        expect_identical(secret, fixture$passphrase)
        calls$preflight <- calls$preflight + 1L
        invisible(TRUE)
      },
      secure_ui = function(viewer_ui) list(secured = viewer_ui),
      secure_server = function(session) auth_state
    )
  }
  viewer_server <- function(input, output, session) {
    calls$starts <- calls$starts + 1L
  }
  app <- runtime$viewer_auth_apply(
    list(page = TRUE),
    viewer_server,
    config,
    viewer_auth_package_root(),
    provider_factory = provider_factory
  )
  expect_identical(app$ui, list(secured = list(page = TRUE)))
  expect_identical(calls$preflight, 1L)
  expect_true(is.function(app$server))

  expect_false(viewer_auth_package_environment_contains(
    app,
    fixture$passphrase,
    runtime
  ))

  shiny::testServer(app$server, {
    session$flushReact()
    expect_identical(calls$starts, 0L)
    auth_state$user <- "viewer"
    session$flushReact()
    expect_identical(calls$starts, 1L)
  })
})

test_that("package-authored app closures do not retain preflight passphrase", {
  runtime <- source_viewer_auth(globalenv())
  fixture <- viewer_auth_fixture()
  config <- viewer_auth_manifest(fixture)
  root <- viewer_auth_package_root()
  app <- runtime$viewer_auth_apply(
    shiny::fluidPage("viewer"),
    function(input, output, session) NULL,
    config,
    root
  )
  expect_false(viewer_auth_package_environment_contains(
    app,
    fixture$passphrase,
    runtime
  ))
})

test_that("authentication CSS is fully scoped and retains accessible states", {
  path <- file.path(viewer_auth_package_root(), "viewer", "www", "auth.css")
  expect_true(file.exists(path))
  css <- paste(readLines(path, warn = FALSE), collapse = "\n")
  blocks <- regmatches(css, gregexpr("[^{}]+\\{", css, perl = TRUE))[[1L]]
  selectors <- trimws(sub("\\{$", "", blocks))
  selectors <- selectors[!startsWith(selectors, "@")]
  selector_parts <- trimws(unlist(strsplit(selectors, ",", fixed = TRUE)))
  has_scope <- function(selector, prefix) {
    identical(selector, prefix) ||
      any(vapply(
        c(" ", ".", ":", "#", "["),
        function(separator) startsWith(selector, paste0(prefix, separator)),
        logical(1)
      ))
  }
  expect_true(all(vapply(
    selector_parts,
    function(selector) {
      has_scope(selector, ".panel-auth") ||
        has_scope(selector, ".cerebro-auth-brand")
    },
    logical(1)
  )))
  expect_match(css, "focus-visible", fixed = TRUE)
  expect_match(css, "@media", fixed = TRUE)
  expect_match(css, "@media (max-width: 576px)", fixed = TRUE)
  expect_false(grepl("[0-9.]rem\\b", css, perl = TRUE))
  expect_match(css, "flex-direction: column", fixed = TRUE)
  expect_match(css, ".panel-auth > .row", fixed = TRUE)
  expect_match(css, ".panel-auth .btn", fixed = TRUE)
  expect_match(css, ".panel-auth .alert", fixed = TRUE)
  expect_false(grepl("display\\s*:\\s*none", css, perl = TRUE))
})
