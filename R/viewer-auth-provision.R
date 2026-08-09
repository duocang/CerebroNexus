.viewerAuthProvisionOpNames <- c(
  "random_bytes",
  "sha256",
  "now",
  "os_type",
  "getenv",
  "setenv",
  "unsetenv",
  "normalize_existing",
  "effective_uid",
  "inspect_path",
  "components_safe",
  "dir_create",
  "chmod",
  "save_rds",
  "read_rds",
  "write_raw",
  "read_raw",
  "rename",
  "list_files",
  "remove_file",
  "remove_dir",
  "namespace_available",
  "package_version",
  "create_db",
  "validate_db",
  "finish_hook"
)

.viewerAuthProvisionOps <- function() {
  ops <- list(
    random_bytes = function(size) openssl::rand_bytes(size),
    sha256 = function(bytes) openssl::sha256(bytes),
    now = function() {
      format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC", usetz = FALSE)
    },
    os_type = function() .Platform$OS.type,
    getenv = function(name) Sys.getenv(name, unset = NA_character_),
    setenv = function(name, value) {
      do.call(Sys.setenv, stats::setNames(list(value), name))
    },
    unsetenv = function(name) Sys.unsetenv(name),
    normalize_existing = function(path) {
      normalizePath(
        path,
        winslash = if (.Platform$OS.type == "windows") "/" else "native",
        mustWork = TRUE
      )
    },
    effective_uid = function() {
      value <- tryCatch(
        system2("id", "-u", stdout = TRUE, stderr = FALSE),
        error = function(e) character()
      )
      if (length(value) == 1L && grepl("^[0-9]+$", value)) {
        value
      } else {
        NA_character_
      }
    },
    inspect_path = function(path) {
      info <- fs::file_info(path)
      exists <- length(info$type) == 1L && !is.na(info$type)
      list(
        exists = exists,
        type = if (exists) as.character(info$type) else NA_character_,
        permissions = if (exists) {
          as.character(info$permissions)
        } else {
          NA_character_
        },
        uid = if (exists) {
          as.character(file.info(path)$uid[[1L]])
        } else {
          NA_character_
        },
        size = if (exists) as.double(info$size) else NA_real_,
        device_id = if (exists) as.character(info$device_id) else NA_character_,
        inode = if (exists) as.character(info$inode) else NA_character_,
        is_link = isTRUE(fs::is_link(path))
      )
    },
    components_safe = function(path) {
      if (identical(.Platform$OS.type, "windows")) {
        return(FALSE)
      }
      current <- path
      checked <- character()
      repeat {
        checked <- c(current, checked)
        parent <- dirname(current)
        if (identical(parent, current)) {
          break
        }
        current <- parent
      }
      all(vapply(checked, function(one) !isTRUE(fs::is_link(one)), logical(1)))
    },
    dir_create = function(path, mode) {
      dir.create(path, mode = mode, showWarnings = FALSE, recursive = FALSE)
    },
    chmod = function(path, mode) {
      Sys.chmod(path, mode = mode, use_umask = FALSE)
      TRUE
    },
    save_rds = function(value, path) {
      saveRDS(value, path, version = 2L)
      TRUE
    },
    read_rds = function(path) readRDS(path),
    write_raw = function(bytes, path) {
      connection <- file(path, open = "wb")
      on.exit(close(connection), add = TRUE)
      writeBin(bytes, connection, useBytes = TRUE)
      TRUE
    },
    read_raw = function(path, size) readBin(path, "raw", n = size),
    rename = function(from, to) isTRUE(file.rename(from, to)),
    list_files = function(path) {
      list.files(
        path,
        all.files = TRUE,
        no.. = TRUE,
        recursive = FALSE,
        full.names = FALSE
      )
    },
    remove_file = function(path) unlink(path, recursive = FALSE) == 0L,
    remove_dir = function(path) isTRUE(file.remove(path)),
    namespace_available = function(package) {
      requireNamespace(package, quietly = TRUE)
    },
    package_version = function(package) utils::packageVersion(package),
    create_db = function(credentials_data, sqlite_path, passphrase) {
      shinymanager::create_db(
        credentials_data = credentials_data,
        sqlite_path = sqlite_path,
        passphrase = passphrase
      )
      TRUE
    },
    validate_db = function(path, passphrase) {
      .viewerAuthValidateDatabase(path, passphrase)
    },
    finish_hook = function(state) invisible(NULL)
  )
  stopifnot(identical(names(ops), .viewerAuthProvisionOpNames))
  ops
}

.viewerAuthRandomRaw <- function(ops, size, purpose) {
  value <- tryCatch(ops$random_bytes(as.integer(size)), error = function(e) {
    NULL
  })
  if (!is.raw(value) || length(value) != size) {
    .viewerAuthProvisionAbort(
      "secret_generation_failed",
      "preflight",
      paste0("Could not generate ", purpose, ".")
    )
  }
  value
}

.viewerAuthProvisionHex <- function(bytes, uppercase = FALSE) {
  stopifnot(is.raw(bytes))
  value <- paste(sprintf("%02x", as.integer(bytes)), collapse = "")
  if (uppercase) toupper(value) else value
}

.viewerAuthReadEnvironmentValue <- function(ops, name) {
  value <- tryCatch(ops$getenv(name), error = function(e) NULL)
  if (!is.character(value) || length(value) != 1L) {
    return(list(ok = FALSE, value = NA_character_))
  }
  list(ok = TRUE, value = value)
}

.viewerAuthResolveEnvironmentName <- function(
  requested,
  ops,
  max_attempts = 100L
) {
  available <- function(name) {
    observed <- .viewerAuthReadEnvironmentValue(ops, name)
    if (!isTRUE(observed$ok)) {
      .viewerAuthProvisionAbort(
        "environment_conflict",
        "environment",
        "Could not inspect the passphrase environment-variable name."
      )
    }
    is.na(observed$value)
  }
  if (!is.null(requested)) {
    if (!available(requested)) {
      .viewerAuthProvisionAbort(
        "environment_conflict",
        "environment",
        "The requested passphrase environment variable is already set."
      )
    }
    return(requested)
  }
  for (attempt in seq_len(max_attempts)) {
    candidate <- paste0(
      "CEREBRO_AUTH_PASSPHRASE_",
      .viewerAuthProvisionHex(
        .viewerAuthRandomRaw(ops, 8L, "an environment-variable suffix"),
        uppercase = TRUE
      )
    )
    if (available(candidate)) return(candidate)
  }
  .viewerAuthProvisionAbort(
    "environment_conflict",
    "environment",
    "Could not reserve an unused passphrase environment-variable name."
  )
}

.viewerAuthPrepareProvisionIdentity <- function(state) {
  operation <- .viewerAuthRandomRaw(state$ops, 16L, "an operation identifier")
  secret <- .viewerAuthRandomRaw(state$ops, 32L, "a database passphrase")
  digest <- tryCatch(
    state$ops$sha256(charToRaw(enc2utf8(state$preflight$target_path))),
    error = function(e) NULL
  )
  if (!is.raw(digest) || length(digest) != 32L) {
    .viewerAuthProvisionAbort(
      "secret_generation_failed",
      "preflight",
      "Could not derive a target identifier."
    )
  }
  state$identity <- list(
    operation_id = .viewerAuthProvisionHex(operation),
    target_hash = substr(.viewerAuthProvisionHex(digest), 1L, 16L),
    passphrase_env = .viewerAuthResolveEnvironmentName(
      state$options$passphrase_env,
      state$ops,
      100L
    )
  )
  state$passphrase <- .viewerAuthProvisionHex(secret)
  invisible(state)
}

# Provider output may include credentials or passphrases.  Deliberately discard
# every R-level channel and only return an opaque success value.
.viewerAuthRunProvider <- function(state, provider) {
  provider_output <- character()
  provider_message <- character()
  output_connection <- textConnection("provider_output", "w", local = TRUE)
  message_connection <- textConnection("provider_message", "w", local = TRUE)
  output_level <- sink.number(type = "output")
  message_level <- sink.number(type = "message")
  on.exit(
    {
      tryCatch(
        while (sink.number(type = "message") > message_level) {
          sink(type = "message")
        },
        error = function(e) NULL
      )
      tryCatch(
        while (sink.number(type = "output") > output_level) {
          sink(type = "output")
        },
        error = function(e) NULL
      )
      tryCatch(close(message_connection), error = function(e) NULL)
      tryCatch(close(output_connection), error = function(e) NULL)
      provider_output <- NULL
      provider_message <- NULL
      state$provider_capture <- list(output = NULL, message = NULL)
    },
    add = TRUE
  )
  sink(output_connection, type = "output")
  sink(message_connection, type = "message")
  value <- tryCatch(
    withCallingHandlers(
      provider(),
      warning = function(condition) invokeRestart("muffleWarning"),
      message = function(condition) invokeRestart("muffleMessage")
    ),
    error = function(condition) {
      structure(list(), class = "viewer_auth_provider_failure")
    }
  )
  list(
    ok = !inherits(value, "viewer_auth_provider_failure"),
    value = if (inherits(value, "viewer_auth_provider_failure")) NULL else value
  )
}

.viewerAuthRequireProvisionDependencies <- function(state) {
  openssl_ok <- isTRUE(state$ops$namespace_available("openssl"))
  shinymanager_ok <- isTRUE(state$ops$namespace_available("shinymanager")) &&
    tryCatch(
      state$ops$package_version("shinymanager") >= numeric_version("1.1.0"),
      error = function(e) FALSE
    )
  if (!openssl_ok || !shinymanager_ok) {
    missing <- c(
      if (!openssl_ok) "openssl",
      if (!shinymanager_ok) "shinymanager"
    )
    .viewerAuthProvisionAbort(
      "missing_dependency",
      "dependency",
      paste0(
        "Authentication provisioning requires: ",
        paste(missing, collapse = ", "),
        "."
      )
    )
  }
  invisible(state)
}

.viewerAuthCreateProvisionDatabase <- function(state) {
  accounts <- state$accounts
  on.exit(
    {
      accounts <- NULL
      state$accounts <- NULL
    },
    add = TRUE
  )
  created <- .viewerAuthRunProvider(state, function() {
    state$ops$create_db(accounts, state$paths$credentials, state$passphrase)
  })
  if (!isTRUE(created$ok) || !isTRUE(created$value)) {
    .viewerAuthProvisionAbort(
      "database_create_failed",
      "database",
      "Could not create the authentication database."
    )
  }
  mode_ok <- tryCatch(
    isTRUE(state$ops$chmod(state$paths$credentials, "0600")),
    error = function(e) FALSE
  )
  if (!mode_ok) {
    .viewerAuthProvisionAbort(
      "database_create_failed",
      "database",
      "Could not secure the authentication database."
    )
  }
  .viewerAuthFreezeProvisionPath(
    state,
    "credentials",
    state$paths$credentials,
    "file",
    "rw-------",
    "database_create_failed",
    "database"
  )
  validated <- .viewerAuthRunProvider(state, function() {
    state$ops$validate_db(state$paths$credentials, state$passphrase)
  })
  if (!isTRUE(validated$ok) || !isTRUE(validated$value)) {
    .viewerAuthProvisionAbort(
      "database_validation_failed",
      "database",
      "The authentication database failed compatibility validation."
    )
  }
  invisible(state)
}

.viewerAuthSecretBytes <- function(name, passphrase) {
  charToRaw(paste0(name, "=", passphrase, "\n"))
}

.viewerAuthWriteProvisionSecret <- function(state) {
  state$secret_bytes <- .viewerAuthSecretBytes(
    state$identity$passphrase_env,
    state$passphrase
  )
  on.exit(state$secret_bytes <- NULL, add = TRUE)
  write_ok <- tryCatch(
    isTRUE(state$ops$write_raw(state$secret_bytes, state$paths$secret_tmp)),
    error = function(e) FALSE
  )
  temp_info <- tryCatch(
    state$ops$inspect_path(state$paths$secret_tmp),
    error = function(e) NULL
  )
  if (is.list(temp_info) && isTRUE(temp_info$exists)) {
    .viewerAuthFreezeProvisionPath(
      state,
      "secret_tmp",
      state$paths$secret_tmp,
      "file",
      code = "artifact_publish_failed",
      stage = "artifact_stage"
    )
  }
  chmod_ok <- isTRUE(write_ok) &&
    tryCatch(
      isTRUE(state$ops$chmod(state$paths$secret_tmp, "0600")),
      error = function(e) FALSE
    )
  if (!isTRUE(write_ok) || !isTRUE(chmod_ok)) {
    .viewerAuthProvisionAbort(
      "artifact_publish_failed",
      "artifact_stage",
      "Could not write the private authentication secret."
    )
  }
  .viewerAuthFreezeProvisionPath(
    state,
    "secret_tmp",
    state$paths$secret_tmp,
    "file",
    "rw-------"
  )
  .viewerAuthControlledRenameRebind(
    state,
    "secret_tmp",
    state$paths$secret_tmp,
    "secret",
    state$paths$secret,
    "file",
    "rw-------",
    "artifact_publish_failed",
    "artifact_stage"
  )
  state$passphrase <- NULL
  invisible(state)
}

.viewerAuthReadProvisionSecret <- function(state, root) {
  bytes <- tryCatch(
    state$ops$read_raw(file.path(root, "viewer-auth.env"), 4097L),
    error = function(e) raw()
  )
  text <- tryCatch(rawToChar(bytes), error = function(e) NA_character_)
  pattern <- paste0("^", state$identity$passphrase_env, "=([0-9a-f]{64})\\n$")
  if (!.viewerAuthScalarString(text) || !grepl(pattern, text)) {
    .viewerAuthProvisionAbort(
      "artifact_publish_failed",
      "publish",
      "The private authentication secret failed validation."
    )
  }
  sub(pattern, "\\1", text)
}
