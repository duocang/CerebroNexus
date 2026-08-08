.viewerAuthSetupOps <- function() {
  list(
    is_interactive = function() base::interactive(),
    read_input = function(prompt) base::readline(prompt),
    read_password = function(prompt) askpass::askpass(prompt),
    random_bytes = function(size) openssl::rand_bytes(size),
    create_db = function(credentials_data, sqlite_path, passphrase) {
      suppressMessages(shinymanager::create_db(
        credentials_data = credentials_data,
        sqlite_path = sqlite_path,
        passphrase = passphrase
      ))
    },
    namespace_available = function(package) {
      requireNamespace(package, quietly = TRUE)
    },
    fs_info = function(path) fs::file_info(path, follow = FALSE),
    base_info = function(path) file.info(path),
    effective_uid = function() {
      value <- suppressWarnings(system2(
        "id",
        "-u",
        stdout = TRUE,
        stderr = FALSE
      ))
      if (
        length(value) != 1L ||
          is.na(value) ||
          !grepl("^[0-9]+$", value, perl = TRUE, useBytes = TRUE)
      ) {
        stop("Could not determine the effective user ID.", call. = FALSE)
      }
      uid <- suppressWarnings(as.numeric(value))
      if (length(uid) != 1L || is.na(uid) || !is.finite(uid) || uid < 0) {
        stop("Could not determine the effective user ID.", call. = FALSE)
      }
      uid
    },
    read_pinned = function(path) {
      .Call(C_cerebro_read_pinned_secret, path)
    },
    write_raw = function(bytes, path) {
      writeBin(bytes, path, useBytes = TRUE)
      TRUE
    },
    link = function(from, to) {
      suppressWarnings(tryCatch(
        isTRUE(file.link(from, to)),
        error = function(condition) FALSE
      ))
    },
    unlink_file = function(path) {
      status <- suppressWarnings(tryCatch(
        unlink(
          path,
          recursive = FALSE,
          force = FALSE,
          expand = FALSE
        ),
        error = function(condition) 1L
      ))
      identical(status, 0L) && !.bundlePathExists(path)
    },
    claim_dir = function(path, mode = "0700") {
      dir.create(
        path,
        recursive = FALSE,
        mode = mode,
        showWarnings = FALSE
      )
    },
    remove_dir = function(path) {
      suppressWarnings(tryCatch(
        isTRUE(file.remove(path)),
        error = function(condition) FALSE
      ))
    },
    create_private_file = function(path) {
      tryCatch(
        {
          fs::file_create(path, mode = "u=rw,go=")
          TRUE
        },
        error = function(condition) FALSE
      )
    },
    chmod = function(path, mode, use_umask = FALSE) {
      isTRUE(Sys.chmod(path, mode, use_umask = use_umask))
    },
    access = function(path, mode) {
      identical(unname(file.access(path, mode = mode)), 0L)
    },
    list_dir = function(path) {
      list.files(
        path,
        all.files = TRUE,
        full.names = TRUE,
        no.. = TRUE
      )
    },
    getenv = function(name, unset = NA_character_) {
      Sys.getenv(name, unset = unset)
    },
    setenv = function(name, value) {
      do.call(Sys.setenv, stats::setNames(list(value), name))
    },
    unsetenv = function(name) Sys.unsetenv(name),
    resource_paths = function() unname(shiny::resourcePaths()),
    entry_exists = function(path) .bundlePathExists(path)
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

.viewerAuthUnsafe <- function(path, changed = FALSE) {
  detail <- if (isTRUE(changed)) {
    "changed during inspection"
  } else {
    "is invalid or unsafe"
  }
  stop(
    "Authentication secret path '",
    path,
    "' ",
    detail,
    ".",
    call. = FALSE
  )
}

.viewerAuthCall <- function(expr) {
  tryCatch(
    withCallingHandlers(
      expr,
      warning = function(condition) invokeRestart("muffleWarning")
    ),
    error = function(condition) NULL
  )
}

.viewerAuthAccessGranted <- function(value) {
  isTRUE(value) || identical(value, 0L) || identical(value, 0)
}

.viewerAuthCanonicalExisting <- function(path) {
  tryCatch(
    normalizePath(path, winslash = "/", mustWork = TRUE),
    error = function(condition) NA_character_
  )
}

.viewerAuthIntendedExisting <- function(path) {
  parent <- .viewerAuthCanonicalExisting(dirname(path))
  if (is.na(parent)) {
    return(NA_character_)
  }
  file.path(parent, basename(path))
}

.viewerAuthSamePath <- function(left, right) {
  if (
    !is.character(left) ||
      length(left) != 1L ||
      is.na(left) ||
      !is.character(right) ||
      length(right) != 1L ||
      is.na(right)
  ) {
    return(FALSE)
  }
  identical(.nativePathKey(left), .nativePathKey(right))
}

.viewerAuthFilesystemRecord <- function(path, type, ops) {
  info <- .viewerAuthCall(ops$fs_info(path))
  if (
    !is.data.frame(info) ||
      nrow(info) != 1L ||
      !"type" %in% names(info) ||
      !identical(as.character(info$type[[1L]]), type) ||
      !all(c("device_id", "inode", "size") %in% names(info))
  ) {
    .viewerAuthUnsafe(path)
  }
  valid_identity_number <- function(field) {
    value <- info[[field]][[1L]]
    is.numeric(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      is.finite(value) &&
      value >= 0
  }
  if (
    !all(vapply(
      c("device_id", "inode", "size"),
      valid_identity_number,
      logical(1)
    ))
  ) {
    .viewerAuthUnsafe(path)
  }
  canonical <- .viewerAuthCanonicalExisting(path)
  intended <- .viewerAuthIntendedExisting(path)
  if (
    is.na(canonical) ||
      is.na(intended) ||
      !.viewerAuthSamePath(canonical, intended)
  ) {
    .viewerAuthUnsafe(path)
  }
  base <- .viewerAuthCall(ops$base_info(path))
  if (
    !is.data.frame(base) ||
      nrow(base) != 1L ||
      !all(c("mode", "uid") %in% names(base)) ||
      (!identical(.Platform$OS.type, "windows") &&
        (is.na(base$mode[[1L]]) || is.na(base$uid[[1L]])))
  ) {
    .viewerAuthUnsafe(path)
  }
  list(
    path = canonical,
    device_id = unname(as.numeric(info$device_id[[1L]])),
    inode = unname(as.numeric(info$inode[[1L]])),
    size = unname(as.numeric(info$size[[1L]])),
    mode = unname(as.integer(base$mode[[1L]])),
    uid = unname(as.numeric(base$uid[[1L]]))
  )
}

.viewerAuthCurrentUid <- function(ops, path) {
  uid <- .viewerAuthCall(ops$effective_uid())
  if (
    length(uid) != 1L ||
      is.na(uid) ||
      !is.numeric(uid) ||
      !is.finite(uid) ||
      uid < 0
  ) {
    .viewerAuthUnsafe(path)
  }
  as.numeric(uid)
}

.viewerAuthReadFileIdentity <- function(
  path,
  mode = "0600",
  ops = .viewerAuthSetupOps(),
  exact_mode = TRUE
) {
  snapshot <- .viewerAuthFilesystemRecord(path, "file", ops)
  access <- .viewerAuthCall(ops$access(path, 6L))
  if (!.viewerAuthAccessGranted(access)) {
    .viewerAuthUnsafe(path)
  }
  if (!identical(.Platform$OS.type, "windows")) {
    required_mode <- as.integer(as.octmode(mode))
    if (isTRUE(exact_mode) && !identical(snapshot$mode, required_mode)) {
      .viewerAuthUnsafe(path)
    }
    if (!identical(snapshot$uid, .viewerAuthCurrentUid(ops, path))) {
      .viewerAuthUnsafe(path)
    }
  }
  snapshot
}

.viewerAuthReadDirectoryIdentity <- function(
  path,
  mode = "0700",
  ops = .viewerAuthSetupOps(),
  exact_mode = TRUE,
  require_owner = TRUE
) {
  snapshot <- .viewerAuthFilesystemRecord(path, "directory", ops)
  access <- .viewerAuthCall(ops$access(path, 3L))
  if (!.viewerAuthAccessGranted(access)) {
    .viewerAuthUnsafe(path)
  }
  if (!identical(.Platform$OS.type, "windows")) {
    required_mode <- as.integer(as.octmode(mode))
    if (isTRUE(exact_mode) && !identical(snapshot$mode, required_mode)) {
      .viewerAuthUnsafe(path)
    }
    if (
      isTRUE(require_owner) &&
        !identical(snapshot$uid, .viewerAuthCurrentUid(ops, path))
    ) {
      .viewerAuthUnsafe(path)
    }
  }
  snapshot[c("path", "device_id", "inode", "mode", "uid")]
}

.viewerAuthSameArtifact <- function(left, right) {
  if (is.null(left) || is.null(right)) {
    return(FALSE)
  }
  fields <- c("device_id", "inode", "size", "mode", "uid", "raw")
  all(vapply(
    fields,
    function(field) {
      !is.null(left[[field]]) &&
        !is.null(right[[field]]) &&
        identical(left[[field]], right[[field]])
    },
    logical(1)
  ))
}

.viewerAuthSameFileIdentity <- function(left, right) {
  if (is.null(left) || is.null(right)) {
    return(FALSE)
  }
  fields <- c("path", "device_id", "inode", "size", "mode", "uid")
  all(vapply(
    fields,
    function(field) {
      if (identical(field, "path")) {
        .viewerAuthSamePath(left[[field]], right[[field]])
      } else {
        identical(left[[field]], right[[field]])
      }
    },
    logical(1)
  ))
}

.viewerAuthSamePathIdentity <- function(left, right) {
  if (is.null(left) || is.null(right)) {
    return(FALSE)
  }
  fields <- c("path", "device_id", "inode", "mode", "uid")
  all(vapply(
    fields,
    function(field) {
      if (identical(field, "path")) {
        .viewerAuthSamePath(left[[field]], right[[field]])
      } else {
        identical(left[[field]], right[[field]])
      }
    },
    logical(1)
  ))
}

.viewerAuthSameOwnedFile <- function(left, right) {
  .viewerAuthSamePathIdentity(left, right)
}

.viewerAuthSameDirectory <- function(left, right) {
  .viewerAuthSamePathIdentity(left, right)
}

.viewerAuthReadSecretFile <- function(
  path,
  ops = .viewerAuthSetupOps()
) {
  intended <- .viewerAuthIntendedExisting(path)
  if (is.na(intended)) {
    .viewerAuthUnsafe(path)
  }
  opened <- .viewerAuthCall(ops$read_pinned(intended))
  required <- c("raw", "device_id", "inode", "size", "mode", "uid")
  if (!is.list(opened) || !identical(names(opened), required)) {
    .viewerAuthUnsafe(path)
  }
  bytes <- opened$raw
  if (!is.raw(bytes)) {
    .viewerAuthUnsafe(path)
  }
  for (field in c("device_id", "inode", "size", "uid")) {
    value <- opened[[field]]
    if (
      !is.numeric(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !is.finite(value) ||
        value < 0
    ) {
      .viewerAuthUnsafe(path)
    }
  }
  if (
    !is.integer(opened$mode) ||
      length(opened$mode) != 1L ||
      is.na(opened$mode) ||
      opened$mode < 0L
  ) {
    .viewerAuthUnsafe(path)
  }
  opened$path <- intended
  opened <- opened[c("path", required[-1L], "raw")]
  if (length(bytes) != 106L || any(bytes == as.raw(0L))) {
    .viewerAuthUnsafe(path)
  }
  value <- tryCatch(rawToChar(bytes), error = function(condition) NA_character_)
  grammar <- paste0(
    "\\ACEREBRO_AUTH_PASSPHRASE_[A-F0-9]{16}=",
    "[a-f0-9]{64}\\n\\z"
  )
  if (
    length(value) != 1L ||
      is.na(value) ||
      !grepl(grammar, value, perl = TRUE, useBytes = TRUE)
  ) {
    .viewerAuthUnsafe(path)
  }
  separator <- regexpr("=", value, fixed = TRUE, useBytes = TRUE)[[1L]]
  opened$raw <- bytes
  opened$env_name <- substr(value, 1L, separator - 1L)
  opened$passphrase <- substr(value, separator + 1L, nchar(value) - 1L)
  opened
}

.viewerAuthRevalidateSecretFile <- function(snapshot, ops) {
  if (is.null(snapshot) || is.null(snapshot$path)) {
    stop("The authentication secret snapshot is unavailable.", call. = FALSE)
  }
  current <- .viewerAuthReadSecretFile(snapshot$path, ops)
  if (
    !.viewerAuthSamePath(snapshot$path, current$path) ||
      !.viewerAuthSameArtifact(snapshot, current)
  ) {
    .viewerAuthUnsafe(snapshot$path, changed = TRUE)
  }
  invisible(current)
}

.viewerAuthResourceRoots <- function(ops) {
  roots <- .viewerAuthCall(ops$resource_paths())
  if (!is.character(roots) || anyNA(roots) || any(!nzchar(roots))) {
    stop(
      "The authentication secret HTTP resource registry is invalid.",
      call. = FALSE
    )
  }
  if (!length(roots)) {
    return(character())
  }
  normalized <- vapply(
    roots,
    function(root) {
      .viewerAuthCanonicalExisting(root)
    },
    character(1)
  )
  if (anyNA(normalized)) {
    stop(
      "The authentication secret HTTP resource registry is invalid.",
      call. = FALSE
    )
  }
  unname(normalized)
}

.viewerAuthRejectSecretResourcePath <- function(path, ops) {
  roots <- .viewerAuthResourceRoots(ops)
  if (
    any(vapply(
      roots,
      function(root) {
        .viewerAuthPathWithin(path, root)
      },
      logical(1)
    ))
  ) {
    stop(
      "The authentication secret must not be located in an HTTP resource directory.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.viewerAuthNearestExistingParent <- function(path, ops) {
  candidate <- path
  repeat {
    if (isTRUE(.viewerAuthCall(ops$entry_exists(candidate)))) {
      return(candidate)
    }
    parent <- dirname(candidate)
    if (identical(parent, candidate)) {
      .viewerAuthUnsafe(path)
    }
    candidate <- parent
  }
}

.viewerAuthPreflightSecretTarget <- function(
  result_target,
  result_parent,
  secret_path,
  ops
) {
  .viewerAuthRejectSecretResourcePath(secret_path, ops)
  if (isTRUE(.viewerAuthCall(ops$entry_exists(result_parent)))) {
    parent_snapshot <- .viewerAuthReadDirectoryIdentity(
      result_parent,
      "0700",
      ops,
      exact_mode = FALSE,
      require_owner = FALSE
    )
    if (!.viewerAuthSamePath(parent_snapshot$path, result_parent)) {
      .viewerAuthUnsafe(result_parent)
    }
    return(list(
      result_parent_snapshot = parent_snapshot,
      parent_anchor_path = NULL,
      parent_anchor_snapshot = NULL
    ))
  }
  anchor <- .viewerAuthNearestExistingParent(result_parent, ops)
  anchor_snapshot <- .viewerAuthReadDirectoryIdentity(
    anchor,
    "0700",
    ops,
    exact_mode = FALSE,
    require_owner = FALSE
  )
  list(
    result_parent_snapshot = NULL,
    parent_anchor_path = anchor_snapshot$path,
    parent_anchor_snapshot = anchor_snapshot
  )
}

.viewerAuthPreflightSimple <- function(
  result_dir,
  ops = .viewerAuthSetupOps()
) {
  if (!isTRUE(ops$is_interactive())) {
    stop("auth = TRUE requires an interactive R session.", call. = FALSE)
  }
  .viewerAuthRequireDependencies(ops)
  result_target <- .stableBundleTarget(result_dir)
  result_parent <- dirname(result_target)
  secret_path <- paste0(result_target, ".auth.env")
  frozen <- .viewerAuthPreflightSecretTarget(
    result_target,
    result_parent,
    secret_path,
    ops
  )
  existing <- if (isTRUE(.viewerAuthCall(ops$entry_exists(secret_path)))) {
    .viewerAuthReadSecretFile(secret_path, ops)
  } else {
    NULL
  }
  state <- new.env(parent = emptyenv())
  state$result_target <- result_target
  state$result_parent <- result_parent
  state$result_parent_snapshot <- frozen$result_parent_snapshot
  state$parent_anchor_path <- frozen$parent_anchor_path
  state$parent_anchor_snapshot <- frozen$parent_anchor_snapshot
  state$accounts <- NULL
  state$env_name <- if (is.null(existing)) NULL else existing$env_name
  state$passphrase <- if (is.null(existing)) NULL else existing$passphrase
  state$secret_path <- secret_path
  state$existing_snapshot <- existing
  state$scratch_dir <- NULL
  state$scratch_snapshot <- NULL
  state$scratch_payload <- NULL
  state$scratch_payload_snapshot <- NULL
  state$candidate_path <- NULL
  state$candidate_snapshot <- NULL
  state$published_snapshot <- NULL
  state$prior_env <- NA_character_
  state$env_installed <- FALSE
  state$committed <- FALSE
  state$ops <- ops
  if (!is.null(state$result_parent_snapshot)) {
    .viewerAuthRevalidateParent(state)
  } else {
    anchor <- .viewerAuthReadDirectoryIdentity(
      state$parent_anchor_path,
      "0700",
      state$ops,
      exact_mode = FALSE,
      require_owner = FALSE
    )
    if (!.viewerAuthSameDirectory(state$parent_anchor_snapshot, anchor)) {
      .viewerAuthUnsafe(state$parent_anchor_path, changed = TRUE)
    }
  }
  state
}

.viewerAuthRandomBytes <- function(ops, size) {
  bytes <- .viewerAuthCall(ops$random_bytes(size))
  if (!is.raw(bytes) || length(bytes) != size) {
    stop(
      "Authentication setup returned an invalid number of random bytes.",
      call. = FALSE
    )
  }
  bytes
}

.viewerAuthGenerateEnvironmentName <- function(
  ops,
  max_attempts = 100L
) {
  if (
    length(max_attempts) != 1L ||
      is.na(max_attempts) ||
      max_attempts < 1L ||
      max_attempts != as.integer(max_attempts)
  ) {
    stop("Authentication environment-name attempts are invalid.", call. = FALSE)
  }
  for (attempt in seq_len(as.integer(max_attempts))) {
    suffix <- .viewerAuthHex(.viewerAuthRandomBytes(ops, 8L), TRUE)
    name <- paste0("CEREBRO_AUTH_PASSPHRASE_", suffix)
    current <- .viewerAuthCall(ops$getenv(name, unset = NA_character_))
    if (is.character(current) && length(current) == 1L && is.na(current)) {
      return(name)
    }
    if (!is.character(current) || length(current) != 1L) {
      stop(
        "Authentication environment-name lookup failed for ",
        name,
        ".",
        call. = FALSE
      )
    }
  }
  stop(
    "Failed to generate an absent authentication environment variable after ",
    max_attempts,
    " attempts.",
    call. = FALSE
  )
}

.viewerAuthCompleteSimple <- function(state) {
  state$accounts <- .viewerAuthCollectAccounts(state$ops)
  if (is.null(state$existing_snapshot)) {
    state$env_name <- .viewerAuthGenerateEnvironmentName(
      state$ops,
      max_attempts = 100L
    )
    state$passphrase <- .viewerAuthHex(
      .viewerAuthRandomBytes(state$ops, 32L),
      uppercase = FALSE
    )
  }
  invisible(state)
}

.viewerAuthCreateStagedDatabase <- function(state, database_path) {
  fail <- function(message) stop(message, call. = FALSE)
  ops <- state$ops
  auth_dir <- dirname(database_path)
  accounts_valid <- is.data.frame(state$accounts) &&
    identical(names(state$accounts), c("user", "password", "admin")) &&
    nrow(state$accounts) > 0L
  if (
    !isTRUE(accounts_valid) ||
      !is.character(state$passphrase) ||
      length(state$passphrase) != 1L ||
      is.na(state$passphrase)
  ) {
    fail("Authentication accounts are not prepared.")
  }
  if (
    !identical(.viewerAuthCall(ops$entry_exists(auth_dir)), FALSE) ||
      !identical(.viewerAuthCall(ops$entry_exists(database_path)), FALSE)
  ) {
    fail("Failed to prepare the staged authentication directory.")
  }
  if (!isTRUE(.viewerAuthCall(ops$claim_dir(auth_dir, "0700")))) {
    fail("Failed to prepare the staged authentication directory.")
  }
  if (!isTRUE(.viewerAuthCall(ops$chmod(auth_dir, "0700", FALSE)))) {
    fail("Failed to harden the staged authentication database.")
  }
  auth_dir_snapshot <- tryCatch(
    .viewerAuthReadDirectoryIdentity(auth_dir, "0700", ops),
    error = function(condition) NULL
  )
  if (is.null(auth_dir_snapshot)) {
    fail("Failed to harden the staged authentication database.")
  }

  created <- .viewerAuthCall(ops$create_db(
    state$accounts,
    database_path,
    state$passphrase
  ))
  if (!isTRUE(created)) {
    fail("Failed to create the staged authentication database.")
  }
  if (!isTRUE(.viewerAuthCall(ops$chmod(database_path, "0600", FALSE)))) {
    fail("Failed to harden the staged authentication database.")
  }
  database_snapshot <- tryCatch(
    .viewerAuthReadFileIdentity(database_path, "0600", ops),
    error = function(condition) NULL
  )
  if (is.null(database_snapshot)) {
    fail("Failed to harden the staged authentication database.")
  }

  validated <- tryCatch(
    {
      .viewerAuthValidateDatabase(
        database_path,
        state$passphrase,
        state$env_name
      )
      TRUE
    },
    error = function(condition) FALSE
  )
  database_after_validation <- tryCatch(
    .viewerAuthReadFileIdentity(database_path, "0600", ops),
    error = function(condition) NULL
  )
  auth_dir_after_validation <- tryCatch(
    .viewerAuthReadDirectoryIdentity(auth_dir, "0700", ops),
    error = function(condition) NULL
  )
  if (
    !isTRUE(validated) ||
      !.viewerAuthSameFileIdentity(
        database_snapshot,
        database_after_validation
      ) ||
      !.viewerAuthSameDirectory(
        auth_dir_snapshot,
        auth_dir_after_validation
      )
  ) {
    fail("Failed to validate the staged authentication database.")
  }

  sidecars <- paste0(database_path, c("-journal", "-wal", "-shm"))
  for (sidecar in sidecars) {
    exists <- .viewerAuthCall(ops$entry_exists(sidecar))
    if (identical(exists, FALSE)) {
      next
    }
    sidecar_snapshot <- tryCatch(
      .viewerAuthReadFileIdentity(
        sidecar,
        "0600",
        ops,
        exact_mode = FALSE
      ),
      error = function(condition) NULL
    )
    if (
      !isTRUE(exists) ||
        is.null(sidecar_snapshot) ||
        !isTRUE(.viewerAuthCall(ops$unlink_file(sidecar)))
    ) {
      fail("Failed to finalize the staged authentication database.")
    }
  }
  if (any(vapply(sidecars, ops$entry_exists, logical(1)))) {
    fail("Failed to finalize the staged authentication database.")
  }

  final_database <- tryCatch(
    .viewerAuthReadFileIdentity(database_path, "0600", ops),
    error = function(condition) NULL
  )
  final_auth_dir <- tryCatch(
    .viewerAuthReadDirectoryIdentity(auth_dir, "0700", ops),
    error = function(condition) NULL
  )
  if (
    !.viewerAuthSameFileIdentity(database_snapshot, final_database) ||
      !.viewerAuthSameDirectory(auth_dir_snapshot, final_auth_dir)
  ) {
    fail("Failed to finalize the staged authentication database.")
  }
  state$accounts <- NULL
  invisible(database_path)
}

.viewerAuthRevalidateParent <- function(state) {
  if (!is.null(state$result_parent_snapshot)) {
    current <- .viewerAuthReadDirectoryIdentity(
      state$result_parent,
      "0700",
      state$ops,
      exact_mode = FALSE,
      require_owner = FALSE
    )
    if (!.viewerAuthSameDirectory(state$result_parent_snapshot, current)) {
      .viewerAuthUnsafe(state$result_parent, changed = TRUE)
    }
    return(invisible(TRUE))
  }
  anchor <- .viewerAuthReadDirectoryIdentity(
    state$parent_anchor_path,
    "0700",
    state$ops,
    exact_mode = FALSE,
    require_owner = FALSE
  )
  if (!.viewerAuthSameDirectory(state$parent_anchor_snapshot, anchor)) {
    .viewerAuthUnsafe(state$parent_anchor_path, changed = TRUE)
  }
  parent_exists <- .viewerAuthCall(state$ops$entry_exists(state$result_parent))
  if (identical(parent_exists, FALSE)) {
    current_target <- tryCatch(
      .stableBundleTarget(state$result_target),
      error = function(condition) NULL
    )
    if (
      is.null(current_target) ||
        !.viewerAuthSamePath(current_target, state$result_target) ||
        !.viewerAuthSamePath(dirname(current_target), state$result_parent)
    ) {
      .viewerAuthUnsafe(state$result_target, changed = TRUE)
    }
    return(invisible(TRUE))
  }
  if (!isTRUE(parent_exists)) {
    .viewerAuthUnsafe(state$result_parent)
  }
  current <- .viewerAuthReadDirectoryIdentity(
    state$result_parent,
    "0700",
    state$ops,
    exact_mode = FALSE,
    require_owner = FALSE
  )
  if (!.viewerAuthSamePath(current$path, state$result_parent)) {
    .viewerAuthUnsafe(state$result_parent)
  }
  state$result_parent_snapshot <- current
  invisible(TRUE)
}

.viewerAuthRemoveScratch <- function(state) {
  .viewerAuthRevalidateParent(state)
  if (
    !is.null(state$scratch_payload) &&
      !is.null(state$scratch_payload_snapshot)
  ) {
    current <- .viewerAuthReadSecretFile(state$scratch_payload, state$ops)
    if (
      !.viewerAuthSamePath(current$path, state$scratch_payload) ||
        !.viewerAuthSameArtifact(current, state$scratch_payload_snapshot)
    ) {
      stop(
        "Authentication secret scratch payload identity changed at '",
        state$scratch_payload,
        "'.",
        call. = FALSE
      )
    }
    removed <- .viewerAuthCall(state$ops$unlink_file(state$scratch_payload))
    if (
      !isTRUE(removed) ||
        isTRUE(.viewerAuthCall(state$ops$entry_exists(state$scratch_payload)))
    ) {
      stop(
        "Failed to remove authentication secret scratch payload at '",
        state$scratch_payload,
        "'.",
        call. = FALSE
      )
    }
    state$scratch_payload <- NULL
    state$scratch_payload_snapshot <- NULL
  }
  current_dir <- .viewerAuthReadDirectoryIdentity(
    state$scratch_dir,
    "0700",
    state$ops
  )
  if (!.viewerAuthSameDirectory(state$scratch_snapshot, current_dir)) {
    stop(
      "Authentication secret scratch directory identity changed at '",
      state$scratch_dir,
      "'.",
      call. = FALSE
    )
  }
  entries <- .viewerAuthCall(state$ops$list_dir(state$scratch_dir))
  if (!is.character(entries) || length(entries) != 0L) {
    stop(
      "Authentication secret scratch directory is not empty at '",
      state$scratch_dir,
      "'.",
      call. = FALSE
    )
  }
  removed <- .viewerAuthCall(state$ops$remove_dir(state$scratch_dir))
  if (
    !isTRUE(removed) ||
      isTRUE(.viewerAuthCall(state$ops$entry_exists(state$scratch_dir)))
  ) {
    stop(
      "Failed to remove authentication secret scratch directory at '",
      state$scratch_dir,
      "'.",
      call. = FALSE
    )
  }
  state$scratch_dir <- NULL
  state$scratch_snapshot <- NULL
  invisible(TRUE)
}

.viewerAuthCreateSecretCandidate <- function(state) {
  if (!is.null(state$existing_snapshot)) {
    return(invisible(state$secret_path))
  }
  if (
    !is.character(state$env_name) ||
      length(state$env_name) != 1L ||
      !is.character(state$passphrase) ||
      length(state$passphrase) != 1L
  ) {
    stop("Authentication secret values are not prepared.", call. = FALSE)
  }
  .viewerAuthRevalidateParent(state)
  scratch <- tempfile(
    pattern = paste0(".", basename(state$secret_path), "-scratch-"),
    tmpdir = state$result_parent
  )
  claimed <- .viewerAuthCall(state$ops$claim_dir(scratch, mode = "0700"))
  if (!isTRUE(claimed)) {
    stop(
      "Failed to claim authentication secret scratch space at '",
      scratch,
      "'.",
      call. = FALSE
    )
  }
  state$scratch_dir <- scratch
  chmod_ok <- if (identical(.Platform$OS.type, "windows")) {
    TRUE
  } else {
    .viewerAuthCall(state$ops$chmod(
      scratch,
      as.octmode("0700"),
      use_umask = FALSE
    ))
  }
  if (!isTRUE(chmod_ok)) {
    stop(
      "Failed to secure authentication secret scratch space at '",
      scratch,
      "'.",
      call. = FALSE
    )
  }
  state$scratch_snapshot <- .viewerAuthReadDirectoryIdentity(
    scratch,
    "0700",
    state$ops
  )

  payload <- file.path(scratch, "payload")
  created <- .viewerAuthCall(state$ops$create_private_file(payload))
  state$scratch_payload <- payload
  if (!isTRUE(created)) {
    stop(
      "Failed to create authentication secret scratch payload at '",
      payload,
      "'.",
      call. = FALSE
    )
  }
  chmod_ok <- if (identical(.Platform$OS.type, "windows")) {
    TRUE
  } else {
    .viewerAuthCall(state$ops$chmod(
      payload,
      as.octmode("0600"),
      use_umask = FALSE
    ))
  }
  if (!isTRUE(chmod_ok)) {
    stop(
      "Failed to secure authentication secret scratch payload at '",
      payload,
      "'.",
      call. = FALSE
    )
  }
  state$scratch_payload_snapshot <- .viewerAuthReadFileIdentity(
    payload,
    "0600",
    state$ops
  )
  bytes <- charToRaw(paste0(state$env_name, "=", state$passphrase, "\n"))
  written <- .viewerAuthCall(state$ops$write_raw(bytes, payload))
  if (!isTRUE(written)) {
    stop(
      "Failed to write authentication secret scratch payload at '",
      payload,
      "'.",
      call. = FALSE
    )
  }
  state$scratch_payload_snapshot <- .viewerAuthReadSecretFile(
    payload,
    state$ops
  )

  .viewerAuthRevalidateParent(state)
  candidate <- tempfile(
    pattern = paste0(".", basename(state$secret_path), "-candidate-"),
    tmpdir = state$result_parent
  )
  linked <- .viewerAuthCall(state$ops$link(payload, candidate))
  if (!isTRUE(linked)) {
    stop(
      "Failed to publish authentication secret candidate at '",
      candidate,
      "'.",
      call. = FALSE
    )
  }
  state$candidate_path <- candidate
  state$candidate_snapshot <- state$scratch_payload_snapshot
  state$candidate_snapshot$path <- candidate
  current <- .viewerAuthReadSecretFile(candidate, state$ops)
  if (
    !.viewerAuthSamePath(current$path, candidate) ||
      !.viewerAuthSameArtifact(state$candidate_snapshot, current)
  ) {
    stop(
      "Authentication secret candidate identity verification failed at '",
      candidate,
      "'.",
      call. = FALSE
    )
  }
  state$candidate_snapshot <- current
  .viewerAuthRemoveScratch(state)
  invisible(candidate)
}

.viewerAuthRevalidateInitialSecret <- function(state) {
  .viewerAuthRevalidateParent(state)
  if (is.null(state$existing_snapshot)) {
    if (isTRUE(.viewerAuthCall(state$ops$entry_exists(state$secret_path)))) {
      stop(
        "The authentication secret target changed during setup at '",
        state$secret_path,
        "'.",
        call. = FALSE
      )
    }
  } else {
    .viewerAuthRevalidateSecretFile(state$existing_snapshot, state$ops)
  }
  invisible(TRUE)
}

.viewerAuthCleanupWarning <- function(path) {
  message_text <- paste0(
    "Could not safely remove authentication setup artifact at '",
    path,
    "'."
  )
  tryCatch(
    warning(message_text, call. = FALSE, immediate. = TRUE),
    error = function(condition) message("Warning: ", message_text)
  )
  invisible(NULL)
}

.viewerAuthCleanupParentValid <- function(state) {
  tryCatch(
    {
      .viewerAuthRevalidateParent(state)
      TRUE
    },
    error = function(condition) FALSE
  )
}

.viewerAuthCleanupFile <- function(
  state,
  path_field,
  snapshot_field,
  clear_path = TRUE
) {
  path <- state[[path_field]]
  snapshot <- state[[snapshot_field]]
  if (is.null(path) || is.null(snapshot)) {
    return(invisible(is.null(path)))
  }
  if (!.viewerAuthCleanupParentValid(state)) {
    .viewerAuthCleanupWarning(path)
    return(invisible(FALSE))
  }
  exists_now <- .viewerAuthCall(state$ops$entry_exists(path))
  if (identical(exists_now, FALSE)) {
    if (isTRUE(clear_path)) {
      state[[path_field]] <- NULL
    }
    state[[snapshot_field]] <- NULL
    return(invisible(TRUE))
  }
  complete_snapshot <- is.raw(snapshot$raw)
  current <- tryCatch(
    if (complete_snapshot) {
      .viewerAuthReadSecretFile(path, state$ops)
    } else {
      .viewerAuthReadFileIdentity(path, "0600", state$ops)
    },
    error = function(condition) NULL
  )
  same_owned_entry <- if (complete_snapshot) {
    .viewerAuthSameArtifact(snapshot, current) &&
      .viewerAuthSamePath(current$path, path)
  } else {
    .viewerAuthSameOwnedFile(snapshot, current)
  }
  if (is.null(current) || !isTRUE(same_owned_entry)) {
    .viewerAuthCleanupWarning(path)
    return(invisible(FALSE))
  }
  removed <- .viewerAuthCall(state$ops$unlink_file(path))
  if (
    !isTRUE(removed) ||
      isTRUE(.viewerAuthCall(state$ops$entry_exists(path)))
  ) {
    .viewerAuthCleanupWarning(path)
    return(invisible(FALSE))
  }
  if (isTRUE(clear_path)) {
    state[[path_field]] <- NULL
  }
  state[[snapshot_field]] <- NULL
  invisible(TRUE)
}

.viewerAuthCleanupScratch <- function(state) {
  if (is.null(state$scratch_dir)) {
    return(invisible(TRUE))
  }
  if (!.viewerAuthCleanupParentValid(state)) {
    .viewerAuthCleanupWarning(state$scratch_dir)
    return(invisible(FALSE))
  }
  if (
    !is.null(state$scratch_payload) &&
      !is.null(state$scratch_payload_snapshot)
  ) {
    .viewerAuthCleanupFile(
      state,
      "scratch_payload",
      "scratch_payload_snapshot"
    )
  } else if (
    !is.null(state$scratch_payload) &&
      isTRUE(.viewerAuthCall(state$ops$entry_exists(state$scratch_payload)))
  ) {
    .viewerAuthCleanupWarning(state$scratch_payload)
  }
  current <- tryCatch(
    .viewerAuthReadDirectoryIdentity(
      state$scratch_dir,
      "0700",
      state$ops
    ),
    error = function(condition) NULL
  )
  if (
    is.null(current) ||
      is.null(state$scratch_snapshot) ||
      !.viewerAuthSameDirectory(state$scratch_snapshot, current)
  ) {
    .viewerAuthCleanupWarning(state$scratch_dir)
    return(invisible(FALSE))
  }
  entries <- .viewerAuthCall(state$ops$list_dir(state$scratch_dir))
  if (!is.character(entries) || length(entries) != 0L) {
    .viewerAuthCleanupWarning(state$scratch_dir)
    return(invisible(FALSE))
  }
  removed <- .viewerAuthCall(state$ops$remove_dir(state$scratch_dir))
  if (
    !isTRUE(removed) ||
      isTRUE(.viewerAuthCall(state$ops$entry_exists(state$scratch_dir)))
  ) {
    .viewerAuthCleanupWarning(state$scratch_dir)
    return(invisible(FALSE))
  }
  state$scratch_dir <- NULL
  state$scratch_snapshot <- NULL
  invisible(TRUE)
}

.viewerAuthRollbackEnvironment <- function(state) {
  if (!isTRUE(state$env_installed)) {
    return(invisible(TRUE))
  }
  restored <- if (
    is.character(state$prior_env) &&
      length(state$prior_env) == 1L &&
      is.na(state$prior_env)
  ) {
    .viewerAuthCall(state$ops$unsetenv(state$env_name))
  } else if (is.character(state$prior_env) && length(state$prior_env) == 1L) {
    .viewerAuthCall(state$ops$setenv(state$env_name, state$prior_env))
  } else {
    NULL
  }
  current <- .viewerAuthCall(state$ops$getenv(
    state$env_name,
    unset = NA_character_
  ))
  if (!isTRUE(restored) || !identical(current, state$prior_env)) {
    .viewerAuthCleanupWarning(state$env_name)
    return(invisible(FALSE))
  }
  state$env_installed <- FALSE
  invisible(TRUE)
}

.viewerAuthRollbackSimple <- function(state) {
  tryCatch(
    .viewerAuthRollbackEnvironment(state),
    error = function(condition) .viewerAuthCleanupWarning(state$env_name)
  )
  .viewerAuthCleanupFile(
    state,
    "secret_path",
    "published_snapshot",
    clear_path = FALSE
  )
  .viewerAuthCleanupFile(state, "candidate_path", "candidate_snapshot")
  .viewerAuthCleanupScratch(state)
  invisible(NULL)
}

.viewerAuthPublishSecret <- function(state) {
  .viewerAuthRevalidateParent(state)
  if (!is.null(state$existing_snapshot)) {
    .viewerAuthRevalidateSecretFile(state$existing_snapshot, state$ops)
    return(invisible(state$secret_path))
  }
  if (is.null(state$candidate_path) || is.null(state$candidate_snapshot)) {
    stop("The authentication secret candidate is not prepared.", call. = FALSE)
  }
  .viewerAuthRevalidateInitialSecret(state)
  .viewerAuthRevalidateSecretFile(state$candidate_snapshot, state$ops)
  linked <- .viewerAuthCall(state$ops$link(
    state$candidate_path,
    state$secret_path
  ))
  if (!isTRUE(linked)) {
    stop(
      "Failed to publish authentication secret without clobbering target '",
      state$secret_path,
      "'.",
      call. = FALSE
    )
  }
  state$published_snapshot <- state$candidate_snapshot
  state$published_snapshot$path <- state$secret_path
  current <- tryCatch(
    .viewerAuthReadSecretFile(state$secret_path, state$ops),
    error = function(condition) {
      .viewerAuthRollbackSimple(state)
      stop(condition)
    }
  )
  if (
    !.viewerAuthSamePath(current$path, state$secret_path) ||
      !.viewerAuthSameArtifact(state$published_snapshot, current)
  ) {
    .viewerAuthRollbackSimple(state)
    stop(
      "Published authentication secret identity verification failed at '",
      state$secret_path,
      "'.",
      call. = FALSE
    )
  }
  state$published_snapshot <- current
  removed <- .viewerAuthCleanupFile(
    state,
    "candidate_path",
    "candidate_snapshot"
  )
  if (!isTRUE(removed)) {
    .viewerAuthRollbackSimple(state)
    stop(
      "Failed to remove the authentication secret candidate.",
      call. = FALSE
    )
  }
  invisible(state$secret_path)
}

.viewerAuthRevalidatePublishedSecret <- function(state) {
  .viewerAuthRevalidateParent(state)
  snapshot <- if (is.null(state$existing_snapshot)) {
    state$published_snapshot
  } else {
    state$existing_snapshot
  }
  .viewerAuthRevalidateSecretFile(snapshot, state$ops)
  invisible(TRUE)
}

.viewerAuthInstallEnvironment <- function(state) {
  if (
    !is.character(state$env_name) ||
      length(state$env_name) != 1L ||
      is.na(state$env_name) ||
      !is.character(state$passphrase) ||
      length(state$passphrase) != 1L ||
      is.na(state$passphrase)
  ) {
    stop(
      "The authentication process environment is not prepared.",
      call. = FALSE
    )
  }
  prior <- .viewerAuthCall(state$ops$getenv(
    state$env_name,
    unset = NA_character_
  ))
  if (!is.character(prior) || length(prior) != 1L) {
    stop(
      "Failed to inspect authentication environment variable ",
      state$env_name,
      ".",
      call. = FALSE
    )
  }
  state$prior_env <- prior
  state$env_installed <- TRUE
  result <- .viewerAuthCall(state$ops$setenv(
    state$env_name,
    state$passphrase
  ))
  current <- .viewerAuthCall(state$ops$getenv(
    state$env_name,
    unset = NA_character_
  ))
  if (!isTRUE(result) || !identical(current, state$passphrase)) {
    stop(
      "Failed to install authentication environment variable ",
      state$env_name,
      ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.viewerAuthCommitSimple <- function(state) {
  state$committed <- TRUE
  invisible(NULL)
}

.viewerAuthClearSimple <- function(state) {
  state$accounts <- NULL
  state$passphrase <- NULL
  state$existing_snapshot <- NULL
  state$scratch_snapshot <- NULL
  state$scratch_payload_snapshot <- NULL
  state$candidate_snapshot <- NULL
  state$published_snapshot <- NULL
  state$prior_env <- NULL
  invisible(NULL)
}

.viewerAuthFinishSimple <- function(state) {
  on.exit(.viewerAuthClearSimple(state), add = TRUE)
  if (!isTRUE(state$committed)) {
    .viewerAuthRollbackSimple(state)
  }
  invisible(NULL)
}
