.viewerAuthUnsafeParent <- function(
  message = "target_dir has an unsafe parent."
) {
  .viewerAuthProvisionAbort("unsafe_parent", "preflight", message)
}

.viewerAuthProvisionPreflight <- function(state) {
  target <- state$options$target_dir
  os_type <- tryCatch(state$ops$os_type(), error = function(e) NA_character_)
  style <- tryCatch(
    .absolutePathStyle(target, os_type = os_type),
    error = function(e) NA_character_
  )
  base <- tryCatch(basename(target), error = function(e) NA_character_)
  base_utf8 <- tryCatch(
    iconv(base, from = "", to = "UTF-8", sub = NA_character_),
    error = function(e) NA_character_
  )
  if (
    !identical(os_type, "unix") ||
      !identical(style, "posix") ||
      !.viewerAuthProvisionScalarString(base_utf8) ||
      base_utf8 %in% c(".", "..") ||
      nchar(base_utf8, type = "bytes") > 128L ||
      .viewerAuthProvisionHasControl(base) ||
      grepl("^\\.cerebro-auth-", base)
  ) {
    .viewerAuthUnsafeParent()
  }
  parent <- tryCatch(
    state$ops$normalize_existing(dirname(target)),
    error = function(e) NA_character_
  )
  if (
    !.viewerAuthProvisionScalarString(parent) ||
      !isTRUE(tryCatch(state$ops$components_safe(parent), error = function(e) {
        FALSE
      }))
  ) {
    .viewerAuthUnsafeParent()
  }
  info <- tryCatch(state$ops$inspect_path(parent), error = function(e) NULL)
  uid <- tryCatch(state$ops$effective_uid(), error = function(e) NA_character_)
  private <- is.list(info) &&
    .viewerAuthProvisionScalarString(uid) &&
    grepl("^[0-9]+$", uid) &&
    identical(info$uid, uid) &&
    identical(info$permissions, "rwx------")
  if (
    !is.list(info) ||
      !isTRUE(info$exists) ||
      !identical(info$type, "directory") ||
      isTRUE(info$is_link) ||
      !private
  ) {
    .viewerAuthUnsafeParent()
  }
  canonical <- file.path(parent, base)
  target_info <- tryCatch(
    state$ops$inspect_path(canonical),
    error = function(e) NULL
  )
  if (!is.list(target_info)) {
    .viewerAuthUnsafeParent()
  }
  if (isTRUE(target_info$is_link)) {
    .viewerAuthUnsafeParent()
  }
  if (isTRUE(target_info$exists)) {
    .viewerAuthProvisionAbort(
      "target_exists",
      "preflight",
      "target_dir already exists."
    )
  }
  state$preflight <- list(
    parent = parent,
    target_path = canonical,
    parent_identity = info[c("device_id", "inode")]
  )
  invisible(state)
}

.viewerAuthParentStillFrozen <- function(state) {
  info <- tryCatch(
    state$ops$inspect_path(state$preflight$parent),
    error = function(e) NULL
  )
  uid <- tryCatch(state$ops$effective_uid(), error = function(e) NA_character_)
  safe <- tryCatch(
    isTRUE(state$ops$components_safe(state$preflight$parent)),
    error = function(e) FALSE
  )
  is.list(info) &&
    safe &&
    .viewerAuthProvisionScalarString(uid) &&
    grepl("^[0-9]+$", uid) &&
    isTRUE(info$exists) &&
    identical(info$type, "directory") &&
    !isTRUE(info$is_link) &&
    identical(info$uid, uid) &&
    identical(info$permissions, "rwx------") &&
    identical(info[c("device_id", "inode")], state$preflight$parent_identity)
}

.viewerAuthPrepareProvisionPaths <- function(state) {
  id <- state$identity$operation_id
  hash <- state$identity$target_hash
  parent <- state$preflight$parent
  lock <- file.path(parent, paste0(".cerebro-auth-", hash, ".lock"))
  stage_basename <- paste0(".cerebro-auth-", hash, "-", id, ".stage")
  stage <- file.path(parent, stage_basename)
  state$paths <- list(
    target = state$preflight$target_path,
    lock = lock,
    owner = file.path(lock, "owner.rds"),
    owner_tmp = file.path(lock, paste0("owner.rds.", id, ".tmp")),
    release_receipt = file.path(
      parent,
      paste0(".cerebro-auth-", hash, "-", id, ".released.rds")
    ),
    release_receipt_tmp = file.path(
      parent,
      paste0(".cerebro-auth-", hash, "-", id, ".released.rds.tmp")
    ),
    stage = stage,
    stage_basename = stage_basename,
    manifest = file.path(stage, "provision.rds"),
    manifest_tmp = file.path(stage, paste0("provision.rds.", id, ".tmp")),
    credentials = file.path(stage, "credentials.sqlite"),
    secret = file.path(stage, "viewer-auth.env"),
    secret_tmp = file.path(stage, paste0("viewer-auth.env.", id, ".tmp"))
  )
  invisible(state)
}

.viewerAuthFreezeProvisionPath <- function(
  state,
  key,
  path,
  type,
  permissions = NULL,
  code = "artifact_publish_failed",
  stage = "artifact_stage"
) {
  info <- tryCatch(state$ops$inspect_path(path), error = function(e) NULL)
  if (
    !is.list(info) ||
      !isTRUE(info$exists) ||
      !identical(info$type, type) ||
      isTRUE(info$is_link) ||
      (!is.null(permissions) && !identical(info$permissions, permissions))
  ) {
    .viewerAuthProvisionAbort(
      code,
      stage,
      "A private provisioning path failed identity validation."
    )
  }
  identity <- info[c("type", "device_id", "inode")]
  frozen <- state$identities[[key]]
  if (!is.null(frozen) && !identical(frozen, identity)) {
    .viewerAuthProvisionAbort(
      code,
      stage,
      "A private provisioning path changed identity."
    )
  }
  state$identities[[key]] <- identity
  invisible(info)
}

.viewerAuthIdentityMatches <- function(state, key, path) {
  expected <- state$identities[[key]]
  if (is.null(expected)) {
    return(FALSE)
  }
  actual <- state$ops$inspect_path(path)
  isTRUE(actual$exists) &&
    !isTRUE(actual$is_link) &&
    identical(actual[c("type", "device_id", "inode")], expected)
}

# A successful rename is not proof that the destination still names our
# generation: an attacker may replace it before the next inspection.  Bind a
# destination identity only after proving it is the frozen source and that the
# source name has disappeared.
.viewerAuthControlledRenameRebind <- function(
  state,
  source_key,
  source_path,
  final_key,
  final_path,
  final_type,
  permissions,
  code,
  stage
) {
  source_identity <- state$identities[[source_key]]
  source_ok <- !is.null(source_identity) &&
    isTRUE(tryCatch(
      .viewerAuthIdentityMatches(state, source_key, source_path),
      error = function(e) FALSE
    ))
  final_before <- tryCatch(
    state$ops$inspect_path(final_path),
    error = function(e) NULL
  )
  if (
    !source_ok ||
      !is.list(final_before) ||
      isTRUE(final_before$exists) ||
      !is.null(state$identities[[final_key]])
  ) {
    .viewerAuthProvisionAbort(
      code,
      stage,
      "Private artifact changed before publish."
    )
  }
  renamed <- tryCatch(
    isTRUE(state$ops$rename(source_path, final_path)),
    error = function(e) FALSE
  )
  final_after <- tryCatch(
    state$ops$inspect_path(final_path),
    error = function(e) NULL
  )
  source_after <- tryCatch(
    state$ops$inspect_path(source_path),
    error = function(e) NULL
  )
  committed <- renamed &&
    is.list(final_after) &&
    isTRUE(final_after$exists) &&
    identical(final_after$type, final_type) &&
    !isTRUE(final_after$is_link) &&
    (is.null(permissions) || identical(final_after$permissions, permissions)) &&
    identical(final_after[c("type", "device_id", "inode")], source_identity) &&
    is.list(source_after) &&
    !isTRUE(source_after$exists)
  if (!committed) {
    # The destination was not proved to be our frozen generation; never leave
    # an identity claim that could authorize later cleanup of it.
    state$identities[[final_key]] <- NULL
    .viewerAuthProvisionAbort(
      code,
      stage,
      "Could not publish the private artifact."
    )
  }
  state$identities[[final_key]] <- source_identity
  state$identities[[source_key]] <- NULL
  invisible(state)
}

.viewerAuthAtomicSaveRds <- function(
  state,
  value,
  final_path,
  temp_path,
  identity_key,
  validator,
  code,
  stage
) {
  temp_key <- paste0(identity_key, "_tmp")
  before_temp <- tryCatch(
    state$ops$inspect_path(temp_path),
    error = function(e) NULL
  )
  if (!is.list(before_temp) || isTRUE(before_temp$exists)) {
    .viewerAuthProvisionAbort(code, stage, "Metadata temp path is not empty.")
  }
  state$identities[[temp_key]] <- NULL
  before_final <- tryCatch(
    state$ops$inspect_path(final_path),
    error = function(e) NULL
  )
  if (!is.list(before_final)) {
    .viewerAuthProvisionAbort(
      code,
      stage,
      "Could not inspect private metadata."
    )
  }
  existed <- isTRUE(before_final$exists)
  if (
    existed &&
      (!tryCatch(
        .viewerAuthIdentityMatches(state, identity_key, final_path),
        error = function(e) FALSE
      ) ||
        !identical(before_final$type, "file") ||
        isTRUE(before_final$is_link) ||
        !identical(before_final$permissions, "rw-------"))
  ) {
    .viewerAuthProvisionAbort(code, stage, "Private metadata changed identity.")
  }
  if (!existed && !is.null(state$identities[[identity_key]])) {
    .viewerAuthProvisionAbort(code, stage, "Private metadata disappeared.")
  }
  write_ok <- tryCatch(
    isTRUE(state$ops$save_rds(value, temp_path)),
    error = function(e) FALSE
  )
  after_write <- tryCatch(
    state$ops$inspect_path(temp_path),
    error = function(e) NULL
  )
  if (is.list(after_write) && isTRUE(after_write$exists)) {
    .viewerAuthFreezeProvisionPath(
      state,
      temp_key,
      temp_path,
      "file",
      code = code,
      stage = stage
    )
  }
  if (!write_ok || is.null(state$identities[[temp_key]])) {
    .viewerAuthProvisionAbort(code, stage, "Could not write private metadata.")
  }
  if (
    !tryCatch(isTRUE(state$ops$chmod(temp_path, "0600")), error = function(e) {
      FALSE
    })
  ) {
    .viewerAuthProvisionAbort(
      code,
      stage,
      "Could not protect private metadata."
    )
  }
  .viewerAuthFreezeProvisionPath(
    state,
    temp_key,
    temp_path,
    "file",
    "rw-------",
    code,
    stage
  )
  pre_rename <- tryCatch(
    state$ops$inspect_path(final_path),
    error = function(e) NULL
  )
  expected <- if (existed) {
    is.list(pre_rename) &&
      isTRUE(pre_rename$exists) &&
      identical(pre_rename$type, "file") &&
      !isTRUE(pre_rename$is_link) &&
      identical(pre_rename$permissions, "rw-------") &&
      tryCatch(
        .viewerAuthIdentityMatches(state, identity_key, final_path),
        error = function(e) FALSE
      )
  } else {
    is.list(pre_rename) && !isTRUE(pre_rename$exists)
  }
  if (!expected) {
    .viewerAuthProvisionAbort(
      code,
      stage,
      "Private metadata changed before publish."
    )
  }
  tryCatch(
    isTRUE(state$ops$rename(temp_path, final_path)),
    error = function(e) FALSE
  )
  after_final <- tryCatch(
    state$ops$inspect_path(final_path),
    error = function(e) NULL
  )
  after_temp <- tryCatch(
    state$ops$inspect_path(temp_path),
    error = function(e) NULL
  )
  temp_identity <- state$identities[[temp_key]]
  committed <- is.list(after_final) &&
    isTRUE(after_final$exists) &&
    identical(after_final$type, "file") &&
    !isTRUE(after_final$is_link) &&
    identical(after_final$permissions, "rw-------") &&
    identical(after_final[c("type", "device_id", "inode")], temp_identity) &&
    is.list(after_temp) &&
    !isTRUE(after_temp$exists)
  if (!committed) {
    .viewerAuthProvisionAbort(
      code,
      stage,
      "Could not publish private metadata."
    )
  }
  state$identities[[identity_key]] <- temp_identity
  state$identities[[temp_key]] <- NULL
  reread <- tryCatch(state$ops$read_rds(final_path), error = function(e) NULL)
  if (!isTRUE(validator(reread))) {
    .viewerAuthProvisionAbort(
      code,
      stage,
      "Private metadata failed validation."
    )
  }
  invisible(reread)
}

.viewerAuthWriteOwnerState <- function(state, owner_state) {
  owner <- .viewerAuthOwnerManifest(
    state$identity$operation_id,
    state$paths$target,
    state$paths$stage_basename,
    state$ops$now(),
    owner_state
  )
  .viewerAuthAtomicSaveRds(
    state,
    owner,
    state$paths$owner,
    state$paths$owner_tmp,
    "owner",
    .viewerAuthValidOwnerManifest,
    "artifact_publish_failed",
    "lock"
  )
  state$owner_state <- owner_state
  invisible(state)
}

.viewerAuthAcquireProvisionLock <- function(state) {
  if (!.viewerAuthParentStillFrozen(state)) {
    .viewerAuthUnsafeParent()
  }
  if (
    !tryCatch(
      isTRUE(state$ops$dir_create(state$paths$lock, "0700")),
      error = function(e) FALSE
    )
  ) {
    .viewerAuthProvisionAbort(
      "lock_conflict",
      "lock",
      "Authentication provisioning is already locked."
    )
  }
  state$lock_claimed <- TRUE
  .viewerAuthFreezeProvisionPath(
    state,
    "lock",
    state$paths$lock,
    "directory",
    "rwx------"
  )
  .viewerAuthWriteOwnerState(state, "claimed")
  invisible(state)
}

.viewerAuthCreateProvisionStage <- function(state) {
  if (!.viewerAuthParentStillFrozen(state)) {
    .viewerAuthUnsafeParent()
  }
  .viewerAuthWriteOwnerState(state, "staging")
  if (
    !tryCatch(
      isTRUE(state$ops$dir_create(state$paths$stage, "0700")),
      error = function(e) FALSE
    )
  ) {
    .viewerAuthProvisionAbort(
      "artifact_publish_failed",
      "artifact_stage",
      "Could not create a private authentication stage."
    )
  }
  state$stage_created <- TRUE
  .viewerAuthFreezeProvisionPath(
    state,
    "stage",
    state$paths$stage,
    "directory",
    "rwx------"
  )
  manifest <- .viewerAuthProvisionManifest(
    state$identity$operation_id,
    "staging",
    state$ops$now(),
    state$identity$passphrase_env,
    state$options$timeout_minutes,
    nrow(state$accounts)
  )
  .viewerAuthAtomicSaveRds(
    state,
    manifest,
    state$paths$manifest,
    state$paths$manifest_tmp,
    "manifest",
    .viewerAuthValidProvisionManifest,
    "artifact_publish_failed",
    "manifest"
  )
  state$manifest_state <- "staging"
  invisible(state)
}

.viewerAuthOwnedArtifactNames <- function(state) {
  c(
    "credentials.sqlite",
    "credentials.sqlite-journal",
    "credentials.sqlite-wal",
    "credentials.sqlite-shm",
    "viewer-auth.env",
    "provision.rds",
    basename(state$paths$manifest_tmp),
    basename(state$paths$secret_tmp)
  )
}
.viewerAuthSafeInspect <- function(state, path) {
  tryCatch(state$ops$inspect_path(path), error = function(e) NULL)
}

.viewerAuthValidChildNames <- function(children) {
  is.character(children) &&
    !anyNA(children) &&
    all(vapply(
      children,
      function(name) {
        .viewerAuthProvisionScalarString(name) &&
          !identical(name, ".") &&
          !identical(name, "..") &&
          identical(basename(name), name) &&
          !.viewerAuthProvisionHasControl(name)
      },
      logical(1)
    ))
}

# Task 4 must freeze each database and secret artifact under these keys before
# it can become eligible for cleanup.  A permitted basename alone is never
# proof that this operation owns a file.
.viewerAuthArtifactIdentityKey <- function(state, name) {
  keys <- c(
    "credentials.sqlite" = "credentials",
    "credentials.sqlite-journal" = "credentials_journal",
    "credentials.sqlite-wal" = "credentials_wal",
    "credentials.sqlite-shm" = "credentials_shm",
    "viewer-auth.env" = "secret",
    "provision.rds" = "manifest"
  )
  if (name %in% names(keys)) {
    return(unname(keys[[name]]))
  }
  if (identical(name, basename(state$paths$manifest_tmp))) {
    return("manifest_tmp")
  }
  if (identical(name, basename(state$paths$secret_tmp))) {
    return("secret_tmp")
  }
  NULL
}

.viewerAuthCleanupDirectory <- function(state, path, identity_key) {
  info <- .viewerAuthSafeInspect(state, path)
  if (!is.list(info)) {
    return(FALSE)
  }
  if (!isTRUE(info$exists)) {
    return(TRUE)
  }
  if (!.viewerAuthIdentityMatches(state, identity_key, path)) {
    return(FALSE)
  }
  children <- tryCatch(state$ops$list_files(path), error = function(e) NULL)
  allowed <- .viewerAuthOwnedArtifactNames(state)
  if (!.viewerAuthValidChildNames(children) || any(!children %in% allowed)) {
    return(FALSE)
  }
  for (name in intersect(children, allowed)) {
    child <- file.path(path, name)
    identity_key <- .viewerAuthArtifactIdentityKey(state, name)
    child_info <- .viewerAuthSafeInspect(state, child)
    if (
      is.null(identity_key) ||
        !is.list(child_info) ||
        !isTRUE(child_info$exists) ||
        !identical(child_info$type, "file") ||
        isTRUE(child_info$is_link) ||
        !isTRUE(tryCatch(
          .viewerAuthIdentityMatches(state, identity_key, child),
          error = function(e) FALSE
        )) ||
        !isTRUE(tryCatch(state$ops$remove_file(child), error = function(e) {
          FALSE
        }))
    ) {
      return(FALSE)
    }
  }
  isTRUE(tryCatch(state$ops$remove_dir(path), error = function(e) FALSE))
}

.viewerAuthOwnerProofPath <- function(state) {
  candidates <- c(state$paths$owner, state$paths$release_receipt)
  valid <- vapply(
    candidates,
    function(path) {
      info <- .viewerAuthSafeInspect(state, path)
      is.list(info) &&
        isTRUE(info$exists) &&
        identical(info$type, "file") &&
        !isTRUE(info$is_link) &&
        identical(info$permissions, "rw-------")
    },
    logical(1)
  )
  if (sum(valid) != 1L) {
    return(NULL)
  }
  candidates[[which(valid)]]
}

.viewerAuthOwnerMatchesState <- function(state) {
  path <- .viewerAuthOwnerProofPath(state)
  if (is.null(path)) {
    return(FALSE)
  }
  key <- if (identical(path, state$paths$owner)) "owner" else "release_receipt"
  tryCatch(
    {
      owner <- state$ops$read_rds(path)
      .viewerAuthIdentityMatches(state, key, path) &&
        .viewerAuthValidOwnerManifest(owner) &&
        identical(owner$operation_id, state$identity$operation_id) &&
        identical(owner$target_path, state$paths$target) &&
        identical(owner$stage_basename, state$paths$stage_basename)
    },
    error = function(e) FALSE
  )
}

.viewerAuthRecoveryPath <- function(state) {
  if (.viewerAuthProvisionScalarString(state$recovery_path)) {
    state$recovery_path
  } else {
    state$paths$lock
  }
}

.viewerAuthLayoutMatchesOwnerState <- function(state) {
  lock_info <- .viewerAuthSafeInspect(state, state$paths$lock)
  proof <- .viewerAuthOwnerProofPath(state)
  stage_info <- .viewerAuthSafeInspect(state, state$paths$stage)
  target_info <- .viewerAuthSafeInspect(state, state$paths$target)
  if (!is.list(lock_info) || !is.list(stage_info) || !is.list(target_info)) {
    return(FALSE)
  }
  lock_exists <- isTRUE(lock_info$exists)
  stage_exists <- isTRUE(stage_info$exists)
  target_exists <- isTRUE(target_info$exists)
  if (is.null(proof)) {
    children <- if (lock_exists) {
      tryCatch(state$ops$list_files(state$paths$lock), error = function(e) NULL)
    } else {
      NULL
    }
    owned_temp <- .viewerAuthValidChildNames(children) &&
      (length(children) == 0L ||
        (identical(children, basename(state$paths$owner_tmp)) &&
          .viewerAuthIdentityMatches(
            state,
            "owner_tmp",
            state$paths$owner_tmp
          )))
    return(
      lock_exists &&
        !is.null(children) &&
        is.null(state$owner_state) &&
        !stage_exists &&
        !target_exists &&
        owned_temp
    )
  }
  if (!.viewerAuthOwnerMatchesState(state)) {
    return(FALSE)
  }
  if (lock_exists) {
    children <- tryCatch(
      state$ops$list_files(state$paths$lock),
      error = function(e) NULL
    )
    allowed <- if (identical(proof, state$paths$owner)) {
      c("owner.rds", basename(state$paths$owner_tmp))
    } else {
      basename(state$paths$owner_tmp)
    }
    if (!.viewerAuthValidChildNames(children) || any(!children %in% allowed)) {
      return(FALSE)
    }
  } else if (!identical(proof, state$paths$release_receipt)) {
    return(FALSE)
  }
  if (stage_exists && target_exists) {
    return(FALSE)
  }
  owner <- tryCatch(state$ops$read_rds(proof), error = function(e) NULL)
  if (!.viewerAuthValidOwnerManifest(owner)) {
    return(FALSE)
  }
  location <- if (stage_exists) state$paths$stage else state$paths$target
  manifest <- if (stage_exists || target_exists) {
    suppressWarnings(tryCatch(
      state$ops$read_rds(file.path(location, "provision.rds")),
      error = function(e) NULL
    ))
  } else {
    NULL
  }
  matches <- .viewerAuthValidProvisionManifest(manifest) &&
    identical(manifest$operation_id, state$identity$operation_id)
  missing_safe <- FALSE
  if (stage_exists && is.null(manifest)) {
    children <- tryCatch(
      state$ops$list_files(state$paths$stage),
      error = function(e) NULL
    )
    missing_safe <- .viewerAuthValidChildNames(children) &&
      (identical(children, character()) ||
        (identical(children, basename(state$paths$manifest_tmp)) &&
          .viewerAuthIdentityMatches(
            state,
            "manifest_tmp",
            state$paths$manifest_tmp
          )) ||
        (identical(children, "provision.rds") &&
          .viewerAuthIdentityMatches(state, "manifest", state$paths$manifest)))
  }
  switch(
    owner$state,
    claimed = !stage_exists && !target_exists,
    staging = !target_exists && (!stage_exists || missing_safe || matches),
    publishing = xor(stage_exists, target_exists) &&
      matches &&
      identical(manifest$state, "ready"),
    published = !stage_exists &&
      target_exists &&
      matches &&
      identical(manifest$state, "ready"),
    FALSE
  )
}

.viewerAuthCleanupProvision <- function(state) {
  if (!isTRUE(state$lock_claimed)) {
    return(NULL)
  }
  incomplete <- function() {
    .viewerAuthProvisionCondition(
      "cleanup_incomplete",
      "cleanup",
      "Authentication cleanup is incomplete.",
      cause_code = if (is.null(state$primary_condition)) {
        NULL
      } else {
        state$primary_condition$code
      },
      recovery_path = .viewerAuthRecoveryPath(state)
    )
  }
  lock_info <- .viewerAuthSafeInspect(state, state$paths$lock)
  if (!is.list(lock_info)) {
    return(incomplete())
  }
  lock_proof <- if (isTRUE(lock_info$exists)) {
    tryCatch(
      .viewerAuthIdentityMatches(state, "lock", state$paths$lock),
      error = function(e) FALSE
    )
  } else {
    identical(.viewerAuthOwnerProofPath(state), state$paths$release_receipt) &&
      .viewerAuthOwnerMatchesState(state)
  }
  if (!isTRUE(lock_proof) || !.viewerAuthLayoutMatchesOwnerState(state)) {
    return(incomplete())
  }
  stage_info <- .viewerAuthSafeInspect(state, state$paths$stage)
  target_info <- .viewerAuthSafeInspect(state, state$paths$target)
  if (!is.list(stage_info) || !is.list(target_info)) {
    return(incomplete())
  }
  stage_exists <- isTRUE(stage_info$exists)
  target_exists <- isTRUE(target_info$exists)
  ok <- !(stage_exists && target_exists)
  if (stage_exists) {
    ok <- ok && .viewerAuthCleanupDirectory(state, state$paths$stage, "stage")
  }
  if (target_exists) {
    ok <- ok && .viewerAuthCleanupDirectory(state, state$paths$target, "target")
  }
  if (!ok) incomplete() else TRUE
}

.viewerAuthReleaseProvisionLock <- function(state) {
  if (!isTRUE(state$lock_claimed)) {
    return(TRUE)
  }
  tryCatch(
    {
      lock_info <- .viewerAuthSafeInspect(state, state$paths$lock)
      lock_exists <- is.list(lock_info) && isTRUE(lock_info$exists)
      owner_info <- .viewerAuthSafeInspect(state, state$paths$owner)
      receipt_info <- .viewerAuthSafeInspect(state, state$paths$release_receipt)
      if (
        !is.list(lock_info) || !is.list(owner_info) || !is.list(receipt_info)
      ) {
        return(FALSE)
      }
      owner_exists <- is.list(owner_info) && isTRUE(owner_info$exists)
      receipt_exists <- is.list(receipt_info) && isTRUE(receipt_info$exists)
      if (owner_exists && receipt_exists) {
        return(FALSE)
      }
      if (!owner_exists && !receipt_exists) {
        if (
          !lock_exists ||
            !is.null(state$owner_state) ||
            !.viewerAuthIdentityMatches(state, "lock", state$paths$lock)
        ) {
          return(FALSE)
        }
        children <- state$ops$list_files(state$paths$lock)
        if (
          !.viewerAuthValidChildNames(children) ||
            any(!children %in% basename(state$paths$owner_tmp)) ||
            (length(children) == 1L &&
              !.viewerAuthIdentityMatches(
                state,
                "owner_tmp",
                state$paths$owner_tmp
              ))
        ) {
          return(FALSE)
        }
        owner <- .viewerAuthOwnerManifest(
          state$identity$operation_id,
          state$paths$target,
          state$paths$stage_basename,
          state$ops$now(),
          "claimed"
        )
        validator <- function(value) {
          .viewerAuthValidOwnerManifest(value) &&
            identical(value$state, "claimed") &&
            identical(value$operation_id, state$identity$operation_id) &&
            identical(value$target_path, state$paths$target) &&
            identical(value$stage_basename, state$paths$stage_basename)
        }
        .viewerAuthAtomicSaveRds(
          state,
          owner,
          state$paths$release_receipt,
          state$paths$release_receipt_tmp,
          "release_receipt",
          validator,
          "artifact_publish_failed",
          "cleanup"
        )
        state$recovery_path <- state$paths$release_receipt
        for (name in children) {
          child <- file.path(state$paths$lock, name)
          info <- .viewerAuthSafeInspect(state, child)
          if (
            !is.list(info) ||
              !isTRUE(info$exists) ||
              !identical(info$type, "file") ||
              isTRUE(info$is_link) ||
              !isTRUE(state$ops$remove_file(child))
          ) {
            return(FALSE)
          }
        }
      } else {
        if (!.viewerAuthOwnerMatchesState(state)) {
          return(FALSE)
        }
        if (
          lock_exists &&
            !.viewerAuthIdentityMatches(state, "lock", state$paths$lock)
        ) {
          return(FALSE)
        }
        if (owner_exists) {
          if (
            !lock_exists ||
              !.viewerAuthIdentityMatches(state, "lock", state$paths$lock)
          ) {
            return(FALSE)
          }
          children <- state$ops$list_files(state$paths$lock)
          allowed <- c("owner.rds", basename(state$paths$owner_tmp))
          if (
            !.viewerAuthValidChildNames(children) || any(!children %in% allowed)
          ) {
            return(FALSE)
          }
          if (basename(state$paths$owner_tmp) %in% children) {
            owner_tmp <- .viewerAuthSafeInspect(state, state$paths$owner_tmp)
            if (
              !is.list(owner_tmp) ||
                !isTRUE(owner_tmp$exists) ||
                !identical(owner_tmp$type, "file") ||
                isTRUE(owner_tmp$is_link) ||
                !tryCatch(
                  .viewerAuthIdentityMatches(
                    state,
                    "owner_tmp",
                    state$paths$owner_tmp
                  ),
                  error = function(e) FALSE
                ) ||
                !isTRUE(state$ops$remove_file(state$paths$owner_tmp))
            ) {
              return(FALSE)
            }
          }
          owner <- state$ops$read_rds(state$paths$owner)
          validator <- function(value) {
            .viewerAuthValidOwnerManifest(value) &&
              identical(value$operation_id, state$identity$operation_id) &&
              identical(value$target_path, state$paths$target) &&
              identical(value$stage_basename, state$paths$stage_basename)
          }
          .viewerAuthAtomicSaveRds(
            state,
            owner,
            state$paths$release_receipt,
            state$paths$release_receipt_tmp,
            "release_receipt",
            validator,
            "artifact_publish_failed",
            "cleanup"
          )
          state$recovery_path <- state$paths$release_receipt
          tryCatch(
            state$ops$remove_file(state$paths$owner),
            error = function(e) FALSE
          )
          owner_after <- .viewerAuthSafeInspect(state, state$paths$owner)
          if (!is.list(owner_after) || isTRUE(owner_after$exists)) {
            return(FALSE)
          }
        }
      }
      after <- .viewerAuthSafeInspect(state, state$paths$lock)
      if (!is.list(after)) {
        return(FALSE)
      }
      if (isTRUE(after$exists)) {
        children <- state$ops$list_files(state$paths$lock)
        if (!.viewerAuthValidChildNames(children) || length(children) != 0L) {
          return(FALSE)
        }
        tryCatch(state$ops$remove_dir(state$paths$lock), error = function(e) {
          FALSE
        })
        after <- .viewerAuthSafeInspect(state, state$paths$lock)
        if (!is.list(after) || isTRUE(after$exists)) return(FALSE)
      }
      state$lock_claimed <- FALSE
      state$recovery_path <- NULL
      receipt <- .viewerAuthSafeInspect(state, state$paths$release_receipt)
      receipt_owned <- is.list(receipt) &&
        isTRUE(receipt$exists) &&
        identical(receipt$type, "file") &&
        !isTRUE(receipt$is_link) &&
        tryCatch(
          .viewerAuthIdentityMatches(
            state,
            "release_receipt",
            state$paths$release_receipt
          ),
          error = function(e) FALSE
        )
      if (receipt_owned) {
        tryCatch(
          state$ops$remove_file(state$paths$release_receipt),
          error = function(e) FALSE
        )
      }
      TRUE
    },
    error = function(e) FALSE
  )
}

.viewerAuthRemoveSqliteSidecars <- function(state) {
  suffixes <- c("-journal", "-wal", "-shm")
  for (suffix in suffixes) {
    path <- paste0(state$paths$credentials, suffix)
    info <- tryCatch(state$ops$inspect_path(path), error = function(e) NULL)
    if (!is.list(info)) {
      .viewerAuthProvisionAbort(
        "artifact_publish_failed",
        "publish",
        "Could not inspect a database sidecar."
      )
    }
    key <- paste0("credentials", sub("^-", "_", suffix))
    if (isTRUE(info$exists)) {
      .viewerAuthFreezeProvisionPath(
        state,
        key,
        path,
        "file",
        NULL,
        "artifact_publish_failed",
        "publish"
      )
    }
    if (
      isTRUE(info$exists) &&
        (!isTRUE(tryCatch(
          .viewerAuthIdentityMatches(state, key, path),
          error = function(e) FALSE
        )) ||
          !isTRUE(tryCatch(
            state$ops$remove_file(path),
            error = function(e) FALSE
          )))
    ) {
      .viewerAuthProvisionAbort(
        "artifact_publish_failed",
        "publish",
        "Could not remove a database sidecar."
      )
    }
  }
  invisible(state)
}

.viewerAuthWriteReadyManifest <- function(state) {
  previous <- tryCatch(
    state$ops$read_rds(state$paths$manifest),
    error = function(e) NULL
  )
  manifest <- .viewerAuthProvisionManifest(
    operation_id = state$identity$operation_id,
    state = "ready",
    created_at = state$ops$now(),
    passphrase_env = state$identity$passphrase_env,
    timeout_minutes = state$options$timeout_minutes,
    user_count = if (.viewerAuthValidProvisionManifest(previous)) {
      previous$user_count
    } else {
      NA_integer_
    }
  )
  .viewerAuthAtomicSaveRds(
    state,
    manifest,
    state$paths$manifest,
    state$paths$manifest_tmp,
    "manifest",
    .viewerAuthValidProvisionManifest,
    "artifact_publish_failed",
    "manifest"
  )
  state$manifest_state <- "ready"
  invisible(state)
}

.viewerAuthPostvalidateProvision <- function(state) {
  root <- state$paths$target
  expected <- c("credentials.sqlite", "viewer-auth.env", "provision.rds")
  children <- tryCatch(state$ops$list_files(root), error = function(e) NULL)
  if (!is.character(children) || !identical(sort(children), sort(expected))) {
    .viewerAuthProvisionAbort(
      "artifact_publish_failed",
      "publish",
      "The published authentication layout is invalid."
    )
  }
  manifest <- tryCatch(
    state$ops$read_rds(file.path(root, "provision.rds")),
    error = function(e) NULL
  )
  if (
    !.viewerAuthValidProvisionManifest(manifest) ||
      !identical(manifest$state, "ready") ||
      !identical(manifest$operation_id, state$identity$operation_id)
  ) {
    .viewerAuthProvisionAbort(
      "artifact_publish_failed",
      "publish",
      "The published authentication manifest is invalid."
    )
  }
  identity_keys <- c(
    "credentials.sqlite" = "credentials",
    "viewer-auth.env" = "secret",
    "provision.rds" = "manifest"
  )
  for (name in expected) {
    info <- tryCatch(
      state$ops$inspect_path(file.path(root, name)),
      error = function(e) NULL
    )
    key <- unname(identity_keys[[name]])
    if (
      !isTRUE(info$exists) ||
        !identical(info$type, "file") ||
        isTRUE(info$is_link) ||
        !identical(info$permissions, "rw-------") ||
        is.null(state$identities[[key]]) ||
        !identical(
          info[c("type", "device_id", "inode")],
          state$identities[[key]]
        )
    ) {
      .viewerAuthProvisionAbort(
        "artifact_publish_failed",
        "publish",
        "A published authentication file failed identity validation."
      )
    }
  }
  passphrase <- .viewerAuthReadProvisionSecret(state, root)
  on.exit(passphrase <- NULL, add = TRUE)
  validated <- .viewerAuthRunProvider(state, function() {
    state$ops$validate_db(file.path(root, "credentials.sqlite"), passphrase)
  })
  if (!isTRUE(validated$ok) || !isTRUE(validated$value)) {
    .viewerAuthProvisionAbort(
      "database_validation_failed",
      "publish",
      "The published authentication database failed validation."
    )
  }
  manifest
}

.viewerAuthPublishProvision <- function(state) {
  if (!.viewerAuthParentStillFrozen(state)) {
    .viewerAuthUnsafeParent()
  }
  .viewerAuthRemoveSqliteSidecars(state)
  .viewerAuthWriteReadyManifest(state)
  .viewerAuthWriteOwnerState(state, "publishing")
  target_info <- tryCatch(
    state$ops$inspect_path(state$paths$target),
    error = function(e) NULL
  )
  if (!is.list(target_info)) {
    .viewerAuthProvisionAbort(
      "artifact_publish_failed",
      "publish",
      "Could not inspect the publication target."
    )
  }
  if (isTRUE(target_info$exists)) {
    .viewerAuthProvisionAbort(
      "target_exists",
      "publish",
      "target_dir appeared before publication."
    )
  }
  .viewerAuthControlledRenameRebind(
    state,
    "stage",
    state$paths$stage,
    "target",
    state$paths$target,
    "directory",
    "rwx------",
    "artifact_publish_failed",
    "publish"
  )
  state$stage_created <- FALSE
  state$target_published <- TRUE
  .viewerAuthPostvalidateProvision(state)
  state$manifest_state <- "ready"
  .viewerAuthWriteOwnerState(state, "published")
  invisible(state)
}
