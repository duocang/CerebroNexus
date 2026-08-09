.viewerAuthProvisionCodes <- c(
  "invalid_accounts",
  "invalid_options",
  "missing_dependency",
  "unsafe_parent",
  "target_exists",
  "lock_conflict",
  "environment_conflict",
  "secret_generation_failed",
  "database_create_failed",
  "database_validation_failed",
  "artifact_publish_failed",
  "environment_install_failed",
  "cleanup_incomplete"
)

.viewerAuthProvisionStages <- c(
  "input",
  "dependency",
  "preflight",
  "lock",
  "artifact_stage",
  "database",
  "manifest",
  "publish",
  "environment",
  "cleanup"
)

.viewerAuthProvisionCondition <- function(
  code,
  stage,
  message,
  cause_code = NULL,
  recovery_path = NULL
) {
  if (
    !is.character(code) ||
      length(code) != 1L ||
      is.na(code) ||
      !(code %in% .viewerAuthProvisionCodes) ||
      !is.character(stage) ||
      length(stage) != 1L ||
      is.na(stage) ||
      !(stage %in% .viewerAuthProvisionStages) ||
      !is.character(message) ||
      length(message) != 1L ||
      is.na(message)
  ) {
    stop("Invalid viewer authentication provisioning condition.", call. = FALSE)
  }
  if (
    !is.null(cause_code) &&
      (!is.character(cause_code) ||
        length(cause_code) != 1L ||
        is.na(cause_code) ||
        !(cause_code %in% .viewerAuthProvisionCodes))
  ) {
    stop("Invalid provisioning cause code.", call. = FALSE)
  }
  if (
    (identical(code, "cleanup_incomplete") && is.null(recovery_path)) ||
      (!is.null(recovery_path) &&
        (!.viewerAuthProvisionScalarString(recovery_path) ||
          !.nativeAbsolutePath(recovery_path)))
  ) {
    stop("Invalid provisioning recovery path.", call. = FALSE)
  }
  classes <- c(
    paste0("cerebro_viewer_auth_", code),
    "cerebro_viewer_auth_provision_error",
    "error",
    "condition"
  )
  structure(
    list(
      message = message,
      call = NULL,
      code = code,
      stage = stage,
      cause_code = cause_code,
      recovery_path = recovery_path
    ),
    class = classes
  )
}

.viewerAuthProvisionError <- function(
  code,
  stage,
  message,
  cause_code = NULL,
  recovery_path = NULL
) {
  .viewerAuthProvisionCondition(code, stage, message, cause_code, recovery_path)
}

.viewerAuthProvisionWarning <- function(
  code,
  stage,
  message,
  cause_code = NULL,
  recovery_path = NULL
) {
  condition <- .viewerAuthProvisionCondition(
    code,
    stage,
    message,
    cause_code,
    recovery_path
  )
  class(condition) <- c(
    class(condition)[
      !class(condition) %in% c("error", "cerebro_viewer_auth_provision_error")
    ],
    "warning",
    "condition"
  )
  condition
}

.viewerAuthProvisionCleanupWarning <- function(
  message,
  recovery_path,
  cause_code = NULL
) {
  .viewerAuthProvisionWarning(
    "cleanup_incomplete",
    "cleanup",
    message,
    cause_code = cause_code,
    recovery_path = recovery_path
  )
}

.viewerAuthProvisionAbort <- function(
  code,
  stage,
  message,
  cause_code = NULL,
  recovery_path = NULL
) {
  stop(.viewerAuthProvisionCondition(
    code,
    stage,
    message,
    cause_code,
    recovery_path
  ))
}

.viewerAuthProvisionScalarString <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
}

.viewerAuthProvisionValidUtf8 <- function(value) {
  is.character(value) &&
    !is.na(value) &&
    !is.na(iconv(value, from = "", to = "UTF-8", sub = NA_character_))
}

.viewerAuthProvisionUtf8 <- function(value) {
  converted <- iconv(value, from = "", to = "UTF-8", sub = NA_character_)
  if (is.na(converted)) NA_character_ else enc2utf8(converted)
}

.viewerAuthProvisionHasControl <- function(value) {
  raw <- charToRaw(enc2utf8(value))
  any(as.integer(raw) <= 31L | as.integer(raw) == 127L)
}

.viewerAuthProvisionAccountInvalid <- function() {
  .viewerAuthProvisionAbort(
    "invalid_accounts",
    "input",
    "Invalid account provisioning input."
  )
}

.viewerAuthNormalizeAccounts <- function(accounts) {
  if (
    !identical(class(accounts), "data.frame") ||
      nrow(accounts) < 1L ||
      nrow(accounts) > 1000L ||
      anyDuplicated(names(accounts)) ||
      (!setequal(names(accounts), c("user", "password")) &&
        !setequal(names(accounts), c("user", "password", "admin")))
  ) {
    .viewerAuthProvisionAccountInvalid()
  }
  if (
    !is.character(accounts$user) ||
      is.factor(accounts$user) ||
      !is.character(accounts$password) ||
      is.factor(accounts$password) ||
      anyNA(accounts$user) ||
      anyNA(accounts$password)
  ) {
    .viewerAuthProvisionAccountInvalid()
  }
  if (
    "admin" %in%
      names(accounts) &&
      (!is.logical(accounts$admin) || anyNA(accounts$admin))
  ) {
    .viewerAuthProvisionAccountInvalid()
  }
  users <- vapply(accounts$user, .viewerAuthProvisionUtf8, character(1))
  passwords <- vapply(accounts$password, .viewerAuthProvisionUtf8, character(1))
  if (anyNA(users) || anyNA(passwords)) {
    .viewerAuthProvisionAccountInvalid()
  }
  users <- gsub(
    "^[ \\t\\r\\n\\h\\v]+|[ \\t\\r\\n\\h\\v]+$",
    "",
    users,
    perl = TRUE
  )
  if (
    any(!nzchar(users)) ||
      anyDuplicated(users) ||
      any(nchar(enc2utf8(users), type = "bytes") > 128L) ||
      any(vapply(users, .viewerAuthProvisionHasControl, logical(1))) ||
      any(nchar(enc2utf8(passwords), type = "bytes") < 12L) ||
      any(nchar(enc2utf8(passwords), type = "bytes") > 1024L) ||
      any(vapply(passwords, .viewerAuthProvisionHasControl, logical(1)))
  ) {
    .viewerAuthProvisionAccountInvalid()
  }
  data.frame(
    user = users,
    password = passwords,
    admin = if ("admin" %in% names(accounts)) {
      accounts$admin
    } else {
      rep(FALSE, nrow(accounts))
    },
    stringsAsFactors = FALSE
  )
}

.viewerAuthNormalizeProvisionOptions <- function(
  target_dir,
  passphrase_env,
  timeout_minutes,
  install_env
) {
  valid_path <- .viewerAuthProvisionScalarString(target_dir) &&
    .viewerAuthProvisionValidUtf8(target_dir) &&
    !.viewerAuthProvisionHasControl(target_dir)
  valid_optional_env <- function(value) {
    if (is.null(value)) {
      return(TRUE)
    }
    .viewerAuthProvisionScalarString(value) &&
      grepl("^[A-Za-z_][A-Za-z0-9_]*$", value)
  }
  valid_timeout <- is.numeric(timeout_minutes) &&
    !is.logical(timeout_minutes) &&
    length(timeout_minutes) == 1L &&
    !is.na(timeout_minutes) &&
    is.finite(timeout_minutes) &&
    timeout_minutes == floor(timeout_minutes) &&
    timeout_minutes >= 1L &&
    timeout_minutes <= 1440L
  if (
    !valid_path ||
      !valid_optional_env(passphrase_env) ||
      !valid_timeout ||
      !is.logical(install_env) ||
      length(install_env) != 1L ||
      is.na(install_env)
  ) {
    .viewerAuthProvisionAbort(
      "invalid_options",
      "input",
      "Invalid provisioning options."
    )
  }
  list(
    target_dir = enc2utf8(target_dir),
    passphrase_env = if (is.null(passphrase_env)) {
      NULL
    } else {
      enc2utf8(passphrase_env)
    },
    timeout_minutes = as.integer(timeout_minutes),
    install_env = install_env
  )
}

.viewerAuthProvisionResult <- function(
  auth,
  secret_file,
  manifest_file,
  user_count,
  environment_installed
) {
  valid_env <- function(value) {
    .viewerAuthProvisionScalarString(value) &&
      grepl("^[A-Za-z_][A-Za-z0-9_]*$", value)
  }
  valid_path <- function(value) {
    .viewerAuthProvisionScalarString(value) && .nativeAbsolutePath(value)
  }
  valid_auth <- identical(class(auth), "list") &&
    identical(
      names(auth),
      c("credentials", "passphrase_env", "timeout_minutes")
    ) &&
    valid_path(auth$credentials) &&
    valid_env(auth$passphrase_env) &&
    is.integer(auth$timeout_minutes) &&
    length(auth$timeout_minutes) == 1L &&
    !is.na(auth$timeout_minutes) &&
    auth$timeout_minutes >= 1L &&
    auth$timeout_minutes <= 1440L
  valid_count <- is.integer(user_count) &&
    length(user_count) == 1L &&
    !is.na(user_count) &&
    user_count >= 1L &&
    user_count <= 1000L
  valid_installed <- is.logical(environment_installed) &&
    length(environment_installed) == 1L &&
    !is.na(environment_installed)
  if (
    !valid_auth ||
      !valid_path(secret_file) ||
      !valid_path(manifest_file) ||
      !valid_count ||
      !valid_installed
  ) {
    stop("Invalid viewer authentication provisioning result.", call. = FALSE)
  }
  structure(
    list(
      schema_version = 1L,
      auth = list(
        credentials = auth$credentials,
        passphrase_env = auth$passphrase_env,
        timeout_minutes = auth$timeout_minutes
      ),
      secret_file = secret_file,
      manifest_file = manifest_file,
      user_count = user_count,
      environment_installed = environment_installed
    ),
    class = "cerebro_viewer_auth_provision"
  )
}

print.cerebro_viewer_auth_provision <- function(x, ...) {
  cat("Viewer authentication provisioning:\n")
  cat("  user count:", x$user_count, "\n")
  cat("  passphrase environment:", x$auth$passphrase_env, "\n")
  cat("  environment installed:", x$environment_installed, "\n")
  invisible(x)
}

.viewerAuthProvisionStateFields <- c(
  "accounts",
  "options",
  "ops",
  "identity",
  "passphrase",
  "preflight",
  "paths",
  "owner_state",
  "manifest_state",
  "identities",
  "lock_claimed",
  "stage_created",
  "target_published",
  "env_owned",
  "environment_installed",
  "committed",
  "provider_capture",
  "secret_bytes",
  "primary_condition",
  "recovery_path"
)

.viewerAuthNewProvisionState <- function(accounts, options, ops) {
  if (!identical(names(ops), .viewerAuthProvisionOpNames)) {
    stop("Invalid provisioning operations.", call. = FALSE)
  }
  state <- new.env(parent = emptyenv())
  values <- list(
    accounts = accounts,
    options = options,
    ops = ops,
    identity = NULL,
    passphrase = NULL,
    preflight = NULL,
    paths = NULL,
    owner_state = NULL,
    manifest_state = NULL,
    identities = list(),
    lock_claimed = FALSE,
    stage_created = FALSE,
    target_published = FALSE,
    env_owned = FALSE,
    environment_installed = FALSE,
    committed = FALSE,
    provider_capture = list(output = NULL, message = NULL),
    secret_bytes = NULL,
    primary_condition = NULL,
    recovery_path = NULL
  )
  for (name in .viewerAuthProvisionStateFields) {
    state[[name]] <- values[[name]]
  }
  state
}

.viewerAuthScrubProvisionState <- function(state) {
  state$accounts <- NULL
  state$passphrase <- NULL
  state$provider_capture <- NULL
  state$secret_bytes <- NULL
  invisible(NULL)
}

.viewerAuthUtcTimestamp <- function(value) {
  .viewerAuthProvisionScalarString(value) &&
    grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$", value)
}

.viewerAuthOwnerManifest <- function(
  operation_id,
  target_path,
  stage_basename,
  created_at,
  state
) {
  list(
    schema_version = 1L,
    operation_id = operation_id,
    state = state,
    target_path = target_path,
    stage_basename = stage_basename,
    created_at = created_at
  )
}

.viewerAuthProvisionManifest <- function(
  operation_id,
  state,
  created_at,
  passphrase_env,
  timeout_minutes,
  user_count
) {
  list(
    schema_version = 1L,
    operation_id = operation_id,
    state = state,
    created_at = created_at,
    passphrase_env = passphrase_env,
    timeout_minutes = as.integer(timeout_minutes),
    user_count = as.integer(user_count),
    artifacts = c(
      credentials = "credentials.sqlite",
      secret = "viewer-auth.env",
      manifest = "provision.rds"
    )
  )
}

.viewerAuthValidOwnerManifest <- function(value) {
  expected <- c(
    "schema_version",
    "operation_id",
    "state",
    "target_path",
    "stage_basename",
    "created_at"
  )
  identical(class(value), "list") &&
    identical(names(value), expected) &&
    identical(value$schema_version, 1L) &&
    .viewerAuthProvisionScalarString(value$operation_id) &&
    grepl("^[0-9a-f]{32}$", value$operation_id) &&
    .viewerAuthProvisionScalarString(value$state) &&
    value$state %in% c("claimed", "staging", "publishing", "published") &&
    .viewerAuthProvisionScalarString(value$target_path) &&
    isTRUE(.nativeAbsolutePath(value$target_path)) &&
    !.viewerAuthProvisionHasControl(value$target_path) &&
    .viewerAuthProvisionScalarString(value$stage_basename) &&
    grepl(
      "^\\.cerebro-auth-[0-9a-f]{16}-[0-9a-f]{32}\\.stage$",
      value$stage_basename
    ) &&
    .viewerAuthUtcTimestamp(value$created_at)
}

.viewerAuthValidProvisionManifest <- function(value) {
  expected <- c(
    "schema_version",
    "operation_id",
    "state",
    "created_at",
    "passphrase_env",
    "timeout_minutes",
    "user_count",
    "artifacts"
  )
  artifacts <- c(
    credentials = "credentials.sqlite",
    secret = "viewer-auth.env",
    manifest = "provision.rds"
  )
  identical(class(value), "list") &&
    identical(names(value), expected) &&
    identical(value$schema_version, 1L) &&
    .viewerAuthProvisionScalarString(value$operation_id) &&
    grepl("^[0-9a-f]{32}$", value$operation_id) &&
    .viewerAuthProvisionScalarString(value$state) &&
    value$state %in% c("staging", "ready") &&
    .viewerAuthUtcTimestamp(value$created_at) &&
    .viewerAuthProvisionScalarString(value$passphrase_env) &&
    grepl("^[A-Za-z_][A-Za-z0-9_]*$", value$passphrase_env) &&
    is.integer(value$timeout_minutes) &&
    length(value$timeout_minutes) == 1L &&
    !is.na(value$timeout_minutes) &&
    value$timeout_minutes >= 1L &&
    value$timeout_minutes <= 1440L &&
    is.integer(value$user_count) &&
    length(value$user_count) == 1L &&
    !is.na(value$user_count) &&
    value$user_count >= 1L &&
    value$user_count <= 1000L &&
    identical(value$artifacts, artifacts)
}
