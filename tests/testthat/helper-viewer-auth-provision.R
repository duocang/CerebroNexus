expect_provision_error <- function(object, code, stage, cause_code = NULL) {
  error <- tryCatch(object, error = function(e) e)
  testthat::expect_s3_class(error, "cerebro_viewer_auth_provision_error")
  testthat::expect_identical(error$code, code)
  testthat::expect_identical(error$stage, stage)
  testthat::expect_identical(error$cause_code, cause_code)
  invisible(error)
}

viewer_auth_provision_test_ops <- function(random_values = NULL, ...) {
  ops <- CerebroNexus:::.viewerAuthProvisionOps()
  overrides <- list(...)
  if (!is.null(random_values)) {
    queue <- random_values
    overrides$random_bytes <- function(size) {
      if (!length(queue)) {
        stop("test random queue exhausted", call. = FALSE)
      }
      value <- queue[[1L]]
      queue <<- queue[-1L]
      value
    }
  }
  unknown <- setdiff(names(overrides), names(ops))
  stopifnot(length(unknown) == 0L)
  ops[names(overrides)] <- overrides
  stopifnot(identical(names(ops), CerebroNexus:::.viewerAuthProvisionOpNames))
  ops
}

viewer_auth_provision_public_fixture <- function(passphrase_env = NULL, ...) {
  testthat::skip_on_os("windows")
  parent <- withr::local_tempdir(.local_envir = parent.frame())
  Sys.chmod(parent, "0700")
  ops <- viewer_auth_provision_test_ops(
    random_values = list(as.raw(0:15), as.raw(16:47), as.raw(48:55)),
    sha256 = function(bytes) as.raw(0:31),
    namespace_available = function(package) TRUE,
    package_version = function(package) numeric_version("1.1.0"),
    create_db = function(credentials_data, sqlite_path, passphrase) {
      writeBin(as.raw(c(0x53, 0x51, 0x4c)), sqlite_path)
      TRUE
    },
    validate_db = function(path, passphrase) TRUE,
    ...
  )
  list(
    accounts = data.frame(
      user = c("alice", "bob"),
      password = c("alice-password", "bob-password-12"),
      admin = c(TRUE, FALSE),
      stringsAsFactors = FALSE
    ),
    passwords = c("alice-password", "bob-password-12"),
    operation_id = paste(sprintf("%02x", 0:15), collapse = ""),
    target = file.path(parent, "viewer-auth"),
    passphrase_env = passphrase_env,
    ops = ops
  )
}

viewer_auth_raw_contains <- function(haystack, text) {
  needle <- charToRaw(enc2utf8(text))
  if (!length(needle) || length(needle) > length(haystack)) {
    return(FALSE)
  }
  any(vapply(
    seq_len(length(haystack) - length(needle) + 1L),
    function(start) {
      identical(haystack[start:(start + length(needle) - 1L)], needle)
    },
    logical(1)
  ))
}

viewer_auth_file_contains <- function(path, text) {
  if (dir.exists(path)) {
    return(FALSE)
  }
  size <- file.info(path)$size[[1L]]
  if (is.na(size) || size < 0) {
    return(TRUE)
  }
  viewer_auth_raw_contains(readBin(path, "raw", n = size), text)
}

viewer_auth_provision_test_state <- function(
  install_env = FALSE,
  .local_envir = parent.frame(),
  ...
) {
  testthat::skip_on_os("windows")
  parent <- withr::local_tempdir(.local_envir = .local_envir)
  Sys.chmod(parent, "0700")
  target <- file.path(parent, "viewer-auth")
  ops <- viewer_auth_provision_test_ops(
    random_values = list(as.raw(0:15), as.raw(16:47), as.raw(48:55)),
    sha256 = function(bytes) as.raw(0:31),
    ...
  )
  CerebroNexus:::.viewerAuthNewProvisionState(
    accounts = data.frame(
      user = "alice",
      password = "alice-password",
      admin = TRUE,
      stringsAsFactors = FALSE
    ),
    options = list(
      target_dir = target,
      passphrase_env = NULL,
      timeout_minutes = 15L,
      install_env = install_env
    ),
    ops = ops
  )
}

viewer_auth_provision_prepared_state <- function(
  ...,
  .local_envir = parent.frame()
) {
  state <- viewer_auth_provision_test_state(..., .local_envir = .local_envir)
  CerebroNexus:::.viewerAuthProvisionPreflight(state)
  CerebroNexus:::.viewerAuthPrepareProvisionIdentity(state)
  CerebroNexus:::.viewerAuthPrepareProvisionPaths(state)
  state
}

viewer_auth_provision_staged_state <- function(
  ...,
  .local_envir = parent.frame()
) {
  overrides <- list(...)
  if (is.null(overrides$create_db)) {
    overrides$create_db <- function(credentials_data, sqlite_path, passphrase) {
      writeBin(as.raw(c(0x53, 0x51, 0x4c)), sqlite_path)
      TRUE
    }
  }
  if (is.null(overrides$validate_db)) {
    overrides$validate_db <- function(path, passphrase) TRUE
  }
  overrides$.local_envir <- .local_envir
  state <- do.call(viewer_auth_provision_prepared_state, overrides)
  CerebroNexus:::.viewerAuthAcquireProvisionLock(state)
  CerebroNexus:::.viewerAuthCreateProvisionStage(state)
  state
}
