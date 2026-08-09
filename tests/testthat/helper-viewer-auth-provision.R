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
