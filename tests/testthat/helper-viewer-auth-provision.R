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
