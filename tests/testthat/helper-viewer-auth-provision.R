expect_provision_error <- function(object, code, stage, cause_code = NULL) {
  error <- tryCatch(object, error = function(e) e)
  testthat::expect_s3_class(error, "cerebro_viewer_auth_provision_error")
  testthat::expect_identical(error$code, code)
  testthat::expect_identical(error$stage, stage)
  testthat::expect_identical(error$cause_code, cause_code)
  invisible(error)
}
