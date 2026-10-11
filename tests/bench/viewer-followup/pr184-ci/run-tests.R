args <- commandArgs(TRUE)
scope <- if (length(args)) args[[1L]] else "full"
out <- "tests/bench/viewer-followup/pr184-ci"
if (scope == "check") {
  Sys.setenv(R_REPOSITORIES = normalizePath(".github/ci-repositories"))
  result <- devtools::check(document = FALSE, manual = FALSE, cran = TRUE,
    vignettes = TRUE, error_on = "never", check_dir = "/tmp/cerebro-pr184-ci-check")
  saveRDS(result, file.path(out, "check-result.rds"))
  cat("TOTAL", length(result$errors), "errors", length(result$warnings), "warnings\n")
  quit(status = if (length(result$errors) || length(result$warnings)) 1L else 0L)
}
devtools::load_all(".")
result <- testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = FALSE)
summary <- as.data.frame(result)
write.csv(summary[c("file", "test", "nb", "failed", "passed", "skipped", "warning", "error")],
  file.path(out, "full.csv"), row.names = FALSE)
cat("TOTAL", sum(summary$passed), "passed", sum(summary$failed), "failed", sum(summary$error),
  "errors", sum(summary$skipped), "skipped", sum(summary$warning), "warnings\n")
quit(status = if (any(summary$error) || any(summary$failed)) 1L else 0L)
