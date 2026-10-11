args <- commandArgs(TRUE)
result <- testthat::test_local(args[[1]],
  filter = "^ir-|^viewer-pack$|^immune-repertoire-validation$|^immune_repertoire$",
  reporter = "summary", stop_on_failure = TRUE)
summary <- as.data.frame(result)
write.csv(summary[c("file", "test", "nb", "failed", "skipped", "warning", "passed")],
  file.path(args[[1]], "tests/bench/viewer-followup/ir-latency/fix/tests.csv"), row.names = FALSE)
cat("TOTAL", sum(summary$passed), "passed;", sum(summary$failed), "failed;",
  sum(summary$skipped), "skipped;", sum(summary$warning), "warnings\n")
