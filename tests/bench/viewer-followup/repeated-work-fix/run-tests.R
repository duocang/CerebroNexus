r <- testthat::test_local(filter = "^repeated-viewer-work$|^ir-|^groups-performance$|^result-page-controls$|^viewer-pack$|^immune_repertoire$", reporter = "summary")
s <- as.data.frame(r)
write.csv(s[c("file", "test", "nb", "failed", "passed", "skipped", "warning")],
  "tests/bench/viewer-followup/repeated-work-fix/tests.csv", row.names = FALSE)
cat("TOTAL", sum(s$passed), "passed", sum(s$failed), "failed", sum(s$skipped), "skipped", sum(s$warning), "warnings\n")
