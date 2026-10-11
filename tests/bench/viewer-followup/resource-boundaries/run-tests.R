# Run from the repository root in the existing WSL R environment.
out <- "tests/bench/viewer-followup/resource-boundaries"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
r <- testthat::test_local(
  filter = "^resource-boundaries$|^utility_functions$|^r-functions$|^exportFromSeurat$|^viewer-review-feedback$|^viewer-sync-performance$|^createShinyApp-sibling$",
  reporter = "summary", stop_on_failure = FALSE
)
s <- as.data.frame(r)
write.csv(s[c("file", "test", "nb", "failed", "passed", "skipped", "warning")],
          file.path(out, "tests.csv"), row.names = FALSE)
cat("TOTAL", sum(s$passed), "passed", sum(s$failed), "failed", sum(s$error),
    "errors", sum(s$skipped), "skipped", sum(s$warning), "warnings\n")
quit(status = if (any(s$error) || any(s$failed)) 1L else 0L)
