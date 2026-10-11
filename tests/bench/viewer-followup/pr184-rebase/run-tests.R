args <- commandArgs(TRUE)
scope <- if (length(args)) args[[1L]] else "unit"
out <- "tests/bench/viewer-followup/pr184-rebase"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
filter <- if (scope == "app-recheck") {
  "^app-inst$"
} else if (scope == "browser") {
  "^app-inst$|^app-immune_repertoire$|^smoke-production$"
} else {
  paste(c(
    "^resource-boundaries$", "^utility_functions$", "^r-functions$",
    "^exportFromSeurat$", "^viewer-review-feedback$", "^viewer-sync-performance$",
    "^createShinyApp-sibling$", "^repeated-viewer-work$", "^ir-",
    "^groups-performance$", "^result-page-controls$", "^viewer-pack$",
    "^immune_repertoire$", "^gene-expression-", "^projection-first-frame-performance$",
    "^hla-", "^spatial$", "^viewer-extra-material-selector$", "^viewer-accessibility-design$"
  ), collapse = "|")
}
r <- testthat::test_local(filter = filter, reporter = "summary", stop_on_failure = FALSE)
s <- as.data.frame(r)
write.csv(s[c("file", "test", "nb", "failed", "passed", "skipped", "warning", "error")],
          file.path(out, paste0(scope, ".csv")), row.names = FALSE)
cat("TOTAL", sum(s$passed), "passed", sum(s$failed), "failed", sum(s$error),
    "errors", sum(s$skipped), "skipped", sum(s$warning), "warnings\n")
quit(status = if (any(s$error) || any(s$failed)) 1L else 0L)
