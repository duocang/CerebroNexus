# Isolate the three failing Builder tests, with the baseline version of the
# only createShinyApp.R function changed by this task. No checkout is modified.
pkgload::load_all(".", quiet = TRUE)
baseline <- "066df6f05059ba5bda4d09ef70f5ab904fe7d879"
original <- parse(text = system2("git", c("show", paste0(baseline, ":R/createShinyApp.R")), stdout = TRUE))
assignment <- Filter(function(x) is.call(x) && identical(x[[1L]], as.name("<-")) &&
                       identical(x[[2L]], as.name(".datasetCatalogFromFiles")), as.list(original))
stopifnot(length(assignment) == 1L)
namespace <- asNamespace("CerebroNexus")
unlockBinding(".datasetCatalogFromFiles", namespace)
eval(assignment[[1L]], namespace)
lockBinding(".datasetCatalogFromFiles", namespace)
cat("Baseline createShinyApp.R function:", baseline, "\n")
setwd("tests/testthat")
expressions <- parse("test-createShinyApp-sibling.R")
selected <- Filter(function(x) {
  if (!is.call(x) || !identical(x[[1L]], as.name("test_that"))) return(TRUE)
  grepl("one CRB cannot be reused|Cerebro data labels cannot|Cerebro data labels must", as.character(x[[2L]]))
}, as.list(expressions))
env <- new.env(parent = as.environment("package:CerebroNexus"))
env$test_that <- testthat::test_that
library(testthat)
testthat::with_reporter("summary", {
  for (expression in selected) eval(expression, env)
})
