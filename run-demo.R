#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
repo_root <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE))
} else {
  normalizePath(".", mustWork = TRUE)
}

suppressPackageStartupMessages(library(CerebroNexus))
source(
  file.path(repo_root, "tests", "bench", "prepare_viewer_1m_data.R"),
  local = TRUE
)

crb <- prepareViewer1mBenchmarkData()
Sys.setenv(CEREBRO_1M_DEMO_CRB = normalizePath(crb, mustWork = TRUE))

shiny::runApp(
  file.path(repo_root, "inst"),
  host = "127.0.0.1",
  port = 7451,
  launch.browser = TRUE
)
