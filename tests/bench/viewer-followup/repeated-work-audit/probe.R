# Bounded diagnostic only: no production changes or browser timing claims.
args <- commandArgs(TRUE)
root <- normalizePath(args[[1]])
out <- file.path(root, "tests/bench/viewer-followup/repeated-work-audit")
pkgload::load_all(root, quiet = TRUE)
library(shiny)
library(dplyr)
env <- new.env(parent = globalenv())
env$`%||%` <- function(a, b) if (is.null(a)) b else a
definitions <- function(file) {
  for (x in parse(file)) {
    if (is.call(x) && identical(x[[1]], as.name("<-")) && is.call(x[[3]]) &&
        identical(x[[3]][[1]], as.name("function"))) eval(x, env)
  }
}
ir_root <- file.path(root, "inst/viewer/immune_repertoire")
definitions(file.path(ir_root, "data.R"))
definitions(file.path(ir_root, "settings.R"))
definitions(file.path(ir_root, "server.R"))
definitions(file.path(root, "inst/viewer/groups/composition.R"))
source(file.path(ir_root, "param_spec.R"), local = env)
env$input <- reactiveValues(ir_tabs = "Homeostasis", ir_p_clone_size = "0.001, 0.01, 0.1, 0.5, 1",
  ir_p_n_boots = 100, ir_p_scale = TRUE)
panel <- function(tab, more) {
  env$input$ir_tabs <- tab
  html <- isolate(as.character(env$ir_analysis_panel(more)))
  file <- file.path(out, paste0("panel-", tab, ".html"))
  writeLines(html, file)
  cat("CONTROL", tab, html, "\n")
}
panel("Homeostasis", FALSE)
panel("Diversity", TRUE)
panel("Abundance", FALSE)

obj <- readCerebro(args[[2]])
raw <- obj$getImmuneRepertoire()
md <- obj$getMetaData()
timings <- data.frame(stage = character(), iteration = integer(), elapsed_ms = numeric())
measure <- function(stage, iteration, expr) {
  start <- nanonext::mclock()
  value <- force(expr)
  elapsed <- nanonext::mclock() - start
  timings <<- rbind(timings, data.frame(stage, iteration, elapsed_ms = elapsed))
  write.csv(timings, file.path(out, "timings.csv"), row.names = FALSE)
  cat(stage, iteration, elapsed, "ms\n")
  value
}
loadNamespace("scRepertoire")
samples <- names(raw)[1:2]
for (i in 1:3) {
  full <- measure("paired-scatter-all-194", i, scRepertoire::clonalScatter(raw,
    cloneCall = "gene", chain = "both", x.axis = samples[1], y.axis = samples[2], exportTable = TRUE))
  narrow <- measure("paired-scatter-selected-2", i, scRepertoire::clonalScatter(raw[samples],
    cloneCall = "gene", chain = "both", x.axis = samples[1], y.axis = samples[2], exportTable = TRUE))
  stopifnot(identical(full, narrow))
}
cat("PAIR_TABLE_IDENTICAL", nrow(full), "rows\n")
for (i in 1:3) {
  invisible(measure("groups-count-long", i, env$groupsCalculateTableAB(md[c("sample", "major_type")],
    "sample", "major_type", "long", FALSE)))
  invisible(measure("groups-count-wide", i, env$groupsCalculateTableAB(md[c("sample", "major_type")],
    "sample", "major_type", "wide", FALSE)))
}
calls <- 0L
original_join <- env$ir_annotate_metadata
env$ir_annotate_metadata <- function(...) { calls <<- calls + 1L; original_join(...) }
env$ir_data_raw <- reactive(raw)
env$getMetaData <- function() md
env$input <- reactiveValues(ir_tabs = "Diversity", ir_groupBy = "severity")
for (x in parse(file.path(ir_root, "data.R"))) {
  if (is.call(x) && identical(x[[1]], as.name("<-")) &&
      as.character(x[[2]]) %in% c("ir_requested_metadata_columns", "ir_data_annotated")) eval(x, env)
}
for (tab in c("Diversity", "Homeostasis", "Abundance", "Diversity")) {
  env$input$ir_tabs <- tab
  shiny:::flushReact()
  invisible(measure(paste0("same-group-tab-", tab), calls + 1L, isolate(env$ir_data_annotated())))
}
cat("ANNOTATION_CALLS_SAME_GROUP", calls, "\n")
cat("SESSION\n")
print(sessionInfo())
