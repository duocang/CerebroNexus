# Read-only bounded measurements. No app monkey patch or production mutation.
args <- commandArgs(TRUE)
root <- normalizePath(args[[1]])
out <- file.path(root, "tests/bench/viewer-followup/repeated-work-fix/quantification")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
setwd(root)
pkgload::load_all(root, quiet = TRUE)
library(shiny)
library(dplyr)
resume <- length(args) > 2L && identical(args[[3]], "tables")
times <- if (resume) read.csv(file.path(out, "timings.csv")) else
  data.frame(stage = character(), arm = character(), iteration = integer(), ms = numeric())
measure <- function(stage, arm, iteration, expr) {
  force(stage); force(arm); force(iteration)
  start <- nanonext::mclock()
  result <- force(expr)
  times <<- rbind(times, data.frame(stage, arm, iteration, ms = nanonext::mclock() - start))
  write.csv(times, file.path(out, "timings.csv"), row.names = FALSE)
  print(tail(times, 1), row.names = FALSE)
  result
}
definitions <- function(expressions, env) {
  for (x in expressions) if (is.call(x) && identical(x[[1]], as.name("<-")) &&
    is.call(x[[3]]) && identical(x[[3]][[1]], as.name("function"))) eval(x, env)
}
old <- new.env(parent = globalenv())
now <- new.env(parent = globalenv())
files <- c("inst/viewer/immune_repertoire/data.R", "inst/viewer/immune_repertoire/visualizations.R",
  "inst/viewer/groups/composition.R", "inst/viewer/utility_functions.R")
for (file in files) {
  definitions(parse(file), now)
  definitions(parse(text = system2("git", c("show", paste0("d40e785a:", file)), stdout = TRUE)), old)
}
for (e in list(old, now)) {
  e$`%||%` <- function(a, b) if (is.null(a)) b else a
  e$IR_PALETTE <- "inferno"
  e$ir_param <- function(id, default) default
}
if (!resume) {
obj <- measure("read-crb", "current-first-process", 1, readCerebro(args[[2]]))
raw <- measure("hydrate-ir-sidecar", "current-first-process", 1, obj$getImmuneRepertoire())
md <- measure("get-metadata", "current-first-process", 1, obj$getMetaData())
invisible(measure("scRepertoire-namespace", "current-first-process", 1, loadNamespace("scRepertoire")))
cat("IDENTITY", length(raw), "samples", sum(vapply(raw, nrow, integer(1))), "IR rows\n")
for (i in 1:3) {
  profile <- measure("sizedist-profile", "current", i, now$ir_clone_size_profiles(raw))
  profile$clone_counts <- NULL
  p <- measure("sizedist-plot-object", "current", i, now$ir_size_profile_plot(profile, "complete"))
  grDevices::pdf(NULL)
  fig <- measure("sizedist-ggplotly", "current", i, plotly::ggplotly(p))
  json <- measure("sizedist-json", "current", i, plotly::plotly_json(fig, jsonedit = FALSE))
  grDevices::dev.off()
  cat("SIZEDIST_JSON_BYTES", nchar(json, type = "bytes"), "\n")
}
# Execute production reactive expressions, including the current field observer.
annotation_env <- function(arm) {
  e <- new.env(parent = if (arm == "baseline") old else now)
  e$input <- reactiveValues(ir_tabs = "Diversity", ir_groupBy = "severity")
  e$ir_data_raw <- reactive(raw)
  e$getMetaData <- function() md
  e$counter <- new.env(); e$counter$n <- 0L
  e$ir_annotate_metadata <- function(...) { e$counter$n <- e$counter$n + 1L; now$ir_annotate_metadata(...) }
  expressions <- if (arm == "baseline") parse(text = system2("git",
    c("show", "d40e785a:inst/viewer/immune_repertoire/data.R"), stdout = TRUE)) else parse(files[[1]])
  for (x in expressions) {
    if (is.call(x) && identical(x[[1]], as.name("<-")) && is.symbol(x[[2]]) &&
        as.character(x[[2]]) %in% c("ir_requested_metadata_columns", "ir_data_annotated")) eval(x, e)
    if (is.call(x) && identical(x[[1]], as.name("observe")) &&
        "ir_requested_metadata_columns" %in% all.names(x)) e$observer <- eval(x, e)
  }
  shiny:::flushReact()
  isolate(e$ir_data_annotated())
  e
}
before <- annotation_env("baseline"); after <- annotation_env("current")
for (i in 1:3) {
  tab <- c("Homeostasis", "Abundance", "Diversity")[[i]]
  a <- measure("same-field-tab-change", "baseline", i, {
    before$input$ir_tabs <- tab; shiny:::flushReact(); isolate(before$ir_data_annotated())
  })
  b <- measure("same-field-tab-change", "current", i, {
    after$input$ir_tabs <- tab; shiny:::flushReact(); isolate(after$ir_data_annotated())
  })
  stopifnot(identical(a, b))
}
cat("ANNOTATION_CALLS", before$counter$n, after$counter$n, "including initial demand\n")
after$observer$destroy()
table <- md[c("sample", "major_type")]
counts <- now$groupsCalculateTableAB(table, "sample", "major_type", "long", FALSE)
for (i in 1:3) {
  # Simulate another presentation of already-prepared current groups.
  a <- measure("groups-wide-presentation", "baseline", i,
    old$groupsCalculateTableAB(table, "sample", "major_type", "wide", FALSE))
  b <- measure("groups-wide-presentation", "current", i,
    now$groupsFormatComposition(counts, "sample", "major_type", "wide", FALSE))
  stopifnot(identical(a, b))
  samples <- names(raw)[1:2]
  pars <- list(cloneCall = "gene", chain = "both")
  a <- measure("paired-scatter-plot-object", "baseline", i,
    old$ir_paired_scatter_panel(raw, pars, samples[1], samples[2], "test"))
  b <- measure("paired-scatter-plot-object", "current", i,
    now$ir_paired_scatter_panel(raw, pars, samples[1], samples[2], "test"))
  stopifnot(isTRUE(all.equal(ggplot2::ggplot_build(a)$data, ggplot2::ggplot_build(b)$data)))
}
} else {
  obj <- readCerebro(args[[2]])
  raw <- obj$getImmuneRepertoire()
  md <- obj$getMetaData()
  loadNamespace("scRepertoire")
  times <- times[!times$stage %in% c("demo-marker", "synthetic-100k-table"), ]
}
demo <- readCerebro(file.path(root, "inst/extdata/examples/example.crb"))
marker <- demo$getMarkerGenes("cerebro_seurat", "seurat_clusters")
cat("DEMO_MARKER", nrow(marker), ncol(marker), "\n")
# One real stored result table, plus a labelled stress fixture, not a real dataset.
stress <- data.frame(gene = sprintf("g%d", seq_len(100000)),
  avg_log2FC = rep(c(.1, -.2, .3), length.out = 100000),
  p_val = seq_len(100000) / 100000, pct = rep(.5, 100000), count = seq_len(100000))
for (name in c("demo-marker", "synthetic-100k-table")) {
  frame <- if (name == "demo-marker") marker else stress
  for (i in 1:3) {
    a <- measure(name, "baseline", i, old$prettifyTable(frame, "none", "t"))
    b <- measure(name, "current", i, now$prettifyTable(frame, "none", "t"))
    # DT emits numeric alignment clauses in discovery order. Sorting those
    # independent clauses tests equivalence without requiring byte order.
    ax <- a$x; bx <- b$x
    ac <- ax$options$rowCallback; bc <- bx$options$rowCallback
    cat("TABLE_DIFF", name, paste(all.equal(ax, bx), collapse = "; "), "\n")
    stopifnot(identical(sort(strsplit(as.character(ac), "\n", fixed = TRUE)[[1]]),
      sort(strsplit(as.character(bc), "\n", fixed = TRUE)[[1]])))
    ax$options$rowCallback <- NULL; bx$options$rowCallback <- NULL
    stopifnot(identical(ax, bx))
  }
}
# Non-default analysis still uses the unchanged scientific implementation.
annotated <- now$ir_annotate_metadata(raw, md, "severity")
for (i in 1:3) {
  set.seed(1700 + i)
  invisible(measure("diversity-strict-both-20-bootstrap", "current", i,
    scRepertoire::clonalDiversity(raw, cloneCall = "strict", chain = "both", n.boots = 20, exportTable = TRUE)))
  invisible(measure("homeostasis-severity-gene-both", "current", i,
    scRepertoire::clonalHomeostasis(annotated, cloneCall = "gene",
      chain = "both", group.by = "severity", palette = "inferno")))
}
write.csv(aggregate(ms ~ stage + arm, times, median), file.path(out, "medians.csv"), row.names = FALSE)
cat("ALL_OUTPUT_EQUALITY_CHECKS_PASSED\n")
print(sessionInfo())
