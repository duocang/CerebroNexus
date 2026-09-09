# Drive one standalone Viewer through the C2 interaction gate.
#
# Usage: Rscript src/21_measure_viewer.R <source> <n_cells> <backend> \
#   <export_repeat> <crb> <result> <query_plan>

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 7L) {
  stop(
    paste(
      "need <source> <n_cells> <backend> <export_repeat>",
      "<crb> <result> <query_plan>"
    ),
    call. = FALSE
  )
}
src_name <- args[1L]
n_cells <- as.numeric(args[2L])
backend <- args[3L]
export_repeat <- as.integer(args[4L])
crb <- args[5L]
result <- args[6L]
query_plan_path <- args[7L]

here <- Sys.getenv("BENCH_ROOT", "")
if (!nzchar(here)) {
  here <- normalizePath("tests/bench")
}
source(file.path(here, "lib", "bench_utils.R"))
source(file.path(here, "lib", "viewer_validation.R"))
if (nzchar(Sys.getenv("BENCH_LIB"))) {
  .libPaths(c(Sys.getenv("BENCH_LIB"), .libPaths()))
}
suppressPackageStartupMessages(library(CerebroNexus))

row <- data.frame(
  run_id = Sys.getenv("BENCH_RUN_ID"),
  profile = Sys.getenv("BENCH_PROFILE", "panel_c2"),
  source = src_name,
  n_cells = n_cells,
  backend = backend,
  export_repeat = export_repeat,
  gene = NA_character_,
  status = "OK",
  correctness = NA_character_,
  bundle_secs = NA_real_,
  launch_secs = NA_real_,
  hover_secs = NA_real_,
  selection_secs = NA_real_,
  zoom_secs = NA_real_,
  gene_secs = NA_real_,
  stringsAsFactors = FALSE
)

fail <- function(stage, error) {
  row$status <- sprintf("FAILED(%s): %s", stage, conditionMessage(error))
  row$correctness <- "FAILED"
  bench_append_row(result, row)
  bench_msg("Viewer failed at %s: %s", stage, conditionMessage(error))
  quit(status = 0L)
}

query_plan <- tryCatch(
  readRDS(query_plan_path),
  error = function(error) fail("query plan", error)
)
first_gene <- query_plan$panel$gene[query_plan$panel$role == "first"]
if (length(first_gene) != 1L || is.na(first_gene) || !nzchar(first_gene)) {
  fail("query plan", simpleError("frozen first gene is missing"))
}
row$gene <- first_gene

metrics <- tryCatch(
  bench_run_viewer_validation(
    crb = crb,
    app_dir = file.path(dirname(crb), "viewer-app"),
    gene = first_gene
  ),
  error = function(error) {
    stage <- if (is.null(error$stage)) "viewer" else error$stage
    fail(stage, error)
  }
)
row$correctness <- metrics$correctness
timings <- grep("_secs$", names(row), value = TRUE)
for (name in timings) {
  row[[name]] <- metrics[[name]]
}
bench_append_row(result, row)
bench_msg(
  "Viewer OK %s / %.0f cells / %s in %.1fs",
  src_name,
  n_cells,
  backend,
  sum(unlist(row[timings]))
)
