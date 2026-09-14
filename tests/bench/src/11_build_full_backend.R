# Build one full-source out-of-core backend and its portable Cerebro shell.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 8L) {
  stop(
    paste(
      "need <source> <n_cells> <backend> <build_repeat>",
      "<order_position> <scratch> <result> <query_plan>"
    ),
    call. = FALSE
  )
}
src_name <- args[1L]
n_cells <- as.numeric(args[2L])
backend <- args[3L]
build_repeat <- as.integer(args[4L])
order_position <- as.integer(args[5L])
scratch <- args[6L]
result <- args[7L]
query_plan_path <- args[8L]

here <- Sys.getenv("BENCH_ROOT", "")
if (!nzchar(here)) {
  here <- normalizePath("tests/bench")
}
source(file.path(here, "config", "sources.R"))
source(file.path(here, "lib", "bench_utils.R"))
source(file.path(here, "lib", "protocol.R"))
source(file.path(here, "lib", "full_source.R"))
if (nzchar(Sys.getenv("BENCH_LIB"))) {
  .libPaths(c(Sys.getenv("BENCH_LIB"), .libPaths()))
}
suppressPackageStartupMessages(library(CerebroNexus))

spec <- BENCH_SOURCES[[src_name]]
if (is.null(spec)) {
  stop("unknown source: ", src_name, call. = FALSE)
}
source_path <- file.path(
  scratch,
  "sources",
  basename(sub("\\?.*$", "", spec$url))
)
out_dir <- file.path(
  scratch,
  "export",
  sprintf("%s_%.0f_%s_r%d", src_name, n_cells, backend, build_repeat)
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
crb <- file.path(out_dir, "bench.crb")
sibling <- if (backend == "bpcells") {
  file.path(out_dir, "bench.bpcells")
} else {
  file.path(out_dir, "bench.h5")
}

row <- data.frame(
  run_id = Sys.getenv("BENCH_RUN_ID"),
  profile = Sys.getenv("BENCH_PROFILE", "panel_c2"),
  source = src_name,
  label = spec$label,
  n_cells = n_cells,
  n_genes = NA_real_,
  nnz = NA_real_,
  backend = backend,
  export_repeat = build_repeat,
  order_position = order_position,
  status = "OK",
  read_secs = NA_real_,
  seurat_secs = 0,
  export_secs = NA_real_,
  crb_mb = NA_real_,
  sibling_mb = NA_real_,
  total_mb = NA_real_,
  rss_mb = NA_real_,
  peak_rss_mb = NA_real_,
  r_peak_mb = NA_real_,
  query_plan_fingerprint = NA_character_,
  stringsAsFactors = FALSE
)

fail <- function(stage, error) {
  row$status <- sprintf("FAILED(%s): %s", stage, conditionMessage(error))
  bench_append_row(result, row)
  bench_msg("FAILED at %s: %s", stage, conditionMessage(error))
  quit(status = 0L)
}

source_matrix <- NULL
row$read_secs <- tryCatch(
  bench_time(source_matrix <<- bench_open_full_source(spec, source_path)),
  error = function(error) fail("open", error)
)
row$n_cells <- ncol(source_matrix)
row$n_genes <- nrow(source_matrix)
if (row$n_cells != n_cells) {
  fail("open", simpleError("scheduled and source cell counts differ"))
}

query_plan <- tryCatch(
  readRDS(query_plan_path),
  error = function(error) fail("query plan", error)
)
if (
  query_plan$n_cells != ncol(source_matrix) ||
    query_plan$n_genes != nrow(source_matrix)
) {
  fail("query plan", simpleError("frozen query-plan dimensions changed"))
}
row$nnz <- query_plan$nnz
row$query_plan_fingerprint <- query_plan$query_plan_fingerprint

row$export_secs <- tryCatch(
  bench_time(bench_write_full_backend(source_matrix, backend, sibling)),
  error = function(error) fail("build", error)
)
obj <- tryCatch(
  bench_make_full_shell(
    source_matrix,
    backend,
    basename(sibling),
    src_name,
    spec$organism,
    Sys.getenv("BENCH_RUN_ID")
  ),
  error = function(error) fail("shell", error)
)
tryCatch(
  saveRDS(obj, crb, version = 3),
  error = function(error) fail("shell", error)
)

row$crb_mb <- bench_path_mb(crb)
row$sibling_mb <- bench_path_mb(sibling)
row$total_mb <- row$crb_mb + row$sibling_mb
row$rss_mb <- bench_rss_mb()
row$peak_rss_mb <- bench_peak_rss_mb()
heap <- gc()
row$r_peak_mb <- sum(heap[, ncol(heap)], na.rm = TRUE)
bench_append_row(result, row)
bench_msg(
  "OK %s: %.0f x %.0f, %.1f MB in %.1fs",
  backend,
  row$n_genes,
  row$n_cells,
  row$total_mb,
  row$export_secs
)
