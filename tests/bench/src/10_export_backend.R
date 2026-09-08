# Export one (source, tier, backend) cell of the grid.
#
# Usage: Rscript src/10_export_backend.R <source> <n_cells> <backend> \
#   <export_repeat> <order_position> <scratch> <result> <query_plan>
#
# Deliberately one process per grid cell rather than one per tier: the embedded
# backend is expected to be killed by the OS (or to hit the 32-bit dgCMatrix
# index limit) at the larger tiers, and that must not take the streaming
# backends of the same tier down with it. Each process writes its own row, so an
# aborted run keeps everything already measured.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 8) {
  stop(
    paste(
      "need <source> <n_cells> <backend> <export_repeat>",
      "<order_position> <scratch> <result> <query_plan>"
    )
  )
}
src_name <- args[1]
n_cells <- as.numeric(args[2])
backend <- args[3]
export_repeat <- as.integer(args[4])
order_position <- as.integer(args[5])
scratch <- args[6]
result <- args[7]
query_plan_path <- args[8]

here <- Sys.getenv("BENCH_ROOT", "")
if (!nzchar(here)) {
  here <- normalizePath("tests/bench")
}
source(file.path(here, "config", "sources.R"))
source(file.path(here, "lib", "remote_h5.R"))
source(file.path(here, "lib", "bench_utils.R"))
source(file.path(here, "lib", "make_seurat.R"))
source(file.path(here, "lib", "protocol.R"))
source(file.path(here, "lib", "access_metrics.R"))
# BENCH_LIB holds the branch under test, installed into the scratch directory so
# the numbers describe this worktree rather than whatever version happens to be
# in the user's global library.
if (nzchar(Sys.getenv("BENCH_LIB"))) {
  .libPaths(c(Sys.getenv("BENCH_LIB"), .libPaths()))
}
suppressPackageStartupMessages(library(CerebroNexus))

spec <- BENCH_SOURCES[[src_name]]
if (is.null(spec)) {
  stop("unknown source: ", src_name)
}
cached <- file.path(scratch, "sources", basename(sub("\\?.*$", "", spec$url)))
if (file.exists(cached)) {
  spec$local_path <- cached
}

out_dir <- file.path(
  scratch,
  "export",
  sprintf("%s_%.0f_%s_r%d", src_name, n_cells, backend, export_repeat)
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
crb <- file.path(out_dir, "bench.crb")

row <- data.frame(
  run_id = Sys.getenv("BENCH_RUN_ID"),
  profile = Sys.getenv("BENCH_PROFILE", "quick"),
  source = src_name,
  label = spec$label,
  n_cells = n_cells,
  n_genes = NA_real_,
  nnz = NA_real_,
  backend = backend,
  export_repeat = export_repeat,
  order_position = order_position,
  status = "OK",
  read_secs = NA_real_,
  seurat_secs = NA_real_,
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

fail <- function(stage, e) {
  row$status <- sprintf("FAILED(%s): %s", stage, conditionMessage(e))
  bench_append_row(result, row)
  bench_msg("FAILED at %s: %s", stage, conditionMessage(e))
  quit(status = 0) # the failure IS the measurement; do not fail the sweep
}

gc(reset = TRUE)
bench_msg("%s / %.0f cells / %s: reading", src_name, n_cells, backend)

m <- NULL
row$read_secs <- tryCatch(
  bench_time(
    m <<- bench_read_subset(spec, n_cells, n_chunks = 4, verbose = TRUE)
  ),
  error = function(e) fail("read", e)
)
row$n_genes <- nrow(m)
row$nnz <- length(m@x)
bench_msg("read done: %d x %d, nnz %.3e", nrow(m), ncol(m), length(m@x))

query_plan <- tryCatch(
  readRDS(query_plan_path),
  error = function(e) fail("query plan", e)
)
if (query_plan$n_cells != ncol(m) || query_plan$n_genes != nrow(m)) {
  fail("query plan", simpleError("frozen query-plan dimensions changed"))
}
row$query_plan_fingerprint <- query_plan$query_plan_fingerprint

obj <- NULL
row$seurat_secs <- tryCatch(
  bench_time(obj <<- bench_make_seurat(m)),
  error = function(e) fail("seurat", e)
)
rm(m)
gc(verbose = FALSE)

bench_msg("exporting %s", backend)
row$export_secs <- tryCatch(
  bench_time(CerebroNexus::exportFromSeurat(
    object = obj,
    assay = "RNA",
    slot = "counts",
    file = crb,
    experiment_name = sprintf("%s_%.0f", src_name, n_cells),
    organism = spec$organism,
    groups = c("sample", "cluster"),
    nUMI = "nUMI",
    nGene = "nGene",
    expression_matrix_mode = backend,
    verbose = FALSE
  )),
  error = function(e) fail("export", e)
)

sibling <- switch(
  backend,
  bpcells = sub("\\.crb$", ".bpcells", crb),
  h5 = sub("\\.crb$", ".h5", crb),
  NULL
)
row$crb_mb <- bench_path_mb(crb)
row$sibling_mb <- bench_path_mb(sibling)
row$total_mb <- sum(c(row$crb_mb, row$sibling_mb), na.rm = TRUE)
row$rss_mb <- bench_rss_mb()
row$peak_rss_mb <- bench_peak_rss_mb()
# gc() alternates (count, Mb) columns and the count named "max used" is NOT the
# figure wanted; the Mb that follows it is. The column index is not fixed
# either: an R with a vector memory limit set inserts a "limit (Mb)" column, so
# the layout is 7 wide here and 6 wide elsewhere. The peak in MB is always the
# last column.
g <- gc()
row$r_peak_mb <- sum(g[, ncol(g)], na.rm = TRUE)

bench_append_row(result, row)
bench_msg(
  "OK %s: total %.1f MB in %.1fs (peak R heap %.0f MB)",
  backend,
  row$total_mb,
  row$export_secs,
  row$r_peak_mb
)
