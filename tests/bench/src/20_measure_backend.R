# Measure hydrated startup, memory, and query latency for one exported .crb.
#
# Usage: Rscript src/20_measure_backend.R <source> <n_cells> <backend> \
#   <export_repeat> <order_position> <access_repeat> <crb> <result> <query_plan>
#
# Runs in its own process so the resident-set reading describes this backend
# only. Reads go through getExpressionRow() / getExpressionBlock() and the
# reads use the public readCerebro() and expression getter paths.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 9) {
  stop(
    paste(
      "need <source> <n_cells> <backend> <export_repeat> <order_position>",
      "<access_repeat> <crb> <result> <query_plan>"
    )
  )
}
src_name <- args[1]
n_cells <- as.numeric(args[2])
backend <- args[3]
export_repeat <- as.integer(args[4])
order_position <- as.integer(args[5])
access_repeat <- as.integer(args[6])
crb <- args[7]
result <- args[8]
query_plan_path <- args[9]

here <- Sys.getenv("BENCH_ROOT", "")
if (!nzchar(here)) {
  here <- normalizePath("tests/bench")
}
source(file.path(here, "lib", "bench_utils.R"))
source(file.path(here, "lib", "protocol.R"))
source(file.path(here, "lib", "access_metrics.R"))
if (nzchar(Sys.getenv("BENCH_LIB"))) {
  .libPaths(c(Sys.getenv("BENCH_LIB"), .libPaths()))
}
suppressPackageStartupMessages({
  library(CerebroNexus)
  library(Matrix)
})

row <- data.frame(
  run_id = Sys.getenv("BENCH_RUN_ID"),
  profile = Sys.getenv("BENCH_PROFILE", "quick"),
  source = src_name,
  n_cells = n_cells,
  backend = backend,
  export_repeat = export_repeat,
  order_position = order_position,
  access_repeat = access_repeat,
  status = "OK",
  startup_secs = NA_real_,
  rss_mb = NA_real_,
  peak_rss_mb = NA_real_,
  first_query_secs = NA_real_,
  hot_p50_secs = NA_real_,
  hot_p95_secs = NA_real_,
  block_secs = NA_real_,
  subset_row_secs = NA_real_,
  subset_block_secs = NA_real_,
  subset_n_cells = NA_real_,
  n_hot = NA_integer_,
  correctness = NA_character_,
  row_fingerprint = NA_character_,
  reference_row_fingerprint = NA_character_,
  block_fingerprint = NA_character_,
  reference_block_fingerprint = NA_character_,
  subset_row_fingerprint = NA_character_,
  reference_subset_row_fingerprint = NA_character_,
  subset_block_fingerprint = NA_character_,
  reference_subset_block_fingerprint = NA_character_,
  query_plan_fingerprint = NA_character_,
  stringsAsFactors = FALSE
)

fail <- function(stage, e) {
  row$status <- sprintf("FAILED(%s): %s", stage, conditionMessage(e))
  bench_append_row(result, row)
  bench_msg("FAILED at %s: %s", stage, conditionMessage(e))
  quit(status = 0)
}

obj <- NULL
query_plan <- tryCatch(
  readRDS(query_plan_path),
  error = function(e) fail("query plan", e)
)
row$startup_secs <- tryCatch(
  bench_time(obj <<- readCerebro(crb)),
  error = function(e) fail("startup", e)
)
row$rss_mb <- bench_rss_mb()

metrics <- tryCatch(
  bench_measure_backend(
    obj,
    query_plan,
    hot_iterations = bench_profile(Sys.getenv(
      "BENCH_PROFILE",
      "quick"
    ))$hot_iterations
  ),
  error = function(e) fail("correctness/access", e)
)
row$first_query_secs <- metrics$first_query_secs
row$hot_p50_secs <- metrics$hot_p50_secs
row$hot_p95_secs <- metrics$hot_p95_secs
row$block_secs <- metrics$block_secs
row$subset_row_secs <- metrics$subset_row_secs
row$subset_block_secs <- metrics$subset_block_secs
row$subset_n_cells <- metrics$subset_n_cells
row$n_hot <- metrics$n_hot
row$correctness <- metrics$correctness
row$row_fingerprint <- metrics$row_fingerprint
row$reference_row_fingerprint <- metrics$reference_row_fingerprint
row$block_fingerprint <- metrics$block_fingerprint
row$reference_block_fingerprint <- metrics$reference_block_fingerprint
row$subset_row_fingerprint <- metrics$subset_row_fingerprint
row$reference_subset_row_fingerprint <-
  metrics$reference_subset_row_fingerprint
row$subset_block_fingerprint <- metrics$subset_block_fingerprint
row$reference_subset_block_fingerprint <-
  metrics$reference_subset_block_fingerprint
row$query_plan_fingerprint <- metrics$query_plan_fingerprint
row$peak_rss_mb <- bench_peak_rss_mb()

bench_append_row(result, row)
bench_msg(
  "%s: hydrated startup %.2fs, rss %.0f MB, hot p50 %.4fs",
  backend,
  row$startup_secs,
  row$rss_mb,
  row$hot_p50_secs
)
