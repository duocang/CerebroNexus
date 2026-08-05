# Measure load / attach / memory / query latency for one exported .crb.
#
# Usage: Rscript src/20_measure_backend.R <source> <n_cells> <backend> \
#   <export_repeat> <order_position> <access_repeat> <crb> <result> <query_plan>
#
# Runs in its own process so the resident-set reading describes this backend
# only. Reads go through getExpressionRow() / getExpressionBlock() and the
# attach goes through .attachExternalExpression() - the same functions the Shiny
# server calls - so the numbers reflect the runtime path rather than raw matrix
# indexing.

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
  here <- normalizePath("tools/bench")
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

# .attachExternalExpression() is app-side code, not exported by the package.
utils_path <- file.path(
  system.file(package = "CerebroNexus"),
  "viewer",
  "utility_functions.R"
)
if (!file.exists(utils_path)) {
  stop("cannot locate utility_functions.R")
}
source(utils_path)

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
  load_secs = NA_real_,
  attach_secs = NA_real_,
  rss_mb = NA_real_,
  first_query_secs = NA_real_,
  hot_p50_secs = NA_real_,
  hot_p95_secs = NA_real_,
  block_prepare_secs = NA_real_,
  block_materialize_secs = NA_real_,
  block_ready_secs = NA_real_,
  n_hot = NA_integer_,
  correctness = NA_character_,
  row_fingerprint = NA_character_,
  reference_row_fingerprint = NA_character_,
  block_fingerprint = NA_character_,
  reference_block_fingerprint = NA_character_,
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
row$load_secs <- tryCatch(
  bench_time(obj <<- readRDS(crb)),
  error = function(e) fail("load", e)
)
row$attach_secs <- tryCatch(
  bench_time(obj <<- .attachExternalExpression(obj, crb)),
  error = function(e) fail("attach", e)
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
row$block_prepare_secs <- metrics$block_prepare_secs
row$block_materialize_secs <- metrics$block_materialize_secs
row$block_ready_secs <- metrics$block_ready_secs
row$n_hot <- metrics$n_hot
row$correctness <- metrics$correctness
row$row_fingerprint <- metrics$row_fingerprint
row$reference_row_fingerprint <- metrics$reference_row_fingerprint
row$block_fingerprint <- metrics$block_fingerprint
row$reference_block_fingerprint <- metrics$reference_block_fingerprint
row$query_plan_fingerprint <- metrics$query_plan_fingerprint

bench_append_row(result, row)
bench_msg(
  "%s: startup %.2fs (load %.2f + attach %.2f), rss %.0f MB, hot p50 %.4fs",
  backend,
  row$load_secs + row$attach_secs,
  row$load_secs,
  row$attach_secs,
  row$rss_mb,
  row$hot_p50_secs
)
