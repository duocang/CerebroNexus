# Prepare one immutable query plan before any timed backend build.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 6L) {
  stop(
    paste(
      "need <source> <n_cells> <scratch> <query_plan>",
      "<result> <query_panel_result>"
    ),
    call. = FALSE
  )
}
src_name <- args[1L]
n_cells <- as.numeric(args[2L])
scratch <- args[3L]
query_plan_path <- args[4L]
result <- args[5L]
query_panel_result <- args[6L]

here <- Sys.getenv("BENCH_ROOT", "")
if (!nzchar(here)) {
  here <- normalizePath("tests/bench")
}
source(file.path(here, "config", "sources.R"))
source(file.path(here, "lib", "remote_h5.R"))
source(file.path(here, "lib", "bench_utils.R"))
source(file.path(here, "lib", "protocol.R"))
source(file.path(here, "lib", "access_metrics.R"))
source(file.path(here, "lib", "full_source.R"))

spec <- BENCH_SOURCES[[src_name]]
if (is.null(spec)) {
  stop("unknown source: ", src_name, call. = FALSE)
}
source_path <- file.path(
  scratch,
  "sources",
  basename(sub("\\?.*$", "", spec$url))
)
if (!file.exists(source_path)) {
  stop("cached benchmark source is missing", call. = FALSE)
}

row <- data.frame(
  run_id = Sys.getenv("BENCH_RUN_ID"),
  profile = Sys.getenv("BENCH_PROFILE"),
  source = src_name,
  n_cells = n_cells,
  n_genes = NA_real_,
  nnz = NA_real_,
  source_prepare_secs = NA_real_,
  query_plan_secs = NA_real_,
  peak_rss_mb = NA_real_,
  query_plan_fingerprint = NA_character_,
  status = "OK",
  stringsAsFactors = FALSE
)

fail <- function(stage, error) {
  row$status <- sprintf("FAILED(%s): %s", stage, conditionMessage(error))
  bench_append_row(result, row)
  bench_msg(
    "query-plan preparation failed at %s: %s",
    stage,
    conditionMessage(error)
  )
  quit(status = 1L)
}

matrix <- NULL
profile <- Sys.getenv("BENCH_PROFILE")
row$source_prepare_secs <- tryCatch(
  bench_time({
    matrix <<- if (identical(profile, "panel_c2")) {
      bench_open_full_source(spec, source_path)
    } else {
      spec$local_path <- source_path
      bench_read_subset(spec, n_cells, n_chunks = 4L, verbose = FALSE)
    }
  }),
  error = function(error) fail("source", error)
)
if (ncol(matrix) != n_cells) {
  fail(
    "source",
    simpleError("prepared source cell count differs from schedule")
  )
}
row$n_genes <- nrow(matrix)
plan <- NULL
row$query_plan_secs <- tryCatch(
  bench_time({
    plan <<- if (identical(profile, "panel_c2")) {
      bench_build_lazy_query_plan(matrix, bench_profile(profile)$query_genes)
    } else {
      bench_build_query_plan(matrix, bench_profile(profile)$query_genes)
    }
  }),
  error = function(error) fail("query plan", error)
)
row$nnz <- plan$nnz
row$query_plan_fingerprint <- plan$query_plan_fingerprint
row$peak_rss_mb <- bench_peak_rss_mb()

dir.create(dirname(query_plan_path), recursive = TRUE, showWarnings = FALSE)
staged <- tempfile("query-plan-", tmpdir = dirname(query_plan_path))
tryCatch(
  saveRDS(plan, staged, version = 3),
  error = function(error) fail("write", error)
)
if (!file.rename(staged, query_plan_path)) {
  unlink(staged)
  fail("write", simpleError("could not publish frozen query plan"))
}
panel_rows <- data.frame(
  run_id = row$run_id,
  profile = row$profile,
  source = row$source,
  n_cells = row$n_cells,
  panel_index = seq_len(nrow(plan$panel)),
  gene = plan$panel$gene,
  nnz = plan$panel$nnz,
  role = plan$panel$role,
  query_plan_fingerprint = plan$query_plan_fingerprint,
  reference_row_fingerprint = plan$reference_row_fingerprint,
  reference_block_fingerprint = plan$reference_block_fingerprint,
  stringsAsFactors = FALSE
)
bench_append_row(query_panel_result, panel_rows)
bench_append_row(result, row)
bench_msg(
  "prepared %s / %.0f cells query plan in %.1fs",
  src_name,
  n_cells,
  row$source_prepare_secs + row$query_plan_secs
)
