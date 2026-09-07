# Preflight a full-source out-of-core Panel C2 schedule.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4L) {
  stop(
    "need <data_inventory.csv> <run_plan.csv> <run_manifest.csv> <output.csv>",
    call. = FALSE
  )
}
inventory <- utils::read.csv(args[1L], stringsAsFactors = FALSE)
plan <- utils::read.csv(args[2L], stringsAsFactors = FALSE)
manifest <- utils::read.csv(args[3L], stringsAsFactors = FALSE)
output <- args[4L]
values <- stats::setNames(as.character(manifest$value), manifest$key)

free_disk_bytes <- function(path) {
  override <- suppressWarnings(as.numeric(Sys.getenv("BENCH_FREE_DISK_BYTES")))
  if (is.finite(override) && override > 0) {
    return(override)
  }
  lines <- system2("df", c("-Pk", shQuote(path)), stdout = TRUE)
  fields <- strsplit(trimws(tail(lines, 1L)), "[[:space:]]+")[[1L]]
  as.numeric(fields[4L]) * 1024
}

planned <- unique(plan[c("source", "n_cells")])
matched <- match(planned$source, inventory$source)
if (anyNA(matched)) {
  stop("inventory does not cover the full-source plan", call. = FALSE)
}
source <- inventory[matched, , drop = FALSE]
memory_mb <- min(
  as.numeric(values[["memory_mb"]]),
  as.numeric(values[["r_vector_limit_mb"]]),
  na.rm = TRUE
)
disk_free <- free_disk_bytes(dirname(output))
# Only 12 queried rows are materialised. Four GiB covers R, native buffers and
# the full-cell metadata shell with a conservative margin.
estimated_peak_mb <- 4096 + planned$n_cells * 12 * 8 / 2^20
# One cached source and one staged backend coexist; 2.5x source bytes leaves a
# compression-independent margin without pretending both backends coexist.
required_disk <- source$source_bytes * 2.5
assessment <- data.frame(
  source = planned$source,
  n_cells = planned$n_cells,
  estimated_nnz = source$nnz,
  estimated_peak_mb = round(estimated_peak_mb),
  memory_budget_mb = round(memory_mb * 0.70),
  source_bytes = source$source_bytes,
  disk_budget_bytes = disk_free * 0.80,
  memory_ok = estimated_peak_mb <= memory_mb * 0.70,
  index_ok = TRUE,
  disk_ok = required_disk <= disk_free * 0.80,
  stringsAsFactors = FALSE
)
assessment$safe <- with(assessment, memory_ok & index_ok & disk_ok)
assessment$reason <- ifelse(
  assessment$safe,
  "safe out-of-core plan",
  paste0(
    ifelse(assessment$memory_ok, "", "insufficient memory; "),
    ifelse(assessment$disk_ok, "", "insufficient disk")
  )
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(assessment, output, row.names = FALSE)
if (any(!assessment$safe)) {
  stop("unsafe full-source out-of-core plan", call. = FALSE)
}
message("full-source out-of-core resource plan is safe")
