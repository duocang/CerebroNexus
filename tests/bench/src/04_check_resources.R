# Refuse a benchmark plan that does not fit the current host.
#
# Usage: Rscript src/04_check_resources.R \
#   <data_inventory.csv> <run_plan.csv> <run_manifest.csv> <output.csv>

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4L) {
  stop(
    "need <data_inventory.csv> <run_plan.csv> <run_manifest.csv> <output.csv>",
    call. = FALSE
  )
}
inventory_path <- args[1]
plan_path <- args[2]
manifest_path <- args[3]
output_path <- args[4]
here <- Sys.getenv("BENCH_ROOT", "")
if (!nzchar(here)) {
  here <- normalizePath("tests/bench")
}
source(file.path(here, "lib", "resource_planning.R"))

free_disk_bytes <- function(path) {
  override <- suppressWarnings(as.numeric(Sys.getenv("BENCH_FREE_DISK_BYTES")))
  if (is.finite(override) && override > 0) {
    return(override)
  }
  lines <- system2("df", c("-Pk", shQuote(path)), stdout = TRUE)
  fields <- strsplit(trimws(tail(lines, 1L)), "[[:space:]]+")[[1]]
  available_kb <- suppressWarnings(as.numeric(fields[4]))
  if (!is.finite(available_kb)) {
    stop("could not determine free disk space", call. = FALSE)
  }
  available_kb * 1024
}

inventory <- utils::read.csv(inventory_path, stringsAsFactors = FALSE)
plan <- utils::read.csv(plan_path, stringsAsFactors = FALSE)
manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
manifest_values <- stats::setNames(as.character(manifest$value), manifest$key)
memory_mb <- suppressWarnings(as.numeric(manifest_values[["memory_mb"]]))
vector_limit_mb <- suppressWarnings(
  as.numeric(manifest_values[["r_vector_limit_mb"]])
)
if (!is.finite(memory_mb) || !is.finite(vector_limit_mb)) {
  stop("run manifest has no usable memory limits", call. = FALSE)
}

assessment <- bench_assess_resources(
  inventory,
  plan,
  memory_mb = memory_mb,
  vector_limit_mb = vector_limit_mb,
  free_disk_bytes = free_disk_bytes(dirname(output_path))
)
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(assessment, output_path, row.names = FALSE)

for (i in seq_len(nrow(assessment))) {
  message(sprintf(
    "%s @ %s cells: %s (estimated %.0f MB; budget %.0f MB)",
    assessment$source[i],
    format(assessment$n_cells[i], big.mark = ",", scientific = FALSE),
    toupper(if (assessment$safe[i]) "safe" else "unsafe"),
    assessment$estimated_peak_mb[i],
    assessment$memory_budget_mb[i]
  ))
}
bench_require_safe_plan(assessment)
