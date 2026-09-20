# Check the complete staged evidence package before immutable publication.
#
# Usage: Rscript src/50_check_outputs.R <stage_dir>

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("need <stage_dir>", call. = FALSE)
}
stage <- normalizePath(args[1], mustWork = TRUE)
manifest_path <- file.path(stage, "run_manifest.csv")
if (!file.exists(manifest_path)) {
  stop("missing staged output: run_manifest.csv", call. = FALSE)
}
manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
values <- stats::setNames(as.character(manifest$value), manifest$key)
profile <- values[["profile"]]
required <- c(
  "00_probe.csv", "05_schedule.csv", "10_export.csv", "20_access.csv",
  "crashes.csv", "query_panel.csv", "query_plan_manifest.csv",
  "resource_check.csv", "run_manifest.csv", "source_manifest.csv",
  "summary.md", "evidence_manifest.csv"
)
if (identical(profile, "publication")) {
  required <- c(
    required,
    file.path("figures", "expression_backend_benchmark_overview.png"),
    file.path("figures", "expression_backend_benchmark_ceiling.png")
  )
}
if (identical(profile, "panel_c2")) {
  required <- c(
    required,
    file.path("figures", "expression_backend_benchmark_overview.png")
  )
}
paths <- file.path(stage, required)
missing <- !file.exists(paths) |
  is.na(file.info(paths)$size) |
  file.info(paths)$size <= 0
if (any(missing)) {
  stop("missing staged output: ", required[missing][1], call. = FALSE)
}

inventory <- utils::read.csv(
  file.path(stage, "evidence_manifest.csv"),
  stringsAsFactors = FALSE
)
if (
  !identical(names(inventory), c("path", "bytes", "md5")) ||
    !nrow(inventory) || anyDuplicated(inventory$path) ||
    any(!grepl("^[0-9a-f]{32}$", inventory$md5)) ||
    any(!is.finite(inventory$bytes) | inventory$bytes < 0)
) {
  stop("evidence_manifest.csv is invalid", call. = FALSE)
}
inventoried_paths <- file.path(stage, inventory$path)
if (any(!file.exists(inventoried_paths))) {
  stop("evidence inventory names a missing file", call. = FALSE)
}
actual_md5 <- unname(tools::md5sum(inventoried_paths))
actual_bytes <- as.numeric(file.info(inventoried_paths)$size)
if (any(actual_md5 != inventory$md5) || any(actual_bytes != inventory$bytes)) {
  stop("evidence inventory does not match staged files", call. = FALSE)
}
required_inventory <- setdiff(gsub("\\\\", "/", required), "evidence_manifest.csv")
if (!all(required_inventory %in% inventory$path)) {
  stop("evidence inventory does not cover all required outputs", call. = FALSE)
}
message("validated complete staged benchmark outputs")
