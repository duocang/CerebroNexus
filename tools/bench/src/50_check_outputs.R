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
required <- "summary.md"
if (identical(profile, "publication")) {
  required <- c(
    required,
    file.path("figures", "expression_backend_benchmark_overview.png"),
    file.path(
      "figures",
      "expression_backend_benchmark_capacity_estimate.png"
    )
  )
}
paths <- file.path(stage, required)
missing <- !file.exists(paths) |
  is.na(file.info(paths)$size) |
  file.info(paths)$size <= 0
if (any(missing)) {
  stop("missing staged output: ", required[missing][1], call. = FALSE)
}
message("validated complete staged benchmark outputs")
