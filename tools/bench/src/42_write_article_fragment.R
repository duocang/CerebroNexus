# Render the compact comparison table that the package vignette includes.
#
# Usage: Rscript src/42_write_article_fragment.R <result_dir> <output.md>

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("need <result_dir> <output.md>", call. = FALSE)
}
here <- Sys.getenv("BENCH_ROOT", "")
if (!nzchar(here)) {
  here <- normalizePath("tools/bench")
}
source(file.path(here, "lib", "reporting.R"))

result_dir <- normalizePath(args[1], mustWork = TRUE)
exports <- utils::read.csv(
  file.path(result_dir, "10_export.csv"),
  stringsAsFactors = FALSE
)
access <- utils::read.csv(
  file.path(result_dir, "20_access.csv"),
  stringsAsFactors = FALSE
)
output <- args[2]
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
writeLines(
  bench_article_comparison_lines(exports, access),
  output,
  useBytes = TRUE
)
message("wrote ", output)
