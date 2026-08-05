# Generate article and supplementary figures from a publication-profile run.
#
# Usage: Rscript src/41_draw_figures.R <result_dir> <out_dir>

args <- commandArgs(trailingOnly = TRUE)
here <- Sys.getenv("BENCH_ROOT", "")
if (!nzchar(here)) {
  here <- normalizePath("tools/bench")
}
source(file.path(here, "lib", "protocol.R"))
source(file.path(here, "lib", "reporting.R"))

result_dir <- if (length(args) >= 1L) {
  normalizePath(args[1], mustWork = TRUE)
} else {
  bench_current_result_dir(file.path(here, "result"))
}
out_dir <- if (length(args) >= 2L) args[2] else "vignettes/img"

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})
source(file.path(here, "lib", "figures.R"))

manifest <- utils::read.csv(
  file.path(result_dir, "run_manifest.csv"),
  stringsAsFactors = FALSE
)
manifest_values <- stats::setNames(as.character(manifest$value), manifest$key)
profile <- bench_profile(manifest_values[["profile"]])
bench_require_article_profile(profile)

exports <- utils::read.csv(
  file.path(result_dir, "10_export.csv"),
  stringsAsFactors = FALSE
)
access <- utils::read.csv(
  file.path(result_dir, "20_access.csv"),
  stringsAsFactors = FALSE
)
dpi <- suppressWarnings(as.integer(Sys.getenv("BENCH_FIGURE_DPI", "600")))
if (!is.finite(dpi) || dpi < 72L) {
  dpi <- 600L
}

figures <- bench_build_publication_figures(exports, access, manifest_values)
bench_save_publication_figures(figures, out_dir, dpi = dpi)

message(
  sprintf(
    "wrote %d publication figures in PNG, PDF, and SVG from %s",
    length(figures),
    manifest_values[["run_id"]]
  )
)
