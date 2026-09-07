# Combine immutable A/B, C1 and C2 benchmark evidence.

args <- commandArgs(trailingOnly = TRUE)
here <- Sys.getenv("BENCH_ROOT", "")
if (!nzchar(here)) {
  here <- normalizePath("tests/bench")
}
source(file.path(here, "lib", "reporting.R"))
root <- if (length(args)) args[1L] else file.path(here, "result")

baseline <- bench_current_result_dir(root)
c1 <- bench_current_result_dir(file.path(root, "panel-c1"))
c2 <- bench_current_result_dir(file.path(root, "panel-c2"))
bench_validate_panel_c_baseline(baseline)

read_values <- function(path) {
  manifest <- utils::read.csv(
    file.path(path, "run_manifest.csv"),
    stringsAsFactors = FALSE
  )
  stats::setNames(as.character(manifest$value), manifest$key)
}
inputs <- list(
  ab = read_values(baseline),
  c1 = read_values(c1),
  c2 = read_values(c2)
)
if (!identical(inputs$c1[["profile"]], "panel_c1")) {
  stop("Panel C1 CURRENT does not point to a panel_c1 run", call. = FALSE)
}
if (!identical(inputs$c2[["profile"]], "panel_c2")) {
  stop("Panel C2 CURRENT does not point to a panel_c2 run", call. = FALSE)
}
out_dir <- file.path(root, "panel-c")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
manifest <- do.call(
  rbind,
  lapply(names(inputs), function(panel) {
    data.frame(
      panel = panel,
      run_id = inputs[[panel]][["run_id"]],
      git_sha = inputs[[panel]][["git_sha"]],
      stringsAsFactors = FALSE
    )
  })
)
utils::write.csv(
  manifest,
  file.path(out_dir, "manifest.csv"),
  row.names = FALSE
)

count_rows <- function(path, file) {
  nrow(utils::read.csv(file.path(path, file), stringsAsFactors = FALSE))
}
summary <- c(
  "# Incremental Panel C benchmark",
  "",
  "Panel A/B remains immutable. Panel C1 extends all three backends to the",
  "largest representable tiers; Panel C2 tests BPCells and H5 on both complete",
  "sources without constructing a full `dgCMatrix`.",
  "",
  "| panel | run | builds | access processes |",
  "|---|---|---:|---:|",
  sprintf(
    "| A/B | `%s` | %d | %d |",
    inputs$ab[["run_id"]],
    count_rows(baseline, "10_export.csv"),
    count_rows(baseline, "20_access.csv")
  ),
  sprintf(
    "| C1 | `%s` | %d | %d |",
    inputs$c1[["run_id"]],
    count_rows(c1, "10_export.csv"),
    count_rows(c1, "20_access.csv")
  ),
  sprintf(
    "| C2 | `%s` | %d | %d |",
    inputs$c2[["run_id"]],
    count_rows(c2, "10_export.csv"),
    count_rows(c2, "20_access.csv")
  ),
  "",
  "See `figures/expression_backend_benchmark_panel_c.png` for the combined view."
)
writeLines(summary, file.path(out_dir, "summary.md"), useBytes = TRUE)
message("wrote combined Panel C manifest and summary")
