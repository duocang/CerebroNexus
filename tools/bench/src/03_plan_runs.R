# Write the deterministic benchmark schedule.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("need <result.csv>", call. = FALSE)
}
result <- args[1]
tsv_result <- if (length(args) >= 2L) args[2] else NULL
here <- Sys.getenv("BENCH_ROOT", "")
if (!nzchar(here)) {
  here <- normalizePath("tools/bench")
}
source(file.path(here, "config", "sources.R"))
source(file.path(here, "lib", "protocol.R"))

profile <- bench_profile(Sys.getenv("BENCH_PROFILE", "quick"))
schedule <- bench_schedule(
  BENCH_SOURCES,
  profile,
  sources = bench_active_sources()
)
dir.create(dirname(result), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(schedule, result, row.names = FALSE)
if (!is.null(tsv_result)) {
  utils::write.table(
    schedule,
    tsv_result,
    sep = "\t",
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE
  )
}
message(
  sprintf(
    "wrote %d scheduled export cells for the %s profile to %s",
    nrow(schedule),
    profile$name,
    result
  )
)
