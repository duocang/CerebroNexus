# Inspect every configured source over ROS3 and record what it contains.
#
# Usage: Rscript src/01_inspect_data.R <result>
#
# Costs a few HTTP range requests per source and no bulk transfer at all, so it
# is safe to run before committing to a sweep. The `dgc_representable` column is
# the interesting one: a source whose non-zero count exceeds 2^31 - 1 cannot be
# held in a dgCMatrix at full size no matter how much RAM the host has.

args <- commandArgs(trailingOnly = TRUE)
result <- if (length(args) >= 1) args[1] else "tests/bench/result/00_probe.csv"

here <- Sys.getenv("BENCH_ROOT", "")
if (!nzchar(here)) {
  here <- normalizePath("tests/bench")
}
source(file.path(here, "config", "sources.R"))
source(file.path(here, "lib", "remote_h5.R"))
source(file.path(here, "lib", "bench_utils.R"))

for (nm in bench_active_sources()) {
  spec <- BENCH_SOURCES[[nm]]
  bench_msg("probing %s", nm)
  p <- tryCatch(bench_probe(spec), error = function(e) {
    bench_msg("  FAILED: %s", conditionMessage(e))
    NULL
  })
  if (is.null(p)) {
    next
  }
  bench_append_row(
    result,
    data.frame(
      source = nm,
      label = p$label,
      kind = p$kind,
      n_cells = p$n_cells,
      n_genes = p$n_genes,
      nnz = p$nnz,
      nnz_per_cell = round(p$nnz_per_cell, 1),
      source_bytes = spec$expected_bytes,
      dgc_gb_full = round(p$dgc_gb_full, 2),
      dgc_representable = p$dgc_representable,
      tiers = paste(
        format(spec$tiers, scientific = FALSE, trim = TRUE),
        collapse = "|"
      ),
      stringsAsFactors = FALSE
    )
  )
  bench_msg(
    "  %s cells x %s genes, nnz %.4e (%.0f/cell), full dgCMatrix %.1f GB%s",
    format(p$n_cells, big.mark = ","),
    format(p$n_genes, big.mark = ","),
    p$nnz,
    p$nnz_per_cell,
    p$dgc_gb_full,
    if (p$dgc_representable) "" else " -- EXCEEDS the 32-bit index limit"
  )
}
