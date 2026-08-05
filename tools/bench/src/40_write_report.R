# Generate an uncertainty-aware Markdown report from a staged or published run.
#
# Usage: Rscript src/40_write_report.R <result_dir>

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

read_if <- function(name) {
  path <- file.path(result_dir, name)
  if (file.exists(path)) {
    utils::read.csv(path, stringsAsFactors = FALSE)
  } else {
    NULL
  }
}

probe <- read_if("00_probe.csv")
exports <- read_if("10_export.csv")
access <- read_if("20_access.csv")
crashes <- read_if("crashes.csv")
manifest <- read_if("run_manifest.csv")
source_manifest <- read_if("source_manifest.csv")

if (is.null(manifest)) {
  stop(
    "run_manifest.csv is required for a scientific benchmark report",
    call. = FALSE
  )
}
manifest_values <- stats::setNames(as.character(manifest$value), manifest$key)
profile <- bench_profile(manifest_values[["profile"]])

interval <- function(row, metric, digits = 2L) {
  bench_format_interval(
    row[[paste0(metric, "_median")]],
    row[[paste0(metric, "_min")]],
    row[[paste0(metric, "_max")]],
    row[[paste0(metric, "_n")]],
    digits = digits
  )
}

out <- c(
  "# Expression-backend benchmark on real public datasets",
  "",
  sprintf("**Run:** `%s`  ", manifest_values[["run_id"]]),
  sprintf("**Profile:** `%s`  ", profile$name),
  sprintf("**Git:** `%s`", manifest_values[["git_sha"]]),
  "",
  paste0("> **Evidence status.** ", bench_evidence_notice(profile)),
  ""
)

if (!is.null(probe) && nrow(probe)) {
  out <- c(
    out,
    "## Sources",
    "",
    "| source | cells | genes | nnz | nnz/cell | full dgCMatrix | representable |",
    "|---|---:|---:|---:|---:|---:|:--:|"
  )
  for (i in seq_len(nrow(probe))) {
    out <- c(
      out,
      sprintf(
        "| %s | %s | %s | %.3e | %.0f | %.1f GB | %s |",
        probe$label[i],
        format(probe$n_cells[i], big.mark = ","),
        format(probe$n_genes[i], big.mark = ","),
        probe$nnz[i],
        probe$nnz_per_cell[i],
        probe$dgc_gb_full[i],
        if (probe$dgc_representable[i]) "yes" else "**no**"
      )
    )
  }
  out <- c(out, "")
}

if (!is.null(exports) && nrow(exports)) {
  exports$r_peak_mb[exports$r_peak_mb > 4e6] <- NA_real_
  export_summary <- bench_summarise_metrics(
    exports,
    group = c("source", "n_cells", "backend"),
    metrics = c("crb_mb", "sibling_mb", "total_mb", "export_secs", "r_peak_mb")
  )
  export_summary <- export_summary[
    order(
      export_summary$source,
      export_summary$n_cells,
      export_summary$backend
    ),
    ,
    drop = FALSE
  ]
  out <- c(
    out,
    "## Export",
    "",
    "Values are median [minimum-maximum], followed by the number of independent export processes.",
    "",
    "| source | cells | backend | total MB | export seconds | peak R heap MB |",
    "|---|---:|---|---:|---:|---:|"
  )
  for (i in seq_len(nrow(export_summary))) {
    row <- export_summary[i, , drop = FALSE]
    out <- c(
      out,
      sprintf(
        "| %s | %s | %s | %s | %s | %s |",
        row$source,
        format(row$n_cells, big.mark = ","),
        row$backend,
        interval(row, "total_mb", 1L),
        interval(row, "export_secs", 1L),
        interval(row, "r_peak_mb", 0L)
      )
    )
  }
  out <- c(out, "")

  failed <- exports[exports$status != "OK", , drop = FALSE]
  if (nrow(failed)) {
    out <- c(
      out,
      "### Caught export failures",
      "",
      "| source | cells | backend | repeat | status |",
      "|---|---:|---|---:|---|"
    )
    for (i in seq_len(nrow(failed))) {
      out <- c(
        out,
        sprintf(
          "| %s | %s | %s | %d | %s |",
          failed$source[i],
          format(failed$n_cells[i], big.mark = ","),
          failed$backend[i],
          failed$export_repeat[i],
          substr(failed$status[i], 1L, 100L)
        )
      )
    }
    out <- c(out, "")
  }
}

if (!is.null(access) && nrow(access)) {
  access <- bench_normalize_access_metrics(access)
  access$backend_ready_secs <- access$load_secs + access$attach_secs
  access$first_gene_ready_secs <-
    access$backend_ready_secs + access$first_query_secs
  access_summary <- bench_summarise_metrics(
    access,
    group = c("source", "n_cells", "backend"),
    metrics = c(
      "backend_ready_secs",
      "first_gene_ready_secs",
      "rss_mb",
      "first_query_secs",
      "hot_p50_secs",
      "hot_p95_secs",
      "block_prepare_secs",
      "block_ready_secs"
    )
  )
  access_summary <- access_summary[
    order(
      access_summary$source,
      access_summary$n_cells,
      access_summary$backend
    ),
    ,
    drop = FALSE
  ]
  out <- c(
    out,
    "## Runtime access",
    "",
    paste0(
      "Backend ready is CRB load plus backend attachment. First gene ready adds ",
      "the first backend getter call in that fresh R process. This server-side ",
      "readiness proxy excludes the Shiny handshake and browser rendering. The ",
      "operating-system file cache is uncontrolled, so it is not a cold-disk measurement."
    ),
    ""
  )
  if (any(access$block_timing_contract == "legacy_prepare_only")) {
    out <- c(
      out,
      paste0(
        "Legacy preparation-only block timings are excluded from ",
        "materialized-read conclusions. Re-run the publication profile to ",
        "populate the materialized 12-gene metric."
      ),
      ""
    )
  }
  out <- c(
    out,
    "| source | cells | backend | backend ready s | first gene ready s | RSS MB | first query s | warmed p50 s | warmed p95 s | block prepare s | materialized block ready s |",
    "|---|---:|---|---:|---:|---:|---:|---:|---:|---:|---:|"
  )
  for (i in seq_len(nrow(access_summary))) {
    row <- access_summary[i, , drop = FALSE]
    out <- c(
      out,
      sprintf(
        "| %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |",
        row$source,
        format(row$n_cells, big.mark = ","),
        row$backend,
        interval(row, "backend_ready_secs", 2L),
        interval(row, "first_gene_ready_secs", 2L),
        interval(row, "rss_mb", 0L),
        interval(row, "first_query_secs", 4L),
        interval(row, "hot_p50_secs", 4L),
        interval(row, "hot_p95_secs", 4L),
        interval(row, "block_prepare_secs", 3L),
        interval(row, "block_ready_secs", 3L)
      )
    )
  }
  out <- c(
    out,
    "",
    sprintf(
      "Correctness: %d/%d access processes matched both source-matrix fingerprints.",
      sum(access$correctness == "OK", na.rm = TRUE),
      nrow(access)
    ),
    ""
  )
}

if (!is.null(exports) && nrow(exports)) {
  usable <- exports[
    exports$status == "OK" & is.finite(exports$r_peak_mb) & !is.na(exports$nnz),
    ,
    drop = FALSE
  ]
  if (nrow(usable)) {
    ceiling_points <- bench_summarise_metrics(
      usable,
      group = c("source", "n_cells", "nnz"),
      metrics = "r_peak_mb"
    )
    ceiling_points$bytes_per_nnz <-
      ceiling_points$r_peak_mb_median * 2^20 / ceiling_points$nnz
    estimate <- stats::median(ceiling_points$bytes_per_nnz)
    out <- c(
      out,
      "## Observed export memory scaling",
      "",
      sprintf(
        paste0(
          "Across %d distinct source/tier points, the median observed peak was ",
          "%.1f bytes per non-zero (range %.1f-%.1f). This is a descriptive ",
          "relationship for this exporter and host, not a universal memory law."
        ),
        nrow(ceiling_points),
        estimate,
        min(ceiling_points$bytes_per_nnz),
        max(ceiling_points$bytes_per_nnz)
      ),
      paste0(
        "No maximum capacity is inferred from successful runs. A stopping ",
        "boundary requires a separately recorded stress profile with observed ",
        "failures; physical RAM, the R vector limit, and the `dgCMatrix` ",
        "32-bit index limit remain separate constraints."
      ),
      ""
    )
  }
}

if (!is.null(crashes) && nrow(crashes)) {
  out <- c(
    out,
    "## Processes killed outright",
    "",
    "| source | cells | backend | repeat | stage | exit |",
    "|---|---:|---|---:|---|---:|"
  )
  for (i in seq_len(nrow(crashes))) {
    out <- c(
      out,
      sprintf(
        "| %s | %s | %s | %d | %s | %d |",
        crashes$source[i],
        format(crashes$n_cells[i], big.mark = ","),
        crashes$backend[i],
        crashes$export_repeat[i],
        crashes$stage[i],
        crashes$exit_code[i]
      )
    )
  }
  out <- c(out, "")
}

out <- c(out, "## Provenance", "")
provenance_keys <- c(
  "generated_at",
  "git_branch",
  "git_dirty",
  "repository_version",
  "package_CerebroNexus",
  "r_version",
  "os",
  "cpu",
  "logical_cores",
  "memory_mb",
  "r_vector_limit_mb"
)
out <- c(out, "| key | value |", "|---|---|")
for (key in provenance_keys[provenance_keys %in% names(manifest_values)]) {
  out <- c(out, sprintf("| %s | %s |", key, manifest_values[[key]]))
}
if (!is.null(source_manifest)) {
  for (i in seq_len(nrow(source_manifest))) {
    out <- c(
      out,
      sprintf(
        "| source `%s` | %s bytes; SHA-256 `%s` |",
        source_manifest$source[i],
        format(source_manifest$bytes[i], big.mark = ","),
        source_manifest$sha256[i]
      )
    )
  }
}
out <- c(out, "")

path <- file.path(result_dir, "summary.md")
writeLines(out, path, useBytes = TRUE)
cat(paste(out, collapse = "\n"), "\n")
message("\nwrote ", path)
