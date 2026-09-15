# Validate and combine the three frozen publication-full study phases.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) {
  stop("need <result_root> <output_dir>", call. = FALSE)
}
here <- Sys.getenv("BENCH_ROOT", "")
if (!nzchar(here)) {
  here <- normalizePath("tests/bench")
}
source(file.path(here, "lib", "reporting.R"))
source(file.path(here, "lib", "protocol.R"))
source(file.path(here, "config", "sources.R"))
root <- normalizePath(args[1L], mustWork = TRUE)
out_dir <- args[2L]
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

read_manifest <- function(path) {
  bench_manifest_values(utils::read.csv(
    file.path(path, "run_manifest.csv"),
    stringsAsFactors = FALSE
  ))
}

manifest_value <- function(values, key) {
  value <- unname(values[key])
  if (!length(value) || is.na(value)) "" else value
}

validate_run <- function(path, profile) {
  output <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(file.path(here, "src", "30_check_measurements.R"), path),
    stdout = TRUE,
    stderr = TRUE,
    env = c(
      paste0("BENCH_ROOT=", here),
      paste0("BENCH_PROFILE=", profile)
    )
  )
  if (!is.null(attr(output, "status"))) {
    stop(
      "study phase validation failed for ",
      profile,
      ":\n",
      paste(output, collapse = "\n"),
      call. = FALSE
    )
  }
}

validate_phase_schedule <- function(path, phase) {
  columns <- c(
    "profile",
    "source",
    "n_cells",
    "comparison",
    "export_repeat",
    "order_position",
    "backend",
    "access_repeats"
  )
  observed <- utils::read.csv(
    file.path(path, "05_schedule.csv"),
    stringsAsFactors = FALSE
  )
  expected <- switch(
    phase,
    ab = bench_schedule(
      BENCH_SOURCES,
      "publication",
      sources = c("mouse_brain_e18", "human_pfc_hbcc")
    ),
    c1 = bench_panel_c_schedule(BENCH_SOURCES, "c1"),
    c2 = bench_panel_c_schedule(BENCH_SOURCES, "c2")
  )[columns]
  observed <- observed[columns]
  rownames(expected) <- NULL
  rownames(observed) <- NULL
  if (!isTRUE(all.equal(observed, expected, check.attributes = FALSE))) {
    stop(
      "phase ",
      toupper(phase),
      " schedule differs from the protocol",
      call. = FALSE
    )
  }
}

validate_query_protocol <- function(path) {
  access <- utils::read.csv(
    file.path(path, "20_access.csv"),
    stringsAsFactors = FALSE
  )
  if (!"n_hot" %in% names(access) || any(access$n_hot != 33L)) {
    stop("study query-panel size differs from the protocol", call. = FALSE)
  }
}

current_inputs <- list(
  ab = list(root = "phases/ab", path = file.path(root, "ab")),
  c1 = list(root = "phases/c1", path = file.path(root, "c1")),
  c2 = list(root = "phases/c2", path = file.path(root, "c2"))
)
profiles <- c(ab = "publication", c1 = "panel_c1", c2 = "panel_c2")
for (panel in names(current_inputs)) {
  current_inputs[[panel]]$manifest <- read_manifest(
    current_inputs[[panel]]$path
  )
  if (
    !identical(current_inputs[[panel]]$manifest[["profile"]], profiles[[panel]])
  ) {
    stop(panel, " phase has the wrong profile", call. = FALSE)
  }
  validate_run(current_inputs[[panel]]$path, profiles[[panel]])
}
validate_phase_schedule(current_inputs$ab$path, "ab")
validate_phase_schedule(current_inputs$c1$path, "c1")
validate_phase_schedule(current_inputs$c2$path, "c2")
validate_query_protocol(current_inputs$ab$path)
validate_query_protocol(current_inputs$c1$path)
validate_query_protocol(current_inputs$c2$path)

study_ids <- vapply(
  current_inputs,
  function(input) manifest_value(input$manifest, "study_id"),
  character(1)
)
git_shas <- vapply(
  current_inputs,
  function(input) manifest_value(input$manifest, "git_sha"),
  character(1)
)
if (!nzchar(study_ids[1L]) || length(unique(study_ids)) != 1L) {
  stop("study phases do not share one study ID", call. = FALSE)
}
if (length(unique(git_shas)) != 1L) {
  stop("study phases do not share one acquisition Git SHA", call. = FALSE)
}

environment_c1 <- bench_compare_environments(
  current_inputs$ab$manifest,
  current_inputs$c1$manifest
)
environment_c2 <- bench_compare_environments(
  current_inputs$ab$manifest,
  current_inputs$c2$manifest
)
if (!environment_c1$comparable || !environment_c2$comparable) {
  differences <- unique(c(
    environment_c1$different,
    environment_c1$missing,
    environment_c2$different,
    environment_c2$missing
  ))
  stop(
    "study phase environment drift: ",
    paste(differences, collapse = ", "),
    call. = FALSE
  )
}

source_hashes <- lapply(current_inputs, function(input) {
  sources <- utils::read.csv(
    file.path(input$path, "source_manifest.csv"),
    stringsAsFactors = FALSE
  )
  hashes <- stats::setNames(as.character(sources$sha256), sources$source)
  hashes[order(names(hashes))]
})
if (
  !identical(source_hashes$ab, source_hashes$c1) ||
    !identical(source_hashes$ab, source_hashes$c2)
) {
  stop("study source SHA-256 values differ across phases", call. = FALSE)
}
expected_hashes <- vapply(
  BENCH_SOURCES[names(source_hashes$ab)],
  function(spec) {
    if (is.null(spec$expected_sha256)) "" else spec$expected_sha256
  },
  character(1)
)
if (
  any(!nzchar(expected_hashes)) ||
    !identical(unname(source_hashes$ab), unname(expected_hashes))
) {
  stop(
    "study source SHA-256 values differ from the pinned inputs",
    call. = FALSE
  )
}
source_field <- function(spec, key) {
  value <- spec[[key]]
  if (is.null(value)) "" else as.character(value)
}
source_provenance <- do.call(
  rbind,
  lapply(names(source_hashes$ab), function(source) {
    spec <- BENCH_SOURCES[[source]]
    data.frame(
      source = source,
      sha256 = unname(source_hashes$ab[[source]]),
      expected_sha256 = source_field(spec, "expected_sha256"),
      accession = source_field(spec, "accession"),
      dataset_id = source_field(spec, "dataset_id"),
      collection_id = source_field(spec, "collection_id"),
      doi = source_field(spec, "doi"),
      landing_page = source_field(spec, "landing_page"),
      stringsAsFactors = FALSE
    )
  })
)
utils::write.csv(
  source_provenance,
  file.path(out_dir, "source_provenance.csv"),
  row.names = FALSE
)

report_sha <- system2(
  "git",
  c("-C", file.path(here, "..", ".."), "rev-parse", "HEAD"),
  stdout = TRUE
)[1L]
manifest <- do.call(
  rbind,
  lapply(names(current_inputs), function(panel) {
    input <- current_inputs[[panel]]
    data.frame(
      study_id = study_ids[[panel]],
      panel = panel,
      phase_path = input$root,
      run_id = input$manifest[["run_id"]],
      profile = input$manifest[["profile"]],
      acquisition_git_sha = input$manifest[["git_sha"]],
      report_git_sha = report_sha,
      generated_at = manifest_value(input$manifest, "generated_at"),
      cpu = manifest_value(input$manifest, "cpu"),
      r_version = manifest_value(input$manifest, "r_version"),
      r_platform = manifest_value(input$manifest, "r_platform"),
      os = manifest_value(input$manifest, "os"),
      memory_mb = manifest_value(input$manifest, "memory_mb"),
      r_vector_limit_mb = manifest_value(input$manifest, "r_vector_limit_mb"),
      benchmark_threads = manifest_value(input$manifest, "benchmark_threads"),
      scratch_df = manifest_value(input$manifest, "scratch_df"),
      storage_description = manifest_value(
        input$manifest,
        "storage_description"
      ),
      package_version = manifest_value(input$manifest, "package_version"),
      package_Matrix = manifest_value(input$manifest, "package_Matrix"),
      package_rhdf5 = manifest_value(input$manifest, "package_rhdf5"),
      package_Seurat = manifest_value(input$manifest, "package_Seurat"),
      package_SeuratObject = manifest_value(
        input$manifest,
        "package_SeuratObject"
      ),
      package_BPCells = manifest_value(input$manifest, "package_BPCells"),
      package_HDF5Array = manifest_value(input$manifest, "package_HDF5Array"),
      stringsAsFactors = FALSE
    )
  })
)
utils::write.csv(
  manifest,
  file.path(out_dir, "study_manifest.csv"),
  row.names = FALSE
)

environment_table <- data.frame(
  comparison = c("A/B versus C1", "A/B versus C2"),
  comparable = c(environment_c1$comparable, environment_c2$comparable),
  missing_fields = c(
    paste(environment_c1$missing, collapse = ";"),
    paste(environment_c2$missing, collapse = ";")
  ),
  different_fields = c(
    paste(environment_c1$different, collapse = ";"),
    paste(environment_c2$different, collapse = ";")
  ),
  stringsAsFactors = FALSE
)

query_plan_metrics <- do.call(
  rbind,
  lapply(names(current_inputs), function(panel) {
    rows <- utils::read.csv(
      file.path(current_inputs[[panel]]$path, "query_plan_manifest.csv"),
      stringsAsFactors = FALSE
    )
    data.frame(panel = panel, rows, stringsAsFactors = FALSE)
  })
)
query_panel <- do.call(
  rbind,
  lapply(names(current_inputs), function(panel) {
    rows <- utils::read.csv(
      file.path(current_inputs[[panel]]$path, "query_panel.csv"),
      stringsAsFactors = FALSE
    )
    data.frame(panel = panel, rows, stringsAsFactors = FALSE)
  })
)
utils::write.csv(
  query_plan_metrics,
  file.path(out_dir, "query_plan_metrics.csv"),
  row.names = FALSE
)
utils::write.csv(
  query_panel,
  file.path(out_dir, "query_panel.csv"),
  row.names = FALSE
)
utils::write.csv(
  environment_table,
  file.path(out_dir, "environment_comparison.csv"),
  row.names = FALSE
)

summarise_phase <- function(data, panel, phase, metrics, units) {
  grouped <- bench_summarise_metrics(
    data,
    group = c("source", "n_cells", "backend"),
    metrics = metrics
  )
  do.call(
    rbind,
    lapply(metrics, function(metric) {
      data.frame(
        panel = panel,
        phase = phase,
        grouped[c("source", "n_cells", "backend")],
        metric = metric,
        unit = units[[metric]],
        median = grouped[[paste0(metric, "_median")]],
        minimum = grouped[[paste0(metric, "_min")]],
        maximum = grouped[[paste0(metric, "_max")]],
        n = grouped[[paste0(metric, "_n")]],
        stringsAsFactors = FALSE
      )
    })
  )
}

export_metrics <- c("export_secs", "total_mb", "peak_rss_mb")
export_units <- c(
  export_secs = "seconds",
  total_mb = "MiB",
  peak_rss_mb = "MiB"
)
access_metrics <- c(
  "load_secs",
  "attach_secs",
  "rss_mb",
  "peak_rss_mb",
  "hot_p50_secs",
  "block_secs"
)
access_units <- c(
  load_secs = "seconds",
  attach_secs = "seconds",
  rss_mb = "MiB",
  peak_rss_mb = "MiB",
  hot_p50_secs = "seconds",
  block_secs = "seconds"
)

metrics <- do.call(
  rbind,
  lapply(names(current_inputs), function(panel) {
    path <- current_inputs[[panel]]$path
    exports <- utils::read.csv(
      file.path(path, "10_export.csv"),
      stringsAsFactors = FALSE
    )
    access <- utils::read.csv(
      file.path(path, "20_access.csv"),
      stringsAsFactors = FALSE
    )
    rbind(
      summarise_phase(exports, panel, "build", export_metrics, export_units),
      summarise_phase(access, panel, "access", access_metrics, access_units)
    )
  })
)
utils::write.csv(
  metrics,
  file.path(out_dir, "combined_metrics.csv"),
  row.names = FALSE
)

ratios <- do.call(
  rbind,
  lapply(split(metrics, metrics$panel), function(panel) {
    reference <- if (identical(panel$panel[1L], "c2")) "bpcells" else "embedded"
    groups <- split(
      panel,
      interaction(panel$phase, panel$metric, drop = TRUE)
    )
    do.call(
      rbind,
      lapply(groups, function(metric) {
        names(metric)[names(metric) == "median"] <- "value_median"
        got <- bench_backend_ratios(metric, "value_median", reference)
        got[c(
          "panel",
          "phase",
          "source",
          "n_cells",
          "metric",
          "backend",
          "reference_backend",
          "ratio"
        )]
      })
    )
  })
)
utils::write.csv(
  ratios,
  file.path(out_dir, "backend_ratios.csv"),
  row.names = FALSE
)

correctness <- do.call(
  rbind,
  lapply(names(current_inputs), function(panel) {
    access <- utils::read.csv(
      file.path(current_inputs[[panel]]$path, "20_access.csv"),
      stringsAsFactors = FALSE
    )
    data.frame(
      panel = panel,
      passed = sum(access$status == "OK" & access$correctness == "OK"),
      total = nrow(access),
      all_passed = all(access$status == "OK" & access$correctness == "OK"),
      stringsAsFactors = FALSE
    )
  })
)
utils::write.csv(
  correctness,
  file.path(out_dir, "correctness.csv"),
  row.names = FALSE
)

metric_rows <- vapply(
  seq_len(nrow(metrics)),
  function(i) {
    row <- metrics[i, ]
    value <- bench_format_interval(
      row$median,
      row$minimum,
      row$maximum,
      row$n
    )
    sprintf(
      "| %s | %s | %s | %.0f | %s | %s | %s %s |",
      toupper(row$panel),
      row$phase,
      row$source,
      row$n_cells,
      row$backend,
      row$metric,
      value,
      row$unit
    )
  },
  character(1)
)
ratio_rows <- vapply(
  seq_len(nrow(ratios)),
  function(i) {
    row <- ratios[i, ]
    sprintf(
      "| %s | %s | %s | %.0f | %s | %s | %.3fx |",
      toupper(row$panel),
      row$phase,
      row$source,
      row$n_cells,
      row$metric,
      paste0(row$backend, " / ", row$reference_backend),
      row$ratio
    )
  },
  character(1)
)
summary <- c(
  "# Publication-full expression-backend benchmark",
  "",
  sprintf("**Study:** `%s`  ", study_ids[1L]),
  sprintf("**Acquisition Git SHA:** `%s`  ", git_shas[1L]),
  sprintf("**Report Git SHA:** `%s`", report_sha),
  "",
  "A/B compares all three backends at 50k and 150k cells. C1 extends that",
  "same-environment comparison to 400k mouse and 300k human cells. C2 tests",
  "BPCells and H5 on both complete sources.",
  "Embedded C2 is not representable by `dgCMatrix` and was not attempted.",
  "",
  "All three phases share the recorded code, CPU, OS, R/dependency versions,",
  "thread count, storage description, and source SHA-256 values.",
  "Query plans were prepared in separate processes before timed builds;",
  "preparation measurements are retained in `query_plan_metrics.csv`, and",
  "the exact ordered gene workloads are retained in `query_panel.csv`.",
  "Results are descriptive medians and observed ranges from independent processes;",
  "no significance test or cross-machine generalisation is claimed.",
  "",
  "## Sources",
  "",
  "Stable accessions, dataset/collection IDs, DOI, landing pages, and the exact",
  "acquired SHA-256 values are recorded in `source_provenance.csv`.",
  "",
  "## Correctness",
  "",
  "| panel | passed access processes | total | all passed |",
  "|---|---:|---:|:---:|",
  sprintf(
    "| %s | %d | %d | %s |",
    toupper(correctness$panel),
    correctness$passed,
    correctness$total,
    ifelse(correctness$all_passed, "yes", "no")
  ),
  "",
  "## Absolute results",
  "",
  "| panel | phase | source | cells | backend | metric | median [range], n unit |",
  "|---|---|---|---:|---|---|---|",
  metric_rows,
  "",
  "## Within-tier backend ratios",
  "",
  "A ratio below 1 means the named backend used less time, space, or memory",
  "than the reference for that source, tier, phase, and metric.",
  "",
  "| panel | phase | source | cells | metric | comparison | ratio |",
  "|---|---|---|---:|---|---|---:|",
  ratio_rows,
  "",
  "See `figures/expression_backend_benchmark_publication_full.png` for the combined view."
)
writeLines(summary, file.path(out_dir, "summary.md"), useBytes = TRUE)
message("validated and wrote publication-full study report")
