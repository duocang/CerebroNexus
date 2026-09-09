bench_protocol <- file.path("..", "bench", "lib", "protocol.R")
bench_reporting <- file.path("..", "bench", "lib", "reporting.R")
bench_viewer <- file.path("..", "bench", "lib", "viewer_validation.R")
bench_root <- normalizePath(file.path("..", "bench"), mustWork = FALSE)

skip_unless_bench_reporting <- function() {
  testthat::skip_if_not(
    file.exists(bench_protocol) && file.exists(bench_reporting),
    "benchmark tree not present (expected when checking a built package)"
  )
}

test_that("C2 viewer schedule selects one build per source and backend", {
  skip_unless_bench_reporting()
  source(bench_protocol, local = TRUE)
  source(file.path(bench_root, "config", "sources.R"), local = TRUE)
  source(bench_viewer, local = TRUE)

  schedule <- bench_viewer_schedule(
    bench_panel_c_schedule(BENCH_SOURCES, "c2")
  )

  expect_equal(nrow(schedule), 4L)
  expect_true(all(schedule$export_repeat == 1L))
  expect_setequal(
    unique(schedule$source),
    c("mouse_brain_e18", "human_pfc_hbcc")
  )
  expect_setequal(unique(schedule$backend), c("bpcells", "h5"))
})

test_that("C2 viewer results must cover the successful schedule", {
  skip_unless_bench_reporting()
  source(bench_protocol, local = TRUE)
  source(file.path(bench_root, "config", "sources.R"), local = TRUE)
  source(bench_viewer, local = TRUE)

  schedule <- bench_viewer_schedule(
    bench_panel_c_schedule(BENCH_SOURCES, "c2")
  )
  rows <- transform(
    schedule,
    run_id = "run-1",
    gene = "gene_1",
    browser = "Chrome fixture",
    status = "OK",
    correctness = "OK",
    bundle_secs = 1,
    launch_secs = 2,
    hover_secs = 0.1,
    selection_secs = 0.2,
    zoom_secs = 0.1,
    gene_secs = 1
  )

  expect_silent(bench_validate_viewer_results(schedule, rows, "run-1"))
  expect_error(
    bench_validate_viewer_results(schedule, rows[-1L, ], "run-1"),
    "does not cover"
  )
  failed <- rows
  failed$status[1L] <- "FAILED(hover): no tooltip"
  expect_error(
    bench_validate_viewer_results(schedule, failed, "run-1"),
    "failed"
  )
  invalid <- rows
  invalid$gene_secs[1L] <- NA_real_
  expect_error(
    bench_validate_viewer_results(schedule, invalid, "run-1"),
    "timings"
  )
})

test_that("metric summaries retain independent-repeat uncertainty", {
  skip_unless_bench_reporting()
  source(bench_protocol, local = TRUE)
  source(bench_reporting, local = TRUE)

  x <- data.frame(
    source = rep("fixture", 6),
    n_cells = rep(1000, 6),
    backend = rep(c("embedded", "h5"), each = 3),
    status = "OK",
    seconds = c(3, 1, 2, 0.3, 0.1, 0.2),
    rss_mb = c(30, 10, 20, 6, 4, 5)
  )
  got <- bench_summarise_metrics(
    x,
    group = c("source", "n_cells", "backend"),
    metrics = c("seconds", "rss_mb")
  )

  embedded <- got[got$backend == "embedded", ]
  expect_equal(embedded$seconds_median, 2)
  expect_equal(embedded$seconds_min, 1)
  expect_equal(embedded$seconds_max, 3)
  expect_equal(embedded$seconds_n, 3L)
  expect_equal(embedded$rss_mb_median, 20)
})

test_that("metric summaries ignore failed rows but not missing values", {
  skip_unless_bench_reporting()
  source(bench_reporting, local = TRUE)

  x <- data.frame(
    backend = c("h5", "h5", "h5"),
    status = c("OK", "OK", "FAILED(query)"),
    seconds = c(1, NA, 100)
  )
  got <- bench_summarise_metrics(x, "backend", "seconds")
  expect_equal(got$seconds_median, 1)
  expect_equal(got$seconds_n, 1L)
  expect_equal(got$rows_n, 2L)
})

test_that("interval formatting exposes range and sample count", {
  skip_unless_bench_reporting()
  source(bench_reporting, local = TRUE)

  expect_equal(
    bench_format_interval(2, 1, 3, 3, digits = 1),
    "2.0 [1.0-3.0], n=3"
  )
  expect_equal(
    bench_format_interval(NA, NA, NA, 0, digits = 1),
    "--"
  )
})

test_that("evidence labels prevent quick runs from sounding definitive", {
  skip_unless_bench_reporting()
  source(bench_protocol, local = TRUE)
  source(bench_reporting, local = TRUE)

  expect_match(
    bench_evidence_notice(bench_profile("quick")),
    "Exploratory"
  )
  expect_match(
    bench_evidence_notice(bench_profile("publication")),
    "Publication-profile"
  )
})

test_that("current result resolution is safe and backward compatible", {
  skip_unless_bench_reporting()
  source(bench_reporting, local = TRUE)

  root <- tempfile("bench-result-root-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  expect_equal(bench_current_result_dir(root), normalizePath(root))

  dir.create(file.path(root, "runs", "run-1"), recursive = TRUE)
  writeLines("run-1", file.path(root, "CURRENT"))
  expect_equal(
    bench_current_result_dir(root),
    normalizePath(file.path(root, "runs", "run-1"))
  )

  writeLines("../escape", file.path(root, "CURRENT"))
  expect_error(bench_current_result_dir(root), "unsafe CURRENT")
})

test_that("report and plots consume repeated publication rows", {
  skip_unless_bench_reporting()
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("patchwork")

  result_dir <- tempfile("bench-report-fixture-")
  out_dir <- tempfile("bench-report-plots-")
  dir.create(result_dir)
  dir.create(out_dir)
  on.exit(unlink(c(result_dir, out_dir), recursive = TRUE), add = TRUE)

  backends <- c("embedded", "bpcells", "h5")
  exports <- expand.grid(
    backend = backends,
    export_repeat = 1:3,
    stringsAsFactors = FALSE
  )
  exports$run_id <- "test-run"
  exports$profile <- "publication"
  exports$source <- "fixture"
  exports$label <- "fixture"
  exports$n_cells <- 1000
  exports$n_genes <- 100
  exports$nnz <- 10000
  exports$order_position <- rep(1:3, 3)
  exports$status <- "OK"
  exports$read_secs <- 1
  exports$seurat_secs <- 1
  exports$export_secs <- rep(c(3, 1, 2), 3)
  exports$crb_mb <- 1
  exports$sibling_mb <- 1
  exports$total_mb <- rep(c(3, 1, 2), 3)
  exports$rss_mb <- 10
  exports$r_peak_mb <- 20
  exports$peak_rss_mb <- 24
  exports$query_plan_fingerprint <- "plan"
  utils::write.csv(
    exports,
    file.path(result_dir, "10_export.csv"),
    row.names = FALSE
  )

  access <- exports[
    rep(seq_len(nrow(exports)), each = 2),
    c(
      "run_id",
      "profile",
      "source",
      "n_cells",
      "backend",
      "export_repeat",
      "order_position"
    )
  ]
  access$access_repeat <- rep(1:2, nrow(exports))
  access$status <- "OK"
  access$load_secs <- 0.1
  access$attach_secs <- 0.2
  access$rss_mb <- rep(c(30, 10, 20), 6)
  access$peak_rss_mb <- access$rss_mb + 5
  access$first_query_secs <- 0.03
  access$hot_p50_secs <- rep(c(0.03, 0.01, 0.02), 6)
  access$hot_p95_secs <- access$hot_p50_secs * 1.2
  access$block_secs <- rep(c(0.3, 0.1, 0.2), 6)
  access$n_hot <- 10L
  access$correctness <- "OK"
  access$row_fingerprint <- access$reference_row_fingerprint <- "row"
  access$block_fingerprint <- access$reference_block_fingerprint <- "block"
  access$query_plan_fingerprint <- "plan"
  utils::write.csv(
    access,
    file.path(result_dir, "20_access.csv"),
    row.names = FALSE
  )

  utils::write.csv(
    data.frame(
      label = "fixture",
      n_cells = 1000,
      n_genes = 100,
      nnz = 10000,
      nnz_per_cell = 10,
      dgc_gb_full = 0.001,
      dgc_representable = TRUE
    ),
    file.path(result_dir, "00_probe.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(
      run_id = character(),
      profile = character(),
      source = character(),
      n_cells = numeric(),
      backend = character(),
      export_repeat = integer(),
      order_position = integer(),
      stage = character(),
      exit_code = integer()
    ),
    file.path(result_dir, "crashes.csv"),
    row.names = FALSE
  )
  manifest <- c(
    run_id = "test-run",
    profile = "publication",
    git_sha = paste(rep("a", 40), collapse = ""),
    generated_at = "2026-08-04",
    git_branch = "test",
    git_dirty = "false",
    package_version = "3.2.0",
    r_version = R.version.string,
    os = "test",
    cpu = "test",
    logical_cores = "1",
    memory_mb = "1024",
    r_vector_limit_mb = "32768"
  )
  utils::write.csv(
    data.frame(key = names(manifest), value = unname(manifest)),
    file.path(result_dir, "run_manifest.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(
      run_id = "test-run",
      source = "fixture",
      url = "https://example.test",
      bytes = 123,
      sha256 = paste(rep("b", 64), collapse = "")
    ),
    file.path(result_dir, "source_manifest.csv"),
    row.names = FALSE
  )

  env <- paste0("BENCH_ROOT=", normalizePath(file.path("..", "bench")))
  report_status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      file.path(bench_root, "src", "40_write_report.R"),
      result_dir
    ),
    stdout = TRUE,
    stderr = TRUE,
    env = env
  )
  expect_null(
    attr(report_status, "status"),
    info = paste(report_status, collapse = "\n")
  )
  report <- readLines(file.path(result_dir, "summary.md"), warn = FALSE)
  expect_true(any(grepl("Publication-profile evidence", report, fixed = TRUE)))
  expect_true(any(grepl("n=3", report, fixed = TRUE)))
  expect_true(any(grepl("peak process RSS MB", report, fixed = TRUE)))

  plot_status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      file.path(bench_root, "src", "41_draw_figures.R"),
      result_dir,
      out_dir
    ),
    stdout = TRUE,
    stderr = TRUE,
    env = env
  )
  expect_null(
    attr(plot_status, "status"),
    info = paste(plot_status, collapse = "\n")
  )
  expect_true(file.exists(file.path(
    out_dir,
    "expression_backend_benchmark_overview.png"
  )))
  expect_true(file.exists(file.path(
    out_dir,
    "expression_backend_benchmark_ceiling.png"
  )))
})

test_that("publication figure labels distinct Viewer workloads", {
  skip_unless_bench_reporting()
  script <- readLines(
    file.path(bench_root, "src", "41_draw_figures.R"),
    warn = FALSE
  )
  source <- paste(script, collapse = "\n")

  expect_match(source, "Interactive single-gene latency", fixed = TRUE)
  expect_match(source, "Marker-panel block latency", fixed = TRUE)
  expect_match(source, 'plot_annotation(tag_levels = "A")', fixed = TRUE)
})

test_that("publication-full report scripts are present", {
  skip_unless_bench_reporting()
  expect_true(file.exists(file.path(
    bench_root,
    "src",
    "42_write_panel_c_report.R"
  )))
  expect_true(file.exists(file.path(
    bench_root,
    "src",
    "43_draw_panel_c_figure.R"
  )))
})

test_that("study environment comparison rejects incompatible phases", {
  skip_unless_bench_reporting()
  source(bench_reporting, local = TRUE)

  baseline <- c(
    r_version = "R 4.6.1",
    r_platform = "x86_64-pc-linux-gnu",
    cpu = "host-a",
    benchmark_threads = "1",
    package_Matrix = "1.7-4"
  )
  same <- baseline
  changed <- baseline
  changed[["cpu"]] <- "host-b"

  expect_true(
    bench_compare_environments(
      baseline,
      same,
      keys = names(baseline)
    )$comparable
  )
  comparison <- bench_compare_environments(
    baseline,
    changed,
    keys = names(baseline)
  )
  expect_false(comparison$comparable)
  expect_equal(comparison$different, "cpu")
})

test_that("frozen run paths cannot escape their result roots", {
  skip_unless_bench_reporting()
  source(bench_reporting, local = TRUE)

  root <- tempfile("bench-frozen-root-")
  dir.create(file.path(root, "runs", "run-1"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  expect_equal(
    bench_result_run_dir(root, "run-1"),
    normalizePath(file.path(root, "runs", "run-1"))
  )
  expect_error(bench_result_run_dir(root, "../escape"), "unsafe run id")
  expect_error(bench_result_run_dir(root, "missing"), "does not exist")
})

test_that("backend ratios retain direction and matched tiers", {
  skip_unless_bench_reporting()
  source(bench_reporting, local = TRUE)

  summary <- data.frame(
    source = rep("fixture", 3),
    n_cells = rep(1000, 3),
    backend = c("embedded", "bpcells", "h5"),
    seconds_median = c(4, 2, 1),
    stringsAsFactors = FALSE
  )
  got <- bench_backend_ratios(
    summary,
    metric = "seconds_median",
    reference = "embedded"
  )

  expect_equal(got$ratio[got$backend == "bpcells"], 0.5)
  expect_equal(got$ratio[got$backend == "h5"], 0.25)
  expect_true(all(got$reference_backend == "embedded"))

  summary$seconds_median[summary$backend == "embedded"] <- 0
  expect_equal(
    nrow(bench_backend_ratios(summary, "seconds_median", "embedded")),
    0L
  )
})

test_that("publication-full report and figure use one frozen study", {
  skip_unless_bench_reporting()
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("patchwork")
  source(bench_protocol, local = TRUE)
  source(file.path(bench_root, "config", "sources.R"), local = TRUE)
  source(bench_viewer, local = TRUE)

  root <- tempfile("publication-full-root-")
  out <- tempfile("publication-full-output-")
  drift_out <- tempfile("publication-full-drift-")
  dir.create(root)
  dir.create(out)
  on.exit(unlink(c(root, out, drift_out), recursive = TRUE), add = TRUE)

  source_hashes <- vapply(
    BENCH_SOURCES[c("mouse_brain_e18", "human_pfc_hbcc")],
    `[[`,
    character(1),
    "expected_sha256"
  )
  make_phase <- function(phase) {
    profile <- c(ab = "publication", c1 = "panel_c1", c2 = "panel_c2")[[phase]]
    run_id <- paste0("fixture-study-", phase)
    run_dir <- file.path(root, phase)
    dir.create(run_dir)
    schedule <- switch(
      phase,
      ab = bench_schedule(
        BENCH_SOURCES,
        "publication",
        sources = c("mouse_brain_e18", "human_pfc_hbcc")
      ),
      c1 = bench_panel_c_schedule(BENCH_SOURCES, "c1"),
      c2 = bench_panel_c_schedule(BENCH_SOURCES, "c2")
    )
    tier_key <- paste(schedule$source, schedule$n_cells, sep = "-")
    exports <- transform(
      schedule,
      run_id = run_id,
      status = "OK",
      export_secs = seq_len(nrow(schedule)),
      total_mb = seq_len(nrow(schedule)) * 10,
      peak_rss_mb = seq_len(nrow(schedule)) * 20,
      query_plan_fingerprint = tier_key
    )
    access <- do.call(
      rbind,
      lapply(seq_len(nrow(schedule)), function(i) {
        repeats <- schedule$access_repeats[i]
        data.frame(
          run_id = run_id,
          profile = profile,
          source = schedule$source[i],
          n_cells = schedule$n_cells[i],
          backend = schedule$backend[i],
          export_repeat = schedule$export_repeat[i],
          access_repeat = seq_len(repeats),
          status = "OK",
          load_secs = seq_len(repeats) / 10,
          attach_secs = seq_len(repeats) / 20,
          rss_mb = 100 + seq_len(repeats),
          peak_rss_mb = 110 + seq_len(repeats),
          hot_p50_secs = seq_len(repeats) / 100,
          block_secs = seq_len(repeats) / 50,
          n_hot = 33L,
          correctness = "OK",
          row_fingerprint = "row",
          reference_row_fingerprint = "row",
          block_fingerprint = "block",
          reference_block_fingerprint = "block",
          query_plan_fingerprint = tier_key[i],
          stringsAsFactors = FALSE
        )
      })
    )
    preparation <- unique(schedule[c("source", "n_cells")])
    preparation <- transform(
      preparation,
      run_id = run_id,
      profile = profile,
      n_genes = 100,
      nnz = n_cells * 10,
      source_prepare_secs = 1,
      query_plan_secs = 2,
      peak_rss_mb = 100,
      query_plan_fingerprint = paste(source, n_cells, sep = "-"),
      status = "OK"
    )
    query_panel <- do.call(
      rbind,
      lapply(seq_len(nrow(preparation)), function(i) {
        data.frame(
          run_id = run_id,
          profile = profile,
          source = preparation$source[i],
          n_cells = preparation$n_cells[i],
          panel_index = seq_len(12L),
          gene = paste0("gene_", seq_len(12L)),
          nnz = seq_len(12L),
          role = c("first", rep("hot", 11L)),
          query_plan_fingerprint = preparation$query_plan_fingerprint[i],
          reference_row_fingerprint = "row",
          reference_block_fingerprint = "block",
          stringsAsFactors = FALSE
        )
      })
    )
    manifest <- c(
      study_id = "fixture-study",
      run_id = run_id,
      profile = profile,
      generated_at = "2026-09-08T12:00:00+0000",
      git_sha = paste(rep("c", 40), collapse = ""),
      git_dirty = "false",
      r_version = "R 4.6.1",
      r_platform = "x86_64-pc-linux-gnu",
      os = "Linux fixture",
      cpu = "fixture CPU",
      benchmark_threads = "1",
      storage_description = "local NVMe ext4",
      scratch_df = "fixture",
      package_version = "5.2.0",
      package_Matrix = "1.7-4",
      package_rhdf5 = "2.52.1",
      package_Seurat = "5.3.1",
      package_SeuratObject = "5.2.0",
      package_BPCells = "0.3.1",
      package_HDF5Array = "1.36.0",
      package_CerebroNexus = "5.2.0",
      memory_mb = "131072",
      r_vector_limit_mb = "131072"
    )
    manifest <- data.frame(
      key = names(manifest),
      value = unname(manifest),
      stringsAsFactors = FALSE
    )
    sources <- data.frame(
      run_id = run_id,
      source = names(source_hashes),
      url = paste0("https://example.test/", names(source_hashes)),
      bytes = c(1000, 2000),
      sha256 = unname(source_hashes),
      stringsAsFactors = FALSE
    )
    resource <- unique(schedule[c("source", "n_cells")])
    resource$safe <- TRUE
    resource$reason <- "safe"
    crashes <- data.frame(
      run_id = character(),
      profile = character(),
      source = character(),
      n_cells = numeric(),
      backend = character(),
      export_repeat = integer(),
      order_position = integer(),
      stage = character(),
      exit_code = integer(),
      stringsAsFactors = FALSE
    )
    viewer <- if (identical(phase, "c2")) {
      transform(
        bench_viewer_schedule(schedule),
        run_id = run_id,
        gene = "gene_1",
        browser = "Chrome fixture",
        status = "OK",
        correctness = "OK",
        bundle_secs = 1,
        launch_secs = 2,
        hover_secs = .1,
        selection_secs = .2,
        zoom_secs = .1,
        gene_secs = 1
      )
    }
    files <- list(
      "05_schedule.csv" = schedule,
      "10_export.csv" = exports,
      "20_access.csv" = access,
      "query_plan_manifest.csv" = preparation,
      "query_panel.csv" = query_panel,
      "run_manifest.csv" = manifest,
      "source_manifest.csv" = sources,
      "resource_check.csv" = resource,
      "crashes.csv" = crashes
    )
    if (identical(phase, "c2")) {
      files[["21_viewer.csv"]] <- viewer
    }
    for (name in names(files)) {
      utils::write.csv(
        files[[name]],
        file.path(run_dir, name),
        row.names = FALSE
      )
    }
  }
  make_phase("ab")
  make_phase("c1")
  make_phase("c2")

  env <- paste0("BENCH_ROOT=", normalizePath(bench_root))
  report <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(file.path(bench_root, "src", "42_write_panel_c_report.R"), root, out),
    stdout = TRUE,
    stderr = TRUE,
    env = env
  )
  expect_null(attr(report, "status"), info = paste(report, collapse = "\n"))
  expect_true(all(file.exists(file.path(
    out,
    c(
      "study_manifest.csv",
      "environment_comparison.csv",
      "query_plan_metrics.csv",
      "query_panel.csv",
      "combined_metrics.csv",
      "backend_ratios.csv",
      "correctness.csv",
      "viewer_metrics.csv",
      "source_provenance.csv",
      "summary.md"
    )
  ))))
  query_panel <- utils::read.csv(
    file.path(out, "query_panel.csv"),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(query_panel), 96L)
  correctness <- utils::read.csv(
    file.path(out, "correctness.csv"),
    stringsAsFactors = FALSE
  )
  expect_equal(correctness$total, c(72L, 36L, 24L))
  expect_true(all(correctness$all_passed))
  viewer <- utils::read.csv(
    file.path(out, "viewer_metrics.csv"),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(viewer), 4L)
  expect_true(all(viewer$status == "OK" & viewer$correctness == "OK"))
  summary <- readLines(file.path(out, "summary.md"), warn = FALSE)
  expect_true(any(grepl("single-run diagnostics", summary, fixed = TRUE)))

  figure <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(file.path(bench_root, "src", "43_draw_panel_c_figure.R"), root, out),
    stdout = TRUE,
    stderr = TRUE,
    env = env
  )
  expect_null(attr(figure, "status"), info = paste(figure, collapse = "\n"))
  expect_true(file.exists(file.path(
    out,
    "figures",
    "expression_backend_benchmark_publication_full.png"
  )))

  viewer_path <- file.path(root, "c2", "21_viewer.csv")
  viewer <- utils::read.csv(viewer_path, stringsAsFactors = FALSE)
  utils::write.csv(viewer[-1L, ], viewer_path, row.names = FALSE)
  missing_viewer <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      file.path(bench_root, "src", "42_write_panel_c_report.R"),
      root,
      tempfile()
    ),
    stdout = TRUE,
    stderr = TRUE,
    env = env
  ))
  expect_false(is.null(attr(missing_viewer, "status")))
  expect_match(paste(missing_viewer, collapse = "\n"), "does not cover")

  viewer$status[1L] <- "FAILED(hover): no tooltip"
  utils::write.csv(viewer, viewer_path, row.names = FALSE)
  failed_viewer <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      file.path(bench_root, "src", "42_write_panel_c_report.R"),
      root,
      tempfile()
    ),
    stdout = TRUE,
    stderr = TRUE,
    env = env
  ))
  expect_false(is.null(attr(failed_viewer, "status")))
  expect_match(
    paste(failed_viewer, collapse = "\n"),
    "Viewer validations failed"
  )
  viewer$status[1L] <- "OK"
  utils::write.csv(viewer, viewer_path, row.names = FALSE)

  manifest_path <- file.path(root, "c2", "run_manifest.csv")
  manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
  manifest$value[manifest$key == "cpu"] <- "different CPU"
  utils::write.csv(manifest, manifest_path, row.names = FALSE)
  drift <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      file.path(bench_root, "src", "42_write_panel_c_report.R"),
      root,
      drift_out
    ),
    stdout = TRUE,
    stderr = TRUE,
    env = env
  ))
  expect_false(is.null(attr(drift, "status")))
  expect_match(paste(drift, collapse = "\n"), "environment drift")

  manifest$value[manifest$key == "cpu"] <- "fixture CPU"
  utils::write.csv(manifest, manifest_path, row.names = FALSE)
  for (phase in c("ab", "c1", "c2")) {
    path <- file.path(root, phase, "source_manifest.csv")
    sources <- utils::read.csv(path, stringsAsFactors = FALSE)
    sources$sha256[sources$source == "mouse_brain_e18"] <- paste(
      rep("0", 64),
      collapse = ""
    )
    utils::write.csv(sources, path, row.names = FALSE)
  }
  hash_out <- tempfile("publication-full-hash-")
  on.exit(unlink(hash_out, recursive = TRUE), add = TRUE)
  hash_drift <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      file.path(bench_root, "src", "42_write_panel_c_report.R"),
      root,
      hash_out
    ),
    stdout = TRUE,
    stderr = TRUE,
    env = env
  ))
  expect_false(is.null(attr(hash_drift, "status")))
  expect_match(paste(hash_drift, collapse = "\n"), "pinned inputs")
})
