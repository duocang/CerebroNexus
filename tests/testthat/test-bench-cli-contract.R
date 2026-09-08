bench_root <- normalizePath(
  file.path("..", "bench"),
  mustWork = FALSE
)

skip_unless_bench_cli <- function() {
  testthat::skip_if_not(
    dir.exists(bench_root),
    "benchmark tree not present (expected when checking a built package)"
  )
}

test_that("sweep cleanup cannot run twice through signal and exit traps", {
  skip_unless_bench_cli()
  sweep <- readLines(file.path(bench_root, "run_sweep.sh"), warn = FALSE)
  cleanup <- sweep[seq(
    grep("^cleanup\\(\\)", sweep),
    grep("^trap cleanup", sweep) - 1L
  )]

  expect_true(any(grepl("trap - EXIT INT TERM", cleanup, fixed = TRUE)))
})

test_that("sweep stages use plain names in a safe publication order", {
  skip_unless_bench_cli()
  expected <- c(
    "01_inspect_data.R",
    "02_record_environment.R",
    "03_plan_runs.R",
    "04_check_resources.R",
    "10_export_backend.R",
    "20_measure_backend.R",
    "30_check_measurements.R",
    "40_write_report.R",
    "41_draw_figures.R",
    "50_check_outputs.R",
    "60_publish_results.R"
  )
  expect_true(all(file.exists(file.path(bench_root, "src", expected))))

  sweep <- readLines(file.path(bench_root, "run_sweep.sh"), warn = FALSE)
  positions <- vapply(
    expected,
    function(name) {
      hit <- grep(name, sweep, fixed = TRUE)
      if (length(hit)) hit[1] else Inf
    },
    numeric(1)
  )
  expect_true(all(is.finite(positions)))
  expect_true(all(diff(positions) > 0))
})

test_that("sweep supports clean external results and verified source caching", {
  skip_unless_bench_cli()
  sweep <- readLines(file.path(bench_root, "run_sweep.sh"), warn = FALSE)
  source_cache <- file.path(bench_root, "lib", "source_cache.sh")
  cache_helper <- if (file.exists(source_cache)) {
    readLines(source_cache, warn = FALSE)
  } else {
    character()
  }

  expect_true(any(grepl("BENCH_RESULT_ROOT", sweep, fixed = TRUE)))
  expect_true(any(grepl("BENCH_SOURCE_CACHE", cache_helper, fixed = TRUE)))
  expect_true(file.exists(source_cache))
})

test_that("sweep isolates benchmark R processes from user startup files", {
  skip_unless_bench_cli()
  sweep <- paste(
    readLines(file.path(bench_root, "run_sweep.sh"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(sweep, "R_ENVIRON_USER=/dev/null", fixed = TRUE)
  expect_match(sweep, "R_PROFILE_USER=/dev/null", fixed = TRUE)
  expect_match(sweep, 'R_LIBS_USER="$SCRATCH/r-user-library"', fixed = TRUE)
})

test_that("Panel C2 build CLI uses only the lazy full-source path", {
  skip_unless_bench_cli()
  script <- file.path(bench_root, "src", "11_build_full_backend.R")

  expect_true(file.exists(script))
  if (!file.exists(script)) {
    return()
  }
  body <- paste(readLines(script, warn = FALSE), collapse = "\n")
  expect_match(body, "bench_open_full_source", fixed = TRUE)
  expect_match(body, "bench_write_full_backend", fixed = TRUE)
  expect_match(body, "bench_make_full_shell", fixed = TRUE)
  expect_match(body, "readRDS(query_plan_path)", fixed = TRUE)
  expect_false(grepl("bench_build_lazy_query_plan", body, fixed = TRUE))
  expect_false(grepl("dgCMatrix", body, fixed = TRUE))
  expect_false(grepl("as.matrix(source_matrix)", body, fixed = TRUE))
})

test_that("query plans are prepared before any timed backend build", {
  skip_unless_bench_cli()
  prepare <- file.path(bench_root, "src", "05_prepare_query_plan.R")
  expect_true(file.exists(prepare))
  if (!file.exists(prepare)) {
    return()
  }

  prepare_body <- paste(readLines(prepare, warn = FALSE), collapse = "\n")
  sampled_body <- paste(
    readLines(
      file.path(bench_root, "src", "10_export_backend.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  sweep <- readLines(file.path(bench_root, "run_sweep.sh"), warn = FALSE)

  expect_match(prepare_body, "bench_build_query_plan", fixed = TRUE)
  expect_match(prepare_body, "bench_build_lazy_query_plan", fixed = TRUE)
  expect_match(prepare_body, "query_panel_result", fixed = TRUE)
  expect_false(grepl("\\bmatrix\\s*<<-", prepare_body, perl = TRUE))
  expect_match(sampled_body, "readRDS(query_plan_path)", fixed = TRUE)
  expect_false(grepl("bench_build_query_plan", sampled_body, fixed = TRUE))
  expect_lt(
    grep("05_prepare_query_plan.R", sweep, fixed = TRUE)[1L],
    grep('"$BENCH_ROOT/src/$BUILD_SCRIPT"', sweep, fixed = TRUE)[1L]
  )
})

test_that("publication-full wrapper owns all three study phases", {
  skip_unless_bench_cli()
  wrapper <- file.path(bench_root, "run_publication_full.sh")
  expect_true(file.exists(wrapper))
  if (!file.exists(wrapper)) {
    return()
  }

  body <- paste(readLines(wrapper, warn = FALSE), collapse = "\n")
  expect_match(body, "BENCH_STUDY_ID", fixed = TRUE)
  expect_match(body, "run_phase ab publication", fixed = TRUE)
  expect_match(body, "run_phase c1 panel_c1", fixed = TRUE)
  expect_match(body, "run_phase c2 panel_c2", fixed = TRUE)
  expect_match(body, "60_publish_results.R", fixed = TRUE)
  expect_match(body, "publication-full", fixed = TRUE)
})

test_that("publication-full figure uses frozen inputs and uncertainty", {
  skip_unless_bench_cli()
  script <- file.path(bench_root, "src", "43_draw_panel_c_figure.R")
  expect_true(file.exists(script))
  if (!file.exists(script)) {
    return()
  }

  body <- paste(readLines(script, warn = FALSE), collapse = "\n")
  expect_match(body, "study_manifest.csv", fixed = TRUE)
  expect_match(
    body,
    "expression_backend_benchmark_publication_full.png",
    fixed = TRUE
  )
  expect_match(body, "geom_point", fixed = TRUE)
  expect_match(body, "geom_errorbar", fixed = TRUE)
  expect_match(body, "not representable", fixed = TRUE)
  expect_false(grepl("bench_current_result_dir", body, fixed = TRUE))
})

test_that("shared sweep selects the full-source build and resource paths", {
  skip_unless_bench_cli()
  sweep <- paste(
    readLines(file.path(bench_root, "run_sweep.sh"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(sweep, "04_check_full_resources.R", fixed = TRUE)
  expect_match(sweep, "11_build_full_backend.R", fixed = TRUE)
})

test_that("source cache reuses only checksum-verified files", {
  skip_unless_bench_cli()
  helper <- file.path(bench_root, "lib", "source_cache.sh")
  expect_true(file.exists(helper))
  if (!file.exists(helper)) {
    return()
  }

  root <- tempfile("bench-cache-")
  origin <- file.path(root, "origin", "fixture.h5")
  cache <- file.path(root, "cache")
  scratch <- file.path(root, "scratch")
  dir.create(dirname(origin), recursive = TRUE)
  dir.create(scratch)
  writeBin(charToRaw("benchmark-fixture"), origin)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  command <- file.path(root, "fetch.sh")
  writeLines(
    c(
      "set -euo pipefail",
      sprintf("source %s", shQuote(helper)),
      sprintf("export BENCH_SOURCE_CACHE=%s", shQuote(cache)),
      sprintf(
        "bench_fetch_source %s %d %s",
        shQuote(paste0("file://", normalizePath(origin))),
        file.size(origin),
        shQuote(scratch)
      ),
      "test -f \"$BENCH_FETCHED_FILE\"",
      "test -n \"$BENCH_FETCHED_SHA256\""
    ),
    command
  )
  first <- system2("bash", command, stdout = TRUE, stderr = TRUE)
  expect_null(attr(first, "status"), info = paste(first, collapse = "\n"))

  unlink(origin)
  second <- system2("bash", command, stdout = TRUE, stderr = TRUE)
  expect_null(attr(second, "status"), info = paste(second, collapse = "\n"))
})

test_that("publication sources and cache enforce pinned SHA-256 values", {
  skip_unless_bench_cli()
  source(file.path(bench_root, "config", "sources.R"), local = TRUE)
  pinned <- vapply(
    BENCH_SOURCES[c("mouse_brain_e18", "human_pfc_hbcc")],
    `[[`,
    character(1),
    "expected_sha256"
  )
  expect_true(all(grepl("^[0-9a-f]{64}$", pinned)))

  helper <- file.path(bench_root, "lib", "source_cache.sh")
  root <- tempfile("bench-pinned-cache-")
  origin <- file.path(root, "origin", "fixture.h5")
  cache <- file.path(root, "cache")
  scratch <- file.path(root, "scratch")
  dir.create(dirname(origin), recursive = TRUE)
  dir.create(scratch)
  writeBin(charToRaw("benchmark-fixture"), origin)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  command <- file.path(root, "fetch-wrong-sha.sh")
  writeLines(
    c(
      "set -euo pipefail",
      sprintf("source %s", shQuote(helper)),
      sprintf("export BENCH_SOURCE_CACHE=%s", shQuote(cache)),
      sprintf(
        "bench_fetch_source %s %d %s %s",
        shQuote(paste0("file://", normalizePath(origin))),
        file.size(origin),
        shQuote(scratch),
        shQuote(paste(rep("0", 64), collapse = ""))
      )
    ),
    command
  )
  rejected <- suppressWarnings(system2(
    "bash",
    command,
    stdout = TRUE,
    stderr = TRUE
  ))
  expect_false(is.null(attr(rejected, "status")))
  expect_match(paste(rejected, collapse = "\n"), "pinned SHA-256")
})

run_bench_rscript <- function(script, args = character(), env = character()) {
  out <- tempfile("bench-cli-stdout-")
  err <- tempfile("bench-cli-stderr-")
  on.exit(unlink(c(out, err)), add = TRUE)
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(file.path(bench_root, "src", script), args),
    stdout = out,
    stderr = err,
    env = c(paste0("BENCH_ROOT=", bench_root), env)
  )
  list(
    status = status,
    stdout = readLines(out, warn = FALSE),
    stderr = readLines(err, warn = FALSE)
  )
}

test_that("run provenance quotes Git magic pathspecs", {
  skip_unless_bench_cli()
  result <- tempfile(fileext = ".csv")
  on.exit(unlink(result), add = TRUE)

  run <- run_bench_rscript("02_record_environment.R", result)
  expect_equal(run$status, 0L, info = paste(run$stderr, collapse = "\n"))
  expect_false(
    any(grepl("syntax error|unexpected token", run$stderr, ignore.case = TRUE)),
    info = paste(run$stderr, collapse = "\n")
  )
  expect_true(file.exists(result))
})

test_that("schedule CLI emits a complete quick-profile grid", {
  skip_unless_bench_cli()
  result <- tempfile(fileext = ".csv")
  on.exit(unlink(result), add = TRUE)

  run <- run_bench_rscript(
    "03_plan_runs.R",
    result,
    env = "BENCH_PROFILE=quick"
  )
  expect_equal(run$status, 0L, info = paste(run$stderr, collapse = "\n"))
  schedule <- utils::read.csv(result, stringsAsFactors = FALSE)
  expect_named(
    schedule,
    c(
      "profile",
      "source",
      "n_cells",
      "comparison",
      "export_repeat",
      "order_position",
      "backend",
      "access_repeats"
    )
  )
  expect_equal(nrow(schedule), 6L)
  expect_setequal(unique(schedule$backend), c("embedded", "bpcells", "h5"))
})

test_that("manifest CLI records source revision and runtime", {
  skip_unless_bench_cli()
  result <- tempfile(fileext = ".csv")
  on.exit(unlink(result), add = TRUE)

  run <- run_bench_rscript(
    "02_record_environment.R",
    result,
    env = c(
      "BENCH_PROFILE=quick",
      "BENCH_STUDY_ID=test-study",
      "BENCH_RUN_ID=test-run",
      "BENCH_THREADS=3",
      "SLURM_JOB_ID=job-42",
      "SLURM_NODELIST=node-a",
      "SLURM_CPUS_PER_TASK=3"
    )
  )
  expect_equal(run$status, 0L, info = paste(run$stderr, collapse = "\n"))
  manifest <- utils::read.csv(result, stringsAsFactors = FALSE)
  values <- stats::setNames(manifest$value, manifest$key)
  expect_equal(values[["study_id"]], "test-study")
  expect_equal(values[["run_id"]], "test-run")
  expect_equal(values[["profile"]], "quick")
  expect_match(values[["git_sha"]], "^[0-9a-f]{40}$")
  expect_match(values[["r_version"]], "^R version")
  expect_true(nzchar(values[["cpu"]]))
  expect_equal(values[["benchmark_threads"]], "3")
  expect_equal(values[["slurm_job_id"]], "job-42")
  expect_equal(values[["slurm_node_list"]], "node-a")
  expect_equal(values[["slurm_cpus_per_task"]], "3")
  script <- paste(
    readLines(file.path(bench_root, "src", "02_record_environment.R")),
    collapse = "\n"
  )
  expect_match(script, "--untracked-files=no", fixed = TRUE)
  expect_match(script, "ls-files", fixed = TRUE)
  expect_match(script, ":(exclude,glob)tests/bench/result/**", fixed = TRUE)
})

test_that("validator CLI accepts complete results and rejects drift", {
  skip_unless_bench_cli()
  stage <- tempfile("bench-stage-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)

  source(file.path(bench_root, "lib", "protocol.R"), local = TRUE)
  specs <- list(fixture = list(tiers = 1000, comparison_tiers = 1000))
  schedule <- bench_schedule(specs, "quick", "fixture")
  exports <- transform(
    schedule,
    status = "OK",
    run_id = "run-1",
    query_plan_fingerprint = "plan"
  )
  access <- do.call(
    rbind,
    lapply(seq_len(nrow(schedule)), function(i) {
      data.frame(
        run_id = "run-1",
        profile = "quick",
        source = schedule$source[i],
        n_cells = schedule$n_cells[i],
        backend = schedule$backend[i],
        export_repeat = schedule$export_repeat[i],
        access_repeat = 1L,
        status = "OK",
        query_plan_fingerprint = "plan",
        correctness = "OK",
        row_fingerprint = "row",
        reference_row_fingerprint = "row",
        block_fingerprint = "block",
        reference_block_fingerprint = "block",
        stringsAsFactors = FALSE
      )
    })
  )
  utils::write.csv(
    schedule,
    file.path(stage, "05_schedule.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    exports,
    file.path(stage, "10_export.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(
      run_id = "run-1",
      profile = "quick",
      source = "fixture",
      n_cells = 1000,
      n_genes = 100,
      nnz = 10000,
      source_prepare_secs = 1,
      query_plan_secs = 1,
      peak_rss_mb = 10,
      query_plan_fingerprint = "plan",
      status = "OK"
    ),
    file.path(stage, "query_plan_manifest.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(
      run_id = "run-1",
      profile = "quick",
      source = "fixture",
      n_cells = 1000,
      panel_index = seq_len(12L),
      gene = paste0("gene_", seq_len(12L)),
      nnz = seq_len(12L),
      role = c("first", rep("hot", 11L)),
      query_plan_fingerprint = "plan",
      reference_row_fingerprint = "row",
      reference_block_fingerprint = "block"
    ),
    file.path(stage, "query_panel.csv"),
    row.names = FALSE
  )
  utils::write.csv(access, file.path(stage, "20_access.csv"), row.names = FALSE)
  utils::write.csv(
    data.frame(
      key = c("run_id", "profile", "git_sha"),
      value = c("run-1", "quick", paste(rep("a", 40), collapse = ""))
    ),
    file.path(stage, "run_manifest.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(
      source = "fixture",
      n_cells = 1000,
      safe = TRUE,
      reason = "safe"
    ),
    file.path(stage, "resource_check.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(
      run_id = "run-1",
      source = "fixture",
      url = "https://example.test/fixture.h5",
      bytes = 123,
      sha256 = paste(rep("b", 64), collapse = ""),
      stringsAsFactors = FALSE
    ),
    file.path(stage, "source_manifest.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(
      source = character(),
      n_cells = numeric(),
      backend = character(),
      export_repeat = integer(),
      stage = character(),
      exit_code = integer()
    ),
    file.path(stage, "crashes.csv"),
    row.names = FALSE
  )

  ok <- run_bench_rscript(
    "30_check_measurements.R",
    stage,
    env = "BENCH_PROFILE=quick"
  )
  expect_equal(ok$status, 0L, info = paste(ok$stderr, collapse = "\n"))

  unlink(file.path(stage, "query_plan_manifest.csv"))
  missing_plan <- run_bench_rscript(
    "30_check_measurements.R",
    stage,
    env = "BENCH_PROFILE=quick"
  )
  expect_false(identical(missing_plan$status, 0L))
  expect_match(
    paste(missing_plan$stderr, collapse = "\n"),
    "query_plan_manifest.csv"
  )
  utils::write.csv(
    data.frame(
      run_id = "run-1",
      profile = "quick",
      source = "fixture",
      n_cells = 1000,
      n_genes = 100,
      nnz = 10000,
      source_prepare_secs = 1,
      query_plan_secs = 1,
      peak_rss_mb = 10,
      query_plan_fingerprint = "plan",
      status = "OK"
    ),
    file.path(stage, "query_plan_manifest.csv"),
    row.names = FALSE
  )

  unlink(file.path(stage, "resource_check.csv"))
  missing_resources <- run_bench_rscript(
    "30_check_measurements.R",
    stage,
    env = "BENCH_PROFILE=quick"
  )
  expect_false(identical(missing_resources$status, 0L))
  expect_match(
    paste(missing_resources$stderr, collapse = "\n"),
    "resource_check.csv"
  )
  utils::write.csv(
    data.frame(
      source = "fixture",
      n_cells = 1000,
      safe = TRUE,
      reason = "safe"
    ),
    file.path(stage, "resource_check.csv"),
    row.names = FALSE
  )

  access$row_fingerprint[1] <- "wrong"
  utils::write.csv(access, file.path(stage, "20_access.csv"), row.names = FALSE)
  bad <- run_bench_rscript(
    "30_check_measurements.R",
    stage,
    env = "BENCH_PROFILE=quick"
  )
  expect_false(identical(bad$status, 0L))
  expect_match(paste(bad$stderr, collapse = "\n"), "fingerprint mismatch")

  access$row_fingerprint[1] <- "row"
  utils::write.csv(access, file.path(stage, "20_access.csv"), row.names = FALSE)
  source_manifest <- utils::read.csv(
    file.path(stage, "source_manifest.csv"),
    stringsAsFactors = FALSE
  )
  source_manifest$sha256 <- "not-a-checksum"
  utils::write.csv(
    source_manifest,
    file.path(stage, "source_manifest.csv"),
    row.names = FALSE
  )
  bad_source <- run_bench_rscript(
    "30_check_measurements.R",
    stage,
    env = "BENCH_PROFILE=quick"
  )
  expect_false(identical(bad_source$status, 0L))
  expect_match(paste(bad_source$stderr, collapse = "\n"), "source SHA-256")
})
