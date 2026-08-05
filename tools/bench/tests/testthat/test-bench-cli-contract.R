bench_root <- normalizePath(
  file.path("..", ".."),
  mustWork = FALSE
)

skip_unless_bench_cli <- function() {
  testthat::skip_if_not(
    dir.exists(bench_root),
    "benchmark source tree is incomplete"
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

test_that("benchmark launchers isolate user R configuration", {
  skip_unless_bench_cli()
  launchers <- c("run_sweep.sh", "run_contract_tests.sh")
  expect_true(all(file.exists(file.path(bench_root, launchers))))
  if (!all(file.exists(file.path(bench_root, launchers)))) {
    return(invisible())
  }

  for (launcher in launchers) {
    lines <- readLines(file.path(bench_root, launcher), warn = FALSE)
    expect_true(
      any(grepl("unset R_LIBS R_LIBS_USER", lines, fixed = TRUE)),
      info = launcher
    )
    expect_true(
      any(grepl("export R_ENVIRON_USER=/dev/null", lines, fixed = TRUE)),
      info = launcher
    )
    expect_true(
      any(grepl("export R_PROFILE_USER=/dev/null", lines, fixed = TRUE)),
      info = launcher
    )
  }

  contract_launcher <- readLines(
    file.path(bench_root, "run_contract_tests.sh"),
    warn = FALSE
  )
  expect_false(any(grepl("--vanilla", contract_launcher, fixed = TRUE)))
})

test_that("contract runner fails closed on benchmark dependencies", {
  skip_unless_bench_cli()
  runner <- readLines(
    file.path(bench_root, "run_contract_tests.R"),
    warn = FALSE
  )
  remote_tests <- readLines(
    file.path(
      bench_root,
      "tests",
      "testthat",
      "test-bench-remote-reader.R"
    ),
    warn = FALSE
  )

  runner_text <- paste(runner, collapse = "\n")
  rhdf5 <- regexpr('"rhdf5"', runner_text, fixed = TRUE)[1]
  testthat <- regexpr('"testthat"', runner_text, fixed = TRUE)[1]
  expect_true(rhdf5 > 0 && testthat > 0)
  expect_lt(rhdf5, testthat)
  expect_true(any(grepl(
    "benchmark contract dependency cannot be loaded",
    runner,
    fixed = TRUE
  )))
  expect_false(any(grepl(
    "skip_if_not_installed",
    remote_tests,
    fixed = TRUE
  )))
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

test_that("exports use each source's declared expression slot", {
  skip_unless_bench_cli()
  export_source <- readLines(
    file.path(bench_root, "src", "10_export_backend.R"),
    warn = FALSE
  )
  expect_true(any(grepl("slot = spec$slot", export_source, fixed = TRUE)))
  expect_true(any(grepl(
    "bench_make_seurat(m, slot = spec$slot)",
    export_source,
    fixed = TRUE
  )))
  expect_false(any(grepl('slot = "counts"', export_source, fixed = TRUE)))
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
    env = c("BENCH_PROFILE=quick", "BENCH_RUN_ID=test-run")
  )
  expect_equal(run$status, 0L, info = paste(run$stderr, collapse = "\n"))
  manifest <- utils::read.csv(result, stringsAsFactors = FALSE)
  values <- stats::setNames(manifest$value, manifest$key)
  expect_equal(values[["run_id"]], "test-run")
  expect_equal(values[["profile"]], "quick")
  expect_match(values[["git_sha"]], "^[0-9a-f]{40}$")
  expect_equal(values[["dependency_environment"]], "default.nix")
  expect_match(
    values[["dependency_environment_git_blob"]],
    "^[0-9a-f]{40}$"
  )
  expect_match(values[["r_version"]], "^R version")
  expect_true(nzchar(values[["cpu"]]))
  description <- read.dcf(file.path(bench_root, "..", "..", "DESCRIPTION"))
  expect_equal(
    values[["repository_version"]],
    unname(description[1, "Version"])
  )
  expect_false("package_version.Version" %in% manifest$key)
})

test_that("sweep finalizes installed-package provenance after installation", {
  skip_unless_bench_cli()
  sweep <- readLines(file.path(bench_root, "run_sweep.sh"), warn = FALSE)
  manifest_calls <- grep("02_record_environment.R", sweep, fixed = TRUE)
  install_call <- grep("R CMD INSTALL", sweep, fixed = TRUE)

  expect_equal(length(manifest_calls), 2L)
  expect_equal(length(install_call), 1L)
  expect_lt(manifest_calls[1], install_call)
  expect_gt(manifest_calls[2], install_call)
})

test_that("source registry pins bytes and SHA-256 for every public file", {
  skip_unless_bench_cli()
  source(file.path(bench_root, "config", "sources.R"), local = TRUE)

  expect_true(all(vapply(
    BENCH_SOURCES,
    function(spec) {
      is.numeric(spec$expected_bytes) &&
        length(spec$expected_bytes) == 1L &&
        is.finite(spec$expected_bytes)
    },
    logical(1)
  )))
  expect_true(all(vapply(
    BENCH_SOURCES,
    function(spec) {
      is.character(spec$expected_sha256) &&
        length(spec$expected_sha256) == 1L &&
        grepl("^[0-9a-f]{64}$", spec$expected_sha256)
    },
    logical(1)
  )))
  expect_equal(BENCH_SOURCES$human_pfc_mssm$expected_bytes, 36092176654)

  sweep <- readLines(file.path(bench_root, "run_sweep.sh"), warn = FALSE)
  expect_true(any(grepl("source identity mismatch", sweep, fixed = TRUE)))
})

test_that("validator CLI accepts complete results and rejects drift", {
  skip_unless_bench_cli()
  stage <- tempfile("bench-stage-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)

  source(file.path(bench_root, "lib", "protocol.R"), local = TRUE)
  specs <- list(fixture = list(tiers = 1000, comparison_tiers = 1000))
  schedule <- bench_schedule(specs, "quick", "fixture")
  exports <- transform(schedule, status = "OK", run_id = "run-1")
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
        correctness = "OK",
        row_fingerprint = "row",
        reference_row_fingerprint = "row",
        block_fingerprint = "block",
        reference_block_fingerprint = "block",
        block_prepare_secs = 0.1,
        block_materialize_secs = 0.2,
        block_ready_secs = 0.3,
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
  utils::write.csv(access, file.path(stage, "20_access.csv"), row.names = FALSE)
  utils::write.csv(
    data.frame(
      key = c(
        "run_id",
        "profile",
        "git_sha",
        "dependency_environment",
        "dependency_environment_git_blob",
        "repository_version",
        "package_CerebroNexus"
      ),
      value = c(
        "run-1",
        "quick",
        paste(rep("a", 40), collapse = ""),
        "default.nix",
        paste(rep("c", 40), collapse = ""),
        "3.2.0",
        "3.2.0"
      )
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

  utils::write.csv(
    access[names(access) != "block_materialize_secs"],
    file.path(stage, "20_access.csv"),
    row.names = FALSE
  )
  missing_block_materialization <- run_bench_rscript(
    "30_check_measurements.R",
    stage,
    env = "BENCH_PROFILE=quick"
  )
  expect_false(identical(missing_block_materialization$status, 0L))
  expect_match(
    paste(missing_block_materialization$stderr, collapse = "\n"),
    "materialized block timing"
  )
  utils::write.csv(
    access,
    file.path(stage, "20_access.csv"),
    row.names = FALSE
  )

  manifest <- utils::read.csv(
    file.path(stage, "run_manifest.csv"),
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    manifest[manifest$key != "dependency_environment_git_blob", ],
    file.path(stage, "run_manifest.csv"),
    row.names = FALSE
  )
  missing_environment <- run_bench_rscript(
    "30_check_measurements.R",
    stage,
    env = "BENCH_PROFILE=quick"
  )
  expect_false(identical(missing_environment$status, 0L))
  expect_match(
    paste(missing_environment$stderr, collapse = "\n"),
    "dependency environment provenance"
  )
  utils::write.csv(
    manifest,
    file.path(stage, "run_manifest.csv"),
    row.names = FALSE
  )

  mismatched_package <- manifest
  mismatched_package$value[
    mismatched_package$key == "package_CerebroNexus"
  ] <- "3.1.9"
  utils::write.csv(
    mismatched_package,
    file.path(stage, "run_manifest.csv"),
    row.names = FALSE
  )
  package_drift <- run_bench_rscript(
    "30_check_measurements.R",
    stage,
    env = "BENCH_PROFILE=quick"
  )
  expect_false(identical(package_drift$status, 0L))
  expect_match(
    paste(package_drift$stderr, collapse = "\n"),
    "installed CerebroNexus version"
  )
  utils::write.csv(
    manifest,
    file.path(stage, "run_manifest.csv"),
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
