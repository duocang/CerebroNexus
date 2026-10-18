bench_root <- normalizePath(file.path("..", "benchmarks", "expression-backends", "scripts"), mustWork = FALSE)

skip_unless_bench_cli <- function() {
  testthat::skip_if_not(
    file.exists(file.path(bench_root, "benchmark_cli.R")),
    "benchmark tree not present (expected when checking a built package)"
  )
}

run_bench_command <- function(command, args = character(), env = character()) {
  out <- tempfile("bench-cli-stdout-")
  err <- tempfile("bench-cli-stderr-")
  on.exit(unlink(c(out, err)), add = TRUE)
  status <- bench_test_system2(
    file.path(R.home("bin"), "Rscript"),
    c(file.path(bench_root, "benchmark_cli.R"), command, args),
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

test_that("benchmark has one library and one CLI", {
  skip_unless_bench_cli()

  expect_true(file.exists(file.path(bench_root, "benchmark.R")))
  expect_true(file.exists(file.path(bench_root, "benchmark_cli.R")))
  expect_true(file.exists(file.path(bench_root, "run.sh")))
  expect_false(file.exists(file.path(bench_root, "run_scale.sh")))
  expect_false(dir.exists(file.path(bench_root, "config")))
  expect_false(dir.exists(file.path(bench_root, "lib")))
  expect_false(dir.exists(file.path(bench_root, "src")))
  expect_false(file.exists(file.path(bench_root, "run_sweep.sh")))
})

test_that("runner keeps isolated processes and safe cleanup", {
  skip_unless_bench_cli()
  runner <- paste(
    readLines(file.path(bench_root, "run.sh"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(runner, "trap - EXIT INT TERM", fixed = TRUE)
  expect_match(runner, 'if [ "${BENCH_KEEP:-0}" != "1" ]; then', fixed = TRUE)
  expect_match(runner, "BENCH_RESULT_ROOT", fixed = TRUE)
  expect_match(runner, "R_ENVIRON_USER=/dev/null", fixed = TRUE)
  expect_match(runner, "R_PROFILE_USER=/dev/null", fixed = TRUE)
  expect_match(runner, 'R_LIBS_USER="$SCRATCH/r-user-library"', fixed = TRUE)
  expect_match(runner, "bench_fetch_source()", fixed = TRUE)

  stages <- c(
    " inspect ",
    " environment ",
    " plan ",
    " query-plan ",
    ' "$build_command" ',
    " access ",
    " validate ",
    " report ",
    " figure ",
    " evidence ",
    " check ",
    " publish "
  )
  positions <- vapply(
    stages,
    function(stage) {
      regexpr(stage, runner, fixed = TRUE)[1L]
    },
    integer(1)
  )
  expect_true(all(positions > 0L))
})

test_that("one public launcher runs Scale independently in the background", {
  skip_unless_bench_cli()
  launcher <- paste(
    readLines(file.path(bench_root, "run.sh"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(launcher, 'ACTION="${1:-run}"', fixed = TRUE)
  expect_match(launcher, "status)", fixed = TRUE)
  expect_match(launcher, 'nohup "$SCRIPT" _worker', fixed = TRUE)
  expect_match(launcher, '"$SCRIPT" _profile', fixed = TRUE)
  expect_match(launcher, "resume)", fixed = TRUE)
  full <- regexpr(
    "run_profile panel_c2 full",
    launcher,
    fixed = TRUE
  )[1L]
  scale <- regexpr(
    "run_profile scale scale",
    launcher,
    fixed = TRUE
  )[1L]
  expect_gt(full, 0L)
  expect_gt(scale, 0L)
  expect_match(launcher, "run|scale) run_profile scale scale ;;", fixed = TRUE)
  expect_match(launcher, "full) run_profile panel_c2 full ;;", fixed = TRUE)
})

test_that("schedule CLI includes embedded through 500k", {
  skip_unless_bench_cli()
  csv <- tempfile(fileext = ".csv")
  tsv <- tempfile(fileext = ".tsv")
  on.exit(unlink(c(csv, tsv)), add = TRUE)

  run <- run_bench_command(
    "plan",
    c(csv, tsv),
    env = "BENCH_PROFILE=scale"
  )
  expect_equal(run$status, 0L, info = paste(run$stderr, collapse = "\n"))
  schedule <- utils::read.csv(csv, stringsAsFactors = FALSE)
  expect_setequal(
    unique(schedule$n_cells),
    c(1e3, 10e3, 50e3, 100e3, 300e3, 500e3, 1306127, 1486324)
  )
  expect_setequal(
    unique(schedule$n_cells[schedule$backend == "embedded"]),
    c(1e3, 10e3, 50e3, 100e3, 300e3, 500e3)
  )
  expect_false(any(schedule$backend == "embedded" & schedule$n_cells > 500e3))
  expect_false(any(grepl("[eE][+-]", readLines(tsv, warn = FALSE))))
  # Both H5 variants and BPCells run at every size, five builds each.
  expect_setequal(
    unique(schedule$backend),
    c("embedded", "bpcells", "h5", "h5_uncompressed")
  )
  out_of_core <- schedule[schedule$backend != "embedded", , drop = FALSE]
  builds <- stats::aggregate(
    export_repeat ~ source + n_cells + backend, out_of_core, length
  )
  tiers <- unique(schedule[c("source", "n_cells")])
  expect_equal(nrow(builds), nrow(tiers) * 3L)
  expect_true(all(builds$export_repeat == 5L))
})

test_that("full-source study compares BPCells with both H5 variants", {
  skip_unless_bench_cli()
  csv <- tempfile(fileext = ".csv")
  tsv <- tempfile(fileext = ".tsv")
  on.exit(unlink(c(csv, tsv)), add = TRUE)

  run <- run_bench_command("plan", c(csv, tsv), env = "BENCH_PROFILE=panel_c2")
  expect_equal(run$status, 0L, info = paste(run$stderr, collapse = "\n"))
  schedule <- utils::read.csv(csv, stringsAsFactors = FALSE)
  expect_setequal(unique(schedule$backend), c("bpcells", "h5", "h5_uncompressed"))
  # Rotation lets each backend lead some builds instead of always running first.
  first <- schedule$backend[schedule$order_position == 1L]
  expect_setequal(unique(first), c("bpcells", "h5", "h5_uncompressed"))
})

test_that("manifest CLI records the run identity", {
  skip_unless_bench_cli()
  result <- tempfile(fileext = ".csv")
  on.exit(unlink(result), add = TRUE)

  run <- run_bench_command(
    "environment",
    result,
    env = c(
      "BENCH_PROFILE=quick",
      "BENCH_STUDY_ID=test-study",
      "BENCH_RUN_ID=test-run",
      "BENCH_THREADS=3"
    )
  )
  expect_equal(run$status, 0L, info = paste(run$stderr, collapse = "\n"))
  manifest <- utils::read.csv(result, stringsAsFactors = FALSE)
  values <- stats::setNames(manifest$value, manifest$key)
  expect_equal(values[["study_id"]], "test-study")
  expect_equal(values[["run_id"]], "test-run")
  expect_equal(values[["benchmark_threads"]], "3")
  expect_match(values[["git_sha"]], "^[0-9a-f]{40}$")
  # Figures relabel "h5" from runs that predate this key as uncompressed.
  expect_match(values[["h5_gzip_level"]], "^[1-9]$")
  expect_equal(values[["h5_uncompressed_gzip_level"]], "0")
  cli <- paste(
    readLines(file.path(bench_root, "benchmark_cli.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(cli, ":(exclude,glob)tests/benchmarks/results/expression-backends/**", fixed = TRUE)
  expect_false(grepl("<<-", cli, fixed = TRUE))
  expect_match(
    cli,
    "build = fingerprint_for(successful_exports)",
    fixed = TRUE
  )
  expect_match(
    cli,
    "access = fingerprint_for(successful_access)",
    fixed = TRUE
  )
})

test_that("source cache reuses only verified files", {
  skip_unless_bench_cli()
  bash <- Sys.which("bash")
  if (.Platform$OS.type == "windows") {
    # The Windows system32 bash launcher runs in WSL and cannot consume
    # native Windows fixture paths. Use Git Bash for this local-file test.
    bash <- file.path(Sys.getenv("ProgramFiles"), "Git", "bin", "bash.exe")
  }
  testthat::skip_if_not(nzchar(bash) && file.exists(bash), "native Bash is unavailable")
  helper <- file.path(bench_root, "run.sh")
  root <- gsub("\\", "/", tempfile("bench-cache-"), fixed = TRUE)
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
      sprintf("source %s", shQuote(normalizePath(helper, winslash = "/"))),
      sprintf("export BENCH_SOURCE_CACHE=%s", shQuote(cache)),
      sprintf(
        "bench_fetch_source %s %d %s",
        shQuote(paste0("file://", normalizePath(origin, winslash = "/"))),
        file.size(origin),
        shQuote(scratch)
      )
    ),
    command
  )
  first <- bench_test_system2(bash, command, stdout = TRUE, stderr = TRUE)
  expect_null(attr(first, "status"), info = paste(first, collapse = "\n"))
  unlink(origin)
  second <- bench_test_system2(bash, command, stdout = TRUE, stderr = TRUE)
  expect_null(attr(second, "status"), info = paste(second, collapse = "\n"))
})

test_that("benchmark sources pin SHA-256 values", {
  skip_unless_bench_cli()
  source(file.path(bench_root, "benchmark.R"), local = TRUE)
  pinned <- vapply(
    BENCH_SOURCES[c("mouse_brain_e18", "human_pfc_hbcc")],
    `[[`,
    character(1),
    "expected_sha256"
  )
  expect_true(all(grepl("^[0-9a-f]{64}$", pinned)))
})

test_that("finalization retries only re-verify an already archived run", {
  skip_unless_bench_cli()
  bash <- Sys.which("bash")
  if (.Platform$OS.type == "windows") {
    bash <- file.path(Sys.getenv("ProgramFiles"), "Git", "bin", "bash.exe")
  }
  testthat::skip_if_not(nzchar(bash) && file.exists(bash), "native Bash is unavailable")
  helper <- file.path(bench_root, "run.sh")
  root <- gsub("\\", "/", tempfile("bench-finish-"), fixed = TRUE)
  dir.create(file.path(root, "bin"), recursive = TRUE)
  dir.create(file.path(root, "stage"))
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  log <- file.path(root, "calls")
  # Record the CLI subcommand instead of running R.
  writeLines(
    c("#!/usr/bin/env bash", sprintf("printf '%%s\n' \"$2\" >> %s", shQuote(log))),
    file.path(root, "bin", "Rscript")
  )
  Sys.chmod(file.path(root, "bin", "Rscript"), "0755")
  finish <- function() {
    unlink(log)
    command <- file.path(root, "finish.sh")
    writeLines(c(
      "set -euo pipefail",
      sprintf("source %s", shQuote(normalizePath(helper, winslash = "/"))),
      sprintf("export PATH=%s:\"$PATH\" BENCH_PROFILE=scale", shQuote(file.path(root, "bin"))),
      sprintf("finish_profile %s %s run-1", shQuote(file.path(root, "stage")), shQuote(file.path(root, "result")))
    ), command)
    output <- bench_test_system2(bash, command, stdout = TRUE, stderr = TRUE)
    expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
    readLines(log)
  }

  expect_identical(
    finish(),
    c("validate", "report", "figure", "evidence", "check", "publish")
  )
  dir.create(file.path(root, "result", "runs", "run-1"), recursive = TRUE)
  expect_identical(finish(), "publish")
})

test_that("only an observed cgroup OOM kill turns a failed build into a skip", {
  skip_unless_bench_cli()
  bash <- Sys.which("bash")
  if (.Platform$OS.type == "windows") {
    bash <- file.path(Sys.getenv("ProgramFiles"), "Git", "bin", "bash.exe")
  }
  testthat::skip_if_not(nzchar(bash) && file.exists(bash), "native Bash is unavailable")
  helper <- file.path(bench_root, "run.sh")
  command <- tempfile("bench-oom-", fileext = ".sh")
  on.exit(unlink(command), add = TRUE)
  killed <- function(before, after) {
    writeLines(c(
      "set -euo pipefail",
      sprintf("source %s", shQuote(normalizePath(helper, winslash = "/"))),
      sprintf("bench_oom_kills() { printf '%%s' %s; }", shQuote(after)),
      sprintf("if bench_oom_killed_since %s; then echo yes; else echo no; fi", shQuote(before))
    ), command)
    output <- bench_test_system2(bash, gsub("\\", "/", command, fixed = TRUE),
      stdout = TRUE, stderr = TRUE)
    expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
    identical(output, "yes")
  }

  expect_true(killed("3", "4"))
  expect_false(killed("3", "3"))
  # Unreadable counters never excuse a crash.
  expect_false(killed("", "4"))
  expect_false(killed("3", ""))
})

test_that("the worker hides the caller's personal R setup from nix-shell", {
  skip_unless_bench_cli()
  bash <- Sys.which("bash")
  if (.Platform$OS.type == "windows") {
    bash <- file.path(Sys.getenv("ProgramFiles"), "Git", "bin", "bash.exe")
  }
  testthat::skip_if_not(nzchar(bash) && file.exists(bash), "native Bash is unavailable")
  helper <- file.path(bench_root, "run.sh")
  root <- gsub("\\", "/", tempfile("bench-renv-"), fixed = TRUE)
  dir.create(file.path(root, "bin"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  seen <- file.path(root, "seen")
  # Stand-in nix-shell records the R environment it would hand to R.
  writeLines(c(
    "#!/usr/bin/env bash",
    sprintf(
      "printf '%%s\n' \"R_LIBS=${R_LIBS-unset}\" \"R_LIBS_SITE=${R_LIBS_SITE-unset}\" \"R_LIBS_USER=$R_LIBS_USER\" \"R_ENVIRON_USER=$R_ENVIRON_USER\" \"R_PROFILE_USER=$R_PROFILE_USER\" > %s",
      shQuote(seen)
    )
  ), file.path(root, "bin", "nix-shell"))
  Sys.chmod(file.path(root, "bin", "nix-shell"), "0755")
  command <- file.path(root, "worker.sh")
  writeLines(c(
    sprintf("export BENCH_CACHE_ROOT=%s", shQuote(file.path(root, "cache"))),
    "export R_LIBS=/home/someone/R_libs R_LIBS_SITE=/home/someone/site R_LIBS_USER=/home/someone/user",
    sprintf("source %s", shQuote(normalizePath(helper, winslash = "/"))),
    sprintf("export PATH=%s:\"$PATH\"", shQuote(file.path(root, "bin"))),
    "mkdir -p \"$STATE_DIR\"",
    "( worker quick )"
  ), command)
  output <- bench_test_system2(bash, command, stdout = TRUE, stderr = TRUE)
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_identical(readLines(seen), c(
    "R_LIBS=unset",
    "R_LIBS_SITE=unset",
    "R_LIBS_USER=/nonexistent",
    "R_ENVIRON_USER=/dev/null",
    "R_PROFILE_USER=/dev/null"
  ))
})

test_that("a filtered plan is recorded and published as a supplement", {
  skip_unless_bench_cli()
  csv <- tempfile(fileext = ".csv")
  tsv <- tempfile(fileext = ".tsv")
  manifest <- tempfile(fileext = ".csv")
  on.exit(unlink(c(csv, tsv, manifest)), add = TRUE)
  filter <- "BENCH_SCHEDULE_FILTER=source=human_pfc_hbcc;n_cells=500000;backend=embedded"

  run <- run_bench_command("plan", c(csv, tsv), env = c("BENCH_PROFILE=scale", filter))
  expect_equal(run$status, 0L, info = paste(run$stderr, collapse = "\n"))
  schedule <- utils::read.csv(csv, stringsAsFactors = FALSE)
  expect_equal(nrow(schedule), 5L)
  expect_true(all(schedule$backend == "embedded" & schedule$n_cells == 500000))

  run <- run_bench_command("environment", manifest, env = c("BENCH_PROFILE=scale", filter))
  expect_equal(run$status, 0L, info = paste(run$stderr, collapse = "\n"))
  values <- with(utils::read.csv(manifest, stringsAsFactors = FALSE), stats::setNames(value, key))
  expect_identical(values[["schedule_filter"]], sub("^BENCH_SCHEDULE_FILTER=", "", filter))

  bash <- Sys.which("bash")
  if (.Platform$OS.type == "windows") {
    bash <- file.path(Sys.getenv("ProgramFiles"), "Git", "bin", "bash.exe")
  }
  testthat::skip_if_not(nzchar(bash) && file.exists(bash), "native Bash is unavailable")
  command <- tempfile(fileext = ".sh")
  on.exit(unlink(command), add = TRUE)
  writeLines(c(
    sprintf("source %s", shQuote(normalizePath(file.path(bench_root, "run.sh"), winslash = "/"))),
    "bench_result_root scale ''",
    "bench_result_root scale 'backend=embedded'"
  ), command)
  roots <- bench_test_system2(bash, gsub("\\", "/", command, fixed = TRUE), stdout = TRUE)
  expect_match(roots[1], "/results/expression-backends/scale$")
  expect_match(roots[2], "/results/expression-backends/supplements/scale$")
})
