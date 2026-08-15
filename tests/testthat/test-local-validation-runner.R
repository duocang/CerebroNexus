local_validation_api <- new.env(parent = globalenv())
sys.source(
  test_path("..", "..", "scripts", "run-local-validation.R"),
  envir = local_validation_api
)

test_that("local validation arguments have safe bounded defaults", {
  parsed <- local_validation_api$local_validation_parse_args(character())

  expect_identical(parsed$mode, "full")
  expect_identical(parsed$logic_workers, 3L)
  expect_identical(parsed$browser_workers, 2L)
  expect_false(parsed$dry_run)
  expect_true(nzchar(parsed$output_dir))

  explicit <- local_validation_api$local_validation_parse_args(c(
    "--mode",
    "tests",
    "--logic-workers",
    "4",
    "--browser-workers",
    "1",
    "--output-dir",
    "validation-logs",
    "--dry-run"
  ))
  expect_identical(explicit$mode, "tests")
  expect_identical(explicit$logic_workers, 4L)
  expect_identical(explicit$browser_workers, 1L)
  expect_identical(explicit$output_dir, "validation-logs")
  expect_true(explicit$dry_run)
})

test_that("local validation arguments reject unsafe concurrency", {
  expect_error(
    local_validation_api$local_validation_parse_args(c("--mode", "quick")),
    "mode"
  )
  expect_error(
    local_validation_api$local_validation_parse_args(
      c("--logic-workers", "5")
    ),
    "logic-workers"
  )
  expect_error(
    local_validation_api$local_validation_parse_args(
      c("--browser-workers", "0")
    ),
    "browser-workers"
  )
  expect_error(
    local_validation_api$local_validation_parse_args(c("--unknown")),
    "Unknown"
  )
})

test_that("test schedule preserves isolation and bounded parallelism", {
  output_dir <- withr::local_tempdir()
  schedule <- local_validation_api$local_validation_schedule(
    logic_workers = 3L,
    browser_workers = 2L,
    mode = "tests",
    output_dir = output_dir
  )

  expect_identical(
    unique(schedule$phase),
    c("logic", "process-sensitive", "browser")
  )
  expect_identical(
    vapply(
      c("logic", "process-sensitive", "browser"),
      function(phase) sum(schedule$phase == phase),
      integer(1)
    ),
    c(logic = 4L, `process-sensitive` = 1L, browser = 6L)
  )
  expect_true(all(schedule$cap[schedule$phase == "logic"] == 3L))
  expect_identical(
    schedule$cap[schedule$phase == "process-sensitive"],
    1L
  )
  expect_true(all(schedule$cap[schedule$phase == "browser"] == 2L))
  expect_true(all(grepl(
    "--strategy weighted",
    schedule$command[schedule$phase %in% c("logic", "browser")],
    fixed = TRUE
  )))
  expect_false(grepl(
    "--strategy weighted",
    schedule$command[schedule$phase == "process-sensitive"],
    fixed = TRUE
  ))
  browser_artifacts <- schedule$artifact_dir[schedule$phase == "browser"]
  expect_length(unique(browser_artifacts), 6L)
  expect_true(all(startsWith(browser_artifacts, output_dir)))
})

test_that("full schedule appends check and pkgdown serially", {
  schedule <- local_validation_api$local_validation_schedule(
    logic_workers = 2L,
    browser_workers = 1L,
    mode = "full",
    output_dir = withr::local_tempdir()
  )

  expect_identical(
    unique(schedule$phase),
    c("logic", "process-sensitive", "browser", "check", "pkgdown")
  )
  expect_identical(schedule$cap[schedule$phase == "check"], 1L)
  expect_identical(schedule$cap[schedule$phase == "pkgdown"], 1L)
  expect_match(
    schedule$command[schedule$phase == "check"],
    "--no-tests",
    fixed = TRUE
  )
  expect_match(
    schedule$command[schedule$phase == "pkgdown"],
    "pkgdown-site",
    fixed = TRUE
  )
})

test_that("aggregate status remains nonzero after later successes", {
  expect_identical(
    local_validation_api$local_validation_exit_code(c(0L, 1L, 0L)),
    1L
  )
  expect_identical(
    local_validation_api$local_validation_exit_code(c(0L, 0L)),
    0L
  )
})

test_that("real child processes obey the cap and keep separate logs", {
  skip_if_not_installed("processx")
  output_dir <- withr::local_tempdir()
  rscript <- shQuote(file.path(R.home("bin"), "Rscript"))
  jobs <- do.call(
    rbind,
    lapply(seq_len(3L), function(index) {
      local_validation_api$local_validation_job(
        "synthetic",
        paste0("job-", index),
        2L,
        paste(
          rscript,
          "-e",
          shQuote(paste0(
            "cat('job-",
            index,
            "\\n'); Sys.sleep(0.25)"
          ))
        )
      )
    })
  )

  results <- local_validation_api$local_validation_run_phase(
    jobs,
    repo_root = test_path("..", ".."),
    output_dir = output_dir,
    poll_interval = 0.01
  )

  expect_identical(results$status, rep(0L, 3L))
  expect_true(all(file.exists(results$log)))
  expect_length(unique(results$log), 3L)
  expect_true(all(vapply(
    seq_len(3L),
    function(index) {
      grepl(
        paste0("job-", index),
        paste(readLines(results$log[[index]], warn = FALSE), collapse = "\n"),
        fixed = TRUE
      )
    },
    logical(1)
  )))
  overlaps <- vapply(
    results$started,
    function(started) {
      sum(results$started <= started & results$ended > started)
    },
    integer(1)
  )
  expect_lte(max(overlaps), 2L)
  expect_gte(max(overlaps), 2L)
})

test_that("a failed child does not prevent later jobs from running", {
  skip_if_not_installed("processx")
  output_dir <- withr::local_tempdir()
  rscript <- shQuote(file.path(R.home("bin"), "Rscript"))
  jobs <- rbind(
    local_validation_api$local_validation_job(
      "synthetic",
      "fails",
      1L,
      paste(rscript, "-e", shQuote("quit(status = 7L)"))
    ),
    local_validation_api$local_validation_job(
      "synthetic",
      "runs-after-failure",
      1L,
      paste(rscript, "-e", shQuote("cat('completed\\n')"))
    )
  )

  results <- local_validation_api$local_validation_run_phase(
    jobs,
    repo_root = test_path("..", ".."),
    output_dir = output_dir,
    poll_interval = 0.01
  )

  expect_identical(results$status, c(7L, 0L))
  expect_match(
    paste(readLines(results$log[[2L]], warn = FALSE), collapse = "\n"),
    "completed",
    fixed = TRUE
  )
  expect_identical(
    local_validation_api$local_validation_exit_code(results$status),
    1L
  )
})

test_that("browser jobs receive isolated runtime environment", {
  output_dir <- withr::local_tempdir()
  schedule <- local_validation_api$local_validation_schedule(
    mode = "tests",
    output_dir = output_dir
  )
  browser <- schedule[schedule$phase == "browser", , drop = FALSE]
  environments <- lapply(
    seq_len(nrow(browser)),
    function(index) {
      local_validation_api$local_validation_job_env(
        browser[index, , drop = FALSE],
        repo_root = test_path("..", "..")
      )
    }
  )

  expect_true(all(vapply(
    environments,
    function(environment) {
      identical(unname(environment[["CEREBRO_RUN_BROWSER_TESTS"]]), "true")
    },
    logical(1)
  )))
  expect_identical(
    vapply(
      environments,
      function(environment) {
        unname(environment[["CEREBRO_TEST_ARTIFACT_DIR"]])
      },
      character(1)
    ),
    browser$artifact_dir
  )
})

test_that("stray process preflight reports only recognizable launchers", {
  lines <- c(
    "101 R --vanilla -e shiny::runApp('app')",
    "102 /usr/bin/Rscript ordinary-analysis.R",
    "103 R -e cerebroApp::launch()"
  )

  expect_identical(
    local_validation_api$local_validation_stray_processes(lines),
    lines[c(1L, 3L)]
  )
})

test_that("cleanup terminates only children owned by the runner", {
  skip_if_not_installed("processx")
  command <- file.path(R.home("bin"), "Rscript")
  owned <- processx::process$new(command, c("-e", "Sys.sleep(10)"))
  unrelated <- processx::process$new(command, c("-e", "Sys.sleep(10)"))
  on.exit({
    if (owned$is_alive()) {
      owned$kill_tree()
    }
    if (unrelated$is_alive()) unrelated$kill_tree()
  })

  local_validation_api$local_validation_terminate_children(list(owned))
  owned$wait(1000)

  expect_false(owned$is_alive())
  expect_true(unrelated$is_alive())
})
