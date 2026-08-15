test_plan_api <- new.env(parent = globalenv())
sys.source(
  test_path("..", "..", "scripts", "run-test-shard.R"),
  envir = test_plan_api
)

test_that("the CI test plan classifies every test file exactly once", {
  test_dir <- test_path()
  plan <- test_plan_api$ci_test_plan(test_dir)
  discovered <- sort(list.files(
    test_dir,
    pattern = "^test-[[:alnum:]_.-]+[.]R$",
    full.names = FALSE
  ))
  classified <- c(plan$logic, plan$process_sensitive, plan$browser)

  expect_setequal(classified, discovered)
  expect_length(classified, length(discovered))
  expect_false(anyDuplicated(classified) > 0L)
})

test_that("process-sensitive and browser tests cannot enter the logic pool", {
  plan <- test_plan_api$ci_test_plan(test_path())
  browser_opt_in <- c(
    "test-builder-loading-browser.R",
    "test-builder-browser.R",
    "test-builder-staged-workflow-browser.R"
  )

  expect_identical(plan$process_sensitive, "test-builder-worker.R")
  expect_false("test-builder-worker.R" %in% c(plan$logic, plan$browser))
  expect_length(plan$browser, 25L)
  expect_length(intersect(plan$browser, plan$logic), 0L)
  expect_true(all(browser_opt_in %in% plan$browser))
  expect_true(all(
    c(
      "test-generated-app-multidataset.R",
      "test-generated-app-pages-immune.R",
      "test-generated-app-security.R"
    ) %in%
      plan$browser
  ))
})

test_that("shard assignment is deterministic and lossless", {
  plan <- test_plan_api$ci_test_plan(test_path())
  first <- test_plan_api$ci_test_shards(plan$logic, 4L)
  second <- test_plan_api$ci_test_shards(rev(plan$logic), 4L)

  expect_identical(first, second)
  expect_setequal(unlist(first, use.names = FALSE), plan$logic)
  expect_length(unlist(first, use.names = FALSE), length(plan$logic))
})

test_that("browser shards allow slow process startup on shared CI runners", {
  withr::local_options(list(chromote.timeout = 10))

  configured <- test_plan_api$ci_configure_browser_runtime()

  expect_identical(configured, 30)
  expect_identical(getOption("chromote.timeout"), 30)
})

test_that("new valid test files automatically join the logic group", {
  test_dir <- withr::local_tempdir()
  explicit <- c(
    test_plan_api$ci_browser_test_files(),
    test_plan_api$ci_process_sensitive_test_files()
  )
  expect_true(all(file.create(file.path(test_dir, explicit))))
  expect_true(file.create(file.path(test_dir, "test-new-contract.R")))

  plan <- test_plan_api$ci_test_plan(test_dir)

  expect_identical(plan$logic, "test-new-contract.R")
  expect_setequal(
    c(plan$logic, plan$process_sensitive, plan$browser),
    c(explicit, "test-new-contract.R")
  )
})

test_that("invalid filenames and duplicate explicit groups are rejected", {
  test_dir <- withr::local_tempdir()
  explicit <- c(
    test_plan_api$ci_browser_test_files(),
    test_plan_api$ci_process_sensitive_test_files()
  )
  expect_true(all(file.create(file.path(test_dir, explicit))))
  expect_true(file.create(file.path(test_dir, "test_invalid.R")))
  expect_error(
    test_plan_api$ci_test_plan(test_dir),
    "Invalid test filename"
  )

  unlink(file.path(test_dir, "test_invalid.R"))
  original <- test_plan_api$ci_process_sensitive_test_files
  on.exit(assign(
    "ci_process_sensitive_test_files",
    original,
    envir = test_plan_api
  ))
  assign(
    "ci_process_sensitive_test_files",
    function() test_plan_api$ci_browser_test_files()[[1L]],
    envir = test_plan_api
  )
  expect_error(
    test_plan_api$ci_test_plan(test_dir),
    "more than one explicit group"
  )
})

test_that("CI workflows use the shared plan without repeating package tests", {
  workflow_dir <- test_path("..", "..", ".github", "workflows")
  workflow_paths <- list.files(
    workflow_dir,
    pattern = "[.]ya?ml$",
    full.names = TRUE
  )
  workflow_lines <- unlist(lapply(workflow_paths, readLines, warn = FALSE))
  active_workflow_lines <- workflow_lines[
    !grepl(
      "^[[:space:]]*#",
      workflow_lines
    )
  ]
  r_test_lines <- readLines(
    file.path(workflow_dir, "R-tests.yaml"),
    warn = FALSE
  )
  r_tests <- paste(r_test_lines, collapse = "\n")
  r_cmd_check <- paste(
    readLines(file.path(workflow_dir, "R-cmd-check.yaml"), warn = FALSE),
    collapse = "\n"
  )
  pkgdown <- paste(
    readLines(file.path(workflow_dir, "pkgdown.yaml"), warn = FALSE),
    collapse = "\n"
  )
  workflow_job <- function(job_name) {
    job_starts <- grep("^  [[:alnum:]_-]+:$", r_test_lines)
    target <- grep(paste0("^  ", job_name, ":$"), r_test_lines)
    if (length(target) != 1L) {
      stop(
        "Expected exactly one top-level workflow job named ",
        job_name,
        call. = FALSE
      )
    }
    following_jobs <- job_starts[job_starts > target]
    end <- if (length(following_jobs)) {
      following_jobs[[1L]] - 1L
    } else {
      length(r_test_lines)
    }
    job_lines <- r_test_lines[seq.int(target, end)]
    job_lines <- job_lines[!grepl("^[[:space:]]*#", job_lines)]
    paste(job_lines, collapse = "\n")
  }
  logic_job <- workflow_job("logic")
  process_sensitive_job <- workflow_job("process_sensitive")
  browser_job <- workflow_job("browser")
  summary_job <- workflow_job("test")

  expect_match(r_tests, "scripts/run-test-shard.R", fixed = TRUE)
  expect_match(
    logic_job,
    "name: logic (${{ matrix.shard }}/4)",
    fixed = TRUE
  )
  expect_match(logic_job, "shard: [1, 2, 3, 4]", fixed = TRUE)
  expect_match(
    logic_job,
    "name: Run logic shard ${{ matrix.shard }}/4",
    fixed = TRUE
  )
  expect_match(logic_job, "--shards 4", fixed = TRUE)
  expect_match(process_sensitive_job, "name: process-sensitive", fixed = TRUE)
  expect_match(
    process_sensitive_job,
    "--group process-sensitive",
    fixed = TRUE
  )
  expect_match(
    browser_job,
    "name: browser (${{ matrix.shard }}/6)",
    fixed = TRUE
  )
  expect_match(browser_job, "fail-fast: false", fixed = TRUE)
  expect_match(browser_job, "max-parallel: 6", fixed = TRUE)
  expect_match(browser_job, "shard: [1, 2, 3, 4, 5, 6]", fixed = TRUE)
  expect_match(
    browser_job,
    "GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}",
    fixed = TRUE
  )
  expect_match(
    browser_job,
    'CEREBRO_RUN_BROWSER_TESTS: "true"',
    fixed = TRUE
  )
  expect_match(
    browser_job,
    paste0(
      "CEREBRO_TEST_ARTIFACT_DIR: ",
      "tests/testthat/_artifacts/browser-${{ matrix.shard }}-of-6"
    ),
    fixed = TRUE
  )
  expect_match(
    browser_job,
    "name: Run browser shard ${{ matrix.shard }}/6",
    fixed = TRUE
  )
  expect_match(browser_job, "--shards 6", fixed = TRUE)
  expect_match(
    browser_job,
    "name: shinytest2-failures-${{ matrix.shard }}-of-6",
    fixed = TRUE
  )
  expect_match(browser_job, "name: Upload test artifacts", fixed = TRUE)
  expect_match(browser_job, "if: always()", fixed = TRUE)
  expect_match(
    browser_job,
    "tests/testthat/_artifacts/",
    fixed = TRUE
  )
  expect_match(browser_job, "tests/testthat/_snaps/", fixed = TRUE)
  expect_match(browser_job, "/tmp/shinytest2*", fixed = TRUE)
  expect_match(summary_job, "name: test", fixed = TRUE)
  expect_match(
    summary_job,
    "needs: [logic, process_sensitive, browser]",
    fixed = TRUE
  )
  expect_false(any(grepl(
    "testthat::test",
    active_workflow_lines,
    fixed = TRUE
  )))
  expect_false(any(grepl(
    "devtools::test",
    active_workflow_lines,
    fixed = TRUE
  )))
  expect_match(r_cmd_check, "--no-tests", fixed = TRUE)
  expect_match(
    pkgdown,
    "dest_dir = 'pkgdown-site'",
    fixed = TRUE
  )
})
