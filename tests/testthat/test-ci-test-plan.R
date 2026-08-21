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
    "test-builder-staged-workflow-browser.R",
    "test-coordinated-views-config-browser.R",
    "test-hla-tcr-main-case-browser.R"
  )

  expect_identical(plan$process_sensitive, "test-builder-worker.R")
  expect_false("test-builder-worker.R" %in% c(plan$logic, plan$browser))
  expect_length(plan$browser, 27L)
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

test_that("weighted shards use stable longest-processing-time assignment", {
  files <- paste0("test-", letters[1:4], ".R")
  weights <- c(
    `test-a.R` = 8,
    `test-b.R` = 7,
    `test-c.R` = 6,
    `test-d.R` = 5
  )

  first <- test_plan_api$ci_test_shards(
    files,
    2L,
    strategy = "weighted",
    weights = weights
  )
  second <- test_plan_api$ci_test_shards(
    rev(files),
    2L,
    strategy = "weighted",
    weights = weights[rev(names(weights))]
  )

  expect_identical(
    first,
    list(
      c("test-a.R", "test-d.R"),
      c("test-b.R", "test-c.R")
    )
  )
  expect_identical(second, first)
  expect_setequal(unlist(first, use.names = FALSE), files)
  expect_false(anyDuplicated(unlist(first, use.names = FALSE)) > 0L)
  expect_identical(
    test_plan_api$ci_test_shard_loads(first, weights),
    c(13, 13)
  )
})

test_that("weighted shard ties prefer filenames and lower shard numbers", {
  files <- c("test-c.R", "test-b.R", "test-a.R")
  weights <- c(`test-c.R` = 1, `test-b.R` = 5, `test-a.R` = 5)

  assigned <- test_plan_api$ci_test_shards(
    files,
    2L,
    strategy = "weighted",
    weights = weights
  )

  expect_identical(
    assigned,
    list(c("test-a.R", "test-c.R"), "test-b.R")
  )
})

test_that("round-robin remains the default and weighted inputs are complete", {
  files <- paste0("test-", letters[1:4], ".R")

  expect_identical(
    test_plan_api$ci_test_shards(files, 2L),
    list(c("test-a.R", "test-c.R"), c("test-b.R", "test-d.R"))
  )
  expect_error(
    test_plan_api$ci_test_shards(
      files,
      2L,
      strategy = "weighted",
      weights = c(`test-a.R` = 1)
    ),
    "weight"
  )
  expect_error(
    test_plan_api$ci_test_shards(files, 2L, strategy = "unknown"),
    "strategy"
  )
})

test_that("runtime weights validate records and fill new files by group", {
  plan <- list(
    all = c("test-a.R", "test-b.R", "test-c.R"),
    logic = c("test-a.R", "test-b.R"),
    process_sensitive = character(),
    browser = "test-c.R"
  )
  weights_path <- withr::local_tempfile(fileext = ".csv")
  write.csv(
    data.frame(
      group = c("logic", "browser"),
      file = c("test-a.R", "test-c.R"),
      seconds = c(10, 20),
      basis = c("measured", "measured")
    ),
    weights_path,
    row.names = FALSE
  )

  weights <- test_plan_api$ci_test_runtime_weights(plan, weights_path)

  expect_identical(names(weights), sort(plan$all))
  expect_identical(as.numeric(weights), c(10, 10, 20))
  expect_true(all(is.finite(weights) & weights > 0))
  expect_identical(attr(weights, "estimated"), "test-b.R")
})

test_that("runtime weights reject malformed or stale records", {
  plan <- list(
    all = c("test-a.R", "test-b.R"),
    logic = "test-a.R",
    process_sensitive = character(),
    browser = "test-b.R"
  )
  weights_path <- withr::local_tempfile(fileext = ".csv")
  write_weights <- function(group, file, seconds, basis = "measured") {
    write.csv(
      data.frame(
        group = group,
        file = file,
        seconds = seconds,
        basis = basis
      ),
      weights_path,
      row.names = FALSE
    )
  }

  write_weights(c("logic", "logic"), c("test-a.R", "test-a.R"), c(1, 2))
  expect_error(
    test_plan_api$ci_test_runtime_weights(plan, weights_path),
    "duplicate"
  )

  write_weights("logic", "test-a.R", 0)
  expect_error(
    test_plan_api$ci_test_runtime_weights(plan, weights_path),
    "positive"
  )

  write_weights("unknown", "test-a.R", 1)
  expect_error(
    test_plan_api$ci_test_runtime_weights(plan, weights_path),
    "group"
  )

  write_weights("logic", "test-stale.R", 1)
  expect_error(
    test_plan_api$ci_test_runtime_weights(plan, weights_path),
    "stale"
  )

  write_weights("browser", "test-a.R", 1)
  expect_error(
    test_plan_api$ci_test_runtime_weights(plan, weights_path),
    "classified"
  )
})

test_that("browser shards allow slow process startup on shared CI runners", {
  withr::local_options(list(chromote.timeout = 10))
  withr::local_envvar(CEREBRO_PACKAGE_SOURCE = NA)

  configured <- test_plan_api$ci_configure_browser_runtime(test_path(
    "..",
    ".."
  ))

  expect_identical(configured, 30)
  expect_identical(getOption("chromote.timeout"), 30)
  expect_identical(
    Sys.getenv("CEREBRO_PACKAGE_SOURCE"),
    normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
  )
})

test_that("the shard CLI accepts explicit weighted strategy", {
  expect_identical(
    test_plan_api$ci_parse_args(character())$strategy,
    "round-robin"
  )
  expect_identical(
    test_plan_api$ci_parse_args(c("--strategy", "weighted"))$strategy,
    "weighted"
  )
  expect_error(
    test_plan_api$ci_parse_args(c("--strategy", "unknown")),
    "strategy"
  )
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

  for (job in list(logic_job, process_sensitive_job, browser_job)) {
    configure_library <- paste0(
      'echo "R_LIBS_USER=$RUNNER_TEMP/cerebronexus-library" ',
      '>> "$GITHUB_ENV"'
    )
    expect_match(
      job,
      configure_library,
      fixed = TRUE
    )
    expect_false(grepl('R_LIBS_USER: ${{ runner.temp }}', job, fixed = TRUE))
    expect_match(job, 'mkdir -p "$R_LIBS_USER"', fixed = TRUE)
    expect_match(job, "R CMD INSTALL", fixed = TRUE)
    expect_match(job, '--library="$R_LIBS_USER" .', fixed = TRUE)
    expect_lt(
      regexpr(configure_library, job, fixed = TRUE),
      regexpr("R CMD INSTALL", job, fixed = TRUE)
    )
    expect_lt(
      regexpr("R CMD INSTALL", job, fixed = TRUE),
      regexpr("Rscript scripts/run-test-shard.R", job, fixed = TRUE)
    )
  }

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
  expect_match(logic_job, "--strategy weighted", fixed = TRUE)
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
  expect_match(browser_job, "--strategy weighted", fixed = TRUE)
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

test_that("the CI workflow matches the process-sensitive test plan", {
  plan <- test_plan_api$ci_test_plan(test_path())
  workflow <- readLines(
    test_path("..", "..", ".github", "workflows", "R-tests.yaml"),
    warn = FALSE
  )
  text <- paste(workflow, collapse = "\n")

  has_process_sensitive_tests <- length(plan$process_sensitive) > 0L
  expect_identical(
    any(grepl("^  process_sensitive:$", workflow)),
    has_process_sensitive_tests
  )
  expect_match(
    text,
    if (has_process_sensitive_tests) {
      "needs: [logic, process_sensitive, browser]"
    } else {
      "needs: [logic, browser]"
    },
    fixed = TRUE
  )
})

test_that("manual pkgdown validation never deploys the site", {
  workflow <- paste(
    readLines(
      test_path("..", "..", ".github", "workflows", "pkgdown.yaml"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    workflow,
    "if: github.event_name == 'push' && github.ref == 'refs/heads/master'",
    fixed = TRUE
  )
})
