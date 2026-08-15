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

  expect_identical(plan$process_sensitive, "test-builder-worker.R")
  expect_false("test-builder-worker.R" %in% plan$logic)
  expect_length(intersect(plan$browser, plan$logic), 0L)
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

test_that("new valid test files automatically join the logic group", {
  test_dir <- local_tempdir()
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
  test_dir <- local_tempdir()
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
  r_tests <- paste(
    readLines(file.path(workflow_dir, "R-tests.yaml"), warn = FALSE),
    collapse = "\n"
  )
  r_cmd_check <- paste(
    readLines(file.path(workflow_dir, "R-cmd-check.yaml"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(r_tests, "scripts/run-test-shard.R", fixed = TRUE)
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
})
