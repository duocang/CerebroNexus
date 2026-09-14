ci_test_plan_api <- new.env(parent = globalenv())
sys.source(
  test_path("..", "..", "scripts", "run-test-shard.R"),
  envir = ci_test_plan_api
)

test_that("the Builder CI plan classifies every present test", {
  plan <- ci_test_plan_api$ci_test_plan(test_path())
  discovered <- sort(list.files(
    test_path(),
    pattern = "^test-[[:alnum:]_.-]+[.]R$",
    full.names = FALSE
  ))

  expect_identical(plan$process_sensitive, "test-builder-worker.R")
  classified <- c(plan$logic, plan$process_sensitive, plan$browser)
  expect_setequal(classified, discovered)
  expect_false(anyDuplicated(classified) > 0L)
})

test_that("round-robin sharding remains deterministic and lossless", {
  files <- paste0("test-", letters[1:4], ".R")

  assigned <- ci_test_plan_api$ci_test_shards(
    files,
    2L
  )

  expect_identical(
    assigned,
    list(c("test-a.R", "test-c.R"), c("test-b.R", "test-d.R"))
  )
  expect_setequal(unlist(assigned, use.names = FALSE), files)
  expect_error(
    ci_test_plan_api$ci_test_shard_files(
      list(
        logic = files,
        process_sensitive = character(),
        browser = character()
      ),
      "logic",
      shard = 1.5,
      shards = 2L
    ),
    "shard"
  )
})

test_that("the shard runner rejects retired weighted options", {
  expect_error(
    ci_test_plan_api$ci_parse_args(c("--strategy", "weighted")),
    "Unknown argument"
  )
})

test_that("browser references match the explicit browser group", {
  files <- list.files(
    test_path(),
    pattern = "^test-[[:alnum:]_.-]+[.]R$",
    full.names = TRUE
  )
  files <- files[basename(files) != "test-ci-test-plan.R"]
  browser_references <- basename(files[vapply(
    files,
    function(file) {
      any(grepl("shinytest2|AppDriver", readLines(file, warn = FALSE)))
    },
    logical(1)
  )])

  expect_true(all(
    browser_references %in% ci_test_plan_api$ci_browser_test_files()
  ))
})

test_that("precheck only checks formatting", {
  precheck <- paste(
    readLines(test_path("..", "..", "scripts", "precheck.sh"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(precheck, "air format --check [.]", perl = TRUE)
  expect_false(grepl("air format [.]($|\\n)", precheck, perl = TRUE))
  expect_false(grepl("run-local-validation", precheck, fixed = TRUE))
  expect_match(precheck, "--group process-sensitive", fixed = TRUE)
  expect_match(precheck, "hw.ncpu", fixed = TRUE)
  expect_match(precheck, "run_parallel_test_groups", fixed = TRUE)
  expect_match(precheck, "CEREBRO_PRECHECK_LOGIC_SHARDS", fixed = TRUE)
})

test_that("the CI workflow includes the Builder process-sensitive group", {
  workflow <- readLines(
    test_path("..", "..", ".github", "workflows", "R-tests.yaml"),
    warn = FALSE
  )
  text <- paste(workflow, collapse = "\n")

  expect_true(any(grepl("^  process_sensitive:$", workflow)))
  expect_match(text, "needs: [logic, process_sensitive, browser]", fixed = TRUE)
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
