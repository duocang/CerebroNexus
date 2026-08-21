ci_test_plan_api <- new.env(parent = globalenv())
sys.source(
  test_path("..", "..", "scripts", "run-test-shard.R"),
  envir = ci_test_plan_api
)

test_that("the Viewer CI plan classifies only tests present in the Viewer layer", {
  plan <- ci_test_plan_api$ci_test_plan(test_path())
  discovered <- sort(list.files(
    test_path(),
    pattern = "^test-[[:alnum:]_.-]+[.]R$",
    full.names = FALSE
  ))

  expect_identical(
    plan$browser,
    c(
      "test-app-immune_repertoire.R",
      "test-app-inst.R",
      "test-app-new-modules.R",
      "test-app-trajectory.R",
      "test-app-viewport-layout.R",
      "test-configured-colors.R",
      "test-coordinated-views-browser.R",
      "test-smoke-production.R",
      "test-viewer-shell-browser.R"
    )
  )
  expect_setequal(c(plan$logic, plan$browser), discovered)
  expect_false(anyDuplicated(c(plan$logic, plan$browser)) > 0L)
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
      list(logic = files, browser = character()),
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

  expect_setequal(browser_references, ci_test_plan_api$ci_browser_test_files())
})

test_that("precheck only checks formatting", {
  precheck <- paste(
    readLines(test_path("..", "..", "scripts", "precheck.sh"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(precheck, "air format --check [.]", perl = TRUE)
  expect_false(grepl("air format [.]($|\\n)", precheck, perl = TRUE))
  expect_false(grepl("run-local-validation", precheck, fixed = TRUE))
})

test_that("the CI workflow aggregates the logic and browser groups", {
  workflow <- readLines(
    test_path("..", "..", ".github", "workflows", "R-tests.yaml"),
    warn = FALSE
  )
  text <- paste(workflow, collapse = "\n")

  expect_false(any(grepl("^  process_sensitive:$", workflow)))
  expect_match(text, "needs: [logic, browser]", fixed = TRUE)
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
