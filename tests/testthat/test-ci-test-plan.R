ci_test_plan_api <- new.env(parent = globalenv())
sys.source(
  test_path("..", "..", "scripts", "run-test-shard.R"),
  envir = ci_test_plan_api
)

test_that("the base CI plan classifies only tests present on upstream master", {
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
      "test-smoke-production.R"
    )
  )
  expect_identical(plan$process_sensitive, character())
  expect_setequal(c(plan$logic, plan$browser), discovered)
  expect_false(anyDuplicated(c(plan$logic, plan$browser)) > 0L)
})

test_that("weighted sharding remains deterministic and lossless", {
  files <- paste0("test-", letters[1:4], ".R")
  weights <- c(
    `test-a.R` = 8,
    `test-b.R` = 7,
    `test-c.R` = 6,
    `test-d.R` = 5
  )

  assigned <- ci_test_plan_api$ci_test_shards(
    files,
    2L,
    strategy = "weighted",
    weights = weights
  )

  expect_identical(
    assigned,
    list(c("test-a.R", "test-d.R"), c("test-b.R", "test-c.R"))
  )
  expect_setequal(unlist(assigned, use.names = FALSE), files)
})
