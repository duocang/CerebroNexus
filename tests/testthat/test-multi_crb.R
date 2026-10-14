# test-multi_crb.R — unit tests for the multi-crb dataset switcher helper.
#
# match_dataset_by_url() lives in the Shiny app's utility_functions.R (sourced
# at app runtime, not exported from the package). We source that file directly
# so the helper can be tested in isolation, without launching the full app.

# Prefer the source tree copy (always current); fall back to the installed
# package location when tests run against an installed build.
ir_utils_candidates <- c(
  testthat::test_path("../../inst/viewer/utility_functions.R"),
  file.path(
    system.file(package = "CerebroNexus"),
    "viewer",
    "utility_functions.R"
  )
)
ir_utils <- ir_utils_candidates[file.exists(ir_utils_candidates)][1]
testthat::skip_if(
  is.na(ir_utils),
  "utility_functions.R not found in source tree or installed package"
)

# Load a parsed declaration, independent of layout and braces in comments.
match_dataset_by_url <- eval(
  viewer_r_assignment(ir_utils, "match_dataset_by_url")[[3]],
  new.env(parent = globalenv())
)

test_that("matches a named dataset by its name", {
  files <- c(alpha = "/data/a.crb", beta = "/data/b.crb")
  expect_equal(
    match_dataset_by_url("beta", files, names(files)),
    "/data/b.crb"
  )
})

test_that("matches by basename when names are absent", {
  files <- c("/data/a.crb", "/data/b.crb")
  expect_equal(
    match_dataset_by_url("b.crb", files, NULL),
    "/data/b.crb"
  )
})

test_that("matches by basename without extension", {
  files <- c("/data/a.crb", "/data/b.crb")
  expect_equal(
    match_dataset_by_url("a", files, NULL),
    "/data/a.crb"
  )
})

test_that("returns empty string when nothing matches", {
  files <- c(alpha = "/data/a.crb")
  expect_identical(
    match_dataset_by_url("nope", files, names(files)),
    ""
  )
})

test_that("name match takes precedence over basename", {
  # a token that is both a name and a different file's basename resolves by name
  files <- c(b = "/data/x.crb", other = "/data/b.crb")
  expect_equal(
    match_dataset_by_url("b", files, names(files)),
    "/data/x.crb"
  )
})
