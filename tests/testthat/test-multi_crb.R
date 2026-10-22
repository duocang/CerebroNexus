# test-multi_crb.R — unit tests for the multi-crb dataset switcher helper.
#
# match_dataset_id_by_url() lives in the Shiny app's utility_functions.R (sourced
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
match_dataset_id_by_url <- eval(
  viewer_r_assignment(ir_utils, "match_dataset_id_by_url")[[3]],
  new.env(parent = globalenv())
)

viewerDatasetIdsValid <- eval(
  viewer_r_assignment(ir_utils, "viewerDatasetIdsValid")[[3]],
  new.env(parent = globalenv())
)

test_that("several configured files need unique, non-empty names", {
  expect_true(viewerDatasetIdsValid(c("a", "b")))
  expect_false(viewerDatasetIdsValid(NULL))
  expect_false(viewerDatasetIdsValid(c("a", "")))
  expect_false(viewerDatasetIdsValid(c("a", NA)))
  expect_false(viewerDatasetIdsValid(c("a", "a")))
})

test_that("matches a dataset by its stable id", {
  files <- c(alpha = "/data/a.crb", beta = "/data/b.crb")
  expect_identical(match_dataset_id_by_url("beta", files), "beta")
})

test_that("file names are not dataset ids", {
  files <- c(alpha = "/data/a.crb", beta = "/data/b.crb")
  expect_identical(match_dataset_id_by_url("b.crb", files), "")
  expect_identical(match_dataset_id_by_url("b", files), "")
  expect_identical(match_dataset_id_by_url("/data/b.crb", files), "")
})

test_that("returns empty string when nothing matches or ids are absent", {
  expect_identical(
    match_dataset_id_by_url("nope", c(alpha = "/data/a.crb")),
    ""
  )
  expect_identical(
    match_dataset_id_by_url("a", c("/data/a.crb", "/data/b.crb")),
    ""
  )
  expect_identical(match_dataset_id_by_url("", c(alpha = "/data/a.crb")), "")
  expect_identical(
    match_dataset_id_by_url(NA_character_, c(alpha = "/data/a.crb")),
    ""
  )
})
