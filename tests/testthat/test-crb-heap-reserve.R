crb_cache <- new.env()
sys.source(viewer_test_path("crb_cache.R"), crb_cache)

test_that("only a large configured CRB reserves heap", {
  small <- withr::local_tempfile(fileext = ".crb")
  writeBin(raw(1024), small)
  expect_false(crb_cache$.reserveCrbHeap(NULL))
  expect_false(crb_cache$.reserveCrbHeap(c(
    small,
    file.path(tempdir(), "missing.crb"),
    "variable_name"
  )))
  large <- withr::local_tempfile(fileext = ".crb")
  writeBin(raw(17 * 1024^2), large)
  expect_true(crb_cache$.reserveCrbHeap(c(small, large)))
})
