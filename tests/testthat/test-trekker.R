# test-trekker.R — Trekker storage and shared client helpers.
#
# Covers the parts of the feature that are pure and don't need a browser: the
# Cerebro `trekker` slot round-trip and the client helpers retained by Linked
# views. Bundle construction and its continuous fields are covered in
# test-coordinated-views.R.

# ---- R6 slot: addTrekker / getTrekker round-trip -------------------------- ##

test_that("getTrekker defaults to NULL for old .crb files", {
  obj <- Cerebro$new()
  # An object that predates the feature carries no Trekker space.
  expect_null(obj$getTrekker())
})

test_that("addTrekker stores the payload and getTrekker returns it verbatim", {
  obj <- Cerebro$new()
  payload <- list(
    barcodes = c("AAA", "CCC"),
    clusters = c(0L, 1L),
    moran = list(list(gene = "Plp1"), list(gene = "Mbp"))
  )
  obj$addTrekker(payload)
  # A non-NULL slot is what lets Linked views build a Trekker space.
  expect_false(is.null(obj$getTrekker()))
  expect_identical(obj$getTrekker(), payload)
})

test_that("addTrekker rejects a non-list", {
  obj <- Cerebro$new()
  expect_error(obj$addTrekker(42), "must be a list")
  expect_null(obj$getTrekker())
})

test_that("Trekker Viewer consumes Builder tissue alignment and palette", {
  path <- testthat::test_path("..", "..", "inst", "viewer", "www", "trekker.js")
  if (!file.exists(path)) {
    path <- system.file("viewer", "www", "trekker.js", package = "CerebroNexus")
  }
  client <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(client, "D.histology_image", fixed = TRUE)
  expect_match(client, "D.histology_image_bounds", fixed = TRUE)
  expect_match(client, "drawTissueImage", fixed = TRUE)
  expect_match(client, "D.builder_colors", fixed = TRUE)
})

test_that("the shared Trekker client rejects stale image loads", {
  path <- testthat::test_path("..", "..", "inst", "viewer", "www", "trekker.js")
  if (!file.exists(path)) {
    path <- system.file("viewer", "www", "trekker.js", package = "CerebroNexus")
  }
  client <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(client, "var dataGeneration = 0;", fixed = TRUE)
  expect_match(client, "var generation = ++dataGeneration;", fixed = TRUE)
  expect_match(client, "ps = 2.2;", fixed = TRUE)
  expect_match(client, "generation !== dataGeneration", fixed = TRUE)
})
