# test-trekker.R — Trekker single-cell spatial-mapping page.
#
# Covers the Cerebro_v1.3 `trekker` slot contract consumed by Linked views.

# ---- R6 slot: addTrekker / getTrekker round-trip -------------------------- ##

test_that("getTrekker defaults to NULL for objects without Trekker data", {
  obj <- Cerebro_v1.3$new()
  expect_null(obj$getTrekker())
})

test_that("addTrekker stores the payload and getTrekker returns it verbatim", {
  obj <- Cerebro_v1.3$new()
  payload <- list(
    barcodes = c("AAA", "CCC"),
    clusters = c(0L, 1L),
    moran = list(list(gene = "Plp1"), list(gene = "Mbp"))
  )
  obj$addTrekker(payload)
  expect_false(is.null(obj$getTrekker()))
  expect_identical(obj$getTrekker(), payload)
})

test_that("addTrekker rejects a non-list", {
  obj <- Cerebro_v1.3$new()
  expect_error(obj$addTrekker(42), "must be a list")
  expect_null(obj$getTrekker())
})
