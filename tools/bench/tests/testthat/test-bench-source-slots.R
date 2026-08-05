test_that("source matrices populate their declared Seurat expression layer", {
  testthat::skip_if_not_installed("Matrix")
  testthat::skip_if_not_installed("Seurat")
  testthat::skip_if_not_installed("SeuratObject")
  bench_root <- normalizePath(file.path("..", ".."), mustWork = TRUE)
  source(file.path(bench_root, "lib", "make_seurat.R"), local = TRUE)

  matrix <- Matrix::sparseMatrix(
    i = c(1, 2, 3),
    j = c(1, 2, 2),
    x = c(1, 2.5, 4),
    dims = c(3, 2),
    dimnames = list(c("g1", "g2", "g3"), c("c1", "c2"))
  )

  counts_object <- bench_make_seurat(matrix, slot = "counts")
  expect_equal(
    SeuratObject::LayerData(counts_object, assay = "RNA", layer = "counts"),
    matrix
  )

  data_object <- bench_make_seurat(matrix, slot = "data")
  expect_equal(
    SeuratObject::LayerData(data_object, assay = "RNA", layer = "data"),
    matrix
  )
  expect_error(bench_make_seurat(matrix, slot = "scale.data"), "counts or data")
})
