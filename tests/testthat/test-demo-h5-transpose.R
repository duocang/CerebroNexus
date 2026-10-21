test_that("the demo H5 writer matches writeTENxMatrix on a cell-major source", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("qs2")
  helper <- testthat::test_path("..", "..", "data-raw", "large-demo", "h5_transposed_write.R")
  skip_if_not(file.exists(helper))
  env <- new.env(parent = globalenv())
  sys.source(helper, envir = env)

  set.seed(2)
  genes_by_cells <- abs(Matrix::rsparsematrix(40L, 700L, 0.1))
  genes_by_cells@x <- round(genes_by_cells@x * 10) + 1
  dimnames(genes_by_cells) <- list(paste0("g", 1:40), paste0("c", 1:700))
  expected <- withr::local_tempfile(fileext = ".h5")
  written <- withr::local_tempfile(fileext = ".h5")
  HDF5Array::writeTENxMatrix(
    methods::as(Matrix::t(genes_by_cells), "CsparseMatrix"),
    expected,
    group = "expression",
    level = 0L
  )
  ## chunks and gene blocks that do not divide the matrix evenly
  env$.demoWriteH5Transposed(
    genes_by_cells,
    written,
    chunk_cells = 250L,
    gene_blocks = 6L,
    verbose = FALSE
  )

  for (dataset in c("data", "indices", "indptr", "shape", "genes", "barcodes")) {
    expect_identical(
      as.vector(rhdf5::h5read(written, paste0("expression/", dataset))),
      as.vector(rhdf5::h5read(expected, paste0("expression/", dataset))),
      info = dataset
    )
  }
  expect_identical(
    as.matrix(HDF5Array::TENxMatrix(written, group = "expression")),
    as.matrix(HDF5Array::TENxMatrix(expected, group = "expression"))
  )
  expect_identical(.h5CellNames(written), colnames(genes_by_cells))
  expect_identical(.h5GeneNames(written), rownames(genes_by_cells))
  expect_length(
    list.files(dirname(written), pattern = "^h5-transpose-"),
    0L
  )
})
