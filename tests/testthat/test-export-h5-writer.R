## .writeTENxH5() must leave the file writeTENxMatrix() leaves: the Viewer reads
## single genes by hyperslab from /expression/{indptr,indices,data}, and those
## reads depend on the datasets' types, chunk shapes, filters and maximum sizes.

h5_writer_matrix <- function(
  n_cells = 90L,
  n_genes = 40L,
  density = 0.3,
  utf8 = FALSE
) {
  set.seed(11)
  genes_by_cells <- Matrix::rsparsematrix(
    n_genes,
    n_cells,
    density,
    rand.x = function(n) round(rexp(n, 0.3), 3) + 0.1
  )
  dimnames(genes_by_cells) <- list(
    paste0(if (utf8) "gène_" else "g", seq_len(n_genes)),
    paste0("cell", seq_len(n_cells), "-1")
  )
  ## cells x genes, the orientation the sidecar is stored in
  methods::as(Matrix::t(genes_by_cells), "CsparseMatrix")
}

h5_writer_layout <- function(file, dataset) {
  handle <- rhdf5::H5Fopen(file, flags = "H5F_ACC_RDONLY")
  on.exit(rhdf5::H5Fclose(handle), add = TRUE)
  dset <- rhdf5::H5Dopen(handle, paste0("expression/", dataset))
  on.exit(rhdf5::H5Dclose(dset), add = TRUE)
  space <- rhdf5::H5Dget_space(dset)
  on.exit(rhdf5::H5Sclose(space), add = TRUE)
  plist <- rhdf5::H5Dget_create_plist(dset)
  on.exit(rhdf5::H5Pclose(plist), add = TRUE)
  type <- rhdf5::H5Dget_type(dset)
  extent <- rhdf5::H5Sget_simple_extent_dims(space)
  list(
    dim = extent$size,
    maxdim = extent$maxsize,
    chunk = rhdf5::H5Pget_chunk(plist),
    n_filters = rhdf5::H5Pget_nfilters(plist),
    type_class = rhdf5::H5Tget_class(type),
    type_size = rhdf5::H5Tget_size(type),
    type_precision = rhdf5::H5Tget_precision(type)
  )
}

expect_same_tenx_h5 <- function(written, expected, info = NULL) {
  datasets <- c("data", "indices", "indptr", "shape", "genes", "barcodes")
  expect_setequal(
    rhdf5::h5ls(written, recursive = TRUE)$name,
    rhdf5::h5ls(expected, recursive = TRUE)$name
  )
  for (dataset in datasets) {
    expect_identical(
      as.vector(rhdf5::h5read(
        written,
        paste0("expression/", dataset),
        bit64conversion = "double"
      )),
      as.vector(rhdf5::h5read(
        expected,
        paste0("expression/", dataset),
        bit64conversion = "double"
      )),
      info = paste(info, dataset, "values")
    )
    expect_identical(
      h5_writer_layout(written, dataset),
      h5_writer_layout(expected, dataset),
      info = paste(info, dataset, "layout")
    )
  }
}

test_that(".writeTENxH5 writes the file writeTENxMatrix writes", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("rhdf5")
  fixtures <- list(
    tiny = h5_writer_matrix(7L, 5L, 0.5),
    ## more values than one chunk, a count that is not a multiple of it
    wide = h5_writer_matrix(3000L, 200L, 0.12),
    utf8 = h5_writer_matrix(30L, 40L, 0.3, utf8 = TRUE)
  )
  for (name in names(fixtures)) {
    for (level in c(0L, 6L)) {
      expected <- withr::local_tempfile(fileext = ".h5")
      written <- withr::local_tempfile(fileext = ".h5")
      HDF5Array::writeTENxMatrix(
        fixtures[[name]],
        expected,
        group = "expression",
        level = level
      )
      .writeTENxH5(
        fixtures[[name]],
        written,
        group = "expression",
        level = level
      )
      expect_same_tenx_h5(written, expected, info = paste(name, "level", level))
    }
  }
})

test_that(".writeTENxH5 writes values past one slab, and slabs that do not divide the data", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("rhdf5")
  testthat::local_mocked_bindings(.TENX_SLAB_LENGTH = 1000L)
  matrix <- h5_writer_matrix(900L, 60L, 0.2)
  expect_gt(length(matrix@x), 3 * 1000L)
  expect_false(length(matrix@x) %% 1000L == 0L)

  expected <- withr::local_tempfile(fileext = ".h5")
  written <- withr::local_tempfile(fileext = ".h5")
  HDF5Array::writeTENxMatrix(matrix, expected, group = "expression", level = 0L)
  .writeTENxH5(matrix, written, group = "expression", level = 0L)
  expect_same_tenx_h5(written, expected)
})

test_that(".writeTENxH5 is read back by the Cerebro readers and by TENxMatrix", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("rhdf5")
  matrix <- h5_writer_matrix()
  written <- withr::local_tempfile(fileext = ".h5")
  .writeTENxH5(matrix, written, group = "expression", level = 0L)

  expect_identical(.h5CellNames(written), rownames(matrix))
  expect_identical(.h5GeneNames(written), colnames(matrix))
  expect_identical(.h5Dim(written), dim(matrix))
  seed <- HDF5Array::TENxMatrix(written, group = "expression")
  expect_identical(
    as.matrix(seed),
    as.matrix(matrix)
  )
})

test_that(".writeTENxH5 keeps the HDF5Array path for what it does not handle", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("DelayedArray")
  skip_if_not_installed("rhdf5")
  matrix <- h5_writer_matrix()
  expect_true(.canWriteTENxDirect(matrix))

  empty <- methods::as(
    Matrix::Matrix(
      0,
      nrow = 4L,
      ncol = 3L,
      sparse = TRUE,
      dimnames = list(letters[1:4], LETTERS[1:3])
    ),
    "CsparseMatrix"
  )
  unnamed <- matrix
  dimnames(unnamed) <- NULL
  delayed <- DelayedArray::DelayedArray(matrix)
  expect_false(.canWriteTENxDirect(empty))
  expect_false(.canWriteTENxDirect(unnamed))
  expect_false(.canWriteTENxDirect(delayed))
  expect_false(.canWriteTENxDirect(as.matrix(matrix)))

  ## the delayed input reaches writeTENxMatrix and writes the same values
  expected <- withr::local_tempfile(fileext = ".h5")
  written <- withr::local_tempfile(fileext = ".h5")
  HDF5Array::writeTENxMatrix(matrix, expected, group = "expression", level = 0L)
  .writeTENxH5(delayed, written, group = "expression", level = 0L)
  expect_same_tenx_h5(written, expected)
})
