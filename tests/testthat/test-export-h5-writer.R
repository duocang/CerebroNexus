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

## `unfiltered`: data and indices of `written` have no filters (level 0)
## where `expected`, from writeTENxMatrix(), has shuffle and deflate
expect_same_tenx_h5 <- function(
  written,
  expected,
  info = NULL,
  unfiltered = FALSE
) {
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
    layout_written <- h5_writer_layout(written, dataset)
    layout_expected <- h5_writer_layout(expected, dataset)
    if (unfiltered && dataset %in% c("data", "indices")) {
      expect_identical(
        layout_written$n_filters,
        0L,
        info = paste(info, dataset, "filters")
      )
      layout_written$n_filters <- NULL
      layout_expected$n_filters <- NULL
    }
    expect_identical(
      layout_written,
      layout_expected,
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
      expect_same_tenx_h5(
        written,
        expected,
        info = paste(name, "level", level),
        unfiltered = level == 0L
      )
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
  expect_same_tenx_h5(written, expected, unfiltered = TRUE)
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

test_that("a genes x cells dgCMatrix goes to the sidecar by compiled code, as .writeTENxH5 writes its transpose", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("rhdf5")
  fixtures <- list(
    tiny = h5_writer_matrix(7L, 5L, 0.5),
    wide = h5_writer_matrix(3000L, 200L, 0.12),
    utf8 = h5_writer_matrix(30L, 40L, 0.3, utf8 = TRUE)
  )
  for (name in names(fixtures)) {
    cells_by_genes <- fixtures[[name]]
    genes_by_cells <- methods::as(Matrix::t(cells_by_genes), "CsparseMatrix")
    for (level in c(0L, 6L)) {
      expected <- withr::local_tempfile(fileext = ".h5")
      written <- withr::local_tempfile(fileext = ".h5")
      .writeTENxH5(
        cells_by_genes,
        expected,
        group = "expression",
        level = level
      )
      expect_true(.writeSidecarFromGenesByCells(
        genes_by_cells,
        written,
        group = "expression",
        level = level
      ))
      expect_same_tenx_h5(written, expected, info = paste(name, "level", level))
    }
  }
})

test_that("the compiled sidecar writer leaves to the R path what it does not handle", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("DelayedArray")
  skip_if_not_installed("rhdf5")
  genes_by_cells <- methods::as(
    Matrix::t(h5_writer_matrix()),
    "CsparseMatrix"
  )
  unnamed <- genes_by_cells
  dimnames(unnamed) <- NULL
  ## a gene listed twice for a cell, which no valid dgCMatrix holds
  twice <- genes_by_cells
  first <- seq(twice@p[[1]] + 1L, twice@p[[2]])
  skip_if(length(first) < 2L, "first cell has fewer than two values")
  twice@i[first[[2]]] <- twice@i[first[[1]]]
  for (case in list(
    unnamed = list(unnamed, "expression"),
    delayed = list(DelayedArray::DelayedArray(genes_by_cells), "expression"),
    other_group = list(genes_by_cells, "matrix"),
    twice = list(twice, "expression")
  )) {
    file <- withr::local_tempfile(fileext = ".h5")
    expect_false(.writeSidecarFromGenesByCells(
      case[[1]],
      file,
      group = case[[2]]
    ))
    expect_false(file.exists(file))
  }
})

test_that("Seurat and SCE exports write the same sidecar with and without the compiled writer", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("SingleCellExperiment")
  genes_by_cells <- methods::as(
    Matrix::t(h5_writer_matrix(300L, 50L, 0.2)),
    "CsparseMatrix"
  )
  sce <- SingleCellExperiment::SingleCellExperiment(
    assays = list(logcounts = genes_by_cells),
    colData = S4Vectors::DataFrame(
      cluster = rep(c("a", "b", "c"), length.out = ncol(genes_by_cells)),
      nUMI = 1,
      nGene = 1,
      row.names = colnames(genes_by_cells)
    ),
    reducedDims = list(
      UMAP = matrix(
        stats::runif(2L * ncol(genes_by_cells)),
        ncol = 2L,
        dimnames = list(colnames(genes_by_cells), c("UMAP_1", "UMAP_2"))
      )
    )
  )
  export <- function(dir) {
    suppressMessages(suppressWarnings(exportFromSCE(
      sce,
      file = file.path(dir, "x.crb"),
      experiment_name = "sidecar",
      organism = "hg",
      groups = "cluster",
      nUMI = "nUMI",
      nGene = "nGene",
      expression_matrix_mode = "h5",
      viewer_binary = "never",
      verbose = FALSE
    )))
    list.files(dir, "[.]h5$", full.names = TRUE)
  }
  compiled <- export(withr::local_tempdir())
  local({
    testthat::local_mocked_bindings(
      .writeSidecarFromGenesByCells = function(...) FALSE
    )
    reference <- export(withr::local_tempdir())
    expect_same_tenx_h5(compiled, reference)
  })
})
