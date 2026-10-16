bench_root_full <- normalizePath(file.path("..", "benchmarks", "expression-backends", "scripts"), mustWork = FALSE)
full_source_lib <- file.path(bench_root_full, "benchmark.R")

skip_unless_full_source_deps <- function() {
  for (package in c("BPCells", "HDF5Array", "rhdf5", "CerebroNexus")) {
    testthat::skip_if_not_installed(package)
  }
}

full_source_fixture <- function(root) {
  m <- Matrix::Matrix(
    matrix(c(1, 0, 2, 0, 3, 0, 4, 0, 5, 6, 0, 7), nrow = 3),
    sparse = TRUE
  )
  m <- methods::as(m, "dgCMatrix")
  rownames(m) <- paste0("g", seq_len(nrow(m)))
  colnames(m) <- paste0("c", seq_len(ncol(m)))
  iterable <- BPCells::write_matrix_memory(m)
  tenx <- file.path(root, "fixture-10x.h5")
  anndata <- file.path(root, "fixture.h5ad")
  BPCells::write_matrix_10x_hdf5(iterable, tenx, type = "double")
  BPCells::write_matrix_anndata_hdf5(iterable, anndata, group = "X")
  list(matrix = m, tenx = tenx, anndata = anndata)
}

test_that("full-source adapters stay lazy and preserve orientation", {
  skip_unless_full_source_deps()
  source(full_source_lib, local = TRUE)
  root <- tempfile("full-source-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- full_source_fixture(root)

  tenx <- bench_open_full_source(list(kind = "tenx"), fixture$tenx)
  anndata <- bench_open_full_source(list(kind = "h5ad"), fixture$anndata)

  expect_s4_class(tenx, "IterableMatrix")
  expect_s4_class(anndata, "IterableMatrix")
  expect_equal(dim(tenx), dim(fixture$matrix))
  expect_equal(dim(anndata), dim(fixture$matrix))
  expect_identical(dimnames(tenx), dimnames(fixture$matrix))
  expect_identical(dimnames(anndata), dimnames(fixture$matrix))
  expect_error(
    bench_open_full_source(list(kind = "matrix"), fixture$matrix),
    "10x or AnnData"
  )
})

test_that("embedded scale input matches the frozen prefix and canonical genes", {
  skip_unless_full_source_deps()
  source(full_source_lib, local = TRUE)
  root <- tempfile("scale-input-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  m <- Matrix::Matrix(matrix(seq_len(24), nrow = 3), sparse = TRUE)
  dimnames(m) <- list(c("ENSG_1", "ENSG|2", "ENSG-2"), paste0("c", 1:8))
  path <- file.path(root, "fixture.h5ad")
  BPCells::write_matrix_anndata_hdf5(BPCells::write_matrix_memory(m), path, group = "X")
  spec <- list(kind = "h5ad")
  lazy <- bench_open_source_tier(spec, path, 4L)
  embedded <- bench_read_export_matrix(spec, path, 4L, "scale")
  expect_identical(dimnames(embedded), dimnames(lazy))
  expect_equal(colnames(embedded), paste0("c", 1:4))
  expect_equal(rownames(embedded), c("ENSG-1", "ENSG-2", "ENSG-2.1"))
  expect_equal(as.matrix(embedded), as.matrix(lazy))
  seurat <- bench_make_seurat(embedded)
  expect_identical(rownames(seurat), rownames(lazy))
  plan <- bench_build_lazy_query_plan(lazy, n_genes = 3L)
  obj <- list(
    getExpressionRow = function(gene, cells = NULL) {
      if (is.null(cells)) cells <- seq_len(ncol(embedded))
      as.numeric(embedded[gene, cells])
    },
    getExpressionBlock = function(genes, cells = NULL) {
      if (is.null(cells)) cells <- seq_len(ncol(embedded))
      embedded[genes, cells, drop = FALSE]
    }
  )
  expect_identical(bench_measure_backend(obj, plan, 1L)$correctness, "OK")
  crb <- file.path(root, "embedded.crb")
  CerebroNexus::exportFromSeurat(
    seurat, assay = "RNA", slot = "counts", file = crb,
    experiment_name = "scale fixture", organism = "hg", groups = c("sample", "cluster"),
    nUMI = "nUMI", nGene = "nGene", expression_matrix_mode = "embedded",
    viewer_binary = "never"
  )
  loaded <- CerebroNexus::readCerebro(crb)
  expect_identical(bench_measure_backend(loaded, plan, 1L)$correctness, "OK")
})

test_that("full-source writers round-trip both runtime backends", {
  skip_unless_full_source_deps()
  source(full_source_lib, local = TRUE)
  root <- tempfile("full-writer-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- full_source_fixture(root)
  source_matrix <- bench_open_full_source(list(kind = "tenx"), fixture$tenx)

  bpcells_path <- file.path(root, "matrix.bpcells")
  h5_path <- file.path(root, "matrix.h5")
  raw_path <- file.path(root, "matrix-uncompressed.h5")
  session_temp <- list.files(tempdir(), all.files = TRUE, no.. = TRUE)
  bench_write_full_backend(source_matrix, "bpcells", bpcells_path)
  bench_write_full_backend(source_matrix, "h5", h5_path)
  bench_write_full_backend(source_matrix, "h5_uncompressed", raw_path)

  # The H5 build's cell-major copy stays in scratch and does not outlive it.
  expect_setequal(
    list.files(tempdir(), all.files = TRUE, no.. = TRUE),
    session_temp
  )
  expect_length(list.files(root, pattern = "^(h5-transpose-|transpose_tmp)"), 0L)

  bpcells <- BPCells::open_matrix_dir(bpcells_path)
  expect_equal(as.matrix(bpcells), as.matrix(fixture$matrix))
  expect_identical(dimnames(bpcells), dimnames(fixture$matrix))
  expect_identical(BPCells::storage_order(bpcells), "row")
  for (path in c(h5_path, raw_path)) {
    h5 <- DelayedArray::t(HDF5Array::TENxMatrix(path, group = "expression"))
    expect_equal(as.matrix(h5), as.matrix(fixture$matrix), info = path)
    expect_identical(dimnames(h5), dimnames(fixture$matrix), info = path)
  }

  h5_filters <- function(path) {
    file <- rhdf5::H5Fopen(path, flags = "H5F_ACC_RDONLY")
    on.exit(rhdf5::H5Fclose(file), add = TRUE)
    data <- rhdf5::H5Dopen(file, "expression/data")
    on.exit(rhdf5::H5Dclose(data), add = TRUE, after = FALSE)
    plist <- rhdf5::H5Dget_create_plist(data)
    on.exit(rhdf5::H5Pclose(plist), add = TRUE, after = FALSE)
    vapply(seq_len(rhdf5::H5Pget_nfilters(plist)), function(i) {
      rhdf5::H5Pget_filter(plist, i)[[2L]]
    }, character(1))
  }
  # "h5" must match the package's compressed sidecar; the reference must not.
  expect_true("deflate" %in% h5_filters(h5_path))
  expect_false("deflate" %in% h5_filters(raw_path))
})

test_that("full-source H5 writer stays on the lazy BPCells path", {
  skip_if_not_installed("HDF5Array")
  source(full_source_lib, local = TRUE)
  body <- paste(readLines(full_source_lib, warn = FALSE), collapse = "\n")

  expect_match(body, "BPCells::write_matrix_10x_hdf5", fixed = TRUE)
  expect_match(body, "rhdf5::H5Lmove", fixed = TRUE)
  expect_match(body, "gzip_level = bench_h5_gzip_level(backend)", fixed = TRUE)
  # rhdf5 does not report the deflate level, so pin it to the package exporter.
  expect_identical(
    bench_h5_gzip_level("h5"),
    HDF5Array::getHDF5DumpCompressionLevel()
  )
  expect_identical(bench_h5_gzip_level("h5_uncompressed"), 0L)
  expect_identical(bench_storage_backend("h5_uncompressed"), "h5")
  expect_identical(bench_storage_backend("bpcells"), "bpcells")
})

test_that("full-source query plan and portable shell use bounded expression", {
  skip_unless_full_source_deps()
  source(full_source_lib, local = TRUE)
  root <- tempfile("full-shell-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- full_source_fixture(root)
  source_matrix <- bench_open_full_source(list(kind = "tenx"), fixture$tenx)

  plan <- bench_build_lazy_query_plan(source_matrix, n_genes = 3L)
  expect_equal(nrow(plan$panel), 3L)
  expect_identical(plan$n_cells, ncol(fixture$matrix))
  expect_identical(plan$n_genes, nrow(fixture$matrix))

  sibling <- file.path(root, "bench.bpcells")
  crb <- file.path(root, "bench.crb")
  bench_write_full_backend(source_matrix, "bpcells", sibling)
  obj <- bench_make_full_shell(
    source_matrix,
    backend = "bpcells",
    location = basename(sibling),
    source_name = "fixture",
    organism = "mm10",
    run_id = "run-1"
  )
  CerebroNexus::saveCerebro(obj, crb)
  payload <- CerebroNexus:::.readCerebroPayload(crb)
  loaded <- CerebroNexus::readCerebro(crb)

  expect_null(payload$expression)
  expect_identical(payload$crb_schema$version, 2L)
  expect_equal(nrow(loaded$getMetaData()), ncol(fixture$matrix))
  expect_identical(loaded$getMetaData()$cell_barcode, colnames(fixture$matrix))
  expect_true(all(c("nUMI", "nGene") %in% names(loaded$getMetaData())))
  expect_equal(nrow(loaded$getProjection("benchmark")), ncol(fixture$matrix))
  expect_identical(loaded$getExperiment()$organism, "mm10")
  expect_identical(loaded$getExpressionBackend()$location, "bench.bpcells")

  metrics <- bench_measure_backend(loaded, plan, hot_iterations = 1L)
  expect_identical(metrics$correctness, "OK")

  # Both H5 variants are opened by the package through its "h5" backend.
  for (backend in c("h5", "h5_uncompressed")) {
    h5_sibling <- file.path(root, paste0(backend, ".h5"))
    h5_crb <- file.path(root, paste0(backend, ".crb"))
    bench_write_full_backend(source_matrix, backend, h5_sibling)
    h5_obj <- bench_make_full_shell(
      source_matrix,
      backend = backend,
      location = basename(h5_sibling),
      source_name = "fixture",
      organism = "mm10",
      run_id = "run-1"
    )
    CerebroNexus::saveCerebro(h5_obj, h5_crb)
    h5_attached <- CerebroNexus::readCerebro(h5_crb)
    expect_identical(h5_attached$getExpressionBackend()$type, "h5", info = backend)
    h5_metrics <- bench_measure_backend(h5_attached, plan, hot_iterations = 1L)
    expect_identical(h5_metrics$correctness, "OK", info = backend)
  }
})
