## Tests for the native h5ad (AnnData) route:
##   .h5ad -> readH5adAsSCE() -> convertH5adToCerebro() -> .crb
##
## The fixtures in fixtures/anndata/ were written by the real anndata/scanpy
## (see make_fixtures.py there); the tests need neither Python nor anndata.

skip_if_not_installed("rhdf5")
skip_if_not_installed("SingleCellExperiment")

fixture <- function(name) testthat::test_path("fixtures", "anndata", name)
read_h5ad <- function(path, ...) {
  suppressWarnings(suppressMessages(readH5adAsSCE(path, ...)))
}
read_fixture <- function(name, ...) read_h5ad(fixture(name), ...)
assay_x <- function(sce, name = "X") SummarizedExperiment::assay(sce, name)

test_that("a CSR h5ad is read as a genes x cells SCE with the right assays", {
  sce <- read_fixture("small_csr.h5ad")
  expect_s4_class(sce, "SingleCellExperiment")
  expect_identical(dim(sce), c(60L, 120L))
  expect_identical(SummarizedExperiment::assayNames(sce), c("X", "counts"))
  expect_identical(rownames(sce)[1:3], c("CD3D", "CD3E", "MS4A1"))
  expect_identical(colnames(sce)[1:2], c("cell000", "cell001"))

  counts <- assay_x(sce, "counts")
  x <- assay_x(sce)
  expect_s4_class(x, "dgCMatrix")
  ## counts are the integers they were written as
  expect_true(all(counts@x == round(counts@x)))
  ## self-checking truth: X is log1p of counts normalised to 1e4
  expected <- log1p(t(t(as.matrix(counts)) / colSums(as.matrix(counts))) * 1e4)
  expect_equal(unname(as.matrix(x)), unname(expected), tolerance = 1e-5)
})

test_that("CSC and dense layouts read identically to CSR", {
  csr <- assay_x(read_fixture("small_csr.h5ad"))
  csc <- assay_x(read_fixture("small_csc.h5ad"))
  dense <- assay_x(read_fixture("small_dense.h5ad"))
  expect_equal(as.matrix(csc), as.matrix(csr))
  expect_equal(as.matrix(dense), as.matrix(csr))
  expect_s4_class(csc, "dgCMatrix")
  expect_s4_class(dense, "dgCMatrix")
  ## the dense fixture carries no layers
  expect_identical(
    SummarizedExperiment::assayNames(read_fixture("small_dense.h5ad")),
    "X"
  )
})

test_that("obs columns of every AnnData encoding are decoded", {
  meta <- SummarizedExperiment::colData(read_fixture("small_csr.h5ad"))
  expect_true(is.factor(meta$cell_type))
  expect_identical(levels(meta$cell_type), c("T cell", "B cell", "Monocyte"))
  expect_identical(
    as.character(meta$cell_type[c(1, 41, 81)]),
    c("T cell", "B cell", "Monocyte")
  )
  expect_true(is.factor(meta$donor))
  expect_identical(levels(meta$donor), c("d0", "d1", "d2"))
  ## categorical with missing values: NaN on every 4th cell
  expect_identical(sum(is.na(meta$batch_note)), 30L)
  expect_true(is.na(meta$batch_note[[1]]))
  ## bool array
  expect_type(meta$is_flagged, "logical")
  expect_identical(sum(meta$is_flagged), 24L)
  ## nullable integer: missing on every 7th cell
  expect_type(meta$n_flag, "integer")
  expect_identical(sum(is.na(meta$n_flag)), 18L)
  expect_identical(meta$n_flag[[2]], 1L)
  expect_type(meta$total_counts, "double")
})

test_that("obsm becomes reducedDims: names mapped, 1-column entries skipped", {
  sce <- read_fixture("small_csr.h5ad")
  expect_setequal(SingleCellExperiment::reducedDimNames(sce), c("UMAP", "PCA"))
  umap <- SingleCellExperiment::reducedDim(sce, "UMAP")
  expect_identical(dim(umap), c(120L, 2L))
  expect_identical(rownames(umap), colnames(sce))
  expect_equal(unname(umap[1, ]), c(5, 0), tolerance = 1e-12)
  expect_identical(ncol(SingleCellExperiment::reducedDim(sce, "PCA")), 5L)
})

test_that("scanpy rank_genes_groups becomes one marker table per groupby", {
  markers <- read_fixture("small_csr.h5ad")@metadata$marker_genes
  expect_identical(names(markers), "scanpy: t-test")
  expect_identical(names(markers[[1]]), "cell_type")
  table <- markers[[1]][["cell_type"]]
  expect_identical(names(table)[1:2], c("cell_type", "gene"))
  expect_setequal(unique(table$cell_type), c("T cell", "B cell", "Monocyte"))
  expect_true(all(
    c("score", "avg_log2FC", "p_val", "p_val_adj") %in% names(table)
  ))
  expect_identical(nrow(table), 24L)
  ## the planted marker genes lead their own cell type
  top <- function(type) table$gene[table$cell_type == type][1:2]
  expect_true(all(top("T cell") %in% c("CD3D", "CD3E")))
  expect_true(all(top("B cell") %in% c("MS4A1", "CD79A")))
  expect_true(all(top("Monocyte") %in% c("LYZ", "S100A8")))
  ## files without scanpy results carry no marker payload
  expect_null(read_fixture("small_csc.h5ad")@metadata$marker_genes)
})

test_that("unsorted CSR indices are repaired, not rejected", {
  copy <- file.path(withr::local_tempdir(), "unsorted.h5ad")
  file.copy(fixture("small_csr.h5ad"), copy)
  indptr <- as.vector(rhdf5::h5read(copy, "X/indptr"))
  indices <- as.vector(rhdf5::h5read(copy, "X/indices"))
  data <- as.vector(rhdf5::h5read(copy, "X/data"))
  first <- seq(indptr[[1]] + 1L, indptr[[2]])
  skip_if(length(first) < 2L, "first cell has fewer than two non-zeros")
  indices[first] <- rev(indices[first])
  data[first] <- rev(data[first])
  rhdf5::h5write(as.integer(indices), copy, "X/indices")
  rhdf5::h5write(as.numeric(data), copy, "X/data")
  repaired <- assay_x(read_h5ad(copy))
  expect_true(methods::validObject(repaired))
  expect_equal(
    as.matrix(repaired),
    as.matrix(assay_x(read_fixture("small_csr.h5ad")))
  )
  ## also when read cells x genes
  skip_if_not_installed("DelayedArray")
  expect_equal(
    as.matrix(assay_x(read_h5ad(copy, .cells_by_genes = "X"))),
    as.matrix(repaired)
  )
})

test_that("one assay is read cells x genes and seen through a lazy transpose", {
  skip_if_not_installed("DelayedArray")
  for (name in c("small_csr.h5ad", "small_csc.h5ad", "small_dense.h5ad")) {
    plain <- read_fixture(name)
    lazy <- read_fixture(name, .cells_by_genes = "X")
    expect_identical(SummarizedExperiment::assayNames(lazy), "X")
    seed <- .cellsByGenesSeed(assay_x(lazy))
    expect_s4_class(seed, "dgCMatrix")
    expect_true(methods::validObject(seed))
    expect_identical(dimnames(seed), rev(dimnames(plain)))
    expect_equal(as.matrix(Matrix::t(seed)), as.matrix(assay_x(plain)))
  }
  ## a layer instead of X
  lazy <- read_fixture("small_csr.h5ad", .cells_by_genes = "counts")
  expect_identical(SummarizedExperiment::assayNames(lazy), "counts")
  expect_equal(
    as.matrix(assay_x(lazy, "counts")),
    as.matrix(assay_x(read_fixture("small_csr.h5ad"), "counts"))
  )
  ## an assay the file does not have: everything is read as usual
  expect_identical(
    SummarizedExperiment::assayNames(read_fixture(
      "small_csr.h5ad",
      .cells_by_genes = "nope"
    )),
    c("X", "counts")
  )
})

test_that("a CSR assay goes straight to a sidecar laid out as the writer lays it out", {
  skip_if_not_installed("HDF5Array")
  dir <- withr::local_tempdir()
  plain <- read_fixture("small_csr.h5ad")
  lazy <- read_fixture(
    "small_csr.h5ad",
    .cells_by_genes = "X",
    .sidecar_dir = dir
  )
  x <- assay_x(lazy)
  sidecar <- .tempSidecarOf(x)
  expect_true(file.exists(sidecar))
  expect_identical(
    dirname(normalizePath(sidecar, winslash = "/")),
    normalizePath(dir, winslash = "/")
  )
  expect_equal(as.matrix(x), as.matrix(assay_x(plain)))
  expect_identical(dimnames(x), dimnames(plain))

  ## every dataset as .writeTENxH5() writes the same matrix
  reference <- file.path(dir, "reference.h5")
  .writeTENxH5(
    methods::as(Matrix::t(assay_x(plain)), "CsparseMatrix"),
    reference
  )
  layout <- function(file, name) {
    fid <- rhdf5::H5Fopen(file)
    on.exit(rhdf5::H5Fclose(fid))
    d <- rhdf5::H5Dopen(fid, paste0("expression/", name))
    on.exit(rhdf5::H5Dclose(d), add = TRUE, after = FALSE)
    plist <- rhdf5::H5Dget_create_plist(d)
    type <- rhdf5::H5Dget_type(d)
    n_filters <- rhdf5::H5Pget_nfilters(plist)
    list(
      rhdf5::H5Sget_simple_extent_dims(rhdf5::H5Dget_space(d)),
      rhdf5::H5Pget_chunk(plist),
      vapply(
        seq_len(n_filters),
        function(i) rhdf5::H5Pget_filter(plist, i)[[2]],
        ""
      ),
      rhdf5::H5Tget_class(type),
      rhdf5::H5Tget_size(type)
    )
  }
  for (name in c("shape", "genes", "barcodes", "data", "indices", "indptr")) {
    expect_identical(
      layout(sidecar, name),
      layout(reference, name),
      info = name
    )
    expect_identical(
      rhdf5::h5read(sidecar, paste0("expression/", name)),
      rhdf5::h5read(reference, paste0("expression/", name)),
      info = name
    )
  }

  ## group summaries are read from the file
  meta <- as.data.frame(SummarizedExperiment::colData(plain))
  values <- unique(meta$cell_type)
  codes <- match(meta$cell_type, values)
  codes[c(2, 9)] <- NA
  expect_equal(
    .groupGeneStats(x, codes, length(values)),
    .groupGeneStats(assay_x(plain), codes, length(values))
  )
  .releaseTempSidecar(sidecar, delete = TRUE)
  expect_false(file.exists(sidecar))
})

test_that("group summaries made while the sidecar is written are used for the same codes only", {
  skip_if_not_installed("HDF5Array")
  dir <- withr::local_tempdir()
  plain <- read_fixture("small_csr.h5ad")
  x <- assay_x(read_fixture(
    "small_csr.h5ad",
    .cells_by_genes = "X",
    .sidecar_dir = dir,
    .sidecar_groups = c("cell_type", "donor", "not_a_column")
  ))
  sidecar <- .tempSidecarOf(x)
  withr::defer(.releaseTempSidecar(sidecar, delete = TRUE))
  meta <- as.data.frame(SummarizedExperiment::colData(plain))
  codes_of <- function(values) {
    values <- as.character(values)
    match(values, unique(values[!is.na(values)]))
  }
  for (group in c("cell_type", "donor")) {
    codes <- codes_of(meta[[group]])
    n <- max(codes, na.rm = TRUE)
    expect_false(is.null(.tempSidecarGroupStats(sidecar, codes, n)))
    ## the kept summaries, not a read of the file
    local({
      testthat::local_mocked_bindings(
        sidecarGroupStatsCpp = function(...) stop("the file was read")
      )
      expect_equal(
        .groupGeneStats(x, codes, n),
        .groupGeneStats(assay_x(plain), codes, n)
      )
    })
  }
  ## other codes read the file
  codes <- codes_of(meta$cell_type)
  codes[[1]] <- NA
  expect_null(.tempSidecarGroupStats(sidecar, codes, max(codes, na.rm = TRUE)))
  expect_equal(
    .groupGeneStats(x, codes, max(codes, na.rm = TRUE)),
    .groupGeneStats(assay_x(plain), codes, max(codes, na.rm = TRUE))
  )
})

test_that("unsorted indices go to the sidecar sorted; duplicated ones fall back to R", {
  skip_if_not_installed("HDF5Array")
  dir <- withr::local_tempdir()
  copy <- file.path(withr::local_tempdir(), "edited.h5ad")
  file.copy(fixture("small_csr.h5ad"), copy)
  indptr <- as.vector(rhdf5::h5read(copy, "X/indptr"))
  indices <- as.vector(rhdf5::h5read(copy, "X/indices"))
  data <- as.vector(rhdf5::h5read(copy, "X/data"))
  first <- seq(indptr[[1]] + 1L, indptr[[2]])
  skip_if(length(first) < 2L, "first cell has fewer than two non-zeros")
  reversed <- indices
  reversed[first] <- rev(indices[first])
  values <- data
  values[first] <- rev(data[first])
  rhdf5::h5write(as.integer(reversed), copy, "X/indices")
  rhdf5::h5write(as.numeric(values), copy, "X/data")
  x <- assay_x(read_h5ad(copy, .cells_by_genes = "X", .sidecar_dir = dir))
  sidecar <- .tempSidecarOf(x)
  expect_false(is.null(sidecar))
  expect_equal(as.matrix(x), as.matrix(assay_x(read_fixture("small_csr.h5ad"))))
  .releaseTempSidecar(sidecar, delete = TRUE)

  ## the same gene twice in a cell: the R reader sums them, no file is left
  twice <- indices
  twice[first[[2]]] <- twice[first[[1]]]
  rhdf5::h5write(as.integer(twice), copy, "X/indices")
  x <- assay_x(read_h5ad(copy, .cells_by_genes = "X", .sidecar_dir = dir))
  expect_null(.tempSidecarOf(x))
  expect_identical(list.files(dir), character(0))
})

test_that("group summaries agree for sparse, lazily transposed and dense input", {
  skip_if_not_installed("DelayedArray")
  sce <- read_fixture("small_csr.h5ad")
  meta <- as.data.frame(SummarizedExperiment::colData(sce))
  meta$cell_type[c(3, 7)] <- NA
  sparse <- assay_x(sce)
  lazy <- assay_x(read_fixture("small_csr.h5ad", .cells_by_genes = "X"))
  groups <- c("cell_type", "donor")
  ## a dense matrix takes the per-group subsets
  reference <- .convertComputeGroupSummaries(
    list(),
    meta,
    as.matrix(sparse),
    groups,
    FALSE
  )
  expect_equal(
    .convertComputeGroupSummaries(list(), meta, sparse, groups, FALSE),
    reference
  )
  expect_equal(
    .convertComputeGroupSummaries(list(), meta, lazy, groups, FALSE),
    reference
  )
  ## stored zeros and negative values do not count as expressing
  shifted <- sparse
  hit <- seq(1L, length(shifted@x), by = 7L)
  shifted@x[hit] <- rep_len(c(0, -1), length(hit))
  expect_equal(
    .convertComputeGroupSummaries(list(), meta, shifted, groups, FALSE),
    .convertComputeGroupSummaries(
      list(),
      meta,
      as.matrix(shifted),
      groups,
      FALSE
    )
  )

  ## either orientation gives the same numbers
  values <- unique(meta$cell_type[!is.na(meta$cell_type)])
  codes <- match(meta$cell_type, values)
  stats <- .groupGeneStats(sparse, codes, length(values))
  expect_identical(stats$n, tabulate(codes, length(values)))
  expect_equal(.groupGeneStats(lazy, codes, length(values)), stats)
  expect_null(.groupGeneStats(as.matrix(sparse), codes, length(values)))
})

test_that("readH5adAsSCE reports bad input clearly", {
  dir <- withr::local_tempdir()
  expect_error(readH5adAsSCE(file.path(dir, "nope.h5ad")), "not found")
  junk <- file.path(dir, "junk.h5ad")
  writeLines("not hdf5", junk)
  expect_error(readH5adAsSCE(junk), "not a readable HDF5")
  expect_error(
    readH5adAsSCE(fixture("small_csr.h5ad"), use_raw = TRUE),
    "no `raw/X`"
  )
})

test_that("convertH5adToCerebro exports the whole file, QC columns found by name", {
  out <- withr::local_tempdir()
  messages <- character()
  path <- withCallingHandlers(
    suppressWarnings(convertH5adToCerebro(
      fixture("small_csr.h5ad"),
      out,
      groups = c("cell_type", "donor", "batch_note"),
      viewer_binary = "never",
      verbose = FALSE
    )),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_false(any(grepl("Computed missing QC column", messages)))
  expect_identical(basename(path), "cerebro_small_csr.crb")
  obj <- readCerebro(path)
  expect_identical(obj$getExperiment()$experiment_name, "small_csr")
  expect_identical(nrow(obj$getMetaData()), 120L)
  expect_setequal(obj$getGroups(), c("cell_type", "donor", "batch_note"))
  expect_true("N/A" %in% obj$getGroupLevels("batch_note"))
  ## scanpy's total_counts / n_genes_by_counts were used as nUMI / nGene
  meta <- obj$getMetaData()
  counts <- as.matrix(assay_x(read_fixture("small_csr.h5ad"), "counts"))
  expect_equal(unname(meta$nUMI), unname(colSums(counts)), tolerance = 1e-5)
  expect_equal(unname(meta$nGene), unname(colSums(counts > 0)))
  ## UMAP only: PCA is dropped when a non-PCA embedding exists
  expect_identical(obj$availableProjections(), "UMAP")
  expect_identical(obj$getMethodsForMarkerGenes(), "scanpy: t-test")
  expect_identical(obj$getGroupsWithMarkerGenes("scanpy: t-test"), "cell_type")
  expect_setequal(obj$getGroupsWithMostExpressedGenes(), obj$getGroups())
})

test_that("convertH5adToCerebro picks layers and backends, rejects wrong calls", {
  skip_if_not_installed("HDF5Array")
  out <- withr::local_tempdir()
  path <- suppressMessages(suppressWarnings(convertH5adToCerebro(
    fixture("small_csr.h5ad"),
    out,
    assay = "counts",
    groups = "cell_type",
    expression_matrix_mode = "h5",
    viewer_binary = "never",
    verbose = FALSE
  )))
  expect_true(file.exists(file.path(out, "cerebro_small_csr.h5")))
  ## the sidecar written from the h5ad was moved, not left behind
  expect_false(any(grepl("^expression-", list.files(out))))
  obj <- readCerebro(path)
  counts <- as.matrix(assay_x(read_fixture("small_csr.h5ad"), "counts"))
  expect_equal(
    unname(as.matrix(obj$getExpressionBlock(c("CD3D", "LYZ")))),
    unname(counts[c("CD3D", "LYZ"), ])
  )
  ## the sidecar is the cells x genes CSC of the assay, sorted
  sidecar <- file.path(out, "cerebro_small_csr.h5")
  expected <- methods::as(
    Matrix::t(methods::as(counts, "CsparseMatrix")),
    "CsparseMatrix"
  )
  expect_identical(
    as.integer(rhdf5::h5read(sidecar, "expression/indptr")),
    expected@p
  )
  expect_identical(
    as.integer(rhdf5::h5read(sidecar, "expression/indices")),
    expected@i
  )
  expect_equal(
    as.numeric(rhdf5::h5read(sidecar, "expression/data")),
    expected@x
  )
  expect_equal(
    obj$getMostExpressedGenes("cell_type"),
    readCerebro(suppressMessages(suppressWarnings(convertH5adToCerebro(
      fixture("small_csr.h5ad"),
      withr::local_tempdir(),
      assay = "counts",
      groups = "cell_type",
      viewer_binary = "never",
      verbose = FALSE
    ))))$getMostExpressedGenes("cell_type")
  )

  ## a QC column the file lacks is computed from the counts layer, which
  ## needs the full read
  messages <- character()
  withCallingHandlers(
    suppressWarnings(convertH5adToCerebro(
      fixture("small_csr.h5ad"),
      withr::local_tempdir(),
      groups = "cell_type",
      nUMI = "umi_missing",
      expression_matrix_mode = "h5",
      viewer_binary = "never",
      verbose = FALSE
    )),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_true(any(grepl("from assay `counts`", messages)))

  expect_error(
    convertH5adToCerebro(fixture("small_csr.h5ad"), out),
    "Character/factor columns in colData"
  )
  expect_error(
    convertH5adToCerebro(
      fixture("small_csr.h5ad"),
      out,
      assay = "nope",
      groups = "cell_type"
    ),
    "Available: X, counts"
  )
  rds <- file.path(out, "x.rds")
  saveRDS(1, rds)
  expect_error(
    convertH5adToCerebro(rds, out, groups = "x"),
    "must be a .h5ad \\(or MuData .h5mu\\) file"
  )
})

test_that("convertSCEToCerebro accepts an .h5ad path directly", {
  out <- withr::local_tempdir()
  path <- suppressMessages(suppressWarnings(convertSCEToCerebro(
    fixture("small_csc.h5ad"),
    out,
    assay = "X",
    groups = "cell_type",
    viewer_binary = "never",
    verbose = FALSE
  )))
  expect_identical(basename(path), "cerebro_small_csc.crb")
})
