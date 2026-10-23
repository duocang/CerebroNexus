tcell_demo_env <- function() {
  helpers <- testthat::test_path(
    "..",
    "..",
    "data-raw",
    "large-demo",
    c("download_helpers.R", "prepare_viewer_tcell_data.R")
  )
  testthat::skip_if_not(
    all(file.exists(helpers)),
    "benchmark tree not present (expected when checking a built package)"
  )
  env <- new.env(parent = globalenv())
  for (helper in helpers) {
    sys.source(helper, envir = env)
  }
  env
}

## Cell table in the columns of the atlas's panC.freq.all.meta.tb.rds.
tcell_meta <- function() {
  data.frame(
    cellID = c("a1", "a2", "b1", "b2", "b3", "b4"),
    cellID.uniq = paste0("ds1.", c("a1", "a2", "b1", "b2", "b3", "b4")),
    patient.uid = c("P1", "P1", "P2", "P2", "P2", "P2"),
    cancerType = c("LC", "LC", "BRCA", "BRCA", "BRCA", "BRCA"),
    loc = c("T", "N", "T", "T", "P", "T"),
    dataset = "ds1",
    dataset.old = "ds1.old",
    stype = c("CD8", "CD4", "CD8", "CD8", "CD8", "CD4"),
    meta.cluster = c(
      "CD8.c01.Tn.MAL",
      "CD4.c01.Tn.TCF7",
      "CD8.c12.Tex.CXCL13",
      "CD8.c06.Tem.GZMK",
      "CD8.c03.Tm.RPS12",
      "CD4.c01.Tn.TCF7"
    ),
    meta.cluster.coarse = "x",
    miniCluster = c("ds1.C1", "ds1.C1", "ds1.C2", "ds1.C3", "ds1.C4", "ds1.C2"),
    tech = "10X",
    dataSource = "zhangLab",
    TCR = c("Prod", NA, "Prod", "None", NA, NA),
    Phase = "G1",
    S.Score = 0.1,
    G2M.Score = 0.2,
    percent.mito = 0.03,
    stringsAsFactors = FALSE
  )
}

tcell_clinical <- function() {
  data.frame(
    patient = c("P1", "P2"),
    cancer_name = c("Lung", "Breast"),
    age = c("60", "50"),
    gender = c("Male", "Female"),
    clinical_stage = c("II", "III"),
    stringsAsFactors = FALSE
  )
}

test_that("the T-cell demo registers the pan-cancer atlas", {
  helper <- viewer_test_path("million_cell_demo.R")
  env <- new.env(parent = baseenv())
  sys.source(helper, envir = env)

  original <- list(
    crb_file_to_load = c(Small = "small.crb"),
    point_size = c(Small = 5),
    point_opacity = c(Small = 1),
    percentage_cells_to_show = 100
  )
  expect_identical(env$viewerAddTcellDemo(original, ""), original)

  root <- withr::local_tempdir()
  crb <- file.path(root, "tcell.crb")
  file.create(crb, file.path(root, "tcell.h5"))
  configured <- env$viewerAddTcellDemo(original, crb)
  label <- "Zheng et al. pan-cancer T cells (398K + TCR)"
  expect_identical(
    unname(configured$crb_file_to_load[label]),
    normalizePath(crb)
  )
  expect_equal(unname(configured$point_size[label]), 1)
  expect_equal(unname(configured$point_opacity[label]), 0.5)
  expect_equal(unname(configured$percentage_cells_to_show[label]), 100)
  ## The earlier data sets keep their appearance.
  expect_equal(unname(configured$point_size["Small"]), 5)

  unlink(file.path(root, "tcell.h5"))
  expect_error(
    env$viewerAddTcellDemo(original, crb),
    "CEREBRO_TCELL_DEMO_CRB must name a .crb with an adjacent .h5 file.",
    fixed = TRUE
  )
})

test_that("the T-cell sources are the Zenodo files with their published sizes", {
  env <- tcell_demo_env()
  files <- env$.viewerTcellFiles()
  expect_named(files, c("expression", "metainfo", "tcr"))
  expect_identical(files$expression$size, 14004090334)
  expect_identical(files$metainfo$size, 60547827)
  expect_identical(files$tcr$size, 16783417)
  for (file in files) {
    expect_match(file$url, "^https://zenodo[.]org/api/records/5461803/files/")
    expect_match(file$md5, "^[0-9a-f]{32}$")
  }
  paths <- env$.viewerTcellPaths(withr::local_tempdir())
  expect_identical(basename(paths$expression), "data.expression.tar.gz")
  expect_identical(basename(dirname(paths$crb)), "cerebro-v1")
  expect_identical(
    dirname(paths$sidecar),
    dirname(paths$immune)
  )
})

test_that("the T-cell cell table keeps the published order and checks its size", {
  env <- tcell_demo_env()
  meta <- tcell_meta()
  metadata <- env$.viewerTcellReadCells(
    meta,
    tcell_clinical(),
    expected_cells = 6L,
    expected_patients = 2L
  )
  expect_identical(rownames(metadata), meta$cellID.uniq)
  expect_identical(metadata$cell_barcode, meta$cellID.uniq)
  expect_identical(as.character(metadata$gender), rep(c("Male", "Female"), c(2, 4)))
  expect_identical(as.character(metadata$lineage), meta$stype)
  expect_true(is.factor(metadata$meta_cluster))
  expect_true(is.numeric(metadata$percent_mito))

  expect_error(
    env$.viewerTcellReadCells(
      meta,
      tcell_clinical(),
      expected_cells = 7L,
      expected_patients = 2L
    ),
    "must describe 7 unique cells of 2 patients"
  )
  duplicated_cells <- meta
  duplicated_cells$cellID.uniq[[2]] <- duplicated_cells$cellID.uniq[[1]]
  expect_error(
    env$.viewerTcellReadCells(
      duplicated_cells,
      tcell_clinical(),
      expected_cells = 6L,
      expected_patients = 2L
    ),
    "unique cells"
  )
  expect_error(
    env$.viewerTcellReadCells(
      meta,
      tcell_clinical()[1, ],
      expected_cells = 6L,
      expected_patients = 2L
    ),
    "no clinical record"
  )
})

test_that("TCR rows become clone calls without NA and keep the chain order", {
  env <- tcell_demo_env()
  tcr <- data.frame(
    Cell_Name = c("a1", "b1", "b2", "b3"),
    dataset.old = "ds1.old",
    CDR3.A1 = c("CAAF", NA, NA, "CXXF"),
    CDR3.A2 = c(NA, "CDDF", NA, NA),
    CDR3.B1 = c("CBBF", "CEEF", NA, NA),
    CDR3.B2 = NA_character_,
    Identifier.A1 = c("TRAV1_AAA(T)CC_TRAJ2", NA, NA, "TRAV9_G_TRAJ1"),
    Identifier.A2 = c(NA, "TRAV3_TTT_TRAJ4", NA, NA),
    Identifier.B1 = c("TRBV5-1_GGG_TRBJ2-3", "TRBV6_CCC_TRBJ1-1", NA, NA),
    Identifier.B2 = NA_character_,
    stringsAsFactors = FALSE
  )
  cells <- env$.viewerTcellTcrCells(tcr, tcell_meta())
  expect_identical(cells, paste0("ds1.", c("a1", "b1", "b2", "b3")))
  repertoire <- env$.viewerTcellRepertoire(tcr, cells)
  ## b2 carries no chain at all and is dropped.
  expect_identical(repertoire$barcode, cells[c(1, 2, 4)])
  expect_identical(
    repertoire$CTgene,
    c("TRAV1.TRAJ2_TRBV5-1.TRBJ2-3", "TRAV3.TRAJ4_TRBV6.TRBJ1-1", "TRAV9.TRAJ1")
  )
  expect_identical(repertoire$CTaa, c("CAAF_CBBF", "CDDF_CEEF", "CXXF"))
  ## "_" only separates the chains, also in the nucleotide-level call.
  expect_identical(
    repertoire$CTnt[[1]],
    "TRAV1:AAA(T)CC:TRAJ2_TRBV5-1:GGG:TRBJ2-3"
  )
  expect_false(any(grepl("NA", unlist(repertoire[-1]), fixed = TRUE)))
  expect_identical(
    repertoire$CTstrict[[3]],
    paste("TRAV9.TRAJ1", "TRAV9:G:TRAJ1", sep = ";")
  )
  expect_named(
    repertoire,
    c("barcode", "CTgene", "CTnt", "CTaa", "CTstrict")
  )
})

test_that("a TCR cell name shared by two cells of the table is not assigned", {
  env <- tcell_demo_env()
  meta <- tcell_meta()
  meta$cellID[[2]] <- "a1"
  tcr <- data.frame(
    Cell_Name = c("a1", "b1"),
    dataset.old = "ds1.old",
    stringsAsFactors = FALSE
  )
  expect_identical(
    env$.viewerTcellTcrCells(tcr, meta),
    c(NA_character_, "ds1.b1")
  )
})

test_that("TCR cells are split by patient and tissue site and must be unique", {
  env <- tcell_demo_env()
  metadata <- env$.viewerTcellReadCells(
    tcell_meta(),
    tcell_clinical(),
    expected_cells = 6L,
    expected_patients = 2L
  )
  repertoire <- data.frame(
    barcode = c("ds1.a1", "ds1.b1", "ds1.b2", "ds1.b3", NA),
    CTgene = "g",
    CTnt = "n",
    CTaa = "a",
    CTstrict = "s",
    stringsAsFactors = FALSE
  )
  samples <- env$.viewerTcellRepertoireBySample(repertoire, metadata)
  expect_identical(names(samples), c("P1 (T)", "P2 (P)", "P2 (T)"))
  expect_identical(samples[["P2 (T)"]]$barcode, c("ds1.b1", "ds1.b2"))
  expect_identical(samples[["P2 (P)"]]$barcode, "ds1.b3")
  expect_identical(rownames(samples[["P1 (T)"]]), "1")
  repertoire$barcode[[2]] <- "ds1.a1"
  expect_error(
    env$.viewerTcellRepertoireBySample(repertoire, metadata),
    "unique cells"
  )
})

test_that("the published pseudotime is kept per cell and the trajectory ranks it", {
  env <- tcell_demo_env()
  meta <- tcell_meta()
  pseudotime <- data.frame(
    mini_cluster = c("ds1.C1", "ds1.C2", "ds1.C3"),
    meta.cluster = c(
      "CD8.c01.Tn.MAL",
      "CD8.c12.Tex.CXCL13",
      "CD8.c06.Tem.GZMK"
    ),
    dpt_pseudotime = c(0.1, 0.9, 0.5),
    stringsAsFactors = FALSE
  )
  projection <- data.frame(
    Dim_1 = 1:6,
    Dim_2 = 6:1,
    row.names = meta$cellID.uniq
  )
  trajectory <- env$.viewerTcellTrajectory(meta, pseudotime, projection)
  ## The CD4 cells of C1 and C2 and the CD8 cell of C4 (not on the path) stay out.
  expect_identical(
    rownames(trajectory$meta),
    paste0("ds1.", c("a1", "b1", "b2"))
  )
  published <- env$.viewerTcellCellPseudotime(meta, pseudotime)
  expect_identical(names(published), meta$cellID.uniq)
  expect_equal(unname(published), c(0.1, NA, 0.9, 0.5, NA, NA))
  ## Rank scaled to (0, 1]: the order of the published values is kept.
  expect_equal(trajectory$meta$pseudotime, c(1 / 3, 1, 2 / 3))
  expect_identical(
    levels(trajectory$meta$state),
    c("CD8.c01.Tn.MAL", "CD8.c06.Tem.GZMK", "CD8.c12.Tex.CXCL13")
  )
  expect_equal(trajectory$meta$DR_1, c(1, 3, 4))
  expect_true(all(
    c("DR_1", "DR_2", "pseudotime", "state") %in% names(trajectory$meta)
  ))
  expect_identical(trajectory$provenance$type, "published_pseudotime")
  ## The line follows the state order of the pseudotime.
  expect_identical(trajectory$edges$source, levels(trajectory$meta$state)[1:2])
  expect_identical(trajectory$edges$target, levels(trajectory$meta$state)[2:3])
  expect_equal(trajectory$edges$source_dim_1, c(1, 4))
  expect_equal(trajectory$edges$target_dim_1, c(4, 3))
})

test_that("per-data-set matrices are joined on the union of their genes", {
  skip_if_not_installed("Matrix")
  env <- tcell_demo_env()
  first <- Matrix::sparseMatrix(
    i = c(1, 2),
    j = c(1, 2),
    x = c(1, 2),
    dims = c(2, 2),
    dimnames = list(c("G2", "G1"), c("c1", "c2"))
  )
  second <- Matrix::sparseMatrix(
    i = c(1, 2),
    j = c(1, 1),
    x = c(5, 6),
    dims = c(2, 1),
    dimnames = list(c("G3", "G2"), "c3")
  )
  never <- Matrix::sparseMatrix(
    i = integer(),
    j = integer(),
    x = numeric(),
    dims = c(1, 1),
    dimnames = list("G9", "c4")
  )
  combined <- env$.viewerTcellCombine(
    list(first, second, never),
    c("c3", "c1", "c4", "c2")
  )
  ## G9 is expressed nowhere and is dropped; columns follow the cell table.
  expect_identical(rownames(combined), c("G1", "G2", "G3"))
  expect_identical(colnames(combined), c("c3", "c1", "c4", "c2"))
  expect_equal(as.matrix(combined)["G2", ], c(c3 = 6, c1 = 1, c4 = 0, c2 = 0))
  expect_equal(as.matrix(combined)["G3", "c3"], 5)
  expect_equal(as.matrix(combined)["G1", "c2"], 2)
  expect_error(
    env$.viewerTcellCombine(list(first), c("c1", "missing")),
    "lack 1 cells"
  )
  ## G2 is expressed by two cells, G1 and G3 by one each.
  frequent <- env$.viewerTcellCombine(
    list(first, second),
    c("c1", "c2", "c3"),
    min_cells = 2L
  )
  expect_identical(rownames(frequent), "G2")
})

test_that("only gene symbols are kept from the mixed gene naming", {
  env <- tcell_demo_env()
  genes <- c(
    "CD3E",
    "ENSG00000012660",
    "5S-rRNA-ENSG00000212595",
    "9588",
    "RP11-529J17.3",
    "AC078788.1",
    "HLA-DRB1"
  )
  expect_identical(
    env$.viewerTcellIsSymbol(genes),
    c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
  )
})

test_that("an expression object needs the normalised matrix and unique cells", {
  skip_if_not_installed("SingleCellExperiment")
  env <- tcell_demo_env()
  counts <- Matrix::sparseMatrix(
    i = c(1, 2, 3, 3),
    j = c(1, 2, 2, 1),
    x = c(1, 2, 3, 4),
    dims = c(3, 2),
    dimnames = list(c("A", "B", "C"), c("x", "y"))
  )
  sce <- SingleCellExperiment::SingleCellExperiment(
    assays = list(norm_exprs = counts),
    colData = S4Vectors::DataFrame(cellID.uniq = c("u1", "u2"))
  )
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(sce, path)
  norm <- env$.viewerTcellReadDataset(path)
  expect_identical(colnames(norm), c("u1", "u2"))
  expect_identical(sort(rownames(norm)), c("A", "B", "C"))
  expect_equal(norm["C", "u2"], 3)

  bare <- SingleCellExperiment::SingleCellExperiment(
    assays = list(logcounts = counts),
    colData = S4Vectors::DataFrame(cellID.uniq = c("u1", "u2"))
  )
  saveRDS(bare, path)
  expect_error(env$.viewerTcellReadDataset(path), "norm_exprs")

  repeated <- sce
  SummarizedExperiment::colData(repeated)$cellID.uniq <- c("u1", "u1")
  saveRDS(repeated, path)
  expect_error(env$.viewerTcellReadDataset(path), "unique cell names")
})

test_that("Ensembl-keyed objects are renamed to their symbols", {
  skip_if_not_installed("SingleCellExperiment")
  env <- tcell_demo_env()
  norm <- Matrix::sparseMatrix(
    i = c(1, 2, 3, 3),
    j = c(1, 2, 1, 2),
    x = c(1, 2, 3, 4),
    dims = c(3, 2),
    dimnames = list(c("ENSG00000001", "ENSG00000002", "ENSG00000003"), c("x", "y"))
  )
  sce <- SingleCellExperiment::SingleCellExperiment(
    assays = list(norm_exprs = norm),
    colData = S4Vectors::DataFrame(cellID.uniq = c("u1", "u2"))
  )
  ## ENSG00000002 and ENSG00000003 share a symbol.
  SummarizedExperiment::rowData(sce)$display.name <- c("CD3E", "CD8A", "CD8A")
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(sce, path)
  renamed <- env$.viewerTcellReadDataset(path)
  expect_setequal(rownames(renamed), c("CD3E", "CD8A"))
  expect_equal(renamed["CD3E", "u1"], 1)
  ## ENSG00000003 is detected in two cells, ENSG00000002 in one.
  expect_equal(renamed["CD8A", ], c(u1 = 3, u2 = 4))

  ## Nothing but identifiers: the object is rejected.
  unnamed <- sce
  SummarizedExperiment::rowData(unnamed)$display.name <- rownames(norm)
  saveRDS(unnamed, path)
  expect_error(env$.viewerTcellReadDataset(path), "other than a symbol")
})

test_that("the joint embedding covers every cell and mixes the batches", {
  skip_if_not_installed("irlba")
  skip_if_not_installed("harmony")
  skip_if_not_installed("uwot")
  env <- tcell_demo_env()
  set.seed(1)
  n_genes <- 120
  n_cells <- 300
  batch <- rep(c("d1", "d2"), each = n_cells / 2)
  group <- rep(rep(1:2, each = n_cells / 4), 2)
  dense <- matrix(
    rpois(n_genes * n_cells, 0.5),
    n_genes,
    n_cells,
    dimnames = list(paste0("g", seq_len(n_genes)), paste0("c", seq_len(n_cells)))
  )
  dense[1:20, group == 2] <- dense[1:20, group == 2] + 3
  expression <- methods::as(log1p(dense), "CsparseMatrix")
  expression <- methods::as(expression, "generalMatrix")
  genes <- env$.viewerTcellVariableGenes(
    expression,
    batch,
    n = 30L,
    bins = 4L,
    min_cells = 20L
  )
  expect_length(genes, 30L)
  expect_true(any(paste0("g", 1:20) %in% genes))
  embedding <- env$.viewerTcellEmbedding(
    expression,
    batch,
    n_genes = 30L,
    n_pcs = 5L,
    threads = 1L
  )
  expect_identical(rownames(embedding), colnames(expression))
  expect_named(embedding, c("Dim_1", "Dim_2"))
  expect_false(anyNA(embedding))
})

test_that("T-cell preparation keeps the previous data set on failure", {
  env <- tcell_demo_env()
  root <- withr::local_tempdir()
  paths <- env$.viewerTcellPaths(root)
  env$.demoRequire <- function(...) NULL
  env$.viewerTcellFiles <- function() list()
  dir.create(dirname(paths$crb), recursive = TRUE)
  writeLines("previous", paths$crb)
  env$.viewerTcellBuildObject <- function(paths, work) {
    writeLines("partial", paths$crb)
    stop("simulated preparation failure")
  }
  expect_error(
    env$prepareViewerTcellDemoData(root),
    "simulated preparation failure"
  )
  expect_identical(readLines(paths$crb), "previous")
  expect_length(list.files(paths$root, pattern = "stage-"), 0L)

  env$.viewerTcellBuildObject <- function(paths, work) {
    writeLines("new", paths$crb)
    file.create(paths$sidecar, paths$immune)
    writeLines(as.character(env$.viewerTcellVersion), paths$stamp)
  }
  expect_identical(env$prepareViewerTcellDemoData(root), paths$crb)
  expect_identical(readLines(paths$crb), "new")
  ## A finished build of the current version is reused.
  env$.viewerTcellBuildObject <- function(...) stop("must reuse the cache")
  expect_identical(env$prepareViewerTcellDemoData(root), paths$crb)
  ## A different version stamp triggers a rebuild.
  writeLines("0", paths$stamp)
  expect_error(env$prepareViewerTcellDemoData(root), "must reuse the cache")
})
