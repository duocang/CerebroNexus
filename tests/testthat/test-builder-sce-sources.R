## SingleCellExperiment and AnnData files as Builder sources.
##
## They enter through builder_read_object(), which hands everything downstream a
## Seurat object, so the inspect / profile / export paths are the ones Seurat
## datasets already use.

skip_if_not_installed("rhdf5")
skip_if_not_installed("SingleCellExperiment")
skip_if_not_installed("SeuratObject")

builder_repo_source("io.R", local = globalenv())
builder_profile_source_runtime(local = globalenv())
builder_repo_source("inspect.R", local = globalenv())
builder_repo_source("adapters.R", local = globalenv())

fixture <- function(name) testthat::test_path("fixtures", "anndata", name)
quiet <- function(x) suppressWarnings(suppressMessages(x))

test_that("the format registry claims AnnData files next to the Seurat ones", {
  extensions <- builder_dataset_extensions()
  expect_true(all(c("rds", "qs2", "qs", "h5ad", "h5mu") %in% extensions))
  expect_identical(builder_match_format("a/b/data.h5ad")$id, "h5ad")
  expect_identical(builder_match_format("data.H5MU")$id, "h5mu")
})

test_that("an h5ad file is read as a Seurat object with its reductions and results", {
  result <- quiet(builder_read_object(fixture("small_csr.h5ad")))
  expect_null(result$error)
  expect_s4_class(result$object, "Seurat")
  expect_identical(result$format, "h5ad")
  expect_equal(ncol(result$object), 120)
  expect_true(all(c("PCA", "UMAP") %in% names(result$object@reductions)))
  expect_true("marker_genes" %in% names(result$object@misc))
  expect_true(all(c("cell_type", "donor") %in% colnames(result$object@meta.data)))
})

test_that("a SingleCellExperiment saved as .rds or .qs2 is read the same way", {
  sce <- quiet(readH5adAsSCE(fixture("small_csr.h5ad")))
  path <- file.path(withr::local_tempdir(), "sce.rds")
  saveRDS(sce, path)
  from_rds <- builder_read_object(path)
  expect_null(from_rds$error)
  expect_s4_class(from_rds$object, "Seurat")
  expect_identical(colnames(from_rds$object), colnames(sce))

  skip_if_not_installed("qs2")
  qs_path <- file.path(dirname(path), "sce.qs2")
  qs2::qs_save(sce, qs_path)
  from_qs <- builder_read_object(qs_path)
  expect_null(from_qs$error)
  expect_s4_class(from_qs$object, "Seurat")
})

test_that("objects that are neither Seurat nor SingleCellExperiment are still refused", {
  path <- file.path(withr::local_tempdir(), "number.rds")
  saveRDS(1L, path)
  expect_identical(
    builder_read_object(path)$error,
    "The file contains a integer object, not a Seurat object."
  )
})

test_that("counts, data layers and spatial coordinates are placed by what they hold", {
  sce <- quiet(readH5adAsSCE(fixture("small_spatial.h5ad")))
  seurat <- convertSCEToSeurat(sce)
  expect_identical(SeuratObject::Layers(seurat), "counts")
  expect_true("spatial" %in% names(seurat@reductions))
  expect_equal(
    unname(SeuratObject::Embeddings(seurat[["spatial"]])[, 1]),
    unname(SingleCellExperiment::reducedDim(sce, "spatial")[, 1])
  )

  SummarizedExperiment::assay(sce, "logcounts") <- log1p(SummarizedExperiment::assay(sce, "X"))
  SummarizedExperiment::assay(sce, "counts") <- SummarizedExperiment::assay(sce, "X")
  both <- convertSCEToSeurat(sce)
  expect_setequal(SeuratObject::Layers(both), c("counts", "data"))
})

test_that("an AnnData file goes through the Builder's own inspect path", {
  adapter <- builder_seurat_file_adapter(fixture("small_csr.h5ad"))
  expect_identical(adapter$format, "h5ad")
  inspected <- quiet(builder_adapter_inspect(adapter))
  profile <- inspected$profile
  expect_s3_class(profile, "builder_dataset_profile")
  expect_identical(profile$identity$cells$count, 120L)
  expect_true(profile$identity$cells$valid)
  expect_true(all(c("cell_type", "donor") %in% names(profile$groups$legacy$levels)))
})

test_that("the same data as Seurat and as AnnData gives the same profile shape", {
  sce <- quiet(readH5adAsSCE(fixture("small_csr.h5ad")))
  converted <- convertSCEToSeurat(sce)
  source <- list(type = "example", location = "x", fingerprint = "f", format = "rds")
  a <- quiet(builder_dataset_profile(converted, source))
  ## the Seurat object a user would have built by hand from the same parts
  manual <- SeuratObject::CreateSeuratObject(
    SeuratObject::LayerData(converted, "counts"),
    meta.data = converted@meta.data
  )
  b <- quiet(builder_dataset_profile(manual, source))
  expect_identical(names(a), names(b))
  expect_identical(a$schema_version, b$schema_version)
  expect_identical(a$identity$cells$ids, b$identity$cells$ids)
  expect_identical(a$identity$features$ids, b$identity$features$ids)
  expect_identical(names(a$groups), names(b$groups))
})

test_that("an exported .crb from the converted object matches the direct conversion", {
  skip_if_not_installed("Seurat")
  sce <- quiet(readH5adAsSCE(fixture("small_csr.h5ad")))
  seurat <- convertSCEToSeurat(sce)
  out <- withr::local_tempdir()
  via_seurat <- file.path(out, "via_seurat.crb")
  quiet(exportFromSeurat(
    seurat, assay = "RNA", slot = "counts", file = via_seurat,
    experiment_name = "x", organism = "hg", groups = c("cell_type", "donor"),
    nUMI = "nCount_RNA", nGene = "nFeature_RNA", verbose = FALSE,
    viewer_binary = "never"
  ))
  direct <- quiet(convertSCEToCerebro(
    sce, file.path(out, "direct"), assay = "X", groups = c("cell_type", "donor"),
    viewer_binary = "never", verbose = FALSE, .source_name = "small.h5ad"
  ))
  a <- readCerebro(via_seurat)
  b <- readCerebro(direct)
  expect_identical(a$getMetaData()$cell_barcode, b$getMetaData()$cell_barcode)
  expect_identical(sort(a$getGroups()), sort(b$getGroups()))
  expect_setequal(names(a$projections), names(b$projections))
  expect_identical(names(a$marker_genes), names(b$marker_genes))
  expect_identical(names(a$marker_genes[[1]]), names(b$marker_genes[[1]]))
})

## ---------------------------------------------------------------------------
## the whole Builder pipeline: file -> inspect -> snapshot -> plan -> build
## ---------------------------------------------------------------------------

builder_e2e_source_runtime(local = globalenv())

build_through_builder <- function(path, id, configure = identity) {
  inspected_object <- quiet(builder_read_object(path))$object
  record <- list(
    id = id,
    label = id,
    make = function() list(object = inspected_object)
  )
  entry <- builder_e2e_entry(record)
  root <- withr::local_tempdir(.local_envir = parent.frame())
  snapshot <- builder_snapshot_seurat(
    inspected_object,
    file.path(root, "snapshot"),
    available_bytes = 2^40
  )
  entry$snapshot <- snapshot
  entry <- configure(entry)
  plan <- builder_freeze_plan(list(entry), file.path(root, "release"), make_app = FALSE)
  expect_null(plan$error)
  stage <- file.path(root, "stage")
  dir.create(stage)
  result <- quiet(builder_execute_plan(plan, stage, stats::setNames(list(snapshot), id)))
  expect_identical(result$state, "success", info = result$error %||% "")
  list(result = result, entry = entry, crb = qs2::qs_read(result$built[[1L]]))
}

test_that("an AnnData file builds a .crb through the Builder, with its results", {
  skip_if_not_installed("qs2")
  built <- build_through_builder(fixture("small_csr.h5ad"), "h5ad_small")
  crb <- built$crb
  expect_s3_class(crb, "Cerebro")
  expect_equal(nrow(crb$getMetaData()), 120)
  expect_true(all(c("cell_type", "donor") %in% crb$getGroups()))
  expect_true(length(crb$projections) >= 1L)
  expect_true(length(crb$marker_genes) >= 1L)
})

test_that("a spatial AnnData file builds a spatial section through the Builder", {
  skip_if_not_installed("qs2")
  built <- build_through_builder(fixture("small_spatial.h5ad"), "h5ad_spatial")
  expect_true(length(built$crb$spatial) >= 1L)
})

test_that("scanpy / scVelo trajectories are offered and built by the Builder", {
  skip_if_not_installed("qs2")
  inspected_trajectories <- quiet(builder_read_object(
    fixture("small_trajectory.h5ad")
  ))$object@misc$trajectories
  built <- build_through_builder(
    fixture("small_trajectory.h5ad"), "h5ad_trajectory",
    configure = function(entry) {
      ## what the Configure page records when the user keeps these trajectories
      entry$settings$included_trajectories <- list(
        scvelo = names(inspected_trajectories$scvelo)[1L],
        paga = names(inspected_trajectories$paga)[1L]
      )
      entry$settings$default_trajectory <- list(
        method = "scvelo",
        name = names(inspected_trajectories$scvelo)[1L]
      )
      entry
    }
  )
  methods <- built$crb$getMethodsForTrajectories()
  expect_true(all(c("paga", "scvelo") %in% methods))
  expect_true(length(built$crb$getNamesOfTrajectories("scvelo")) >= 1L)
})

test_that("a scirpy repertoire is offered and built by the Builder", {
  skip_if_not_installed("qs2")
  built <- build_through_builder(fixture("small_airr.h5ad"), "h5ad_airr")
  expect_true(length(built$crb$immune_repertoire) >= 1L ||
    length(built$crb$getImmuneRepertoire()) >= 1L)
})

test_that("a normalised-only file still gets per-cell count columns", {
  sce <- quiet(readH5adAsSCE(fixture("small_csr.h5ad")))
  SummarizedExperiment::assay(sce, "counts") <- NULL
  SummarizedExperiment::assay(sce, "X") <- log1p(SummarizedExperiment::assay(sce, "X") / 3 + 0.37)
  seurat <- convertSCEToSeurat(sce)
  expect_identical(SeuratObject::Layers(seurat), "data")
  expect_true(all(seurat$nCount_RNA > 0))
  expect_true(all(seurat$nFeature_RNA > 0))
})
