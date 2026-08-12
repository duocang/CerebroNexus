skip_if_not_installed("Seurat")

test_that("bundled PBMC Seurat input converts into a standalone Shiny app", {
  seurat_file <- system.file(
    "extdata/examples/pbmc_seurat.rds",
    package = "CerebroNexus"
  )
  expect_true(nzchar(seurat_file))

  root <- withr::local_tempdir()
  conversion_dir <- file.path(root, "conversion")
  app_dir <- file.path(root, "app")

  convertSeuratToCerebro(
    seurat_file = seurat_file,
    result_dir = conversion_dir,
    assay = "RNA",
    slot = "data",
    experiment_name = "PBMC example",
    organism = "Human",
    groups = c("sample", "seurat_clusters"),
    groups_naming = list("seurat_clusters" = "cluster"),
    verbose = FALSE
  )

  crb_files <- list.files(
    conversion_dir,
    pattern = "[.]crb$",
    full.names = TRUE
  )
  expect_length(crb_files, 1L)

  createShinyApp(
    cerebro_data = c("PBMC example" = crb_files),
    result_dir = app_dir,
    launch_browser = FALSE,
    verbose = FALSE
  )

  expect_true(file.exists(file.path(app_dir, "app.R")))
  expect_true(file.exists(file.path(app_dir, "cerebro_config.rds")))
  expect_true(dir.exists(file.path(app_dir, "private-data")))
})
