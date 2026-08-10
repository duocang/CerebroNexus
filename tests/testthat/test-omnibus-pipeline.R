omnibus_example_path <- function(file) {
  installed <- system.file(
    "extdata",
    "examples",
    file,
    package = "CerebroNexus"
  )
  if (nzchar(installed)) {
    return(installed)
  }
  testthat::test_path("..", "..", "inst", "extdata", "examples", file)
}

test_that("bundled Omnibus artifacts share the declared expression universe", {
  skip_if_not_installed("Seurat")

  seurat_path <- omnibus_example_path("demo_omnibus_seurat.rds")
  crb_path <- omnibus_example_path("demo_omnibus.crb")
  expect_true(file.exists(seurat_path))
  expect_true(file.exists(crb_path))

  seurat <- readRDS(seurat_path)
  crb <- readRDS(crb_path)
  expect_s4_class(seurat, "Seurat")
  expect_s3_class(crb, "Cerebro")
  expect_equal(unname(dim(seurat)), c(80L, 120L))
  expect_setequal(crb$getCellNames(), colnames(seurat))
  expect_setequal(crb$getGeneNames(), rownames(seurat))
})

test_that("bundled Omnibus CRB covers every declared Viewer data surface", {
  crb <- readRDS(omnibus_example_path("demo_omnibus.crb"))

  expect_setequal(
    crb$getGroups(),
    c("seurat_clusters", "orig.ident", "cell_type", "phase")
  )
  expect_identical(crb$getCellCycle(), "phase")
  expect_true(length(crb$getGeneLists()) > 0L)
  expect_identical(crb$availableProjections(), "umap")
  expect_false(is.null(crb$getTree("cell_type")))
  expect_identical(crb$getGroupsWithMostExpressedGenes(), "cell_type")
  expect_identical(crb$getGroupsWithMeanExpression(), "cell_type")
  expect_true(length(crb$getMethodsForMarkerGenes()) > 0L)
  expect_true(length(crb$getMethodsForEnrichedPathways()) > 0L)
  expect_identical(crb$getMethodsForTrajectories(), "monocle2")
  expect_true(length(crb$getExtraMaterialCategories()) > 0L)
  expect_false(is.null(crb$getTrekker()))

  repertoire <- crb$getImmuneRepertoire()
  expect_setequal(names(repertoire), c("sample_A", "sample_B"))
  expect_setequal(
    unique(unlist(lapply(repertoire, function(x) x$receptor))),
    c("TCR", "BCR")
  )
  expect_setequal(unique(crb$getHLATyping()$sample), c("sample_A", "sample_B"))
})

test_that("bundled Omnibus FOV carries its synthetic image inside the CRB", {
  crb <- readRDS(omnibus_example_path("demo_omnibus.crb"))
  expect_identical(crb$availableSpatial(), "omnibus_fov")

  spatial <- crb$getSpatialData("omnibus_fov")
  expect_match(spatial$histology_image, "^data:image/png;base64,")
  expect_identical(
    names(spatial$histology_image_bounds),
    c("xmin", "xmax", "ymin", "ymax")
  )
  expect_true(all(
    spatial$coordinates$x >= spatial$histology_image_bounds[["xmin"]]
  ))
  expect_true(all(
    spatial$coordinates$x <= spatial$histology_image_bounds[["xmax"]]
  ))
  expect_true(all(
    spatial$coordinates$y >= spatial$histology_image_bounds[["ymin"]]
  ))
  expect_true(all(
    spatial$coordinates$y <= spatial$histology_image_bounds[["ymax"]]
  ))
})

test_that("the source app loads Omnibus first without an external image", {
  app_path <- testthat::test_path("..", "..", "inst", "app.R")
  app_source <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

  omnibus_entry <- regexpr(
    '"Omnibus"\\s*=\\s*"extdata/examples/demo_omnibus[.]crb"',
    app_source,
    perl = TRUE
  )[[1L]]
  pbmc_entry <- regexpr(
    '"PBMC - Full \\(T\\+B\\)"\\s*=',
    app_source,
    perl = TRUE
  )[[1L]]
  expect_gt(omnibus_entry, 0L)
  expect_lt(omnibus_entry, pbmc_entry)
  expect_match(app_source, '"crb_pick_smallest_file"\\s*=\\s*FALSE')

  spatial_match <- regexec(
    '(?s)"spatial_images"\\s*=\\s*c\\((.*?)\\n\\s*\\)',
    app_source,
    perl = TRUE
  )
  spatial_options <- regmatches(app_source, spatial_match)[[1L]][[2L]]
  expect_false(grepl('"Omnibus"', spatial_options, fixed = TRUE))
})

test_that("Omnibus converts and bundles into a standalone Shiny app", {
  skip_if_not_installed("Seurat")

  root <- withr::local_tempdir()
  source_rds <- file.path(root, "demo_omnibus_seurat.rds")
  expect_true(file.copy(
    omnibus_example_path("demo_omnibus_seurat.rds"),
    source_rds
  ))

  convert_dir <- file.path(root, "converted")
  dir.create(convert_dir)
  convertSeuratToCerebro(
    seurat_file = source_rds,
    result_dir = convert_dir,
    assay = "RNA",
    slot = "data",
    experiment_name = "Synthetic Omnibus",
    organism = "Human",
    groups = c("seurat_clusters", "orig.ident", "cell_type", "phase"),
    cell_cycle = "phase",
    add_most_expressed_genes = FALSE,
    verbose = FALSE
  )
  generated_path <- file.path(
    convert_dir,
    "cerebro_demo_omnibus_seurat.crb"
  )
  expect_true(file.exists(generated_path))

  generated <- readRDS(generated_path)
  bundled <- readRDS(omnibus_example_path("demo_omnibus.crb"))
  expect_equal(dim(generated$getExpressionMatrix()), c(80L, 120L))
  expect_setequal(generated$getCellNames(), bundled$getCellNames())
  expect_setequal(generated$getGeneNames(), bundled$getGeneNames())
  expect_setequal(generated$getGroups(), bundled$getGroups())
  expect_equal(
    generated$getSpatialData("omnibus_fov")$histology_image,
    bundled$getSpatialData("omnibus_fov")$histology_image
  )
  expect_equal(
    generated$getSpatialData("omnibus_fov")$histology_image_bounds,
    bundled$getSpatialData("omnibus_fov")$histology_image_bounds
  )
  expect_equal(
    generated$getMethodsForTrajectories(),
    bundled$getMethodsForTrajectories()
  )
  expect_equal(
    generated$getImmuneRepertoire(),
    bundled$getImmuneRepertoire()
  )
  expect_equal(generated$getHLATyping(), bundled$getHLATyping())
  expect_equal(generated$getTrekker(), bundled$getTrekker())
  expect_equal(
    generated$getExtraMaterialCategories(),
    bundled$getExtraMaterialCategories()
  )

  app_dir <- file.path(root, "app")
  createShinyApp(
    cerebro_data = c(Omnibus = generated_path),
    result_dir = app_dir,
    launch_browser = FALSE,
    verbose = FALSE
  )
  expect_true(file.exists(file.path(app_dir, "app.R")))
  expect_true(file.exists(file.path(app_dir, "cerebro_config.rds")))
  expect_true(dir.exists(file.path(app_dir, "viewer")))

  private_crbs <- list.files(
    file.path(app_dir, "private-data"),
    pattern = "[.]crb$",
    full.names = TRUE
  )
  expect_length(private_crbs, 1L)
  private_object <- readRDS(private_crbs[[1L]])
  expect_s3_class(private_object, "Cerebro")
  expect_match(
    private_object$getSpatialData("omnibus_fov")$histology_image,
    "^data:image/png;base64,"
  )

  utility_source <- readLines(
    file.path(app_dir, "viewer", "utility_functions.R"),
    warn = FALSE
  )
  expect_false(any(grepl("CerebroNexus::", utility_source, fixed = TRUE)))
})
