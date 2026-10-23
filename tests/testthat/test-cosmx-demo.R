cosmx_demo_env <- function() {
  helpers <- testthat::test_path(
    "..",
    "..",
    "data-raw",
    "large-demo",
    c("download_helpers.R", "prepare_viewer_cosmx_data.R")
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

## Flat files of one section as NanoString writes them: cell_ID 0 rows hold the
## transcripts outside any cell, and the negative probes sit among the genes.
cosmx_flat_files <- function(sample = "Lung1", dir = tempfile("cosmx-")) {
  dir.create(dir)
  withr::defer(unlink(dir, recursive = TRUE), envir = parent.frame())
  metadata <- data.frame(
    fov = c(1, 1, 2),
    cell_ID = c(1, 2, 1),
    Area = c(100, 200, 300),
    AspectRatio = c(1, 1.5, 2),
    CenterX_local_px = 1,
    CenterY_local_px = 1,
    CenterX_global_px = c(10, 20, 30),
    CenterY_global_px = c(100, 200, 300),
    Width = c(10, 20, 30),
    Height = c(11, 21, 31),
    Mean.MembraneStain = 1,
    Max.MembraneStain = 2,
    Mean.PanCK = 3,
    Max.PanCK = 4,
    Mean.CD45 = 5,
    Max.CD45 = 6,
    Mean.CD3 = 7,
    Max.CD3 = 8,
    Mean.DAPI = 9,
    Max.DAPI = 10
  )
  expression <- data.frame(
    fov = c(1, 1, 1, 2, 2),
    cell_ID = c(0, 1, 2, 0, 1),
    GENEA = c(9, 1, 0, 9, 4),
    GENEB = c(9, 0, 2, 9, 0),
    GENEC = c(9, 3, 0, 9, 0),
    NegPrb1 = c(9, 0, 1, 9, 1),
    NegPrb2 = c(9, 1, 1, 9, 0)
  )
  ## Rows in a different order than the metadata, as a check on the join.
  expression <- expression[c(5, 3, 2, 1, 4), ]
  files <- c(
    expression = file.path(dir, paste0(sample, "_exprMat_file.csv")),
    metadata = file.path(dir, paste0(sample, "_metadata_file.csv"))
  )
  utils::write.csv(expression, files[["expression"]], row.names = FALSE)
  utils::write.csv(metadata, files[["metadata"]], row.names = FALSE)
  files
}

test_that("the CosMx demo registers the NSCLC sections", {
  helper <- viewer_test_path("million_cell_demo.R")
  env <- new.env(parent = baseenv())
  sys.source(helper, envir = env)

  original <- list(
    crb_file_to_load = c(Small = "small.crb"),
    point_size = c(Small = 5),
    point_opacity = c(Small = 1),
    percentage_cells_to_show = 100
  )
  expect_identical(env$viewerAddCosMxDemo(original, ""), original)

  root <- withr::local_tempdir()
  crb <- file.path(root, "cosmx.crb")
  file.create(crb, file.path(root, "cosmx.h5"))
  configured <- env$viewerAddCosMxDemo(original, crb)
  label <- "CosMx SMI NSCLC (8 sections)"
  expect_identical(
    unname(configured$crb_file_to_load[label]),
    normalizePath(crb)
  )
  expect_equal(unname(configured$point_size[label]), 1)
  expect_equal(unname(configured$point_opacity[label]), 0.6)
  expect_equal(unname(configured$percentage_cells_to_show[label]), 100)

  ## The four large demos can be registered together.
  both <- env$viewerAddTcellDemo(configured, crb)
  expect_true(label %in% names(both$crb_file_to_load))

  unlink(file.path(root, "cosmx.h5"))
  expect_error(
    env$viewerAddCosMxDemo(original, crb),
    "CEREBRO_COSMX_DEMO_CRB must name a .crb with an adjacent .h5 file.",
    fixed = TRUE
  )
})

test_that("the CosMx sources are the eight section archives with published sums", {
  env <- cosmx_demo_env()
  samples <- env$.viewerCosMxSamples()
  expect_identical(nrow(samples), 8L)
  expect_false(anyDuplicated(samples$sample) > 0L)
  expect_match(samples$md5, "^[0-9a-f]{32}$")
  expect_equal(sum(samples$size), 11312173246)

  files <- env$.viewerCosMxFiles()
  expect_named(files, samples$sample)
  expect_identical(
    files$Lung9_Rep1$url,
    paste0(
      "https://nanostring-public-share.s3.us-west-2.amazonaws.com/",
      "SMI-Compressed/Lung9_Rep1/Lung9_Rep1%20SMI%20Flat%20data.tar.gz"
    )
  )
  expect_identical(files$Lung9_Rep1$size, 1078783664)
  expect_identical(files$Lung9_Rep1$md5, "0c9ff318a6f72625e40381cc0d10504c")

  paths <- env$.viewerCosMxPaths(withr::local_tempdir())
  expect_identical(
    unname(basename(paths$archives[["Lung12"]])),
    "Lung12_SMI_Flat_data.tar.gz"
  )
  expect_identical(basename(dirname(paths$crb)), "cerebro-v1")
  expect_identical(dirname(paths$crb), dirname(paths$sidecar))
  expect_identical(
    unname(env$.viewerCosMxMembers("Lung12")[["metadata"]]),
    "Lung12/Lung12-Flat_files_and_images/Lung12_metadata_file.csv"
  )
})

test_that("a section is read into counts, cells and mirrored coordinates", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("Matrix")
  env <- cosmx_demo_env()
  section <- env$.viewerCosMxReadSample(cosmx_flat_files("Lung1"), "Lung1")

  barcodes <- c("Lung1_1_1", "Lung1_1_2", "Lung1_2_1")
  counts <- section$counts
  expect_s4_class(counts, "dgCMatrix")
  expect_identical(rownames(counts), c("GENEA", "GENEB", "GENEC"))
  expect_identical(colnames(counts), barcodes)
  expect_equal(
    as.matrix(counts),
    matrix(
      c(1, 0, 3, 0, 2, 0, 4, 0, 0),
      3,
      3,
      dimnames = list(c("GENEA", "GENEB", "GENEC"), barcodes)
    )
  )

  cells <- section$cells
  expect_identical(rownames(cells), barcodes)
  expect_identical(cells$cell_barcode, barcodes)
  expect_identical(cells$sample, rep("Lung1", 3))
  expect_identical(cells$fov, c("Lung1_F01", "Lung1_F01", "Lung1_F02"))
  expect_identical(cells$nUMI, c(4L, 2L, 4L))
  expect_identical(cells$nGene, c(2L, 1L, 1L))
  ## The negative probes are not genes; their totals describe each cell.
  expect_identical(cells$neg_probe_counts, c(1L, 2L, 1L))
  expect_equal(cells$mean_dapi, rep(9, 3))
  expect_equal(cells$area, c(100, 200, 300))

  ## The slide's y axis grows downwards, the Viewer's upwards.
  coordinates <- section$coordinates
  expect_identical(rownames(coordinates), barcodes)
  expect_equal(coordinates$x, c(10, 20, 30))
  expect_equal(coordinates$y, c(-100, -200, -300))
})

test_that("the tissue name drops the replicate suffix", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("Matrix")
  env <- cosmx_demo_env()
  files <- cosmx_flat_files("Lung5_Rep2")
  section <- env$.viewerCosMxReadSample(files, "Lung5_Rep2")
  expect_identical(unique(section$cells$tissue), "Lung5")
  expect_identical(unique(section$cells$sample), "Lung5_Rep2")
  files <- cosmx_flat_files("Lung6")
  expect_identical(
    unique(env$.viewerCosMxReadSample(files, "Lung6")$cells$tissue),
    "Lung6"
  )
})

test_that("inconsistent flat files are rejected", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("Matrix")
  env <- cosmx_demo_env()

  files <- cosmx_flat_files("Lung1")
  metadata <- utils::read.csv(files[["metadata"]])
  metadata$cell_ID[[3]] <- 99
  utils::write.csv(metadata, files[["metadata"]], row.names = FALSE)
  expect_error(
    env$.viewerCosMxReadSample(files, "Lung1"),
    "different cells"
  )

  files <- cosmx_flat_files("Lung1")
  metadata <- utils::read.csv(files[["metadata"]])
  metadata$CenterX_global_px <- NULL
  utils::write.csv(metadata, files[["metadata"]], row.names = FALSE)
  expect_error(
    env$.viewerCosMxReadSample(files, "Lung1"),
    "CenterX_global_px"
  )
})

test_that("only the flat CSV files are unpacked from a section archive", {
  env <- cosmx_demo_env()
  skip_if(!nzchar(Sys.which("tar")), "no tar program")
  root <- withr::local_tempdir()
  source <- file.path(root, "source")
  folder <- file.path(source, "Lung1", "Lung1-Flat_files_and_images")
  dir.create(folder, recursive = TRUE)
  for (name in c("exprMat_file.csv", "metadata_file.csv", "tx_file.csv")) {
    writeLines(name, file.path(folder, paste0("Lung1_", name)))
  }
  writeLines("image", file.path(folder, "overlay.jpg"))
  archive <- file.path(root, "Lung1.tar.gz")
  withr::with_dir(source, utils::tar(archive, "Lung1", compression = "gzip", tar = "tar"))

  out <- file.path(root, "out")
  files <- env$.viewerCosMxExtract(archive, "Lung1", out)
  expect_named(files, c("expression", "metadata"))
  expect_true(all(file.exists(files)))
  extracted <- list.files(out, recursive = TRUE)
  expect_length(extracted, 2L)
  expect_false(any(grepl("tx_file|jpg", extracted)))

  broken <- file.path(root, "broken.tar.gz")
  withr::with_dir(
    source,
    utils::tar(broken, "Lung1/Lung1-Flat_files_and_images/overlay.jpg", compression = "gzip", tar = "tar")
  )
  expect_error(
    env$.viewerCosMxExtract(broken, "Lung1", file.path(root, "out2")),
    "."
  )
})

test_that("CosMx preparation keeps the previous data set on failure", {
  env <- cosmx_demo_env()
  root <- withr::local_tempdir()
  paths <- env$.viewerCosMxPaths(root)
  env$.demoRequire <- function(...) NULL
  env$.viewerCosMxFiles <- function(...) list()
  dir.create(dirname(paths$crb), recursive = TRUE)
  writeLines("previous", paths$crb)
  env$.viewerCosMxBuildObject <- function(paths, work) {
    writeLines("partial", paths$crb)
    stop("simulated preparation failure")
  }
  expect_error(
    env$prepareViewerCosMxDemoData(root),
    "simulated preparation failure"
  )
  expect_identical(readLines(paths$crb), "previous")
  expect_length(list.files(paths$root, pattern = "stage-"), 0L)

  env$.viewerCosMxBuildObject <- function(paths, work) {
    writeLines("new", paths$crb)
    file.create(paths$sidecar)
    writeLines(as.character(env$.viewerCosMxVersion), paths$stamp)
  }
  expect_identical(env$prepareViewerCosMxDemoData(root), paths$crb)
  expect_identical(readLines(paths$crb), "new")
  env$.viewerCosMxBuildObject <- function(...) stop("must reuse the cache")
  expect_identical(env$prepareViewerCosMxDemoData(root), paths$crb)
  writeLines("0", paths$stamp)
  expect_error(env$prepareViewerCosMxDemoData(root), "must reuse the cache")
})
