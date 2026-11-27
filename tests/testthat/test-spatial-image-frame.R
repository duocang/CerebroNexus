## A Visium tissue picture is placed by its scale factor alone: spot
## coordinates are full-resolution pixels (column, row) and the picture is the
## full-resolution image scaled by `tissue_*_scalef`. The Viewer draws y
## upwards and a picture's first row at `ymax`, so the import negates y once
## (y = -pxl_row) and places the picture at [0, w/s] x [-h/s, 0]. Then the
## picture needs no flip, and the section is drawn the way up it was imaged.

skip_if_not_installed("rhdf5")
skip_if_not_installed("SingleCellExperiment")

fixture <- anndata_test_fixture
quiet <- function(x) suppressWarnings(suppressMessages(x))
read_h5ad <- function(path, ...) quiet(readH5adAsSCE(path, ...))

## The spot coordinates exactly as the file stores them: (pxl_col, pxl_row),
## named by the cells in the file's order.
raw_spots <- function(path) {
  spots <- t(rhdf5::h5read(path, "obsm/spatial"))
  rownames(spots) <- colnames(read_h5ad(path, spatial_images = FALSE))
  spots
}

## Where the Viewer draws a point on a picture, in the picture's own pixels
## (column, row from the top), for a picture shown without flip or offset
## (background-model.js: the first row at ymax, the first column at xmin).
viewer_pixel <- function(x, y, bounds, width, height) {
  cbind(
    col = (x - bounds[["xmin"]]) /
      (bounds[["xmax"]] - bounds[["xmin"]]) *
      width,
    row = (bounds[["ymax"]] - y) /
      (bounds[["ymax"]] - bounds[["ymin"]]) *
      height
  )
}

test_that("the frame helpers negate y and place a picture above it", {
  expect_identical(.spatialPixelFrame(), "pixels_y_negated")
  expect_equal(
    .spatialPixelImageBounds(width = 200, height = 100, scale = 0.5),
    c(xmin = 0, xmax = 400, ymin = -200, ymax = 0)
  )
  expect_error(.spatialPixelImageBounds(200, 100, 0), "scale factor")
})

test_that("readH5adAsSCE puts Visium spots and pictures in one upright frame", {
  skip_if_not_installed("png")
  path <- fixture("small_visium.h5ad")
  sce <- read_h5ad(path)
  spots <- raw_spots(path)[colnames(sce), ]
  spatial <- SingleCellExperiment::reducedDim(sce, "spatial")
  expect_equal(unname(spatial[, 1]), unname(spots[, 1]))
  expect_equal(unname(spatial[, 2]), -unname(spots[, 2]))
  expect_identical(sce@metadata$spatial_coordinate_frame, "pixels_y_negated")

  hires <- sce@metadata$spatial_images$section1$hires
  scale <- 0.051033426 / 4 * 2
  expect_equal(
    hires$histology_image_bounds,
    c(xmin = 0, xmax = 288 / scale, ymin = -300 / scale, ymax = 0),
    tolerance = 1e-6
  )
})

test_that("drawn without a flip, every spot lands on its own picture pixel", {
  skip_if_not_installed("png")
  path <- fixture("small_visium.h5ad")
  sce <- read_h5ad(path, spatial_images = c("hires", "lowres"))
  spots <- raw_spots(path)
  spatial <- SingleCellExperiment::reducedDim(sce, "spatial")
  scalef <- c(hires = 0.051033426 / 4 * 2, lowres = 0.051033426 / 4)
  sizes <- list(hires = c(288, 300), lowres = c(144, 150))
  for (library in c("section1", "section2")) {
    cells <- colnames(sce)[sce$library_id == library]
    images <- sce@metadata$spatial_images[[library]]
    for (label in names(images)) {
      drawn <- viewer_pixel(
        spatial[cells, 1],
        spatial[cells, 2],
        images[[label]]$histology_image_bounds,
        sizes[[label]][[1L]],
        sizes[[label]][[2L]]
      )
      ## Space Ranger's own mapping: pixel = full-resolution pixel x scalef
      expected <- spots[cells, , drop = FALSE] * scalef[[label]]
      expect_equal(
        unname(drawn),
        unname(expected),
        tolerance = 1e-6,
        info = paste(library, label)
      )
    }
  }
})

test_that("a file without Visium scale factors keeps its coordinates", {
  path <- fixture("small_spatial.h5ad")
  sce <- read_h5ad(path)
  expect_null(sce@metadata$spatial_coordinate_frame)
  spots <- raw_spots(path)[colnames(sce), ]
  expect_equal(
    unname(SingleCellExperiment::reducedDim(sce, "spatial")),
    unname(spots)
  )
})

test_that("the frame is recorded in the .crb and undone by exportToH5ad", {
  skip_if_not_installed("png")
  path <- fixture("small_visium.h5ad")
  crb <- quiet(convertH5adToCerebro(
    path,
    withr::local_tempdir(),
    groups = c("library_id", "layer"),
    viewer_binary = "never",
    verbose = FALSE
  ))
  obj <- readCerebro(crb)
  section <- obj$getSpatialData("spatial [section1]", FALSE)
  expect_identical(section$coordinate_frame, "pixels_y_negated")
  expect_true(all(section$coordinates$y <= 0))

  out <- file.path(withr::local_tempdir(), "back.h5ad")
  quiet(exportToH5ad(crb, out))
  ## on disk the spots are full-resolution pixels again, as scanpy expects,
  ## also where they are kept as a projection
  expect_equal(raw_spots(out), raw_spots(path)[rownames(raw_spots(out)), ])
  expect_true("X_spatial" %in% rhdf5::h5ls(out)$name)
  expect_equal(
    t(rhdf5::h5read(out, "obsm/X_spatial")),
    unname(raw_spots(out))
  )
  expect_equal(
    as.numeric(rhdf5::h5read(
      out,
      "uns/spatial/section1/scalefactors/tissue_hires_scalef"
    )),
    0.051033426 / 4 * 2,
    tolerance = 1e-6
  )
})

test_that("the Seurat route carries the frame into the .crb", {
  skip_if_not_installed("SeuratObject")
  sce <- read_h5ad(fixture("small_visium.h5ad"))
  seurat <- convertSCEToSeurat(sce)
  ## recorded per section, so a section added later (merged from another
  ## object, read as Seurat reads Visium) is not taken for one of these
  expect_identical(
    seurat@misc$spatial_coordinate_frame,
    c(section1 = "pixels_y_negated", section2 = "pixels_y_negated")
  )
  native_cells <- colnames(seurat)[seurat$library_id == "section1"]
  spots <- raw_spots(fixture("small_visium.h5ad"))[native_cells, ]
  seurat[["native"]] <- SeuratObject::CreateFOV(
    data.frame(x = spots[, 1], y = spots[, 2], cell = native_cells),
    type = "centroids",
    assay = SeuratObject::DefaultAssay(seurat),
    key = "native_"
  )
  file <- file.path(withr::local_tempdir(), "seurat.crb")
  quiet(exportFromSeurat(
    seurat,
    assay = SeuratObject::DefaultAssay(seurat),
    slot = "counts",
    file = file,
    experiment_name = "visium",
    organism = "hg",
    groups = "library_id",
    nUMI = "nCount_RNA",
    nGene = "nFeature_RNA",
    viewer_binary = "never"
  ))
  obj <- readCerebro(file)
  expect_setequal(names(obj$spatial), c("section1", "section2", "native"))
  for (name in c("section1", "section2")) {
    expect_identical(
      obj$getSpatialData(name, FALSE)$coordinate_frame,
      "pixels_y_negated",
      info = name
    )
  }
  expect_null(obj$getSpatialData("native", FALSE)$coordinate_frame)
})
