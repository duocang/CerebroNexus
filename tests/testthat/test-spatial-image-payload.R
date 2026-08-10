valid_spatial_image_payload <- function() {
  list(
    histology_image = "data:image/png;base64,AA==",
    histology_image_bounds = c(
      xmin = 0,
      xmax = 100,
      ymin = 0,
      ymax = 80
    )
  )
}

test_that("spatial image payload validation accepts a contained FOV image", {
  payload <- valid_spatial_image_payload()
  coordinates <- data.frame(x = c(10, 90), y = c(5, 75))

  expect_identical(
    .validateCerebroSpatialImage(payload, "omnibus_fov", coordinates),
    list(histology_images = list(`Tissue background` = payload))
  )
})

test_that("spatial image collections match uniquely named Seurat images", {
  payloads <- list(
    omnibus_fov = list(
      `H&E` = valid_spatial_image_payload(),
      DAPI = valid_spatial_image_payload()
    )
  )

  expect_identical(
    .validateCerebroSpatialImages(payloads, "omnibus_fov"),
    payloads
  )
  expect_null(.validateCerebroSpatialImages(NULL, "omnibus_fov"))
  expect_error(
    .validateCerebroSpatialImages(
      list(unknown_fov = valid_spatial_image_payload()),
      "omnibus_fov"
    ),
    "unknown_fov.*not present"
  )
  expect_error(
    .validateCerebroSpatialImages(
      structure(list(list()), names = "unknown_fov"),
      "omnibus_fov"
    ),
    "unknown_fov.*not present"
  )
  expect_error(
    .validateCerebroSpatialImages(
      structure(list(list()), names = ""),
      "omnibus_fov"
    ),
    "non-empty"
  )
})

test_that("Seurat spatial image payloads accept the legacy shorthand", {
  payload <- valid_spatial_image_payload()

  normalized <- .validateCerebroSpatialImages(
    list(omnibus_fov = payload),
    "omnibus_fov"
  )

  expect_named(normalized$omnibus_fov, "Tissue background")
  expect_identical(normalized$omnibus_fov[["Tissue background"]], payload)
})

test_that("spatial image payload validation rejects malformed images", {
  payload <- valid_spatial_image_payload()
  coordinates <- data.frame(x = c(10, 90), y = c(5, 75))

  payload$histology_image <- "not-a-data-uri"
  expect_error(
    .validateCerebroSpatialImage(payload, "omnibus_fov", coordinates),
    "omnibus_fov.*data:image"
  )

  payload <- valid_spatial_image_payload()
  payload$histology_image_bounds <- c(
    left = 0,
    right = 100,
    top = 0,
    bottom = 80
  )
  expect_error(
    .validateCerebroSpatialImage(payload, "omnibus_fov", coordinates),
    "xmin.*xmax.*ymin.*ymax"
  )

  payload <- valid_spatial_image_payload()
  payload$histology_image_bounds[["xmax"]] <- Inf
  expect_error(
    .validateCerebroSpatialImage(payload, "omnibus_fov", coordinates),
    "finite"
  )

  payload <- valid_spatial_image_payload()
  payload$histology_image_bounds[["xmin"]] <- 100
  expect_error(
    .validateCerebroSpatialImage(payload, "omnibus_fov", coordinates),
    "xmin.*less than.*xmax"
  )
})

test_that("spatial image payload validation rejects unusable coordinates", {
  payload <- valid_spatial_image_payload()

  expect_error(
    .validateCerebroSpatialImage(
      payload,
      "omnibus_fov",
      data.frame(row = 10, column = 20)
    ),
    "numeric.*x.*y"
  )
  expect_error(
    .validateCerebroSpatialImage(
      payload,
      "omnibus_fov",
      data.frame(x = 101, y = 10)
    ),
    "outside.*bounds"
  )
  expect_error(
    .validateCerebroSpatialImage(
      payload,
      "omnibus_fov",
      data.frame(x = NA_real_, y = 10)
    ),
    "finite"
  )
})

test_that("exportFromSeurat preserves a declared FOV image payload", {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("SeuratObject")

  object <- make_synthetic_spatial_seurat(n_cells = 12, n_genes = 10, seed = 7)
  payload <- valid_spatial_image_payload()
  payload$histology_image_bounds <- c(
    xmin = 0,
    xmax = 100,
    ymin = 0,
    ymax = 100
  )
  object@misc$cerebro_spatial_images <- list(
    fov = list(`Tissue stain` = payload)
  )
  output <- tempfile(fileext = ".crb")

  exportFromSeurat(
    object = object,
    assay = "Spatial",
    slot = "data",
    file = output,
    experiment_name = "Synthetic FOV image",
    organism = "mouse",
    groups = c("seurat_clusters", "cell_type_final"),
    nUMI = "nCount_Spatial",
    nGene = "nFeature_Spatial",
    verbose = FALSE
  )

  spatial <- readRDS(output)$getSpatialData("fov")
  expect_identical(
    spatial$histology_images[["Tissue stain"]],
    payload
  )
})
