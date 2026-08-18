##----------------------------------------------------------------------------##

builder_multisection_source_extras <- function(local = parent.frame()) {
  relative <- file.path(
    "viewer",
    "core",
    "spatial_coordinate_contract.R"
  )
  core <- testthat::test_path("..", "..", "inst", relative)
  if (!file.exists(core)) {
    core <- system.file(relative, package = "CerebroNexus")
  }
  sys.source(core, envir = local)
  builder_repo_source("spatial.R", local = local)
  builder_repo_source("extras.R", local = local)
}
## Objects holding several tissue sections.
##
## Every shipped demo `.crb` has exactly one spatial entry, and every other
## assertion in the suite indexes `availableSpatial()[1]`. That blind spot let
## two real defects live: the builder wrote one histology image into every
## section, and the viewer could not reach any section but the first.
##
## Fully synthetic -- no network, no data packages.
##----------------------------------------------------------------------------##

test_that("export keeps one spatial entry per tissue section", {
  skip_if_not_installed("Seurat")

  obj <- make_synthetic_multisection_seurat()
  expect_equal(
    names(obj@images),
    c("sectionA1", "sectionA2", "sectionB1")
  )

  dir <- withr::local_tempdir()
  crb_path <- convert_synthetic_to_crb(obj, dir, "multisection")
  crb <- readRDS(crb_path)

  expect_equal(
    crb$availableSpatial(),
    c("sectionA1", "sectionA2", "sectionB1")
  )

  ## Each entry must carry its OWN cells and its OWN patch of coordinate space.
  ## A section silently handed the whole object's coordinates is the documented
  ## failure mode of the metadata fallback in .getSpatialData, and it would show
  ## up here as overlapping ranges.
  ranges <- lapply(crb$availableSpatial(), function(nm) {
    co <- crb$getSpatialData(nm)$coordinates
    list(
      n = nrow(co),
      lo = min(co[, 1]),
      hi = max(co[, 1]),
      cells = rownames(co)
    )
  })
  names(ranges) <- crb$availableSpatial()

  expect_equal(
    vapply(ranges, function(r) r$n, numeric(1)),
    c(sectionA1 = 30, sectionA2 = 30, sectionB1 = 40)
  )
  ## Built 500 apart, so the sections cannot overlap unless something is wrong.
  expect_lt(ranges$sectionA1$hi, ranges$sectionA2$lo)
  expect_lt(ranges$sectionA2$hi, ranges$sectionB1$lo)
  ## No cell may appear in two sections.
  all_cells <- unlist(lapply(ranges, function(r) r$cells), use.names = FALSE)
  expect_equal(anyDuplicated(all_cells), 0L)
})

test_that("histology is attached per section, not once for all of them", {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("png")
  skip_if_not_installed("base64enc")

  local({
    builder_multisection_source_extras(environment())

    obj <- make_synthetic_multisection_seurat()
    dir <- withr::local_tempdir()
    crb_path <- convert_synthetic_to_crb(obj, dir, "multisection")

    ## One image and one extent per section, each derived from that section's
    ## own coordinates. Writing a single pair into every section -- the defect
    ## this guards -- would put one slide behind every other slide's cells.
    sections <- c("sectionA1", "sectionA2", "sectionB1")
    images <- list()
    for (i in seq_along(sections)) {
      arr <- array(stats::runif(6 * 6 * 3), dim = c(6, 6, 3))
      enc <- builder_encode_image(arr, max_px = 6)
      expect_null(enc$error)
      images[[sections[i]]] <- list(
        uri = enc$uri,
        bounds = list(
          xmin = (i - 1) * 500,
          xmax = (i - 1) * 500 + 100,
          ymin = 0,
          ymax = 80
        )
      )
    }

    applied <- builder_attach_histology(crb_path, images)
    expect_null(applied$error)
    expect_setequal(applied$applied, sections)

    crb <- readRDS(crb_path)
    bounds <- lapply(sections, function(nm) {
      records <- crb$getSpatialData(nm)$histology_images
      expect_length(records, 1L)
      records[[1L]]$histology_image_bounds
    })
    names(bounds) <- sections

    ## Distinct extents, and each one actually frames its own section's cells.
    expect_equal(
      vapply(bounds, function(b) b[["xmin"]], numeric(1)),
      c(sectionA1 = 0, sectionA2 = 500, sectionB1 = 1000)
    )
    for (nm in sections) {
      sd <- crb$getSpatialData(nm)
      record <- sd$histology_images[[1L]]
      expect_true(
        grepl("^data:image/png;base64,", record$histology_image),
        info = nm
      )
      expect_true(
        all(
          sd$coordinates[, 1] >= bounds[[nm]][["xmin"]] &
            sd$coordinates[, 1] <= bounds[[nm]][["xmax"]]
        ),
        info = paste(nm, "cells must sit inside their own image extent")
      )
    }

    ## The four documented bound keys, per section.
    for (nm in sections) {
      expect_setequal(
        names(
          crb$getSpatialData(nm)$histology_images[[1L]]$histology_image_bounds
        ),
        c("xmin", "xmax", "ymin", "ymax")
      )
    }
  })
})

test_that("attaching to some sections leaves the others without an image", {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("png")
  skip_if_not_installed("base64enc")

  local({
    builder_multisection_source_extras(environment())

    obj <- make_synthetic_multisection_seurat()
    dir <- withr::local_tempdir()
    crb_path <- convert_synthetic_to_crb(obj, dir, "multisection")

    arr <- array(stats::runif(6 * 6 * 3), dim = c(6, 6, 3))
    enc <- builder_encode_image(arr, max_px = 6)
    applied <- builder_attach_histology(
      crb_path,
      list(
        sectionA2 = list(
          uri = enc$uri,
          bounds = list(xmin = 500, xmax = 600, ymin = 0, ymax = 80)
        )
      )
    )
    expect_equal(applied$applied, "sectionA2")

    ## A partial attachment is legitimate -- the viewer simply offers no
    ## background for the bare sections -- but it must not bleed across.
    crb <- readRDS(crb_path)
    expect_length(crb$getSpatialData("sectionA2")$histology_images, 1L)
    expect_length(crb$getSpatialData("sectionA1")$histology_images, 0L)
    expect_length(crb$getSpatialData("sectionB1")$histology_images, 0L)
  })
})

test_that("attaching nothing, or to a section that is absent, is handled", {
  skip_if_not_installed("Seurat")

  local({
    builder_multisection_source_extras(environment())

    obj <- make_synthetic_multisection_seurat()
    dir <- withr::local_tempdir()
    crb_path <- convert_synthetic_to_crb(obj, dir, "multisection")

    ## No images at all: a no-op, not an error -- most data sets have none.
    expect_equal(
      builder_attach_histology(crb_path, list())$applied,
      character()
    )

    ## A name that is not in the object is a mistake worth reporting.
    bad <- builder_attach_histology(
      crb_path,
      list(
        nosuchsection = list(uri = "data:image/png;base64,AA", bounds = list())
      )
    )
    expect_false(is.null(bad$error))
  })
})

test_that("named image stacks remain isolated between spatial sections", {
  local({
    builder_multisection_source_extras(environment())
    record <- function(section, label, dx) {
      builder_alignment_record(
        source = list(name = paste0(label, ".png"), type = "image/png"),
        source_uri = "data:image/png;base64,AAAA",
        uri = "data:image/png;base64,AAAA",
        base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
        parameters = list(dx = dx),
        section = list(id = section, kind = "spatial")
      )
    }
    images <- list(
      sectionA1 = list(
        `H&E` = record("sectionA1", "H&E", 4),
        DAPI = record("sectionA1", "DAPI", 7)
      ),
      sectionA2 = list(
        `H&E` = record("sectionA2", "H&E", 0),
        DAPI = record("sectionA2", "DAPI", 1)
      ),
      sectionB1 = list(`H&E` = record("sectionB1", "H&E", 2))
    )
    copied <- builder_alignment_apply_transform_to_matching_label(
      images,
      "sectionA1",
      "DAPI"
    )
    expect_identical(copied$sectionA2$DAPI$dx, 7)
    expect_identical(copied$sectionB1[["H&E"]], images$sectionB1[["H&E"]])
    expect_false("DAPI" %in% names(copied$sectionB1))
  })
})
