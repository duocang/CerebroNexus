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

load_omnibus_publisher <- function() {
  builder_path <- testthat::test_path(
    "..",
    "..",
    "data-raw",
    "build_omnibus_demo.R"
  )
  expressions <- parse(builder_path)
  definition <- which(vapply(
    expressions,
    function(expression) {
      is.call(expression) &&
        identical(expression[[1L]], as.name("<-")) &&
        identical(expression[[2L]], as.name("publish_omnibus_artifacts"))
    },
    logical(1)
  ))
  if (length(definition) != 1L) {
    stop("Omnibus publisher helper is not defined exactly once.", call. = FALSE)
  }
  environment <- new.env(parent = baseenv())
  eval(expressions[[definition]], envir = environment)
  environment$publish_omnibus_artifacts
}

test_that("Omnibus publication restores every target after any rename failure", {
  publish <- load_omnibus_publisher()
  artifact_names <- paste0("artifact-", seq_len(5L), ".dat")

  for (failure_index in seq_along(artifact_names)) {
    root <- withr::local_tempdir()
    stage_dir <- file.path(root, paste0("stage-", failure_index))
    output_dir <- file.path(root, paste0("output-", failure_index))
    dir.create(stage_dir)
    dir.create(output_dir)
    staged <- file.path(stage_dir, artifact_names)
    destinations <- file.path(output_dir, artifact_names)
    for (index in seq_along(artifact_names)) {
      writeLines(paste("new", index), staged[[index]])
      writeLines(paste("old", index), destinations[[index]])
    }

    rename_count <- 0L
    injected_rename <- function(from, to) {
      rename_count <<- rename_count + 1L
      expect_identical(dirname(from), dirname(to))
      if (rename_count == failure_index) {
        return(FALSE)
      }
      file.rename(from, to)
    }

    expect_error(
      publish(
        staged = staged,
        destinations = destinations,
        publish_rename = injected_rename
      ),
      paste0("artifact-", failure_index, "[.]dat")
    )
    expect_equal(
      unname(vapply(destinations, readLines, "", warn = FALSE)),
      paste("old", seq_along(artifact_names))
    )
    leftovers <- list.files(
      output_dir,
      all.files = TRUE,
      no.. = TRUE,
      pattern = "[.](publish|backup)-"
    )
    expect_length(leftovers, 0L)
  }
})

test_that("bundled Omnibus artifacts share the declared expression universe", {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("png")

  seurat_path <- omnibus_example_path("demo_omnibus_seurat.rds")
  crb_path <- omnibus_example_path("demo_omnibus.crb")
  marker_path <- omnibus_example_path("demo_omnibus_markers.csv")
  donor_b_image <- omnibus_example_path("demo_omnibus_donorB_if.png")
  donor_c_image <- omnibus_example_path("demo_omnibus_donorC_review.png")
  expect_true(all(file.exists(c(
    seurat_path,
    crb_path,
    marker_path,
    donor_b_image,
    donor_c_image
  ))))

  seurat <- readRDS(seurat_path)
  crb <- readRDS(crb_path)
  expect_s4_class(seurat, "Seurat")
  expect_s3_class(crb, "Cerebro")
  expect_equal(unname(dim(seurat)), c(80L, 120L))
  expect_setequal(crb$getCellNames(), colnames(seurat))
  expect_setequal(crb$getGeneNames(), rownames(seurat))
  expect_setequal(unique(seurat$orig.ident), c("donorA", "donorB", "donorC"))
  expect_equal(as.integer(table(seurat$orig.ident)), rep(40L, 3L))
  expect_identical(
    unname(seurat$condition),
    unname(ifelse(seurat$orig.ident == "donorB", "Treatment", "Control"))
  )

  markers <- utils::read.csv(marker_path, stringsAsFactors = FALSE)
  required_marker_columns <- c(
    "cluster",
    "gene",
    "avg_log2FC",
    "pct.1",
    "pct.2",
    "p_val_adj",
    "on_cell_surface"
  )
  expect_equal(nrow(markers), 40L)
  expect_true(all(required_marker_columns %in% colnames(markers)))
  expect_equal(
    as.integer(table(factor(
      markers$cluster,
      levels = unique(seurat$cell_type)
    ))),
    rep(10L, 4L)
  )
  expect_setequal(markers$gene, sprintf("Gene%03d", seq_len(40L)))
  expect_equal(dim(png::readPNG(donor_b_image)), c(72L, 96L, 3L))
  expect_equal(dim(png::readPNG(donor_c_image)), c(90L, 110L, 3L))

  misc_images <- seurat@misc$cerebro_spatial_images
  expect_false("donorC tissue" %in% names(misc_images))
  canonical_payloads <- unlist(
    lapply(misc_images, function(images) {
      vapply(images, `[[`, "", "histology_image")
    }),
    use.names = FALSE
  )
  external_paths <- c(donor_b_image, donor_c_image)
  external_base64 <- vapply(
    external_paths,
    base64enc::base64encode,
    ""
  )
  expect_false(any(vapply(
    external_base64,
    function(encoded) {
      any(grepl(encoded, canonical_payloads, fixed = TRUE))
    },
    logical(1)
  )))
  decoded_payloads <- lapply(canonical_payloads, function(payload) {
    base64enc::base64decode(sub("^[^,]+,", "", payload))
  })
  external_bytes <- lapply(external_paths, function(path) {
    readBin(path, what = "raw", n = file.info(path)$size)
  })
  expect_false(any(vapply(
    external_bytes,
    function(bytes) {
      any(vapply(decoded_payloads, identical, logical(1), bytes))
    },
    logical(1)
  )))
})

test_that("bundled Omnibus CRB covers every declared Viewer data surface", {
  crb <- readRDS(omnibus_example_path("demo_omnibus.crb"))

  expect_setequal(
    crb$getGroups(),
    c("seurat_clusters", "orig.ident", "cell_type", "phase", "condition")
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
  expect_setequal(names(repertoire), c("donorA", "donorB"))
  expect_setequal(
    unique(unlist(lapply(repertoire, function(x) x$receptor))),
    c("TCR", "BCR")
  )
  expect_setequal(
    unique(crb$getHLATyping()$sample),
    c("donorA", "donorB", "donorC")
  )
})

test_that("bundled Omnibus spatial entries preserve cells, images, and bounds", {
  skip_if_not_installed("Seurat")

  seurat <- readRDS(omnibus_example_path("demo_omnibus_seurat.rds"))
  crb <- readRDS(omnibus_example_path("demo_omnibus.crb"))
  spatial_names <- c("donorA tissue", "donorB tissue", "donorC tissue")
  expected_labels <- list(
    `donorA tissue` = c("H&E", "DAPI"),
    `donorB tissue` = "H&E",
    `donorC tissue` = character(0)
  )
  expect_identical(Seurat::Images(seurat), spatial_names)
  expect_identical(crb$availableSpatial(), spatial_names)

  seurat_cells <- lapply(spatial_names, function(name) {
    Seurat::Cells(seurat[[name]])
  })
  expect_true(all(lengths(seurat_cells) == 40L))
  expect_length(unique(unlist(seurat_cells)), 120L)
  expect_setequal(unlist(seurat_cells), colnames(seurat))

  source_coordinates <- lapply(spatial_names, function(name) {
    SeuratObject::GetTissueCoordinates(seurat[[name]])[, c("x", "y")]
  })
  names(source_coordinates) <- spatial_names
  donor_a <- source_coordinates[["donorA tissue"]]
  donor_a_radius <- sqrt(
    (donor_a$x - mean(range(donor_a$x)))^2 +
      (donor_a$y - mean(range(donor_a$y)))^2
  )
  expect_lt(max(donor_a_radius) - min(donor_a_radius), 1e-8)

  donor_b <- source_coordinates[["donorB tissue"]]
  expect_equal(length(unique(donor_b$x)), 8L)
  expect_equal(length(unique(donor_b$y)), 5L)
  expect_equal(nrow(unique(donor_b)), 40L)

  donor_c <- source_coordinates[["donorC tissue"]]
  distance_to_edge <- pmin(
    abs(donor_c$y - 100),
    abs(4 * donor_c$x + 3 * donor_c$y - 3900),
    abs(12 * donor_c$x - 7 * donor_c$y - 500)
  )
  expect_true(all(distance_to_edge < 1e-8))
  expect_true(all(vapply(
    list(c(100, 100), c(900, 100), c(450, 700)),
    function(vertex) {
      any(
        abs(donor_c$x - vertex[[1L]]) < 1e-8 &
          abs(donor_c$y - vertex[[2L]]) < 1e-8
      )
    },
    logical(1)
  )))

  bounds <- lapply(spatial_names, function(name) {
    spatial <- crb$getSpatialData(name)
    expect_equal(nrow(spatial$coordinates), 40L)
    expect_setequal(
      rownames(spatial$coordinates),
      Seurat::Cells(seurat[[name]])
    )
    if (length(expected_labels[[name]]) == 0L) {
      expect_identical(spatial$histology_images, list())
    } else {
      expect_named(spatial$histology_images, expected_labels[[name]])
    }
    lapply(spatial$histology_images, function(image) {
      expect_match(image$histology_image, "^data:image/png;base64,")
      expect_identical(
        names(image$histology_image_bounds),
        c("xmin", "xmax", "ymin", "ymax")
      )
      expect_true(all(
        spatial$coordinates$x >= image$histology_image_bounds[["xmin"]] &
          spatial$coordinates$x <= image$histology_image_bounds[["xmax"]] &
          spatial$coordinates$y >= image$histology_image_bounds[["ymin"]] &
          spatial$coordinates$y <= image$histology_image_bounds[["ymax"]]
      ))
      image$histology_image_bounds
    })
  })
  names(bounds) <- spatial_names
  entry_bounds <- lapply(spatial_names, function(name) {
    spatial <- crb$getSpatialData(name)
    c(
      xmin = min(spatial$coordinates$x),
      xmax = max(spatial$coordinates$x),
      ymin = min(spatial$coordinates$y),
      ymax = max(spatial$coordinates$y)
    )
  })
  expect_length(unique(vapply(entry_bounds, paste, collapse = ":", "")), 3L)
  expect_length(bounds[["donorA tissue"]], 2L)
  expect_length(bounds[["donorB tissue"]], 1L)
  expect_length(bounds[["donorC tissue"]], 0L)
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

  spatial_start <- regexpr(
    '"spatial_images"\\s*=\\s*list\\(',
    app_source,
    perl = TRUE
  )[[1L]]
  settings_start <- regexpr(
    '"spatial_image_settings"\\s*=',
    app_source,
    perl = TRUE
  )[[1L]]
  expect_gt(spatial_start, 0L)
  expect_gt(settings_start, spatial_start)
  spatial_options <- substr(app_source, spatial_start, settings_start - 1L)
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
    groups = c(
      "seurat_clusters",
      "orig.ident",
      "cell_type",
      "phase",
      "condition"
    ),
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
  expect_identical(generated$availableSpatial(), bundled$availableSpatial())
  for (spatial_name in generated$availableSpatial()) {
    expect_equal(
      generated$getSpatialData(spatial_name)$histology_images,
      bundled$getSpatialData(spatial_name)$histology_images
    )
  }
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
  expect_named(
    private_object$getSpatialData("donorA tissue")$histology_images,
    c("H&E", "DAPI")
  )
  expect_named(
    private_object$getSpatialData("donorB tissue")$histology_images,
    "H&E"
  )
  expect_identical(
    private_object$getSpatialData("donorC tissue")$histology_images,
    list()
  )

  utility_source <- readLines(
    file.path(app_dir, "viewer", "utility_functions.R"),
    warn = FALSE
  )
  expect_false(any(grepl("CerebroNexus::", utility_source, fixed = TRUE)))
})
