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

public_api_omnibus_result <- local({
  result <- NULL
  function() {
    if (!is.null(result)) {
      return(result)
    }
    skip_if_not_installed("HDF5Array")
    script <- testthat::test_path(
      "..",
      "..",
      "data-raw",
      "verify_omnibus_public_api.R"
    )
    root <- withr::local_tempdir(.local_envir = parent.frame())
    status <- system2(
      file.path(R.home("bin"), "Rscript"),
      c(shQuote(script), shQuote(root)),
      stdout = TRUE,
      stderr = TRUE
    )
    expect_identical(
      attr(status, "status"),
      NULL,
      info = paste(status, collapse = "\n")
    )
    result <<- list(root = root, output = status)
    result
  }
})

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

test_that("Omnibus publication rolls back when publish rename throws", {
  publish <- load_omnibus_publisher()
  artifact_names <- paste0("artifact-", seq_len(5L), ".dat")

  for (failure_index in seq_along(artifact_names)) {
    root <- withr::local_tempdir()
    stage_dir <- file.path(root, paste0("stage-error-", failure_index))
    output_dir <- file.path(root, paste0("output-error-", failure_index))
    dir.create(stage_dir)
    dir.create(output_dir)
    staged <- file.path(stage_dir, artifact_names)
    destinations <- file.path(output_dir, artifact_names)
    for (index in seq_along(artifact_names)) {
      writeLines(paste("new", index), staged[[index]])
      writeLines(paste("old", index), destinations[[index]])
    }

    rename_count <- 0L
    throwing_rename <- function(from, to) {
      rename_count <<- rename_count + 1L
      if (rename_count == failure_index) {
        stop("injected publish error", call. = FALSE)
      }
      file.rename(from, to)
    }

    expect_error(
      publish(
        staged = staged,
        destinations = destinations,
        publish_rename = throwing_rename
      ),
      "injected publish error.*restored"
    )
    expect_equal(
      unname(vapply(destinations, readLines, "", warn = FALSE)),
      paste("old", seq_along(artifact_names))
    )
    expect_length(
      list.files(
        output_dir,
        all.files = TRUE,
        no.. = TRUE,
        pattern = "[.](publish|backup)-"
      ),
      0L
    )
  }
})

test_that("Omnibus publication preserves an unrestored backup", {
  publish <- load_omnibus_publisher()
  root <- withr::local_tempdir()
  stage_dir <- file.path(root, "stage-restore")
  output_dir <- file.path(root, "output-restore")
  dir.create(stage_dir)
  dir.create(output_dir)
  artifact_names <- paste0("artifact-", seq_len(5L), ".dat")
  staged <- file.path(stage_dir, artifact_names)
  destinations <- file.path(output_dir, artifact_names)
  for (index in seq_along(artifact_names)) {
    writeLines(paste("new", index), staged[[index]])
    writeLines(paste("old", index), destinations[[index]])
  }

  restore_count <- 0L
  failing_restore <- function(from, to) {
    restore_count <<- restore_count + 1L
    if (restore_count == 3L) {
      return(FALSE)
    }
    file.rename(from, to)
  }
  failure <- tryCatch(
    publish(
      staged = staged,
      destinations = destinations,
      publish_rename = function(from, to) FALSE,
      restore_rename = failing_restore
    ),
    error = identity
  )
  expect_s3_class(failure, "error")
  expect_match(conditionMessage(failure), "rollback was incomplete")

  backup_leftovers <- list.files(
    output_dir,
    all.files = TRUE,
    no.. = TRUE,
    pattern = "[.]backup-",
    full.names = TRUE
  )
  expect_length(backup_leftovers, 1L)
  expect_match(basename(backup_leftovers), "^[.]artifact-3[.]dat[.]backup-")
  expect_true(grepl(
    backup_leftovers,
    conditionMessage(failure),
    fixed = TRUE
  ))
  expect_identical(readLines(backup_leftovers, warn = FALSE), "old 3")
  expect_false(file.exists(destinations[[3L]]))
  expect_equal(
    unname(vapply(destinations[-3L], readLines, "", warn = FALSE)),
    paste("old", c(1L, 2L, 4L, 5L))
  )
  expect_length(
    list.files(
      output_dir,
      all.files = TRUE,
      no.. = TRUE,
      pattern = "[.]publish-"
    ),
    0L
  )
})

test_that("successful Omnibus publication leaves no temporary files", {
  publish <- load_omnibus_publisher()
  root <- withr::local_tempdir()
  stage_dir <- file.path(root, "stage-success")
  output_dir <- file.path(root, "output-success")
  dir.create(stage_dir)
  dir.create(output_dir)
  artifact_names <- paste0("artifact-", seq_len(5L), ".dat")
  staged <- file.path(stage_dir, artifact_names)
  destinations <- file.path(output_dir, artifact_names)
  for (index in seq_along(artifact_names)) {
    writeLines(paste("new", index), staged[[index]])
    writeLines(paste("old", index), destinations[[index]])
  }

  expect_identical(publish(staged, destinations), destinations)
  expect_equal(
    unname(vapply(destinations, readLines, "", warn = FALSE)),
    paste("new", seq_along(artifact_names))
  )
  expect_length(
    list.files(
      output_dir,
      all.files = TRUE,
      no.. = TRUE,
      pattern = "[.](publish|backup)-"
    ),
    0L
  )
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

test_that("the Omnibus public API verifier exposes the documented workflow", {
  verifier <- testthat::test_path(
    "..",
    "..",
    "data-raw",
    "verify_omnibus_public_api.R"
  )
  expect_true(file.exists(verifier))

  source <- paste(readLines(verifier, warn = FALSE), collapse = "\n")
  expect_match(source, "convertSeuratToCerebro\\(")
  expect_match(source, 'seurat_file\\s*=\\s*"inputs/demo_omnibus_seurat[.]rds"')
  expect_match(
    source,
    'groups\\s*=\\s*c\\("orig[.]ident",\\s*"condition",\\s*"cell_type"\\)'
  )
  expect_match(
    source,
    'groups_naming\\s*=\\s*list\\("orig[.]ident"\\s*=\\s*"sample",\\s*"cell_type"\\s*=\\s*"cluster"\\)'
  )
  expect_match(source, 'marker_method\\s*=\\s*"Synthetic markers"')
  expect_match(source, 'expression_matrix_mode\\s*=\\s*"h5"')
  expect_match(source, "createShinyApp\\(")
  expect_match(
    source,
    'cerebro_data\\s*=\\s*c\\(Omnibus\\s*=\\s*"output/cerebro_demo_omnibus_seurat[.]crb"\\)'
  )
  expect_match(
    source,
    'welcome_message\\s*=\\s*"<h2>Synthetic Omnibus Atlas</h2>"'
  )
  expect_false(grepl(":::", source, fixed = TRUE))
})

expect_omnibus_group_rename_error <- function(object, mapping, regexp) {
  before <- serialize(object, NULL, version = 3)
  expect_error(
    convertSeuratToCerebro(
      seurat_file = object,
      result_dir = withr::local_tempdir(.local_envir = parent.frame()),
      assay = "RNA",
      slot = "data",
      experiment_name = "Rename preflight",
      organism = "Human",
      groups = c("orig.ident", "condition", "cell_type"),
      groups_naming = mapping,
      add_most_expressed_genes = FALSE,
      expression_matrix_mode = "embedded",
      verbose = FALSE
    ),
    regexp
  )
  expect_identical(serialize(object, NULL, version = 3), before)
}

test_that("group renames reject metadata target collisions before mutation", {
  skip_if_not_installed("Seurat")
  object <- readRDS(omnibus_example_path("demo_omnibus_seurat.rds"))
  object$sample <- paste0("existing-", seq_len(ncol(object)))

  expect_omnibus_group_rename_error(
    object,
    list("orig.ident" = "sample", "cell_type" = "cluster"),
    "metadata conflict.*orig[.]ident.*sample"
  )
})

test_that("group renames reject keyed misc collisions before mutation", {
  skip_if_not_installed("Seurat")
  source <- readRDS(omnibus_example_path("demo_omnibus_seurat.rds"))
  old_tree <- source@misc$trees$cell_type
  new_tree <- old_tree
  new_tree$tip.label <- paste0("existing-", seq_along(new_tree$tip.label))
  conflicting_values <- list(
    trees = new_tree,
    most_expressed_genes = data.frame(
      cluster = "existing",
      gene = "ExistingGene",
      pct = 1
    ),
    mean_expression = data.frame(
      cluster = "existing",
      gene = "ExistingGene",
      mean_expr = 1
    )
  )

  for (collection in names(conflicting_values)) {
    object <- source
    object@misc[[collection]][["cluster"]] <- conflicting_values[[collection]]
    expect_false(identical(
      object@misc[[collection]][["cell_type"]],
      object@misc[[collection]][["cluster"]]
    ))
    expect_omnibus_group_rename_error(
      object,
      list("cell_type" = "cluster"),
      paste0(collection, ".*cell_type.*cluster.*both keys")
    )
  }
})

test_that("group rename preflight rejects ambiguous target names", {
  skip_if_not_installed("Seurat")
  object <- readRDS(omnibus_example_path("demo_omnibus_seurat.rds"))

  expect_omnibus_group_rename_error(
    object,
    list("orig.ident" = "renamed", "cell_type" = "renamed"),
    "Multiple.*renamed"
  )
  expect_omnibus_group_rename_error(
    object,
    list("cell_type" = ""),
    "non-empty scalar"
  )
  expect_omnibus_group_rename_error(
    object,
    list("cell_type" = c("cluster", "other")),
    "non-empty scalar"
  )
})

test_that("public APIs convert committed Omnibus inputs into a standalone H5 app", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("callr")
  skip_on_os("windows")

  run <- public_api_omnibus_result()
  root <- run$root
  input_dir <- file.path(root, "inputs")
  output_dir <- file.path(root, "output")
  app_dir <- file.path(root, "my_app")
  expected_inputs <- c(
    "demo_omnibus_seurat.rds",
    "demo_omnibus_markers.csv",
    "demo_omnibus_donorB_if.png",
    "demo_omnibus_donorC_review.png"
  )
  expect_true(all(file.exists(file.path(input_dir, expected_inputs))))

  crb_path <- file.path(output_dir, "cerebro_demo_omnibus_seurat.crb")
  expect_true(file.exists(crb_path))
  crb <- readRDS(crb_path)
  backend <- crb$getExpressionBackend()
  expect_identical(backend$type, "h5")
  expect_true(nzchar(backend$location))
  expect_false(grepl("^(/|[A-Za-z]:)", backend$location))
  h5_path <- file.path(output_dir, backend$location)
  expect_true(file.exists(h5_path))
  h5_matrix <- HDF5Array::TENxMatrix(h5_path, group = "expression")
  expect_equal(dim(h5_matrix), c(120L, 80L))

  expect_setequal(crb$getGroups(), c("sample", "condition", "cluster"))
  expect_false(any(c("orig.ident", "cell_type") %in% crb$getGroups()))
  expect_false(is.null(crb$getTree("cluster")))
  expect_true("cluster" %in% crb$getGroupsWithMostExpressedGenes())
  expect_false("cell_type" %in% crb$getGroupsWithMostExpressedGenes())
  expect_true("cluster" %in% crb$getGroupsWithMeanExpression())
  expect_false("cell_type" %in% crb$getGroupsWithMeanExpression())
  metadata <- crb$getMetaData()
  expect_setequal(unique(metadata$sample), c("donorA", "donorB", "donorC"))
  expect_setequal(unique(metadata$condition), c("Control", "Treatment"))
  expect_setequal(
    unique(metadata$cluster),
    c("T cell", "B cell", "Myeloid", "Stromal")
  )
  expect_identical(crb$getMethodsForMarkerGenes(), "Synthetic markers")
  marker_groups <- crb$getGroupsWithMarkerGenes("Synthetic markers")
  imported_markers <- do.call(
    rbind,
    lapply(marker_groups, function(group) {
      crb$getMarkerGenes("Synthetic markers", group)
    })
  )
  expected_markers <- utils::read.csv(
    file.path(input_dir, "demo_omnibus_markers.csv"),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(imported_markers), nrow(expected_markers))
  expect_setequal(imported_markers$gene, expected_markers$gene)

  expected_spatial_labels <- list(
    `donorA tissue` = c("H&E", "DAPI"),
    `donorB tissue` = c("H&E", "IF panel"),
    `donorC tissue` = character()
  )
  expect_identical(crb$availableSpatial(), names(expected_spatial_labels))
  for (spatial_name in names(expected_spatial_labels)) {
    spatial <- crb$getSpatialData(spatial_name)
    if (length(expected_spatial_labels[[spatial_name]]) == 0L) {
      expect_identical(spatial$histology_images, list())
    } else {
      expect_setequal(
        names(spatial$histology_images),
        expected_spatial_labels[[spatial_name]]
      )
    }
    for (label in names(spatial$histology_images)) {
      image <- spatial$histology_images[[label]]
      expect_match(image$histology_image, "^data:image/png;base64,")
      expect_identical(
        names(image$histology_image_bounds),
        c("xmin", "xmax", "ymin", "ymax")
      )
      bounds <- image$histology_image_bounds
      expect_true(all(
        spatial$coordinates$x >= bounds[["xmin"]] &
          spatial$coordinates$x <= bounds[["xmax"]] &
          spatial$coordinates$y >= bounds[["ymin"]] &
          spatial$coordinates$y <= bounds[["ymax"]]
      ))
    }
  }

  expect_true(file.exists(file.path(app_dir, "app.R")))
  expect_true(file.exists(file.path(app_dir, "cerebro_config.rds")))
  expect_true(dir.exists(file.path(app_dir, "viewer")))
  expect_true(dir.exists(file.path(app_dir, "extdata")))
  private_crb <- file.path(
    app_dir,
    "private-data",
    "cerebro_demo_omnibus_seurat.crb"
  )
  private_h5 <- file.path(app_dir, "private-data", backend$location)
  expect_true(file.exists(private_crb))
  expect_true(file.exists(private_h5))

  config <- readRDS(file.path(app_dir, "cerebro_config.rds"))
  expect_identical(
    config$welcome_message,
    "<h2>Synthetic Omnibus Atlas</h2>"
  )
  expect_identical(
    config$.bundle_run_options$shiny_app_options$port,
    8080L
  )
  expect_identical(
    config$.bundle_run_options$shiny_app_options$host,
    "127.0.0.1"
  )
  expect_identical(
    config$.bundle_run_options$max_request_size_bytes,
    as.double(8000 * 1024^2)
  )
  configured_crb <- unname(config$crb_file_to_load[["Omnibus"]])
  expect_identical(config$.bundle_backend_plan$schema_version, 1L)
  expect_identical(
    config$.bundle_backend_plan$entries[[configured_crb]],
    list(type = "h5", mode = "bundled", location = backend$location)
  )
  external <- config$spatial_images$Omnibus[["donorC tissue"]][[
    "Pathology review"
  ]]
  external_path <- if (is.list(external)) external$path else external
  expect_true(nzchar(external_path))
  expect_false(grepl("^(/|[A-Za-z]:)", external_path))
  expect_true(file.exists(file.path(app_dir, external_path)))

  private_object <- readRDS(private_crb)
  expect_identical(private_object$getExpressionBackend(), backend)
  expect_setequal(
    names(private_object$getSpatialData("donorA tissue")$histology_images),
    c("H&E", "DAPI")
  )
  expect_setequal(
    names(private_object$getSpatialData("donorB tissue")$histology_images),
    c("H&E", "IF panel")
  )
  expect_identical(
    private_object$getSpatialData("donorC tissue")$histology_images,
    list()
  )
  utility <- file.path(app_dir, "viewer", "utility_functions.R")
  utility_source <- readLines(utility, warn = FALSE)
  expect_false(any(grepl("CerebroNexus::", utility_source, fixed = TRUE)))

  hermetic_lib <- withr::local_tempdir()
  linked_any <- FALSE
  for (lib in .libPaths()) {
    for (package in list.dirs(lib, recursive = FALSE, full.names = FALSE)) {
      if (identical(package, "CerebroNexus")) {
        next
      }
      destination <- file.path(hermetic_lib, package)
      if (!file.exists(destination)) {
        linked <- tryCatch(
          file.symlink(file.path(lib, package), destination),
          error = function(error) FALSE
        )
        linked_any <- linked_any || isTRUE(linked)
      }
    }
  }
  skip_if_not(linked_any, "could not build a hermetic library via symlinks")
  runtime_result <- callr::r(
    function(app_dir) {
      if (requireNamespace("CerebroNexus", quietly = TRUE)) {
        stop("CerebroNexus is reachable; the library is not hermetic")
      }
      setwd(app_dir)
      config <- readRDS("cerebro_config.rds")
      assign("Cerebro.options", config, envir = .GlobalEnv)
      runtime <- new.env(parent = globalenv())
      sys.source("viewer/utility_functions.R", envir = runtime)
      crb <- unname(config$crb_file_to_load[[1L]])
      object <- runtime$get_or_load_crb(
        crb,
        config$.bundle_backend_plan,
        unname(config$crb_file_to_load)
      )
      list(
        backend = object$getExpressionBackend(),
        dimension = dim(object$expression),
        groups = object$getGroups(),
        spatial = object$availableSpatial()
      )
    },
    args = list(app_dir = app_dir),
    libpath = hermetic_lib
  )
  expect_identical(runtime_result$backend, backend)
  expect_equal(runtime_result$dimension, c(80L, 120L))
  expect_setequal(runtime_result$groups, c("sample", "condition", "cluster"))
  expect_identical(runtime_result$spatial, names(expected_spatial_labels))
})

test_that("embedded Omnibus fallback still exercises both public functions", {
  skip_if(requireNamespace("HDF5Array", quietly = TRUE))
  skip_if_not_installed("Seurat")

  root <- withr::local_tempdir()
  inputs <- file.path(root, "inputs")
  output <- file.path(root, "output")
  dir.create(inputs)
  dir.create(output)
  input_names <- c(
    "demo_omnibus_seurat.rds",
    "demo_omnibus_markers.csv",
    "demo_omnibus_donorB_if.png",
    "demo_omnibus_donorC_review.png"
  )
  expect_true(all(file.copy(
    file.path(
      testthat::test_path("../../inst/extdata/examples"),
      input_names
    ),
    file.path(inputs, input_names)
  )))

  convertSeuratToCerebro(
    seurat_file = file.path(inputs, "demo_omnibus_seurat.rds"),
    result_dir = output,
    assay = "RNA",
    slot = "data",
    experiment_name = "Synthetic Omnibus",
    organism = "Human",
    groups = c("orig.ident", "condition", "cell_type"),
    groups_naming = list("orig.ident" = "sample", "cell_type" = "cluster"),
    marker_file = file.path(inputs, "demo_omnibus_markers.csv"),
    marker_method = "Synthetic markers",
    spatial_images = list(
      "donorB tissue" = c(
        "IF panel" = file.path(inputs, "demo_omnibus_donorB_if.png")
      )
    ),
    expression_matrix_mode = "embedded",
    verbose = FALSE
  )
  crb <- file.path(output, "cerebro_demo_omnibus_seurat.crb")
  createShinyApp(
    cerebro_data = c(Omnibus = crb),
    spatial_images = list(
      Omnibus = list(
        "donorC tissue" = c(
          "Pathology review" = file.path(
            inputs,
            "demo_omnibus_donorC_review.png"
          )
        )
      )
    ),
    result_dir = file.path(root, "my_app"),
    launch_browser = FALSE,
    verbose = FALSE
  )
  expect_identical(readRDS(crb)$getExpressionBackend()$type, "embedded")
  expect_true(file.exists(file.path(root, "my_app", "app.R")))
})
