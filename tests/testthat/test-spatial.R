# test-spatial.R — Tests for the spatial data backend + Shiny tab
#
# Scope: the backend data layer (Session A) and the interactive Spatial Shiny
# tab wiring (Session B). Backend contract tests come first; the module-parse
# and UI/server wiring guards follow.

shiny_root <- viewer_test_path()
# demo_spatial.crb is the synthetic Xenium demo that carries spatial data;
# the other bundled demos (PBMC sets, trajectory) have no spatial field.
spatial_crb <- system.file(
  "extdata/examples/demo_spatial.crb",
  package = "CerebroNexus"
)

test_that("demo_spatial.crb exposes spatial data via class methods", {
  skip_if_not(file.exists(spatial_crb))
  crb <- readRDS(spatial_crb)
  spatial <- crb$availableSpatial()
  expect_true(is.character(spatial))
  expect_true(length(spatial) > 0)
})

test_that("demo_spatial.crb spatial data is accessible and complete", {
  skip_if_not(file.exists(spatial_crb))
  crb <- readRDS(spatial_crb)
  spatial <- crb$availableSpatial()
  skip_if(length(spatial) == 0)
  data <- crb$getSpatialData(spatial[1])
  expect_true(is.list(data))
  expect_true(all(c("coordinates", "expression") %in% names(data)))
  expect_true(is.data.frame(data$coordinates))
  expect_true(nrow(data$coordinates) > 0)
  # exportFromSeurat crops coordinates to a 2D projection for plotting.
  expect_true(ncol(data$coordinates) >= 2)
  expect_true(nrow(data$expression) > 0)
  expect_true(ncol(data$expression) > 0)
})

test_that("getSpatialData errors on unknown spatial entry", {
  skip_if_not(file.exists(spatial_crb))
  crb <- readRDS(spatial_crb)
  expect_error(crb$getSpatialData("__not_a_real_image__"))
})

test_that("spatial accessor methods are defined on the class", {
  cls <- Cerebro
  for (m in c("addSpatialData", "getSpatialData", "availableSpatial")) {
    expect_true(is.function(cls$public_methods[[m]]), info = m)
  }
})

test_that("addSpatialData validates its input structure", {
  # A malformed entry (missing coordinates/expression) must be rejected so the
  # class contract getSpatialData() relies on cannot be violated silently.
  cls_text <- paste(
    deparse(Cerebro$public_methods$addSpatialData),
    collapse = "\n"
  )
  expect_match(cls_text, "coordinates", fixed = TRUE)
  expect_match(cls_text, "expression", fixed = TRUE)
})

test_that("spatial utility wrappers are defined in the app scope", {
  # The Spatial tab (Session B) calls these free functions. They were missing
  # from dev and must be present before the module is mounted. Cross-line-
  # tolerant regex per project convention (air may reflow).
  util_src <- paste(
    readLines(file.path(shiny_root, "utility_functions.R")),
    collapse = "\n"
  )
  for (fn in c(
    "availableSpatial",
    "getSpatialData",
    "serverSideGeneSelector"
  )) {
    expect_match(
      util_src,
      paste0(fn, "[\\s]{0,3}<-[\\s]{0,3}function"),
      perl = TRUE,
      info = fn
    )
  }
})

test_that("exportFromSeurat carries the spatial extraction path", {
  # Guard that the spatial export block survived the port: exportFromSeurat must
  # reference the internal .getSpatialData() extractor and stash results via
  # addSpatialData(). Reading the deparsed function body is robust to air reflow.
  fn_text <- paste(deparse(exportFromSeurat), collapse = "\n")
  expect_match(fn_text, ".getSpatialData", fixed = TRUE)
  expect_match(fn_text, ".getSpatialBoundaries", fixed = TRUE)
  expect_match(fn_text, "addSpatialData", fixed = TRUE)
})

test_that("Xenium segmentation normalizes to the cell-boundary contract", {
  skip_if_not_installed("SeuratObject")
  object <- builder_content_spatial_example_object("xenium-fov")
  expect_null(CerebroNexus:::.getSpatialBoundaries(
    object,
    "xenium-fov",
    colnames(object)
  ))
  image <- object@images[[1L]]
  boundaries <- methods::slot(image, "boundaries")
  cells <- colnames(object)[1:2]
  boundaries$segmentation <- SeuratObject::CreateSegmentation(data.frame(
    x = c(0, 1, 1, 0, 2, 3, 3, 2),
    y = c(0, 0, 1, 1, 2, 2, 3, 3),
    cell = rep(cells, each = 4L)
  ))
  methods::slot(image, "boundaries") <- boundaries
  methods::slot(image, "molecules") <- list(
    RNA = SeuratObject::CreateMolecules(data.frame(
      x = c(0.25, 0.75, 2.25, 2.75),
      y = c(0.25, 0.75, 2.25, 2.75),
      gene = c("Gene1", "Gene2", "Gene1", "Gene2")
    ))
  )
  object@images[[1L]] <- image

  result <- CerebroNexus:::.getSpatialBoundaries(
    object,
    "xenium-fov",
    colnames(object)
  )

  expect_named(result, c("cell_barcode", "x", "y", "part"))
  expect_setequal(unique(result$cell_barcode), cells)
  expect_true(all(is.finite(result$x)) && all(is.finite(result$y)))
  expect_true(all(result$part == "1"))

  molecules <- CerebroNexus:::.getSpatialMolecules(
    object,
    "xenium-fov",
    max_molecules = 3L
  )
  expect_named(
    molecules,
    c("data", "genes", "total_count", "exported_count", "truncated")
  )
  expect_named(molecules$data, c("gene", "x", "y"))
  expect_identical(molecules$total_count, 4L)
  expect_identical(molecules$exported_count, 3L)
  expect_true(molecules$truncated)

  path <- tempfile(fileext = ".crb")
  on.exit(unlink(path), add = TRUE)
  exportFromSeurat(
    object = object,
    assay = "RNA",
    slot = "counts",
    file = path,
    experiment_name = "Xenium boundaries",
    organism = "hg",
    groups = c("sample", "seurat_clusters"),
    nUMI = "nCount_RNA",
    nGene = "nFeature_RNA",
    projections = "umap",
    spatial_coordinate_transforms = list(
      `xenium-fov` = list(rotation_degrees = 90, scale = 2)
    ),
    verbose = FALSE
  )
  stored <- readRDS(path)$getSpatialData("xenium-fov")
  expect_named(stored$boundaries, c("cell_barcode", "x", "y", "part"))
  expect_setequal(unique(stored$boundaries$cell_barcode), cells)
  pivot <- stored$coordinate_transform$pivot
  expect_equal(
    stored$boundaries$x,
    pivot[["x"]] - 2 * (result$y - pivot[["y"]]),
    tolerance = 1e-10
  )
  expect_equal(
    stored$boundaries$y,
    pivot[["y"]] + 2 * (result$x - pivot[["x"]]),
    tolerance = 1e-10
  )
  expect_named(
    stored$molecules,
    c("data", "genes", "total_count", "exported_count", "truncated")
  )
  expect_equal(
    stored$molecules$data$x,
    pivot[["x"]] - 2 * (c(0.25, 2.25, 0.75, 2.75) - pivot[["y"]]),
    tolerance = 1e-10
  )
})

test_that("cell boundary payloads are validated and capped by complete cells", {
  boundaries <- data.frame(
    cell_barcode = rep(c("cell-1", "cell-2"), each = 4L),
    x = c(0, 1, 1, 0, 2, 3, 3, 2),
    y = c(0, 0, 1, 1, 2, 2, 3, 3),
    part = "1",
    stringsAsFactors = FALSE
  )

  payload <- spatial_cell_boundaries(boundaries, c("cell-1", "cell-2"), 4L)
  expect_named(payload, c("cell_barcode", "part", "x", "y"))
  expect_identical(unique(payload$cell_barcode), "cell-1")
  expect_length(payload$x, 4L)
  expect_identical(spatial_cell_boundaries(list(), "cell-1"), list())
  expect_identical(
    spatial_cell_boundaries(boundaries, "cell-1", Inf),
    list()
  )
})

test_that("spatial hierarchy resolves sample and ROI values by barcode", {
  metadata <- data.frame(
    cell_barcode = c("c1", "c2", "c3"),
    sample = c("S1", "S1", "S2"),
    roi_id = c("R1", "R2", "R3"),
    stringsAsFactors = FALSE
  )

  sample <- spatial_metadata_facet(metadata, c("c3", "c1"), "sample")
  roi <- spatial_metadata_facet(
    metadata,
    c("c3", "c1"),
    c("roi", "roi_id")
  )

  expect_identical(sample$field, "sample")
  expect_identical(sample$by_cell, c(c3 = "S2", c1 = "S1"))
  expect_identical(roi$values, c("R3", "R1"))
})

test_that("spatial sampling is scoped to the selected FOV, sample, and ROI", {
  cells_file <- file.path(
    system.file("viewer", package = "CerebroNexus"),
    "spatial",
    "obj_projection_cells_to_show.R"
  )
  metadata <- data.frame(
    cell_barcode = paste0("c", 1:5),
    sample = c("S1", "S1", "S1", "S2", "S2"),
    roi_id = c("R1", "R1", "R2", "R1", "R1"),
    cluster = "A",
    stringsAsFactors = FALSE
  )
  rownames(metadata) <- metadata$cell_barcode
  atlas <- list(
    fov1 = list(
      coordinates = data.frame(
        x = 1:3,
        y = 4:6,
        row.names = paste0("c", 1:3)
      )
    ),
    fov2 = list(
      coordinates = data.frame(
        x = 7:8,
        y = 9:10,
        row.names = paste0("c", 4:5)
      )
    )
  )
  server <- function(input, output, session) {
    getGroups <- function() character()
    getMetaData <- function() metadata
    getSpatialData <- function(name) atlas[[name]]
    sys.source(cells_file, envir = environment())
  }

  shiny::testServer(server, {
    session$setInputs(
      spatial_projection_percentage_cells_to_show = 100,
      spatial_projection_to_display = "fov1",
      spatial_projection_sample = "S1",
      spatial_projection_roi = "R1"
    )
    expect_setequal(spatial_projection_cells_to_show(), 1:2)
  })
})

test_that("molecule overlays accept one gene and cap browser points", {
  molecules <- list(
    data = data.frame(
      gene = c("Gene1", "Gene2", "Gene1", "Gene1"),
      x = c(1, 2, 3, 4),
      y = c(5, 6, 7, 8),
      stringsAsFactors = FALSE
    ),
    genes = c("Gene1", "Gene2"),
    total_count = 4L,
    exported_count = 4L,
    truncated = FALSE
  )

  overlay <- spatial_molecule_overlay(molecules, "Gene1", 2L)
  expect_named(overlay, c("x", "y"))
  expect_length(overlay$x, 2L)
  expect_identical(spatial_molecule_overlay(molecules, "missing"), list())
  expect_identical(
    spatial_molecule_overlay(molecules, "Gene1", scoped = TRUE),
    list()
  )
})

test_that("exportFromSeurat exports a spatial reduction without an image", {
  skip_if_not_installed("SeuratObject")
  skip_if_not_installed("Seurat")

  set.seed(7)
  counts <- matrix(
    stats::rpois(12L * 8L, lambda = 3),
    nrow = 12L,
    dimnames = list(paste0("Gene", seq_len(12L)), paste0("Cell", seq_len(8L)))
  )
  object <- SeuratObject::CreateSeuratObject(
    counts = methods::as(counts, "CsparseMatrix")
  )
  object <- Seurat::NormalizeData(object, verbose = FALSE)
  object$cluster <- rep(c("A", "B"), each = 4L)
  make_reduction <- function(key) {
    values <- matrix(
      stats::rnorm(16L),
      nrow = 8L,
      dimnames = list(colnames(object), paste0(key, seq_len(2L)))
    )
    SeuratObject::CreateDimReducObject(
      embeddings = values,
      key = key,
      assay = "RNA"
    )
  }
  object[["umap"]] <- make_reduction("UMAP_")
  object[["spatial"]] <- make_reduction("SPATIAL_")
  path <- tempfile(fileext = ".crb")
  on.exit(unlink(path), add = TRUE)

  exportFromSeurat(
    object = object,
    assay = "RNA",
    slot = "data",
    file = path,
    experiment_name = "spatial reduction",
    organism = "hg",
    groups = "cluster",
    nUMI = "nCount_RNA",
    nGene = "nFeature_RNA",
    add_all_meta_data = TRUE,
    projections = "umap",
    verbose = FALSE
  )

  crb <- readRDS(path)
  expect_identical(crb$availableSpatial(), "spatial")
  spatial <- crb$getSpatialData("spatial")
  expect_equal(nrow(spatial$coordinates), ncol(object))
  expect_identical(rownames(spatial$coordinates), colnames(object))
})

##----------------------------------------------------------------------------##
## Session B: Shiny tab wiring guards.
##----------------------------------------------------------------------------##

test_that("all spatial module files parse without errors", {
  spatial_dir <- file.path(shiny_root, "spatial")
  skip_if_not(dir.exists(spatial_dir), message = "spatial module missing")
  mod_files <- list.files(spatial_dir, pattern = "\\.R$", full.names = TRUE)
  expect_true(length(mod_files) > 0)
  for (fpath in mod_files) {
    expect_no_error(parse(file = fpath))
  }
})

test_that("background-image selection only recreates image calibration controls", {
  # The scatter controls retain user-selected values while moving between
  # backgrounds. Keep them in their own renderUI so that the selected image
  # can safely seed the calibration controls without recreating the point-size,
  # point-opacity, or sampling sliders.
  ui_file <- file.path(
    shiny_root,
    "spatial",
    "UI_projection_additional_parameters.R"
  )
  src <- paste(readLines(ui_file), collapse = "\n")

  expect_match(
    src,
    'output\\[\\["spatial_projection_scatter_parameters_UI"\\]\\][[:space:]]*<-[[:space:]]*renderUI',
    perl = TRUE
  )
  expect_match(
    src,
    'output\\[\\["spatial_projection_background_parameters_UI"\\]\\][[:space:]]*<-[[:space:]]*renderUI',
    perl = TRUE
  )

  scatter_src <- sub(
    '^[\\s\\S]*?output\\[\\["spatial_projection_scatter_parameters_UI"\\]\\][[:space:]]*<-[[:space:]]*renderUI\\(\\{',
    "",
    src,
    perl = TRUE
  )
  scatter_src <- sub(
    'output\\[\\["spatial_projection_background_parameters_UI"\\]\\][[:space:]]*<-[[:space:]]*renderUI[\\s\\S]*$',
    "",
    scatter_src,
    perl = TRUE
  )
  expect_false(
    grepl(
      'input\\[\\["spatial_projection_background_image"\\]\\]',
      scatter_src,
      fixed = FALSE
    )
  )

  projection_ui <- paste(
    readLines(file.path(shiny_root, "spatial", "UI_projection.R")),
    collapse = "\n"
  )
  expect_match(
    projection_ui,
    'uiOutput\\("spatial_projection_scatter_parameters_UI"\\)'
  )
  expect_no_match(
    projection_ui,
    'uiOutput\\("spatial_projection_background_select_UI"\\)'
  )
  main_parameters_ui <- paste(
    readLines(file.path(
      shiny_root,
      "spatial",
      "UI_projection_main_parameters.R"
    )),
    collapse = "\n"
  )
  expect_match(
    main_parameters_ui,
    'output\\[\\["spatial_projection_background_selector_UI"\\]\\][[:space:]]*<-[[:space:]]*renderUI',
    perl = TRUE
  )
  expect_match(
    main_parameters_ui,
    '"spatial_projection_roi_background_images"',
    fixed = TRUE
  )
  expect_match(main_parameters_ui, "background_control", fixed = TRUE)
  expect_match(
    projection_ui,
    'uiOutput\\("spatial_projection_background_selector_UI"\\)'
  )
  expect_match(
    projection_ui,
    paste0(
      'class = "cerebro-viz-primary",[[:space:]]*',
      'uiOutput\\("spatial_projection_main_parameters_UI"\\),[[:space:]]*',
      'uiOutput\\("spatial_projection_background_selector_UI"\\)'
    ),
    perl = TRUE
  )
  expect_match(
    projection_ui,
    'uiOutput\\("spatial_projection_background_parameters_UI"\\)'
  )
  expect_match(
    projection_ui,
    'class = "spatial-image-controls"',
    fixed = TRUE
  )
  additional_ui <- paste(
    readLines(
      file.path(shiny_root, "spatial", "UI_projection_additional_parameters.R")
    ),
    collapse = "\n"
  )
  for (class_name in c(
    "cerebro-settings-content",
    "cerebro-settings-full",
    "spatial-image-offset-row",
    "spatial-image-checks",
    "spatial-image-actions"
  )) {
    expect_match(additional_ui, class_name, fixed = TRUE)
  }
  expect_no_match(additional_ui, "cerebroSettingsSection(", fixed = TRUE)
  expect_match(additional_ui, '"Opacity"', fixed = TRUE)
  expect_no_match(additional_ui, '"Image opacity"', fixed = TRUE)
  expect_match(additional_ui, 'paste0(id, "_num")', fixed = TRUE)
  expect_match(additional_ui, 'label = "Show % of observations"', fixed = TRUE)
  expect_match(
    additional_ui,
    "the exported molecule records are not assigned to cells or ROIs.",
    fixed = TRUE
  )
  expect_match(
    additional_ui,
    'slider_number_input(\n        "spatial_projection_background_opacity"',
    fixed = TRUE
  )
  for (obsolete_class in c(
    "spatial-image-control-group",
    "spatial-image-transform-group",
    "spatial-image-scale-pair",
    "spatial-image-flips"
  )) {
    expect_no_match(additional_ui, obsolete_class, fixed = TRUE)
  }
  settings_css <- paste(
    readLines(file.path(shiny_root, "www", "custom.css")),
    collapse = "\n"
  )
  expect_match(settings_css, ".spatial-image-offset-row {", fixed = TRUE)
  expect_match(
    settings_css,
    ".spatial-image-offset-slider .irs-grid",
    fixed = TRUE
  )
  expect_match(settings_css, "grid-template-columns: 1fr;", fixed = TRUE)
  expect_match(settings_css, "margin-top: 12px;", fixed = TRUE)
  expect_match(
    settings_css,
    ".cerebro-settings-section {",
    fixed = TRUE
  )
  expect_no_match(
    settings_css,
    ".spatial-image-controls .shiny-panel-conditional {\n  display: grid;",
    fixed = TRUE
  )
  expect_match(projection_ui, '"Appearance"', fixed = TRUE)
  expect_match(projection_ui, '"Background image"', fixed = TRUE)
})

test_that("ImageFeaturePlot reaches getExpressionMatrix as a Cerebro method", {
  # getExpressionMatrix / getMeanExpressionForCells are Cerebro R6 methods,
  # not bare functions — they must be called through data_set()$. A bare
  # getExpressionMatrix(...) crashed the ImageFeaturePlot (gene-coloured) path
  # with "could not find function". Guard every expression-method call in the
  # spatial module against the bare form.
  spatial_dir <- file.path(shiny_root, "spatial")
  skip_if_not(dir.exists(spatial_dir), message = "spatial module missing")
  methods <- c("getExpressionMatrix", "getMeanExpressionForCells")
  for (fpath in list.files(spatial_dir, pattern = "\\.R$", full.names = TRUE)) {
    src <- paste(readLines(fpath), collapse = "\n")
    for (m in methods) {
      # a call to the method NOT immediately preceded by `$`
      bare <- gregexpr(
        paste0("(^|[^$[:alnum:]_.])", m, "\\("),
        src,
        perl = TRUE
      )[[1]]
      expect_true(
        bare[1] == -1,
        info = paste0(
          "bare ",
          m,
          "() in ",
          basename(fpath),
          " — use data_set()$"
        )
      )
    }
  }
})

test_that("group_filters widget the spatial tab depends on is present", {
  # spatial/UI_projection_group_filters.R calls registerGroupFiltersUI() and
  # registerGroupFiltersInfo(); those are only defined in the shared module,
  # which must be shipped and sourced or the tab errors on mount.
  widget <- file.path(
    shiny_root,
    "module",
    "group_filters",
    "group_filters_widget.R"
  )
  skip_if_not(file.exists(widget))
  widget_src <- paste(readLines(widget), collapse = "\n")
  for (fn in c("registerGroupFiltersUI", "registerGroupFiltersInfo")) {
    expect_match(
      widget_src,
      paste0(fn, "[\\s]{0,3}<-[\\s]{0,3}function"),
      perl = TRUE,
      info = fn
    )
  }
})

test_that("spatial UI defines correct tabName", {
  ui_file <- file.path(shiny_root, "spatial", "UI.R")
  skip_if_not(file.exists(ui_file))
  content <- paste(readLines(ui_file), collapse = "\n")
  expect_match(content, 'tabName\\s*=\\s*"spatial"', perl = TRUE)
})

test_that("Spatial tab is wired into the app UI and server", {
  # Guard the integration points so a future refactor that drops the wiring
  # (module present but never mounted) fails loudly. Cross-line-tolerant regex
  # per project convention (air may reflow).
  ui_src <- paste(
    readLines(file.path(shiny_root, "shiny_UI.R")),
    collapse = "\n"
  )
  expect_match(ui_src, "spatial/UI\\.R")
  expect_match(ui_src, "tab_spatial")
  expect_match(ui_src, 'conditionalSidebarItem\\("Spatial", "spatial"')

  server_src <- paste(
    readLines(file.path(shiny_root, "shiny_server.R")),
    collapse = "\n"
  )
  expect_match(server_src, "spatial/server\\.R")
  expect_match(server_src, "group_filters/group_filters_widget\\.R")
  expect_match(
    server_src,
    'toggleConditionalTab\\([\\s\\S]{0,80}"spatial"',
    perl = TRUE
  )
})

##----------------------------------------------------------------------------##
## Spatial background image: createShinyApp production channel + demo wiring.
##----------------------------------------------------------------------------##

test_that("createShinyApp preserves legacy and nested spatial settings APIs", {
  args <- names(formals(createShinyApp))
  expect_true("spatial_images" %in% args)
  expect_true("spatial_image_settings" %in% args)
  expect_true("spatial_plot_rotation" %in% args)
  expect_false("..." %in% args)
  expect_true(all(
    c(
      "spatial_images_flip_x",
      "spatial_images_flip_y",
      "spatial_images_scale_x",
      "spatial_images_scale_y",
      "spatial_images_offset_x",
      "spatial_images_offset_y"
    ) %in%
      args
  ))
})

test_that("createShinyApp migrates legacy spatial settings into its bundle", {
  skip_if_not(file.exists(spatial_crb))
  img <- system.file(
    "extdata/examples/demo_spatial_visium_he.png",
    package = "CerebroNexus"
  )
  skip_if_not(file.exists(img))
  out_dir <- file.path(
    tempdir(),
    paste0("cerebro_spatial_legacy_", Sys.getpid())
  )
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  suppressWarnings(suppressMessages(
    createShinyApp(
      cerebro_data = c("Legacy spatial" = spatial_crb),
      result_dir = out_dir,
      spatial_images = c("Legacy spatial" = img),
      spatial_images_flip_x = c("Legacy spatial" = TRUE),
      spatial_images_flip_y = c("Legacy spatial" = FALSE),
      spatial_images_scale_x = c("Legacy spatial" = 0.9),
      spatial_images_scale_y = c("Legacy spatial" = 1.1),
      spatial_images_offset_x = c("Legacy spatial" = 12),
      spatial_images_offset_y = c("Legacy spatial" = -8),
      spatial_plot_rotation = c("Legacy spatial" = 37),
      launch_browser = FALSE,
      verbose = FALSE
    )
  ))

  spatial_name <- readRDS(spatial_crb)$availableSpatial()[[1L]]
  cfg <- readRDS(file.path(out_dir, "cerebro_config.rds"))
  preset <- cfg[["spatial_image_settings"]][["Legacy spatial"]][[
    spatial_name
  ]][["Tissue background"]]
  expect_identical(preset[["flip_x"]], TRUE)
  expect_identical(preset[["flip_y"]], FALSE)
  expect_equal(preset[["scale_x"]], 0.9)
  expect_equal(preset[["scale_y"]], 1.1)
  expect_equal(preset[["offset_x"]], 12)
  expect_equal(preset[["offset_y"]], -8)
  expect_equal(
    cfg[["spatial_plot_rotation"]][["Legacy spatial"]][[spatial_name]],
    37
  )
})

test_that("Spatial UI seeds and resets every field from the shared image preset", {
  ui <- paste(
    readLines(
      file.path(shiny_root, "spatial", "UI_projection_additional_parameters.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  controls <- paste(
    readLines(
      file.path(shiny_root, "spatial", "obj_projection_background_controls.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    ui,
    "spatial_projection_background_opacity[\\s\\S]{0,200}preset\\$opacity",
    perl = TRUE
  )
  expect_match(
    controls,
    "spatial_projection_background_opacity[\\s\\S]{0,120}value = preset\\$opacity",
    perl = TRUE
  )
  for (numeric_id in c(
    "spatial_projection_background_opacity_num",
    "spatial_projection_background_scale_num",
    "spatial_projection_background_scale_x_num",
    "spatial_projection_background_scale_y_num",
    "spatial_projection_background_rotate_num"
  )) {
    expect_match(controls, numeric_id, fixed = TRUE)
  }
  expect_match(controls, '"spatial_copy_preset"', fixed = TRUE)
  page_js <- paste(
    readLines(file.path(shiny_root, "spatial", "js_page_helpers.js")),
    collapse = "\n"
  )
  expect_match(page_js, "navigator.clipboard.writeText", fixed = TRUE)
  expect_match(page_js, "document.execCommand('copy')", fixed = TRUE)
})

test_that("createShinyApp bundles a spatial image and writes the option", {
  # End-to-end exercise of the new side-copy + option-write path: a matched
  # spatial image must be copied into the bundle and its stored path rewritten
  # to the spatial-assets/<file> form inside cerebro_config.rds.
  skip_if_not(file.exists(spatial_crb))
  img <- tempfile(fileext = ".png")
  # 1x1 transparent PNG is enough; the copy path does not decode the image.
  writeBin(
    as.raw(c(
      0x89,
      0x50,
      0x4e,
      0x47,
      0x0d,
      0x0a,
      0x1a,
      0x0a,
      0x00,
      0x00,
      0x00,
      0x0d,
      0x49,
      0x48,
      0x44,
      0x52,
      0x00,
      0x00,
      0x00,
      0x01,
      0x00,
      0x00,
      0x00,
      0x01,
      0x08,
      0x06,
      0x00,
      0x00,
      0x00,
      0x1f,
      0x15,
      0xc4,
      0x89,
      0x00,
      0x00,
      0x00,
      0x0a,
      0x49,
      0x44,
      0x41,
      0x54,
      0x78,
      0x9c,
      0x63,
      0x00,
      0x01,
      0x00,
      0x00,
      0x05,
      0x00,
      0x01,
      0x0d,
      0x0a,
      0x2d,
      0xb4,
      0x00,
      0x00,
      0x00,
      0x00,
      0x49,
      0x45,
      0x4e,
      0x44,
      0xae,
      0x42,
      0x60,
      0x82
    )),
    img
  )
  out_dir <- file.path(tempdir(), paste0("cerebro_spatial_", Sys.getpid()))
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  suppressWarnings(suppressMessages(
    createShinyApp(
      cerebro_data = c("Xenium demo" = spatial_crb),
      result_dir = out_dir,
      spatial_images = c("Xenium demo" = img),
      launch_browser = FALSE,
      verbose = FALSE
    )
  ))

  cfg_path <- file.path(out_dir, "cerebro_config.rds")
  expect_true(file.exists(cfg_path))
  cfg <- readRDS(cfg_path)
  expect_true(!is.null(cfg[["spatial_images"]]))
  # path rewritten to the bundle-relative spatial asset directory
  spatial_name <- readRDS(spatial_crb)$availableSpatial()[[1L]]
  stored <- cfg[["spatial_images"]][["Xenium demo"]][[spatial_name]][[
    "Tissue background"
  ]]
  expect_match(stored, "^spatial-assets/", perl = TRUE)
  # and the image really landed in the bundle
  expect_true(file.exists(file.path(out_dir, stored)))
})

test_that("createShinyApp rejects unmatched spatial_images", {
  skip_if_not(file.exists(spatial_crb))
  img <- tempfile(fileext = ".png")
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47)), img)
  out_dir <- file.path(
    tempdir(),
    paste0("cerebro_spatial_unmatched_", Sys.getpid())
  )
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  expect_error(
    suppressMessages(
      createShinyApp(
        cerebro_data = c("Xenium demo" = spatial_crb),
        result_dir = out_dir,
        spatial_images = c("no_such_dataset" = img),
        launch_browser = FALSE,
        verbose = FALSE
      )
    ),
    "dataset `no_such_dataset` is not present"
  )
  expect_false(dir.exists(out_dir))
})

test_that("Visium ships its H&E as an EXTERNAL image, not embedded", {
  # Visium deliberately demonstrates the external-image path: the H&E lives in a
  # standalone PNG loaded via `spatial_images`, and the .crb carries NO embedded
  # image (unlike MERFISH/Xenium). This keeps the .crb small and exercises the
  # spatial_images code path as a live example.
  png <- system.file(
    "extdata/examples/demo_spatial_visium_he.png",
    package = "CerebroNexus"
  )
  skip_if(png == "" || !file.exists(png), message = "visium H&E png missing")
  expect_gt(file.info(png)$size, 0)

  crb_path <- system.file(
    "extdata/examples/demo_spatial_visium.crb",
    package = "CerebroNexus"
  )
  skip_if(
    crb_path == "" || !file.exists(crb_path),
    message = "visium crb missing"
  )
  crb <- readRDS(crb_path)
  spatial_name <- crb$availableSpatial()[1]
  sd <- .normalizeSpatialDataImages(
    crb$getSpatialData(spatial_name),
    spatial_name
  )
  expect_identical(sd$histology_images, list())

  # app.R must wire the external image via spatial_images for the Visium dataset
  app_src <- paste(
    readLines(system.file("app.R", package = "CerebroNexus")),
    collapse = "\n"
  )
  expect_match(app_src, "spatial_images", fixed = TRUE)
  expect_match(app_src, "demo_spatial_visium_he\\.png", perl = TRUE)

  renderer <- new.env(parent = globalenv())
  sys.source(
    file.path(
      system.file("viewer", package = "CerebroNexus"),
      "spatial",
      "func_projection_update_plot.R"
    ),
    envir = renderer
  )
  configured_image <- "extdata/examples/demo_spatial_visium_he.png"
  expect_identical(
    renderer$authorized_spatial_image_path(
      configured_image,
      configured_image,
      system.file(package = "CerebroNexus")
    ),
    normalizePath(png, winslash = "/", mustWork = TRUE)
  )
})

test_that("renderer uses selected descriptor bounds without changing cell axes", {
  skip_if_not_installed("base64enc")
  root <- withr::local_tempdir()
  dir.create(file.path(root, "spatial-assets"))
  image_path <- file.path(root, "spatial-assets", "atlas.png")
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47)), image_path)

  renderer <- new.env(parent = globalenv())
  renderer$Cerebro.options <- list(cerebro_root = root)
  sys.source(
    file.path(
      system.file("viewer", package = "CerebroNexus"),
      "utility_functions.R"
    ),
    envir = renderer
  )
  sys.source(
    file.path(
      system.file("viewer", package = "CerebroNexus"),
      "spatial",
      "func_projection_update_plot.R"
    ),
    envir = renderer
  )
  rendered <- NULL
  renderer$cerebroCellViewRender <- function(
    id,
    meta,
    data,
    hover = list(),
    extra = list()
  ) {
    rendered <<- list(meta = meta, data = data, extra = extra)
  }

  params <- list(
    color_variable = "score",
    split_by = "",
    background_image = spatial_background_key("external", "Atlas"),
    background_descriptor = list(
      source = "external",
      label = "Atlas",
      path = "spatial-assets/atlas.png",
      bounds = c(xmin = -10, xmax = 110, ymin = -20, ymax = 120)
    ),
    background_identity = list(
      dataset = "Atlas",
      spatial_name = "section",
      source = "external",
      label = "Atlas"
    ),
    background_image_allowlist = "spatial-assets/atlas.png",
    n_dimensions = 2,
    x_range = c(0, 100),
    y_range = c(10, 90),
    background_flip_x = FALSE,
    background_flip_y = FALSE,
    background_scale_x = 1,
    background_scale_y = 1,
    background_offset_x = 0,
    background_offset_y = 0,
    background_rotation = 37,
    background_opacity = 1,
    plot_type = "ImageFeaturePlot",
    point_size = 5,
    point_opacity = 1,
    draw_border = FALSE,
    show_cell_boundaries = TRUE,
    show_molecules = TRUE,
    molecule_gene = "Gene1",
    hover_info = FALSE
  )
  call_renderer <- function(plot_parameters, assignments = character()) {
    renderer$spatial_projection_update_plot(list(
      cells_df = data.frame(
        cell_barcode = c("cell-1", "cell-2"),
        score = c(1, 2),
        cluster = factor(c("C1", "C2")),
        sample_roi = c("lesion", "border")
      ),
      coordinates = data.frame(x = c(20, 80), y = c(30, 70)),
      reset_axes = FALSE,
      color_assignments = assignments,
      hover_info = c("first", "second"),
      cell_boundaries = list(
        cell_barcode = rep(c("cell-1", "cell-2"), each = 4L),
        part = rep("1", 8L),
        x = c(0, 1, 1, 0, 2, 3, 3, 2),
        y = c(0, 0, 1, 1, 2, 2, 3, 3)
      ),
      molecule_points = list(x = c(1, 2), y = c(3, 4)),
      plot_parameters = plot_parameters
    ))
  }

  call_renderer(params)
  expect_identical(
    unlist(rendered$meta$image_bounds[c("xmin", "xmax", "ymin", "ymax")]),
    c(xmin = -10, xmax = 110, ymin = -20, ymax = 120)
  )
  expect_identical(rendered$data$x_range, c(0, 100))
  expect_identical(rendered$data$y_range, c(10, 90))
  expect_identical(as.numeric(rendered$data$x), c(20, 80))
  expect_identical(as.numeric(rendered$data$y), c(30, 70))
  expect_identical(rendered$meta$background_rotation, 37)
  expect_length(rendered$extra$cell_boundaries$x, 8L)
  expect_length(rendered$extra$molecule_points$x, 2L)
  expect_identical(
    rendered$meta$background_identity,
    params$background_identity
  )

  params$background_descriptor$bounds <- NULL
  call_renderer(params)
  expect_identical(
    unlist(rendered$meta$image_bounds[c("xmin", "xmax", "ymin", "ymax")]),
    c(xmin = 20, xmax = 80, ymin = 30, ymax = 70)
  )
  expect_identical(rendered$data$x_range, c(0, 100))
  expect_identical(rendered$data$y_range, c(10, 90))

  params$background_image <- "none"
  params$background_descriptor <- NULL
  call_renderer(params)
  expect_null(rendered$meta$background_image)
  expect_identical(rendered$data$x_range, c(0, 100))
  expect_identical(rendered$data$y_range, c(10, 90))
  expect_identical(as.numeric(rendered$data$x), c(20, 80))
  expect_identical(as.numeric(rendered$data$y), c(30, 70))

  params$background_image <- spatial_background_key("external", "Atlas")
  params$background_descriptor <- list(
    source = "external",
    label = "Atlas",
    path = "spatial-assets/atlas.png",
    bounds = c(xmin = -10, xmax = 110, ymin = -20, ymax = 120)
  )
  params$color_variable <- "cluster"
  params$plot_type <- "ImageDimPlot"
  params$split_by <- "sample_roi"
  call_renderer(params, c(C1 = "#111111", C2 = "#eeeeee"))
  expect_identical(
    vapply(rendered$data$panels, `[[`, character(1), "label"),
    c("lesion", "border")
  )
  expect_identical(
    lapply(rendered$data$panels, `[[`, "selection_key"),
    list("cell-1", "cell-2")
  )
  expect_true(nzchar(rendered$meta$background_image))
  expect_true(all(vapply(
    rendered$data$panels,
    function(panel) is.null(panel$background_image),
    logical(1)
  )))
  expect_true(all(vapply(
    rendered$data$panels,
    function(panel) {
      identical(panel$x_range, c(0, 100)) &&
        identical(panel$y_range, c(10, 90))
    },
    logical(1)
  )))
  expect_identical(
    as.character(rendered$data$selection_key),
    c("cell-1", "cell-2")
  )
  expect_identical(rendered$data$group, c("C1", "C2"))
  expect_identical(unlist(rendered$meta$traces), c("C1", "C2"))
  expect_identical(rendered$meta$group_colors, c("#111111", "#eeeeee"))

  panel_preset <- list(
    flipX = FALSE,
    flipY = FALSE,
    scaleX = 1,
    scaleY = 1,
    offsetX = 0,
    offsetY = 0,
    rotation = 0,
    opacity = 0.8
  )
  panel_config <- function(roi, label, bounds) {
    list(
      descriptor = list(
        source = "external",
        label = label,
        path = "spatial-assets/atlas.png",
        bounds = bounds
      ),
      identity = list(
        dataset = "Atlas",
        spatial_name = "section",
        source = "external",
        label = label,
        roi = roi
      ),
      preset = panel_preset,
      image_allowlist = "spatial-assets/atlas.png"
    )
  }
  params$background_image <- "none"
  params$background_descriptor <- NULL
  params$background_identity <- NULL
  params$background_image_allowlist <- character()
  params$roi_mode <- "separate"
  params$roi_backgrounds <- list(
    lesion = panel_config(
      "lesion",
      "Lesion H&E",
      c(xmin = 10, xmax = 30, ymin = 20, ymax = 40)
    ),
    border = panel_config(
      "border",
      "Border H&E",
      c(xmin = 70, xmax = 90, ymin = 60, ymax = 80)
    )
  )
  call_renderer(params, c(C1 = "#111111", C2 = "#eeeeee"))

  expect_null(rendered$meta$background_image)
  expect_true(all(vapply(
    rendered$data$panels,
    function(panel) nzchar(panel$background_image),
    logical(1)
  )))
  expect_identical(
    vapply(rendered$data$panels, `[[`, character(1), "image_label"),
    c("Lesion H&E", "Border H&E")
  )
  expect_identical(
    lapply(rendered$data$panels, function(panel) panel$image_identity$roi),
    list("lesion", "border")
  )
})

test_that("shared Canvas owns spatial background identity and appearance", {
  engine <- paste(
    readLines(viewer_test_path("www", "cell_views.js")),
    collapse = "\n"
  )
  controls <- paste(
    readLines(viewer_test_path(
      "spatial",
      "obj_projection_background_controls.R"
    )),
    collapse = "\n"
  )

  expect_match(engine, "JSON.stringify(meta.background_identity)", fixed = TRUE)
  expect_match(
    engine,
    "rotation: Number(meta.background_rotation) || 0",
    fixed = TRUE
  )
  expect_match(engine, "function updateSingleBackground", fixed = TRUE)
  expect_match(engine, "function drawCellBoundaries", fixed = TRUE)
  expect_match(engine, "space.cellBoundaries", fixed = TRUE)
  expect_match(engine, "function drawMolecules", fixed = TRUE)
  expect_match(engine, "space.molecules", fixed = TRUE)
  expect_match(engine, "stashImgState(space)", fixed = TRUE)
  expect_match(controls, '"cell_view_background"', fixed = TRUE)

  renderer_src <- paste(
    readLines(viewer_test_path("spatial", "func_projection_update_plot.R")),
    collapse = "\n"
  )
  expect_match(
    renderer_src,
    'plot_parameters[["background_identity"]]',
    fixed = TRUE
  )
  expect_match(
    renderer_src,
    "image_identity = rendered$background_identity",
    fixed = TRUE
  )
  expect_match(
    renderer_src,
    'payload[["meta"]] <- c(background_meta, payload[["meta"]])',
    fixed = TRUE
  )
})

test_that("multi-spatial main UI preserves sliceB and uses its image choices", {
  main_ui <- viewer_test_path(
    "spatial",
    "UI_projection_main_parameters.R"
  )
  atlas <- list(
    sliceA = list(
      coordinates = data.frame(
        x = 1:2,
        y = 3:4,
        row.names = c("a1", "a2")
      ),
      histology_images = list(
        `H&E` = list(histology_image = "data:image/png;base64,HE")
      )
    ),
    sliceB = list(
      coordinates = data.frame(
        x = 101:102,
        y = 203:204,
        row.names = c("b1", "b2")
      ),
      histology_images = list(
        IF = list(histology_image = "data:image/png;base64,IF")
      )
    ),
    sliceC = list(
      coordinates = data.frame(
        x = 301:302,
        y = 403:404,
        row.names = c("c1", "c2")
      )
    )
  )
  server <- function(input, output, session) {
    data_set <- function() TRUE
    availableSpatial <- function() names(atlas)
    getSpatialData <- function(name) atlas[[name]]
    getMetaData <- function() {
      data.frame(
        cell_barcode = c("a1", "a2", "b1", "b2", "c1", "c2"),
        sample = c("S1", "S1", "S2", "S2", "S2", "S2"),
        roi_id = c("A", "B", "C", "D", "E", "F"),
        group = c("a", "b", "a", "b", "a", "b"),
        row.names = c("a1", "a2", "b1", "b2", "c1", "c2")
      )
    }
    getGroups <- function() "group"
    serverSideGeneSelector <- function(...) invisible(NULL)
    Cerebro.options <- list(
      spatial_images = list(
        Atlas = list(
          sliceA = c(DAPI = "spatial-assets/Atlas/sliceA/dapi.png"),
          sliceB = c(MIBI = "spatial-assets/Atlas/sliceB/mibi.png")
        )
      )
    )
    available_crb_files <- list(
      files = c(Atlas = "atlas.crb"),
      selected = "atlas.crb"
    )
    sys.source(main_ui, envir = environment())
  }

  shiny::testServer(server, {
    session$flushReact()
    default_html <- as.character(
      output$spatial_projection_main_parameters_UI$html
    )
    expect_match(default_html, 'value="S1" selected', fixed = TRUE)
    expect_match(default_html, 'value="sliceA" selected', fixed = TRUE)
    expect_false(grepl('value="sliceB"', default_html, fixed = TRUE))
    expect_false(grepl("All samples", default_html, fixed = TRUE))
    expect_match(
      default_html,
      '<option value="__none__" selected>None</option>',
      fixed = TRUE
    )

    session$setInputs(
      spatial_projection_sample = "S2",
      spatial_projection_to_display = "sliceB"
    )
    session$flushReact()
    expect_length(
      as.character(output$spatial_projection_background_selector_UI$html),
      0L
    )
    session$setInputs(spatial_projection_roi = "C")
    session$flushReact()
    main_html <- as.character(
      output$spatial_projection_main_parameters_UI$html
    )
    background_html <- as.character(
      output$spatial_projection_background_selector_UI$html
    )
    expect_match(main_html, 'value="sliceB" selected', fixed = TRUE)
    expect_match(background_html, "embedded::IF", fixed = TRUE)
    expect_match(background_html, "external::MIBI", fixed = TRUE)
    expect_false(grepl("embedded::H&amp;E", background_html, fixed = TRUE))
    expect_false(grepl("external::DAPI", background_html, fixed = TRUE))
    expect_match(main_html, 'value="S2" selected', fixed = TRUE)
    expect_match(main_html, 'value="C"', fixed = TRUE)
    expect_match(main_html, 'value="D"', fixed = TRUE)
    expect_match(main_html, 'value="sliceC"', fixed = TRUE)
    expect_false(grepl('value="sliceA"', main_html, fixed = TRUE))
    expect_false(grepl('value="A"', main_html, fixed = TRUE))
    expect_false(grepl('value="B"', main_html, fixed = TRUE))
    expect_false(grepl('value="E"', main_html, fixed = TRUE))
    expect_false(grepl('value="F"', main_html, fixed = TRUE))
    expect_identical(getSpatialData("sliceB")$coordinates$x, 101:102)
  })
})

test_that("bundled real demos embed a genuine tissue image in the .crb", {
  # MERFISH carries its real DAPI inside the CRB. Visium and Xenium use external
  # files; Slide-seq carries no image.
  for (f in "demo_spatial_merfish") {
    path <- system.file(
      file.path("extdata/examples", paste0(f, ".crb")),
      package = "CerebroNexus"
    )
    skip_if(path == "" || !file.exists(path), message = paste0(f, " missing"))
    crb <- readRDS(path)
    spatial_name <- crb$availableSpatial()[1]
    sd <- .normalizeSpatialDataImages(
      crb$getSpatialData(spatial_name),
      spatial_name
    )
    expect_named(sd$histology_images, "Tissue background", info = f)
    image <- sd$histology_images[["Tissue background"]]
    expect_match(image$histology_image, "^data:image/", info = f)
    b <- image$histology_image_bounds
    expect_true(
      all(c("xmin", "xmax", "ymin", "ymax") %in% names(b)),
      info = f
    )
    # cells must fall inside the image's coordinate-space extent
    coords <- sd$coordinates
    expect_true(
      min(coords$x) >= b[["xmin"]] &&
        max(coords$x) <= b[["xmax"]] &&
        min(coords$y) >= b[["ymin"]] &&
        max(coords$y) <= b[["ymax"]],
      info = f
    )
  }
})

##----------------------------------------------------------------------------##
## Real multi-platform demos: each shipped .crb (Visium / Slide-seq v2 / MERFISH
## / Xenium) must load with a usable spatial slot. These are built
## from genuine public data by data-raw/build_spatial_demos.R.
##----------------------------------------------------------------------------##

real_spatial_demos <- c(
  visium = "extdata/examples/demo_spatial_visium.crb",
  slideseq = "extdata/examples/demo_spatial_slideseq.crb",
  merfish = "extdata/examples/demo_spatial_merfish.crb",
  xenium = "extdata/examples/demo_spatial_xenium.crb"
)

test_that("each real spatial demo exposes coordinates with x/y", {
  for (nm in names(real_spatial_demos)) {
    path <- system.file(real_spatial_demos[[nm]], package = "CerebroNexus")
    skip_if(
      path == "" || !file.exists(path),
      message = paste0(nm, " demo missing")
    )
    crb <- readRDS(path)
    images <- crb$availableSpatial()
    expect_true(length(images) > 0, info = nm)
    sd <- crb$getSpatialData(images[1])
    expect_true(all(c("coordinates", "expression") %in% names(sd)), info = nm)
    coords <- sd$coordinates
    expect_true(all(c("x", "y") %in% colnames(coords)), info = nm)
    expect_true(nrow(coords) > 0, info = nm)
    # coordinates and expression must share cells so the tab can colour points
    expect_true(
      length(intersect(rownames(coords), colnames(sd$expression))) > 0,
      info = nm
    )
    # x/y must be finite numerics, not all-NA
    expect_true(any(is.finite(coords$x)) && any(is.finite(coords$y)), info = nm)
  }
})

test_that("real spatial demos are wired into the bundled dropdown", {
  # The three technology-labelled demos must appear in app.R's crb_file_to_load
  # so the switcher offers them. Cross-line-tolerant per project convention.
  app_src <- paste(
    readLines(system.file("app.R", package = "CerebroNexus")),
    collapse = "\n"
  )
  for (f in real_spatial_demos) {
    expect_match(
      app_src,
      gsub(".", "\\.", basename(f), fixed = TRUE),
      perl = TRUE
    )
  }
  # the labels must name the technology in brackets
  expect_match(app_src, "Visium")
  expect_match(app_src, "Slide-seq")
  expect_match(app_src, "MERFISH")
  expect_match(app_src, "Xenium")
})

test_that("image-free demo (Slide-seq) carries no histology image", {
  # This platform records positions, not a tissue photo, so a genuine
  # an empty `histology_images` manifest is the correct state.
  for (f in c("demo_spatial_slideseq")) {
    path <- system.file(
      file.path("extdata/examples", paste0(f, ".crb")),
      package = "CerebroNexus"
    )
    skip_if(path == "" || !file.exists(path), message = paste0(f, " missing"))
    crb <- readRDS(path)
    spatial_name <- crb$availableSpatial()[1]
    sd <- .normalizeSpatialDataImages(
      crb$getSpatialData(spatial_name),
      spatial_name
    )
    expect_identical(sd$histology_images, list(), info = f)
  }
})

test_that("embedded image demos store the image natively with no flip flag", {
  # Embedded images are stored in their native orientation; there is no per-.crb
  # render-flip flag (removed — display alignment is a user control in the tab).
  # Guard that the image is present and no stale flip flag lingers.
  for (f in "demo_spatial_merfish") {
    path <- system.file(
      file.path("extdata/examples", paste0(f, ".crb")),
      package = "CerebroNexus"
    )
    skip_if(path == "" || !file.exists(path), message = paste0(f, " missing"))
    crb <- readRDS(path)
    spatial_name <- crb$availableSpatial()[1]
    sd <- .normalizeSpatialDataImages(
      crb$getSpatialData(spatial_name),
      spatial_name
    )
    expect_named(sd$histology_images, "Tissue background", info = f)
    expect_null(sd$histology_image_flip_y, info = f)
  }
})

test_that("app.R ships the Visium H&E overlay pre-aligned", {
  # The bundled Visium demo opens with its H&E overlay already aligned to the
  # points, so users see a correct overlay without nudging it. app.R therefore
  # sets the per-dataset alignment presets (move + scale + a vertical flip that
  # this dataset needs); the Spatial tab still lets users adjust or Reset.
  app_src <- paste(
    readLines(system.file("app.R", package = "CerebroNexus")),
    collapse = "\n"
  )
  expect_match(app_src, "\"spatial_image_settings\"", fixed = TRUE)
  expect_match(app_src, "offset_x = 600", fixed = TRUE)
  expect_match(app_src, "scale_x = 1.55", fixed = TRUE)
  expect_match(app_src, "flip_y = TRUE", fixed = TRUE)
})

test_that("app.R configures MERFISH plot and image rotations separately", {
  app_src <- paste(
    readLines(system.file("app.R", package = "CerebroNexus")),
    collapse = "\n"
  )
  expect_match(
    app_src,
    '"fov" = 90',
    fixed = TRUE
  )
  expect_match(
    app_src,
    '"Tissue background" = list(rotation = 90)',
    fixed = TRUE
  )
})

##----------------------------------------------------------------------------##
## Regression: .getSpatialData must tolerate a coordinate source that carries an
## NA-named / blank-named column (Slide-seq GetTissueCoordinates returns such a
## frame). Before the as_df sanitiser this crashed with
## "undefined columns selected".
##----------------------------------------------------------------------------##

test_that(".getSpatialData tolerates a real Slide-seq object (NA-named coord col)", {
  # The ssHippo GetTissueCoordinates frame carries a column literally named NA,
  # which used to crash the extractor with "undefined columns selected". This is
  # the exact object behind demo_spatial_slideseq.crb. Skipped unless the source
  # data package is installed (it is not a hard test dependency).
  skip_if_not_installed("Seurat")
  skip_if_not_installed("ssHippo.SeuratData")

  suppressWarnings(suppressMessages(
    utils::data("ssHippo", package = "ssHippo.SeuratData")
  ))
  obj <- get("ssHippo")
  obj <- suppressWarnings(Seurat::UpdateSeuratObject(obj))
  set.seed(1)
  obj <- suppressWarnings(subset(obj, cells = sample(colnames(obj), 200)))

  # confirm the pathological column really is present in the raw source
  tc <- Seurat::GetTissueCoordinates(obj)
  expect_true(any(is.na(colnames(tc))))

  extractor <- getFromNamespace(".getSpatialData", "CerebroNexus")
  res <- extractor(obj, image = "image", layer = "counts", assay = "Spatial")
  expect_true(all(c("x", "y") %in% colnames(res$coordinates)))
  expect_true(nrow(res$coordinates) > 0)
  # the NA-named column must not have leaked through the sanitiser
  expect_false(any(is.na(colnames(res$coordinates))))
})
