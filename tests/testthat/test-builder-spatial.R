builder_spatial_test_source <- function(file, local = parent.frame()) {
  path <- builder_spatial_test_inst_path("builder", file)
  if (file.exists(path)) {
    sys.source(path, envir = local)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

sys.source(
  builder_spatial_test_inst_path(
    "viewer",
    "core",
    "spatial_coordinate_contract.R"
  ),
  envir = environment()
)
sys.source(
  builder_spatial_test_inst_path(
    "viewer",
    "core",
    "spatial_coordinate_transform.R"
  ),
  envir = environment()
)
builder_spatial_test_source("spatial.R")
builder_spatial_test_source("preview.R")
builder_spatial_test_source("extras.R")
builder_spatial_test_source("worker.R")
builder_spatial_test_source("plan/defaults.R")
builder_spatial_test_source("spatial_alignment_server.R")

test_that("alignment capability is limited to Spatial and Trekker datasets", {
  skip_if_not_installed("SeuratObject")
  spatial <- builder_content_spatial_example_object(c("section-a", "section-b"))
  plain <- spatial
  methods::slot(plain, "images", check = FALSE) <- list()
  plain@misc$trekker <- NULL
  trekker <- plain
  trekker@misc$trekker <- .builder_content_spatial_demo_payload()

  expect_length(builder_spatial_alignment_sections(plain), 0L)

  spatial_sections <- builder_spatial_alignment_sections(spatial)
  expect_identical(
    vapply(spatial_sections, `[[`, character(1), "id"),
    c("section-a", "section-b")
  )
  expect_true(all(vapply(
    spatial_sections,
    function(section) identical(section$kind, "spatial"),
    logical(1)
  )))

  trekker_sections <- builder_spatial_alignment_sections(trekker)
  expect_length(trekker_sections, 1L)
  expect_identical(trekker_sections[[1L]]$id, "trekker")
  expect_identical(trekker_sections[[1L]]$kind, "trekker")
  expect_match(trekker_sections[[1L]]$unit, "physical", ignore.case = TRUE)
})

test_that("preview cache pruning drops only the removed dataset", {
  shared <- list(
    "dataset-a" = list(status = "ready"),
    "dataset-b" = list(status = "ready")
  )
  spatial <- list(
    "dataset-a::section-1" = list(status = "ready"),
    "dataset-a::section-2" = list(status = "pending"),
    "dataset-b::section-1" = list(status = "ready")
  )

  expect_identical(
    names(builder_preview_cache_drop_dataset(shared, "dataset-a")),
    "dataset-b"
  )
  expect_identical(
    names(builder_spatial_preview_cache_drop_dataset(spatial, "dataset-a")),
    "dataset-b::section-1"
  )
})

test_that("alignment projection prefers UMAP then the current default then PCA", {
  expect_identical(
    builder_alignment_projection(c("pca", "umap"), "pca"),
    "umap"
  )
  expect_identical(
    builder_alignment_projection(c("pca", "tsne"), "tsne"),
    "tsne"
  )
  expect_identical(
    builder_alignment_projection(c("pca", "tsne"), "missing"),
    "pca"
  )
  expect_null(builder_alignment_projection("tsne", "missing"))
})

test_that("external Builder images reject active or forged payloads", {
  skip_if_not_installed("base64enc")
  png_bytes <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
  png_uri <- paste0(
    "data:image/png;base64,",
    base64enc::base64encode(png_bytes)
  )
  expect_identical(builder_parse_image_uri(png_uri)$mime, "image/png")
  expect_error(
    builder_parse_image_uri("data:image/svg+xml;base64,PHN2Zz48L3N2Zz4="),
    "unsupported MIME"
  )
  expect_error(
    builder_parse_image_uri("data:image/png;base64,PHN2Zz48L3N2Zz4="),
    "does not match"
  )
  expect_error(
    builder_materialize_image_uri(png_uri, tempfile(fileext = ".svg")),
    "extension does not match"
  )
})

test_that("alignment previews are display-only Plotly figures", {
  frame <- data.frame(
    x = c(-1, 1),
    y = c(1, -1),
    group = c("A", "B"),
    cell_barcode = c("cell-a", "cell-b"),
    stringsAsFactors = FALSE
  )
  plot <- plotly::plotly_build(builder_alignment_plot(frame))

  expect_true(isTRUE(plot$x$config$staticPlot))
  expect_false(isTRUE(plot$x$config$displayModeBar))
  expect_true(isTRUE(plot$x$config$responsive))
  expect_false(identical(plot$x$layout$dragmode, "select"))
  expect_null(plot$x$data[[1L]]$customdata)
  expect_lte(sum(unlist(plot$x$layout$margin)), 36)
})

test_that("alignment server does not subscribe to Plotly selection events", {
  server <- paste(
    readLines(
      builder_spatial_test_inst_path("builder", "spatial_alignment_server.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_false(grepl("plotly::event_data(", server, fixed = TRUE))
  expect_false(grepl("builder_alignment_event_cells", server, fixed = TRUE))
  expect_false(grepl("selected_cells", server, fixed = TRUE))
  expect_false(grepl("session$onFlushed(", server, fixed = TRUE))
  expect_false(grepl(".clientValue-", server, fixed = TRUE))
})

test_that("alignment preview requeues when its render contract changes", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")

  entry <- list(
    id = "dataset-a",
    snapshot = list(
      path = "/private/dataset-a",
      owner_token = "owner-a",
      object_md5 = strrep("a", 32L)
    ),
    profile = list(images = "section-a", extras = list()),
    settings = list(
      name = "Dataset A",
      images = list(),
      default_group = "cluster",
      default_projection = "umap",
      palette = "cerebro"
    )
  )
  current_entry <- shiny::reactiveVal(entry)
  current <- shiny::reactiveVal(entry$id)
  alignment_preview <- shiny::reactiveVal(NULL)
  spatial_coords <- shiny::reactiveVal(NULL)
  requests <- list()

  shiny::testServer(
    function(input, output, session) {
      builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) current_entry(),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) {
          requests[[length(requests) + 1L]] <<- request
          TRUE
        },
        commit_images = function(entry, images) NULL,
        alignment_preview = alignment_preview,
        spatial_coords = spatial_coords
      )
    },
    {
      session$flushReact()
      expect_length(requests, 1L)

      renamed <- current_entry()
      renamed$settings$name <- "Dataset A renamed"
      current_entry(renamed)
      session$flushReact()
      expect_length(requests, 1L)

      regrouped <- current_entry()
      regrouped$settings$default_group <- "sample"
      current_entry(regrouped)
      session$flushReact()
      expect_length(requests, 2L)
      expect_identical(requests[[2L]]$group, "sample")

      replaced <- current_entry()
      replaced$snapshot$object_md5 <- strrep("b", 32L)
      current_entry(replaced)
      session$flushReact()
      expect_length(requests, 3L)
    }
  )
})

test_that("pending tissue image requires its matching preview and snapshot", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  skip_if_not_installed("png")
  skip_if_not_installed("base64enc")

  image_path <- tempfile(fileext = ".png")
  on.exit(unlink(image_path), add = TRUE)
  write_dummy_png(image_path)

  entry <- list(
    id = "dataset-a",
    snapshot = list(
      path = "/private/dataset-a",
      owner_token = "owner-a",
      object_md5 = strrep("a", 32L)
    ),
    profile = list(images = "section-a", extras = list()),
    settings = list(
      name = "Dataset A",
      images = list(),
      default_group = "cluster",
      default_projection = "umap",
      palette = "cerebro",
      spatial_point_appearance = list(
        "section-a" = list(point_opacity = 0.65, point_size = 6)
      )
    )
  )
  current_entry <- shiny::reactiveVal(entry)
  current <- shiny::reactiveVal(entry$id)
  alignment_preview <- shiny::reactiveVal(NULL)
  spatial_coords <- shiny::reactiveVal(NULL)
  committed <- list()
  commit_count <- 0L
  preview <- list(
    available = TRUE,
    bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    section = list(id = "section-a", kind = "spatial", unit = "pixels"),
    projection_name = "umap",
    capped = FALSE,
    transcriptome = data.frame(
      cell_id = c("cell-a", "cell-b"),
      x = c(-1, 1),
      y = c(-1, 1),
      group = c("A", "B"),
      stringsAsFactors = FALSE
    ),
    spatial = data.frame(
      cell_id = c("cell-a", "cell-b"),
      x = c(2, 8),
      y = c(3, 7),
      group = c("A", "B"),
      stringsAsFactors = FALSE
    )
  )

  shiny::testServer(
    function(input, output, session) {
      alignment <- builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) current_entry(),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) TRUE,
        commit_images = function(entry, images) {
          updated <- entry
          updated$settings$images <- images
          current_entry(updated)
          committed <<- images
          commit_count <<- commit_count + 1L
        },
        alignment_preview = alignment_preview,
        spatial_coords = spatial_coords
      )
    },
    {
      session$flushReact()
      expect_null(alignment$draft())

      session$setInputs(
        `enhance-tissue_image_file` = data.frame(
          name = "section-a.png",
          size = file.info(image_path)$size,
          type = "image/png",
          datapath = image_path,
          stringsAsFactors = FALSE
        )
      )
      session$flushReact()
      expect_null(alignment$draft())

      alignment_preview(preview)
      session$flushReact()

      expect_identical(alignment$draft()$source$name, "section-a.png")
      expect_false("saved" %in% names(alignment$draft()))
      expect_match(alignment$draft()$source_uri, "^data:image/png;base64,")
      expect_identical(alignment$draft()$point_opacity, 0.65)
      expect_identical(alignment$draft()$point_size, 6)
      expect_null(current_entry()$settings$spatial_point_appearance[[
        "section-a"
      ]])
      expect_named(committed, "section-a")
      expect_identical(commit_count, 1L)

      suppressWarnings(session$setInputs(`enhance-drop_image` = 1L))
      session$flushReact()
      session$setInputs(`enhance-remove_image_confirm` = 1L)
      session$flushReact()
      expect_null(alignment$draft())
      expect_identical(commit_count, 2L)
      expect_identical(
        current_entry()$settings$spatial_point_appearance[["section-a"]],
        list(point_opacity = 0.65, point_size = 6)
      )

      alignment_preview(NULL)
      session$setInputs(
        `enhance-tissue_image_file` = data.frame(
          name = "stale-section-a.png",
          size = file.info(image_path)$size,
          type = "image/png",
          datapath = image_path,
          stringsAsFactors = FALSE
        )
      )
      session$flushReact()

      replaced <- current_entry()
      replaced$snapshot$object_md5 <- strrep("b", 32L)
      current_entry(replaced)
      session$flushReact()
      alignment_preview(preview)
      session$flushReact()

      expect_null(alignment$draft())
      expect_identical(commit_count, 2L)
    }
  )
})

test_that("alignment controls auto-commit before dataset switches", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  skip_if_not_installed("png")
  skip_if_not_installed("base64enc")

  image_path <- tempfile(fileext = ".png")
  on.exit(unlink(image_path), add = TRUE)
  write_dummy_png(image_path)
  image <- png::readPNG(image_path)
  encoded <- builder_encode_image(image)
  record <- builder_alignment_record(
    source = list(name = "section-a.png", type = "image/png"),
    source_uri = encoded$uri,
    uri = encoded$uri,
    base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    image_geometry = encoded,
    section = list(id = "section-a", kind = "spatial")
  )
  entry <- list(
    id = "dataset-a",
    snapshot = list(
      path = "/private/dataset-a",
      owner_token = "owner-a",
      object_md5 = strrep("a", 32L)
    ),
    profile = list(images = "section-a", extras = list()),
    settings = list(
      name = "Dataset A",
      images = list(`section-a` = list(`H&E` = record)),
      default_group = "cluster",
      default_projection = "umap",
      palette = "cerebro"
    )
  )
  preview <- list(
    available = TRUE,
    bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    section = list(id = "section-a", kind = "spatial", unit = "pixels"),
    projection_name = "umap",
    capped = FALSE,
    transcriptome = data.frame(
      cell_id = c("cell-a", "cell-b"),
      x = c(-1, 1),
      y = c(-1, 1),
      group = c("A", "B")
    ),
    spatial = data.frame(
      cell_id = c("cell-a", "cell-b"),
      x = c(2, 8),
      y = c(3, 7),
      group = c("A", "B")
    )
  )
  current_entry <- shiny::reactiveVal(entry)
  current <- shiny::reactiveVal(entry$id)
  alignment_preview <- shiny::reactiveVal(preview)
  spatial_coords <- shiny::reactiveVal(NULL)
  switched <- character()

  shiny::testServer(
    function(input, output, session) {
      alignment <- builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) current_entry(),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) TRUE,
        commit_images = function(entry, images) {
          entry$settings$images <- images
          current_entry(entry)
          invisible(entry)
        },
        alignment_preview = alignment_preview,
        spatial_coords = spatial_coords
      )
    },
    {
      session$flushReact()
      alignment_preview(preview)
      session$flushReact()
      loaded_image <- alignment$raw_image()
      expect_false(is.null(loaded_image))

      switch_accepted <- alignment$request_dataset_switch(
        "dataset-b",
        function() {
          switched <<- c(switched, "immediate")
        }
      )
      expect_true(switch_accepted)
      expect_identical(switched, "immediate")

      session$setInputs(`enhance-img_dx` = 0)
      session$flushReact()

      session$setInputs(`enhance-img_dx` = 2)
      session$flushReact()
      expect_identical(alignment$draft()$dx, 2)
      expect_false("outside" %in% names(alignment$draft()))
      expect_identical(
        current_entry()$settings$images[["section-a"]][["H&E"]]$dx,
        2
      )
      alignment$request_dataset_switch("dataset-b", function() {
        switched <<- c(switched, "after-dx")
      })
      expect_identical(switched, c("immediate", "after-dx"))
      expect_identical(alignment$draft()$dx, 2)

      session$setInputs(`enhance-img_rotate` = 90)
      session$flushReact()
      expect_identical(alignment$draft()$rotation, 90)
      expect_identical(
        current_entry()$settings$images[["section-a"]][["H&E"]]$rotation,
        90
      )

      session$setInputs(`enhance-image_flip_x` = TRUE)
      session$flushReact()
      expect_true(alignment$draft()$flip_x)
      expect_true(
        current_entry()$settings$images[["section-a"]][["H&E"]]$flip_x
      )

      alignment$raw_image(loaded_image)
      session$flushReact()
      session$setInputs(`enhance-img_dx` = 1)
      session$flushReact()
      canonical <- alignment$current_record()
      expect_identical(alignment$draft()$uri, canonical$uri)
      expect_identical(alignment$draft()$bounds, canonical$bounds)
      expect_false("outside" %in% names(alignment$draft()))
      expect_false("total" %in% names(alignment$draft()))
      expect_false("outside" %in% names(canonical))
      expect_false("total" %in% names(canonical))
      alignment$request_dataset_switch("dataset-b", function() {
        switched <<- c(switched, "after-final-change")
      })
      expect_identical(
        switched,
        c("immediate", "after-dx", "after-final-change")
      )
      expect_identical(alignment$draft()$dx, 1)
      expect_identical(
        current_entry()$settings$images[["section-a"]][["H&E"]]$dx,
        1
      )
    }
  )
})

test_that("new images inherit the active image appearance", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  skip_if_not_installed("png")
  skip_if_not_installed("base64enc")

  image_path <- tempfile(fileext = ".png")
  on.exit(unlink(image_path), add = TRUE)
  write_dummy_png(image_path)
  encoded <- builder_encode_image(png::readPNG(image_path))
  parameters <- builder_alignment_defaults()
  parameters[c("point_opacity", "point_size")] <- list(0.65, 6)
  existing_record <- builder_alignment_record(
    source = list(name = "duplicate.png", type = "image/png"),
    source_uri = encoded$uri,
    uri = encoded$uri,
    base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    parameters = parameters,
    section = list(id = "section-a", kind = "spatial")
  )
  active_record <- existing_record
  active_record$source$name <- "DAPI.png"
  active_record[c("point_opacity", "point_size")] <- list(0.7, 7)

  entry <- list(
    id = "dataset-a",
    snapshot = list(
      path = "/private/dataset-a",
      owner_token = "owner-a",
      object_md5 = strrep("a", 32L)
    ),
    profile = list(images = "section-a", extras = list()),
    settings = list(
      name = "Dataset A",
      images = list(
        `section-a` = list(
          `duplicate.png` = existing_record,
          DAPI = active_record
        )
      ),
      default_group = "cluster",
      default_projection = "umap",
      palette = "cerebro"
    )
  )
  current_entry <- shiny::reactiveVal(entry)
  current <- shiny::reactiveVal(entry$id)
  alignment_preview <- shiny::reactiveVal(NULL)
  spatial_coords <- shiny::reactiveVal(NULL)
  commit_count <- 0L
  preview <- list(
    available = TRUE,
    bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    section = list(id = "section-a", kind = "spatial", unit = "pixels"),
    projection_name = "umap",
    capped = FALSE,
    transcriptome = data.frame(
      cell_id = c("cell-a", "cell-b"),
      x = c(-1, 1),
      y = c(-1, 1),
      group = c("A", "B"),
      stringsAsFactors = FALSE
    ),
    spatial = data.frame(
      cell_id = c("cell-a", "cell-b"),
      x = c(2, 8),
      y = c(3, 7),
      group = c("A", "B"),
      stringsAsFactors = FALSE
    )
  )
  upload <- data.frame(
    name = "duplicate.png",
    size = file.info(image_path)$size,
    type = "image/png",
    datapath = image_path,
    stringsAsFactors = FALSE
  )

  shiny::testServer(
    function(input, output, session) {
      alignment <- builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) current_entry(),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) TRUE,
        commit_images = function(entry, images) {
          updated <- current_entry()
          updated$settings$images <- images
          current_entry(updated)
          commit_count <<- commit_count + 1L
        },
        alignment_preview = alignment_preview,
        spatial_coords = spatial_coords
      )
    },
    {
      session$flushReact()
      alignment_preview(preview)
      session$flushReact()
      expect_identical(alignment$active_image(), "duplicate.png")
      expect_named(
        current_entry()$settings$images[["section-a"]],
        c("duplicate.png", "DAPI")
      )
      expect_identical(
        current_entry()$settings$images[["section-a"]][["duplicate.png"]][
          c("point_opacity", "point_size")
        ],
        list(point_opacity = 0.65, point_size = 6)
      )
      session$setInputs(`enhance-active_image` = "DAPI")
      session$flushReact()
      expect_identical(alignment$active_image(), "DAPI")

      suppressWarnings(session$setInputs(`enhance-tissue_image_file` = upload))
      session$flushReact()
      expect_true(alignment$pending_upload()$awaiting_label)

      session$setInputs(
        `enhance-new_image_label` = "PAS",
        `enhance-add_image_confirm` = 1L
      )
      session$flushReact()

      expect_named(
        current_entry()$settings$images[["section-a"]],
        c("duplicate.png", "DAPI", "PAS")
      )
      expect_identical(
        current_entry()$settings$images[["section-a"]][["PAS"]][
          c("point_opacity", "point_size")
        ],
        list(point_opacity = 0.7, point_size = 7)
      )
      expect_null(alignment$pending_upload())
      expect_identical(commit_count, 1L)

      session$setInputs(`enhance-active_image` = "DAPI")
      session$flushReact()
      session$setInputs(`enhance-remove_image_confirm` = 1L)
      session$flushReact()

      expect_identical(alignment$active_image(), "PAS")
      expect_named(
        current_entry()$settings$images[["section-a"]],
        c("duplicate.png", "PAS")
      )
      expect_identical(
        current_entry()$settings$images[["section-a"]][["duplicate.png"]][
          c("point_opacity", "point_size")
        ],
        list(point_opacity = 0.65, point_size = 6)
      )
      expect_identical(
        current_entry()$settings$images[["section-a"]][["PAS"]][
          c("point_opacity", "point_size")
        ],
        list(point_opacity = 0.7, point_size = 7)
      )
      expect_identical(commit_count, 2L)
    }
  )
})

test_that("alignment preview joins both spaces by cell identity", {
  skip_if_not_installed("SeuratObject")
  object <- builder_content_spatial_example_object("section-a")
  group <- colnames(object@meta.data)[[1L]]

  model <- builder_alignment_preview_model(
    object,
    default_projection = "pca",
    group = group,
    section_id = "section-a",
    layer = "counts",
    max_cells = 12L
  )

  expect_true(model$available)
  expect_identical(model$section$id, "section-a")
  expect_lte(nrow(model$transcriptome), 12L)
  expect_identical(
    model$transcriptome$cell_barcode,
    model$spatial$cell_barcode
  )
  expect_identical(model$transcriptome$group, model$spatial$group)
  expect_named(model$bounds, c("xmin", "xmax", "ymin", "ymax"))
  expect_true(all(is.finite(unlist(model$bounds))))
})

test_that("bounded alignment previews never retain full coverage coordinates", {
  skip_if_not_installed("SeuratObject")
  object <- builder_content_spatial_example_object("section-a")
  preview <- builder_alignment_preview_model(
    object,
    section_id = "section-a",
    layer = "counts",
    max_cells = 2L
  )

  expect_true(preview$available)
  expect_lte(nrow(preview$spatial), 2L)
  expect_false("coverage" %in% names(preview))
  expect_equal(preview$total_cells, ncol(object))

  scene <- builder_spatial_canvas_scene(
    preview,
    colors = character(),
    point_appearance = list(point_opacity = 0.65, point_size = 6),
    identity = "dataset::section-a",
    generation = 1L,
    dataset = "dataset-a",
    snapshot_identity = "snapshot-a",
    section = "section-a"
  )
  expect_false("coverage" %in% names(scene))
  expect_identical(scene$dataset, "dataset-a")
  expect_identical(scene$snapshotIdentity, "snapshot-a")
  expect_identical(scene$section, "section-a")
  expect_identical(scene$controls$point_opacity, 0.65)
  expect_identical(scene$controls$point_size, 6)
})

test_that("alignment preview resolves layer membership without expression data", {
  preview_source <- paste(
    deparse(body(builder_alignment_preview_model)),
    collapse = "\n"
  )

  expect_false(grepl(".getExpressionMatrix(", preview_source, fixed = TRUE))
  expect_match(
    preview_source,
    "builder_alignment_layer_cells(",
    fixed = TRUE
  )
})

test_that("Trekker alignment preview uses its physical and transcriptome spaces", {
  skip_if_not_installed("SeuratObject")
  object <- builder_content_spatial_example_object()
  methods::slot(object, "images", check = FALSE) <- list()
  payload <- .builder_content_spatial_demo_payload()
  object@misc$trekker <- payload

  model <- builder_alignment_preview_model(
    object,
    default_projection = "pca",
    section_id = "trekker",
    max_cells = 4L
  )

  expect_true(model$available)
  expect_identical(model$projection_name, "Trekker UMAP")
  expect_identical(model$section$kind, "trekker")
  expect_identical(
    model$transcriptome$cell_barcode,
    model$spatial$cell_barcode
  )
  expect_lte(nrow(model$spatial), 4L)
  expect_false(anyNA(model$spatial[, c("x", "y", "group")]))
})

test_that("alignment preview fails safely when no paired spaces exist", {
  skip_if_not_installed("SeuratObject")
  object <- builder_content_spatial_example_object()
  methods::slot(object, "images", check = FALSE) <- list()
  object@misc$trekker <- NULL

  model <- builder_alignment_preview_model(object, default_projection = "pca")
  expect_false(model$available)
  expect_match(model$message, "Spatial or Trekker", fixed = TRUE)
})

test_that("default image fit preserves aspect ratio and covers physical bounds", {
  fitted <- builder_alignment_fit_bounds(
    list(xmin = 0, xmax = 100, ymin = 0, ymax = 100),
    c(width = 200, height = 100)
  )
  expect_equal(fitted, list(xmin = -50, xmax = 150, ymin = 0, ymax = 100))
  expect_equal(
    (fitted$xmax - fitted$xmin) / (fitted$ymax - fitted$ymin),
    2
  )
  expect_lte(fitted$xmin, 0)
  expect_gte(fitted$xmax, 100)
  expect_lte(fitted$ymin, 0)
  expect_gte(fitted$ymax, 100)
})

test_that("default image fit covers decimal extrema despite floating error", {
  bounds <- list(
    xmin = 17.52,
    xmax = 4151.96,
    ymin = 3.92,
    ymax = 3173.76
  )
  fitted <- builder_alignment_fit_bounds(
    bounds,
    c(width = 320, height = 240)
  )
  cover <- builder_bounds_cover(
    fitted,
    list(
      c(bounds$xmin, bounds$xmax),
      c(bounds$ymin, bounds$ymax)
    )
  )

  expect_identical(cover$outside, 0L)
  expect_lte(fitted$xmin, bounds$xmin)
  expect_gte(fitted$xmax, bounds$xmax)
  expect_lte(fitted$ymin, bounds$ymin)
  expect_gte(fitted$ymax, bounds$ymax)
})

test_that("canonical alignment transform is deterministic and complete", {
  original <- list(xmin = 0, xmax = 100, ymin = 25, ymax = 75)
  parameters <- list(
    dx = 10,
    dy = -5,
    scale = 1.5,
    rotation = 30,
    flip_x = TRUE,
    flip_y = FALSE,
    image_opacity = 0.7,
    point_opacity = 0.9,
    point_size = 6
  )
  record <- builder_alignment_record(
    source = list(name = "section-a.png", type = "image/png"),
    source_uri = "data:image/png;base64,AAAA",
    uri = "data:image/png;base64,BBBB",
    base_bounds = original,
    parameters = parameters,
    section = list(id = "section-a", kind = "spatial")
  )

  expect_named(
    record,
    c(
      "source",
      "source_uri",
      "uri",
      "base_bounds",
      "bounds",
      "dx",
      "dy",
      "scale",
      "rotation",
      "flip_x",
      "flip_y",
      "image_opacity",
      "point_opacity",
      "point_size",
      "section_id",
      "section_kind"
    ),
    ignore.order = TRUE
  )
  expect_false("saved" %in% names(record))
  expect_equal(
    record$bounds,
    list(xmin = -15, xmax = 135, ymin = 7.5, ymax = 82.5)
  )
  expect_identical(
    builder_alignment_transform_bounds(original, parameters),
    record$bounds
  )
  expect_identical(
    builder_alignment_transform_bounds(original, parameters),
    builder_alignment_transform_bounds(original, parameters)
  )
})

test_that("rotated alignment bounds preserve one data-unit scale per image pixel", {
  base_bounds <- list(xmin = 0, xmax = 92, ymin = 0, ymax = 56)
  image_geometry <- list(
    source_width = 920,
    source_height = 560,
    extent_width = 1060,
    extent_height = 870
  )

  oriented <- builder_alignment_oriented_bounds(
    base_bounds,
    image_geometry
  )

  expect_equal(
    (oriented$xmax - oriented$xmin) / image_geometry$extent_width,
    (oriented$ymax - oriented$ymin) / image_geometry$extent_height,
    tolerance = 1e-12
  )
  expect_equal(
    c(
      x = (oriented$xmin + oriented$xmax) / 2,
      y = (oriented$ymin + oriented$ymax) / 2
    ),
    c(x = 46, y = 28),
    tolerance = 1e-12
  )

  record <- builder_alignment_record(
    source = list(name = "directional.png", type = "image/png"),
    source_uri = "data:image/png;base64,SOURCE",
    uri = "data:image/png;base64,ROTATED",
    base_bounds = base_bounds,
    parameters = list(dx = 7, dy = -3, scale = 1.2, rotation = -23),
    image_geometry = image_geometry,
    section = list(id = "FOV_A", kind = "spatial")
  )
  expected <- builder_adjust_bounds(oriented, dx = 7, dy = -3, scale = 1.2)
  expect_equal(record$bounds, expected, tolerance = 1e-12)
})

test_that("alignment plot preserves decimal coordinate rotation labels", {
  skip_if_not_installed("plotly")
  frame <- data.frame(
    cell_barcode = c("a", "b", "c"),
    x = c(0, 20, 80),
    y = c(0, 55, 10),
    group = c("A", "B", "C")
  )
  transform <- .spx_coordinate_transform_normalize(
    list(rotation_degrees = 37.5, scale = 1.2),
    frame
  )
  plot <- plotly::plotly_build(builder_alignment_plot(
    frame,
    coordinate_frame = .builder_alignment_bounds(frame),
    coordinate_transform = transform
  ))
  traces <- plot$x$data
  label_traces <- vapply(
    traces,
    function(trace) identical(trace$mode, "text"),
    logical(1)
  )
  label <- traces[[which(label_traces)]]$text

  expect_identical(unname(label), "+37.5°")
})

test_that("reset and apply-to-all preserve each section image identity", {
  defaults <- builder_alignment_defaults()
  first <- builder_alignment_record(
    source = list(name = "first.png", type = "image/png"),
    source_uri = "data:image/png;base64,FIRST",
    uri = "data:image/png;base64,FIRST",
    base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    parameters = modifyList(
      defaults,
      list(
        dx = 4,
        rotation = 20,
        flip_x = TRUE,
        image_opacity = 0.4,
        point_opacity = 0.5,
        point_size = 7
      )
    ),
    section = list(id = "first", kind = "spatial")
  )
  second <- builder_alignment_record(
    source = list(name = "second.png", type = "image/png"),
    source_uri = "data:image/png;base64,SECOND",
    uri = "data:image/png;base64,SECOND",
    base_bounds = list(xmin = 100, xmax = 130, ymin = 50, ymax = 70),
    parameters = defaults,
    section = list(id = "second", kind = "spatial")
  )

  copied <- builder_alignment_apply_transform_to_all(
    list(first = first, second = second),
    "first"
  )
  expect_identical(copied$second$source_uri, second$source_uri)
  expect_identical(copied$second$source$name, "second.png")
  expect_identical(copied$second$rotation, first$rotation)
  expect_identical(copied$second$flip_x, first$flip_x)
  expect_identical(copied$second$dx, first$dx)
  expect_identical(copied$second$image_opacity, first$image_opacity)
  expect_identical(copied$second$point_opacity, first$point_opacity)
  expect_identical(copied$second$point_size, first$point_size)
  expect_false(identical(copied$second$bounds, first$bounds))

  reset <- builder_alignment_reset(first)
  expect_identical(reset$source_uri, first$source_uri)
  expect_identical(reset$base_bounds, first$base_bounds)
  expect_identical(reset$dx, 0)
  expect_identical(reset$rotation, 0)
  expect_identical(reset$bounds, first$base_bounds)
  expect_false("saved" %in% names(reset))
})

test_that("legacy image records drop saved flags and gain canonical defaults", {
  legacy <- list(
    uri = "data:image/png;base64,AAAA",
    bounds = list(xmin = 0, xmax = 4, ymin = 0, ymax = 3),
    saved = FALSE
  )
  normalized <- builder_alignment_normalize(legacy, section_id = "fov")
  expect_false("saved" %in% names(normalized))
  expect_identical(normalized$base_bounds, legacy$bounds)
  expect_identical(normalized$section_id, "fov")
  expect_identical(normalized$point_size, 5)
})

test_that("named spatial image collections normalize without losing labels", {
  record <- function(section, filename) {
    builder_alignment_record(
      source = list(name = filename, type = "image/png", size = 4),
      source_uri = "data:image/png;base64,AAAA",
      uri = "data:image/png;base64,AAAA",
      base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      section = list(id = section, kind = "spatial")
    )
  }
  images <- list(
    section_a = list(
      `H&E` = record("section_a", "H&E.png"),
      DAPI = record("section_a", "DAPI.png")
    )
  )

  normalized <- builder_image_collection_normalize(images)
  expect_named(normalized, "section_a")
  expect_named(normalized$section_a, c("H&E", "DAPI"))
  expect_identical(builder_image_collection_count(normalized), 2L)
  expect_identical(
    vapply(
      builder_image_collection_flatten(normalized),
      `[[`,
      "",
      "image_label"
    ),
    c("H&E", "DAPI")
  )
  expect_error(
    builder_image_collection_normalize(list(
      section_a = setNames(
        list(record(
          "section_a",
          "bad.png"
        )),
        ""
      )
    )),
    "non-empty"
  )
  duplicate <- structure(
    list(record("section_a", "A.png"), record("section_a", "B.png")),
    names = c("H&E", "H&E")
  )
  expect_error(
    builder_image_collection_normalize(list(section_a = duplicate)),
    "unique"
  )

  legacy <- list(section_a = record("section_a", "H&E.png"))
  upgraded <- builder_image_collection_normalize(legacy)
  expect_named(upgraded$section_a, "H&E.png")
})

test_that("named spatial image actions preserve unaffected records", {
  record <- function(section, filename, dx = 0) {
    builder_alignment_record(
      source = list(name = filename, type = "image/png", size = 4),
      source_uri = paste0("data:image/png;base64,", filename),
      uri = paste0("data:image/png;base64,", filename),
      base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      parameters = list(dx = dx),
      section = list(id = section, kind = "spatial")
    )
  }
  he <- record("section_a", "he.png", dx = 3)
  dapi <- record("section_a", "dapi.png")
  images <- builder_image_collection_add(list(), "section_a", "H&E", he)
  images <- builder_image_collection_add(images, "section_a", "DAPI", dapi)
  expect_named(images$section_a, c("H&E", "DAPI"))
  expect_identical(images$section_a[["H&E"]], he)

  renamed <- builder_image_collection_rename(
    images,
    "section_a",
    "DAPI",
    "IF"
  )
  expect_named(renamed$section_a, c("H&E", "IF"))
  expect_identical(renamed$section_a[["IF"]], dapi)
  expect_identical(renamed$section_a[["H&E"]], he)
  expect_error(
    builder_image_collection_rename(renamed, "section_a", "IF", "H&E"),
    "unique"
  )

  removed <- builder_image_collection_remove(renamed, "section_a", "H&E")
  expect_named(removed$section_a, "IF")
  expect_identical(removed$section_a$IF, dapi)
})

test_that("matching-label transform never crosses image identities", {
  record <- function(section, filename, dx) {
    builder_alignment_record(
      source = list(name = filename, type = "image/png", size = 4),
      source_uri = paste0("data:image/png;base64,", filename),
      uri = paste0("data:image/png;base64,", filename),
      base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      parameters = list(dx = dx),
      section = list(id = section, kind = "spatial")
    )
  }
  images <- list(
    section_a = list(
      `H&E` = record("section_a", "a-he.png", 4),
      DAPI = record("section_a", "a-dapi.png", 7)
    ),
    section_b = list(
      `H&E` = record("section_b", "b-he.png", 0),
      DAPI = record("section_b", "b-dapi.png", 1)
    )
  )
  copied <- builder_alignment_apply_transform_to_matching_label(
    images,
    "section_a",
    "H&E"
  )
  expect_identical(copied$section_b[["H&E"]]$dx, 4)
  expect_false("saved" %in% names(copied$section_b[["H&E"]]))
  expect_identical(copied$section_b$DAPI, images$section_b$DAPI)
})

test_that("matching-label rendering rolls back and reports failed sections", {
  record <- function(section, source_uri, dx) {
    builder_alignment_record(
      source = list(
        name = paste0(section, ".png"),
        type = "image/png",
        size = 4
      ),
      source_uri = source_uri,
      uri = source_uri,
      base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      parameters = list(dx = dx),
      section = list(id = section, kind = "spatial")
    )
  }
  original <- list(
    section_a = list(H = record("section_a", "good", 4)),
    section_b = list(H = record("section_b", "good", 0)),
    section_c = list(H = record("section_c", "bad", 0))
  )
  transformed <- builder_alignment_apply_transform_to_matching_label(
    original,
    "section_a",
    "H"
  )
  rendered <- builder_alignment_render_matching_label(
    transformed,
    original,
    "H",
    read_image = function(uri) {
      if (identical(uri, "bad")) {
        return(list(error = "unsafe image"))
      }
      list(array = array(1, dim = c(2L, 2L, 3L)), width = 2L, height = 2L)
    },
    encode_image = function(...) {
      list(
        uri = "rendered",
        bytes = 4L,
        width = 2L,
        height = 2L,
        source_width = 2L,
        source_height = 2L,
        extent_width = 2L,
        extent_height = 2L,
        display_width = 2L,
        display_height = 2L
      )
    }
  )

  expect_identical(rendered$successful_sections, c("section_a", "section_b"))
  expect_identical(rendered$failed_sections, "section_c")
  expect_identical(rendered$images$section_c$H, original$section_c$H)
  expect_identical(rendered$images$section_a$H$uri, "rendered")
  expect_identical(rendered$images$section_b$H$uri, "rendered")
  expect_identical(rendered$images$section_b$H$dx, 4)

  server <- paste(
    readLines(
      builder_spatial_test_inst_path("builder", "spatial_alignment_server.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    server,
    "count <- length(rendered$successful_sections)",
    fixed = TRUE
  )
  expect_match(server, "if (count > 0L)", fixed = TRUE)
})

test_that("snapshot drop acknowledgement clears every preview cache", {
  imports <- paste(
    readLines(
      builder_spatial_test_inst_path("builder", "server", "imports.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(imports, "builder_preview_cache_drop_dataset(", fixed = TRUE)
  expect_match(
    imports,
    "builder_spatial_preview_cache_drop_dataset(",
    fixed = TRUE
  )
  expect_match(imports, "projection_previews(", fixed = TRUE)
  expect_match(imports, "trajectory_previews(", fixed = TRUE)
  expect_match(imports, "spatial_previews(", fixed = TRUE)
})

test_that("coordinate drafts stay partitioned and reject stale browser events", {
  drafts <- list()
  first <- builder_coordinate_drafts_put(
    drafts,
    dataset = "dataset-a",
    snapshot_identity = "snapshot-a",
    section = "fov-a",
    spec = list(rotation_degrees = 37.5, scale = 1),
    sequence = 4
  )
  expect_true(first$accepted)

  second <- builder_coordinate_drafts_put(
    first$drafts,
    dataset = "dataset-a",
    snapshot_identity = "snapshot-a",
    section = "fov-b",
    spec = list(rotation_degrees = -12, scale = 1),
    sequence = 5
  )
  stale <- builder_coordinate_drafts_put(
    second$drafts,
    dataset = "dataset-a",
    snapshot_identity = "snapshot-a",
    section = "fov-a",
    spec = list(rotation_degrees = 0, scale = 1),
    sequence = 3
  )

  expect_false(stale$accepted)
  expect_identical(
    builder_coordinate_drafts_get(stale$drafts, "dataset-a", "fov-a")$spec,
    list(schema_version = 1L, rotation_degrees = 37.5, scale = 1)
  )
  expect_identical(
    builder_coordinate_drafts_get(stale$drafts, "dataset-a", "fov-b")$spec,
    list(schema_version = 1L, rotation_degrees = -12, scale = 1)
  )
})

test_that("coordinate draft pruning removes deleted and stale snapshots", {
  drafts <- list()
  drafts <- builder_coordinate_drafts_put(
    drafts,
    "dataset-a",
    "old-snapshot",
    "fov-a",
    list(rotation_degrees = 10, scale = 1),
    1
  )$drafts
  drafts <- builder_coordinate_drafts_put(
    drafts,
    "dataset-b",
    "snapshot-b",
    "fov-b",
    list(rotation_degrees = 20, scale = 1),
    2
  )$drafts
  drafts <- builder_coordinate_drafts_put(
    drafts,
    "dataset-c",
    "snapshot-c",
    "fov-c",
    list(rotation_degrees = 30, scale = 1),
    3
  )$drafts

  pruned <- builder_coordinate_drafts_prune(
    drafts,
    list(
      list(id = "dataset-a", snapshot_identity = "new-snapshot"),
      list(id = "dataset-b", snapshot_identity = "snapshot-b")
    )
  )

  expect_null(builder_coordinate_drafts_get(
    pruned$drafts,
    "dataset-a",
    "fov-a"
  ))
  expect_identical(
    builder_coordinate_drafts_get(
      pruned$drafts,
      "dataset-b",
      "fov-b"
    )$spec$rotation_degrees,
    20
  )
  expect_null(builder_coordinate_drafts_get(
    pruned$drafts,
    "dataset-c",
    "fov-c"
  ))
  expect_setequal(pruned$removed, c("dataset-a::fov-a", "dataset-c::fov-c"))
})

test_that("coordinate draft pruning treats artifact entries as snapshotless", {
  drafts <- builder_coordinate_drafts_put(
    list(),
    "dataset-a",
    "old-snapshot",
    "fov-a",
    list(rotation_degrees = 10, scale = 1),
    1
  )$drafts

  pruned <- builder_coordinate_drafts_prune(
    drafts,
    list(list(id = "dataset-a", snapshot_identity = NULL))
  )

  expect_null(pruned$drafts[["dataset-a"]])
  expect_identical(pruned$removed, "dataset-a::fov-a")
})

test_that("artifact entries do not initialize editable Spatial state", {
  skip_if_not_installed("shiny")
  entry <- list(
    id = "dataset-a",
    load_state = "artifact_ready",
    snapshot = NULL,
    profile = list(images = "fov-a", extras = list()),
    settings = list(
      name = "Dataset A",
      images = list(
        fov_a = list(
          `H&E` = list(
            project_asset = list(path = "spatial-assets/image.png"),
            bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10)
          )
        )
      )
    )
  )
  current <- shiny::reactiveVal(entry$id)

  shiny::testServer(
    function(input, output, session) {
      alignment <- builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) entry,
        entries = shiny::reactiveVal(list(entry)),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) stop("artifact entry requested a preview"),
        commit_images = function(entry, images) {
          stop("artifact entry committed editable images")
        },
        alignment_preview = shiny::reactiveVal(NULL),
        spatial_coords = shiny::reactiveVal(NULL)
      )
    },
    {
      session$flushReact()
      expect_null(alignment$active_section())
      expect_null(alignment$active_image())
      expect_null(alignment$draft())

      expect_true(alignment$restore_project_selection(list(
        dataset = "dataset-a",
        section = "fov-a",
        image = "H&E"
      )))
      session$flushReact()
      expect_null(alignment$active_section())
      expect_null(alignment$active_image())
    }
  )
})

test_that("points-only Spatial FOV appearance persists without an image", {
  skip_if_not_installed("shiny")
  entry <- list(
    id = "dataset-a",
    snapshot = list(
      path = "/private/dataset-a",
      owner_token = "owner-a",
      object_md5 = strrep("a", 32L)
    ),
    profile = list(images = "fov-a", extras = list()),
    settings = list(
      name = "Dataset A",
      images = list(),
      default_group = "cluster",
      default_projection = "umap",
      palette = "cerebro",
      spatial_point_appearance = list(
        "fov-a" = list(point_opacity = 0.65, point_size = 6)
      )
    )
  )
  current_entry <- shiny::reactiveVal(entry)
  current <- shiny::reactiveVal(entry$id)
  preview <- list(
    available = TRUE,
    bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    section = list(id = "fov-a", kind = "spatial", unit = "pixels"),
    projection_name = "umap",
    capped = FALSE,
    transcriptome = data.frame(
      cell_id = c("cell-a", "cell-b"),
      x = c(-1, 1),
      y = c(-1, 1),
      group = c("A", "B")
    ),
    spatial = data.frame(
      cell_id = c("cell-a", "cell-b"),
      x = c(2, 8),
      y = c(3, 7),
      group = c("A", "B")
    )
  )
  preview_state <- shiny::reactiveVal(NULL)

  shiny::testServer(
    function(input, output, session) {
      alignment <- builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) current_entry(),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) TRUE,
        commit_images = function(updated, images) {
          updated$settings$images <- images
          current_entry(updated)
          invisible(updated)
        },
        alignment_preview = preview_state,
        spatial_coords = shiny::reactiveVal(NULL)
      )
    },
    {
      session$flushReact()
      session$flushReact()
      expect_true(alignment$restore_project_settings("dataset-a"))
      session$flushReact()
      preview_state(preview)
      session$flushReact()
      expect_identical(alignment$active_section(), "fov-a")
      expect_identical(
        alignment$point_appearance(),
        list(opacity = 0.65, size = 6)
      )
      scene_generation <- alignment$canvas_contract()$generation
      renamed <- current_entry()
      renamed$settings$name <- "Renamed without changing Spatial"
      current_entry(renamed)
      session$flushReact()
      expect_identical(
        alignment$canvas_contract()$generation,
        scene_generation
      )
      session$setInputs(`enhance-point_opacity` = 70)
      session$flushReact()

      expect_identical(
        current_entry()$settings$spatial_point_appearance[["fov-a"]],
        list(point_opacity = 0.7, point_size = 6)
      )

      identity <- .builder_worker_identity(current_entry()$snapshot)
      session$setInputs(
        builder_spatial_coordinate_draft = list(
          dataset = "dataset-a",
          snapshotIdentity = identity,
          section = "fov-a",
          rotationDegrees = 35,
          sequence = 1
        )
      )
      before_reset <- alignment$canvas_contract()$resetToken
      session$setInputs(`enhance-reset_coordinate_transform` = 1L)
      session$flushReact()

      defaults <- builder_alignment_defaults()
      expect_identical(
        alignment$coordinate_drafts()[["dataset-a"]][["fov-a"]]$spec,
        list(schema_version = 1L, rotation_degrees = 0, scale = 1)
      )
      expect_identical(
        current_entry()$settings$spatial_point_appearance[["fov-a"]],
        defaults[c("point_opacity", "point_size")]
      )
      expect_identical(
        alignment$point_appearance(),
        list(opacity = defaults$point_opacity, size = defaults$point_size)
      )
      expect_gt(alignment$canvas_contract()$resetToken, before_reset)
    }
  )
})

test_that("coordinate drafts batch materialize transforms without image state", {
  record <- function(section) {
    builder_alignment_record(
      source = list(name = paste0(section, ".png")),
      source_uri = "data:image/png;base64,AA==",
      uri = "data:image/png;base64,AA==",
      base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      section = list(id = section, kind = "spatial")
    )
  }
  entry <- list(
    id = "dataset-a",
    settings = list(
      spatial_coordinate_transforms = list(
        "fov-b" = list(rotation_degrees = 19, scale = 1)
      ),
      images = list(
        "fov-a" = list(image = record("fov-a")),
        "fov-b" = list(image = record("fov-b")),
        "fov-c" = list(image = record("fov-c"))
      )
    )
  )
  drafts <- list()
  drafts <- builder_coordinate_drafts_put(
    drafts,
    "dataset-a",
    "snapshot-a",
    "fov-a",
    list(rotation_degrees = 42, scale = 1),
    1
  )$drafts
  drafts <- builder_coordinate_drafts_put(
    drafts,
    "dataset-a",
    "snapshot-a",
    "fov-b",
    list(rotation_degrees = 0, scale = 1),
    2
  )$drafts

  applied <- builder_coordinate_drafts_apply_entry(
    entry,
    drafts[["dataset-a"]],
    snapshot_identity = "snapshot-a"
  )

  expect_true(applied$changed)
  expect_setequal(applied$sections, c("fov-a", "fov-b"))
  expect_identical(
    applied$entry$settings$spatial_coordinate_transforms[["fov-a"]],
    list(schema_version = 1L, rotation_degrees = 42, scale = 1)
  )
  expect_null(applied$entry$settings$spatial_coordinate_transforms[["fov-b"]])
  expect_false(
    "saved" %in%
      names(
        applied$entry$settings$images[["fov-a"]]$image
      )
  )
  expect_false(
    "saved" %in%
      names(
        applied$entry$settings$images[["fov-b"]]$image
      )
  )
  expect_false(
    "saved" %in%
      names(
        applied$entry$settings$images[["fov-c"]]$image
      )
  )
})

test_that("coordinate release events stay light until one batched materialization", {
  skip_if_not_installed("shiny")
  entry <- list(
    id = "dataset-a",
    snapshot = list(
      path = "/private/dataset-a",
      owner_token = "owner-a",
      object_md5 = strrep("a", 32L)
    ),
    profile = list(images = c("fov-a", "fov-b"), extras = list()),
    settings = list(
      name = "Dataset A",
      images = list(),
      spatial_coordinate_transforms = list(),
      default_group = "cluster",
      default_projection = "umap",
      palette = "cerebro"
    )
  )
  current_entry <- shiny::reactiveVal(entry)
  current <- shiny::reactiveVal(entry$id)
  commit_count <- 0L
  identity <- .builder_worker_identity(entry$snapshot)

  shiny::testServer(
    function(input, output, session) {
      alignment <- builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) current_entry(),
        entries = shiny::reactive(list(current_entry())),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) TRUE,
        commit_images = function(updated, images) {
          updated$settings$images <- images
          current_entry(updated)
          commit_count <<- commit_count + 1L
          invisible(updated)
        },
        alignment_preview = shiny::reactiveVal(NULL),
        spatial_coords = shiny::reactiveVal(NULL)
      )
    },
    {
      session$flushReact()
      session$setInputs(
        builder_spatial_coordinate_draft = list(
          dataset = "dataset-a",
          snapshotIdentity = identity,
          section = "fov-a",
          rotationDegrees = 37.5,
          sequence = 2
        )
      )
      session$setInputs(`enhance-active_section` = "fov-b")
      session$setInputs(
        builder_spatial_coordinate_draft = list(
          dataset = "dataset-a",
          snapshotIdentity = identity,
          section = "fov-b",
          rotationDegrees = -12,
          sequence = 3
        )
      )
      session$setInputs(
        builder_spatial_coordinate_draft = list(
          dataset = "dataset-a",
          snapshotIdentity = identity,
          section = "fov-a",
          rotationDegrees = 0,
          sequence = 1
        )
      )
      session$flushReact()

      expect_identical(commit_count, 0L)
      expect_identical(
        alignment$coordinate_drafts()[["dataset-a"]][[
          "fov-a"
        ]]$spec$rotation_degrees,
        37.5
      )
      expect_identical(
        alignment$coordinate_drafts()[["dataset-a"]][[
          "fov-b"
        ]]$spec$rotation_degrees,
        -12
      )

      materialized <- alignment$materialize_coordinate_drafts()
      expect_true(materialized$ok)
      expect_identical(commit_count, 1L)
      expect_identical(
        current_entry()$settings$spatial_coordinate_transforms[[
          "fov-a"
        ]]$rotation_degrees,
        37.5
      )
      expect_identical(
        current_entry()$settings$spatial_coordinate_transforms[[
          "fov-b"
        ]]$rotation_degrees,
        -12
      )
      expect_length(alignment$coordinate_drafts(), 0L)
    }
  )
})

test_that("project restore replaces default coordinate drafts authoritatively", {
  skip_if_not_installed("shiny")
  entry <- list(
    id = "ds1",
    snapshot = list(
      path = "/private/ds1",
      owner_token = "owner-ds1",
      object_md5 = strrep("a", 32L)
    ),
    profile = list(images = "section_a_1_fov_1", extras = list()),
    settings = list(
      name = "Dataset 1",
      images = list(),
      spatial_coordinate_transforms = list(),
      default_group = "cluster",
      default_projection = "umap",
      palette = "cerebro"
    )
  )
  preview <- list(
    available = TRUE,
    bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    section = list(
      id = "section_a_1_fov_1",
      kind = "spatial",
      unit = "pixels"
    ),
    projection_name = "umap",
    capped = FALSE,
    spatial = data.frame(
      cell_barcode = c("cell-a", "cell-b"),
      x = c(2, 8),
      y = c(3, 7),
      group = c("A", "B"),
      stringsAsFactors = FALSE
    )
  )
  current_entry <- shiny::reactiveVal(entry)
  current <- shiny::reactiveVal(entry$id)
  alignment_preview <- shiny::reactiveVal(preview)
  input_messages <- list()
  identity <- .builder_worker_identity(entry$snapshot)

  shiny::testServer(
    function(input, output, session) {
      session$sendInputMessage <- function(input_id, message) {
        input_messages[[length(input_messages) + 1L]] <<- list(
          id = input_id,
          message = message
        )
        invisible()
      }
      alignment <- builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) current_entry(),
        entries = shiny::reactive(list(current_entry())),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) TRUE,
        commit_images = function(updated, images) {
          updated$settings$images <- images
          current_entry(updated)
          invisible(updated)
        },
        alignment_preview = alignment_preview,
        spatial_coords = shiny::reactiveVal(NULL)
      )
    },
    {
      session$flushReact()
      alignment_preview(preview)
      session$flushReact()
      initial_scene <- alignment$canvas_contract()
      expect_identical(initial_scene$controls$coordinateRotation, 0)
      session$setInputs(
        builder_spatial_coordinate_draft = list(
          dataset = "ds1",
          snapshotIdentity = identity,
          section = "section_a_1_fov_1",
          rotationDegrees = 0,
          sequence = 1,
          generation = initial_scene$generation
        )
      )
      session$flushReact()

      restored <- current_entry()
      restored$settings$spatial_coordinate_transforms <- list(
        section_a_1_fov_1 = list(rotation_degrees = 66.9, scale = 1)
      )
      current_entry(restored)
      expect_identical(
        current_entry()$settings$spatial_coordinate_transforms[[
          "section_a_1_fov_1"
        ]]$rotation_degrees,
        66.9
      )
      expect_identical(
        alignment$coordinate_drafts()[["ds1"]][[
          "section_a_1_fov_1"
        ]]$spec$rotation_degrees,
        0
      )
      alignment$restore_project_settings("ds1")
      session$setInputs(
        builder_spatial_coordinate_draft = list(
          dataset = "ds1",
          snapshotIdentity = identity,
          section = "section_a_1_fov_1",
          rotationDegrees = 0,
          sequence = 2,
          generation = initial_scene$generation
        )
      )
      session$flushReact()
      session$flushReact()

      expect_identical(
        current_entry()$settings$spatial_coordinate_transforms[[
          "section_a_1_fov_1"
        ]]$rotation_degrees,
        66.9
      )
      slider_updates <- Filter(
        function(item) identical(item$id, "enhance-coordinate_rotation"),
        input_messages
      )
      expect_gt(length(slider_updates), 0L)
      expect_identical(
        as.numeric(tail(slider_updates, 1L)[[1L]]$message$value),
        66.9
      )
      expect_identical(
        alignment$canvas_contract()$controls$coordinateRotation,
        66.9
      )
      expect_length(alignment$coordinate_drafts(), 0L)

      restored_scene <- alignment$canvas_contract()
      session$setInputs(
        builder_spatial_coordinate_draft = list(
          dataset = "ds1",
          snapshotIdentity = identity,
          section = "section_a_1_fov_1",
          rotationDegrees = 12.5,
          sequence = 3,
          generation = restored_scene$generation
        )
      )
      session$flushReact()
      expect_identical(
        alignment$coordinate_drafts()[["ds1"]][[
          "section_a_1_fov_1"
        ]]$spec$rotation_degrees,
        12.5
      )
    }
  )
})

test_that("failed coordinate materialization retains its session drafts", {
  skip_if_not_installed("shiny")
  entry <- list(
    id = "dataset-a",
    snapshot = list(
      path = "/private/dataset-a",
      owner_token = "owner-a",
      object_md5 = strrep("a", 32L)
    ),
    profile = list(images = "fov-a", extras = list()),
    settings = list(images = list(), spatial_coordinate_transforms = list())
  )
  current_entry <- shiny::reactiveVal(entry)
  current <- shiny::reactiveVal(entry$id)
  identity <- .builder_worker_identity(entry$snapshot)

  shiny::testServer(
    function(input, output, session) {
      alignment <- builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) current_entry(),
        entries = shiny::reactive(list(current_entry())),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) TRUE,
        commit_images = function(entry, images) FALSE,
        alignment_preview = shiny::reactiveVal(NULL),
        spatial_coords = shiny::reactiveVal(NULL)
      )
    },
    {
      session$flushReact()
      session$setInputs(
        builder_spatial_coordinate_draft = list(
          dataset = "dataset-a",
          snapshotIdentity = identity,
          section = "fov-a",
          rotationDegrees = 18,
          sequence = 1
        )
      )
      session$flushReact()

      materialized <- alignment$materialize_coordinate_drafts(notify = FALSE)
      expect_false(materialized$ok)
      expect_identical(
        alignment$coordinate_drafts()[["dataset-a"]][[
          "fov-a"
        ]]$spec$rotation_degrees,
        18
      )
      expect_null(current_entry()$settings$spatial_coordinate_transforms[[
        "fov-a"
      ]])
    }
  )
})

test_that("serialized alignment payload excludes editing bytes and local paths", {
  record <- builder_alignment_record(
    source = list(name = "tissue.png", type = "image/png"),
    source_uri = "data:image/png;base64,SOURCE",
    uri = "data:image/png;base64,DISPLAY",
    base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 5),
    parameters = list(dx = 2, rotation = 15, point_size = 7),
    section = list(id = "fov", kind = "spatial")
  )
  record$datapath <- "/private/tmp/upload.png"
  payload <- builder_alignment_payload(record)

  expect_named(
    payload,
    c(
      "source",
      "builder_managed",
      "dx",
      "dy",
      "scale",
      "rotation",
      "flip_x",
      "flip_y",
      "image_opacity",
      "point_opacity",
      "point_size"
    )
  )
  expect_identical(payload$source, "tissue.png")
  expect_true(payload$builder_managed)
  expect_false(any(grepl("/private/tmp", unlist(payload), fixed = TRUE)))
  expect_false("source_uri" %in% names(payload))
  expect_false("datapath" %in% names(payload))
})

test_that("Spatial and Trekker alignments are partitioned without collision", {
  spatial <- list(
    fov = list(
      uri = "data:image/png;base64,SPATIAL",
      bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      section_kind = "spatial"
    ),
    trekker = list(
      uri = "data:image/png;base64,TREKKER",
      bounds = list(xmin = 20, xmax = 30, ymin = 40, ymax = 50),
      section_kind = "trekker"
    )
  )
  split <- builder_partition_alignments(spatial)
  expect_identical(names(split$spatial), "fov")
  expect_identical(split$trekker$uri, spatial$trekker$uri)
  expect_false("trekker" %in% names(split$spatial))
})

test_that("real Seurat image coordinates normalize by barcode", {
  skip_if_not_installed("SeuratObject")
  object <- builder_content_spatial_example_object()
  image <- names(object@images)[[1L]]
  raw <- SeuratObject::GetTissueCoordinates(object[[image]])

  contract <- builder_spatial_contract(object, image = image)
  coordinates <- contract$coordinates
  source_index <- match(coordinates$cell_barcode, raw$cell)

  expect_named(coordinates, c("cell_barcode", "x", "y"))
  expect_identical(coordinates$cell_barcode, SeuratObject::Cells(object))
  expect_equal(coordinates$x, raw$x[source_index])
  expect_equal(coordinates$y, raw$y[source_index])
  expect_identical(contract$preview, contract$export)

  legacy <- builder_spatial_coords(object, image)
  expect_identical(legacy[[1L]], coordinates$x)
  expect_identical(legacy[[2L]], coordinates$y)
})

test_that("metadata coordinates require explicit x and y selections", {
  cells <- c("cell-a", "cell-b", "cell-c")
  metadata <- data.frame(
    quality = c(100, 200, 300),
    x = c(1, 2, 3),
    y = c(4, 5, 6),
    row.names = cells
  )

  expect_error(
    builder_spatial_contract(metadata, cells, source = "metadata"),
    "explicit"
  )
  contract <- builder_spatial_contract(
    metadata,
    cells,
    coord_cols = c("x", "y"),
    source = "metadata"
  )
  expect_identical(contract$coordinates$x, metadata$x)
  expect_identical(contract$coordinates$y, metadata$y)
  expect_false(identical(contract$coordinates$x, metadata$quality))
  expect_error(
    builder_spatial_contract(
      metadata,
      cells,
      coord_cols = c("x", "x"),
      source = "metadata"
    ),
    "distinct"
  )

  arbitrary <- data.frame(
    quality = c(9, 8, 7),
    foo = c(11, 12, 13),
    bar = c(21, 22, 23),
    row.names = cells
  )
  explicit <- builder_spatial_contract(
    arbitrary,
    cells,
    coord_cols = c("foo", "bar"),
    source = "metadata"
  )
  expect_identical(explicit$coordinates$x, arbitrary$foo)
  expect_identical(explicit$coordinates$y, arbitrary$bar)
})

test_that("Seurat metadata subclasses fail before row-name dispatch", {
  skip_if_not_installed("SeuratObject")
  touched <- FALSE
  assign(
    "dimnames.builder_seurat_metadata_trap",
    function(value) {
      touched <<- TRUE
      stop("untrusted Seurat metadata dimnames method executed")
    },
    envir = .GlobalEnv
  )
  on.exit(
    rm("dimnames.builder_seurat_metadata_trap", envir = .GlobalEnv),
    add = TRUE
  )

  object <- builder_content_spatial_example_object()
  metadata <- methods::slot(object, "meta.data")
  metadata$x <- seq_len(nrow(metadata))
  metadata$y <- rev(metadata$x)
  class(metadata) <- c("builder_seurat_metadata_trap", "data.frame")
  methods::slot(object, "meta.data", check = FALSE) <- metadata

  expect_error(
    builder_spatial_contract(object, coord_cols = c("x", "y")),
    "unclassed base matrix|exact base data frame"
  )
  expect_false(touched)
})

test_that("Seurat coordinate aliases win over physical column order", {
  cells <- c("cell-a", "cell-b", "cell-c")
  coordinates <- data.frame(
    quality = c(99, 98, 97),
    y = c(40, 50, 60),
    x = c(10, 20, 30),
    cell = rev(cells)
  )

  contract <- builder_spatial_contract(
    coordinates,
    cells,
    barcodes = coordinates$cell,
    source = "seurat_image"
  )
  index <- match(cells, coordinates$cell)

  expect_identical(contract$coordinate_columns, c(x = "x", y = "y"))
  expect_identical(contract$coordinates$cell_barcode, cells)
  expect_identical(contract$coordinates$x, coordinates$x[index])
  expect_identical(contract$coordinates$y, coordinates$y[index])
})

test_that("spatial subsets filter outside rows before preview and export", {
  cells <- c("cell-a", "cell-b", "cell-c")
  metadata <- data.frame(
    foo = c(20, NA_real_, 10),
    bar = c(2, Inf, 1),
    row.names = c("cell-b", "outside", "cell-a")
  )
  contract <- builder_spatial_contract(
    metadata,
    cells,
    coord_cols = c("foo", "bar"),
    source = "metadata"
  )

  expect_identical(contract$match$extra, "outside")
  expect_identical(
    contract$coordinates$cell_barcode,
    c("cell-a", "cell-b")
  )
  expect_identical(contract$coordinates$x, c(10, 20))
  expect_false("outside" %in% contract$preview$cell_barcode)
  expect_false("outside" %in% contract$export$cell_barcode)
  expect_identical(contract$preview, contract$export)
})

test_that("spatial identity and coordinate damage fails closed", {
  cells <- c("cell-a", "cell-b", "cell-c")
  valid <- data.frame(
    x = c(1, 2),
    y = c(3, 4),
    row.names = cells[1:2]
  )

  duplicate <- valid
  attr(duplicate, "row.names") <- c("cell-a", "cell-a")
  expect_error(
    builder_spatial_contract(
      duplicate,
      cells,
      coord_cols = c("x", "y"),
      source = "metadata"
    ),
    "duplicate"
  )

  blank <- valid
  attr(blank, "row.names") <- c("cell-a", "")
  expect_error(
    builder_spatial_contract(
      blank,
      cells,
      coord_cols = c("x", "y"),
      source = "metadata"
    ),
    "blank"
  )

  expect_error(
    builder_spatial_contract(
      valid,
      c("cell-a", "cell-a", "cell-c"),
      coord_cols = c("x", "y"),
      source = "metadata"
    ),
    "duplicate"
  )

  non_finite <- valid
  non_finite$x[[2L]] <- Inf
  expect_error(
    builder_spatial_contract(
      non_finite,
      cells,
      coord_cols = c("x", "y"),
      source = "metadata"
    ),
    "finite"
  )
})

test_that("classed numeric spatial coordinates fail without method dispatch", {
  touched <- FALSE
  assign(
    "as.numeric.builder_coordinate_trap",
    function(value, ...) {
      touched <<- TRUE
      stop("untrusted coordinate conversion executed")
    },
    envir = .GlobalEnv
  )
  on.exit(
    rm("as.numeric.builder_coordinate_trap", envir = .GlobalEnv),
    add = TRUE
  )
  cells <- c("cell-a", "cell-b")
  coordinates <- data.frame(
    x = structure(c(1, 2), class = "builder_coordinate_trap"),
    y = c(3, 4),
    row.names = cells
  )

  expect_error(
    builder_spatial_contract(
      coordinates,
      cells,
      coord_cols = c("x", "y"),
      source = "metadata"
    ),
    "unclassed|base integer|base double"
  )
  expect_false(touched)
})

test_that("classed spatial coordinate tables fail without method dispatch", {
  touched <- FALSE
  method_names <- paste0(
    c("as.data.frame.", "dim.", "dimnames.", "row.names."),
    "builder_table_trap"
  )
  for (method_name in method_names) {
    assign(
      method_name,
      function(value, ...) {
        touched <<- TRUE
        stop("untrusted coordinate table method executed")
      },
      envir = .GlobalEnv
    )
  }
  on.exit(rm(list = method_names, envir = .GlobalEnv), add = TRUE)

  cells <- c("cell-a", "cell-b")
  matrix_table <- matrix(
    c(1, 2, 3, 4),
    nrow = 2L,
    dimnames = list(cells, c("x", "y"))
  )
  class(matrix_table) <- "builder_table_trap"
  data_frame_table <- data.frame(
    x = c(1, 2),
    y = c(3, 4),
    row.names = cells
  )
  class(data_frame_table) <- c("builder_table_trap", "data.frame")

  for (kind in c("matrix", "data_frame")) {
    touched <- FALSE
    coordinate_table <- if (identical(kind, "matrix")) {
      matrix_table
    } else {
      data_frame_table
    }
    expect_error(
      builder_spatial_contract(
        coordinate_table,
        cells,
        coord_cols = c("x", "y"),
        source = "metadata"
      ),
      "unclassed base matrix|exact base data frame",
      info = kind
    )
    expect_false(touched, info = kind)
  }
})

test_that("all supported image channel kinds normalize to RGBA", {
  gray <- matrix(c(0, 0.25, 0.75, 1), nrow = 2L)
  gray_alpha <- array(
    c(gray, matrix(c(1, 0.75, 0.5, 0.25), nrow = 2L)),
    dim = c(2L, 2L, 2L)
  )
  rgb <- array(seq(0, 1, length.out = 12L), dim = c(2L, 2L, 3L))
  rgba <- array(seq(0, 1, length.out = 16L), dim = c(2L, 2L, 4L))

  normalized <- list(
    grayscale = builder_normalize_image(gray, 20L),
    grayscale_alpha = builder_normalize_image(gray_alpha, 20L),
    rgb = builder_normalize_image(rgb, 20L),
    rgba = builder_normalize_image(rgba, 20L)
  )

  for (kind in names(normalized)) {
    got <- normalized[[kind]]
    expect_null(got$error, info = kind)
    expect_identical(dim(got$array), c(2L, 2L, 4L), info = kind)
    expect_identical(got$source_channel_kind, kind, info = kind)
  }
  for (channel in 1:3) {
    expect_equal(normalized$grayscale$array[,, channel], gray)
    expect_equal(
      normalized$grayscale_alpha$array[,, channel],
      gray_alpha[,, 1L]
    )
  }
  expect_equal(normalized$grayscale$array[,, 4L], matrix(1, 2L, 2L))
  expect_equal(
    normalized$grayscale_alpha$array[,, 4L],
    gray_alpha[,, 2L]
  )
  expect_equal(normalized$rgba$array, rgba)
})

test_that("image normalization records dimensions and bounds display size", {
  image <- array(seq(0, 1, length.out = 8L * 4L * 3L), dim = c(4L, 8L, 3L))
  got <- builder_normalize_image(image, max_display_px = 3L)

  expect_identical(got$source_dimensions, c(width = 8L, height = 4L))
  expect_identical(
    got$display_dimensions,
    c(width = got$display_width, height = got$display_height)
  )
  expect_lte(max(got$display_dimensions), 3L)
  expect_identical(dim(got$array)[3L], 4L)
})

test_that("image normalization rejects unsafe arrays and display limits", {
  cases <- list(
    non_numeric = array("x", dim = c(2L, 2L, 3L)),
    non_finite = array(c(rep(0, 11L), Inf), dim = c(2L, 2L, 3L)),
    out_of_range = array(c(rep(0, 11L), 2), dim = c(2L, 2L, 3L)),
    zero_channels = array(numeric(), dim = c(2L, 2L, 0L)),
    five_channels = array(0, dim = c(2L, 2L, 5L)),
    bad_dimensions = structure(numeric(4L), dim = c(2L, 2L, 1L, 1L))
  )
  for (name in names(cases)) {
    got <- builder_normalize_image(cases[[name]], 20L)
    expect_type(got$error, "character")
    expect_true(nzchar(got$error), info = name)
  }

  valid <- array(0, dim = c(2L, 2L, 3L))
  for (limit in list(
    0,
    -1,
    NA_real_,
    Inf,
    .Machine$integer.max + 1,
    numeric(),
    c(1, 2)
  )) {
    got <- builder_normalize_image(valid, limit)
    expect_match(got$error, "display", ignore.case = TRUE)
  }
})

test_that("PNG and JPEG read while TIFF variants give conversion guidance", {
  skip_if_not_installed("png")
  skip_if_not_installed("jpeg")
  directory <- withr::local_tempdir()
  rgb <- array(seq(0, 1, length.out = 27L), dim = c(3L, 3L, 3L))
  png_path <- file.path(directory, "image.png")
  jpeg_path <- file.path(directory, "image.jpeg")
  png::writePNG(rgb, png_path)
  jpeg::writeJPEG(rgb, jpeg_path)

  expect_null(builder_read_image(png_path)$error)
  expect_null(builder_read_image(jpeg_path)$error)

  for (extension in c("tif", "tiff", "ome.tif", "ome.tiff")) {
    path <- file.path(directory, paste0("image.", extension))
    file.create(path)
    got <- builder_read_image(path)
    expect_match(got$error, "convert", ignore.case = TRUE, info = extension)
    expect_match(got$error, "PNG", fixed = TRUE, info = extension)
    expect_match(got$error, "JPEG", fixed = TRUE, info = extension)
  }
})

test_that("image headers enforce a pixel budget before decoded arrays are kept", {
  skip_if_not_installed("png")
  skip_if_not_installed("jpeg")
  skip_if_not_installed("base64enc")
  directory <- withr::local_tempdir()
  rgb <- array(seq(0, 1, length.out = 27L), dim = c(3L, 3L, 3L))
  png_path <- file.path(directory, "budget.png")
  jpeg_path <- file.path(directory, "budget.jpeg")
  png::writePNG(rgb, png_path)
  jpeg::writeJPEG(rgb, jpeg_path)

  expect_identical(
    builder_image_file_dimensions(png_path, "budget.png"),
    c(width = 3L, height = 3L)
  )
  expect_identical(
    builder_image_file_dimensions(jpeg_path, "budget.jpeg"),
    c(width = 3L, height = 3L)
  )
  expect_match(
    builder_read_image(png_path, max_pixels = 8L)$error,
    "pixel safety limit",
    fixed = TRUE
  )
  expect_match(
    builder_read_image(jpeg_path, max_pixels = 8L)$error,
    "pixel safety limit",
    fixed = TRUE
  )
  expect_match(
    builder_read_image(png_path, max_encoded_bytes = 8L)$error,
    "encoded size limit",
    fixed = TRUE
  )
  uri <- paste0(
    "data:image/png;base64,",
    base64enc::base64encode(png_path)
  )
  expect_match(
    builder_read_image_uri(uri, max_pixels = 8L)$error,
    "pixel safety limit",
    fixed = TRUE
  )
  expect_match(
    builder_read_image_uri(uri, max_encoded_bytes = 8L)$error,
    "encoded size limit",
    fixed = TRUE
  )
})

test_that("the default resident raster is bounded by decoded R memory", {
  worst_case_bytes <- as.numeric(BUILDER_IMAGE_MAX_PIXELS) *
    BUILDER_IMAGE_DECODE_CHANNELS *
    8

  expect_lte(worst_case_bytes, BUILDER_IMAGE_MAX_RESIDENT_RASTER_BYTES)
  expect_lte(BUILDER_IMAGE_MAX_RESIDENT_RASTER_BYTES, 64 * 1024^2)
})

test_that("JPEG metadata scanning is bounded before main-process decode", {
  path <- withr::local_tempfile(fileext = ".jpeg")
  writeBin(
    c(as.raw(c(0xff, 0xd8)), raw(BUILDER_IMAGE_MAX_HEADER_BYTES)),
    path
  )

  dimensions <- builder_image_file_dimensions(path, "padded.jpeg")
  expect_true(is.list(dimensions))
  expect_match(dimensions$error, "metadata", ignore.case = TRUE)
  expect_match(
    builder_read_image(path, filename = "padded.jpeg")$error,
    "metadata",
    ignore.case = TRUE
  )
  parser <- paste(deparse(body(.builder_jpeg_dimensions)), collapse = "\n")
  expect_false(grepl("which(", parser, fixed = TRUE))
})

test_that("malformed PNG metadata is rejected before URI raster decode", {
  skip_if_not_installed("base64enc")
  malformed <- c(
    as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)),
    as.raw(c(0x00, 0x00, 0x00, 0x0c)),
    charToRaw("IHDR"),
    as.raw(c(0x00, 0x00, 0x00, 0x03)),
    as.raw(c(0x00, 0x00, 0x00, 0x03))
  )
  uri <- paste0(
    "data:image/png;base64,",
    base64enc::base64encode(malformed)
  )

  expect_null(.builder_png_dimensions(malformed))
  expect_match(
    builder_read_image_uri(uri)$error,
    "metadata",
    ignore.case = TRUE
  )
})

test_that("oversized PNG uint32 dimensions fail through the safety budget", {
  path <- withr::local_tempfile(fileext = ".png")
  header <- c(
    as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)),
    as.raw(c(0x00, 0x00, 0x00, 0x0d)),
    charToRaw("IHDR"),
    as.raw(c(0x80, 0x00, 0x00, 0x00)),
    as.raw(c(0x00, 0x00, 0x00, 0x01))
  )
  writeBin(header, path)

  dimensions <- builder_image_file_dimensions(path, "oversized.png")
  expect_identical(unname(dimensions), c(2147483648, 1))
  expect_false(anyNA(dimensions))
  expect_match(
    builder_read_image(path, filename = "oversized.png")$error,
    "pixel safety limit",
    fixed = TRUE
  )
})

test_that("image encoding can retain only the bounded editable raster", {
  skip_if_not_installed("png")
  skip_if_not_installed("base64enc")
  rgb <- array(seq(0, 1, length.out = 12L * 8L * 3L), dim = c(8L, 12L, 3L))

  encoded <- builder_encode_image(
    rgb,
    max_px = 4L,
    retain_normalized_array = TRUE
  )

  expect_null(encoded$error)
  encoded_bytes <- base64enc::base64decode(sub(
    "^[^,]+,",
    "",
    encoded$uri
  ))
  expect_identical(
    encoded$content_md5,
    unclass(as.character(openssl::md5(encoded_bytes)))
  )
  expect_identical(dim(encoded$normalized_array), c(3L, 4L, 4L))
  expect_identical(encoded$source_dimensions, c(width = 12L, height = 8L))
  expect_lte(prod(dim(encoded$normalized_array)), 4L * 4L * 4L)

  rotated <- builder_encode_image(
    encoded$normalized_array,
    max_px = 4L,
    rotate = 90,
    source_dimensions = encoded$source_dimensions
  )
  expect_null(rotated$error)
  expect_identical(rotated$source_dimensions, c(width = 12L, height = 8L))
})

test_that("image read failures never expose a server-side upload path", {
  skip_if_not_installed("png")
  path <- withr::local_tempfile(fileext = ".png")
  writeBin(charToRaw("not a png"), path)

  got <- builder_read_image(path, filename = "tissue.png")

  expect_identical(
    got$error,
    "Could not read this image. Check that it is a valid PNG or JPEG file."
  )
  expect_false(grepl(path, got$error, fixed = TRUE))
})

test_that("encoding round-trips grayscale alpha through RGBA", {
  skip_if_not_installed("png")
  skip_if_not_installed("base64enc")
  gray <- matrix(c(0, 0.25, 0.75, 1), nrow = 2L)
  alpha <- matrix(c(1, 0.5, 0.25, 0), nrow = 2L)
  gray_alpha <- array(c(gray, alpha), dim = c(2L, 2L, 2L))

  encoded <- builder_encode_image(gray_alpha, max_px = 10L)
  expect_null(encoded$error)
  expect_identical(encoded$source_channel_kind, "grayscale_alpha")
  raw <- base64enc::base64decode(sub("^[^,]+,", "", encoded$uri))
  path <- withr::local_tempfile(fileext = ".png")
  writeBin(raw, path)
  decoded <- png::readPNG(path)

  expect_identical(dim(decoded), c(2L, 2L, 4L))
  for (channel in 1:3) {
    expect_equal(decoded[,, channel], gray, tolerance = 1 / 255)
  }
  expect_equal(decoded[,, 4L], alpha, tolerance = 1 / 255)

  rotated <- builder_rotate_array(gray_alpha, 90)
  expect_identical(dim(rotated)[3L], 4L)
  expect_true(any(rotated[,, 4L] < 1))
})

test_that("quarter-turn rotations preserve every RGBA pixel exactly", {
  rgba <- array(
    seq(0.01, 0.96, length.out = 2L * 3L * 4L),
    dim = c(2L, 3L, 4L)
  )
  pixel_signatures <- function(image) {
    sort(as.vector(apply(image, c(1L, 2L), paste, collapse = ":")))
  }
  rotations <- lapply(
    c(`90` = 90, `180` = 180, `270` = 270, `-90` = -90),
    function(angle) {
      builder_rotate_array(rgba, angle)
    }
  )
  expected_dimensions <- list(
    `90` = c(3L, 2L, 4L),
    `180` = c(2L, 3L, 4L),
    `270` = c(3L, 2L, 4L),
    `-90` = c(3L, 2L, 4L)
  )

  for (angle in names(rotations)) {
    rotated <- rotations[[angle]]
    expect_identical(dim(rotated), expected_dimensions[[angle]], info = angle)
    expect_identical(
      pixel_signatures(rotated),
      pixel_signatures(rgba),
      info = angle
    )
    expect_identical(
      sort(as.vector(rotated[,, 4L])),
      sort(as.vector(rgba[,, 4L])),
      info = angle
    )
  }
  expect_identical(
    rotations[["180"]],
    rgba[
      rev(seq_len(dim(rgba)[1L])),
      rev(seq_len(dim(rgba)[2L])),
      ,
      drop = FALSE
    ]
  )
  expect_identical(rotations[["-90"]], rotations[["270"]])

  labelled <- array(1, dim = c(2L, 3L, 4L))
  labelled[,, 1L] <- matrix(1:6 / 10, nrow = 2L, byrow = TRUE)
  expect_identical(
    builder_rotate_array(labelled, 90)[,, 1L],
    matrix(c(3, 6, 2, 5, 1, 4) / 10, nrow = 3L, byrow = TRUE)
  )
  expect_identical(
    builder_rotate_array(labelled, -90)[,, 1L],
    matrix(c(4, 1, 5, 2, 6, 3) / 10, nrow = 3L, byrow = TRUE)
  )

  implementation <- paste(
    deparse(body(.builder_rotate_rgba)),
    collapse = "\n"
  )
  expect_false(grepl("aperm(", implementation, fixed = TRUE))
  expect_false(grepl("return(arr[", implementation, fixed = TRUE))
})

test_that("rotation plans preserve dimensions for thin images", {
  plan <- NULL
  expect_no_error(
    plan <- builder_rotation_plan(
      width = 1L,
      height = 4000L,
      degrees = 45,
      max_edge = 200L
    )
  )
  if (is.null(plan)) {
    return(invisible())
  }

  expect_identical(
    names(plan$input_dimensions),
    c("width", "height")
  )
  expect_lte(max(plan$output_dimensions), 200L)
})

test_that("valid thin images encode through the rotation plan", {
  skip_if_not_installed("png")
  skip_if_not_installed("base64enc")
  image <- array(
    seq(0.01, 0.99, length.out = 4000L * 4L),
    dim = c(4000L, 1L, 4L)
  )

  encoded <- builder_encode_image(image, max_px = 200L, rotate = 45)

  expect_null(encoded$error)
  expect_identical(
    encoded$source_dimensions,
    c(width = 1L, height = 4000L)
  )
  expect_lte(max(encoded$display_dimensions), 200L)
})

test_that("arbitrary rotation plans bound allocation before mapping", {
  expect_true(exists("builder_rotation_plan", mode = "function"))
  if (!exists("builder_rotation_plan", mode = "function")) {
    return(invisible())
  }

  plan <- builder_rotation_plan(
    width = 4000L,
    height = 4000L,
    degrees = 45,
    max_edge = 1400L
  )
  expect_identical(
    plan$full_extent_dimensions,
    c(width = 5657L, height = 5657L)
  )
  expect_lte(max(plan$output_dimensions), 1400L)
  expect_lt(max(plan$input_dimensions), 4000L)
  expect_true(plan$prescaled)

  moderate <- array(
    seq(0.01, 0.99, length.out = 80L * 100L * 4L),
    dim = c(80L, 100L, 4L)
  )
  rotated <- builder_rotate_array(moderate, 45, max_edge = 50L)
  expect_lte(max(dim(rotated)[1:2]), 50L)
  expect_identical(dim(rotated)[3L], 4L)
  expect_true(any(rotated[,, 4L] == 0))

  implementation <- paste(deparse(body(builder_rotate_array)), collapse = "\n")
  for (full_grid in c(
    "yy <- matrix",
    "xx <- matrix",
    "src_index <- ifelse"
  )) {
    expect_false(grepl(full_grid, implementation, fixed = TRUE))
  }

  encode_implementation <- paste(
    deparse(body(builder_encode_image)),
    collapse = "\n"
  )
  normalization_calls <- gregexpr(
    "builder_normalize_image",
    encode_implementation,
    fixed = TRUE
  )[[1L]]
  expect_identical(sum(normalization_calls > 0L), 1L)
})

test_that("arbitrary rotation preserves tiny raster pixels and alpha", {
  single <- array(c(0.2, 0.4, 0.6, 0.8), dim = c(1L, 1L, 4L))
  single_rotated <- builder_rotate_array(single, 45, max_edge = 10L)
  expect_true(any(single_rotated[,, 4L] == single[,, 4L]))
  expect_true(any(apply(
    single_rotated,
    c(1L, 2L),
    function(pixel) identical(as.numeric(pixel), as.numeric(single[1, 1, ]))
  )))

  strip <- array(0, dim = c(1L, 3L, 4L))
  strip[,, 1L] <- c(0.1, 0.2, 0.3)
  strip[,, 2L] <- c(0.4, 0.5, 0.6)
  strip[,, 3L] <- c(0.7, 0.8, 0.9)
  strip[,, 4L] <- c(0.25, 0.5, 0.75)
  strip_rotated <- builder_rotate_array(strip, 45, max_edge = 10L)
  expect_setequal(
    strip_rotated[,, 4L][strip_rotated[,, 4L] > 0],
    strip[,, 4L]
  )
})

test_that("rotation prescaling samples thin images from pixel centres", {
  plan <- builder_rotation_plan(
    width = 100L,
    height = 3L,
    degrees = 45,
    max_edge = 10L
  )
  expect_identical(plan$input_dimensions[["height"]], 1L)
  thin <- array(0, dim = c(3L, 100L, 4L))
  thin[1L, , 1L] <- 0.1
  thin[2L, , 1L] <- 0.5
  thin[3L, , 1L] <- 0.9
  thin[,, 4L] <- 1
  normalized <- builder_normalize_image(
    thin,
    max_display_px = plan$input_max_edge,
    display_dimensions = plan$input_dimensions
  )

  expect_identical(
    dim(normalized$array)[1:2],
    unname(rev(plan$input_dimensions))
  )
  expect_true(all(normalized$array[,, 1L] == 0.5))
})

test_that("small nonzero rotations keep extent and encoded pixels aligned", {
  skip_if_not_installed("png")
  skip_if_not_installed("base64enc")
  image <- array(
    seq(0.01, 0.99, length.out = 2L * 4L * 4L),
    dim = c(2L, 4L, 4L)
  )
  encoded <- builder_encode_image(image, max_px = 10L, rotate = 0.005)

  expect_identical(encoded$extent_dimensions, c(width = 5L, height = 3L))
  expect_identical(encoded$display_dimensions, encoded$extent_dimensions)
  bounds <- builder_image_bounds(
    "pixels",
    list(c(0, 1), c(0, 1)),
    encoded
  )
  expect_identical(
    c(width = bounds$xmax, height = bounds$ymax),
    encoded$extent_dimensions
  )
})

test_that("encoded display limits preserve transformed source extent", {
  skip_if_not_installed("png")
  skip_if_not_installed("base64enc")
  image <- array(
    seq(0, 1, length.out = 100L * 200L * 3L),
    dim = c(100L, 200L, 3L)
  )

  encoded <- builder_encode_image(image, max_px = 20L, rotate = 45)
  expect_lte(max(encoded$display_dimensions), 20L)
  expect_identical(encoded$source_dimensions, c(width = 200L, height = 100L))
  expect_identical(encoded$extent_dimensions, c(width = 213L, height = 213L))
  expect_identical(encoded$source_channel_kind, "rgb")
  expect_identical(encoded$display_channels, 4L)
  expect_identical(encoded$display_channel_kind, "rgba")
  expect_identical(encoded$channel_kind, "rgba")

  expect_identical(
    builder_image_bounds("pixels", list(1, 1), encoded),
    list(xmin = 0, xmax = 213L, ymin = 0, ymax = 213L)
  )
  expect_identical(
    builder_image_bounds(
      "physical",
      list(1, 1),
      encoded,
      um_per_px = 0.5
    ),
    list(xmin = 0, xmax = 106.5, ymin = 0, ymax = 106.5)
  )
})

test_that("rotated image extent drives bounds and Plotly aspect", {
  skip_if_not_installed("png")
  skip_if_not_installed("base64enc")
  skip_if_not_installed("plotly")
  image <- array(
    seq(0.01, 0.99, length.out = 2L * 4L * 4L),
    dim = c(2L, 4L, 4L)
  )
  expected <- list(
    `90` = c(width = 2L, height = 4L),
    `45` = c(width = 5L, height = 5L)
  )

  for (angle in names(expected)) {
    encoded <- builder_encode_image(
      image,
      max_px = 2L,
      rotate = as.numeric(angle)
    )
    expect_identical(
      encoded$source_dimensions,
      c(width = 4L, height = 2L),
      info = angle
    )
    expect_identical(
      encoded$extent_dimensions,
      expected[[angle]],
      info = angle
    )
    expect_lte(max(encoded$display_dimensions), 2L)
    bounds <- builder_image_bounds(
      "pixels",
      list(c(0, 1), c(0, 1)),
      encoded
    )
    expect_identical(
      bounds,
      list(
        xmin = 0,
        xmax = expected[[angle]][["width"]],
        ymin = 0,
        ymax = expected[[angle]][["height"]]
      ),
      info = angle
    )
    built <- plotly::plotly_build(builder_overlay_plot(
      data.frame(sx = c(0, 1), sy = c(0, 1)),
      encoded$uri,
      bounds
    ))
    layout_image <- built$x$layout$images[[1L]]
    expect_equal(
      layout_image$sizex / layout_image$sizey,
      expected[[angle]][["width"]] / expected[[angle]][["height"]],
      info = angle
    )
  }
})

test_that("alignment records propagate transformed extent facts", {
  picture <- list(
    uri = "data:image/png;base64,AAAA",
    bytes = 4L,
    width = 2L,
    height = 2L,
    source_width = 4L,
    source_height = 2L,
    extent_width = 2L,
    extent_height = 4L,
    display_width = 2L,
    display_height = 2L
  )
  per_section <- list(
    section = list(
      bounds = list(xmin = 0, xmax = 2L, ymin = 0, ymax = 4L),
      cover = list(outside = 0L, total = 2L)
    )
  )
  paired <- builder_pair_sections(picture, per_section)$section

  for (field in c(
    "source_width",
    "source_height",
    "extent_width",
    "extent_height",
    "display_width",
    "display_height"
  )) {
    expect_identical(paired[[field]], picture[[field]], info = field)
  }

  app <- builder_app_source_text()
  alignment_server <- paste(
    readLines(
      builder_spatial_test_inst_path(
        "builder",
        "spatial_alignment_server.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  session <- paste(
    readLines(
      builder_spatial_test_inst_path("builder", "session.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(alignment_server, '"extent_width"', fixed = TRUE)
  expect_match(alignment_server, '"extent_height"', fixed = TRUE)
  expect_match(alignment_server, "builder_alignment_record", fixed = TRUE)
  expect_match(alignment_server, "builder_encode_image", fixed = TRUE)
  expect_match(app, "nxt\\$extent_width")
  expect_match(app, "nxt\\$extent_height")
  expect_match(session, "extent_width")
  expect_match(session, "extent_height")
})

test_that("one slide applied to every section keeps each section's own extent", {
  picture <- list(
    uri = "data:image/png;base64,AAAA",
    bytes = 4L,
    width = 300L,
    height = 240L
  )
  per_section <- list(
    A = list(
      bounds = list(xmin = 0, xmax = 100, ymin = 0, ymax = 80),
      cover = list(outside = 0L, total = 100L)
    ),
    B = list(
      bounds = list(xmin = 500, xmax = 600, ymin = 0, ymax = 80),
      cover = list(outside = 0L, total = 100L)
    ),
    C = list(
      bounds = list(xmin = 2000, xmax = 2100, ymin = 0, ymax = 80),
      cover = list(outside = 7L, total = 100L)
    )
  )

  got <- builder_pair_sections(picture, per_section)
  expect_identical(unique(vapply(got, function(x) x$uri, "")), picture$uri)
  expect_identical(
    vapply(got, function(x) x$bounds$xmin, numeric(1)),
    c(A = 0, B = 500, C = 2000)
  )
  expect_identical(
    vapply(got, function(x) x$outside, integer(1)),
    c(A = 0L, B = 0L, C = 7L)
  )
})

test_that("Linked views seeds Builder image opacity from saved alignment", {
  path <- builder_spatial_test_inst_path(
    "viewer",
    "coordinated_views",
    "bundle.R"
  )
  bundle <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(bundle, "histology_images", fixed = TRUE)
  expect_match(bundle, "histology_image_bounds", fixed = TRUE)
  expect_match(bundle, "histology_alignment", fixed = TRUE)
  expect_match(bundle, "image_opacity", fixed = TRUE)
})
