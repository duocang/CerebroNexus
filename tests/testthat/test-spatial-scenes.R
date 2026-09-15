test_that("spatial scene choices show only FOV identifiers", {
  metadata <- data.frame(
    cell_barcode = paste0("cell-", 1:4),
    sample = c("S1", "S1", "S2", "S2"),
    sample_roi = c("S1_lesion", "S1_border", "S2_normal", "S2_normal"),
    stringsAsFactors = FALSE
  )
  rownames(metadata) <- metadata$cell_barcode
  spatial_data <- list(
    `fov-a` = list(
      coordinates = matrix(
        1:4,
        ncol = 2L,
        dimnames = list(c("cell-1", "cell-2"), c("x", "y"))
      )
    ),
    `fov-b` = list(
      coordinates = matrix(
        5:8,
        ncol = 2L,
        dimnames = list(c("cell-3", "cell-4"), c("x", "y"))
      )
    )
  )

  choices <- spatial_scene_choices(
    c("fov-a", "fov-b"),
    spatial_data,
    metadata
  )

  expect_identical(unname(choices), c("fov-a", "fov-b"))
  expect_identical(names(choices), c("fov-a", "fov-b"))
})

test_that("Viewer opens multiple ROIs in the Builder's separate layout", {
  expect_identical(spatial_default_roi_selection(c("ROI1", "ROI2")), "__separate__")
  expect_identical(spatial_default_roi_selection("ROI1"), "__all__")
  expect_identical(spatial_default_roi_selection(character()), "__all__")
})

test_that("Viewer keeps split-by on the shared interactive Canvas", {
  root <- testthat::test_path("..", "..", "inst", "viewer", "spatial")
  controls <- paste(
    readLines(file.path(root, "UI_projection_main_parameters.R"), warn = FALSE),
    collapse = "\n"
  )
  layout <- paste(
    readLines(file.path(root, "UI_projection.R"), warn = FALSE),
    collapse = "\n"
  )
  parameters <- paste(
    readLines(
      file.path(root, "obj_projection_parameters_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(controls, "spatial_projection_split_by", fixed = TRUE)
  expect_match(
    layout,
    'cerebroCellViewOutput("spatial_projection")',
    fixed = TRUE
  )
  expect_no_match(layout, "spatial_projection_split_plot", fixed = TRUE)
  expect_match(parameters, "split_by", fixed = TRUE)

  engine <- paste(
    readLines(file.path(root, "..", "www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(engine, "space.xRange = panel.x_range", fixed = TRUE)
  expect_match(engine, "space.yRange = panel.y_range", fixed = TRUE)
  expect_match(
    engine,
    "panel.background_image || meta.background_image",
    fixed = TRUE
  )
  expect_match(engine, "JSON.stringify(panel.image_identity)", fixed = TRUE)

  expect_match(
    controls,
    "spatial_projection_roi_background_images",
    fixed = TRUE
  )
  expect_match(controls, "grouped_choices", fixed = TRUE)
  expect_match(controls, "multiple = TRUE", fixed = TRUE)
  expect_match(controls, 'plugins = list("remove_button")', fixed = TRUE)
  expect_match(controls, "onItemAdd", fixed = TRUE)
  expect_false(grepl("maxItems", controls, fixed = TRUE))
  expect_match(
    controls,
    'identical(selected_roi, "__all__")',
    fixed = TRUE
  )
})

test_that("spatial split choices reuse safe categorical metadata", {
  metadata <- data.frame(
    cell_barcode = paste0("cell-", 1:6),
    numeric_group = rep(1:2, each = 3),
    cell_type = rep(c("T", "B"), 3),
    condition = factor(rep(c("control", "treated"), each = 3)),
    passed_qc = rep(c(TRUE, FALSE), 3),
    nUMI = seq_len(6),
    constant = "same",
    identifier = paste0("id-", 1:6),
    stringsAsFactors = FALSE
  )

  expect_identical(
    spatial_split_columns(
      metadata,
      metadata$cell_barcode,
      groups = "numeric_group"
    ),
    c("numeric_group", "cell_type", "condition", "passed_qc")
  )
})

test_that("spatial categorical columns have no arbitrary level cap", {
  metadata <- data.frame(
    cell_barcode = paste0("cell-", 1:30),
    cluster = factor(c(paste0("C", 1:24), paste0("C", 1:6))),
    unregistered = factor(c(paste0("U", 1:13), rep("U1", 17))),
    stringsAsFactors = FALSE
  )

  expect_identical(
    spatial_split_columns(
      metadata,
      metadata$cell_barcode,
      groups = "cluster"
    ),
    c("cluster", "unregistered")
  )
})

test_that("optional Spatial selectors retain their reset choices", {
  controls <- paste(
    readLines(viewer_test_path("spatial", "UI_projection_main_parameters.R")),
    collapse = "\n"
  )

  expect_match(controls, "length(sample_values) > 1L", fixed = TRUE)
  expect_false(grepl('"All samples"', controls, fixed = TRUE))
  expect_match(controls, "spatial_split_columns(", fixed = TRUE)
  expect_match(controls, 'class = "spatial-image-dim-controls"', fixed = TRUE)
  expect_match(
    controls,
    'spatial_split_none_value <- "__none__"',
    fixed = TRUE
  )
  expect_match(controls, '"None" = spatial_split_none_value', fixed = TRUE)
})

test_that("Separate ROIs groups and normalizes one background per ROI", {
  rois <- paste0("ROI", 1:4)
  images <- list()
  for (roi in rois) {
    for (suffix in c("A", "B")) {
      label <- paste(roi, suffix, sep = "-")
      images[[label]] <- list(
        histology_image = paste0("data:image/png;base64,", label),
        histology_image_bounds = c(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
        roi_field = "sample_roi",
        roi_value = roi
      )
    }
  }
  groups <- spatial_roi_background_groups(
    list(histology_images = images),
    options = NULL,
    dataset = "dataset",
    spatial_name = "fov",
    roi_values = c(rois, "ROI-without-image")
  )

  expect_named(groups, rois)
  expect_true(all(vapply(
    groups,
    function(group) {
      identical(
        names(group$choices),
        c(
          "No Background",
          paste0(group$roi, "-A"),
          paste0(group$roi, "-B")
        )
      )
    },
    logical(1)
  )))

  selected <- c(
    groups$ROI1$tokens[["embedded::ROI1-B"]],
    groups$ROI2$tokens[["none"]],
    groups$ROI3$tokens[["embedded::ROI3-A"]],
    groups$ROI3$tokens[["embedded::ROI3-B"]],
    "invalid"
  )
  expect_identical(
    spatial_roi_background_selections(groups, selected),
    list(
      ROI1 = "embedded::ROI1-B",
      ROI2 = "none",
      ROI3 = "embedded::ROI3-A",
      ROI4 = "none"
    )
  )
  expect_identical(
    spatial_roi_background_selections(groups, selected[-4L]),
    list(
      ROI1 = "embedded::ROI1-B",
      ROI2 = "none",
      ROI3 = "embedded::ROI3-A",
      ROI4 = "none"
    )
  )
  expect_identical(
    spatial_roi_background_selections(groups, NULL),
    list(
      ROI1 = "embedded::ROI1-A",
      ROI2 = "embedded::ROI2-A",
      ROI3 = "embedded::ROI3-A",
      ROI4 = "embedded::ROI4-A"
    )
  )
  expect_identical(
    spatial_roi_background_selections(groups, character()),
    list(
      ROI1 = "none",
      ROI2 = "none",
      ROI3 = "none",
      ROI4 = "none"
    )
  )
})
