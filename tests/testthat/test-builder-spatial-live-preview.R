builder_spatial_canvas_node <- function() {
  skip_if(Sys.which("node") == "", "node not on PATH")
  skip_if_not_installed("jsonlite")
  runner <- testthat::test_path("builder-spatial-canvas-runtime-node.js")
  output <- tryCatch(
    system2("node", runner, stdout = TRUE, stderr = TRUE),
    error = function(error) NULL
  )
  if (is.null(output)) {
    skip("node cannot be launched from this R test process")
  }
  expect_identical(attr(output, "status") %||% 0L, 0L, info = output)
  jsonlite::fromJSON(tail(output, 1L), simplifyVector = FALSE)
}

test_that("Spatial Builder uses a persistent Canvas live preview", {
  root <- testthat::test_path("..", "..", "inst", "builder")
  ui <- paste(
    readLines(file.path(root, "ui", "enhance_stage.R"), warn = FALSE),
    collapse = "\n"
  )
  app <- paste(
    readLines(file.path(root, "app.R"), warn = FALSE),
    collapse = "\n"
  )
  server <- paste(
    readLines(file.path(root, "spatial_alignment_server.R"), warn = FALSE),
    collapse = "\n"
  )
  canvas <- file.path(root, "www", "builder-spatial-canvas.js")

  expect_true(file.exists(canvas))
  expect_match(ui, 'tags$canvas(', fixed = TRUE)
  expect_match(ui, 'class = "builder-spatial-canvas"', fixed = TRUE)
  expect_match(ui, '`aria-label` = label', fixed = TRUE)
  expect_false(grepl("plotly::plotlyOutput(", ui, fixed = TRUE))
  expect_match(app, '"builder-spatial-canvas.js"', fixed = TRUE)
  expect_false(grepl('plotly::renderPlotly({', server, fixed = TRUE))
  expect_match(server, '"builder_spatial_canvas_scene"', fixed = TRUE)
  expect_match(server, "builder_spatial_canvas_scene(", fixed = TRUE)
  expect_false(grepl("encoded <- shiny::reactive({", server, fixed = TRUE))
})

test_that("Canvas renderer owns bounded raw points and latest-only controls", {
  root <- testthat::test_path("..", "..", "inst", "builder")
  js <- paste(
    readLines(
      file.path(root, "www", "builder-spatial-canvas.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(js, "requestAnimationFrame", fixed = TRUE)
  expect_match(js, "var POINT_EDGE_PADDING = 18;", fixed = TRUE)
  expect_match(
    js,
    "var IMAGE_EDGE_PADDING = POINT_EDGE_PADDING / 2;",
    fixed = TRUE
  )
  expect_match(js, "builder_spatial_canvas_scene", fixed = TRUE)
  expect_match(js, "builder_spatial_canvas_reset", fixed = TRUE)
  expect_match(js, "generation", fixed = TRUE)
  expect_match(js, "resetToken", fixed = TRUE)
  expect_match(js, "viewKey", fixed = TRUE)
  expect_match(js, "devicePixelRatio", fixed = TRUE)
  expect_match(js, "Math.min(window.devicePixelRatio || 1, 2)", fixed = TRUE)
  expect_false(grepl("fitImageToViewport", js, fixed = TRUE))
  expect_match(js, "var left = screen({x: b.xmin, y: cy});", fixed = TRUE)
  expect_match(js, "var right = screen({x: b.xmax, y: cy});", fixed = TRUE)
  expect_match(js, "new ResizeObserver(schedule)", fixed = TRUE)
  expect_match(js, "pointermove", fixed = TRUE)
  expect_match(js, 'scene.layout === "separate"', fixed = TRUE)
  expect_match(js, "scene.roiImages", fixed = TRUE)
  expect_match(js, "loadImages", fixed = TRUE)
  expect_match(js, "drawImage(ctx, scene, screen, roiImage", fixed = TRUE)
  expect_match(js, "imageFitViewports", fixed = TRUE)
  expect_false(grepl("ctx.fillText(group", js, fixed = TRUE))
  expect_match(js, '"builder_spatial_roi_select"', fixed = TRUE)
  expect_match(js, "if (!groups.length)", fixed = TRUE)
  expect_match(js, "state.releaseGuardId = null;", fixed = TRUE)
  expect_match(js, "Shiny.setInputValue", fixed = TRUE)
  expect_match(js, "finishInteraction", fixed = TRUE)
  expect_match(js, '"builder_spatial_coordinate_draft"', fixed = TRUE)
  expect_match(js, "snapshotIdentity", fixed = TRUE)
  expect_match(js, "coordinateSequence", fixed = TRUE)
  expect_match(js, "generation: scene.generation", fixed = TRUE)
  expect_false(grepl(">= state.resetToken", js, fixed = TRUE))
  expect_false(grepl("Plotly", js, fixed = TRUE))
})

test_that("Canvas runtime keeps legacy persisted viewports and deduplicates Ion events", {
  result <- builder_spatial_canvas_node()

  legacy_view <- function(bounds, width, height, padding) {
    view_width <- bounds$xmax - bounds$xmin
    view_height <- bounds$ymax - bounds$ymin
    plot_width <- width - padding * 2
    plot_height <- height - padding * 2
    target_aspect <- plot_width / plot_height
    centre_x <- (bounds$xmin + bounds$xmax) / 2
    centre_y <- (bounds$ymin + bounds$ymax) / 2
    if (view_width / view_height < target_aspect) {
      view_width <- view_height * target_aspect
    } else {
      view_height <- view_width / target_aspect
    }
    list(
      xmin = centre_x - view_width / 2,
      xmax = centre_x + view_width / 2,
      ymin = centre_y - view_height / 2,
      ymax = centre_y + view_height / 2
    )
  }
  as_numeric_bounds <- function(value) {
    lapply(value, as.numeric)
  }

  expect_equal(
    as_numeric_bounds(result$overlay$viewports$`__section__`),
    legacy_view(
      list(xmin = 0, xmax = 10, ymin = 0, ymax = 20),
      500,
      400,
      2
    ),
    tolerance = 1e-12
  )
  expect_equal(
    mean(unlist(as_numeric_bounds(result$separate$viewports$A)[c(
      "xmin",
      "xmax"
    )])),
    -45,
    tolerance = 1e-12
  )
  expect_equal(
    mean(unlist(as_numeric_bounds(result$separate$viewports$A)[c(
      "ymin",
      "ymax"
    )])),
    100,
    tolerance = 1e-12
  )
  expect_equal(
    mean(unlist(as_numeric_bounds(result$separate$imageFitViewports$A)[c(
      "xmin",
      "xmax"
    )])),
    -45,
    tolerance = 1e-12
  )
  expect_equal(
    mean(unlist(as_numeric_bounds(result$separate$imageFitViewports$A)[c(
      "ymin",
      "ymax"
    )])),
    100,
    tolerance = 1e-12
  )
  expect_equal(as.numeric(result$separateFirstPoint$x), 127, tolerance = 1e-12)
  expect_equal(as.numeric(result$separateFirstPoint$y), 200, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(
    result$overlay$viewports$`__section__`,
    result$overlay$imageFitViewports$`__section__`
  )))
  expect_identical(result$numberValue, "0.01")
  expect_length(result$commits, 1L)
  expect_identical(as.numeric(result$commits[[1L]]$value$controls$scale), 0.01)
  expect_identical(as.numeric(result$commits[[1L]]$value$sequence), 1)
  expect_identical(result$tinyNumberValue, "0.001")
  expect_identical(result$tinySliderValue, "0.001")
  expect_identical(result$tinyStep, "0.001")
  expect_identical(result$authoritativeStep, "0.001")
  expect_length(result$tinyScaleCommits, 1L)
  expect_identical(
    as.numeric(result$tinyScaleCommits[[1L]]$value$controls$scale),
    0.001
  )
  expect_identical(
    as.numeric(result$tinyScaleCommits[[1L]]$value$sequence),
    2
  )
  expect_identical(as.integer(result$stopped), 2L)
  expect_identical(as.integer(result$pendingTimers), 0L)
})

test_that("paired number controls do not echo slider updates through Shiny", {
  root <- testthat::test_path("..", "..", "inst", "builder")
  js <- paste(
    readLines(
      file.path(root, "www", "builder-spatial-canvas.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  server <- paste(
    readLines(file.path(root, "spatial_alignment_server.R"), warn = FALSE),
    collapse = "\n"
  )
  sync_start <- regexpr(
    "sync_coordinate_control <- function",
    server,
    fixed = TRUE
  )[[1L]]
  sync_end <- regexpr(
    'sync_coordinate_control("img_scale", 0, 10)',
    server,
    fixed = TRUE
  )[[1L]]
  sync_block <- substr(server, sync_start, sync_end - 1L)

  expect_match(js, "function syncNumberControl(target)", fixed = TRUE)
  expect_match(js, "syncNumberControl(target);", fixed = TRUE)
  expect_false(grepl(
    "shiny::observeEvent(\n      input[[slider_id]]",
    sync_block,
    fixed = TRUE
  ))
})

test_that("point rotation refits one shared image and coordinate viewport", {
  root <- testthat::test_path("..", "..", "inst", "builder")
  js <- paste(
    readLines(
      file.path(root, "www", "builder-spatial-canvas.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  server <- paste(
    readLines(file.path(root, "spatial_alignment_server.R"), warn = FALSE),
    collapse = "\n"
  )
  compact_js <- gsub("[[:space:]]+", " ", js)

  expect_match(
    compact_js,
    "viewportLayout( scene.bounds, angle",
    fixed = TRUE
  )
  expect_match(
    compact_js,
    "viewportLayout( bounds, angle",
    fixed = TRUE
  )
  expect_match(js, "drawImage(ctx, scene, screen)", fixed = TRUE)
  expect_match(js, "drawPoints(ctx, scene, screen)", fixed = TRUE)
  expect_match(js, "drawImage(ctx, scene, screen, roiImage", fixed = TRUE)
  expect_false(grepl("pointLayout", compact_js, fixed = TRUE))
  expect_false(grepl(
    "include_coordinate_rotation = TRUE",
    server,
    fixed = TRUE
  ))
})

test_that("Canvas hover coalesces pointer work and reuses rendered screen points", {
  root <- testthat::test_path("..", "..", "inst", "builder")
  js <- paste(
    readLines(
      file.path(root, "www", "builder-spatial-canvas.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(js, "screenPoints: []", fixed = TRUE)
  expect_match(js, "state.screenPoints = new Array(p.x.length);", fixed = TRUE)
  expect_match(js, "state.screenPoints[index] = at;", fixed = TRUE)
  expect_match(
    js,
    "var cosine = Math.cos(angle), sine = Math.sin(angle);",
    fixed = TRUE
  )
  expect_match(js, "function setupCanvasHover(node)", fixed = TRUE)
  expect_match(js, 'node.addEventListener("pointermove"', fixed = TRUE)
  expect_match(js, "if (!state.hoverFrame)", fixed = TRUE)
  expect_match(js, "window.requestAnimationFrame(updateHover)", fixed = TRUE)
  expect_match(
    js,
    "window.cancelAnimationFrame(state.hoverFrame)",
    fixed = TRUE
  )
  expect_false(grepl(
    'document.addEventListener("pointermove"',
    js,
    fixed = TRUE
  ))
})

test_that("same-token scene refreshes preserve browser-local controls", {
  root <- testthat::test_path("..", "..", "inst", "builder")
  js <- paste(
    readLines(
      file.path(root, "www", "builder-spatial-canvas.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(js, "viewChanged || resetToken > state.resetToken", fixed = TRUE)
})

test_that("Canvas scene refreshes reuse image bytes already sent", {
  root <- testthat::test_path("..", "..", "inst", "builder")
  server <- paste(
    readLines(file.path(root, "spatial_alignment_server.R"), warn = FALSE),
    collapse = "\n"
  )
  js <- paste(
    readLines(
      file.path(root, "www", "builder-spatial-canvas.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(server, "builder_spatial_canvas_cache_sources(", fixed = TRUE)
  expect_match(js, "image.sourceKey || image.uri", fixed = TRUE)
})

test_that("Reset image waits for server-authoritative controls", {
  root <- testthat::test_path("..", "..", "inst", "builder")
  js <- paste(
    readLines(
      file.path(root, "www", "builder-spatial-canvas.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_false(grepl("function resetImageControls", js, fixed = TRUE))
  expect_match(js, 'closest("#enhance-reset_align")', fixed = TRUE)
  expect_match(js, "pendingAuthoritativeControls", fixed = TRUE)
  expect_match(js, "flushControlCommit();", fixed = TRUE)
})

test_that("programmatic Image settings restore cannot commit partial values", {
  root <- testthat::test_path("..", "..", "inst", "builder")
  server <- paste(
    readLines(file.path(root, "spatial_alignment_server.R"), warn = FALSE),
    collapse = "\n"
  )
  start <- regexpr(
    "commit_alignment_controls <- function()",
    server,
    fixed = TRUE
  )[[1L]]
  block <- substr(server, start, start + 2200L)
  expected_start <- regexpr(
    "expected <- shiny::isolate(expected_controls())",
    block,
    fixed = TRUE
  )[[1L]]
  block <- substr(block, expected_start, nchar(block))
  expect_match(
    block,
    paste0(
      "if \\(\\s*!is.null\\(expected\\)\\s*\\) \\{",
      "[\\s\\S]*?expected_controls\\(NULL\\)",
      "[\\s\\S]*?!isTRUE\\(all.equal\\("
    ),
    perl = TRUE
  )
  expect_match(
    server,
    "settled_parameters <- shiny::debounce(parameters, millis = 50)",
    fixed = TRUE
  )
  expect_match(
    gsub("[[:space:]]+", " ", server),
    paste0(
      "parameters <- shiny::reactive({ browser <- browser_control_state() ",
      "owner <- active_control_owner()"
    ),
    fixed = TRUE
  )
})

test_that("control commits carry a complete spatial owner and flush on switch", {
  root <- testthat::test_path("..", "..", "inst", "builder")
  js <- paste(
    readLines(
      file.path(root, "www", "builder-spatial-canvas.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  server <- paste(
    readLines(file.path(root, "spatial_alignment_server.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(js, '"builder_spatial_alignment_controls"', fixed = TRUE)
  expect_match(js, "snapshotIdentity: scene.snapshotIdentity", fixed = TRUE)
  expect_match(js, "image: scene.activeImage", fixed = TRUE)
  expect_match(js, "window.setTimeout(flushControlCommit, 50)", fixed = TRUE)
  expect_match(server, "event_control_owner <- function(event)", fixed = TRUE)
  expect_match(
    server,
    "apply_browser_controls <- function(event)",
    fixed = TRUE
  )
  expect_match(server, "control_event_sequences", fixed = TRUE)
})

test_that("preview settlement cannot overwrite live control values", {
  root <- testthat::test_path("..", "..", "inst", "builder")
  server <- paste(
    readLines(file.path(root, "spatial_alignment_server.R"), warn = FALSE),
    collapse = "\n"
  )
  start <- regexpr(
    "shiny::observeEvent(alignment_preview()",
    server,
    fixed = TRUE
  )[[1L]]
  block <- substr(server, start, start + 1600L)

  expect_match(
    block,
    "update_position_steps(draft(), preview$bounds)",
    fixed = TRUE
  )
  expect_false(grepl("update_controls(", block, fixed = TRUE))
})

test_that("image replacement clears stale decoded pixels", {
  root <- testthat::test_path("..", "..", "inst", "builder")
  js <- paste(
    readLines(
      file.path(root, "www", "builder-spatial-canvas.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    js,
    "if (state.imageKey !== key) state.image = null;",
    fixed = TRUE
  )
  expect_match(js, "state.imageKey = null;", fixed = TRUE)
})

test_that("Spatial preview worker contract does not include coordinate drafts", {
  root <- testthat::test_path("..", "..", "inst", "builder")
  server <- paste(
    readLines(file.path(root, "spatial_alignment_server.R"), warn = FALSE),
    collapse = "\n"
  )
  preview <- paste(
    readLines(file.path(root, "preview.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_false(grepl("coordinate_preview_transforms", server, fixed = TRUE))
  expect_false(grepl(
    "coordinate_transforms = coordinate_draft",
    server,
    fixed = TRUE
  ))
  expect_match(preview, "raw sampled spatial coordinates", fixed = TRUE)
})
