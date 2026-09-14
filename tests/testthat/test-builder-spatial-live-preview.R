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
  expect_match(js, '"builder_spatial_roi_select"', fixed = TRUE)
  expect_match(js, "Shiny.setInputValue", fixed = TRUE)
  expect_match(js, "finishInteraction", fixed = TRUE)
  expect_match(js, '"builder_spatial_coordinate_draft"', fixed = TRUE)
  expect_match(js, "snapshotIdentity", fixed = TRUE)
  expect_match(js, "coordinateSequence", fixed = TRUE)
  expect_match(js, "generation: scene.generation", fixed = TRUE)
  expect_false(grepl(">= state.resetToken", js, fixed = TRUE))
  expect_false(grepl("Plotly", js, fixed = TRUE))
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
    "viewportLayout(bounds, angle",
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

test_that("Reset image updates browser-local controls immediately", {
  root <- testthat::test_path("..", "..", "inst", "builder")
  js <- paste(
    readLines(
      file.path(root, "www", "builder-spatial-canvas.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(js, "resetImageControls", fixed = TRUE)
  expect_match(js, 'closest("#enhance-reset_align")', fixed = TRUE)
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
      "if \\(\\s*!isTRUE\\(all.equal\\([\\s\\S]*?\\)\\)\\s*\\) \\{",
      "\\s*return\\(invisible\\(FALSE\\)\\)\\s*\\}",
      "\\s*expected_controls\\(NULL\\)"
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
    "parameters <- shiny::reactive({ current_draft <- shiny::isolate(draft())",
    fixed = TRUE
  )
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
