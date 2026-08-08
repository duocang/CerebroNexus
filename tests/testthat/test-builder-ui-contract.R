builder_asset_path <- function(...) {
  relative <- file.path(...)
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    relative
  )
  if (!file.exists(path)) {
    path <- system.file(
      file.path("builder", relative),
      package = "CerebroNexus"
    )
  }
  path
}

builder_asset_text <- function(...) {
  paste(readLines(builder_asset_path(...), warn = FALSE), collapse = "\n")
}

test_that("builder UI copy is English-only", {
  files <- c(
    "app.R",
    "adapters.R",
    "io.R",
    "spatial.R",
    "inspect.R",
    "preview.R",
    "extras.R",
    "analysis.R",
    "worker.R",
    "session.R"
  )
  text <- paste(
    vapply(files, function(file) builder_asset_text(file), character(1)),
    collapse = "\n"
  )

  expect_false(
    grepl("[\u3400-\u9fff]", text),
    info = "User-visible builder assets must not contain Han-script UI copy"
  )
})

test_that("builder uses a responsive card grid", {
  css <- builder_asset_text("www", "builder.css")

  expect_match(css, "#workbench \\{")
  expect_match(css, "\\.builder-stage,")
  expect_match(css, "grid-template-columns: 19.5rem")
  expect_match(css, "@media \\(max-width: 42.5rem\\)")
})

test_that("dataset uploads use an amber trigger without replacing the native chooser", {
  app <- builder_asset_text("app.R")
  css <- builder_asset_text("www", "builder.css")

  expect_match(app, 'tags$input(', fixed = TRUE)
  expect_match(app, 'type = "file"', fixed = TRUE)
  expect_match(app, 'id = "dataset_files"', fixed = TRUE)
  expect_match(app, 'class = "dataset-file-button"', fixed = TRUE)
  expect_match(app, '"Add datasets…"', fixed = TRUE)
  expect_false(grepl('fileInput(', app, fixed = TRUE))
  expect_false(grepl('dataset_files.*class = "btn', app, perl = TRUE))
  expect_match(css, ".dataset-file-button", fixed = TRUE)
  expect_match(css, "background: var(--c-amber)", fixed = TRUE)
})

test_that("dataset removal remains a soft red text action", {
  rail <- builder_asset_text("ui", "dataset_rail.R")
  css <- builder_asset_text("www", "builder.css")

  expect_match(rail, '"Remove"', fixed = TRUE)
  expect_false(grepl('`data-icon` = "remove"', rail, fixed = TRUE))
  expect_match(css, ".ds-del", fixed = TRUE)
  expect_match(css, "border: 1px solid transparent", fixed = TRUE)
  expect_match(css, "background: var(--c-error-50)", fixed = TRUE)
})

test_that("example cards keep a consistent compact height", {
  css <- builder_asset_text("www", "builder.css")

  expect_match(css, ".example-btn", fixed = TRUE)
  expect_match(css, ".ex-inner", fixed = TRUE)
  expect_match(css, "height: 4.65rem", fixed = TRUE)
})

test_that("builder interaction states follow the amber theme", {
  css <- builder_asset_text("www", "builder.css")

  expect_match(css, "border-color: var(--c-amber) !important", fixed = TRUE)
  expect_match(
    css,
    "box-shadow: 0 0 0 3px var(--c-amber-50) !important",
    fixed = TRUE
  )
  expect_match(css, ".selectize-dropdown .option.active", fixed = TRUE)
  expect_match(css, "background: var(--c-amber-50) !important", fixed = TRUE)
  expect_match(css, "color: var(--c-text) !important", fixed = TRUE)
  expect_match(css, ".example-btn:hover .ex-inner", fixed = TRUE)
  expect_match(css, "border-color: var(--c-amber-100)", fixed = TRUE)
  expect_false(grepl(
    ".selectize-dropdown .active { background: var(--c-blue-50)",
    css,
    fixed = TRUE
  ))
  expect_false(grepl(
    ".example-btn:hover .ex-inner {\n  border-color: var(--c-blue-100)",
    css,
    fixed = TRUE
  ))
})

test_that("builder client avoids layout-measurement animation loops", {
  js <- builder_asset_text("www", "builder.js")

  expect_false(grepl("getBoundingClientRect", js, fixed = TRUE))
  expect_false(grepl("ResizeObserver", js, fixed = TRUE))
  expect_false(grepl("requestAnimationFrame", js, fixed = TRUE))
  expect_false(grepl("offsetWidth", js, fixed = TRUE))
  expect_false(grepl("#detail", js, fixed = TRUE))
  expect_false(grepl("swatch_change", js, fixed = TRUE))
})

test_that("builder keeps primary actions in flow and exposes a narrow manager", {
  css <- builder_asset_text("www", "builder.css")

  expect_match(css, "\\.actionbar \\{\\s*position: static", perl = TRUE)
  expect_false(grepl(
    "\\.actionbar \\{[^}]*position: fixed",
    css,
    perl = TRUE
  ))
  expect_match(css, "@media (max-width: 56.25rem)", fixed = TRUE)
  expect_match(css, "@media (max-width: 42.5rem)", fixed = TRUE)
  expect_match(css, ".rail-summary", fixed = TRUE)
  expect_match(css, ".rail.is-manager-open", fixed = TRUE)
  expect_match(css, ".builder-stage[aria-current=\"stage\"]", fixed = TRUE)
})

test_that("builder client owns accessible dialog and live-state semantics", {
  js <- builder_asset_text("www", "builder.js")

  expect_match(js, 'setAttribute("role", "dialog")', fixed = TRUE)
  expect_match(js, 'setAttribute("aria-modal", "true")', fixed = TRUE)
  expect_match(js, 'event.key === "Escape"', fixed = TRUE)
  expect_match(js, "focusableElements", fixed = TRUE)
  expect_match(js, "restoreFocus", fixed = TRUE)
  expect_false(grepl("window.confirm", js, fixed = TRUE))
  expect_match(
    js,
    'querySelector(\'[aria-modal="true"]:not([hidden])\')',
    fixed = TRUE
  )
  expect_false(grepl("closeFileDialog", js, fixed = TRUE))

  expect_match(js, "builder-live-status", fixed = TRUE)
  expect_match(js, 'setAttribute("aria-live", "polite")', fixed = TRUE)
  expect_match(js, "scheduleStatusAnnouncement", fixed = TRUE)
  expect_match(js, 'setAttribute("aria-current", "stage")', fixed = TRUE)
  expect_match(js, "window.__builderPrimaryActionVisible", fixed = TRUE)
  expect_match(js, "window.__builderMotionDuration", fixed = TRUE)
  expect_match(
    js,
    'matchMedia("(prefers-reduced-motion: reduce)")',
    fixed = TRUE
  )
})

test_that("builder previews and colour controls have text equivalents", {
  js <- builder_asset_text("www", "builder.js")

  expect_match(js, "plotly_afterplot", fixed = TRUE)
  expect_match(js, "builder-preview-summary", fixed = TRUE)
  expect_match(js, 'input[type="color"]', fixed = TRUE)
  expect_match(js, "colourLabel", fixed = TRUE)
  expect_match(js, "value.toUpperCase()", fixed = TRUE)
  expect_false(grepl("app_dir", js, fixed = TRUE))
  expect_false(grepl("publish", js, ignore.case = TRUE))
})

test_that("Inspect shows compact detected-content tags instead of audit output", {
  inspect <- builder_asset_text("ui", "inspect_stage.R")
  review <- builder_asset_text("ui", "review_stage.R")
  stats <- builder_asset_text("www", "stats.js")

  expect_match(inspect, "builder-content-tags", fixed = TRUE)
  expect_false(grepl("Verified profile", inspect, fixed = TRUE))
  expect_false(grepl("View all detected content", inspect, fixed = TRUE))
  expect_false(grepl("QC preview uses", inspect, fixed = TRUE))
  expect_false(grepl("projection(s) and", inspect, fixed = TRUE))
  expect_false(grepl("group_distribution$bucket", inspect, fixed = TRUE))
  expect_false(grepl("builder-stats-chart", inspect, fixed = TRUE))
  expect_match(review, "expected-versus-verified", fixed = TRUE)
  expect_match(review, "Expected after build", fixed = TRUE)
  expect_match(review, "Verified after build", fixed = TRUE)
  expect_match(stats, 'setAttribute("aria-label"', fixed = TRUE)
  expect_match(stats, "prefers-reduced-motion", fixed = TRUE)
  expect_false(grepl("fetch|XMLHttpRequest|https?://", stats))
})

test_that("first-run guidance and recovery stay focused", {
  app <- builder_asset_text("app.R")
  js <- builder_asset_text("www", "builder.js")
  css <- builder_asset_text("www", "builder.css")
  rail <- builder_asset_text("ui", "dataset_rail.R")
  status <- builder_asset_text("ui", "build_status.R")

  expect_false(grepl('source("help.R"', app, fixed = TRUE))
  expect_match(app, "builder-first-run", fixed = TRUE)
  expect_match(status, "builder-recovery-action", fixed = TRUE)
  expect_match(js, "localStorage", fixed = TRUE)
  expect_false(grepl("builder-glossary", js, fixed = TRUE))
  expect_match(js, "isTextInput", fixed = TRUE)
  for (shortcut in c("Enter", "KeyO", "KeyZ")) {
    expect_match(js, shortcut, fixed = TRUE)
  }
  expect_match(css, ".builder-first-run", fixed = TRUE)
  expect_false(grepl("builder-glossary", css, fixed = TRUE))

  production <- paste(app, js, css, rail, sep = "\n")
  for (removed in c(
    "Start from a template",
    "Standard PBMC",
    "Spatial transcriptome",
    "Minimal CRB",
    "builder-template",
    "apply_template",
    "Batch edit selected datasets",
    "Use current group",
    "Use current projection",
    "Use current storage",
    "builder-batch",
    "batch_settings",
    "builder_batch_command",
    "builder_apply_batch_settings"
  )) {
    expect_false(grepl(removed, production, fixed = TRUE), info = removed)
  }
})

test_that("icons ship inline with no network request", {
  js <- builder_asset_text("www", "icons.js")
  app <- builder_asset_text("app.R")

  expect_match(js, "svg", fixed = TRUE)
  expect_false(grepl("http", js, ignore.case = TRUE))
  expect_false(grepl("fetch|XMLHttpRequest", js))
  expect_match(app, 'tags$script(src = paste0("icons.js"', fixed = TRUE)
})

test_that("top-level states use icons plus text and a bounded motion contract", {
  css <- builder_asset_text("www", "builder.css")
  icons <- builder_asset_text("www", "icons.js")

  for (state in c("empty", "loading", "blocking", "failure", "recovery")) {
    expect_match(css, paste0(".is-", state), fixed = TRUE)
  }
  for (icon in c("check", "warning", "block", "spinner", "question")) {
    expect_match(icons, paste0('"', icon, '"'), fixed = TRUE)
  }
  expect_match(css, "@media (prefers-reduced-motion: reduce)", fixed = TRUE)
})

test_that("pipeline visualization reflects the current step", {
  js <- builder_asset_text("www", "builder.js")
  status <- builder_asset_text("ui", "build_status.R")

  expect_match(js, "pipeline", fixed = TRUE)
  expect_match(js, 'setAttribute("aria-current"', fixed = TRUE)
  expect_match(status, "builder-build-pipeline", fixed = TRUE)
  expect_match(status, "Queued", fixed = TRUE)
  expect_match(status, "Building", fixed = TRUE)
  expect_match(status, "Complete", fixed = TRUE)
  expect_false(grepl('pipeline_state.*"verify"', status))
  expect_match(
    builder_asset_text("app.R"),
    'builder_build_pipeline_ui("building")',
    fixed = TRUE
  )
  expect_false(grepl(
    'build_phase %in% c("running", "cancelling")',
    builder_asset_text("app.R"),
    fixed = TRUE
  ))
})

test_that("dense stages default to plain summaries and bounded details", {
  enhance <- builder_asset_text("ui", "enhance_stage.R")
  review <- builder_asset_text("ui", "review_stage.R")
  rail <- builder_asset_text("ui", "dataset_rail.R")
  css <- builder_asset_text("www", "builder.css")

  expect_match(enhance, "Optional analyses", fixed = TRUE)
  expect_match(enhance, "What this changes", fixed = TRUE)
  expect_match(review, "Technical plan details", fixed = TRUE)
  expect_match(review, "Detailed manifest", fixed = TRUE)
  expect_match(review, "builder_review_bounded_lines", fixed = TRUE)
  for (label in c("Move up", "Move down", "Remove")) {
    expect_match(rail, label, fixed = TRUE)
  }
  expect_match(rail, "ds-actions", fixed = TRUE)
  expect_match(css, ".ds-actions", fixed = TRUE)
  expect_false(grepl("builder-select-initial", rail, fixed = TRUE))
  expect_false(grepl("builder-duplicate", rail, fixed = TRUE))
})

test_that("result actions remain native keyboard controls", {
  status <- builder_asset_text("ui", "build_status.R")

  for (id in c("open_app", "reveal_folder", "copy_path", "copy_report")) {
    expect_match(
      status,
      paste0('actionButton(\n        "', id, '"'),
      fixed = TRUE
    )
  }
  expect_false(grepl('tabindex = "-1"', status, fixed = TRUE))
})

test_that("spatial translation does not invalidate image encoding", {
  lines <- readLines(builder_asset_path("app.R"), warn = FALSE)
  start <- grep("encoded_image <- reactive", lines, fixed = TRUE)
  finish <- grep("image_base_bounds <- reactive", lines, fixed = TRUE)
  encoded <- paste(lines[start:(finish - 1L)], collapse = "\n")

  expect_false(grepl("img_dx|img_dy|img_scale", encoded))
  expect_match(encoded, "img_rotate", fixed = TRUE)
})
