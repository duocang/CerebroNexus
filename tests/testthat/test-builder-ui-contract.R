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

builder_stylesheet_files <- c(
  "builder.tokens.css",
  "builder.base.css",
  "builder.layout.css",
  "builder.components.css",
  "builder.features.css"
)

builder_stylesheet_text <- function(
  files = builder_stylesheet_files
) {
  paste(
    vapply(
      files,
      function(file) builder_asset_text("www", file),
      character(1)
    ),
    collapse = "\n"
  )
}

test_that("builder UI includes the Bootstrap dependency required by modals", {
  app_env <- new.env(parent = globalenv())
  withr::local_dir(dirname(builder_asset_path("app.R")))
  sys.source("app.R", envir = app_env)

  dependency_names <- vapply(
    htmltools::renderTags(app_env$ui)$dependencies,
    function(dependency) dependency$name,
    character(1),
    USE.NAMES = FALSE
  )

  expect_contains(dependency_names, "bootstrap")
})

test_that("builder stylesheets load in explicit responsibility order", {
  app_env <- new.env(parent = globalenv())
  withr::local_dir(dirname(builder_asset_path("app.R")))
  sys.source("app.R", envir = app_env)

  head_html <- htmltools::renderTags(app_env$ui)$head
  stylesheet_tags <- regmatches(
    head_html,
    gregexpr(
      '<link\\b[^>]*\\brel="stylesheet"[^>]*>',
      head_html,
      perl = TRUE
    )
  )[[1L]]
  stylesheet_hrefs <- sub(
    '.*\\bhref="([^"]+)".*',
    "\\1",
    stylesheet_tags,
    perl = TRUE
  )
  stylesheet_files <- sub("\\?.*$", "", stylesheet_hrefs)
  builder_hrefs <- stylesheet_hrefs[
    grepl(
      "^builder\\.(?:tokens|base|layout|components|features|css)",
      stylesheet_files
    )
  ]
  builder_files <- sub("\\?.*$", "", builder_hrefs)
  expected_hrefs <- vapply(
    builder_stylesheet_files,
    function(file) {
      paste0(file, app_env$asset_stamp(file.path("www", file)))
    },
    character(1),
    USE.NAMES = FALSE
  )

  expect_identical(builder_files, builder_stylesheet_files)
  expect_identical(builder_hrefs, expected_hrefs)
  expect_false("builder.css" %in% stylesheet_files)
})

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
  css <- builder_stylesheet_text()
  tokens_css <- builder_stylesheet_text("builder.tokens.css")
  layout_css <- builder_stylesheet_text("builder.layout.css")
  components_css <- builder_stylesheet_text("builder.components.css")
  features_css <- builder_stylesheet_text("builder.features.css")

  expect_match(css, "#workbench \\{")
  expect_match(css, "\\.builder-stage,")
  expect_match(
    layout_css,
    "grid-template-columns: var(--builder-rail-width) minmax(0, 1fr)",
    fixed = TRUE
  )
  expect_match(layout_css, "@media (max-width: 58rem)", fixed = TRUE)
  expect_match(layout_css, "@media (max-width: 40rem)", fixed = TRUE)
  expect_match(components_css, "@media (max-width: 40rem)", fixed = TRUE)
  expect_match(features_css, "@media (max-width: 40rem)", fixed = TRUE)
  expect_match(tokens_css, "--builder-measure-copy: 48rem", fixed = TRUE)
  expect_match(tokens_css, "--builder-measure-form: 56rem", fixed = TRUE)
  expect_match(tokens_css, "--builder-measure-data: none", fixed = TRUE)
  expect_match(
    layout_css,
    "#workbench {[^}]*gap: var\\(--builder-section-gap\\);",
    perl = TRUE
  )
  expect_match(
    layout_css,
    "\\.builder-stage,[^}]*padding: var\\(--builder-stage-padding\\);",
    perl = TRUE
  )
  expect_match(
    components_css,
    "\\.builder-card \\{[^}]*padding: var\\(--builder-stage-padding\\);",
    perl = TRUE
  )
  expect_match(
    features_css,
    paste0(
      "\\.stage-intro,\\s*\\.builder-measure-copy \\{[^}]*",
      "width: 100%;[^}]*max-width: var\\(--builder-measure-copy\\);"
    ),
    perl = TRUE
  )
  expect_match(
    components_css,
    paste0(
      "\\.builder-form-grid,\\s*\\.builder-advanced-grid \\{[^}]*",
      "width: 100%;[^}]*",
      "max-width: var\\(--builder-measure-form\\);"
    ),
    perl = TRUE
  )
  expect_match(
    components_css,
    paste0(
      "\\.builder-measure-form \\{[^}]*width: 100%;[^}]*",
      "max-width: var\\(--builder-measure-form\\);"
    ),
    perl = TRUE
  )
  expect_match(
    features_css,
    paste0(
      "\\.viewer-analysis-results,\\s*\\.viewer-specialized-content,",
      "\\s*\\.builder-preview-grid,\\s*\\.builder-measure-data \\{",
      "[^}]*width: 100%;[^}]*max-width: var\\(--builder-measure-data\\);"
    ),
    perl = TRUE
  )
})

test_that("builder exposes one compact responsive component system", {
  css <- builder_stylesheet_text()
  layout_css <- builder_stylesheet_text("builder.layout.css")

  for (token in c(
    "--c-surface-muted",
    "--c-border-strong",
    "--c-text-muted",
    "--c-text-subtle",
    "--space-5",
    "--radius-sm",
    "--radius-md",
    "--radius-lg",
    "--radius-pill",
    "--shadow-sm",
    "--shadow-md",
    "--shadow-dialog",
    "--duration-fast",
    "--duration-normal"
  )) {
    expect_match(css, token, fixed = TRUE)
  }
  for (component in c(
    ".builder-shell",
    ".builder-content",
    ".builder-card",
    ".builder-section",
    ".builder-subcard",
    ".builder-form-grid",
    ".builder-field",
    ".builder-action-row",
    ".builder-disclosure",
    ".builder-file-picker",
    ".builder-file-list",
    ".builder-file-item",
    ".builder-empty-state",
    ".builder-status",
    ".builder-controls-grid",
    ".builder-preview-grid",
    ".builder-dialog",
    ".builder-dialog-actions"
  )) {
    expect_match(css, component, fixed = TRUE)
  }
  expect_match(css, "--duration-fast: 120ms", fixed = TRUE)
  expect_match(css, "--duration-normal: 180ms", fixed = TRUE)
  expect_match(css, "--builder-text-muted: var(--c-text-2)", fixed = TRUE)
  expect_match(css, "--builder-text-subtle: var(--c-text-3)", fixed = TRUE)
  expect_match(css, "--builder-page-gutter: 26px", fixed = TRUE)
  expect_false(grepl(
    "\\.shell,\\s*\\.builder-shell \\{[^}]*82\\.5rem",
    css,
    perl = TRUE
  ))
  expect_false(grepl(
    "\\.actionbar \\{[^}]*82\\.5rem",
    css,
    perl = TRUE
  ))
  expect_false(grepl(
    "\\.actionbar \\.inner \\{[^}]*82\\.5rem",
    css,
    perl = TRUE
  ))
  expect_match(
    css,
    paste0(
      "\\.shell,\\s*\\.builder-shell \\{[^}]*width: 100%;",
      "[^}]*max-width: none;[^}]*padding: var\\(--space-6\\) ",
      "var\\(--builder-page-gutter\\);"
    ),
    perl = TRUE
  )
  expect_false(grepl(".actionbar", layout_css, fixed = TRUE))
  expect_match(css, "@media (max-width: 68.75rem)", fixed = TRUE)
  expect_match(css, "@media (max-width: 58rem)", fixed = TRUE)
  expect_match(css, "@media (max-width: 40rem)", fixed = TRUE)
  tablet_start <- regexpr(
    "@media (max-width: 68.75rem)",
    layout_css,
    fixed = TRUE
  )[[1L]]
  manager_start <- regexpr(
    "@media (max-width: 58rem)",
    layout_css,
    fixed = TRUE
  )[[1L]]
  mobile_start <- regexpr(
    "@media (max-width: 40rem)",
    layout_css,
    fixed = TRUE
  )[[1L]]
  expect_gt(tablet_start, 0L)
  expect_gt(manager_start, tablet_start)
  expect_gt(mobile_start, manager_start)
  tablet_css <- substr(layout_css, tablet_start, manager_start - 1L)
  expect_match(css, "@media (prefers-reduced-motion: reduce)", fixed = TRUE)
  expect_match(css, ".builder-file-picker--sidebar", fixed = TRUE)
  expect_match(css, ".builder-file-picker--content", fixed = TRUE)
  expect_match(css, "width: fit-content", fixed = TRUE)
  expect_false(grepl(
    "\\.builder-action-row\\s*\\{[^}]*margin-top\\s*:\\s*auto",
    css,
    perl = TRUE
  ))
  expect_match(css, "--duration-base: var(--duration-normal)", fixed = TRUE)
  expect_match(css, ".btn-quiet:hover", fixed = TRUE)
  expect_false(grepl(
    "\\.btn-quiet:hover\\s*\\{[^}]*var\\(--c-error\\)",
    css,
    perl = TRUE
  ))
})

test_that("enhancement groups and previews use one quiet density system", {
  css <- builder_stylesheet_text("builder.features.css")

  expect_match(
    css,
    paste0(
      "\\.enhance-group \\{[^}]*padding: 0;[^}]*",
      "border: 0;[^}]*background: transparent;"
    ),
    perl = TRUE
  )
  expect_match(
    css,
    paste0(
      "\\.enhance-group \\+ \\.enhance-group \\{[^}]*",
      "margin-top: var\\(--space-6\\);[^}]*",
      "padding-top: var\\(--space-6\\);[^}]*",
      "border-top: 1px solid var\\(--c-border\\);"
    ),
    perl = TRUE
  )
  expect_false(grepl(
    "enhance-attachment-block + .enhance-attachment-block",
    css,
    fixed = TRUE
  ))
  expect_false(grepl("enhance-attachment-block--spatial", css, fixed = TRUE))
  expect_false(grepl("aspect-ratio: 1;", css, fixed = TRUE))
  expect_match(css, "--spatial-preview-aspect", fixed = TRUE)
  js <- builder_asset_text("www", "builder.js")
  canvas_js <- builder_asset_text("www", "builder-spatial-canvas.js")
  enhance_ui <- builder_asset_text("ui", "enhance_stage.R")
  alignment_server <- builder_asset_text("spatial_alignment_server.R")
  expect_false(grepl('ns("coordinate_scale")', enhance_ui, fixed = TRUE))
  expect_false(grepl(
    "enhance-coordinate_scale",
    alignment_server,
    fixed = TRUE
  ))
  expect_false(grepl("enhance-coordinate_scale", js, fixed = TRUE))
  for (client_renderer in c(
    "spatialDraftRevision",
    "scheduleSpatialAlignmentDraft",
    "applyContinuousCoordinateTransform",
    "scheduleContinuousCoordinateRotation",
    "applyContinuousSpatialAlignment",
    "scheduleContinuousSpatialAlignment"
  )) {
    expect_false(grepl(client_renderer, js, fixed = TRUE))
  }
  expect_match(
    alignment_server,
    "transforms[[section]] <- spec",
    fixed = TRUE
  )
  expect_match(alignment_server, "current_record <- draft_record", fixed = TRUE)
  expect_match(
    alignment_server,
    "session$sendCustomMessage(\"builder_spatial_canvas_scene\"",
    fixed = TRUE
  )
  expect_false(grepl("syncSpatialPreviewAspect", js, fixed = TRUE))
  expect_false(grepl("syncSpatialWorkbench", js, fixed = TRUE))
  expect_match(js, 'sidebar.addEventListener("wheel"', fixed = TRUE)
  expect_match(js, "event.preventDefault()", fixed = TRUE)
  expect_match(js, "window.scrollBy(", fixed = TRUE)
  expect_match(js, "{ passive: false }", fixed = TRUE)
  expect_match(canvas_js, "ResizeObserver", fixed = TRUE)
  expect_match(canvas_js, "builder_spatial_canvas_scene", fixed = TRUE)
  expect_match(canvas_js, "requestAnimationFrame", fixed = TRUE)
  expect_match(
    canvas_js,
    'event.target.closest(".shiny-input-container, .form-group")',
    fixed = TRUE
  )
  expect_match(
    canvas_js,
    'document.addEventListener("pointermove", handleControlEvent, true)',
    fixed = TRUE
  )
  rotate_at <- regexpr("context.rotate(-radians)", canvas_js, fixed = TRUE)[[
    1L
  ]]
  scale_at <- regexpr("context.scale(", canvas_js, fixed = TRUE)[[1L]]
  expect_gt(rotate_at, 0L)
  expect_gt(scale_at, rotate_at)
  expect_false(grepl("plotly_afterplot", js, fixed = TRUE))
  expect_match(
    css,
    "100dvh - var\\(--builder-spatial-viewport-offset\\) -[^;]*var\\(--builder-workflow-progress-height",
    perl = TRUE
  )
  expect_match(
    css,
    "\\.spatial-alignment-sidebar \\{[^}]*overflow-y: auto;",
    perl = TRUE
  )
  expect_match(
    css,
    paste0(
      "\\.spatial-image-options\\[open\\] \\{[^}]*",
      "max-height: none;[^}]*",
      "overflow: visible;"
    ),
    perl = TRUE
  )
  expect_match(
    css,
    paste0(
      "\\.spatial-alignment-figure \\{[^}]*",
      "border: 1px solid var\\(--c-border\\);[^}]*",
      "border-radius: var\\(--radius-md\\);[^}]*",
      "background: var\\(--c-surface\\);"
    ),
    perl = TRUE
  )
  expect_match(
    css,
    paste0(
      "\\.spatial-alignment-legend-wrap \\{[^}]*",
      "margin: 0;[^}]*padding: var\\(--space-3\\);[^}]*",
      "background: var\\(--c-surface\\);"
    ),
    perl = TRUE
  )
  expect_false(grepl("background: #fbfaf8", css, fixed = TRUE))
  expect_match(
    css,
    paste0(
      "\\.spatial-alignment-title \\{[^}]*",
      "font-size: 1rem;[^}]*font-weight: 700;"
    ),
    perl = TRUE
  )
  expect_match(
    css,
    paste0(
      "\\.builder-preview-grid,\\s*",
      "\\.spatial-alignment-plots,\\s*",
      "\\.spatial-alignment-controls \\{[^}]*",
      "grid-template-columns: minmax\\(0, 1fr\\)"
    ),
    perl = TRUE
  )
})

test_that("Builder framed surfaces use one explicit title hierarchy", {
  components <- builder_stylesheet_text("builder.components.css")
  features <- builder_stylesheet_text("builder.features.css")

  expect_match(
    components,
    paste0(
      "\\.builder-stage-section > h3 \\{[^}]*margin: 0 0 var\\(--space-3\\);[^}]*",
      "font-size: 1rem;[^}]*line-height: 1.4;"
    ),
    perl = TRUE
  )
  expect_match(
    features,
    paste0(
      "\\.builder-viewer-content-head h4 \\{[^}]*margin: 0;[^}]*",
      "font-size: \\.9375rem;"
    ),
    perl = TRUE
  )
  expect_match(
    features,
    "\\.enhance-group > h4 \\{[^}]*font-size: \\.9375rem;",
    perl = TRUE
  )
  expect_match(
    components,
    "\\.notice > h4 \\{[^}]*font-size: \\.9375rem;",
    perl = TRUE
  )
  expect_match(
    features,
    "\\.builder-detected-content h4 \\{[^}]*font-size: \\.9375rem;",
    perl = TRUE
  )
  expect_match(
    features,
    "\\.spatial-alignment-title \\{[^}]*font-size: 1rem;",
    perl = TRUE
  )
  expect_match(
    features,
    paste0(
      "\\.enhance-attachment-block--tables > h5,\\s*",
      "\\.spatial-alignment-legend-wrap h5 \\{[^}]*",
      "font-size: \\.875rem;"
    ),
    perl = TRUE
  )
})

test_that("Builder has no duplicate dataset context banner", {
  js <- builder_asset_text("www", "builder.js")
  review <- paste(
    readLines(builder_profile_inst_path("builder", "server", "review.R")),
    collapse = "\n"
  )

  expect_false(grepl("__builderFocusDatasetContext", js, fixed = TRUE))
  expect_false(grepl('uiOutput("dataset_context")', review, fixed = TRUE))
  expect_false(grepl('output[["dataset_context"]]', review, fixed = TRUE))
})

test_that("Viewer Group catalog interactions use stable names and client search", {
  js <- builder_asset_text("www", "builder.js")
  css <- builder_stylesheet_text()
  core <- builder_core_stage_source_text()

  expect_match(core, "viewer-group-include", fixed = TRUE)
  expect_match(core, "viewer-group-default", fixed = TRUE)
  expect_match(core, "Find metadata", fixed = TRUE)
  expect_match(js, "filterViewerGroups", fixed = TRUE)
  expect_match(js, "updateViewerGroupSelection", fixed = TRUE)
  expect_match(js, "viewer-group-focus", fixed = TRUE)
  expect_match(css, ".builder-viewer-content", fixed = TRUE)
  expect_match(css, ".viewer-group-row", fixed = TRUE)
})

test_that("dataset uploads use an amber trigger without replacing the native chooser", {
  app <- builder_asset_text("app.R")
  css <- builder_stylesheet_text()

  expect_match(app, 'tags$input(', fixed = TRUE)
  expect_match(app, 'type = "file"', fixed = TRUE)
  expect_match(app, 'id = "dataset_files"', fixed = TRUE)
  expect_match(
    app,
    'class = "dataset-file-control builder-file-picker builder-file-picker--sidebar"',
    fixed = TRUE
  )
  expect_match(app, "builder-file-input", fixed = TRUE)
  expect_match(app, "builder-file-trigger", fixed = TRUE)
  expect_match(
    app,
    'class = "dataset-file-button builder-file-trigger"',
    fixed = TRUE
  )
  expect_match(app, '"Add datasets…"', fixed = TRUE)
  expect_false(grepl('fileInput(', app, fixed = TRUE))
  expect_false(grepl('dataset_files.*class = "btn', app, perl = TRUE))
  expect_match(css, ".dataset-file-button", fixed = TRUE)
  expect_match(css, "background: var(--builder-action)", fixed = TRUE)
})

test_that("supplementary tables use an amber native multi-file chooser", {
  stage <- builder_asset_text("ui", "enhance_stage.R")
  css <- builder_stylesheet_text()

  expect_match(
    stage,
    'class = "shiny-input-file enhance-table-file-input builder-file-input"',
    fixed = TRUE
  )
  expect_match(stage, 'multiple = "multiple"', fixed = TRUE)
  expect_match(stage, 'accept = ".csv,.tsv,.txt"', fixed = TRUE)
  expect_match(
    stage,
    'class = "enhance-table-file-control builder-file-picker builder-file-picker--content"',
    fixed = TRUE
  )
  expect_match(
    stage,
    'class = "enhance-table-file-button builder-file-trigger"',
    fixed = TRUE
  )
  expect_match(stage, '`tabindex` = "0"', fixed = TRUE)
  expect_match(stage, 'role = "button"', fixed = TRUE)
  expect_match(stage, 'span("+ Add tables…")', fixed = TRUE)
  expect_match(
    app <- builder_app_source_text(),
    'span("Table name")',
    fixed = TRUE
  )
  expect_match(app, 'class = "enhance-table-display-name"', fixed = TRUE)
  expect_false(grepl('textInput(ns("table_path")', stage, fixed = TRUE))
  expect_false(grepl('textInput(ns("table_name")', stage, fixed = TRUE))
  expect_false(grepl('actionButton(ns("add_table")', stage, fixed = TRUE))
  expect_match(css, ".enhance-table-file-button", fixed = TRUE)
  expect_match(css, ".enhance-table-file-control:hover", fixed = TRUE)
  expect_match(css, ".enhance-table-file-input:focus-visible", fixed = TRUE)
})

test_that("all local attachments use accessible native file inputs", {
  app <- builder_asset_text("app.R")
  stage <- builder_asset_text("ui", "enhance_stage.R")
  js <- builder_asset_text("www", "builder.js")
  css <- builder_stylesheet_text()

  expect_match(app, 'accept = paste(', fixed = TRUE)
  expect_match(app, 'multiple = "multiple"', fixed = TRUE)
  expect_match(stage, 'accept = ".csv,.tsv,.txt"', fixed = TRUE)
  expect_match(stage, 'multiple = "multiple"', fixed = TRUE)
  expect_match(stage, 'accept = ".png,.jpg,.jpeg"', fixed = TRUE)
  expect_match(
    stage,
    'class = "enhance-tissue-file-control builder-file-picker builder-file-picker--compact"',
    fixed = TRUE
  )
  expect_match(js, 'closest(".builder-file-trigger")', fixed = TRUE)
  expect_match(js, 'event.key === "Enter" || event.key === " "', fixed = TRUE)
  expect_match(
    css,
    ".builder-file-input:focus-visible + .builder-file-trigger",
    fixed = TRUE
  )
  expect_false(grepl(
    "\\.builder-file-input\\s*\\{[^}]*display\\s*:\\s*none",
    css,
    perl = TRUE
  ))
  expect_false(grepl(
    "CSV / TSV path|PNG / JPEG path|fakepath",
    paste(app, stage)
  ))
  expect_false(grepl("browser_panel|browse_open|browse_dir", paste(app, stage)))
})

test_that("dataset removal remains a soft red text action", {
  rail <- builder_asset_text("ui", "dataset_rail.R")
  css <- builder_stylesheet_text()

  expect_match(rail, '"Remove"', fixed = TRUE)
  expect_false(grepl('`data-icon` = "remove"', rail, fixed = TRUE))
  expect_match(css, ".ds-del", fixed = TRUE)
  expect_match(css, "border: 1px solid transparent", fixed = TRUE)
  expect_match(css, "background: var(--c-error-50)", fixed = TRUE)
})

test_that("example cards keep a consistent compact height", {
  css <- builder_stylesheet_text()

  expect_match(css, ".example-btn", fixed = TRUE)
  expect_match(css, ".ex-inner", fixed = TRUE)
  expect_match(css, "min-height: 4.65rem", fixed = TRUE)
})

test_that("builder interaction states follow the amber theme", {
  css <- builder_stylesheet_text()
  tokens <- builder_stylesheet_text("builder.tokens.css")

  expect_match(
    css,
    "border-color: var(--builder-action) !important",
    fixed = TRUE
  )
  expect_match(
    css,
    "box-shadow: 0 0 0 3px var(--c-amber-50) !important",
    fixed = TRUE
  )
  expect_match(css, ".selectize-dropdown .option.active", fixed = TRUE)
  expect_match(css, "background: var(--c-amber-50) !important", fixed = TRUE)
  expect_match(css, "color: var(--c-text) !important", fixed = TRUE)
  expect_match(css, ".example-btn:hover .ex-inner", fixed = TRUE)
  expect_match(css, "border-color: var(--builder-hover-border)", fixed = TRUE)
  expect_match(css, "background: var(--builder-hover-bg)", fixed = TRUE)
  expect_match(
    tokens,
    "--builder-hover-border: var(--c-amber-300)",
    fixed = TRUE
  )
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

test_that("group colors use native bounded controls without projection palettes", {
  core <- builder_core_stage_source_text()
  js <- builder_asset_text("www", "builder.js")
  css <- builder_stylesheet_text()

  expect_match(core, 'type = "color"', fixed = TRUE)
  expect_match(core, 'paste("Show all", model$total, "colors")', fixed = TRUE)
  expect_match(core, '"Show fewer"', fixed = TRUE)
  expect_match(core, '"Find a group value"', fixed = TRUE)
  expect_match(js, "group-color-toggle", fixed = TRUE)
  expect_match(js, "group-color-search", fixed = TRUE)
  expect_match(css, ".group-color-grid", fixed = TRUE)
  expect_match(css, ".group-color-item:hover", fixed = TRUE)
  expect_match(css, ".group-color-input:focus-visible", fixed = TRUE)
  expect_false(grepl("umap_palette|pca_palette|tsne_palette", core))
})

test_that("Viewer content cards preserve disclosure state across Shiny redraws", {
  core <- builder_core_stage_source_text()
  js <- builder_asset_text("www", "builder.js")

  expect_false(grepl('open = "open"', core, fixed = TRUE))
  expect_match(core, "data-disclosure-key", fixed = TRUE)
  expect_match(js, "viewerDisclosureState", fixed = TRUE)
  expect_match(js, "setupPersistentDisclosures", fixed = TRUE)
  expect_match(js, 'details[data-disclosure-key]', fixed = TRUE)
  expect_match(js, "setupViewerContentAccordions", fixed = TRUE)
  expect_match(js, 'addEventListener("toggle"', fixed = TRUE)
  expect_match(js, "sibling.open = false", fixed = TRUE)
})

test_that("Spatial alignment distinguishes FOVs from section-owned images", {
  enhance <- builder_asset_text("ui", "enhance_stage.R")
  js <- builder_asset_text("www", "builder.js")

  expect_match(enhance, "Requires spatial FOVs and coordinates.", fixed = TRUE)
  expect_match(
    enhance,
    "Named images remain separate within each FOV.",
    fixed = TRUE
  )
  expect_match(enhance, '"Spatial capture (FOV)"', fixed = TRUE)
  expect_match(js, "builder_spatial_section_state", fixed = TRUE)
  expect_false(grepl(
    "One saved image per tissue section.",
    enhance,
    fixed = TRUE
  ))
})

test_that("Review layout and staged workflow contracts stay user-facing", {
  css <- builder_stylesheet_text()
  app <- builder_app_source_text()
  workflow_ui <- builder_asset_text("ui", "workflow.R")
  review_server <- builder_asset_text("server", "review.R")
  inspect_ui <- builder_asset_text("ui", "inspect_stage.R")
  core_ui <- builder_core_stage_source_text()
  enhance_ui <- builder_asset_text("ui", "enhance_stage.R")

  expect_match(css, ".review-app-grid", fixed = TRUE)
  expect_match(css, ".review-page-tags", fixed = TRUE)
  expect_match(css, ".review-output-fields", fixed = TRUE)
  expect_match(css, ".review-needs-attention", fixed = TRUE)
  expect_match(
    css,
    ".review-app-options > summary::before",
    fixed = TRUE
  )
  expect_match(
    css,
    ".review-app-options[open] > summary::before",
    fixed = TRUE
  )
  expect_match(
    css,
    "grid-template-columns: repeat(2, minmax(0, 1fr))",
    fixed = TRUE
  )
  expect_match(css, "overflow-wrap: anywhere", fixed = TRUE)
  expect_match(app, 'uiOutput("workflow_progress")', fixed = TRUE)
  expect_false(grepl('uiOutput("actionbar")', app, fixed = TRUE))
  expect_false(grepl('uiOutput("result_card")', app, fixed = TRUE))
  expect_match(workflow_ui, 'class = "builder-workflow-progress"', fixed = TRUE)
  expect_match(workflow_ui, '`aria-label` = "Builder progress"', fixed = TRUE)
  expect_match(workflow_ui, "builder_stage_footer_ui(", fixed = TRUE)
  expect_match(workflow_ui, '"continue_to_review"', fixed = TRUE)
  expect_match(
    review_server,
    'class = "builder-stage builder-stage-shell builder-stage-configure"',
    fixed = TRUE
  )
  expect_match(review_server, '"Choose data to include"', fixed = TRUE)
  expect_match(review_server, '" dataset"', fixed = TRUE)
  expect_match(review_server, '" ready"', fixed = TRUE)
  expect_false(grepl("Ready to review", review_server, fixed = TRUE))
  for (source in list(inspect_ui, core_ui, enhance_ui)) {
    expect_match(source, "builder-stage-section", fixed = TRUE)
    expect_false(grepl("builder-card builder-section", source, fixed = TRUE))
  }
  expect_false(grepl(
    'actionButton(\n      "build",\n      "Build",',
    app,
    fixed = TRUE
  ))
})

test_that("Build dialogs are reserved for real output conflicts", {
  js <- builder_asset_text("www", "builder.js")

  expect_match(js, "showBuildDialog", fixed = TRUE)
  expect_match(js, 'dialog.setAttribute("role", "dialog")', fixed = TRUE)
  expect_match(js, 'dialog.setAttribute("aria-modal", "true")', fixed = TRUE)
  expect_match(
    js,
    'dialog.setAttribute("aria-labelledby", title.id)',
    fixed = TRUE
  )
  expect_match(js, "trapDialogKeydown", fixed = TRUE)
  expect_match(js, "restoreFocus(dialog)", fixed = TRUE)
  expect_match(js, "Choose another folder", fixed = TRUE)
  expect_match(js, "Replace existing files", fixed = TRUE)
  expect_false(grepl("Ready to build all datasets?", js, fixed = TRUE))
  expect_false(grepl('message.type === "datasets"', js, fixed = TRUE))
  expect_false(grepl('action: "continue"', js, fixed = TRUE))
  expect_false(grepl("review_required|attention_required", js))
  expect_match(js, 'message.action === "close"', fixed = TRUE)
  expect_false(grepl("window.confirm", js, fixed = TRUE))
})

test_that("active Build states disable every stage action", {
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  model <- list(output = list(private_app = FALSE, crb_count = 2L))
  active <- htmltools::renderTags(app_env$builder_build_stage_controls_ui(
    "/tmp/output",
    controls_disabled = TRUE
  ))$html
  idle <- htmltools::renderTags(app_env$builder_build_stage_controls_ui(
    "/tmp/output",
    controls_disabled = FALSE
  ))$html

  for (id in "choose_output_folder") {
    disabled_button <- paste0(
      '<button(?=[^>]*id="',
      id,
      '")(?=[^>]* disabled)[^>]*>'
    )
    expect_match(
      active,
      disabled_button,
      perl = TRUE,
      info = id
    )
    expect_false(
      grepl(
        disabled_button,
        idle,
        perl = TRUE
      ),
      info = id
    )
  }

  active_footer <- htmltools::renderTags(
    app_env$builder_build_stage_footer_ui(
      list(state = "ready", can_build = FALSE),
      controls_disabled = TRUE
    )
  )$html
  idle_footer <- htmltools::renderTags(
    app_env$builder_build_stage_footer_ui(
      list(state = "ready", can_build = TRUE),
      controls_disabled = FALSE
    )
  )$html
  expect_match(active_footer, "builder-stage-footer", fixed = TRUE)
  for (id in c("back_to_review", "build")) {
    expect_match(
      active_footer,
      paste0('<button(?=[^>]*id="', id, '")(?=[^>]* disabled)[^>]*>'),
      perl = TRUE
    )
    expect_false(grepl(
      paste0('<button(?=[^>]*id="', id, '")(?=[^>]* disabled)[^>]*>'),
      idle_footer,
      perl = TRUE
    ))
  }

  blocked_build <- htmltools::renderTags(
    app_env$builder_build_stage_status_ui(list(
      state = "ready",
      can_build = FALSE
    ))
  )$html
  ready_build <- htmltools::renderTags(
    app_env$builder_build_stage_status_ui(list(
      state = "ready",
      can_build = TRUE
    ))
  )$html
  expect_match(
    blocked_build,
    '<button(?=[^>]*id="build")(?=[^>]* disabled)[^>]*>',
    perl = TRUE
  )
  expect_false(grepl(
    '<button(?=[^>]*id="build")(?=[^>]* disabled)[^>]*>',
    ready_build,
    perl = TRUE
  ))
  shell <- htmltools::renderTags(
    app_env$builder_build_workbench_ui(model)
  )$html
  expect_identical(
    lengths(regmatches(
      shell,
      gregexpr('id="build-stage-status"', shell, fixed = TRUE)
    )),
    1L
  )
  expect_match(shell, 'role="status"', fixed = TRUE)
  expect_match(shell, 'aria-live="polite"', fixed = TRUE)
  expect_match(shell, 'aria-atomic="true"', fixed = TRUE)
  expect_match(shell, 'id="build_stage_status_content"', fixed = TRUE)
  expect_match(shell, 'id="build_stage_footer"', fixed = TRUE)
  expect_false(grepl('id="build-stage-status"', ready_build, fixed = TRUE))
})

test_that("builder client removes per-dataset compact review navigation", {
  js <- builder_asset_text("www", "builder.js")

  for (legacy in c(
    "setupCompactReviewNavigator",
    "measureCompactReviewNavigator",
    "review_compact_dataset",
    "dataset-compact-segment",
    ".rail-review-status"
  )) {
    expect_false(grepl(legacy, js, fixed = TRUE), info = legacy)
  }
  expect_match(js, ".rail-readiness-status", fixed = TRUE)
})

test_that("builder styles remove per-dataset review selectors", {
  css <- builder_asset_text("www", "builder.features.css")

  for (legacy in c(
    "dataset-compact-review",
    "dataset-compact-track",
    "dataset-review-progress",
    "dataset-review-footer",
    ".rail-review-status",
    ".reviewed",
    ".reviewing",
    ".not-reviewed"
  )) {
    expect_false(grepl(legacy, css, fixed = TRUE), info = legacy)
  }
})

test_that("dataset rail uses one selected state and one soft-danger action", {
  layout <- builder_asset_text("www", "builder.layout.css")
  components <- builder_asset_text("www", "builder.components.css")
  features <- builder_asset_text("www", "builder.features.css")
  js <- builder_asset_text("www", "builder.js")

  expect_match(
    layout,
    "\\.ds\\.is-active\\s*\\{[^}]*background:\\s*var\\(--builder-selection-bg\\)",
    perl = TRUE
  )
  expect_match(
    layout,
    ".ds-picker .ds:not(.is-active):has(.ds-pick:hover)",
    fixed = TRUE
  )
  expect_false(grepl(
    "\\.ds--import\\.is-active\\s*\\{",
    components,
    perl = TRUE
  ))
  expect_match(components, ".btn-remove-soft", fixed = TRUE)
  expect_false(grepl(".btn-remove-soft", features, fixed = TRUE))
  expect_match(js, 'confirm.className = "btn btn-remove-soft"', fixed = TRUE)
  expect_match(
    js,
    'row.className = "ds ds--import is-active is-importing ds--client-upload"',
    fixed = TRUE
  )
})

test_that("Builder action orange follows the contrast-safe Nexus brand shade", {
  tokens <- builder_asset_text("www", "builder.tokens.css")
  logo <- builder_asset_text("..", "viewer", "www", "cerebronexus.svg")

  expect_match(tokens, "--builder-action: #c9500b;", fixed = TRUE)
  expect_match(
    tokens,
    "--builder-selection-bg: var(--builder-action);",
    fixed = TRUE
  )
  expect_match(logo, 'fill="#ea6a0f"', fixed = TRUE)
})

test_that("stage focus targets the active heading", {
  js <- builder_asset_text("www", "builder.js")

  expect_match(
    js,
    'addCustomMessageHandler("builder_focus_stage"',
    fixed = TRUE
  )
  expect_match(js, 'var heading = stage.querySelector("h2")', fixed = TRUE)
  expect_match(js, "heading.focus({ preventScroll: true })", fixed = TRUE)
})

test_that("per-dataset compact review server inputs are removed", {
  app <- builder_app_source_text()

  expect_false(grepl("review_compact_", app, fixed = TRUE))
  expect_match(app, 'id = "workbench",', fixed = TRUE)
  expect_match(app, 'tabindex = "-1"', fixed = TRUE)
})

test_that("builder keeps primary actions in flow and exposes a narrow manager", {
  css <- builder_stylesheet_text()
  js <- builder_asset_text("www", "builder.js")

  expect_false(grepl(
    ".actionbar",
    builder_asset_text("www", "builder.layout.css"),
    fixed = TRUE
  ))
  expect_match(css, "@media (max-width: 68.75rem)", fixed = TRUE)
  expect_match(css, "@media (max-width: 58rem)", fixed = TRUE)
  expect_match(css, "@media (max-width: 40rem)", fixed = TRUE)
  expect_match(css, ".rail-summary", fixed = TRUE)
  expect_match(css, ".rail.is-manager-open", fixed = TRUE)
  expect_match(css, ".builder-stage[aria-current=\"stage\"]", fixed = TRUE)
  expect_match(
    js,
    'window.matchMedia("(max-width: 58rem)")',
    fixed = TRUE
  )
})

test_that("staged workflow owns responsive styles and one safe focus handler", {
  layout <- builder_asset_text("www", "builder.layout.css")
  components <- builder_asset_text("www", "builder.components.css")
  features <- builder_asset_text("www", "builder.features.css")
  js <- builder_asset_text("www", "builder.js")
  server <- paste(
    builder_asset_text("server", "workflow.R"),
    builder_asset_text("server", "review.R")
  )

  expect_false(grepl(".actionbar", layout, fixed = TRUE))
  expect_match(components, ".builder-workflow-progress", fixed = TRUE)
  expect_match(components, ".is-unavailable", fixed = TRUE)
  expect_match(
    components,
    ".builder-workflow-stage-link:focus-visible",
    fixed = TRUE
  )
  expect_false(grepl('content: "Current "', components, fixed = TRUE))
  expect_false(grepl(".builder-stage-actions", components, fixed = TRUE))
  expect_match(components, ".builder-stage-footer", fixed = TRUE)
  for (stage in c("configure", "review", "build")) {
    expect_match(features, paste0(".builder-stage-", stage), fixed = TRUE)
  }
  expect_length(
    gregexpr(
      'addCustomMessageHandler("builder_focus_stage"',
      js,
      fixed = TRUE
    )[[1]],
    1L
  )
  expect_false(grepl("builder_focus_review", js, fixed = TRUE))
  expect_false(grepl("builder_focus_build", js, fixed = TRUE))
  expect_match(components, ".builder-stage-shell", fixed = TRUE)
  expect_match(components, ".builder-stage-summary", fixed = TRUE)
  expect_match(components, ".builder-stage-section", fixed = TRUE)
  expect_match(
    components,
    "\\.builder-stage-section \\{[^}]*border: 1px solid var\\(--c-border\\);",
    perl = TRUE
  )
  expect_match(
    components,
    "\\.builder-stage-section \\{[^}]*background: var\\(--c-surface\\);",
    perl = TRUE
  )
  expect_match(
    components,
    "\\.builder-stage-section \\{[^}]*box-shadow: var\\(--shadow-1\\);",
    perl = TRUE
  )
  expect_match(components, ".builder-stage-footer", fixed = TRUE)
  expect_match(components, ".builder-stage-footer-actions", fixed = TRUE)
  expect_match(
    components,
    ".builder-stage-footer-actions .btn { width: 100%; }",
    fixed = TRUE
  )
  expect_match(
    js,
    'behavior: reducedMotion.matches ? "auto" : "smooth"',
    fixed = TRUE
  )
  expect_match(js, 'document.querySelector(".topbar")', fixed = TRUE)
  expect_match(js, "heading.style.scrollMarginTop", fixed = TRUE)
  expect_match(
    js,
    'scheduleStatusAnnouncement("Opened " + id + " step.")',
    fixed = TRUE
  )
  expect_false(grepl("builder_focus_review|builder_focus_build", server))
  expect_match(
    server,
    '"builder_focus_stage", list(id = "configure")',
    fixed = TRUE
  )
  expect_match(
    server,
    '"builder_focus_stage", list(id = "review")',
    fixed = TRUE
  )
  expect_match(
    server,
    '"builder_focus_stage", list(id = "build")',
    fixed = TRUE
  )
  expect_match(components, "transition-duration: 0s !important", fixed = TRUE)
  expect_match(server, "is.list(workflow()$confirmation)", fixed = TRUE)
  expect_match(server, '`data-workflow-stage` = "upload"', fixed = TRUE)
  browser <- paste(
    readLines(testthat::test_path(
      "test-builder-staged-workflow-browser.R"
    )),
    collapse = "\n"
  )
  browser_helper <- paste(
    readLines(testthat::test_path(
      "helper-builder-browser-contract.R"
    )),
    collapse = "\n"
  )
  expect_match(browser, "builder-loading-stage", fixed = TRUE)
  expect_match(browser, "builder_expect_clean_browser_logs(app)", fixed = TRUE)
  expect_match(browser_helper, "app$get_logs()", fixed = TRUE)
})

test_that("builder client owns accessible dialog and live-state semantics", {
  js <- builder_asset_text("www", "builder.js")

  expect_match(js, 'setAttribute("role", "dialog")', fixed = TRUE)
  expect_match(js, 'setAttribute("aria-modal", "true")', fixed = TRUE)
  expect_match(js, 'event.key === "Escape"', fixed = TRUE)
  expect_match(js, "focusableElements", fixed = TRUE)
  expect_match(js, "restoreFocus", fixed = TRUE)
  expect_false(grepl("window.confirm", js, fixed = TRUE))
  expect_match(js, 'Array.from(', fixed = TRUE)
  expect_match(
    js,
    'document.querySelectorAll(\'[aria-modal="true"]\')',
    fixed = TRUE
  )
  expect_match(js, 'return !dialog.closest("[hidden]");', fixed = TRUE)
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

test_that("builder UI keeps semantic colors and states on the token system", {
  tokens <- builder_asset_text("www", "builder.tokens.css")
  css <- builder_stylesheet_text()
  features <- builder_asset_text("www", "builder.features.css")
  components <- builder_asset_text("www", "builder.components.css")
  stats <- builder_asset_text("www", "stats.js")

  for (tone in c(
    "metadata",
    "projection",
    "spatial",
    "analysis",
    "trajectory",
    "immune",
    "extra"
  )) {
    expect_match(tokens, paste0("--builder-tag-", tone, "-bg"), fixed = TRUE)
    expect_match(
      features,
      paste0(".builder-content-tag.is-", tone),
      fixed = TRUE
    )
    expect_match(
      features,
      paste0("background: var(--builder-tag-", tone, "-bg)"),
      fixed = TRUE
    )
  }
  for (tone in c(
    "core",
    "analysis",
    "spatial",
    "trajectory",
    "immune",
    "extra"
  )) {
    expect_match(features, paste0(".review-page-tag.is-", tone), fixed = TRUE)
  }
  expect_false(grepl(".review-page-tag.tone-", features, fixed = TRUE))
  expect_match(features, ".review-auth-dependency", fixed = TRUE)
  expect_match(features, ".review-auth-summary", fixed = TRUE)
  expect_false(grepl("font-weight: 650|font-weight: 750", css))
  expect_match(components, "@keyframes spin", fixed = TRUE)
  expect_false(grepl(
    "@keyframes builder-import-spin",
    components,
    fixed = TRUE
  ))
  expect_match(stats, "--builder-action", fixed = TRUE)
  expect_false(grepl("#C94718", stats, fixed = TRUE))
})

test_that("Builder keeps login account editing outside the redrawn workbench", {
  app <- builder_app_source_text()

  expect_match(app, 'builder_auth_dialog_ui()', fixed = TRUE)
  expect_match(app, 'uiOutput("workflow_progress")', fixed = TRUE)
  expect_false(grepl('uiOutput("actionbar")', app, fixed = TRUE))
  expect_false(grepl('uiOutput("result_card")', app, fixed = TRUE))
  expect_match(app, 'id = "builder-live-status"', fixed = TRUE)
  expect_lt(
    regexpr('uiOutput("workflow_progress")', app, fixed = TRUE),
    regexpr('builder_auth_dialog_ui()', app, fixed = TRUE)
  )
  expect_lt(
    regexpr('builder_auth_dialog_ui()', app, fixed = TRUE),
    regexpr('id = "builder-live-status"', app, fixed = TRUE)
  )

  local({
    builder_repo_source(file.path("ui", "review_stage.R"))
    html <- builder_stage_html(builder_auth_dialog_ui())
    expect_match(html, 'id="builder-auth-dialog"', fixed = TRUE)
    expect_match(html, 'role="dialog"', fixed = TRUE)
    expect_match(html, 'aria-modal="true"', fixed = TRUE)
    expect_match(html, 'aria-labelledby="builder-auth-title"', fixed = TRUE)
    expect_match(html, 'tabindex="-1"', fixed = TRUE)
    expect_match(html, "Login accounts", fixed = TRUE)
    expect_match(html, 'id="builder-auth-error"', fixed = TRUE)
    expect_match(html, 'data-auth-rows="true"', fixed = TRUE)
    expect_identical(
      lengths(gregexpr('id="builder-auth-dialog"', html, fixed = TRUE)),
      1L
    )
    expect_false(grepl("<input", html, fixed = TRUE))
  })
})

test_that("Builder auth client keeps validation generic and restores redraw focus", {
  js <- builder_asset_text("www", "builder.js")

  expect_match(js, '"Login accounts could not be saved."', fixed = TRUE)
  expect_false(grepl("error.textContent = (message", js, fixed = TRUE))
  expect_match(js, "builder-auth-open", fixed = TRUE)
  expect_match(js, "__builderRestoreFocusFallback", fixed = TRUE)
  expect_match(
    js,
    "authEditor.committed = authCopy(authEditor.snapshot);",
    fixed = TRUE
  )
  expect_match(js, "function clearAuthError()", fixed = TRUE)
  expect_match(js, "if (authEditor.saving) return;", fixed = TRUE)
  expect_match(js, "setAuthSaving(nonce);", fixed = TRUE)
  expect_match(js, "setAuthSaving(false);", fixed = TRUE)
  expect_match(js, "message.nonce !== authEditor.saving", fixed = TRUE)
  expect_match(js, 'dialog.querySelectorAll("input, button")', fixed = TRUE)
  expect_match(js, 'send("builder_auth_accounts", null);', fixed = TRUE)
})

test_that("Builder auth modal uses motion and colour tokens accessibly", {
  css <- builder_stylesheet_text()

  expect_match(css, ".builder-auth-backdrop.is-visible", fixed = TRUE)
  expect_match(css, "var(--duration-base)", fixed = TRUE)
  expect_match(css, ".builder-auth-error", fixed = TRUE)
  expect_match(css, "color: var(--c-error)", fixed = TRUE)
  expect_match(
    css,
    ".builder-auth-backdrop,\n  .builder-auth-dialog { transition: none; }",
    fixed = TRUE
  )
})

test_that("static example cards show loading without hiding the directory", {
  js <- builder_asset_text("www", "builder.js")
  css <- builder_stylesheet_text()

  expect_match(js, "function messageValues(value)", fixed = TRUE)
  expect_match(
    js,
    "return Array.isArray(value) ? value : [value];",
    fixed = TRUE
  )
  expect_match(
    js,
    "new Set(messageValues(message && message.ids))",
    fixed = TRUE
  )
  expect_match(js, "message.loading", fixed = TRUE)
  expect_match(js, 'classList.toggle("is-loading"', fixed = TRUE)
  expect_match(js, 'textContent = isLoading ? "Loading…"', fixed = TRUE)
  expect_match(js, 'classList.toggle("is-taken"', fixed = TRUE)
  expect_match(css, ".example-btn.is-loading", fixed = TRUE)
  expect_match(css, ".example-btn.is-taken { display: none; }", fixed = TRUE)
  expect_match(js, "function registerExampleMessageHandler", fixed = TRUE)
  expect_match(
    js,
    'document.addEventListener("shiny:connected", function ()',
    fixed = TRUE
  )
  expect_match(js, "registerExampleMessageHandler();", fixed = TRUE)
})

test_that("Builder JavaScript waits for a usable document before initialization", {
  js <- builder_asset_text("www", "builder.js")

  expect_match(js, "function initializeBuilder()", fixed = TRUE)
  expect_match(
    js,
    'document.addEventListener("DOMContentLoaded", initializeBuilder',
    fixed = TRUE
  )
  expect_match(js, 'document.readyState === "loading"', fixed = TRUE)
  expect_match(js, "new MutationObserver(enhanceDynamicContent)", fixed = TRUE)
})

test_that("explicitly declared creatable selects use the integrated menu enhancer", {
  js <- builder_asset_text("www", "builder.js")
  base_css <- builder_asset_text("www", "builder.base.css")
  component_css <- builder_asset_text("www", "builder.components.css")

  expect_match(js, "function setupCreatableSelects()", fixed = TRUE)
  expect_match(js, "function setupCreatableSelect(root)", fixed = TRUE)
  expect_match(
    js,
    "function normalizeCreatableSelectValue(value)",
    fixed = TRUE
  )
  expect_match(js, "[data-builder-creatable-select='true']", fixed = TRUE)
  expect_match(js, "builder-creatable-select-row", fixed = TRUE)
  expect_match(
    js,
    '"Use " + maximumLength + " characters or fewer."',
    fixed = TRUE
  )
  expect_match(js, "selectize.addOption", fixed = TRUE)
  expect_match(js, "selectize.settings.valueField", fixed = TRUE)
  expect_match(js, "selectize.settings.labelField", fixed = TRUE)
  expect_match(js, "selectize.setValue", fixed = TRUE)
  expect_match(js, "selectize.close()", fixed = TRUE)
  expect_match(js, "selectize.ignoreFocus = true", fixed = TRUE)
  expect_match(js, "selectize.isFocused = true", fixed = TRUE)
  expect_match(
    base_css,
    ".selectize-control.single .selectize-input",
    fixed = TRUE
  )
  expect_match(component_css, ".builder-creatable-select-row", fixed = TRUE)
  expect_match(component_css, ".builder-creatable-select-add", fixed = TRUE)
})

test_that("builder previews and colour controls have text equivalents", {
  js <- builder_asset_text("www", "builder.js")
  canvas_js <- builder_asset_text("www", "builder-spatial-canvas.js")

  expect_match(canvas_js, "builder-spatial-canvas-summary", fixed = TRUE)
  expect_match(canvas_js, "aria-label", fixed = TRUE)
  expect_match(canvas_js, "setAttribute(\"aria-label\"", fixed = TRUE)
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
  css <- builder_stylesheet_text()
  for (tone in c("analysis", "trajectory", "immune", "extra")) {
    expect_match(css, paste0(".builder-content-tag.is-", tone), fixed = TRUE)
  }
  expect_false(grepl(".builder-content-tag.is-optional", css, fixed = TRUE))
  expect_false(grepl("View all detected content", inspect, fixed = TRUE))
  expect_false(grepl("QC preview uses", inspect, fixed = TRUE))
  expect_false(grepl("projection(s) and", inspect, fixed = TRUE))
  expect_false(grepl("group_distribution$bucket", inspect, fixed = TRUE))
  expect_false(grepl("builder-stats-chart", inspect, fixed = TRUE))
  expect_match(review, "Content available from the CRBs", fixed = TRUE)
  expect_false(grepl("expected-versus-verified", review, fixed = TRUE))
  expect_false(grepl("Expected after build", review, fixed = TRUE))
  expect_false(grepl("Verified after build", review, fixed = TRUE))
  expect_match(stats, 'setAttribute("aria-label"', fixed = TRUE)
  expect_match(stats, "prefers-reduced-motion", fixed = TRUE)
  expect_false(grepl("fetch|XMLHttpRequest|https?://", stats))
})

test_that("first-run guidance and recovery stay focused", {
  app <- builder_asset_text("app.R")
  js <- builder_asset_text("www", "builder.js")
  css <- builder_stylesheet_text()
  rail <- builder_asset_text("ui", "dataset_rail.R")
  status <- builder_asset_text("ui", "build_status.R")

  expect_false(grepl('source("help.R"', app, fixed = TRUE))
  expect_match(app, "builder-first-run", fixed = TRUE)
  expect_match(app, 'class = "btn builder-first-run-dismiss"', fixed = TRUE)
  expect_false(grepl(
    'class = "btn btn-primary builder-first-run-dismiss"',
    app,
    fixed = TRUE
  ))
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
  css <- builder_stylesheet_text()
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
  build_server <- builder_asset_text("server", "build.R")

  expect_match(js, "pipeline", fixed = TRUE)
  expect_match(js, 'setAttribute("aria-current"', fixed = TRUE)
  expect_match(status, "builder-build-pipeline", fixed = TRUE)
  expect_match(status, "Queued", fixed = TRUE)
  expect_match(status, "Building", fixed = TRUE)
  expect_match(status, "Complete", fixed = TRUE)
  expect_false(grepl('pipeline_state.*"verify"', status))
  expect_match(
    status,
    'builder_build_pipeline_ui("building")',
    fixed = TRUE
  )
  expect_match(
    build_server,
    'identical(workflow()$stage, "build")',
    fixed = TRUE
  )
  expect_false(grepl(
    'builder_build_pipeline_ui("building")',
    build_server,
    fixed = TRUE
  ))
  expect_false(grepl(
    'build_phase %in% c("running", "cancelling")',
    builder_app_source_text(),
    fixed = TRUE
  ))
})

test_that("dense stages default to plain summaries and bounded details", {
  enhance <- builder_asset_text("ui", "enhance_stage.R")
  review <- builder_asset_text("ui", "review_stage.R")
  rail <- builder_asset_text("ui", "dataset_rail.R")
  css <- builder_stylesheet_text()

  expect_match(enhance, "Optional analyses", fixed = TRUE)
  expect_false(grepl("What this changes", enhance, fixed = TRUE))
  expect_match(review, "Frozen plan revision", fixed = TRUE)
  expect_identical(
    lengths(regmatches(
      review,
      gregexpr("builder_review_stage_ui <- function", review, fixed = TRUE)
    )),
    1L
  )
  expect_false(grepl("builder_review_bounded_lines", review, fixed = TRUE))
  for (label in c("Move up", "Move down", "Remove")) {
    expect_match(rail, label, fixed = TRUE)
  }
  expect_match(rail, "ds-actions", fixed = TRUE)
  expect_match(css, ".ds-actions", fixed = TRUE)
  expect_false(grepl("builder-select-initial", rail, fixed = TRUE))
  expect_false(grepl("builder-duplicate", rail, fixed = TRUE))
})

test_that("active build mutation controls and conflict replies are nonce-bound", {
  js <- builder_asset_text("www", "builder.js")
  build <- builder_asset_text("server", "build.R")

  expect_match(js, "builder_dataset_mutation_lock", fixed = TRUE)
  expect_match(js, "applyDatasetMutationLock", fixed = TRUE)
  expect_match(js, 'nonce: message.nonce', fixed = TRUE)
  expect_match(build, "next_conflict_nonce", fixed = TRUE)
  expect_match(build, "!identical(event$nonce, flow$nonce)", fixed = TRUE)
  expect_match(build, '!identical(flow$stage, "conflict")', fixed = TRUE)
})

test_that("Enhance analyses use amber selectable cards with quiet info controls", {
  css <- builder_stylesheet_text()

  expect_match(css, ".enhance-module-select", fixed = TRUE)
  expect_match(css, ".enhance-module-title", fixed = TRUE)
  expect_match(css, ".enhance-info-button", fixed = TRUE)
  expect_match(css, ".enhance-module:hover", fixed = TRUE)
  expect_match(css, "background: var(--c-amber-50)", fixed = TRUE)
  expect_match(
    css,
    "\\.enhance-module:hover \\{[\\s\\S]*?transform: none;",
    perl = TRUE
  )
  expect_match(css, ".enhance-module:focus-within", fixed = TRUE)
  expect_match(
    css,
    "\\.enhance-module:focus-within \\{[\\s\\S]*?background: var\\(--c-surface\\);",
    perl = TRUE
  )
  expect_false(grepl(
    ".enhance-module:hover,\n.enhance-module:focus-within",
    css,
    fixed = TRUE
  ))
  expect_match(css, ".enhance-module:has(input:checked)", fixed = TRUE)
  expect_match(css, "background: var(--builder-selection-bg);", fixed = TRUE)
  expect_match(css, ".enhance-module.is-blocked", fixed = TRUE)
  expect_match(
    css,
    '.enhance-module:has(input:checked):hover',
    fixed = TRUE
  )
  expect_match(css, "@media (prefers-reduced-motion: reduce)", fixed = TRUE)
  expect_match(
    css,
    ".enhance-module { transform: none; }",
    fixed = TRUE
  )
})

test_that("Enhance info opens a transient accessible facts dialog", {
  js <- builder_asset_text("www", "builder.js")

  expect_match(js, '.enhance-info-button', fixed = TRUE)
  expect_match(js, "showAnalysisInfo", fixed = TRUE)
  expect_match(js, "builder-analysis-info-backdrop", fixed = TRUE)
  expect_match(js, "builder-analysis-info-dialog", fixed = TRUE)
  expect_match(js, 'event.preventDefault()', fixed = TRUE)
  expect_match(js, 'event.stopPropagation()', fixed = TRUE)
  expect_match(js, "prepareDialog(dialog, infoButton, close)", fixed = TRUE)
  expect_match(js, "restoreFocus(dialog)", fixed = TRUE)
  expect_match(js, "updateDialogLock()", fixed = TRUE)
  expect_match(
    js,
    'dialog.setAttribute("aria-labelledby", title.id)',
    fixed = TRUE
  )
  expect_match(
    js,
    'dialog.setAttribute("aria-describedby", description.id)',
    fixed = TRUE
  )
  expect_match(js, 'if (event.target === backdrop) close()', fixed = TRUE)
  expect_match(
    js,
    'event.target.closest(".enhance-module-checkbox")',
    fixed = TRUE
  )
  expect_match(js, 'event.key === "Enter"', fixed = TRUE)
  expect_match(js, "enhanceCheckbox.click()", fixed = TRUE)
  for (label in c(
    "Available in",
    "Typical time",
    "Requires",
    "Network",
    "If already present",
    "If skipped"
  )) {
    expect_match(js, label, fixed = TRUE)
  }
  expect_match(
    js,
    "title.textContent = infoButton.dataset.title",
    fixed = TRUE
  )
  expect_match(
    js,
    "description.textContent = infoButton.dataset.description",
    fixed = TRUE
  )
  expect_match(js, "value.textContent = fact.value", fixed = TRUE)

  css <- builder_stylesheet_text()
  expect_match(css, ".builder-analysis-info-backdrop", fixed = TRUE)
  expect_match(css, ".builder-analysis-info-dialog", fixed = TRUE)
  expect_match(css, ".builder-analysis-info-facts", fixed = TRUE)
  expect_match(
    css,
    "grid-template-columns: repeat(2, minmax(0, 1fr))",
    fixed = TRUE
  )
  expect_match(css, ".is-wide", fixed = TRUE)
})

test_that("result actions remain native keyboard controls", {
  status <- builder_asset_text("ui", "build_status.R")

  for (id in c("open_app", "reveal_folder", "copy_path", "copy_report")) {
    expect_match(
      status,
      paste0('actionButton\\(\\s*"', id, '"'),
      perl = TRUE
    )
  }
  expect_false(grepl('tabindex = "-1"', status, fixed = TRUE))
})

test_that("spatial live controls do not publish or encode images", {
  lines <- readLines(
    builder_asset_path("spatial_alignment_server.R"),
    warn = FALSE
  )
  start <- grep("  mark_unsaved <- function", lines, fixed = TRUE)
  finish <- grep("  save_current <- function", lines, fixed = TRUE)
  mark_unsaved <- paste(lines[start:(finish - 1L)], collapse = "\n")

  expect_match(mark_unsaved, "commit_section", fixed = TRUE)
  expect_false(grepl("builder_encode_image", mark_unsaved, fixed = TRUE))
  expect_false(grepl("finalize_current_record", mark_unsaved, fixed = TRUE))
  expect_false(grepl("publish_canvas", mark_unsaved, fixed = TRUE))
})

test_that("transient layers expose state-bearing motion lifecycle", {
  js <- builder_asset_text("www", "builder.js")
  layout_css <- builder_stylesheet_text("builder.layout.css")
  components_css <- builder_stylesheet_text("builder.components.css")

  for (function_name in c(
    "showTransientLayer",
    "removeTransientLayer",
    "updateMotionDuration"
  )) {
    expect_match(js, paste0("function ", function_name, "("), fixed = TRUE)
  }
  expect_match(js, 'getPropertyValue("--duration-normal")', fixed = TRUE)
  expect_match(js, "var normalMotionDuration = 180", fixed = TRUE)
  expect_match(js, "var match = duration.match(", fixed = TRUE)
  expect_match(js, 'match[2] === "ms"', fixed = TRUE)
  expect_match(js, "requestAnimationFrame", fixed = TRUE)
  expect_match(
    js,
    'dialog.classList.add(visibleClass || "is-visible")',
    fixed = TRUE
  )
  expect_match(js, 'event.target !== dialog', fixed = TRUE)
  expect_match(js, "window.__builderMotionDuration + 60", fixed = TRUE)
  expect_length(
    regmatches(
      js,
      gregexpr("showTransientLayer(backdrop, dialog);", js, fixed = TRUE)
    )[[1L]],
    5L
  )
  expect_match(js, "setMarkerDialog", fixed = TRUE)
  expect_match(js, '"builder_marker_dialog", setMarkerDialog', fixed = TRUE)
  expect_length(
    regmatches(
      js,
      gregexpr("backdrop.remove()", js, fixed = TRUE)
    )[[1L]],
    1L
  )
  manager_close <- substr(
    js,
    regexpr("function closeDatasetManager()", js, fixed = TRUE)[[1L]],
    regexpr("function openDatasetManager()", js, fixed = TRUE)[[1L]] - 1L
  )
  aria_position <- regexpr(
    'summary.setAttribute("aria-expanded", "false")',
    manager_close,
    fixed = TRUE
  )[[1L]]
  focus_position <- regexpr("restoreFocus(rail)", manager_close, fixed = TRUE)[[
    1L
  ]]
  remove_position <- regexpr(
    "removeTransientLayer(",
    manager_close,
    fixed = TRUE
  )[[1L]]
  expect_gt(aria_position, 0L)
  expect_gt(focus_position, aria_position)
  expect_gt(remove_position, focus_position)

  expect_match(
    layout_css,
    ".rail-manager-backdrop.is-visible",
    fixed = TRUE
  )
  expect_match(layout_css, ".rail.is-manager-visible", fixed = TRUE)
  expect_match(
    components_css,
    ".builder-confirm-backdrop.is-visible",
    fixed = TRUE
  )
  expect_match(
    components_css,
    ".builder-analysis-info-backdrop.is-visible",
    fixed = TRUE
  )
  expect_match(components_css, ".builder-dialog.is-visible", fixed = TRUE)
  expect_match(
    paste(layout_css, components_css, sep = "\n"),
    "transition: opacity var(--duration-normal)",
    fixed = TRUE
  )
  expect_false(grepl(
    "transition:[^;]*(width|height|top|left|padding|grid)",
    paste(layout_css, components_css, sep = "\n"),
    perl = TRUE
  ))
})
