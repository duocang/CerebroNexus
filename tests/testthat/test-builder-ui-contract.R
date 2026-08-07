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
  expect_match(css, "@media \\(max-width: 43.75rem\\)")
})

test_that("builder exposes one compact responsive component system", {
  css <- builder_asset_text("www", "builder.css")

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
  expect_match(css, "max-width: 82.5rem", fixed = TRUE)
  expect_match(css, "@media (max-width: 68.75rem)", fixed = TRUE)
  expect_match(css, "@media (max-width: 43.75rem)", fixed = TRUE)
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

test_that("dataset uploads use an amber trigger without replacing the native chooser", {
  app <- builder_asset_text("app.R")
  css <- builder_asset_text("www", "builder.css")

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
  expect_match(css, "background: var(--c-amber)", fixed = TRUE)
})

test_that("supplementary tables use an amber native multi-file chooser", {
  stage <- builder_asset_text("ui", "enhance_stage.R")
  css <- builder_asset_text("www", "builder.css")

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
    app <- builder_asset_text("app.R"),
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
  css <- builder_asset_text("www", "builder.css")

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

test_that("group colors use native bounded controls without projection palettes", {
  core <- builder_asset_text("ui", "core_stage.R")
  js <- builder_asset_text("www", "builder.js")
  css <- builder_asset_text("www", "builder.css")

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

test_that("Review uses a compact responsive user-facing layout", {
  css <- builder_asset_text("www", "builder.css")
  app <- builder_asset_text("app.R")

  expect_match(css, ".review-app-grid", fixed = TRUE)
  expect_match(css, ".review-page-tags", fixed = TRUE)
  expect_match(css, ".review-output-fields", fixed = TRUE)
  expect_match(css, ".review-needs-attention", fixed = TRUE)
  expect_match(
    css,
    "grid-template-columns: repeat(2, minmax(0, 1fr))",
    fixed = TRUE
  )
  expect_match(css, "overflow-wrap: anywhere", fixed = TRUE)
  expect_match(app, '"Build"', fixed = TRUE)
  expect_match(app, '"Choose a folder…"', fixed = TRUE)
  expect_match(app, '"Building…"', fixed = TRUE)
  expect_false(grepl(
    'actionButton(\n      "build",\n      "Build",',
    app,
    fixed = TRUE
  ))
})

test_that("Build dialogs use accessible modal semantics and plain language", {
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
  expect_match(js, "Ready to build all datasets?", fixed = TRUE)
  expect_match(js, "Back to review", fixed = TRUE)
  expect_match(js, "Continue", fixed = TRUE)
  expect_match(js, "Choose another folder", fixed = TRUE)
  expect_match(js, "Replace existing files", fixed = TRUE)
  expect_match(js, "Some datasets have not been reviewed", fixed = TRUE)
  expect_match(js, "Review every dataset before building.", fixed = TRUE)
  expect_match(js, "Review now", fixed = TRUE)
  expect_match(js, "Some datasets still need attention", fixed = TRUE)
  expect_match(js, "Fix issues", fixed = TRUE)
  expect_match(js, "All ", fixed = TRUE)
  expect_match(js, " datasets have been reviewed.", fixed = TRUE)
  expect_false(grepl("window.confirm", js, fixed = TRUE))
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
  expect_match(css, "@media (max-width: 68.75rem)", fixed = TRUE)
  expect_match(css, "@media (max-width: 43.75rem)", fixed = TRUE)
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

test_that("static example cards show loading without hiding the directory", {
  js <- builder_asset_text("www", "builder.js")
  css <- builder_asset_text("www", "builder.css")

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
  css <- builder_asset_text("www", "builder.css")
  for (tone in c("analysis", "trajectory", "immune", "extra")) {
    expect_match(css, paste0(".builder-content-tag.is-", tone), fixed = TRUE)
  }
  expect_false(grepl(".builder-content-tag.is-optional", css, fixed = TRUE))
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
  expect_false(grepl("What this changes", enhance, fixed = TRUE))
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

test_that("Enhance analyses use amber selectable cards with quiet info controls", {
  css <- builder_asset_text("www", "builder.css")

  expect_match(css, ".enhance-module-select", fixed = TRUE)
  expect_match(css, ".enhance-module-title", fixed = TRUE)
  expect_match(css, ".enhance-info-button", fixed = TRUE)
  expect_match(css, ".enhance-module:hover", fixed = TRUE)
  expect_match(css, "background: var(--c-amber-50)", fixed = TRUE)
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
  expect_match(css, "background: var(--c-amber);", fixed = TRUE)
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

  css <- builder_asset_text("www", "builder.css")
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
      paste0('actionButton(\n        "', id, '"'),
      fixed = TRUE
    )
  }
  expect_false(grepl('tabindex = "-1"', status, fixed = TRUE))
})

test_that("spatial translation does not invalidate image encoding", {
  lines <- readLines(
    builder_asset_path("spatial_alignment_server.R"),
    warn = FALSE
  )
  start <- grep("encoded <- shiny::reactive", lines, fixed = TRUE)
  finish <- grep("current_record <- shiny::reactive", lines, fixed = TRUE)
  encoded <- paste(lines[start:(finish - 1L)], collapse = "\n")

  expect_false(grepl("img_dx|img_dy|img_scale|opacity|point_size", encoded))
  expect_match(encoded, "orientation()", fixed = TRUE)
})
