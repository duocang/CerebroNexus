builder_data_page_runtime <- function() {
  runtime <- new.env(parent = globalenv())
  runtime$builder_example_directory <- function() list()
  runtime$builder_example_buttons_ui <- function(...) {
    shiny::tags$button(
      class = "builder-data-source example-btn",
      shiny::tags$span(
        class = "builder-data-source-copy",
        "Complete Viewer data"
      ),
      shiny::tags$span(
        class = "builder-data-source-tag is-example",
        "Load Example"
      )
    )
  }
  runtime$builder_stage_footer_ui <- function(status, ...) {
    shiny::tags$footer(
      class = "builder-stage-footer",
      shiny::tags$p(class = "builder-stage-footer-status", status),
      ...
    )
  }
  sys.source(
    builder_profile_inst_path("builder", "ui", "dataset_rail.R"),
    envir = runtime
  )
  runtime
}

builder_data_page_html <- function() {
  runtime <- builder_data_page_runtime()
  htmltools::renderTags(runtime$builder_empty_workbench_ui(
    formats = c("rds", "qs", "qs2"),
    examples = list()
  ))$html
}

test_that("Data page makes the whole dropzone the upload action", {
  withr::local_package("shiny")
  html <- builder_data_page_html()

  expect_match(html, "builder-dataset-dropzone-copy", fixed = TRUE)
  expect_match(html, "builder-add-datasets", fixed = TRUE)
  expect_match(html, '<button', fixed = TRUE)
  expect_match(html, 'type="button"', fixed = TRUE)
  expect_false(grepl('role="button"', html, fixed = TRUE))
  expect_identical(
    lengths(regmatches(html, gregexpr("Add your data", html, fixed = TRUE))),
    1L
  )
  expect_false(grepl("builder-dataset-dropzone-actions", html, fixed = TRUE))
  expect_false(grepl(">Add files<", html, fixed = TRUE))
  expect_false(grepl("builder-browse-dataset-files", html, fixed = TRUE))
  expect_false(grepl("builder-local-dataset-files", html, fixed = TRUE))
  expect_match(html, "Load Example", fixed = TRUE)
  expect_false(grepl("Start with one dataset", html, fixed = TRUE))
  expect_false(grepl("builder-stage-footer", html, fixed = TRUE))
})

test_that("Data page names readers disabled by missing packages", {
  runtime <- builder_data_page_runtime()
  html <- htmltools::renderTags(runtime$builder_empty_workbench_ui(
    formats = list(
      list(id = "rds", label = "RDS", available = TRUE),
      list(
        id = "qs",
        label = "qs",
        available = FALSE,
        unavailable_reason = paste(
          "QS files are disabled because the R package 'qs' is not installed.",
          'Install it with install.packages("qs"), then restart Builder.'
        )
      )
    ),
    examples = list()
  ))$html

  expect_match(html, "builder-format-unavailable", fixed = TRUE)
  expect_match(html, "QS files are disabled", fixed = TRUE)
  expect_match(html, 'install.packages("qs")', fixed = TRUE)
  expect_match(html, ">RDS<", fixed = TRUE)
  expect_false(grepl(">RDS, QS<", html, fixed = TRUE))
})

test_that("Data page keeps the workspace informative while imports run", {
  withr::local_package("shiny")
  runtime <- builder_data_page_runtime()
  html <- htmltools::renderTags(
    runtime$builder_importing_workbench_ui(2L)
  )$html

  expect_match(html, "builder-loading-state", fixed = TRUE)
  expect_match(html, "Preparing your workspace", fixed = TRUE)
  expect_match(html, "2 datasets are loading", fixed = TRUE)
  expect_match(html, "builder-loading-skeleton", fixed = TRUE)
  expect_match(html, 'aria-busy="true"', fixed = TRUE)
  expect_match(html, 'role="status"', fixed = TRUE)
  expect_match(html, 'aria-live="polite"', fixed = TRUE)
  expect_false(grepl('class="spinner"', html, fixed = TRUE))
})

test_that("Data page replaces loading copy when every import failed", {
  withr::local_package("shiny")
  runtime <- builder_data_page_runtime()
  html <- htmltools::renderTags(
    runtime$builder_failed_import_workbench_ui(2L)
  )$html

  expect_match(html, "builder-import-failed-state", fixed = TRUE)
  expect_match(html, "Datasets could not be loaded", fixed = TRUE)
  expect_match(html, "Read the error in the dataset list", fixed = TRUE)
  expect_match(html, 'role="alert"', fixed = TRUE)
  expect_false(grepl("Preparing your workspace", html, fixed = TRUE))
  expect_false(grepl('aria-busy="true"', html, fixed = TRUE))
})

test_that("Dataset rail presents one concise readiness status", {
  withr::local_package("shiny")
  runtime <- builder_data_page_runtime()
  model <- list(
    index = 1L,
    id = "dataset-a",
    label = "Dataset A",
    cells = 180L,
    format = "Built-in example",
    import_elapsed_ms = 5500,
    readiness_label = "Blocked",
    checked = FALSE,
    selected = TRUE,
    confirm = FALSE,
    can_up = FALSE,
    can_down = TRUE
  )
  html <- htmltools::renderTags(
    runtime$builder_dataset_rail_row_ui(model)
  )$html

  expect_false(grepl("rail-readiness-status", html, fixed = TRUE))
  expect_false(grepl(">Blocked<", html, fixed = TRUE))
  expect_identical(
    lengths(regmatches(html, gregexpr("Needs checking", html, fixed = TRUE))),
    1L
  )
  expect_match(html, 'role="status"', fixed = TRUE)
  expect_match(html, 'aria-live="polite"', fixed = TRUE)
  expect_match(html, 'aria-atomic="true"', fixed = TRUE)
})

test_that("Dataset rail distinguishes saved CRBs from loaded sources", {
  withr::local_package("shiny")
  runtime <- builder_data_page_runtime()
  model <- list(
    index = 1L,
    id = "dataset-a",
    label = "Dataset A",
    cells = 180L,
    format = "qs",
    import_elapsed_ms = NA_real_,
    readiness_label = "CRB ready",
    checked = TRUE,
    selected = TRUE,
    confirm = FALSE,
    can_up = FALSE,
    can_down = FALSE
  )

  crb_html <- htmltools::renderTags(
    runtime$builder_dataset_rail_row_ui(model)
  )$html
  model$readiness_label <- "Ready"
  source_html <- htmltools::renderTags(
    runtime$builder_dataset_rail_row_ui(model)
  )$html

  expect_match(crb_html, ">Checked<", fixed = TRUE)
  expect_match(crb_html, "CRB ready", fixed = TRUE)
  expect_match(source_html, ">Checked<", fixed = TRUE)
  expect_match(source_html, "No CRB", fixed = TRUE)
})

test_that("Data page shows its footer only after a dataset exists", {
  withr::local_package("shiny")
  runtime <- builder_data_page_runtime()
  html <- htmltools::renderTags(runtime$builder_empty_workbench_ui(
    dataset_count = 1L,
    formats = c("rds", "qs", "qs2"),
    examples = list()
  ))$html

  expect_match(html, "builder-stage-footer", fixed = TRUE)
  expect_match(html, ">1 dataset<", fixed = TRUE)
  expect_match(html, "Configure Datasets", fixed = TRUE)
})

test_that("Data page upload action prefers native files with a browser fallback", {
  js <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(js, "addDatasetFiles(addDatasets)", fixed = TRUE)
  expect_match(js, "function addDatasetFiles", fixed = TRUE)
  expect_match(js, 'send("builder_choose_dataset_files"', fixed = TRUE)
  expect_match(js, '"builder_dataset_picker_fallback"', fixed = TRUE)
  expect_match(js, "function openDatasetPicker(message)", fixed = TRUE)
  expect_false(grepl("choose_local_datasets", js, fixed = TRUE))
  expect_false(grepl("startFilePickerRecovery", js, fixed = TRUE))
  expect_false(grepl("datasetFileSource", js, fixed = TRUE))
  expect_false(grepl("tableFileSource", js, fixed = TRUE))
  expect_false(grepl(
    '".builder-dataset-dropzone.builder-add-datasets"',
    js,
    fixed = TRUE
  ))
  expect_false(grepl(
    "addDatasetFiles(datasetDropzone)",
    js,
    fixed = TRUE
  ))
  expect_false(grepl("browseDatasets", js, fixed = TRUE))
  expect_false(grepl("localDatasets", js, fixed = TRUE))
  expect_match(js, "enqueueClientFiles(event.dataTransfer", fixed = TRUE)
})

test_that("Data page updates do not replay full-page motion", {
  components <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.components.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  layout <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.layout.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(components, "#workbench.recalculating {", fixed = TRUE)
  expect_match(
    components,
    ".builder-stage-upload {\n  animation: none;",
    fixed = TRUE
  )
  expect_match(
    components,
    "#shiny-notification-panel .shiny-notification-message",
    fixed = TRUE
  )
  expect_match(
    layout,
    ".builder-shell:not(:has(.rail .ds)) {\n  min-height: 0;",
    fixed = TRUE
  )
  expect_match(layout, "max-height: 0;", fixed = TRUE)
  expect_match(layout, "transition: min-height", fixed = TRUE)
})

test_that("the real example action uses the approved secondary label", {
  app <- paste(
    readLines(builder_profile_inst_path("builder", "app.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(app, '"Load Example"', fixed = TRUE)
  expect_match(app, "data-examples", fixed = TRUE)

  js <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(js, "example.dataset.examples", fixed = TRUE)
  expect_match(js, "members.forEach", fixed = TRUE)
})

test_that("dataset and table uploads do not have server-side file pickers", {
  imports <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "imports.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  enhancements <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "enhancements.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_false(grepl("choose_local_datasets", imports, fixed = TRUE))
  expect_false(grepl("builder_cancel_file_picker", imports, fixed = TRUE))
  expect_false(grepl("enhance-choose_local_tables", enhancements, fixed = TRUE))
  expect_false(grepl("native_table_picker_active", enhancements, fixed = TRUE))
})

test_that("Precision Scientific is the final Builder presentation layer", {
  app <- paste(
    readLines(builder_profile_inst_path("builder", "app.R"), warn = FALSE),
    collapse = "\n"
  )
  theme_path <- builder_profile_inst_path(
    "builder",
    "www",
    "builder.editorial.css"
  )

  expect_match(
    app,
    '"builder.features.css",\n  "builder.editorial.css"',
    fixed = TRUE
  )
  expect_true(file.exists(theme_path))

  theme <- paste(readLines(theme_path, warn = FALSE), collapse = "\n")
  expect_false(grepl("--builder-font-serif:", theme, fixed = TRUE))
  expect_match(
    theme,
    "font-family: var(--builder-font-sans)",
    fixed = TRUE
  )
  expect_match(theme, ".builder-stage-header h2", fixed = TRUE)
  expect_match(theme, "--c-text-3: #68706b", fixed = TRUE)
  expect_match(theme, "touch-action: manipulation", fixed = TRUE)
  expect_match(theme, "-webkit-tap-highlight-color", fixed = TRUE)
  expect_match(theme, ".builder-loading-state", fixed = TRUE)
  expect_match(theme, ".builder-loading-skeleton", fixed = TRUE)
  expect_match(
    theme,
    paste(
      ".builder-detected-content .builder-content-tag {",
      "  border: 0;",
      "  background: transparent;",
      sep = "\n"
    ),
    fixed = TRUE
  )
  expect_match(theme, ".builder-data-source:focus-visible", fixed = TRUE)
  layout <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.layout.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(layout, "min-height: calc(100dvh - 12rem)", fixed = TRUE)
  expect_match(theme, ".ds.is-active::before", fixed = TRUE)
  expect_match(
    theme,
    ".builder-configure-metadata-chips > span",
    fixed = TRUE
  )
  expect_match(theme, ".builder-viewer-card[open]", fixed = TRUE)
  expect_match(
    theme,
    paste0(
      ".ds[data-load-state=\"ready\"] .ds-actions {\n",
      "  display: grid;\n",
      "  grid-template-columns: minmax(0, 1fr) minmax(0, 1fr) auto;"
    ),
    fixed = TRUE
  )
  expect_match(
    theme,
    paste0(
      ".ds[data-load-state=\"ready\"] .ds-actions > button {\n",
      "  min-width: 0;"
    ),
    fixed = TRUE
  )
  expect_match(theme, "@media (max-width: 58rem)", fixed = TRUE)

  stage <- paste(
    readLines(
      builder_profile_inst_path("builder", "ui", "core_stage", "stage.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(stage, 'h3("Essentials")', fixed = TRUE)
  expect_match(stage, 'h4("Viewer content")', fixed = TRUE)
  expect_match(stage, 'h4("Views")', fixed = TRUE)
  expect_false(grepl('h4("CRB content")', stage, fixed = TRUE))
})
