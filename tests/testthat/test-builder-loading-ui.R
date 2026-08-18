loading_path <- builder_profile_inst_path("builder", "loading.R")
rail_path <- builder_profile_inst_path("builder", "ui", "dataset_rail.R")
if (file.exists(loading_path)) {
  sys.source(loading_path, envir = globalenv())
}
if (file.exists(rail_path)) {
  sys.source(rail_path, envir = globalenv())
}

builder_loading_stylesheet_files <- c(
  "builder.tokens.css",
  "builder.base.css",
  "builder.layout.css",
  "builder.components.css",
  "builder.features.css"
)

test_that("loading UI uses the final five-file stylesheet manifest", {
  app <- paste(
    readLines(builder_profile_inst_path("builder", "app.R"), warn = FALSE),
    collapse = "\n"
  )
  manifest <- regmatches(
    app,
    regexpr(
      "builder_stylesheet_files <- c\\([^)]+\\)",
      app,
      perl = TRUE
    )
  )
  files <- regmatches(
    manifest,
    gregexpr('"builder(?:\\.[^."]+)*\\.css"', manifest, perl = TRUE)
  )[[1L]]
  files <- gsub('"', "", files, fixed = TRUE)

  expect_identical(files, builder_loading_stylesheet_files)
  expect_false("builder.css" %in% files)
})

test_that("workbench loading view follows the selected import row", {
  workflow <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "workflow.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(workflow, "loading_id <- active_import_id()", fixed = TRUE)
  expect_match(workflow, "selected_workflow_stage <- reactive({", fixed = TRUE)
  expect_match(workflow, 'return("configure")', fixed = TRUE)
  expect_false(grepl("loading_id <- import_focus_id()", workflow, fixed = TRUE))
  expect_false(grepl(
    "if (!is.null(import_focus_id()))",
    workflow,
    fixed = TRUE
  ))
})

test_that("the Builder initial HTML contains a stable non-empty shell", {
  app <- readLines(builder_profile_inst_path("builder", "app.R"), warn = FALSE)
  pre_server <- paste(
    app[seq_len(grep("server <- function", app, fixed = TRUE)[1L] - 1L)],
    collapse = "\n"
  )

  expect_match(pre_server, 'id = "ds_list"', fixed = TRUE)
  expect_match(pre_server, 'id = "ds_ready_list"', fixed = TRUE)
  expect_match(pre_server, 'id = "ds_import_list"', fixed = TRUE)
  expect_match(pre_server, 'id = "workbench"', fixed = TRUE)
  expect_match(pre_server, "builder_empty_workbench_ui()", fixed = TRUE)
  expect_match(pre_server, "builder-live-status", fixed = TRUE)
  expect_match(pre_server, 'id = "dataset_files"', fixed = TRUE)
  expect_match(pre_server, "builder_example_buttons_ui()", fixed = TRUE)
  expect_false(grepl('uiOutput("workbench")', pre_server, fixed = TRUE))
  expect_false(grepl('uiOutput("ds_list")', pre_server, fixed = TRUE))
})

test_that("the initial shell explains background workspace startup", {
  app <- paste(
    readLines(builder_profile_inst_path("builder", "app.R"), warn = FALSE),
    collapse = "\n"
  )
  foundation <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "foundation.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  client <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(app, 'id = "builder-worker-status"', fixed = TRUE)
  expect_match(app, "Starting background workspace", fixed = TRUE)
  expect_match(app, "Loading dataset readers and analysis tools", fixed = TRUE)
  expect_match(foundation, "session$onFlushed(", fixed = TRUE)
  expect_match(foundation, "start_builder_worker()", fixed = TRUE)
  expect_match(foundation, "shiny::isolate(worker())", fixed = TRUE)
  expect_match(client, '"builder_worker_status"', fixed = TRUE)
  expect_match(client, "builderWorkerReady", fixed = TRUE)
  expect_match(client, 'detail.hidden = state === "ready"', fixed = TRUE)
  expect_match(client, 'status.classList.add("is-dismissed")', fixed = TRUE)
  expect_match(client, "}, 1500)", fixed = TRUE)

  css <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.components.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(css, "position: fixed", fixed = TRUE)
  expect_match(css, ".builder-worker-status.is-dismissed", fixed = TRUE)
  expect_match(
    css,
    ".builder-worker-status.is-ready .builder-worker-status-copy span",
    fixed = TRUE
  )
})

test_that("project-folder selection remains available during client upload", {
  client <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  import_sensitive <- regmatches(
    client,
    regexpr(
      "(?s)var importSensitive = \\[.*?var workerSensitive =",
      client,
      perl = TRUE
    )
  )

  expect_false(grepl('"create_project"', import_sensitive, fixed = TRUE))
})

test_that("optional authentication packages are checked only on demand", {
  app <- paste(
    readLines(builder_profile_inst_path("builder", "app.R"), warn = FALSE),
    collapse = "\n"
  )
  build_server <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "build.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_false(grepl(
    "auth_capability <- builder_auth_capability()",
    app,
    fixed = TRUE
  ))
  expect_match(app, "auth_capability <- local({", fixed = TRUE)
  expect_match(build_server, "auth_capability()", fixed = TRUE)
})

test_that("dataset selection uses one hidden single-file Shiny transport", {
  app <- readLines(builder_profile_inst_path("builder", "app.R"), warn = FALSE)
  pre_server <- paste(
    app[seq_len(grep("server <- function", app, fixed = TRUE)[1L] - 1L)],
    collapse = "\n"
  )

  expect_match(pre_server, 'id = "builder_add_datasets"', fixed = TRUE)
  expect_match(pre_server, 'tags$label(', fixed = TRUE)
  expect_match(pre_server, 'role = "button"', fixed = TRUE)
  expect_match(
    pre_server,
    'class = "shiny-input-file builder-upload-transport"',
    fixed = TRUE
  )
  expect_match(pre_server, 'hidden = "hidden"', fixed = TRUE)
  expect_false(grepl('multiple = "multiple"', pre_server, fixed = TRUE))
  expect_match(pre_server, 'id = "ds_client_import_queue"', fixed = TRUE)
  expect_match(pre_server, '`aria-live` = "polite"', fixed = TRUE)
  expect_match(pre_server, '`aria-relevant` = "additions text"', fixed = TRUE)
  expect_identical(
    lengths(regmatches(
      pre_server,
      gregexpr('type = "file"', pre_server, fixed = TRUE)
    )),
    1L
  )
  expect_match(
    pre_server,
    "options(shiny.maxRequestSize = 10 * 1024^3)",
    fixed = TRUE
  )
})

test_that("ready and importing rail regions update independently", {
  app <- builder_app_source_text()

  expect_match(
    app,
    'sendCustomMessage("builder_dataset_rail_patch"',
    fixed = TRUE
  )
  expect_match(
    app,
    'sendCustomMessage("builder_import_rail_patch"',
    fixed = TRUE
  )
  expect_false(grepl("output$ds_import_list <- renderUI({", app, fixed = TRUE))
  expect_false(grepl("output$ds_ready_list <- renderUI({", app, fixed = TRUE))
  expect_false(grepl("output$ds_list <- renderUI({", app, fixed = TRUE))
})

test_that("import rail patches preserve unchanged sibling rows", {
  first <- builder_import_entry(
    "ds1",
    "Patient one",
    list(kind = "file", staged_path = "/private/ds1.rds"),
    filename = "ds1.rds",
    file_type = "RDS",
    size = 2048
  )
  second <- builder_import_entry(
    "ds2",
    "Patient two",
    list(kind = "file", staged_path = "/private/ds2.rds"),
    filename = "ds2.rds",
    file_type = "RDS",
    size = 4096
  )
  entries <- list(ds1 = first, ds2 = second)
  before <- builder_import_rail_patch(entries, current = "ds1")

  entries$ds1$load_state <- "reading"
  entries$ds1$progress_label <- "Reading Seurat object…"
  after <- builder_import_rail_patch(entries, current = "ds1")

  expect_null(names(before$rows))
  expect_identical(
    unname(vapply(before$rows, `[[`, character(1), "id")),
    c("ds1", "ds2")
  )
  expect_false(identical(
    before$rows[[1L]]$fingerprint,
    after$rows[[1L]]$fingerprint
  ))
  expect_identical(before$rows[[2L]]$fingerprint, after$rows[[2L]]$fingerprint)
  expect_match(after$rows[[1L]]$html, 'data-import-id="ds1"', fixed = TRUE)
  expect_match(after$rows[[1L]]$html, 'data-import-fingerprint="', fixed = TRUE)
})

test_that("loading workbench remains visible and accessible", {
  fun <- get0("builder_loading_workbench_ui", mode = "function")
  expect_true(is.function(fun))
  if (!is.function(fun)) {
    return()
  }
  entry <- builder_import_entry(
    "ds1",
    "All content",
    list(kind = "example", example = "all_content")
  )
  entry$load_state <- "inspecting"
  entry$progress_label <- "Checking cells, genes and metadata…"
  html <- htmltools::renderTags(fun(entry))$html

  expect_match(html, "Loading dataset", fixed = TRUE)
  expect_match(html, "All content", fixed = TRUE)
  expect_match(html, "Checking cells, genes and metadata…", fixed = TRUE)
  expect_match(html, 'aria-live="polite"', fixed = TRUE)
  expect_match(html, "builder-loading-stage", fixed = TRUE)
  expect_false(grepl("builder-remove-import", html, fixed = TRUE))
  expect_false(grepl(">Remove<", html, fixed = TRUE))
  expect_false(grepl("spinner", html, fixed = TRUE))
})

test_that("loading rail rows expose safe status and real actions", {
  fun <- get0("builder_import_rail_ui", mode = "function")
  expect_true(is.function(fun))
  if (!is.function(fun)) {
    return()
  }
  entry <- builder_import_entry(
    "ds1",
    "patient-one",
    list(
      kind = "file",
      staged_path = "/private/session/upload-123/object.rds"
    ),
    filename = "patient-one.rds",
    file_type = "RDS",
    size = 2048
  )
  html <- htmltools::renderTags(fun(list(ds1 = entry), current = "ds1"))$html

  expect_match(html, "patient-one", fixed = TRUE)
  expect_match(html, "Waiting to load", fixed = TRUE)
  expect_match(html, "builder-pick-import", fixed = TRUE)
  expect_match(html, "builder-remove-import", fixed = TRUE)
  expect_match(html, "ds ds--import is-active is-importing", fixed = TRUE)
  expect_match(html, 'data-load-state="queued"', fixed = TRUE)
  expect_match(html, 'aria-current="true"', fixed = TRUE)
  expect_match(
    html,
    'aria-label="Remove queued import patient-one"',
    fixed = TRUE
  )
  expect_match(html, "Remove from queue", fixed = TRUE)
  expect_match(html, "ds-state-dot", fixed = TRUE)
  expect_match(html, 'data-started-at-ms="', fixed = TRUE)
  expect_false(grepl("data-elapsed-ms", html, fixed = TRUE))
  expect_false(grepl("/private/session", html, fixed = TRUE))
})

test_that("active imports offer the established server cancellation action", {
  entry <- builder_import_entry(
    "ds1",
    "patient-one",
    list(kind = "file", staged_path = "/private/session/object.rds")
  )
  queue <- builder_import_add(builder_import_queue(), entry)
  queue <- builder_import_transition(queue, "ds1", "reading", 1L)

  rail_html <- htmltools::renderTags(
    builder_import_rail_ui(queue$entries, current = "ds1")
  )$html
  workbench_html <- htmltools::renderTags(
    builder_loading_workbench_ui(queue$entries[["ds1"]])
  )$html

  expect_match(rail_html, "builder-remove-import", fixed = TRUE)
  expect_match(rail_html, "Cancel active import patient-one", fixed = TRUE)
  expect_match(rail_html, ">Cancel<", fixed = TRUE)
  expect_false(grepl("builder-remove-import", workbench_html, fixed = TRUE))
  expect_false(grepl("Remove from queue", rail_html, fixed = TRUE))
  expect_false(grepl(">Remove<", workbench_html, fixed = TRUE))
})

test_that("error rows offer Retry and Remove without internal details", {
  fun <- get0("builder_import_rail_ui", mode = "function")
  expect_true(is.function(fun))
  if (!is.function(fun)) {
    return()
  }
  entry <- builder_import_entry(
    "ds1",
    "broken",
    list(kind = "file", staged_path = "/private/session/broken.rds")
  )
  queue <- builder_import_add(builder_import_queue(), entry)
  queue <- builder_import_transition(queue, "ds1", "reading", 1L)
  queue <- builder_import_transition(
    queue,
    "ds1",
    "error",
    1L,
    error = "/private/session/broken.rds: invalid object"
  )
  html <- htmltools::renderTags(fun(queue$entries, current = "ds1"))$html

  expect_match(html, "Could not load dataset", fixed = TRUE)
  expect_match(html, "builder-retry-import", fixed = TRUE)
  expect_match(html, "builder-remove-import", fixed = TRUE)
  expect_match(html, 'data-load-state="error"', fixed = TRUE)
  expect_match(html, "is-error", fixed = TRUE)
  expect_match(html, 'aria-label="Remove failed import broken"', fixed = TRUE)
  expect_match(html, 'data-elapsed-ms="', fixed = TRUE)
  expect_false(grepl("data-started-at-ms", html, fixed = TRUE))
  expect_false(grepl("/private/session", html, fixed = TRUE))
  expect_false(grepl("stack", html, ignore.case = TRUE))
})

test_that("client scheduler serializes file and example dispatch", {
  client <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  imports <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "imports.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  css <- paste(
    vapply(
      builder_loading_stylesheet_files,
      function(file) {
        paste(
          readLines(
            builder_profile_inst_path("builder", "www", file),
            warn = FALSE
          ),
          collapse = "\n"
        )
      },
      character(1)
    ),
    collapse = "\n"
  )

  expect_match(client, "var clientImportQueue = []", fixed = TRUE)
  expect_match(client, "var activeClientImport = null", fixed = TRUE)
  expect_match(client, "function openDatasetPicker()", fixed = TRUE)
  expect_match(client, 'picker.multiple = true', fixed = TRUE)
  expect_match(client, "function enqueueClientFiles(fileList)", fixed = TRUE)
  expect_match(client, "function enqueueExample(example)", fixed = TRUE)
  expect_match(client, "function dispatchNextClientImport()", fixed = TRUE)
  expect_match(client, "if (activeClientImport) return", fixed = TRUE)
  expect_match(client, "new DataTransfer()", fixed = TRUE)
  expect_match(client, "transport.files = transfer.files", fixed = TRUE)
  expect_match(client, "transport.files.length !== 1", fixed = TRUE)
  expect_match(client, "try {", fixed = TRUE)
  expect_match(client, "failClientDispatch", fixed = TRUE)
  expect_match(client, "builder_import_example", fixed = TRUE)
  expect_match(
    client,
    'showClientLoadingWorkbench(entry.name, "Waiting to load…")',
    fixed = TRUE
  )
  expect_match(client, "builder_client_import_dispatch", fixed = TRUE)
  expect_match(client, "builder-cancel-client-import", fixed = TRUE)
  expect_match(client, "entry !== activeClientImport", fixed = TRUE)
  expect_match(client, 'addEventListener("drop"', fixed = TRUE)
  expect_match(
    client,
    'document.addEventListener("shiny:disconnected"',
    fixed = TRUE
  )
  expect_match(client, "importSyncPending = true", fixed = TRUE)
  expect_match(client, "builder_import_sync_request", fixed = TRUE)
  expect_match(client, "builder_import_sync", fixed = TRUE)
  expect_match(client, 'entry.state = "unknown"', fixed = TRUE)
  expect_match(client, "Waiting to restore the import state", fixed = TRUE)
  expect_match(client, 'getElementById("ds_client_import_queue")', fixed = TRUE)
  expect_match(client, 'dot.className = "ds-state-dot"', fixed = TRUE)
  expect_match(client, "Uploading…", fixed = TRUE)
  expect_match(client, "Waiting · ", fixed = TRUE)
  expect_match(client, "Possible duplicate", fixed = TRUE)
  expect_match(client, "applyClientImportQueueLock", fixed = TRUE)
  expect_match(imports, "if (builder_session_closed())", fixed = TRUE)
  expect_match(client, "textContent", fixed = TRUE)
  expect_match(client, ".builder-pick-import", fixed = TRUE)
  expect_match(client, ".builder-retry-import", fixed = TRUE)
  expect_match(client, ".builder-remove-import", fixed = TRUE)
  expect_false(grepl("beginClientDatasetUpload", client, fixed = TRUE))
  expect_false(grepl('send("use_example"', client, fixed = TRUE))
  expect_match(css, ".builder-loading-stage", fixed = TRUE)
  expect_match(css, ".builder-loading-progress", fixed = TRUE)
  expect_match(
    css,
    ".ds--client-upload .builder-cancel-client-import",
    fixed = TRUE
  )
  expect_match(css, "position: static", fixed = TRUE)
  expect_match(css, "align-self: flex-end", fixed = TRUE)
  expect_match(css, "prefers-reduced-motion: reduce", fixed = TRUE)
})

test_that("dataset import feedback stays legible while Shiny recalculates", {
  css <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.components.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(css, "#ds_import_list.recalculating", fixed = TRUE)
  expect_match(css, "#ds_client_import_queue.recalculating", fixed = TRUE)
  expect_match(css, "--shiny-fade-opacity: 1", fixed = TRUE)
})
