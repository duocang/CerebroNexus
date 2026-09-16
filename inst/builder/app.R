##----------------------------------------------------------------------------##
## Build Cerebro data sets from Seurat objects, by pointing and clicking.
##
## Several objects per session: each becomes a .crb, and they are bundled into
## one app with the data set switcher, which is how a lab usually wants to hand
## a project over -- one link, every sample behind a dropdown.
##
## Objects are opened only in an isolated worker process. This is still a tool
## for your own machine or RStudio Server session, not a service to deploy for
## untrusted users.
##----------------------------------------------------------------------------##

.builder_app_shiny_was_attached <- "package:shiny" %in% search()
if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("The shiny package is required to run Builder.", call. = FALSE)
}
## Import Shiny's public API into this App environment without attaching the
## package to the caller's search path.
.builder_app_environment <- environment()
for (.builder_shiny_export in getNamespaceExports("shiny")) {
  if (!exists(
    .builder_shiny_export,
    envir = .builder_app_environment,
    inherits = FALSE
  )) {
    assign(
      .builder_shiny_export,
      getExportedValue("shiny", .builder_shiny_export),
      envir = .builder_app_environment
    )
  }
}
rm(.builder_shiny_export, .builder_app_environment)

`%||%` <- function(a, b) if (is.null(a)) b else a

## Bootstrap the UTF-8 loader without changing the caller's process-wide
## encoding option. launchCerebroBuilder() runs in the user's R session, so a
## global option change here would survive after the App stops.
base::source(
  "prerequisite.R",
  local = TRUE,
  encoding = "UTF-8"
)
source <- function(file, local = FALSE, ...) {
  envir <- if (isTRUE(local)) {
    parent.frame()
  } else if (is.environment(local)) {
    local
  } else {
    globalenv()
  }
  builder_source_utf8(file, envir = envir)
}
builder_source_package <- builder_source_package_root()
builder_runtime_version <- builder_runtime_package_version()
runtime_capability <- NULL

.builder_app_process_state <- new.env(parent = emptyenv())
.builder_app_process_state$active <- FALSE

builder_app_restore_process_state <- function() {
  if (!isTRUE(.builder_app_process_state$active)) {
    return(invisible(FALSE))
  }
  options(.builder_app_process_state$request_size)
  source_package <- .builder_app_process_state$source_package
  if (is.na(source_package)) {
    Sys.unsetenv("CEREBRO_PACKAGE_SOURCE")
  } else {
    do.call(
      Sys.setenv,
      stats::setNames(list(source_package), "CEREBRO_PACKAGE_SOURCE")
    )
  }
  if (
    !isTRUE(.builder_app_process_state$shiny_was_attached) &&
      "package:shiny" %in% search()
  ) {
    try(detach("package:shiny", unload = FALSE, character.only = TRUE), silent = TRUE)
  }
  .builder_app_process_state$active <- FALSE
  invisible(TRUE)
}

builder_app_start <- function(.capability = builder_runtime_capability()) {
  if (isTRUE(.builder_app_process_state$active)) {
    return(invisible(FALSE))
  }
  if (!isTRUE(.capability$available)) {
    stop(.capability$reason, call. = FALSE)
  }
  runtime_capability <<- .capability
  .builder_app_process_state$request_size <- options("shiny.maxRequestSize")
  .builder_app_process_state$source_package <- Sys.getenv(
    "CEREBRO_PACKAGE_SOURCE",
    unset = NA_character_
  )
  .builder_app_process_state$shiny_was_attached <-
    .builder_app_shiny_was_attached
  .builder_app_process_state$active <- TRUE
  shiny::onStop(builder_app_restore_process_state)
  ## Serialized Seurat objects can be substantially larger than Shiny's
  ## default 5 MiB upload limit. Respect a launcher-supplied cap.
  if (is.null(getOption("shiny.maxRequestSize"))) {
    options(shiny.maxRequestSize = 10 * 1024^3)
  }
  if (!is.null(builder_source_package)) {
    builder_activate_source_package(builder_source_package)
  }
  invisible(TRUE)
}

source(
  file.path("core", "bundle_path_contract.R"),
  local = TRUE
)
source("publish.R", local = TRUE)
source("app_bundle.R", local = TRUE)
source("report.R", local = TRUE)
source("coordinator.R", local = TRUE)

## runApp() sets the working directory to the app directory.
source("io.R", local = TRUE)
source("project.R", local = TRUE)
source(
  file.path(
    "..",
    "viewer",
    "core",
    "viewer_content_contract.R"
  ),
  local = TRUE
)
source(
  file.path(
    "..",
    "viewer",
    "core",
    "spatial_coordinate_contract.R"
  ),
  local = TRUE
)
source(
  file.path(
    "..",
    "viewer",
    "core",
    "spatial_coordinate_transform.R"
  ),
  local = TRUE
)
source("spatial.R", local = TRUE)
source(
  file.path(
    "..",
    "viewer",
    "hla_tcr_motifs",
    "core",
    "hla_typing.R"
  ),
  local = TRUE
)
source(
  file.path(
    "..",
    "viewer",
    "hla_tcr_motifs",
    "core",
    "hla_motif_core.R"
  ),
  local = TRUE
)
source(
  file.path(
    "..",
    "viewer",
    "hla_tcr_motifs",
    "core",
    "hla_association_core.R"
  ),
  local = TRUE
)
source("manifest.R", local = TRUE)
source("content_tables.R", local = TRUE)
source("content_immune.R", local = TRUE)
source("content_spatial.R", local = TRUE)
source("content.R", local = TRUE)
source("profile.R", local = TRUE)
source("inspect.R", local = TRUE)
source("recommend.R", local = TRUE)
source("adapters.R", local = TRUE)
source("preview.R", local = TRUE)
source("stats.R", local = TRUE)
source("extras.R", local = TRUE)
source("analysis.R", local = TRUE)
source("marker_import.R", local = TRUE)
source("build.R", local = TRUE)
source("state.R", local = TRUE)
source("review.R", local = TRUE)
source("workflow.R", local = TRUE)
source("loading.R", local = TRUE)
source("async.R", local = TRUE)
source(file.path("ui", "dataset_rail.R"), local = TRUE)
source("plan.R", local = TRUE)
source(file.path("ui", "inspect_stage.R"), local = TRUE)
source(file.path("ui", "core_stage.R"), local = TRUE)
source(file.path("ui", "marker_import.R"), local = TRUE)
source(file.path("ui", "enhance_stage.R"), local = TRUE)
source(file.path("ui", "review_stage.R"), local = TRUE)
source(file.path("ui", "workflow.R"), local = TRUE)
source(file.path("ui", "build_status.R"), local = TRUE)
source(file.path("ui", "project.R"), local = TRUE)
source("worker.R", local = TRUE)
source("session.R", local = TRUE)
source("spatial_alignment_server.R", local = TRUE)

builder_projection_preview_contract <- function(entry, projections) {
  list(
    dataset = entry$id,
    snapshot_identity = .builder_worker_identity(entry$snapshot),
    group = entry$settings$default_group %||% NULL,
    projections = projections
  )
}

builder_trajectory_preview_contract <- function(entry, trajectories) {
  list(
    dataset = entry$id,
    snapshot_identity = .builder_worker_identity(entry$snapshot),
    trajectories = trajectories
  )
}

builder_table_inventory_owner <- function(entry, project_epoch) {
  if (!is.list(entry) || !builder_has_text(entry$id)) {
    return(NULL)
  }
  snapshot_identity <- .builder_worker_identity(entry$snapshot)
  epoch <- suppressWarnings(as.double(project_epoch))
  if (
    !builder_has_text(snapshot_identity) ||
      length(epoch) != 1L ||
      is.na(epoch) ||
      !is.finite(epoch)
  ) {
    return(NULL)
  }
  list(
    dataset = as.character(entry$id),
    snapshot_identity = snapshot_identity,
    project_epoch = epoch
  )
}

builder_table_inventory_owner_is_current <- function(
  owner,
  entry,
  project_epoch
) {
  !is.null(owner) && identical(
    owner,
    builder_table_inventory_owner(entry, project_epoch)
  )
}

builder_preview_cache_hit <- function(cache, id, contract) {
  record <- cache[[id]] %||% NULL
  is.list(record) && identical(record$contract, contract)
}

builder_preview_cache_begin <- function(cache, id, contract) {
  cache[[id]] <- list(
    contract = contract,
    frames = list(),
    status = "pending"
  )
  cache
}

builder_preview_cache_store <- function(cache, id, frames) {
  record <- cache[[id]] %||% list(contract = NULL)
  record$frames <- frames %||% list()
  record$status <- "ready"
  cache[[id]] <- record
  cache
}

builder_preview_cache_frames <- function(cache, id) {
  record <- cache[[id]] %||% NULL
  if (is.list(record)) record$frames %||% list() else list()
}

builder_preview_revision_independent <- function(kind) {
  kind %in%
    c(
      "projection_previews",
      "trajectory_previews",
      "spatial_preview"
    )
}

app_capability <- builder_app_capability()
auth_capability <- builder_auth_capability

## The viewer's wordmark, inlined. The builder and the viewer are one product
## and should look like it; inlining avoids a resource path that would have to
## resolve both from the installed package and from a source checkout.
cerebro_wordmark <- local({
  candidates <- c(
    system.file(
      "viewer/www/cerebronexus.svg",
      package = "CerebroNexus"
    ),
    file.path("..", "viewer", "www", "cerebronexus.svg")
  )
  hit <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(hit)) {
    HTML(paste(readLines(hit[1], warn = FALSE), collapse = "\n"))
  } else {
    NULL
  }
})

## Stamp the assets so a browser that already has them does not keep them past
## an upgrade. Shiny serves www/ with caching on, and a cached stylesheet
## against new markup is worse than either alone -- the classes change and the
## rules that give them meaning do not arrive. The stamp is the file's own
## mtime, so it changes exactly when the file does, including while editing.
asset_stamp <- function(file) {
  mt <- tryCatch(file.mtime(file), error = function(e) NA)
  if (is.na(mt)) {
    return("")
  }
  paste0("?v=", as.integer(as.numeric(mt)))
}

builder_stylesheet_files <- c(
  "builder.tokens.css",
  "builder.base.css",
  "builder.layout.css",
  "builder.components.css",
  "builder.features.css",
  "builder.editorial.css"
)

builder_stylesheet_tags <- function(
  files = builder_stylesheet_files
) {
  tagList(lapply(files, function(file) {
    tags$link(
      rel = "stylesheet",
      href = paste0(file, asset_stamp(file.path("www", file)))
    )
  }))
}

builder_example_buttons_ui <- function(examples = builder_example_directory()) {
  tagList(lapply(examples, function(ex) {
    members <- ex$examples %||% list(list(id = ex$id, label = ex$label))
    tags$button(
      class = "builder-data-source example-btn",
      type = "button",
      `data-ex` = ex$id,
      `data-label` = ex$label,
      `data-examples` = jsonlite::toJSON(
        members,
        auto_unbox = TRUE
      ),
      `aria-disabled` = "false",
      tags$span(
        class = "builder-data-source-icon",
        `aria-hidden` = "true",
        shiny::icon("wand-magic-sparkles")
      ),
      tags$span(
        class = "builder-data-source-copy",
        tags$strong(class = "ex-label", ex$label),
        tags$small(class = "ex-detail", ex$detail)
      ),
      tags$span(
        class = "builder-data-source-tag is-example",
        "Load Example"
      )
    )
  }))
}

builder_app_acknowledge_build <- function(protocol, request_id) {
  builder_protocol_acknowledge(protocol, request_id)
}

builder_app_build_action <- function(result, id) {
  result <- builder_as_result(result)
  if (identical(result$state, "recovery_required")) {
    return(list(
      type = "fail",
      id = id,
      error = result$error %||%
        result$message %||%
        "Release recovery is required."
    ))
  }
  builder_build_action(result, id)
}

builder_app_settle_release <- function(
  release,
  value,
  .publish = builder_coordinator_publish,
  .abort = builder_coordinator_abort,
  .release_error = builder_release_error_result
) {
  if (
    !is.list(release) ||
      !is.list(release$handle) ||
      !builder_stage_has_text(release$handle$target %||% "")
  ) {
    return(builder_result_failure(
      "The parent release coordinator identity was lost."
    ))
  }
  settled <- builder_coordinator_settle(
    release$handle,
    value,
    .publish = .publish,
    .abort = .abort,
    .recovery = function(target) NULL
  )
  if (!isTRUE(settled$ok)) {
    return(.release_error(settled$error, release$handle$target))
  }
  typed <- try(builder_as_result(settled$value), silent = TRUE)
  if (inherits(typed, "try-error")) {
    return(builder_result_failure(
      "The worker returned an unsupported terminal build result."
    ))
  }
  typed
}

ui <- tagList(
  shiny::bootstrapLib(),
  tags$head(
    builder_stylesheet_tags(),
    tags$script(src = paste0("icons.js", asset_stamp("www/icons.js"))),
    tags$script(
      src = paste0(
        "builder-spatial-canvas.js",
        asset_stamp("www/builder-spatial-canvas.js")
      )
    ),
    tags$script(src = paste0("builder.js", asset_stamp("www/builder.js"))),
    tags$title("Cerebro Dataset Builder")
  ),
  tags$a(
    class = "builder-skip-link",
    href = "#builder-workspace",
    "Skip to workspace"
  ),
  tags$header(
    class = "topbar builder-project-header",
    div(
      class = "builder-project-brand",
      div(class = "wordmark", cerebro_wordmark),
      if (!is.null(builder_runtime_version)) {
        tags$span(
          class = "builder-runtime-version",
          `aria-label` = paste("CerebroNexus version", builder_runtime_version),
          paste0("v", builder_runtime_version)
        )
      },
      tags$h1(class = "visually-hidden", "CerebroNexus Builder")
    ),
    uiOutput("busy", inline = TRUE),
    builder_project_toolbar_ui()
  ),
  div(
    id = "builder-worker-status",
    class = "builder-worker-status is-starting",
    `aria-hidden` = "false",
    span(class = "builder-worker-status-dot", `aria-hidden` = "true"),
    span(
      class = "builder-worker-status-copy",
      strong(
        id = "builder-worker-status-title",
        "Starting background workspace\u2026"
      ),
      span(
        id = "builder-worker-status-detail",
        "Loading dataset readers and analysis tools\u2026"
      )
    )
  ),
  uiOutput("workflow_progress"),
  div(
    class = "shell builder-shell",
    div(
      class = "rail",
      div(
        class = "rail-head",
        span("Datasets"),
        span(
          textOutput("ds_count", inline = TRUE),
          uiOutput("rail_undo", inline = TRUE)
        )
      ),
      div(
        id = "ds_list",
        class = "ds-picker",
        div(
          id = "ds_ready_list",
          class = "shiny-html-output",
          builder_dataset_rail_ui(builder_state())
        ),
        div(
          id = "ds_import_list",
          class = "shiny-html-output"
        ),
        div(
          id = "ds_client_import_queue",
          class = "builder-client-import-queue",
          `aria-live` = "polite",
          `aria-relevant` = "additions text"
        )
      ),
      div(
        class = "rail-add-actions",
        tags$button(
          type = "button",
          class = "btn builder-add-datasets",
          `aria-label` = "Add dataset files",
          tags$span(
            class = "rail-add-title",
            tags$span(
              class = "rail-add-icon",
              `aria-hidden` = "true",
              shiny::icon("folder-open")
            ),
            tags$strong(class = "builder-add-label", "Add files")
          ),
          tags$small(class = "rail-add-detail", "RDS, QS, or QS2")
        ),
        tags$input(
          id = "dataset_files",
          name = "dataset_files",
          class = "shiny-input-file builder-upload-transport",
          type = "file",
          accept = builder_file_accept(builder_dataset_extensions()),
          hidden = "hidden"
        )
      )
    ),
    tags$main(
      id = "builder-workspace",
      class = "builder-content",
      tabindex = "-1",
      div(
        id = "pane",
        div(
          id = "workbench",
          class = "shiny-html-output",
          tabindex = "-1",
          builder_empty_workbench_ui(
            formats = builder_formats,
            examples = builder_example_directory()
          )
        )
      )
    ),
  ),
  div(
    id = "builder-operation-overlay",
    class = "builder-operation-overlay",
    role = "status",
    `aria-live` = "polite",
    `aria-hidden` = "true",
    `aria-labelledby` = "builder-operation-overlay-title",
    `aria-describedby` = paste(
      "builder-operation-overlay-message",
      "builder-operation-overlay-detail"
    ),
    tabindex = "-1",
    div(
      class = "builder-operation-overlay-card",
      span(
        class = "builder-operation-overlay-icon",
        `aria-hidden` = "true",
        span(class = "spinner"),
        span(class = "builder-operation-success-mark", "\u2713"),
        span(class = "builder-operation-error-mark", "!")
      ),
      div(
        class = "builder-operation-overlay-copy",
        strong(
          id = "builder-operation-overlay-title",
          "Working on your Builder project"
        ),
        span(
          id = "builder-operation-overlay-message",
          "Keep this page open."
        ),
        span(
          id = "builder-operation-overlay-detail",
          class = "builder-operation-overlay-detail"
        ),
        tags$div(
          id = "builder-build-progress",
          class = "builder-build-progress",
          hidden = "hidden",
          tags$ol(
            class = "builder-build-progress-steps",
            tags$li(`data-build-phase` = "prepare", "Prepare release"),
            tags$li(`data-build-phase` = "datasets", "Build datasets"),
            tags$li(`data-build-phase` = "viewer", "Package Viewer"),
            tags$li(`data-build-phase` = "publish", "Verify & publish")
          )
        )
      ),
      div(
        id = "builder-operation-overlay-actions",
        class = "builder-operation-overlay-actions",
        `aria-hidden` = "true"
      )
    )
  ),
  builder_auth_dialog_ui(),
  builder_marker_dialog_ui(),
  div(
    id = "builder-live-status",
    class = "visually-hidden",
    role = "status",
    `aria-live` = "polite",
    `aria-atomic` = "true"
  )
)

server <- function(input, output, session) {
  for (.builder_server_source in c(
    "server/foundation.R",
    "server/imports.R",
    "server/datasets.R",
    "server/enhancements.R",
    "server/review.R",
    "server/workflow.R",
    "server/build.R",
    "server/project.R"
  )) {
    source(.builder_server_source, local = TRUE)
  }
  rm(.builder_server_source)
}

shinyApp(ui, server, onStart = builder_app_start)
