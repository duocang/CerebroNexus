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

library(shiny)

`%||%` <- function(a, b) if (is.null(a)) b else a

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
source("adapters.R", local = TRUE)
source("preview.R", local = TRUE)
source("stats.R", local = TRUE)
source("extras.R", local = TRUE)
source("analysis.R", local = TRUE)
source("build.R", local = TRUE)
source("prerequisite.R", local = TRUE)
source("state.R", local = TRUE)
source("loading.R", local = TRUE)
source(file.path("ui", "dataset_rail.R"), local = TRUE)
source("plan.R", local = TRUE)
source(file.path("ui", "inspect_stage.R"), local = TRUE)
source(file.path("ui", "core_stage.R"), local = TRUE)
source(file.path("ui", "enhance_stage.R"), local = TRUE)
source(file.path("ui", "review_stage.R"), local = TRUE)
source(file.path("ui", "build_status.R"), local = TRUE)
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

builder_preview_revision_independent <- function(kind) {
  kind %in% c("projection_previews", "trajectory_previews")
}

app_capability <- builder_app_capability()

## Inline icons: an icon set would be another dependency, and emoji are not
## icons.
icon_svg <- function(path, label = NULL) {
  tags$svg(
    class = "icon",
    viewBox = "0 0 24 24",
    fill = "none",
    stroke = "currentColor",
    `stroke-width` = "2",
    `stroke-linecap` = "round",
    `stroke-linejoin` = "round",
    `aria-hidden` = if (is.null(label)) "true" else NULL,
    role = if (is.null(label)) NULL else "img",
    if (!is.null(label)) tags$title(label),
    tags$path(d = path)
  )
}
ICON_PLUS <- "M12 5v14M5 12h14"
ICON_TRASH <- "M3 6h18M8 6V4h8v2M19 6l-1 14H6L5 6"

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

builder_example_buttons_ui <- function(examples = builder_example_directory()) {
  div(
    class = "builder-example-directory",
    lapply(examples, function(ex) {
      tags$button(
        class = "btn example-btn",
        type = "button",
        `data-ex` = ex$id,
        `data-label` = ex$label,
        `aria-disabled` = "false",
        tags$span(
          class = "ex-inner",
          tags$span(class = "ex-label", ex$label),
          tags$span(class = "ex-detail", ex$detail)
        )
      )
    })
  )
}

builder_protocol_is_quiescent <- function(protocol) {
  .builder_protocol_assert(protocol)
  is.null(protocol$pending) &&
    !length(protocol$queue) &&
    !length(protocol$awaiting_ack) &&
    identical(protocol$build_status, "idle")
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
  target <- release$handle$target
  if (identical(value$state, "success") && isTRUE(value$publishable)) {
    published <- try(.publish(release$handle, value), silent = TRUE)
    if (!inherits(published, "try-error")) {
      return(builder_as_result(published))
    }
    publication_error <- conditionMessage(attr(published, "condition"))
    try(.abort(release$handle), silent = TRUE)
    return(.release_error(publication_error, target))
  }
  cleaned <- try(.abort(release$handle), silent = TRUE)
  if (inherits(cleaned, "try-error") || !isTRUE(cleaned$aborted)) {
    cleanup_error <- if (inherits(cleaned, "try-error")) {
      conditionMessage(attr(cleaned, "condition"))
    } else {
      "The assigned build stage could not be cleaned."
    }
    return(.release_error(cleanup_error, target))
  }
  typed <- try(builder_as_result(value), silent = TRUE)
  if (inherits(typed, "try-error")) {
    return(builder_result_failure(
      "The worker returned an unsupported terminal build result."
    ))
  }
  typed
}

ui <- tagList(
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = paste0("builder.css", asset_stamp("www/builder.css"))
    ),
    tags$script(src = paste0("icons.js", asset_stamp("www/icons.js"))),
    tags$script(src = paste0("stats.js", asset_stamp("www/stats.js"))),
    tags$script(src = paste0("builder.js", asset_stamp("www/builder.js"))),
    tags$script(HTML(
      paste0(
        "Shiny.addCustomMessageHandler('builder_copy_text', function(message) { navigator.clipboard.writeText(message.text); });",
        "Shiny.addCustomMessageHandler('builder_click', function(message) { var el = document.getElementById(message.id); if (el && !el.disabled) el.click(); });"
      )
    )),
    tags$title("Cerebro Dataset Builder")
  ),
  div(
    class = "topbar",
    div(class = "wordmark", cerebro_wordmark),
    div(class = "divider"),
    h1("Dataset Builder"),
    span(
      class = "hint",
      style = "font-size:.82rem;margin:0",
      "Turn Seurat objects into a ready-to-run visual app"
    ),
    uiOutput("busy", inline = TRUE),
    span(class = "formats", textOutput("format_line", inline = TRUE))
  ),
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
        div(
          id = "ds_ready_list",
          class = "shiny-html-output",
          builder_dataset_rail_ui(builder_state())
        ),
        div(
          id = "ds_import_list",
          class = "shiny-html-output"
        )
      ),
      div(
        class = "rail-add",
        div(
          class = "dataset-file-control builder-file-picker builder-file-picker--sidebar",
          tags$input(
            id = "dataset_files",
            name = "dataset_files",
            class = "shiny-input-file dataset-file-input builder-file-input",
            type = "file",
            multiple = "multiple",
            accept = paste(
              paste0(
                ".",
                unique(unlist(lapply(builder_formats, `[[`, "extensions")))
              ),
              collapse = ","
            ),
            `tabindex` = "-1"
          ),
          tags$label(
            `for` = "dataset_files",
            class = "dataset-file-button builder-file-trigger",
            `tabindex` = "0",
            role = "button",
            icon_svg(ICON_PLUS),
            span("Add datasets…")
          )
        ),
        div(class = "or", "or try an example"),
        builder_example_buttons_ui(),
        uiOutput("add_error")
      )
    ),
    div(
      id = "pane",
      class = "builder-content",
      div(
        id = "workbench",
        class = "shiny-html-output",
        builder_empty_workbench_ui()
      ),
      uiOutput("result_card")
    )
  ),
  uiOutput("actionbar"),
  div(
    id = "builder-live-status",
    class = "visually-hidden",
    role = "status",
    `aria-live` = "polite",
    `aria-atomic` = "true"
  ),
  div(
    class = "builder-first-run",
    `data-first-run` = "true",
    role = "region",
    `aria-label` = "Getting started",
    h2("Build your first Viewer in three steps"),
    tags$ol(
      tags$li("Add a dataset or choose a bundled example."),
      tags$li("Check what was found and adjust the defaults."),
      tags$li("Review the exact output, then build.")
    ),
    tags$button(
      type = "button",
      class = "btn btn-primary builder-first-run-dismiss",
      "Got it"
    )
  )
)

server <- function(input, output, session) {
  ## Each entry: id, path, format, object, profile, settings.
  ## `settings` is what the user chose; it is written back whenever an input
  ## changes so switching between data sets does not lose it.
  store <- reactiveVal(builder_state())
  imports <- reactiveVal(builder_import_queue(max_active = 1L))
  active_import_id <- reactiveVal(NULL)
  example_directory_sent <- reactiveVal(NULL)
  current_id <- reactiveVal(NULL)
  update_current_id <- function(value) {
    if (!identical(value, isolate(current_id()))) {
      current_id(value)
    }
    invisible(value)
  }
  observe({
    update_current_id(store()$current_dataset)
  })
  app_store_compat_entries <- function(state, datasets, mark = FALSE) {
    ids <- vapply(
      datasets,
      function(entry) {
        stopifnot(
          is.list(entry),
          builder_has_text(entry$id),
          is.list(entry$settings)
        )
        entry$id
      },
      character(1)
    )
    stopifnot(!anyDuplicated(ids))
    state$datasets <- datasets
    if (is.null(state$current_dataset) || !state$current_dataset %in% ids) {
      state["current_dataset"] <- list(if (length(ids)) ids[[1L]] else NULL)
    }
    state$revision <- as.integer(state$revision %||% 0L) + 1L
    if (isTRUE(mark)) {
      state$.state_only_fixture <- TRUE
    }
    structure(state, class = c("builder_state", "list"))
  }
  use_state_only_fixture <- function(datasets = list()) {
    fixture <- app_store_compat_entries(builder_state(), datasets, mark = TRUE)
    store(fixture)
    invisible(fixture)
  }
  sets <- function(value) {
    if (missing(value)) {
      return(store()$datasets)
    }
    current_state <- isolate(store())
    updated <- try(
      builder_reduce_state(
        current_state,
        list(type = "replace_all", datasets = value)
      ),
      silent = TRUE
    )
    if (inherits(updated, "try-error")) {
      if (!isTRUE(current_state$.state_only_fixture)) {
        stop(attr(updated, "condition"))
      }
      ## Explicit state-only fixtures may carry the legacy minimal records used
      ## by UI tests. The fixture mark must exist before this setter is called.
      updated <- app_store_compat_entries(
        current_state,
        value
      )
    }
    store(updated)
    invisible(value)
  }
  current <- function(value) {
    if (missing(value)) {
      return(current_id())
    }
    if (is.null(value)) {
      return(invisible(NULL))
    }
    updated <- builder_reduce_state(
      isolate(store()),
      list(type = "select", id = value)
    )
    store(updated)
    active_import_id(NULL)
    update_current_id(updated$current_dataset)
    invisible(value)
  }
  result <- reactiveVal(NULL)
  build_flow <- reactiveVal(list(stage = "idle", plan = NULL))
  review_options <- reactiveVal(builder_review_options())
  review_validation <- reactiveVal(list(ok = TRUE, error = NULL))
  enhance_contract <- reactiveVal(list(
    id = NULL,
    organism = NULL,
    analyses = character()
  ))
  seq_id <- reactiveVal(0L)
  add_error <- reactiveVal(NULL)
  preview_frame <- reactiveVal(NULL)
  projection_previews <- reactiveVal(list(dataset = NULL, frames = list()))
  trajectory_previews <- reactiveVal(list(dataset = NULL, frames = list()))
  projection_preview_contract <- reactiveVal(NULL)
  trajectory_preview_contract <- reactiveVal(NULL)
  spatial_coords <- reactiveVal(NULL)
  alignment_preview <- reactiveVal(NULL)

  entry_of <- function(id) {
    all <- sets()
    hit <- Filter(function(e) identical(e$id, id), all)
    if (length(hit)) hit[[1]] else NULL
  }

  observe({
    state <- store()
    id <- state$current_dataset
    entries <- state$datasets %||% list()
    index <- which(vapply(
      entries,
      function(entry) identical(entry$id, id),
      logical(1)
    ))
    entry <- if (length(index) == 1L) entries[[index]] else NULL
    next_contract <- list(
      id = id,
      organism = entry$settings$organism %||% NULL,
      analyses = unname(entry$settings$analyses %||% character())
    )
    if (!identical(next_contract, isolate(enhance_contract()))) {
      enhance_contract(next_contract)
    }
  })

  ## Names end up as the app's dataset switcher labels, so duplicates are not
  ## cosmetic: they block the build. Loading the same example twice is an
  ## obvious thing to do, so suffix rather than refuse.
  unique_name <- function(label) {
    taken <- vapply(sets(), function(e) e$settings$name, character(1))
    if (!label %in% taken) {
      return(label)
    }
    n <- 2L
    while (paste0(label, " ", n) %in% taken) {
      n <- n + 1L
    }
    paste0(label, " ", n)
  }

  replace_entry <- function(updated) {
    all <- isolate(sets())
    index <- which(vapply(
      all,
      function(entry) identical(entry$id, updated$id),
      logical(1)
    ))
    if (length(index) != 1L) {
      return(invisible(FALSE))
    }
    existing <- all[[index]]
    if (identical(existing$settings, updated$settings)) {
      return(invisible(FALSE))
    }
    existing$settings <- updated$settings
    existing$revision <- as.integer(existing$revision %||% 0L) + 1L
    current_state <- isolate(store())
    updated_state <- try(
      builder_reduce_state(
        current_state,
        list(type = "replace", id = existing$id, entry = existing)
      ),
      silent = TRUE
    )
    if (inherits(updated_state, "try-error")) {
      if (!isTRUE(current_state$.state_only_fixture)) {
        stop(attr(updated_state, "condition"))
      }
      all[[index]] <- existing
      updated_state <- app_store_compat_entries(current_state, all)
    }
    store(updated_state)
    current_protocol <- protocol()
    if (!is.null(current_protocol)) {
      protocol(builder_protocol_dataset(
        current_protocol,
        existing$id,
        existing$revision,
        .builder_worker_identity(existing$snapshot)
      ))
    }
    invisible(TRUE)
  }

  output$format_line <- renderText(builder_format_summary())
  output$ds_count <- renderText({
    n <- length(store()$datasets) + length(imports()$entries)
    if (n == 0) "" else paste0(n)
  })
  output$rail_undo <- renderUI({
    if (!isTRUE(store()$can_undo_remove)) {
      return(NULL)
    }
    actionLink("undo_remove", "Undo remove")
  })

  ## -- the worker process ---------------------------------------------------
  ## Objects live there, never here. The protocol owns queue order, request
  ## identity and acknowledgement barriers; the poller only applies a result
  ## after that identity has been validated.
  worker <- reactiveVal(NULL)
  worker_available <- reactiveVal(FALSE)
  protocol <- reactiveVal(NULL)
  build_state <- reactiveVal(builder_build_state())
  active_release <- reactiveVal(NULL)
  request_sequence <- reactiveVal(0L)
  pending_snapshot_drops <- reactiveVal(list())
  pending_sources <- reactiveVal(character())
  pending_uploads <- reactiveVal(list())
  cancelled_loads <- reactiveVal(character())
  import_of <- function(id) {
    builder_import_find(imports(), id)
  }
  set_import_state <- function(id, state, generation, error = NULL) {
    updated <- try(
      builder_import_transition(
        isolate(imports()),
        id,
        state,
        generation,
        error = error
      ),
      silent = TRUE
    )
    if (inherits(updated, "try-error")) {
      return(invisible(FALSE))
    }
    imports(updated)
    invisible(TRUE)
  }
  forget_import <- function(id) {
    current_imports <- isolate(imports())
    if (is.null(builder_import_find(current_imports, id))) {
      return(invisible(FALSE))
    }
    imports(builder_import_remove(current_imports, id))
    if (identical(isolate(active_import_id()), id)) {
      active_import_id(NULL)
    }
    invisible(TRUE)
  }
  release_pending_source <- function(payload, drop_import = TRUE) {
    if (!is.list(payload) || !identical(payload$kind, "load")) {
      return(invisible(FALSE))
    }
    value <- if (identical(payload$source, "file")) {
      payload$path
    } else {
      payload$example
    }
    key <- builder_source_key(payload$source, value)
    pending_sources(builder_source_release(pending_sources(), key))
    id <- as.character(payload$id %||% character())
    if (length(id) == 1L && !is.na(id) && nzchar(id)) {
      if (isTRUE(drop_import)) {
        forget_import(id)
      }
      builder_import_progress_remove(payload$progress_path %||% "")
      uploads <- pending_uploads()
      uploads[[id]] <- NULL
      pending_uploads(uploads)
      cancelled_loads(setdiff(cancelled_loads(), id))
    }
    invisible(TRUE)
  }
  enqueue <- function(req) {
    current_protocol <- protocol()
    if (is.null(current_protocol) || !isTRUE(worker_available())) {
      add_error("The background worker is not ready yet.")
      return(invisible(FALSE))
    }
    dataset <- req$id %||% "session"
    entry <- entry_of(dataset)
    revision <- if (!is.null(req$dataset_revision)) {
      as.integer(req$dataset_revision)
    } else if (is.null(entry)) {
      0L
    } else {
      as.integer(entry$revision %||% 0L)
    }
    snapshot_identity <- req$snapshot_identity %||%
      if (is.null(entry)) {
        NULL
      } else {
        .builder_worker_identity(entry$snapshot)
      }
    if (!is.null(entry)) {
      current_protocol <- builder_protocol_dataset(
        current_protocol,
        dataset,
        revision,
        snapshot_identity
      )
    }
    replaceable <- req$kind %in%
      c(
        "preview",
        "projection_previews",
        "trajectory_previews",
        "coords",
        "spatial_preview"
      )
    if (replaceable) {
      request_sequence(request_sequence() + 1L)
      if (builder_preview_revision_independent(req$kind)) {
        req$revision_independent <- TRUE
      }
      request <- builder_query(
        kind = req$kind,
        dataset = dataset,
        generation = request_sequence(),
        slot = req$replaces %||% req$kind,
        payload = req,
        revision = revision,
        snapshot_identity = snapshot_identity
      )
    } else {
      if (identical(req$kind, "build") && is.null(req$id)) {
        request_sequence(request_sequence() + 1L)
        req$id <- paste0("build-", request_sequence())
      }
      request <- builder_command(
        kind = req$kind,
        dataset = dataset,
        payload = req,
        revision = revision,
        snapshot_identity = snapshot_identity
      )
    }
    queued <- try(builder_enqueue(current_protocol, request), silent = TRUE)
    if (inherits(queued, "try-error")) {
      add_error(conditionMessage(attr(queued, "condition")))
      return(invisible(FALSE))
    }
    protocol(queued)
    invisible(TRUE)
  }
  busy_note <- reactiveVal(NULL)

  update_build_state <- function(action) {
    updated <- try(builder_reduce_build(build_state(), action), silent = TRUE)
    if (inherits(updated, "try-error")) {
      add_error(conditionMessage(attr(updated, "condition")))
      return(invisible(FALSE))
    }
    build_state(updated)
    invisible(TRUE)
  }

  abort_release_result <- function(release, reason) {
    if (is.null(release)) {
      return(builder_result_failure(reason))
    }
    try(builder_coordinator_abort(release$handle), silent = TRUE)
    builder_release_error_result(reason, release$handle$target)
  }

  settle_failed_builds <- function(recovery, reason) {
    failed <- Filter(
      function(request) identical(request$kind, "build"),
      recovery$failed %||% list()
    )
    for (request in failed) {
      release <- isolate(active_release())
      release_result <- builder_result_failure(reason)
      if (!is.null(release) && identical(release$id, request$build_id)) {
        release_result <- abort_release_result(release, reason)
        active_release(NULL)
      }
      state <- build_state()
      build_id <- request$build_id
      if (is.null(build_id) || !nzchar(build_id)) {
        next
      }
      if (
        !state$status %in% c("running", "cancelling") ||
          !identical(state$id, build_id)
      ) {
        started <- try(
          builder_reduce_build(
            state,
            list(type = "start", id = build_id, revision = 0L)
          ),
          silent = TRUE
        )
        if (inherits(started, "try-error")) {
          next
        }
        build_state(started)
        state <- started
      }
      action <- if (identical(state$status, "cancelling")) {
        list(type = "cancelled", id = build_id)
      } else {
        list(type = "fail", id = build_id, error = reason)
      }
      update_build_state(action)
      result(release_result)
    }
  }

  apply_protocol_recovery <- function(
    current_protocol,
    recovered_worker,
    reason,
    retry_persistent,
    error = NULL
  ) {
    recovered <- try(
      builder_protocol_recover(
        current_protocol,
        epoch = if (isTRUE(retry_persistent)) {
          recovered_worker$epoch
        } else {
          .builder_worker_epoch()
        },
        reason = reason,
        retry_persistent = retry_persistent
      ),
      silent = TRUE
    )
    busy_note(NULL)
    if (inherits(recovered, "try-error")) {
      protocol(NULL)
      worker_available(FALSE)
      add_error(paste0(
        "The worker protocol could not be recovered. Restart this Builder session. ",
        conditionMessage(attr(recovered, "condition"))
      ))
      return(invisible(FALSE))
    }
    worker(recovered_worker)
    worker_available(isTRUE(retry_persistent))
    protocol(recovered$protocol)
    settle_failed_builds(recovered, reason)
    failed_requests <- recovered$failed %||% list()
    invisible(lapply(failed_requests, function(request) {
      payload <- request$payload
      if (identical(payload$kind, "load")) {
        entry <- isolate(import_of(payload$id))
        if (!is.null(entry)) {
          set_import_state(
            payload$id,
            "error",
            payload$import_generation %||% entry$generation,
            reason
          )
          release_pending_source(payload, drop_import = FALSE)
          return(invisible(TRUE))
        }
      }
      release_pending_source(payload)
    }))
    retried_builds <- Filter(
      function(request) identical(request$kind, "build"),
      recovered$retried %||% list()
    )
    if (length(retried_builds)) {
      release <- isolate(active_release())
      if (!is.null(release)) {
        try(builder_coordinator_abort(release$handle), silent = TRUE)
        active_release(NULL)
      }
    }

    if (!is.null(error)) {
      has_failed_load <- any(vapply(
        failed_requests,
        function(request) identical(request$payload$kind, "load"),
        logical(1)
      ))
      public_error <- if (has_failed_load) {
        builder_import_public_error(error)
      } else {
        error
      }
      add_error(paste0(
        public_error,
        " No queued action was left pending; restart this Builder session."
      ))
      return(invisible(FALSE))
    }
    retried <- length(recovered$retried %||% list())
    failed <- length(recovered$failed %||% list())
    discarded <- length(recovered$discarded %||% list())
    detail <- c(
      if (retried) paste(retried, "persistent action(s) will resume"),
      if (failed) paste(failed, "action(s), including any Build, were stopped"),
      if (discarded) {
        paste(discarded, "obsolete preview request(s) were discarded")
      }
    )
    add_error(paste0(
      "The background worker restarted from immutable snapshots.",
      if (length(detail)) {
        paste0(" ", paste(detail, collapse = "; "), ".")
      } else {
        ""
      }
    ))
    invisible(TRUE)
  }

  restart_worker_protocol <- function(
    current_worker,
    current_protocol,
    reason
  ) {
    restarted <- try(builder_worker_restart(current_worker), silent = TRUE)
    typed_failure <- inherits(restarted, "builder_worker_restart_failed") ||
      (is.list(restarted) && identical(restarted$event, "restart_failed"))
    if (inherits(restarted, "try-error") || typed_failure) {
      failed_worker <- if (
        is.list(restarted) &&
          inherits(restarted$worker, "builder_worker")
      ) {
        restarted$worker
      } else {
        current_worker
      }
      restart_error <- if (inherits(restarted, "try-error")) {
        conditionMessage(attr(restarted, "condition"))
      } else {
        restarted$error %||% "The background worker could not restart."
      }
      return(apply_protocol_recovery(
        current_protocol,
        failed_worker,
        reason,
        retry_persistent = FALSE,
        error = restart_error
      ))
    }
    apply_protocol_recovery(
      current_protocol,
      restarted,
      reason,
      retry_persistent = TRUE
    )
  }

  observe({
    if (!is.null(worker())) {
      return()
    }
    started <- builder_session_start(getwd())
    if (!is.null(started$error)) {
      add_error(started$error)
      return()
    }
    worker(started$worker)
    worker_available(TRUE)
    protocol(builder_request_protocol(started$worker$epoch))
  })

  session$onSessionEnded(function() {
    current_worker <- isolate(worker())
    if (!is.null(current_worker)) {
      stopped <- try(
        builder_worker_stop(current_worker, grace_ms = 5000L),
        silent = TRUE
      )
      if (inherits(stopped, "try-error") || !isTRUE(stopped$stopped)) {
        return()
      }
      if (!isTRUE(stopped$worker$cleanup_safe)) {
        return()
      }
      current_worker <- stopped$worker
      released_all <- TRUE
      released_identities <- character()
      for (snapshot in current_worker$snapshot_registry) {
        identity <- .builder_worker_identity(snapshot)
        if (identity %in% released_identities) {
          next
        }
        released <- try(.builder_snapshot_release(snapshot), silent = TRUE)
        released_all <- released_all && isTRUE(released)
        if (isTRUE(released)) {
          released_identities <- c(released_identities, identity)
        }
      }
      builder_import_progress_cleanup(current_worker$snapshot_root)
      remaining <- list.files(
        current_worker$snapshot_root,
        all.files = TRUE,
        no.. = TRUE
      )
      if (
        isTRUE(current_worker$owns_root) &&
          released_all &&
          !length(remaining)
      ) {
        unlink(current_worker$snapshot_root, recursive = TRUE, force = TRUE)
      }
    }
    release <- isolate(active_release())
    if (!is.null(release)) {
      try(builder_coordinator_abort(release$handle), silent = TRUE)
      active_release(NULL)
    }
  })

  ## -- native file picker and examples --------------------------------------
  ## An example already on the list is not an offer any more. Which ones are
  ## taken is derived state, so it is pushed rather than re-rendered.
  observe({
    directory <- builder_example_directory_state(sets(), imports())
    if (identical(directory, isolate(example_directory_sent()))) {
      return()
    }
    example_directory_sent(directory)
    session$sendCustomMessage(
      "builder_used_examples",
      directory
    )
  })

  start_load <- function(kind, arg, label, file_meta = NULL) {
    rs <- worker()
    if (is.null(rs)) {
      add_error("The background worker is not ready yet.")
      return()
    }
    reservation <- builder_source_reserve(sets(), pending_sources(), kind, arg)
    if (!isTRUE(reservation$ok)) {
      return(invisible(FALSE))
    }
    pending_sources(reservation$pending)
    add_error(NULL)
    seq_id(seq_id() + 1L)
    id <- paste0("ds", seq_id())
    filename <- NULL
    file_type <- NULL
    file_size <- NA_real_
    if (identical(kind, "file") && is.list(file_meta)) {
      uploads <- pending_uploads()
      filename <- builder_safe_file_name(file_meta$name, paste0(label, ".rds"))
      file_type <- builder_file_type_label(filename, file_meta$type)
      file_size <- suppressWarnings(as.numeric(file_meta$size %||% NA_real_))
      uploads[[id]] <- list(
        id = id,
        filename = filename,
        type = file_type,
        size = file_size,
        visible = TRUE
      )
      pending_uploads(uploads)
    }
    generation <- 1L
    progress_path <- NULL
    snapshot_root <- rs$snapshot_root %||% NULL
    if (
      is.character(snapshot_root) &&
        length(snapshot_root) == 1L &&
        !is.na(snapshot_root) &&
        dir.exists(snapshot_root)
    ) {
      candidate <- try(
        builder_import_progress_path(snapshot_root, id, generation),
        silent = TRUE
      )
      if (!inherits(candidate, "try-error")) {
        progress_path <- candidate
      }
    }
    source_descriptor <- list(
      kind = kind,
      staged_path = if (identical(kind, "file")) arg else NULL,
      example = if (identical(kind, "example")) arg else NULL,
      reservation_key = reservation$key,
      fingerprint = if (identical(kind, "example")) {
        paste0("example:", arg, ":builder-profile-v1")
      } else {
        info <- suppressWarnings(file.info(arg))
        paste(
          suppressWarnings(as.numeric(info$size[[1L]] %||% file_size)),
          suppressWarnings(as.numeric(info$mtime[[1L]] %||% NA_real_)),
          sep = ":"
        )
      }
    )
    pending_entry <- builder_import_entry(
      id = id,
      label = label,
      source = source_descriptor,
      filename = filename,
      file_type = file_type,
      size = file_size,
      generation = generation
    )
    imports(builder_import_add(isolate(imports()), pending_entry))
    active_import_id(id)
    queued <- enqueue(list(
      kind = "load",
      source = kind,
      id = id,
      path = if (identical(kind, "file")) arg else NA_character_,
      example = if (identical(kind, "example")) arg else NULL,
      label = label,
      import_generation = generation,
      progress_path = progress_path,
      note = paste0("Loading ", label, "…")
    ))
    if (!isTRUE(queued)) {
      pending_sources(builder_source_release(
        pending_sources(),
        reservation$key
      ))
      uploads <- pending_uploads()
      uploads[[id]] <- NULL
      pending_uploads(uploads)
      forget_import(id)
      builder_import_progress_remove(progress_path %||% "")
    }
    invisible(isTRUE(queued))
  }

  observeEvent(input$dataset_files, {
    uploads <- input$dataset_files
    if (
      !is.data.frame(uploads) ||
        !all(c("name", "datapath") %in% names(uploads)) ||
        !nrow(uploads)
    ) {
      return()
    }
    paths <- as.character(uploads$datapath)
    labels <- as.character(uploads$name)
    sizes <- if ("size" %in% names(uploads)) {
      suppressWarnings(as.numeric(uploads$size))
    } else {
      rep(NA_real_, nrow(uploads))
    }
    types <- if ("type" %in% names(uploads)) {
      as.character(uploads$type)
    } else {
      rep("", nrow(uploads))
    }
    valid <- !is.na(paths) & nzchar(paths) & !is.na(labels) & nzchar(labels)
    paths <- paths[valid]
    labels <- labels[valid]
    sizes <- sizes[valid]
    types <- types[valid]
    duplicate <- duplicated(paths) |
      paths %in%
        vapply(
          sets(),
          function(entry) entry$path,
          character(1)
        )
    for (i in which(!duplicate)) {
      start_load(
        "file",
        paths[[i]],
        tools::file_path_sans_ext(basename(labels[[i]])),
        file_meta = list(
          name = labels[[i]],
          type = types[[i]],
          size = sizes[[i]]
        )
      )
    }
    if (any(duplicate)) {
      add_error(paste0(
        sum(duplicate),
        if (sum(duplicate) == 1L) " file has" else " files have",
        " already been added."
      ))
    }
  })

  observeEvent(input$use_example, {
    used <- as.character(unlist(Filter(
      Negate(is.null),
      lapply(sets(), function(entry) entry$example)
    )))
    if (input$use_example %in% used) {
      return()
    }
    ex <- Filter(
      function(e) identical(e$id, input$use_example),
      builder_examples()
    )
    if (!length(ex)) {
      return()
    }
    start_load("example", ex[[1]]$id, ex[[1]]$label)
  })

  remove_pending_import <- function(id) {
    entry <- isolate(import_of(id))
    if (
      !is.character(id) ||
        length(id) != 1L ||
        is.na(id) ||
        !nzchar(id) ||
        is.null(entry)
    ) {
      return(invisible(FALSE))
    }
    current_protocol <- isolate(protocol())
    if (is.null(current_protocol)) {
      return(invisible(FALSE))
    }
    belongs <- function(request) {
      !is.null(request) &&
        identical(request$dataset, id) &&
        identical(request$kind, "load")
    }
    running <- belongs(current_protocol$pending) ||
      any(vapply(current_protocol$awaiting_ack, belongs, logical(1)))
    if (running) {
      uploads <- isolate(pending_uploads())
      if (!is.null(uploads[[id]])) {
        uploads[[id]]$visible <- FALSE
        pending_uploads(uploads)
      }
      cancelled_loads(unique(c(cancelled_loads(), id)))
      forget_import(id)
      return(invisible(TRUE))
    }
    if (identical(entry$load_state, "error")) {
      release_pending_source(list(
        kind = "load",
        source = entry$source$kind,
        id = entry$id,
        path = entry$source$staged_path,
        example = entry$source$example
      ))
      return(invisible(TRUE))
    }
    forgotten <- try(
      builder_protocol_forget_dataset(
        current_protocol,
        id,
        reason = "upload_cancelled"
      ),
      silent = TRUE
    )
    if (inherits(forgotten, "try-error")) {
      add_error("This file could not be removed while it was loading.")
      return(invisible(FALSE))
    }
    removed <- c(forgotten$failed, forgotten$discarded)
    if (!length(removed)) {
      return(invisible(FALSE))
    }
    protocol(forgotten$protocol)
    invisible(lapply(removed, function(request) {
      release_pending_source(request$payload)
    }))
    invisible(TRUE)
  }

  observeEvent(input$cancel_pending_upload, {
    event <- input$cancel_pending_upload
    id <- if (is.list(event) && !is.object(event)) {
      .subset2(event, "id")
    } else {
      NULL
    }
    remove_pending_import(id)
  })

  observeEvent(input$remove_import, {
    event <- input$remove_import
    id <- if (is.list(event) && !is.object(event)) {
      .subset2(event, "id")
    } else {
      NULL
    }
    remove_pending_import(id)
  })

  observeEvent(input$pick_import, {
    event <- input$pick_import
    id <- if (is.list(event) && !is.object(event)) {
      .subset2(event, "id")
    } else {
      event
    }
    if (
      is.character(id) &&
        length(id) == 1L &&
        !is.na(id) &&
        !is.null(isolate(import_of(id)))
    ) {
      active_import_id(id)
    }
  })

  observeEvent(input$retry_import, {
    event <- input$retry_import
    id <- if (is.list(event) && !is.object(event)) {
      .subset2(event, "id")
    } else {
      NULL
    }
    entry <- if (is.character(id) && length(id) == 1L) {
      isolate(import_of(id))
    } else {
      NULL
    }
    if (is.null(entry) || !identical(entry$load_state, "error")) {
      return()
    }
    next_queue <- builder_import_retry(isolate(imports()), id)
    entry <- builder_import_find(next_queue, id)
    current_worker <- isolate(worker())
    progress_path <- NULL
    if (
      is.list(current_worker) &&
        is.character(current_worker$snapshot_root) &&
        length(current_worker$snapshot_root) == 1L &&
        dir.exists(current_worker$snapshot_root)
    ) {
      candidate <- try(
        builder_import_progress_path(
          current_worker$snapshot_root,
          id,
          entry$generation
        ),
        silent = TRUE
      )
      if (!inherits(candidate, "try-error")) {
        progress_path <- candidate
      }
    }
    imports(next_queue)
    active_import_id(id)
    cancelled_loads(setdiff(cancelled_loads(), id))
    queued <- enqueue(list(
      kind = "load",
      source = entry$source$kind,
      id = entry$id,
      path = entry$source$staged_path,
      example = entry$source$example,
      label = entry$label,
      import_generation = entry$generation,
      progress_path = progress_path,
      note = paste0("Loading ", entry$label, "…")
    ))
    if (!isTRUE(queued)) {
      set_import_state(
        id,
        "error",
        entry$generation,
        "The background worker is not ready yet."
      )
    }
  })

  ## -- dispatcher: send the next request when the worker is free ----------
  observe({
    current_protocol <- protocol()
    if (is.null(current_protocol)) {
      return()
    }
    dispatched <- builder_protocol_dispatch(current_protocol)
    if (is.null(dispatched$request)) {
      return()
    }
    current_worker <- worker()
    req(current_worker)
    protocol(dispatched$protocol)
    request <- dispatched$request
    nxt <- request$payload
    if (identical(nxt$kind, "load")) {
      set_import_state(
        nxt$id,
        "reading",
        nxt$import_generation %||% 1L
      )
    }
    if (identical(nxt$kind, "build")) {
      # Legacy prohibition: never use `plan <- builder_make_plan` here.
      plan <- nxt$plan
      plan_error <- plan$error
      if (
        is.null(plan_error) &&
          length(plan$existing_targets) &&
          !isTRUE(plan$overwrite)
      ) {
        plan_error <- paste0(
          "These outputs already exist: ",
          paste(basename(plan$existing_targets), collapse = ", "),
          ". Choose another folder or replace the matching files."
        )
      }
      if (!is.null(plan_error)) {
        completed <- builder_protocol_complete(
          dispatched$protocol,
          builder_worker_response(
            request,
            list(error = plan_error)
          )
        )
        result(builder_result_failure(plan_error))
        protocol(builder_protocol_acknowledge(
          completed$protocol,
          request$request_id
        ))
        busy_note(NULL)
        return()
      }
      coordinator <- try(
        builder_coordinator_prepare(plan, request$build_id),
        silent = TRUE
      )
      if (inherits(coordinator, "try-error")) {
        plan_error <- conditionMessage(attr(coordinator, "condition"))
        completed <- builder_protocol_complete(
          dispatched$protocol,
          builder_worker_response(request, list(error = plan_error))
        )
        result(builder_release_error_result(
          plan_error,
          plan$output_release$directory
        ))
        protocol(builder_protocol_acknowledge(
          completed$protocol,
          request$request_id
        ))
        busy_note(NULL)
        return()
      }
      nxt$plan <- plan
      nxt$coordinator <- coordinator
      active_release(list(
        id = request$build_id,
        handle = coordinator,
        plan = plan
      ))
      update_build_state(list(
        type = "start",
        id = request$build_id,
        revision = plan$revision
      ))
    }
    started_call <- try(
      switch(
        nxt$kind,
        load = if (identical(nxt$source, "file")) {
          builder_session_load(
            current_worker,
            nxt$id,
            nxt$path,
            request,
            progress_path = nxt$progress_path,
            import_generation = nxt$import_generation %||% 1L
          )
        } else {
          builder_session_example(
            current_worker,
            nxt$id,
            nxt$example,
            request,
            progress_path = nxt$progress_path,
            import_generation = nxt$import_generation %||% 1L
          )
        },
        preview = builder_session_preview(
          current_worker,
          nxt$id,
          nxt$reduction,
          nxt$group,
          BUILDER_PREVIEW_MAX,
          request
        ),
        projection_previews = builder_session_projection_previews(
          current_worker,
          nxt$id,
          nxt$projections,
          nxt$group,
          nxt$max_cells %||% 600L,
          request
        ),
        trajectory_previews = builder_session_trajectory_previews(
          current_worker,
          nxt$id,
          nxt$trajectories,
          nxt$max_cells %||% 600L,
          request
        ),
        coords = builder_session_coords(
          current_worker,
          nxt$id,
          nxt$image,
          request
        ),
        spatial_preview = builder_session_spatial_preview(
          current_worker,
          nxt$id,
          nxt$default_projection,
          nxt$group,
          nxt$section,
          4000L,
          request
        ),
        align_all = builder_session_section_bounds(
          current_worker,
          nxt$id,
          nxt$sections,
          nxt$mode,
          nxt$extent_width,
          nxt$extent_height,
          nxt$um_per_px,
          nxt$dx,
          nxt$dy,
          nxt$scale,
          request
        ),
        build = builder_session_build(
          current_worker,
          nxt$plan,
          request,
          coordinator = nxt$coordinator
        ),
        drop = builder_session_drop(current_worker, nxt$id, request)
      ),
      silent = TRUE
    )
    if (inherits(started_call, "try-error")) {
      if (identical(nxt$kind, "build")) {
        release <- isolate(active_release())
        if (!is.null(release)) {
          try(builder_coordinator_abort(release$handle), silent = TRUE)
          active_release(NULL)
        }
      }
      dispatch_error <- conditionMessage(attr(started_call, "condition"))
      if (identical(nxt$kind, "load")) {
        dispatch_error <- builder_import_public_error(
          dispatch_error,
          nxt$path %||% character()
        )
      }
      restart_worker_protocol(
        current_worker,
        dispatched$protocol,
        dispatch_error
      )
      return()
    }
    busy_note(nxt$note)
  })

  ## -- one poller drains whatever the worker was asked to do ---------------
  observe({
    current_protocol <- protocol()
    if (is.null(current_protocol) || is.null(current_protocol$pending)) {
      return()
    }
    current_worker <- worker()
    req(current_worker)
    invalidateLater(100, session)
    got <- try(builder_session_poll(current_worker), silent = TRUE)
    if (inherits(got, "try-error")) {
      poll_error <- conditionMessage(attr(got, "condition"))
      pending_payload <- current_protocol$pending$payload
      if (identical(pending_payload$kind, "load")) {
        poll_error <- builder_import_public_error(
          poll_error,
          pending_payload$path %||% character()
        )
      }
      restart_worker_protocol(
        current_worker,
        current_protocol,
        poll_error
      )
      return()
    }
    worker(got$worker)
    if (identical(got$event, "restarted")) {
      apply_protocol_recovery(
        current_protocol,
        got$worker,
        "The background worker stopped before returning its result.",
        retry_persistent = TRUE
      )
      return()
    }
    if (identical(got$event, "restart_failed")) {
      apply_protocol_recovery(
        current_protocol,
        got$worker,
        "The background worker stopped and could not be restored.",
        retry_persistent = FALSE,
        error = got$error %||% "The background worker could not restart."
      )
      return()
    }
    request <- current_protocol$pending
    p <- request$payload
    if (is.null(got$result)) {
      if (identical(p$kind, "load") && !is.null(p$progress_path)) {
        progress <- builder_import_progress_read(
          p$progress_path,
          p$import_generation %||% 1L
        )
        if (!is.null(progress)) {
          set_import_state(
            p$id,
            progress$stage,
            progress$generation
          )
        }
      }
      return()
    }
    if (!is.null(got$result$error)) {
      worker_error <- if (identical(p$kind, "load")) {
        builder_import_public_error(
          got$result$error,
          p$path %||% character()
        )
      } else {
        got$result$error
      }
      if (identical(request$kind, "build")) {
        release <- isolate(active_release())
        release_result <- builder_result_failure(worker_error)
        if (!is.null(release)) {
          release_result <- abort_release_result(release, worker_error)
          active_release(NULL)
        }
        result(release_result)
      } else {
        add_error(worker_error)
      }
      restart_worker_protocol(
        got$worker,
        current_protocol,
        worker_error
      )
      return()
    }
    completed <- builder_protocol_complete(
      current_protocol,
      got$result$value
    )
    if (!is.null(completed$protocol$pending)) {
      restart_worker_protocol(
        got$worker,
        current_protocol,
        "A worker response did not match the pending request."
      )
      return()
    }
    protocol(completed$protocol)
    busy_note(NULL)
    if (!isTRUE(completed$accepted)) {
      release_pending_source(p)
      if (isTRUE(request$persistent)) {
        protocol(builder_protocol_acknowledge(
          protocol(),
          request$request_id
        ))
        if (identical(request$kind, "drop")) {
          restart_worker_protocol(
            got$worker,
            protocol(),
            "A stale dataset release result was rejected."
          )
          return()
        }
        add_error(
          "A stale persistent worker result was discarded. Retry the action."
        )
      }
      return()
    }
    value <- completed$value
    if (!is.null(completed$error)) {
      cancelled <- identical(p$kind, "load") && p$id %in% cancelled_loads()
      if (identical(request$kind, "build")) {
        result(builder_result_failure(completed$error))
        update_build_state(list(
          type = "fail",
          id = request$build_id,
          error = completed$error
        ))
      } else if (identical(p$kind, "load") && !cancelled) {
        set_import_state(
          p$id,
          "error",
          p$import_generation %||% 1L,
          completed$error
        )
        builder_import_progress_remove(p$progress_path %||% "")
        add_error(NULL)
      } else if (!cancelled) {
        add_error(completed$error)
      }
      if (cancelled) {
        release_pending_source(p)
      }
      if (isTRUE(request$persistent)) {
        protocol(builder_protocol_acknowledge(
          protocol(),
          request$request_id
        ))
      }
      return()
    }
    if (identical(request$kind, "build") && isTRUE(request$persistent)) {
      on.exit(
        {
          current <- isolate(protocol())
          if (!is.null(current)) {
            acknowledged <- try(
              builder_app_acknowledge_build(current, request$request_id),
              silent = TRUE
            )
            if (inherits(acknowledged, "try-error")) {
              protocol(NULL)
              worker_available(FALSE)
              add_error(paste0(
                "The completed Build could not be acknowledged. ",
                "Restart this Builder session."
              ))
            } else {
              protocol(acknowledged)
            }
          }
        },
        add = TRUE
      )
    }

    if (identical(p$kind, "load")) {
      cancelled <- p$id %in% cancelled_loads()
      if (!is.null(value$error)) {
        if (!cancelled) {
          set_import_state(
            p$id,
            "error",
            p$import_generation %||% 1L,
            value$error
          )
          builder_import_progress_remove(p$progress_path %||% "")
          add_error(NULL)
        } else {
          release_pending_source(p)
        }
        protocol(builder_protocol_acknowledge(protocol(), request$request_id))
        return()
      }
      pending_entry <- isolate(import_of(p$id))
      if (
        is.null(pending_entry) ||
          !identical(
            pending_entry$generation,
            as.integer(p$import_generation %||% 1L)
          )
      ) {
        cancelled <- TRUE
      }
      if (cancelled) {
        updated_worker <- try(
          builder_worker_register_snapshot(got$worker, p$id, value$snapshot),
          silent = TRUE
        )
        if (inherits(updated_worker, "try-error")) {
          restart_worker_protocol(
            got$worker,
            protocol(),
            conditionMessage(attr(updated_worker, "condition"))
          )
          return()
        }
        worker(updated_worker)
        identity <- .builder_worker_identity(value$snapshot)
        accepted <- builder_protocol_dataset(protocol(), p$id, 1L, identity)
        acknowledged <- try(
          builder_protocol_acknowledge(accepted, request$request_id),
          silent = TRUE
        )
        if (inherits(acknowledged, "try-error")) {
          protocol(NULL)
          worker_available(FALSE)
          add_error(paste0(
            "The cancelled upload could not be released safely. ",
            "Restart this Builder session."
          ))
          return()
        }
        protocol(acknowledged)
        pending_drops <- pending_snapshot_drops()
        pending_drops[[p$id]] <- identity
        pending_snapshot_drops(pending_drops)
        release_pending_source(p)
        queued <- enqueue(list(
          kind = "drop",
          id = p$id,
          dataset_revision = 1L,
          snapshot_identity = identity,
          note = "Releasing cancelled upload…"
        ))
        if (!isTRUE(queued)) {
          pending_drops[[p$id]] <- NULL
          pending_snapshot_drops(pending_drops)
          add_error(
            "The cancelled upload will be released when this session closes."
          )
        }
        return()
      }
      profile <- value$profile
      set_import_state(
        p$id,
        "preparing",
        p$import_generation %||% 1L
      )
      entry <- list(
        id = p$id,
        source_id = p$id,
        output_id = p$id,
        selector_value = p$id,
        path = p$path,
        ## Which built-in example produced this, so removing it puts the
        ## example back on offer. NULL for anything read from a file.
        example = p$example,
        format = value$format,
        profile = profile,
        dataset_profile = value$dataset_profile,
        snapshot = value$snapshot,
        revision = 1L,
        reviewed_revision = NULL,
        ## Level names per grouping variable, in the order the exporter will
        ## produce them -- the keys a configured palette has to match.
        levels = value$levels %||% list(),
        settings = builder_default_settings(
          profile,
          unique_name(p$label),
          dataset_profile = value$dataset_profile
        )
      )
      updated_worker <- try(
        builder_worker_register_snapshot(
          got$worker,
          p$id,
          value$snapshot
        ),
        silent = TRUE
      )
      if (inherits(updated_worker, "try-error")) {
        restart_worker_protocol(
          got$worker,
          protocol(),
          conditionMessage(attr(updated_worker, "condition"))
        )
        return()
      }
      worker(updated_worker)
      next_state <- builder_reduce_state(
        isolate(store()),
        list(type = "add", entry = entry)
      )
      store(next_state)
      protocol(builder_protocol_dataset(
        protocol(),
        p$id,
        entry$revision,
        .builder_worker_identity(entry$snapshot)
      ))
      first_unreviewed <- builder_next_unreviewed(next_state$datasets)
      watched <- identical(isolate(active_import_id()), p$id)
      next_current <- builder_import_ready_target(
        watched = watched,
        current_id = isolate(current()),
        loaded_id = p$id,
        first_unreviewed = first_unreviewed
      )
      if (!identical(next_current, isolate(current()))) {
        current(next_current)
      }
      session$sendCustomMessage(
        "builder_import_status",
        list(text = paste0(entry$settings$name, " is ready."))
      )
      set_import_state(
        p$id,
        "ready",
        p$import_generation %||% 1L
      )
      release_pending_source(p)
      result(NULL)
    } else if (identical(p$kind, "preview")) {
      if (identical(current(), p$id)) {
        preview_frame(value)
      }
    } else if (identical(p$kind, "projection_previews")) {
      if (identical(current(), p$id)) {
        projection_previews(list(dataset = p$id, frames = value %||% list()))
      }
    } else if (identical(p$kind, "trajectory_previews")) {
      if (identical(current(), p$id)) {
        trajectory_previews(list(dataset = p$id, frames = value %||% list()))
      }
    } else if (identical(p$kind, "coords")) {
      if (
        identical(current(), p$id) &&
          identical(active_slice(), p$image)
      ) {
        spatial_coords(value)
      }
    } else if (identical(p$kind, "spatial_preview")) {
      if (
        identical(current(), p$id) &&
          identical(active_slice(), p$section)
      ) {
        alignment_preview(value)
        if (isTRUE(value$available)) {
          spatial_coords(list(
            x = value$spatial$x,
            y = value$spatial$y,
            sx = value$spatial$x,
            sy = value$spatial$y
          ))
        }
      }
    } else if (identical(p$kind, "align_all")) {
      apply_section_bounds(p$id, value, p$picture)
    } else if (identical(p$kind, "build")) {
      release <- isolate(active_release())
      if (
        is.null(release) ||
          !identical(release$id, request$build_id)
      ) {
        release <- NULL
      }
      value <- builder_app_settle_release(release, value)
      active_release(NULL)
      result(value)
      update_build_state(builder_app_build_action(value, request$build_id))
    } else if (identical(p$kind, "drop")) {
      active_state <- store()
      retained <- active_state$datasets
      if (is.list(active_state$last_removed)) {
        retained <- c(retained, list(active_state$last_removed$entry))
      }
      pending_drops <- pending_snapshot_drops()
      # The transition excludes other_drop_ids with a shared pending identity.
      released <- try(
        builder_snapshot_release_transition(
          worker = got$worker,
          id = p$id,
          identity = request$snapshot_identity,
          retained = retained,
          pending = pending_drops,
          release = function(worker, id, identity) {
            builder_worker_release_snapshot(
              worker,
              id,
              expected_identity = identity
            )
          },
          unregister = function(worker, id) {
            worker$snapshot_registry[[id]] <- NULL
            worker
          },
          identity_of = .builder_worker_identity
        ),
        silent = TRUE
      )
      if (inherits(released, "try-error")) {
        restart_worker_protocol(
          got$worker,
          protocol(),
          conditionMessage(attr(released, "condition"))
        )
        return()
      }
      worker(released$worker)
      pending_snapshot_drops(released$pending)
      all <- Filter(function(e) !identical(e$id, p$id), sets())
      sets(all)
      if (identical(current(), p$id)) {
        next_id <- builder_next_unreviewed(all)
        current(if (length(all)) next_id %||% all[[1]]$id else NULL)
        result(NULL)
      }
      acknowledged <- try(
        builder_protocol_acknowledge(protocol(), request$request_id),
        silent = TRUE
      )
      if (inherits(acknowledged, "try-error")) {
        protocol(NULL)
        worker_available(FALSE)
        add_error(paste0(
          "The dataset was removed, but its protocol acknowledgement failed. ",
          "Restart this Builder session."
        ))
        return()
      }
      forgotten <- try(
        builder_protocol_forget_dataset(acknowledged, p$id),
        silent = TRUE
      )
      if (inherits(forgotten, "try-error")) {
        protocol(NULL)
        worker_available(FALSE)
        add_error(paste0(
          "The dataset was removed, but its queued actions could not be ",
          "cleared. Restart this Builder session."
        ))
        return()
      }
      protocol(forgotten$protocol)
      cancelled <- length(forgotten$failed) + length(forgotten$discarded)
      if (cancelled) {
        showNotification(
          paste0(
            "Dataset removed; ",
            cancelled,
            " obsolete queued action",
            if (cancelled == 1L) " was" else "s were",
            " cancelled."
          ),
          type = "message",
          duration = 5
        )
      }
      return()
    }
    if (isTRUE(request$persistent) && !identical(request$kind, "build")) {
      protocol(builder_protocol_acknowledge(protocol(), request$request_id))
    }
  })

  update_enhance_histology_choices <- function(entry) {
    choices <- names(entry$settings$images %||% list()) %||% character()
    invisible(choices)
  }

  commit_enhance_images <- function(entry, images) {
    entry$settings$images <- images
    replace_entry(entry)
    update_enhance_histology_choices(entry)
    invisible(entry)
  }

  ## Take the per-section extents the worker computed and pair each with the
  ## one shared picture.
  apply_section_bounds <- function(id, per_section, a) {
    if (is.null(a) || !length(per_section)) {
      return()
    }
    e <- isolate(entry_of(id))
    if (is.null(e)) {
      return()
    }
    paired <- builder_pair_sections(a, per_section)
    imgs <- utils::modifyList(e$settings$images %||% list(), paired)
    short <- names(Filter(function(x) x$outside > 0, paired))
    commit_enhance_images(e, imgs)

    ## Saying "done" when four of five slides have every cell off the image is
    ## how the earlier version of this hid its own bug.
    if (length(short)) {
      showNotification(
        paste0(
          "Applied to ",
          length(per_section),
          " sections, but cells still fall outside the image in: ",
          paste(short, collapse = ", "),
          ". Bounding-box mode usually works best for multi-section objects."
        ),
        type = "warning",
        duration = 10
      )
    } else {
      showNotification(
        paste0(
          "The image was fitted to the coordinates of all ",
          length(per_section),
          " sections."
        ),
        type = "message",
        duration = 5
      )
    }
  }

  output$add_error <- renderUI({
    msg <- add_error()
    if (is.null(msg)) {
      return(NULL)
    }
    div(class = "notice bad", msg)
  })

  ## -- the rail ------------------------------------------------------------
  output$ds_ready_list <- renderUI({
    builder_dataset_rail_ui(store(), current())
  })

  output$ds_import_list <- renderUI({
    builder_import_rail_ui(imports()$entries, active_import_id())
  })

  ## -- keep the current entry's settings in step with Core -----------------
  core_setting_inputs <- c(
    name = "core-name",
    organism = "core-organism",
    assay = "core-assay",
    layer = "core-layer",
    nUMI = "core-nUMI",
    nGene = "core-nGene",
    expression_backend = "core-backend"
  )
  observeEvent(
    input[["core-assay"]],
    {
      id <- current()
      if (
        is.null(id) ||
          !identical(input[["core-rendered_for"]], id)
      ) {
        return()
      }
      e <- isolate(entry_of(id))
      req(e)
      controls <- builder_core_assay_controls(
        e$profile,
        e$settings,
        input[["core-assay"]]
      )
      for (field in names(controls)) {
        updateSelectInput(
          session,
          paste0("core-", field),
          choices = controls[[field]]$choices,
          selected = controls[[field]]$selected
        )
      }
    },
    ignoreInit = TRUE
  )
  observe({
    id <- current()
    rendered_for <- input[["core-rendered_for"]]
    if (is.null(id) || !identical(rendered_for, id)) {
      return()
    }
    values <- lapply(core_setting_inputs, function(input_id) input[[input_id]])
    if (any(vapply(values, is.null, logical(1)))) {
      return()
    }
    entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
    req(entry)
    next_settings <- entry$settings
    for (setting in names(core_setting_inputs)) {
      next_settings[[setting]] <- values[[setting]]
    }
    assay_controls <- builder_core_assay_controls(
      entry$profile,
      next_settings,
      next_settings$assay
    )
    for (field in names(assay_controls)) {
      next_settings[[field]] <- assay_controls[[field]]$selected
    }
    if (!next_settings$organism %in% c("hg", "mm")) {
      next_settings$analyses <- setdiff(
        next_settings$analyses %||% character(),
        "percent_mt_ribo"
      )
    }
    entry$settings <- next_settings
    replace_entry(entry)
  })

  group_catalog_for_entry <- function(entry) {
    builder_group_catalog_model(list(
      metadata_catalog = entry$dataset_profile$viewer_content$metadata %||%
        entry$profile$viewer_content$metadata %||%
        list(),
      group_choices = unname(entry$profile$group_candidates %||% character()),
      included_groups = entry$settings$included_groups %||%
        entry$settings$groups %||%
        character(),
      default_group = entry$settings$default_group %||% NULL,
      suggested_groups = entry$profile$group_preselect %||%
        entry$settings$included_groups %||%
        character(),
      metadata_policy = entry$settings$metadata_policy %||%
        entry$settings$recommendations$metadata %||%
        list(),
      levels = entry$levels %||% list()
    ))
  }

  send_group_state <- function(entry, message = NULL) {
    session$sendCustomMessage(
      "builder_group_state",
      list(
        dataset = entry$id,
        included = unname(entry$settings$included_groups %||% character()),
        default = entry$settings$default_group %||% NULL,
        message = message
      )
    )
  }

  group_focus_value <- function(value) {
    if (is.list(value)) value$group %||% NULL else value
  }

  projection_catalog_for_entry <- function(entry) {
    catalog <- entry$dataset_profile$viewer_content$projections %||%
      entry$profile$viewer_content$projections %||%
      list()
    if (is.list(catalog) && length(catalog)) {
      return(catalog)
    }
    ids <- unname(as.character(entry$profile$reductions %||% character()))
    fallback <- lapply(ids, function(id) {
      list(
        id = id,
        name = id,
        kind = if (grepl("pca", id, ignore.case = TRUE)) "pca" else "other",
        dimensions = 2L,
        cell_count = entry$profile$n_cells %||% 0L,
        available = TRUE
      )
    })
    names(fallback) <- ids
    fallback
  }

  trajectory_catalog_for_entry <- function(entry) {
    entry$dataset_profile$viewer_content$trajectories %||%
      entry$profile$viewer_content$trajectories %||%
      list()
  }

  selectable_trajectory_selection <- function(catalog) {
    selected <- list()
    for (record in catalog %||% list()) {
      if (
        !is.list(record) ||
          !isTRUE(record$selectable) ||
          !builder_stage_has_text(record$method %||% "") ||
          !builder_stage_has_text(record$name %||% "")
      ) {
        next
      }
      selected[[record$method]] <- unique(c(
        selected[[record$method]],
        record$name
      ))
    }
    selected
  }

  send_projection_state <- function(entry, message = NULL) {
    session$sendCustomMessage(
      "builder_projection_state",
      list(
        dataset = entry$id,
        included = unname(
          entry$settings$included_projections %||% character()
        ),
        default = entry$settings$default_projection %||% NULL,
        point_size = entry$settings$overview_point_size %||% 5,
        message = message
      )
    )
  }

  send_trajectory_state <- function(entry, message = NULL) {
    included <- list()
    for (method in names(entry$settings$included_trajectories %||% list())) {
      for (name in entry$settings$included_trajectories[[method]]) {
        included[[length(included) + 1L]] <- list(
          method = method,
          name = name
        )
      }
    }
    session$sendCustomMessage(
      "builder_trajectory_state",
      list(
        dataset = entry$id,
        included = included,
        default = entry$settings$default_trajectory %||% NULL,
        message = message
      )
    )
  }

  observeEvent(
    input[["core-projection_action"]],
    {
      id <- current()
      action <- input[["core-projection_action"]]
      if (
        is.null(id) ||
          !identical(input[["core-rendered_for"]], id) ||
          !is.list(action) ||
          !identical(action$action, "set")
      ) {
        return()
      }
      entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
      req(entry)
      catalog <- projection_catalog_for_entry(entry)
      available <- names(catalog)[vapply(
        catalog,
        function(item) is.list(item) && isTRUE(item$available),
        logical(1)
      )]
      included <- unique(as.character(unlist(
        action$included %||% character(),
        use.names = FALSE
      )))
      included <- available[available %in% included]
      if (!length(included)) {
        send_projection_state(
          entry,
          "Keep at least one projection selected."
        )
        return()
      }
      default <- action$default
      if (!builder_stage_has_text(default %||% "") || !default %in% included) {
        default <- included[[1L]]
      }
      entry$settings$included_projections <- included
      entry$settings$reductions <- included
      entry$settings$default_projection <- default
      if (isTRUE(replace_entry(entry))) {
        send_projection_state(entry)
      }
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input[["core-point_size"]],
    {
      id <- current()
      value <- suppressWarnings(as.numeric(input[["core-point_size"]]))
      if (
        is.null(id) ||
          !identical(input[["core-rendered_for"]], id) ||
          length(value) != 1L ||
          is.na(value) ||
          !is.finite(value) ||
          value < 0 ||
          value > 20
      ) {
        return()
      }
      entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
      req(entry)
      entry$settings$overview_point_size <- value
      if (isTRUE(replace_entry(entry))) {
        send_projection_state(entry)
      }
    },
    ignoreInit = TRUE
  )

  parse_trajectory_action <- function(value, selectable) {
    records <- value %||% list()
    if (!is.list(records)) {
      return(list())
    }
    selected <- list()
    for (record in records) {
      if (
        !is.list(record) ||
          !builder_stage_has_text(record$method %||% "") ||
          !builder_stage_has_text(record$name %||% "") ||
          !record$method %in% names(selectable) ||
          !record$name %in% selectable[[record$method]]
      ) {
        next
      }
      selected[[record$method]] <- unique(c(
        selected[[record$method]],
        record$name
      ))
    }
    selected
  }

  observeEvent(
    input[["core-trajectory_action"]],
    {
      id <- current()
      action <- input[["core-trajectory_action"]]
      if (
        is.null(id) ||
          !identical(input[["core-rendered_for"]], id) ||
          !is.list(action) ||
          !identical(action$action, "set")
      ) {
        return()
      }
      entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
      req(entry)
      selectable <- selectable_trajectory_selection(
        trajectory_catalog_for_entry(entry)
      )
      included <- parse_trajectory_action(action$included, selectable)
      default <- action$default
      if (
        !is.list(default) ||
          !builder_stage_has_text(default$method %||% "") ||
          !builder_stage_has_text(default$name %||% "") ||
          !default$method %in% names(included) ||
          !default$name %in% included[[default$method]]
      ) {
        default <- .builder_state_first_trajectory(included)
      } else {
        default <- list(method = default$method, name = default$name)
      }
      entry$settings$included_trajectories <- included
      entry$settings["default_trajectory"] <- list(default)
      if (isTRUE(replace_entry(entry))) {
        send_trajectory_state(entry)
      }
    },
    ignoreInit = TRUE
  )

  observe({
    req(isTRUE(worker_available()))
    id <- current()
    req(id)
    entry <- builder_upgrade_viewer_content_entry(entry_of(id))
    req(entry)
    catalog <- projection_catalog_for_entry(entry)
    ids <- names(catalog)[vapply(
      catalog,
      function(item) is.list(item) && isTRUE(item$available),
      logical(1)
    )]
    contract <- builder_projection_preview_contract(entry, ids)
    if (identical(contract, isolate(projection_preview_contract()))) {
      return()
    }
    projection_previews(list(dataset = id, frames = list()))
    if (!length(ids)) {
      projection_preview_contract(contract)
      return()
    }
    queued <- enqueue(list(
      kind = "projection_previews",
      id = id,
      dataset_revision = entry$revision,
      projections = ids,
      group = entry$settings$default_group %||% NULL,
      max_cells = 600L,
      replaces = "viewer-projection-gallery"
    ))
    if (isTRUE(queued)) projection_preview_contract(contract)
  })

  observe({
    req(isTRUE(worker_available()))
    id <- current()
    req(id)
    entry <- builder_upgrade_viewer_content_entry(entry_of(id))
    req(entry)
    trajectories <- selectable_trajectory_selection(
      trajectory_catalog_for_entry(entry)
    )
    contract <- builder_trajectory_preview_contract(entry, trajectories)
    if (identical(contract, isolate(trajectory_preview_contract()))) {
      return()
    }
    trajectory_previews(list(dataset = id, frames = list()))
    if (!length(trajectories)) {
      trajectory_preview_contract(contract)
      return()
    }
    queued <- enqueue(list(
      kind = "trajectory_previews",
      id = id,
      dataset_revision = entry$revision,
      trajectories = trajectories,
      max_cells = 600L,
      replaces = "viewer-trajectory-gallery"
    ))
    if (isTRUE(queued)) trajectory_preview_contract(contract)
  })

  observeEvent(
    input[["core-group_action"]],
    {
      id <- current()
      action <- input[["core-group_action"]]
      if (
        is.null(id) ||
          !identical(input[["core-rendered_for"]], id) ||
          !is.list(action) ||
          !identical(action$action, "set")
      ) {
        return()
      }
      entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
      req(entry)
      catalog <- group_catalog_for_entry(entry)
      eligible <- vapply(
        Filter(function(item) isTRUE(item$eligible), catalog$items),
        `[[`,
        character(1),
        "id"
      )
      included <- unique(as.character(unlist(
        action$included %||% character(),
        use.names = FALSE
      )))
      included <- eligible[eligible %in% included]
      if (!length(included)) {
        send_group_state(
          entry,
          "Keep at least one Viewer Group selected."
        )
        return()
      }
      default <- action$default
      if (!builder_stage_has_text(default %||% "") || !default %in% included) {
        default <- included[[1L]]
      }
      current_included <- entry$settings$included_groups %||% character()
      removed <- setdiff(current_included, included)
      next_overrides <- builder_settings_color_overrides(entry$settings)
      for (group in removed) {
        next_overrides[[group]] <- NULL
      }
      entry$settings$included_groups <- included
      entry$settings$groups <- included
      entry$settings$default_group <- default
      entry$settings$group_color_overrides <- next_overrides
      changed <- replace_entry(entry)
      if (isTRUE(changed)) {
        send_group_state(entry)
      }
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input[["core-cell_cycle"]],
    {
      id <- current()
      if (
        is.null(id) ||
          !identical(input[["core-rendered_for"]], id)
      ) {
        return()
      }
      entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
      req(entry)
      metadata <- entry$dataset_profile$viewer_content$metadata %||%
        entry$profile$viewer_content$metadata %||%
        list()
      available <- builder_cell_cycle_candidate_ids(metadata)
      selected <- unname(as.character(
        input[["core-cell_cycle"]] %||% character()
      ))
      selected <- available[available %in% selected]
      if (
        identical(selected, entry$settings$cell_cycle_columns %||% character())
      ) {
        return()
      }
      entry$settings$cell_cycle_columns <- selected
      replace_entry(entry)
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE
  )

  output[["core-group_detail"]] <- renderUI({
    id <- current()
    rendered_for <- input[["core-rendered_for"]]
    if (is.null(id) || !identical(rendered_for, id)) {
      return(NULL)
    }
    entry <- builder_upgrade_viewer_content_entry(entry_of(id))
    req(entry)
    catalog <- group_catalog_for_entry(entry)
    focus <- group_focus_value(input[["core-group_focus"]]) %||%
      entry$settings$default_group %||%
      catalog$focus
    builder_group_detail_ui(
      "core",
      builder_group_detail_model(catalog, focus)
    )
  })

  output[["core-group_colors"]] <- renderUI({
    id <- current()
    rendered_for <- input[["core-rendered_for"]]
    if (is.null(id) || !identical(rendered_for, id)) {
      return(NULL)
    }
    entry <- builder_upgrade_viewer_content_entry(entry_of(id))
    req(entry)
    catalog <- group_catalog_for_entry(entry)
    group <- group_focus_value(input[["core-group_focus"]]) %||%
      entry$settings$default_group %||%
      catalog$focus %||%
      ""
    if (!group %in% (entry$settings$included_groups %||% character())) {
      return(NULL)
    }
    levels <- entry$levels[[group]] %||% character()
    model <- builder_group_colors_model(
      group,
      levels,
      entry$settings$palette %||% "cerebro",
      builder_settings_color_overrides(entry$settings)
    )
    builder_group_colors_ui("core", model)
  })

  output[["core-projection_gallery"]] <- renderUI({
    id <- current()
    rendered_for <- input[["core-rendered_for"]]
    if (is.null(id) || !identical(rendered_for, id)) {
      return(NULL)
    }
    entry <- builder_upgrade_viewer_content_entry(entry_of(id))
    req(entry)
    preview <- projection_previews()
    frames <- if (identical(preview$dataset, id)) preview$frames else list()
    group <- entry$settings$default_group %||% ""
    levels <- entry$levels[[group]] %||% character()
    colors <- if (length(levels)) {
      builder_level_colors(
        levels,
        entry$settings$palette %||% "cerebro",
        builder_settings_color_overrides(entry$settings)[[group]] %||%
          character()
      )
    } else {
      character()
    }
    model <- builder_projection_catalog_model(list(
      projection_catalog = projection_catalog_for_entry(entry),
      included_projections = entry$settings$included_projections,
      default_projection = entry$settings$default_projection,
      overview_point_size = entry$settings$overview_point_size,
      projection_previews = frames,
      preview_colors = colors
    ))
    builder_projection_catalog_ui("core", model)
  })

  output[["core-trajectory_gallery"]] <- renderUI({
    id <- current()
    rendered_for <- input[["core-rendered_for"]]
    if (is.null(id) || !identical(rendered_for, id)) {
      return(NULL)
    }
    entry <- builder_upgrade_viewer_content_entry(entry_of(id))
    req(entry)
    preview <- trajectory_previews()
    frames <- if (identical(preview$dataset, id)) preview$frames else list()
    model <- builder_trajectory_catalog_model(list(
      trajectory_catalog = trajectory_catalog_for_entry(entry),
      included_trajectories = entry$settings$included_trajectories,
      default_trajectory = entry$settings$default_trajectory,
      trajectory_previews = frames
    ))
    builder_trajectory_catalog_ui("core", model)
  })

  observeEvent(
    input[["core-group_color"]],
    {
      id <- current()
      change <- input[["core-group_color"]]
      if (
        is.null(id) ||
          !identical(input[["core-rendered_for"]], id) ||
          !is.list(change) ||
          !builder_stage_has_text(change$group %||% "") ||
          !builder_stage_has_text(change$level %||% "")
      ) {
        return()
      }
      entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
      req(entry)
      group <- as.character(change$group)
      level <- as.character(change$level)
      if (
        !group %in% (entry$settings$included_groups %||% character()) ||
          !level %in% (entry$levels[[group]] %||% character())
      ) {
        return()
      }
      current_overrides <- builder_settings_color_overrides(entry$settings)
      next_overrides <- builder_update_color_override(
        current_overrides,
        group,
        level,
        change$color
      )
      if (identical(next_overrides, current_overrides)) {
        return()
      }
      entry$settings$group_color_overrides <- next_overrides
      entry$settings$colors <- NULL
      replace_entry(entry)
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input[["core-reset_colors"]],
    {
      id <- current()
      if (
        is.null(id) ||
          !identical(input[["core-rendered_for"]], id)
      ) {
        return()
      }
      entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
      req(entry)
      group <- group_focus_value(input[["core-group_focus"]]) %||%
        entry$settings$default_group %||%
        ""
      if (!group %in% (entry$settings$included_groups %||% character())) {
        return()
      }
      current_overrides <- builder_settings_color_overrides(entry$settings)
      next_overrides <- builder_reset_color_overrides(current_overrides, group)
      if (identical(next_overrides, current_overrides)) {
        return()
      }
      entry$settings$group_color_overrides <- next_overrides
      entry$settings$colors <- NULL
      replace_entry(entry)
    },
    ignoreInit = TRUE
  )

  ## Assay-dependent controls above use the namespaced Core inputs.
  invisible(lapply(builder_analysis_steps(), function(step) {
    observeEvent(
      input[[paste0("enhance-analysis_", step$id)]],
      {
        id <- current()
        if (
          is.null(id) ||
            !identical(input[["enhance-rendered_for"]], id)
        ) {
          return()
        }
        entry <- isolate(entry_of(id))
        req(entry)
        selected <- entry$settings$analyses %||% character()
        requested <- isTRUE(input[[paste0("enhance-analysis_", step$id)]])
        analysis_profile <- builder_enhance_analysis_profile(
          entry$profile,
          entry$settings$organism
        )
        blocked <- builder_step_blocked(step, analysis_profile, selected)
        if (requested && !is.null(blocked)) {
          return()
        }
        if (requested) {
          selected <- unique(c(selected, step$id))
        } else {
          selected <- setdiff(selected, step$id)
        }
        entry$settings$analyses <- builder_normalize_analyses(
          selected,
          builder_profile_has(entry$profile, "marker_genes")
        )
        replace_entry(entry)
      },
      ignoreInit = TRUE
    )
  }))

  ## -- supplementary tables -------------------------------------------------
  observeEvent(input[["enhance-table_files"]], {
    id <- current()
    req(id)
    entry <- entry_of(id)
    req(entry)
    uploads <- input[["enhance-table_files"]]
    req(is.data.frame(uploads), nrow(uploads) > 0L)
    for (index in seq_len(nrow(uploads))) {
      filename <- basename(uploads$name[[index]])
      display_name <- builder_table_unique_name(
        builder_table_default_name(filename),
        names(entry$settings$tables %||% list()) %||% character()
      )
      got <- builder_read_table(
        uploads$datapath[[index]],
        display_name,
        filename = filename
      )
      if (!is.null(got$error)) {
        showNotification(
          paste0(filename, ": ", got$error),
          type = "error",
          duration = 8
        )
        next
      }
      got$file_name <- filename
      got$file_type <- toupper(tools::file_ext(filename))
      got$file_size <- suppressWarnings(as.numeric(uploads$size[[index]]))
      entry$settings$tables[[got$name]] <- got
    }
    replace_entry(entry)
  })

  observeEvent(
    input[["enhance-table_action"]],
    {
      id <- current()
      req(id)
      action <- input[["enhance-table_action"]]
      req(is.list(action), is.character(action$key), nzchar(action$key))
      entry <- entry_of(id)
      req(entry)
      tables <- entry$settings$tables %||% list()
      if (!action$key %in% names(tables)) {
        return()
      }
      if (identical(action$action, "remove")) {
        tables[[action$key]] <- NULL
      } else if (identical(action$action, "rename")) {
        new_name <- trimws(as.character(action$name %||% ""))
        if (
          !nzchar(new_name) ||
            (new_name %in% names(tables) && !identical(new_name, action$key))
        ) {
          showNotification(
            "Table names must be non-empty and unique.",
            type = "error",
            duration = 5
          )
          return()
        }
        table <- tables[[action$key]]
        table$name <- new_name
        tables[[action$key]] <- NULL
        tables[[new_name]] <- table
      } else {
        return()
      }
      entry$settings$tables <- tables
      replace_entry(entry)
    },
    ignoreInit = TRUE
  )

  alignment_server <- builder_spatial_alignment_server(
    input = input,
    output = output,
    session = session,
    current = current,
    entry_of = entry_of,
    worker = worker,
    enqueue = enqueue,
    commit_images = commit_enhance_images,
    alignment_preview = alignment_preview,
    spatial_coords = spatial_coords
  )
  active_slice <- alignment_server$active_section

  ## -- what the last build produced ---------------------------------------
  output$result_card <- renderUI({
    r <- result()
    if (is.null(r)) {
      return(NULL)
    }
    builder_build_status_ui(builder_build_status_model(r))
  })

  run_result_action <- function(action) {
    current_result <- isolate(result())
    req(inherits(current_result, "builder_result"))
    outcome <- try(action(current_result), silent = TRUE)
    if (inherits(outcome, "try-error") || !isTRUE(outcome)) {
      message <- if (inherits(outcome, "try-error")) {
        conditionMessage(attr(outcome, "condition"))
      } else {
        "The requested result action could not be completed."
      }
      showNotification(message, type = "error")
    }
  }
  copy_result_value <- function(value) {
    session$sendCustomMessage("builder_copy_text", list(text = value))
    TRUE
  }
  observeEvent(input$open_app, {
    run_result_action(builder_open_final_app)
  })
  observeEvent(input$reveal_folder, {
    run_result_action(builder_reveal_release)
  })
  observeEvent(input$copy_path, {
    run_result_action(function(value) {
      builder_copy_result_path(value, "release", .copy = copy_result_value)
    })
  })
  observeEvent(input$copy_report, {
    run_result_action(function(value) {
      builder_copy_result_path(value, "report", .copy = copy_result_value)
    })
  })
  observeEvent(input$retry_failed_analysis, {
    session$sendCustomMessage("builder_click", list(id = "build"))
  })
  observeEvent(input$remove_failed_analysis, {
    current_result <- isolate(result())
    dataset_id <- current_result$failed_dataset_id %||% NULL
    req(builder_stage_has_text(dataset_id %||% ""))
    failed <- current_result$retry_closure %||% character()
    entry <- isolate(entry_of(dataset_id))
    req(entry)
    if (length(intersect(entry$settings$analyses %||% character(), failed))) {
      entry$settings$analyses <- setdiff(entry$settings$analyses, failed)
      replace_entry(entry)
    }
    session$onFlushed(
      function() {
        session$sendCustomMessage("builder_click", list(id = "build"))
      },
      once = TRUE
    )
  })
  observeEvent(input$restart_worker, {
    current_result <- isolate(result())
    req(inherits(current_result, "builder_result"))
    req(isTRUE(current_result$restartable_worker))
    current_worker <- isolate(worker())
    current_protocol <- isolate(protocol())
    req(current_worker, current_protocol)
    restart_worker_protocol(
      current_worker,
      current_protocol,
      "The worker was restarted from saved snapshots."
    )
  })

  ## -- the action bar ------------------------------------------------------
  validate_review_inputs <- function(values) {
    next_options <- try(do.call(builder_review_options, values), silent = TRUE)
    if (inherits(next_options, "try-error")) {
      review_validation(list(
        ok = FALSE,
        error = conditionMessage(attr(next_options, "condition"))
      ))
      return(invisible(FALSE))
    }
    review_options(next_options)
    review_validation(list(ok = TRUE, error = NULL))
    invisible(TRUE)
  }

  observe({
    current_options <- isolate(review_options())
    values <- list(
      welcome_message = input[["review-welcome_message"]],
      point_size = input[["review-point_size"]] %||% 5,
      variable_to_compare = input[["review-variable_to_compare"]],
      host = current_options$host,
      port = current_options$port,
      max_request_size = current_options$max_request_size,
      display_mode = current_options$display_mode,
      launch_browser = current_options$launch_browser,
      show_upload_ui = input[["review-show_upload_ui"]]
    )
    if (any(vapply(values, is.null, logical(1)))) {
      return()
    }
    validate_review_inputs(values)
  })

  freeze_plan_for_output <- function(out_dir, overwrite = FALSE) {
    pending <- imports()$entries
    if (length(pending)) {
      states <- vapply(pending, `[[`, character(1), "load_state")
      message <- if (any(states == "error")) {
        "Retry or remove datasets that could not load."
      } else {
        "Wait for all datasets to finish loading before building."
      }
      return(builder_plan_error(message, "imports_pending"))
    }
    validation <- review_validation()
    if (!isTRUE(validation$ok)) {
      return(builder_plan_error(
        validation$error %||% "Review options are invalid.",
        "invalid_review_options"
      ))
    }
    all <- sets()
    if (!length(all)) {
      return(builder_plan_error("No datasets yet.", "empty_release"))
    }
    typed <- review_options()
    app_options <- builder_review_options_for_plan(typed)
    builder_freeze_plan(
      entries = all,
      out_dir = out_dir,
      make_app = isTRUE(input$make_app),
      overwrite = isTRUE(overwrite),
      app_options = app_options
    )
  }

  frozen_review_plan <- reactive({
    plan <- freeze_plan_for_output(
      file.path(tempdir(), "cerebro-builder-output-preview"),
      overwrite = FALSE
    )
    if (inherits(plan, "builder_build_plan")) {
      plan$output_pending <- TRUE
    }
    plan
  })

  review_report <- reactive({
    pending <- imports()$entries
    if (length(pending)) {
      states <- vapply(pending, `[[`, character(1), "load_state")
      if (any(states == "error")) {
        return(list(
          ok = FALSE,
          msg = "Retry or remove datasets that could not load."
        ))
      }
      return(list(
        ok = FALSE,
        msg = "Wait for all datasets to finish loading before building."
      ))
    }
    plan <- frozen_review_plan()
    if (!builder_review_can_build(plan)) {
      issue_count <- if (
        inherits(plan, "builder_build_plan") &&
          identical(plan$readiness, "ready")
      ) {
        max(1L, length(builder_review_model(plan)$warnings))
      } else {
        1L
      }
      return(list(
        ok = FALSE,
        msg = paste0(
          "Resolve ",
          issue_count,
          " required setting",
          if (issue_count == 1L) "" else "s",
          " before building."
        )
      ))
    }
    list(
      ok = TRUE,
      msg = paste0(
        length(plan$items),
        " dataset",
        if (length(plan$items) == 1L) "" else "s",
        " ready"
      )
    )
  })

  output[["enhance-analysis_modules"]] <- renderUI({
    contract <- enhance_contract()
    req(contract$id)
    entry <- isolate(entry_of(contract$id))
    req(entry)
    builder_enhance_modules_ui(
      "enhance",
      builder_enhance_modules(
        entry$profile,
        list(
          organism = contract$organism,
          analyses = contract$analyses
        )
      )
    )
  })

  output[["enhance-table_list"]] <- renderUI({
    id <- current()
    req(id)
    entry <- entry_of(id)
    req(entry)
    tables <- entry$settings$tables %||% list()
    if (!length(tables)) {
      return(NULL)
    }
    div(
      class = "enhance-table-list builder-file-list",
      h5("Added tables"),
      lapply(names(tables), function(key) {
        table <- tables[[key]]
        filename <- table$file_name %||% paste0(key, ".csv")
        file_type <- table$file_type %||% toupper(tools::file_ext(filename))
        file_size <- builder_review_human_size(table$file_size %||% NA_real_)
        file_summary <- paste(file_type, file_size, sep = " · ")
        div(
          class = "enhance-table-item builder-file-item",
          div(
            class = "enhance-table-file-meta",
            span(class = "enhance-table-filename", filename),
            span(class = "enhance-table-type", file_summary),
            span(
              class = "builder-status builder-status--ready",
              "Ready"
            )
          ),
          tags$label(
            class = "enhance-table-name-field",
            span("Table name"),
            tags$input(
              type = "text",
              class = "enhance-table-display-name",
              value = key,
              `data-table-key` = key,
              `aria-label` = paste("Display name for", filename)
            )
          ),
          tags$button(
            type = "button",
            class = "enhance-table-remove",
            `data-table-key` = key,
            "Remove"
          )
        )
      })
    )
  })

  output$workbench <- renderUI({
    loading_id <- active_import_id()
    loading_entry <- if (is.null(loading_id)) {
      NULL
    } else {
      builder_import_find(imports(), loading_id)
    }
    if (!is.null(loading_entry)) {
      return(builder_loading_workbench_ui(loading_entry))
    }
    id <- current()
    entry <- isolate(entry_of(id))
    if (is.null(entry)) {
      return(builder_empty_workbench_ui())
    }
    state <- try(builder_dataset_state(entry), silent = TRUE)
    attention <- if (inherits(state, "try-error")) {
      character()
    } else {
      state$attention_ids
    }
    blockers <- if (inherits(state, "try-error")) {
      "Dataset state could not be validated."
    } else {
      state$blocking_ids
    }
    inspect_model <- builder_inspect_model(
      profile = entry$profile,
      state = if (inherits(state, "try-error")) {
        list(
          attention_ids = attention,
          blocking_ids = blockers,
          manifest = list()
        )
      } else {
        state
      },
      format = entry$format,
      dataset_id = entry$id,
      settings = entry$settings
    )
    if (!inherits(state, "try-error")) {
      entry <- state$entry
    }
    settings <- entry$settings
    assay_profile <- entry$profile$assay_profiles[[settings$assay]] %||%
      list(
        layers = entry$profile$layers,
        nUMI_choices = entry$profile$nUMI,
        nGene_choices = entry$profile$nGene
      )
    core_model <- c(
      settings[c(
        "name",
        "organism",
        "included_groups",
        "default_group",
        "cell_cycle_columns",
        "included_projections",
        "default_projection",
        "overview_point_size",
        "included_trajectories",
        "default_trajectory",
        "assay",
        "layer",
        "nUMI",
        "nGene"
      )],
      list(
        id = entry$id,
        organism_choices = c(
          "Human (hg)" = "hg",
          "Mouse (mm)" = "mm",
          "Other" = "other"
        ),
        group_choices = unname(entry$profile$group_candidates),
        suggested_groups = entry$profile$group_preselect %||%
          settings$included_groups %||%
          character(),
        metadata_catalog = entry$dataset_profile$viewer_content$metadata %||%
          entry$profile$viewer_content$metadata %||%
          list(),
        metadata_policy = if (inherits(state, "try-error")) {
          entry$settings$metadata_policy %||%
            entry$settings$recommendations$metadata %||%
            list()
        } else {
          state$metadata_policy %||% list()
        },
        analysis_manifest = if (inherits(state, "try-error")) {
          list()
        } else {
          state$manifest %||% list()
        },
        content_manifest = if (inherits(state, "try-error")) {
          list()
        } else {
          state$manifest %||% list()
        },
        analysis_acknowledgements = if (inherits(state, "try-error")) {
          character()
        } else {
          state$acknowledgements %||% character()
        },
        projection_catalog = projection_catalog_for_entry(entry),
        trajectory_catalog = trajectory_catalog_for_entry(entry),
        levels = entry$levels %||% list(),
        projection_choices = entry$profile$reductions,
        assay_choices = entry$profile$assays,
        layer_choices = assay_profile$layers,
        nUMI_choices = assay_profile$nUMI_choices,
        nGene_choices = assay_profile$nGene_choices,
        backend = settings$expression_backend %||% "embedded",
        backend_choices = c(
          "Embedded" = "embedded",
          "HDF5" = "h5",
          "BPCells" = "bpcells"
        ),
        metadata_attention = if (length(attention)) {
          paste("Metadata needs attention:", paste(attention, collapse = ", "))
        } else {
          ""
        }
      )
    )
    tagList(
      builder_dataset_context_ui(store(), current()),
      builder_inspect_stage_ui("inspect", inspect_model),
      builder_core_stage_ui("core", core_model),
      builder_enhance_stage_ui(
        "enhance",
        builder_enhance_model(
          id = entry$id,
          profile = entry$profile,
          state = if (inherits(state, "try-error")) list() else state,
          settings = entry$settings,
          modules = list()
        ),
        dynamic_modules = TRUE
      ),
      uiOutput("review_stage"),
      conditionalPanel(
        condition = "input.make_app === true",
        builder_review_controls_ui("review", isolate(review_options()))
      ),
      div(
        class = "dataset-review-footer",
        actionButton(
          "review_current_dataset",
          if (is.null(builder_next_unreviewed(sets(), current()))) {
            "Looks good — finish review"
          } else {
            "Looks good — review next dataset"
          },
          class = "btn btn-action dataset-review-confirm"
        )
      )
    )
  })

  focus_dataset_settings <- function(message = NULL) {
    session$sendCustomMessage("builder_focus_dataset", list(message = message))
  }

  select_dataset_index <- function(offset) {
    entries <- isolate(sets())
    ids <- vapply(entries, `[[`, character(1), "id")
    index <- match(isolate(current()), ids)
    target <- index + offset
    if (!is.na(index) && target >= 1L && target <= length(ids)) {
      current(ids[[target]])
      focus_dataset_settings()
    }
  }

  observeEvent(input$review_previous_dataset, select_dataset_index(-1L))
  observeEvent(input$review_next_dataset, select_dataset_index(1L))

  observeEvent(input$review_current_dataset, {
    id <- isolate(current())
    entries <- isolate(sets())
    index <- match(id, vapply(entries, `[[`, character(1), "id"))
    req(!is.na(index))
    entry <- entries[[index]]
    status <- builder_dataset_review_status(entry, TRUE)
    if (identical(status$id, "needs-attention")) {
      showNotification(
        "Resolve this dataset’s highlighted issues before marking it reviewed.",
        type = "warning",
        duration = 5
      )
      return()
    }
    entry$reviewed_revision <- as.integer(entry$revision %||% 0L)
    next_state <- builder_reduce_state(
      isolate(store()),
      list(type = "replace", id = id, entry = entry)
    )
    store(next_state)
    next_unreviewed <- builder_next_unreviewed(next_state$datasets, id)
    if (!is.null(next_unreviewed)) {
      next_entry <- next_state$datasets[[match(
        next_unreviewed,
        vapply(next_state$datasets, `[[`, character(1), "id")
      )]]
      current(next_unreviewed)
      focus_dataset_settings(paste0(
        entry$settings$name,
        " marked as reviewed. Opening ",
        next_entry$settings$name,
        "."
      ))
    } else {
      session$sendCustomMessage(
        "builder_focus_review",
        list(
          message = paste0(entry$settings$name, " marked as reviewed.")
        )
      )
    }
  })

  output$review_stage <- renderUI({
    plan <- frozen_review_plan()
    if (
      inherits(plan, "builder_build_plan") &&
        is.list(plan) &&
        identical(plan$readiness, "ready")
    ) {
      builder_review_stage_ui("review", builder_review_model(plan, result()))
    } else {
      builder_review_blocked_ui(
        "review",
        if (is.list(plan)) plan$error %||% NULL else NULL
      )
    }
  })

  datasets_present <- reactiveVal(FALSE)
  observe({
    present <- length(sets()) > 0L || length(imports()$entries) > 0L
    if (!identical(present, isolate(datasets_present()))) {
      datasets_present(present)
    }
  })

  output$actionbar <- renderUI({
    if (!isTRUE(datasets_present())) {
      return(NULL)
    }
    make_app_control <- builder_app_control(
      app_capability,
      current_value = isolate(input$make_app)
    )
    div(
      class = "actionbar",
      div(
        class = "inner",
        span(
          class = "grow actionbar-output-note",
          "Choose where to save the CRB files and App folder."
        ),
        uiOutput("review_action_summary", inline = TRUE),
        make_app_control,
        uiOutput(
          "build_actions",
          inline = TRUE,
          class = "builder-action-row"
        )
      )
    )
  })

  output$review_action_summary <- renderUI({
    if (!isTRUE(datasets_present())) {
      return(NULL)
    }
    span(class = "summary", review_report()$msg)
  })

  ## Build state changes frequently while the fields above are user-edited.
  ## Keeping the buttons in their own output prevents a protocol transition
  ## from recreating those inputs and resetting the browser's current values.
  output$build_actions <- renderUI({
    rep <- review_report()
    flow <- build_flow()
    current_protocol <- protocol()
    build_phase <- if (is.null(current_protocol)) {
      "idle"
    } else {
      current_protocol$build_status %||% "idle"
    }
    build_in_flight <- build_phase %in% c("queued", "running", "cancelling")
    protocol_quiescent <- !is.null(current_protocol) &&
      builder_protocol_is_quiescent(current_protocol)
    actionButton(
      "build",
      switch(
        flow$stage,
        choosing_folder = "Choose a folder…",
        building = "Building…",
        "Build"
      ),
      class = "btn btn-action",
      disabled = !isTRUE(rep$ok) ||
        !identical(flow$stage, "idle") ||
        build_in_flight ||
        !protocol_quiescent ||
        !isTRUE(worker_available())
    )
  })

  observe({
    current_flow <- build_flow()
    current_protocol <- protocol()
    if (
      identical(current_flow$stage, "building") &&
        !is.null(current_protocol) &&
        builder_protocol_is_quiescent(current_protocol)
    ) {
      build_flow(list(stage = "idle", plan = NULL))
    }
  })

  ## -- build ---------------------------------------------------------------
  ## The whole export runs in the worker: analyses, matrix write, bundle. This
  ## process only sends a plan and waits for the report, so the page keeps
  ## answering while a marker-gene run takes its minutes.
  enqueue_build_plan <- function(plan) {
    rs <- worker()
    req(rs)
    current_protocol <- isolate(protocol())
    req(builder_protocol_is_quiescent(current_protocol))
    plan <- unserialize(serialize(plan, NULL, version = 3L))
    result(NULL)
    queued <- enqueue(list(
      kind = "build",
      plan = plan,
      note = paste0(
        "Building ",
        length(plan$items),
        " dataset",
        if (length(plan$items) == 1L) "" else "s",
        "…"
      )
    ))
    build_flow(list(
      stage = if (isTRUE(queued)) "building" else "idle",
      plan = NULL
    ))
    invisible(isTRUE(queued))
  }

  prepare_selected_output <- function(path, overwrite = FALSE) {
    plan <- isolate(freeze_plan_for_output(path, overwrite = overwrite))
    if (
      !inherits(plan, "builder_build_plan") ||
        !identical(plan$readiness, "ready")
    ) {
      build_flow(list(stage = "idle", plan = NULL))
      showNotification(
        plan$error %||% "The selected folder cannot be used.",
        type = "error",
        duration = 6
      )
      return(invisible(FALSE))
    }
    if (length(plan$existing_targets) && !isTRUE(overwrite)) {
      build_flow(list(stage = "conflict", plan = plan))
      session$sendCustomMessage(
        "builder_build_dialog",
        list(
          type = "conflict",
          title = "Files already exist",
          files = basename(plan$existing_targets)
        )
      )
      return(invisible(FALSE))
    }
    enqueue_build_plan(plan)
  }

  choose_build_folder <- function() {
    build_flow(list(stage = "choosing_folder", plan = NULL))
    session$onFlushed(
      function() {
        choice <- builder_choose_output_directory()
        if (identical(choice$status, "cancelled")) {
          build_flow(list(stage = "idle", plan = NULL))
          return()
        }
        if (!identical(choice$status, "selected")) {
          build_flow(list(stage = "idle", plan = NULL))
          showNotification(
            choice$error %||% "The folder picker could not be opened.",
            type = "error",
            duration = 6
          )
          return()
        }
        prepare_selected_output(choice$path)
      },
      once = TRUE
    )
  }

  observeEvent(input$build, {
    req(identical(isolate(build_flow())$stage, "idle"))
    plan <- isolate(frozen_review_plan())
    datasets <- isolate(sets())
    review_statuses <- lapply(
      datasets,
      builder_dataset_review_status,
      active = FALSE
    )
    attention <- which(vapply(
      review_statuses,
      function(status) identical(status$id, "needs-attention"),
      logical(1)
    ))
    unreviewed <- which(
      !vapply(datasets, builder_dataset_is_reviewed, logical(1))
    )
    if (length(attention)) {
      target <- datasets[[attention[[1L]]]]$id
      build_flow(list(
        stage = "attention_required",
        plan = NULL,
        target = target
      ))
      session$sendCustomMessage(
        "builder_build_dialog",
        list(
          type = "needs_attention",
          title = "Some datasets still need attention",
          names = vapply(
            datasets[attention],
            function(entry) {
              paste0(
                entry$settings$name,
                " — Resolve the highlighted settings."
              )
            },
            character(1)
          )
        )
      )
      return()
    }
    if (length(unreviewed)) {
      target <- datasets[[unreviewed[[1L]]]]$id
      build_flow(list(stage = "review_required", plan = NULL, target = target))
      session$sendCustomMessage(
        "builder_build_dialog",
        list(
          type = "unreviewed",
          title = "Some datasets have not been reviewed",
          names = vapply(
            datasets[unreviewed],
            function(entry) entry$settings$name,
            character(1)
          )
        )
      )
      return()
    }
    if (!builder_review_can_build(plan)) {
      showNotification(
        plan$error %||% "Resolve the highlighted settings before building.",
        type = "warning",
        duration = 6
      )
      return()
    }
    if (length(datasets) >= 2L) {
      build_flow(list(stage = "confirming", plan = NULL))
      session$sendCustomMessage(
        "builder_build_dialog",
        list(
          type = "datasets",
          title = "Ready to build all datasets?",
          count = length(datasets),
          names = vapply(
            datasets,
            function(entry) entry$settings$name %||% "Dataset",
            character(1)
          )
        )
      )
      return()
    }
    choose_build_folder()
  })

  observeEvent(input$builder_build_dialog, {
    action <- input$builder_build_dialog$action %||% "cancel"
    flow <- isolate(build_flow())
    if (identical(action, "continue") && identical(flow$stage, "confirming")) {
      choose_build_folder()
    } else if (
      identical(action, "replace") &&
        identical(flow$stage, "conflict") &&
        inherits(flow$plan, "builder_build_plan")
    ) {
      prepare_selected_output(flow$plan$out_dir, overwrite = TRUE)
    } else if (
      identical(action, "choose_another") &&
        identical(flow$stage, "conflict")
    ) {
      choose_build_folder()
    } else if (
      identical(action, "review_now") &&
        identical(flow$stage, "review_required")
    ) {
      current(flow$target)
      build_flow(list(stage = "idle", plan = NULL))
      focus_dataset_settings()
    } else if (
      identical(action, "fix_issues") &&
        identical(flow$stage, "attention_required")
    ) {
      current(flow$target)
      build_flow(list(stage = "idle", plan = NULL))
      focus_dataset_settings()
    } else {
      build_flow(list(stage = "idle", plan = NULL))
    }
  })

  validate_rail_removal <- function(next_state, id) {
    builder_validate_next_plan(
      next_state,
      out_dir = file.path(tempdir(), "cerebro-builder-output-preview"),
      make_app = isTRUE(isolate(input$make_app)),
      overwrite = FALSE
    )
  }

  remove_dataset <- function(
    previous_state,
    updated,
    id,
    validation
  ) {
    ids <- vapply(previous_state$datasets, `[[`, character(1), "id")
    entry <- previous_state$datasets[[match(id, ids)]]
    previous_removed <- previous_state$last_removed
    if (is.list(previous_removed)) {
      identity <- .builder_worker_identity(previous_removed$entry$snapshot)
      pending_drops <- isolate(pending_snapshot_drops())
      pending_drops[[previous_removed$id]] <- identity
      pending_snapshot_drops(pending_drops)
      queued <- enqueue(list(
        kind = "drop",
        id = previous_removed$id,
        dataset_revision = previous_removed$entry$revision %||% 0L,
        snapshot_identity = identity,
        note = "Releasing memory…"
      ))
      if (!isTRUE(queued)) {
        pending_drops[[previous_removed$id]] <- NULL
        pending_snapshot_drops(pending_drops)
      }
    }
    result(NULL)
    showNotification(
      tagList(
        paste0("Removed ", entry$settings$name, ". "),
        actionLink("undo_remove", "Undo")
      ),
      type = "message",
      duration = 10
    )
  }

  # observeEvent(input$drop_ds, ...) is owned by builder_dataset_rail_server().
  rail_controller <- builder_dataset_rail_server(
    input = input,
    session = session,
    store = store,
    validate_remove = validate_rail_removal,
    on_select = function(id) {
      active_import_id(NULL)
      result(NULL)
    },
    on_remove = remove_dataset,
    on_undo = function() result(NULL),
    on_validation = function(validation) {
      if (isTRUE(validation$ok)) {
        add_error(NULL)
      } else if (!identical(validation$code, "confirmation_required")) {
        add_error(validation$message)
      }
    }
  )

  output$busy <- renderUI({
    note <- busy_note()
    if (is.null(note)) {
      return(NULL)
    }
    current_protocol <- protocol()
    build_phase <- current_protocol$build_status %||% "idle"
    pipeline <- if (identical(build_phase, "queued")) {
      builder_build_pipeline_ui("queued")
    } else if (identical(build_phase, "running")) {
      builder_build_pipeline_ui("building")
    } else {
      NULL
    }
    div(
      class = paste("busy", if (is.null(pipeline)) NULL else "is-building"),
      if (is.null(pipeline)) span(class = "spinner"),
      pipeline,
      span(note)
    )
  })
}

shinyApp(ui, server)
