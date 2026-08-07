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
source(file.path("ui", "dataset_rail.R"), local = TRUE)
source("plan.R", local = TRUE)
source(file.path("ui", "inspect_stage.R"), local = TRUE)
source(file.path("ui", "core_stage.R"), local = TRUE)
source(file.path("ui", "enhance_stage.R"), local = TRUE)
source(file.path("ui", "review_stage.R"), local = TRUE)
source(file.path("ui", "build_status.R"), local = TRUE)
source("worker.R", local = TRUE)
source("session.R", local = TRUE)

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
    class = "shell",
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
      uiOutput("ds_list"),
      div(
        class = "rail-add",
        div(
          class = "dataset-file-control",
          tags$input(
            id = "dataset_files",
            name = "dataset_files",
            class = "shiny-input-file dataset-file-input",
            type = "file",
            multiple = "multiple",
            accept = paste(
              paste0(
                ".",
                unique(unlist(lapply(builder_formats, `[[`, "extensions")))
              ),
              collapse = ","
            )
          ),
          tags$label(
            `for` = "dataset_files",
            class = "dataset-file-button",
            icon_svg(ICON_PLUS),
            span("Add datasets…")
          )
        ),
        div(class = "or", "or try an example"),
        uiOutput("example_buttons"),
        uiOutput("add_error")
      )
    ),
    div(
      id = "pane",
      uiOutput("workbench"),
      uiOutput("result_card")
    )
  ),
  uiOutput("actionbar"),
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
    update_current_id(updated$current_dataset)
    invisible(value)
  }
  result <- reactiveVal(NULL)
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
  spatial_coords <- reactiveVal(NULL)

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
    n <- length(store()$datasets)
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
  release_pending_source <- function(payload) {
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
    replaceable <- req$kind %in% c("preview", "coords")
    if (replaceable) {
      request_sequence(request_sequence() + 1L)
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
    invisible(lapply(recovered$failed %||% list(), function(request) {
      release_pending_source(request$payload)
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
      add_error(paste0(
        error,
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
  ## The typed store is the durable authority for example availability.
  output$example_buttons <- renderUI({
    used <- as.character(unlist(Filter(
      Negate(is.null),
      lapply(sets(), function(entry) entry$example)
    )))
    tagList(lapply(builder_examples(), function(ex) {
      taken <- ex$id %in%
        used ||
        builder_source_key("example", ex$id) %in% pending_sources()
      tags$button(
        class = if (taken) "btn example-btn is-taken" else "btn example-btn",
        `data-ex` = ex$id,
        disabled = if (taken) "disabled",
        `aria-disabled` = if (taken) "true" else "false",
        ## One wrapper, because the collapse animates the button's single grid
        ## row to 0fr -- two children would be two rows and only the first
        ## would close.
        tags$span(
          class = "ex-inner",
          tags$span(class = "ex-label", ex$label),
          tags$span(class = "ex-detail", ex$detail)
        )
      )
    }))
  })

  ## An example already on the list is not an offer any more. Which ones are
  ## taken is derived state, so it is pushed rather than re-rendered.
  observe({
    used <- Filter(
      Negate(is.null),
      lapply(sets(), function(e) e$example)
    )
    pending_examples <- sub(
      "^example:",
      "",
      grep("^example:", pending_sources(), value = TRUE)
    )
    session$sendCustomMessage(
      "builder_used_examples",
      list(ids = unique(c(as.character(unlist(used)), pending_examples)))
    )
  })

  start_load <- function(kind, arg, label) {
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
    queued <- enqueue(list(
      kind = "load",
      source = kind,
      id = id,
      path = if (identical(kind, "file")) arg else NA_character_,
      example = if (identical(kind, "example")) arg else NULL,
      label = label,
      note = paste0("Loading ", label, "…")
    ))
    if (!isTRUE(queued)) {
      pending_sources(builder_source_release(
        pending_sources(),
        reservation$key
      ))
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
    valid <- !is.na(paths) & nzchar(paths) & !is.na(labels) & nzchar(labels)
    paths <- paths[valid]
    labels <- labels[valid]
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
        tools::file_path_sans_ext(basename(labels[[i]]))
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
          ". Enable “Replace existing outputs” to publish atomically over them."
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
          builder_session_load(current_worker, nxt$id, nxt$path, request)
        } else {
          builder_session_example(current_worker, nxt$id, nxt$example, request)
        },
        preview = builder_session_preview(
          current_worker,
          nxt$id,
          nxt$reduction,
          nxt$group,
          BUILDER_PREVIEW_MAX,
          request
        ),
        coords = builder_session_coords(
          current_worker,
          nxt$id,
          nxt$image,
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
      restart_worker_protocol(
        current_worker,
        dispatched$protocol,
        conditionMessage(attr(started_call, "condition"))
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
      restart_worker_protocol(
        current_worker,
        current_protocol,
        conditionMessage(attr(got, "condition"))
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
    if (is.null(got$result)) {
      return()
    }
    request <- current_protocol$pending
    p <- request$payload
    if (!is.null(got$result$error)) {
      if (identical(request$kind, "build")) {
        release <- isolate(active_release())
        release_result <- builder_result_failure(got$result$error)
        if (!is.null(release)) {
          release_result <- abort_release_result(release, got$result$error)
          active_release(NULL)
        }
        result(release_result)
      } else {
        add_error(got$result$error)
      }
      restart_worker_protocol(
        got$worker,
        current_protocol,
        got$result$error
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
      release_pending_source(p)
      if (identical(request$kind, "build")) {
        result(builder_result_failure(completed$error))
        update_build_state(list(
          type = "fail",
          id = request$build_id,
          error = completed$error
        ))
      } else {
        add_error(completed$error)
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
      if (!is.null(value$error)) {
        release_pending_source(p)
        add_error(value$error)
        protocol(builder_protocol_acknowledge(protocol(), request$request_id))
        return()
      }
      profile <- value$profile
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
        revision = 0L,
        ## Level names per grouping variable, in the order the exporter will
        ## produce them -- the keys a configured palette has to match.
        levels = value$levels %||% list(),
        settings = builder_default_settings(profile, unique_name(p$label))
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
      store(builder_reduce_state(
        isolate(store()),
        list(type = "add", entry = entry)
      ))
      release_pending_source(p)
      protocol(builder_protocol_dataset(
        protocol(),
        p$id,
        entry$revision,
        .builder_worker_identity(entry$snapshot)
      ))
      current(p$id)
      result(NULL)
    } else if (identical(p$kind, "preview")) {
      if (identical(current(), p$id)) {
        preview_frame(value)
      }
    } else if (identical(p$kind, "coords")) {
      if (
        identical(current(), p$id) &&
          identical(active_slice(), p$image)
      ) {
        spatial_coords(value)
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
        current(if (length(all)) all[[1]]$id else NULL)
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
    updateCheckboxGroupInput(
      session,
      "enhance-histology_to_retain",
      choices = choices,
      selected = choices
    )
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
  output$ds_list <- renderUI({
    builder_dataset_rail_ui(store(), current())
  })

  ## -- keep the current entry's settings in step with Core -----------------
  core_setting_inputs <- c(
    name = "core-name",
    organism = "core-organism",
    default_group = "core-default_group",
    default_projection = "core-default_projection",
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
    entry <- isolate(entry_of(id))
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
  update_enhance_table_choices <- function(entry) {
    choices <- names(entry$settings$tables %||% list()) %||% character()
    updateCheckboxGroupInput(
      session,
      "enhance-tables_to_retain",
      choices = choices,
      selected = choices
    )
    invisible(choices)
  }

  observeEvent(input[["enhance-add_table"]], {
    id <- current()
    req(id)
    entry <- entry_of(id)
    req(entry)
    got <- builder_read_table(
      trimws(input[["enhance-table_path"]] %||% ""),
      trimws(input[["enhance-table_name"]] %||% "")
    )
    if (!is.null(got$error)) {
      showNotification(got$error, type = "error", duration = 8)
      return()
    }
    entry$settings$tables[[got$name]] <- got
    replace_entry(entry)
    update_enhance_table_choices(entry)
    updateTextInput(session, "enhance-table_path", value = "")
    updateTextInput(session, "enhance-table_name", value = "")
  })

  observeEvent(
    input[["enhance-tables_to_retain"]],
    {
      id <- current()
      req(id)
      if (!identical(input[["enhance-rendered_for"]], id)) {
        return()
      }
      entry <- isolate(entry_of(id))
      req(entry)
      retained <- input[["enhance-tables_to_retain"]] %||% character()
      entry$settings <- builder_enhance_retain(
        entry$settings,
        "tables",
        retained
      )
      replace_entry(entry)
      update_enhance_table_choices(entry)
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input[["enhance-histology_to_retain"]],
    {
      id <- current()
      req(id)
      if (!identical(input[["enhance-rendered_for"]], id)) {
        return()
      }
      entry <- isolate(entry_of(id))
      req(entry)
      retained <- input[["enhance-histology_to_retain"]] %||% character()
      entry$settings <- builder_enhance_retain(
        entry$settings,
        "images",
        retained
      )
      commit_enhance_images(entry, entry$settings$images)
    },
    ignoreInit = TRUE
  )

  ## -- histology background -------------------------------------------------
  ## The raw image is decoded once and kept as an array; the sliders then only
  ## re-encode it, so dragging is cheap and never loses quality by re-reading a
  ## picture that has already been resampled.
  raw_image <- reactiveVal(NULL)

  output[["enhance-has_image"]] <- reactive(!is.null(raw_image()))
  outputOptions(output, "enhance-has_image", suspendWhenHidden = FALSE)

  ## Which tissue section the alignment controls currently describe. An object
  ## can hold several, each with its own coordinates and its own slide.
  active_slice <- reactiveVal(NULL)

  observeEvent(current(), {
    raw_image(NULL)
    spatial_coords(NULL)
    id <- current()
    rs <- worker()
    e <- isolate(entry_of(id))
    if (is.null(id) || is.null(rs) || is.null(e)) {
      active_slice(NULL)
      return()
    }
    if (!length(e$profile$images)) {
      active_slice(NULL)
      return()
    }
    active_slice(e$profile$images[1])
    enqueue(list(
      kind = "coords",
      id = id,
      image = e$profile$images[1],
      replaces = "coords",
      note = "Loading spatial coordinates…"
    ))
  })

  ## Switching section re-reads that section's coordinates and drops the
  ## in-progress alignment, which described a different slide.
  observeEvent(input[["enhance-active_slice"]], {
    id <- current()
    req(id)
    e <- isolate(entry_of(id))
    req(e)
    nm <- input[["enhance-active_slice"]]
    if (!nzchar(nm) || identical(nm, isolate(active_slice()))) {
      return()
    }
    active_slice(nm)
    raw_image(NULL)
    spatial_coords(NULL)
    enqueue(list(
      kind = "coords",
      id = id,
      image = nm,
      replaces = "coords",
      note = paste0("Loading coordinates for ", nm, "…")
    ))
  })

  observeEvent(input[["enhance-attach_image"]], {
    img <- builder_read_image(trimws(input[["enhance-image_path"]] %||% ""))
    if (!is.null(img$error)) {
      showNotification(img$error, type = "error", duration = 8)
      return()
    }
    raw_image(img)
    updateSliderInput(session, "enhance-img_dx", value = 0)
    updateSliderInput(session, "enhance-img_dy", value = 0)
    updateSliderInput(session, "enhance-img_scale", value = 1)
    updateSliderInput(session, "enhance-img_rotate", value = 0)
  })

  observeEvent(input[["enhance-reset_align"]], {
    updateSliderInput(session, "enhance-img_dx", value = 0)
    updateSliderInput(session, "enhance-img_dy", value = 0)
    updateSliderInput(session, "enhance-img_scale", value = 1)
    updateSliderInput(session, "enhance-img_rotate", value = 0)
    updateCheckboxInput(session, "enhance-image_flip", value = FALSE)
    updateCheckboxInput(session, "enhance-image_flip_x", value = FALSE)
  })

  ## Slider ranges have to be in the data's own units, or "left a bit" means
  ## nothing on an object whose coordinates run to 20,000.
  observeEvent(spatial_coords(), {
    co <- spatial_coords()
    req(!is.null(co))
    ## Round to something a person can read: coordinate spans are arbitrary
    ## reals and an unrounded slider shows -93.3687755886931 as its label.
    nice <- function(v) {
      if (!is.finite(v) || v <= 0) {
        return(1)
      }
      signif(v, 2)
    }
    span_x <- nice(diff(range(co$x, na.rm = TRUE)))
    span_y <- nice(diff(range(co$y, na.rm = TRUE)))
    updateSliderInput(
      session,
      "enhance-img_dx",
      min = -span_x,
      max = span_x,
      value = 0,
      step = nice(span_x / 200)
    )
    updateSliderInput(
      session,
      "enhance-img_dy",
      min = -span_y,
      max = span_y,
      value = 0,
      step = nice(span_y / 200)
    )
  })

  ## Encoding is the expensive part. Translation and scale only alter the
  ## extent, so they must not decode, rotate and base64-encode the image again.
  encoded_image <- reactive({
    img <- raw_image()
    req(!is.null(img))
    enc <- builder_encode_image(
      img$array,
      max_px = input[["enhance-image_max_px"]] %||% 1400,
      flip_y = isTRUE(input[["enhance-image_flip"]]),
      flip_x = isTRUE(input[["enhance-image_flip_x"]]),
      rotate = input[["enhance-img_rotate"]] %||% 0
    )
    if (!is.null(enc$error)) {
      return(list(error = enc$error))
    }
    enc
  })

  image_base_bounds <- reactive({
    enc <- encoded_image()
    co <- spatial_coords()
    req(!is.null(co))
    if (!is.null(enc$error)) {
      return(enc)
    }
    b0 <- builder_image_bounds(
      input[["enhance-image_bounds_mode"]] %||% "pixels",
      list(co$x, co$y),
      enc,
      um_per_px = input[["enhance-image_um"]] %||% 1
    )
    if (!is.null(b0$error)) {
      return(list(error = b0$error))
    }
    b0
  })

  ## What the sliders currently describe: a cached encoded picture and a
  ## lightweight extent adjustment.
  aligned <- reactive({
    enc <- encoded_image()
    b0 <- image_base_bounds()
    co <- spatial_coords()
    req(!is.null(co))
    if (!is.null(enc$error)) {
      return(enc)
    }
    if (!is.null(b0$error)) {
      return(b0)
    }
    bounds <- builder_adjust_bounds(
      b0,
      dx = input[["enhance-img_dx"]] %||% 0,
      dy = input[["enhance-img_dy"]] %||% 0,
      scale = input[["enhance-img_scale"]] %||% 1
    )
    cover <- builder_bounds_cover(bounds, list(co$x, co$y))
    list(enc = enc, bounds = bounds, cover = cover)
  })

  output[["enhance-overlay_plot"]] <- plotly::renderPlotly({
    a <- aligned()
    req(is.null(a$error))
    plt <- builder_overlay_plot(spatial_coords(), a$enc$uri, a$bounds)
    req(!is.null(plt))
    plt
  })

  ## What the sliders currently describe, as one section's stored entry.
  current_alignment <- function() {
    a <- aligned()
    if (is.null(a) || !is.null(a$error)) {
      return(a)
    }
    list(
      uri = a$enc$uri,
      bounds = a$bounds,
      bytes = a$enc$bytes,
      width = a$enc$width,
      height = a$enc$height,
      source_width = a$enc$source_width,
      source_height = a$enc$source_height,
      extent_width = a$enc$extent_width,
      extent_height = a$enc$extent_height,
      display_width = a$enc$display_width,
      display_height = a$enc$display_height,
      outside = a$cover$outside,
      total = a$cover$total
    )
  }

  observeEvent(input[["enhance-apply_align"]], {
    id <- current()
    req(id)
    e <- entry_of(id)
    req(e)
    nm <- active_slice()
    req(!is.null(nm))
    a <- current_alignment()
    if (!is.null(a$error)) {
      showNotification(a$error, type = "error", duration = 8)
      return()
    }
    imgs <- e$settings$images %||% list()
    imgs[[nm]] <- a
    commit_enhance_images(e, imgs)
    showNotification(
      paste0("Alignment saved for section “", nm, "”."),
      type = "message",
      duration = 4
    )
  })

  ## Sections cut from one block often share a slide scan. Doing the alignment
  ## by hand for twelve sections is the kind of chore that makes people skip
  ## backgrounds entirely -- but "same slide" is not "same extent". Sections sit
  ## at different offsets in the coordinate space, so the extent is re-derived
  ## per section in the worker; only the picture is shared.
  observeEvent(input[["enhance-apply_align_all"]], {
    id <- current()
    req(id)
    e <- entry_of(id)
    req(e)
    a <- current_alignment()
    if (is.null(a)) {
      return()
    }
    if (!is.null(a$error)) {
      showNotification(a$error, type = "error", duration = 8)
      return()
    }
    ## The encoded picture is kept here; sending 50 kB of base64 to the worker
    ## and back once per section would be pure waste.
    enqueue(list(
      kind = "align_all",
      id = id,
      sections = e$profile$images,
      mode = input[["enhance-image_bounds_mode"]] %||% "pixels",
      extent_width = a$extent_width,
      extent_height = a$extent_height,
      um_per_px = input[["enhance-image_um"]] %||% 1,
      dx = input[["enhance-img_dx"]] %||% 0,
      dy = input[["enhance-img_dy"]] %||% 0,
      scale = input[["enhance-img_scale"]] %||% 1,
      picture = a,
      replaces = "align_all",
      note = paste0(
        "Fitting the image to ",
        length(e$profile$images),
        " sections…"
      )
    ))
  })

  observeEvent(input[["enhance-drop_image"]], {
    id <- current()
    req(id)
    e <- entry_of(id)
    req(e)
    nm <- active_slice()
    imgs <- e$settings$images %||% list()
    if (!is.null(nm)) {
      imgs[[nm]] <- NULL
    }
    commit_enhance_images(e, imgs)
    raw_image(NULL)
  })

  output[["enhance-image_state"]] <- renderUI({
    a <- if (is.null(raw_image())) NULL else aligned()
    id <- current()
    e <- if (is.null(id)) NULL else entry_of(id)
    nm <- active_slice()
    stored <- if (is.null(e)) list() else (e$settings$images %||% list())
    saved <- if (is.null(nm)) NULL else stored[[nm]]
    n_slices <- if (is.null(e)) 0L else length(e$profile$images)

    tagList(
      if (!is.null(a) && !is.null(a$error)) div(class = "notice bad", a$error),
      if (!is.null(a) && is.null(a$error)) {
        div(
          class = if (a$cover$outside > 0) "notice warn" else "notice ok",
          sprintf(
            "%d × %d px, %.2f MB. %s",
            a$enc$width,
            a$enc$height,
            a$enc$bytes / 1048576,
            if (a$cover$outside > 0) {
              sprintf(
                "%d/%d cells fall outside the image.",
                a$cover$outside,
                a$cover$total
              )
            } else {
              "All cells fall inside the image."
            }
          )
        )
      },
      if (!is.null(saved)) {
        builder_enhance_saved_image_ui("enhance", nm, n_slices)
      } else if (is.null(raw_image())) {
        p(class = "hint", "No background image yet.")
      },
      ## With several sections it is not otherwise visible which of them are
      ## still bare -- and a section with no image loses the whole background
      ## picker in the viewer, so the controls appear and disappear as the user
      ## switches. Worth stating rather than discovering.
      if (n_slices > 1) {
        done <- intersect(e$profile$images, names(stored))
        missing <- setdiff(e$profile$images, names(stored))
        div(
          class = "hint",
          style = "margin-top:.5rem",
          if (length(missing)) {
            paste0(
              length(done),
              "/",
              n_slices,
              " sections have images. Missing: ",
              paste(missing, collapse = ", ")
            )
          } else {
            paste0("All ", n_slices, " sections have background images.")
          }
        )
      }
    )
  })

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
    values <- list(
      welcome_message = input[["review-welcome_message"]],
      point_size = input[["review-point_size"]],
      variable_to_compare = input[["review-variable_to_compare"]],
      host = input[["review-host"]],
      port = input[["review-port"]],
      max_request_size = input[["review-max_request_size"]],
      display_mode = input[["review-display_mode"]],
      launch_browser = input[["review-launch_browser"]],
      show_upload_ui = input[["review-show_upload_ui"]]
    )
    if (any(vapply(values, is.null, logical(1)))) {
      return()
    }
    validate_review_inputs(values)
  })

  frozen_review_plan <- reactive({
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
      out_dir = trimws(
        input$out_dir %||% file.path(path.expand("~"), "cerebro")
      ),
      make_app = isTRUE(input$make_app),
      overwrite = isTRUE(input$overwrite),
      app_options = app_options
    )
  })

  review_report <- reactive({
    plan <- frozen_review_plan()
    if (!builder_review_can_build(plan)) {
      return(list(ok = FALSE, msg = plan$error %||% "Review the frozen plan."))
    }
    list(
      ok = TRUE,
      msg = paste0(
        length(plan$items),
        " dataset",
        if (length(plan$items) == 1L) "" else "s",
        " · frozen revision ",
        plan$revision
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

  output$workbench <- renderUI({
    id <- current()
    entry <- isolate(entry_of(id))
    if (is.null(entry)) {
      return(NULL)
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
        "default_group",
        "default_projection",
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
      )
    )
  })

  output$review_stage <- renderUI({
    plan <- frozen_review_plan()
    if (builder_review_can_build(plan)) {
      builder_review_stage_ui("review", builder_review_model(plan, result()))
    } else {
      div(class = "card notice warn", plan$error %||% "Review is not ready.")
    }
  })

  datasets_present <- reactiveVal(FALSE)
  observe({
    present <- length(sets()) > 0L
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
        div(
          class = "grow output-field",
          tags$label(
            class = "visually-hidden",
            `for` = "out_dir",
            "Output directory"
          ),
          textInput(
            "out_dir",
            NULL,
            width = "100%",
            value = isolate(input$out_dir) %||%
              file.path(path.expand("~"), "cerebro")
          )
        ),
        uiOutput("review_action_summary", inline = TRUE),
        make_app_control,
        checkboxInput(
          "overwrite",
          "Replace existing outputs",
          value = isTRUE(isolate(input$overwrite)),
          width = "auto"
        ),
        uiOutput(
          "build_actions",
          inline = TRUE,
          style = "display:inline-flex;align-items:center;gap:1rem"
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
      "Build",
      class = "btn btn-action",
      disabled = if (
        !builder_review_can_build(frozen_review_plan()) ||
          build_in_flight ||
          !protocol_quiescent ||
          !isTRUE(worker_available())
      ) {
        "disabled"
      }
    )
  })

  ## -- build ---------------------------------------------------------------
  ## The whole export runs in the worker: analyses, matrix write, bundle. This
  ## process only sends a plan and waits for the report, so the page keeps
  ## answering while a marker-gene run takes its minutes.
  observeEvent(input$build, {
    rs <- worker()
    req(rs)
    current_protocol <- isolate(protocol())
    req(builder_protocol_is_quiescent(current_protocol))
    plan <- isolate(frozen_review_plan())
    req(builder_review_can_build(plan))
    plan <- unserialize(serialize(plan, NULL, version = 3L))
    result(NULL)
    enqueue(list(
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
  })

  validate_rail_removal <- function(next_state, id) {
    out_dir <- trimws(
      isolate(input$out_dir) %||%
        file.path(
          path.expand("~"),
          "cerebro"
        )
    )
    builder_validate_next_plan(
      next_state,
      out_dir = out_dir,
      make_app = isTRUE(isolate(input$make_app)),
      overwrite = isTRUE(isolate(input$overwrite))
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
