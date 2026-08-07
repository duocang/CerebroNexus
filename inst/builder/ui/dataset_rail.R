##----------------------------------------------------------------------------##
## Persistent dataset rail UI and controller boundaries.
##
## Ordering and selection semantics live in state.R. This file renders the rail,
## validates Shiny events and next-plan removal, reserves input sources, and
## coordinates snapshot alias/release transitions through injected callbacks.
##----------------------------------------------------------------------------##

.builder_rail_or <- function(value, fallback) {
  if (is.null(value)) fallback else value
}

builder_source_key <- function(kind, value) {
  if (
    !is.character(kind) ||
      length(kind) != 1L ||
      is.na(kind) ||
      !kind %in% c("file", "example") ||
      !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(value)
  ) {
    stop("A source key requires one file path or example id.", call. = FALSE)
  }
  normalized <- if (identical(kind, "file")) {
    path <- path.expand(value)
    if (!grepl("^(/|[A-Za-z]:[/\\\\])", path)) {
      path <- file.path(getwd(), path)
    }
    if (file.exists(path)) {
      path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    }
    path <- gsub("\\\\", "/", path)
    prefix <- if (grepl("^[A-Za-z]:/", path)) {
      substr(path, 1L, 3L)
    } else {
      "/"
    }
    parts <- strsplit(sub("^([A-Za-z]:)?/+", "", path), "/", fixed = FALSE)[[
      1L
    ]]
    collapsed <- character()
    for (part in parts) {
      if (!nzchar(part) || identical(part, ".")) {
        next
      }
      if (identical(part, "..")) {
        if (length(collapsed)) collapsed <- head(collapsed, -1L)
      } else {
        collapsed <- c(collapsed, part)
      }
    }
    paste0(prefix, paste(collapsed, collapse = "/"))
  } else {
    value
  }
  paste(kind, normalized, sep = ":")
}

builder_source_release <- function(pending, key) {
  setdiff(as.character(pending), key)
}

builder_source_reserve <- function(entries, pending, kind, value) {
  key <- builder_source_key(kind, value)
  stored <- unlist(
    lapply(entries, function(entry) {
      if (!is.null(entry$example)) {
        return(builder_source_key("example", entry$example))
      }
      if (
        is.character(entry$path) &&
          length(entry$path) == 1L &&
          !is.na(entry$path) &&
          nzchar(entry$path)
      ) {
        return(builder_source_key("file", entry$path))
      }
      NULL
    }),
    use.names = FALSE
  )
  if (key %in% stored) {
    return(list(
      ok = FALSE,
      code = "source_in_store",
      key = key,
      pending = pending
    ))
  }
  pending <- unique(as.character(pending))
  if (key %in% pending) {
    return(list(
      ok = FALSE,
      code = "source_pending",
      key = key,
      pending = pending
    ))
  }
  list(ok = TRUE, code = NULL, key = key, pending = c(pending, key))
}

builder_snapshot_release_transition <- function(
  worker,
  id,
  identity,
  retained,
  pending,
  release,
  unregister,
  identity_of = function(snapshot) snapshot$identity
) {
  stopifnot(
    is.function(release),
    is.function(unregister),
    is.function(identity_of)
  )
  retained_shared <- any(vapply(
    retained,
    function(entry) {
      identical(identity_of(entry$snapshot), identity)
    },
    logical(1)
  ))
  other_ids <- setdiff(names(pending), id)
  pending_shared <- any(vapply(
    pending[other_ids],
    identical,
    logical(1),
    y = identity
  ))
  updated_worker <- if (retained_shared || pending_shared) {
    unregister(worker, id)
  } else {
    release(worker, id, identity)
  }
  updated_pending <- pending
  updated_pending[[id]] <- NULL
  list(worker = updated_worker, pending = updated_pending)
}

.builder_rail_readiness <- function(dataset_state) {
  records <- list(
    ready = list(label = "Ready", icon = "\u2713"),
    needs_attention = list(label = "Needs attention", icon = "!"),
    blocked = list(label = "Blocked", icon = "\u00d7"),
    loading = list(label = "Loading", icon = "\u2026"),
    reload_required = list(label = "Reload required", icon = "\u21bb")
  )
  .builder_rail_or(
    records[[dataset_state$readiness]],
    list(label = "Checking", icon = "\u2026")
  )
}

builder_dataset_is_reviewed <- function(entry) {
  revision <- as.integer(.builder_rail_or(entry$revision, 0L))
  reviewed <- entry$reviewed_revision
  is.numeric(reviewed) &&
    length(reviewed) == 1L &&
    !is.na(reviewed) &&
    identical(as.integer(reviewed), revision)
}

builder_dataset_review_status <- function(entry, active = FALSE) {
  dataset_state <- builder_dataset_state(entry)
  if (!identical(dataset_state$readiness, "ready")) {
    return(list(id = "needs-attention", label = "Needs attention"))
  }
  if (builder_dataset_is_reviewed(entry)) {
    return(list(id = "reviewed", label = "Reviewed"))
  }
  if (isTRUE(active)) {
    return(list(id = "reviewing", label = "Reviewing"))
  }
  list(id = "not-reviewed", label = "Not reviewed")
}

builder_review_progress <- function(entries) {
  reviewed <- vapply(entries, builder_dataset_is_reviewed, logical(1))
  list(
    reviewed = sum(reviewed),
    total = length(entries),
    complete = all(reviewed)
  )
}

builder_next_unreviewed <- function(entries, current_id = NULL) {
  ids <- vapply(entries, `[[`, character(1), "id")
  pending <- ids[!vapply(entries, builder_dataset_is_reviewed, logical(1))]
  pending <- setdiff(pending, .builder_rail_or(current_id, character()))
  if (length(pending)) pending[[1L]] else NULL
}

builder_compact_dataset_review_ui <- function(entries, index, progress) {
  ids <- vapply(entries, `[[`, character(1), "id")
  shiny::tags$nav(
    class = "dataset-compact-review",
    `aria-label` = "Compact dataset review navigation",
    `aria-hidden` = "true",
    shiny::tags$button(
      id = "review_compact_previous_dataset",
      type = "button",
      class = "dataset-compact-step dataset-compact-previous action-button",
      disabled = if (index == 1L) "disabled" else NULL,
      `aria-label` = "Previous dataset",
      "Previous"
    ),
    shiny::div(
      class = "dataset-compact-track",
      lapply(seq_along(entries), function(i) {
        entry <- entries[[i]]
        shiny::tags$button(
          type = "button",
          class = paste(
            c(
              "dataset-compact-segment",
              if (i == index) "is-current",
              if (builder_dataset_is_reviewed(entry)) {
                "is-reviewed"
              } else {
                "is-pending"
              }
            ),
            collapse = " "
          ),
          `data-dataset-id` = entry$id,
          `aria-current` = if (i == index) "step" else NULL,
          `aria-label` = paste0(
            "Dataset ",
            i,
            " of ",
            length(ids),
            ", ",
            .builder_rail_or(entry$settings$name, entry$id)
          ),
          shiny::span(class = "dataset-compact-index", i),
          shiny::span(
            class = "dataset-compact-name",
            .builder_rail_or(entry$settings$name, entry$id)
          )
        )
      })
    ),
    shiny::tags$button(
      id = "review_compact_next_dataset",
      type = "button",
      class = "dataset-compact-step dataset-compact-next action-button",
      disabled = if (index == length(ids)) "disabled" else NULL,
      `aria-label` = "Next dataset",
      "Next"
    ),
    shiny::span(
      class = "dataset-compact-summary",
      paste(progress$reviewed, "of", progress$total, "reviewed")
    )
  )
}

builder_dataset_context_ui <- function(state, current = state$current_dataset) {
  ids <- vapply(state$datasets, `[[`, character(1), "id")
  index <- match(current, ids)
  if (is.na(index)) {
    return(NULL)
  }
  entry <- state$datasets[[index]]
  progress <- builder_review_progress(state$datasets)
  multiple <- length(ids) > 1L
  cells <- .builder_rail_or(entry$profile$n_cells, 0L)
  genes <- .builder_rail_or(
    entry$profile$n_genes,
    .builder_rail_or(entry$profile$n_features, 0L)
  )
  context <- shiny::div(
    class = paste("dataset-context", if (multiple) "is-multiple" else ""),
    tabindex = "-1",
    shiny::div(
      class = "dataset-context-copy",
      if (multiple) {
        shiny::span(
          class = "dataset-context-position",
          paste("Dataset", index, "of", length(ids))
        )
      },
      shiny::h2(class = "dataset-context-title", entry$settings$name),
      shiny::span(
        class = "dataset-context-counts",
        paste0(
          format(cells, big.mark = ","),
          " cells · ",
          format(genes, big.mark = ","),
          " genes"
        )
      )
    ),
    if (multiple) {
      shiny::div(
        class = "dataset-review-progress",
        shiny::span(
          paste(progress$reviewed, "of", progress$total, "datasets reviewed")
        ),
        shiny::div(
          class = "dataset-review-progress-track",
          role = "progressbar",
          `aria-label` = "Datasets reviewed",
          `aria-valuemin` = "0",
          `aria-valuemax` = progress$total,
          `aria-valuenow` = progress$reviewed,
          shiny::span(
            style = paste0(
              "width:",
              if (progress$total) {
                100 * progress$reviewed / progress$total
              } else {
                0
              },
              "%"
            )
          )
        )
      )
    },
    if (multiple) {
      shiny::div(
        class = "dataset-context-navigation",
        shiny::tags$button(
          id = "review_previous_dataset",
          type = "button",
          class = "btn btn-default action-button",
          disabled = if (index == 1L) "disabled" else NULL,
          "Previous"
        ),
        shiny::tags$button(
          id = "review_next_dataset",
          type = "button",
          class = "btn btn-default action-button",
          disabled = if (index == length(ids)) "disabled" else NULL,
          "Next"
        )
      )
    }
  )
  if (!multiple) {
    return(context)
  }
  shiny::tagList(
    context,
    builder_compact_dataset_review_ui(state$datasets, index, progress)
  )
}

builder_dataset_remove_requires_confirmation <- function(entry) {
  settings <- entry$settings
  spatial <- .builder_rail_or(entry$spatial_drafts, list())
  length(.builder_rail_or(settings, list())) > 0L || length(spatial) > 0L
}

.builder_rail_validation <- function(
  ok,
  code = NULL,
  message = NULL,
  dataset_ids = character(),
  expected_members = character(),
  plan = NULL
) {
  structure(
    list(
      ok = isTRUE(ok),
      code = code,
      message = message,
      dataset_ids = dataset_ids,
      expected_members = expected_members,
      plan = plan
    ),
    class = c("builder_rail_validation", "list")
  )
}

builder_validate_next_plan <- function(
  state,
  out_dir,
  make_app = FALSE,
  overwrite = FALSE,
  freeze_plan = builder_freeze_plan
) {
  entries <- builder_datasets_for_plan(state)
  dataset_ids <- vapply(entries, `[[`, character(1), "id")
  if (!length(dataset_ids)) {
    return(.builder_rail_validation(
      TRUE,
      code = "empty_release",
      message = "Removing this row leaves no release members.",
      dataset_ids = character(),
      expected_members = character()
    ))
  }
  if (
    !is.character(out_dir) ||
      length(out_dir) != 1L ||
      is.na(out_dir) ||
      !nzchar(trimws(out_dir))
  ) {
    return(.builder_rail_validation(
      FALSE,
      code = "missing_output_target",
      message = paste0(
        "Choose an output directory before removing a dataset so the next ",
        "frozen release can be verified."
      ),
      dataset_ids = dataset_ids
    ))
  }
  if (!is.function(freeze_plan)) {
    return(.builder_rail_validation(
      FALSE,
      code = "missing_plan_authority",
      message = "The frozen-plan authority is unavailable.",
      dataset_ids = dataset_ids
    ))
  }
  plan <- freeze_plan(
    entries = entries,
    out_dir = trimws(out_dir),
    make_app = isTRUE(make_app),
    overwrite = isTRUE(overwrite),
    app_options = builder_app_options_for_plan(state)
  )
  if (!is.list(plan)) {
    return(.builder_rail_validation(
      FALSE,
      code = "invalid_next_plan",
      message = "The next frozen release plan is invalid.",
      dataset_ids = dataset_ids
    ))
  }
  if (!is.null(plan$error)) {
    return(.builder_rail_validation(
      FALSE,
      code = .builder_rail_or(plan$error_code, "invalid_next_plan"),
      message = .builder_rail_or(
        plan$error,
        "The next frozen release plan is invalid."
      ),
      dataset_ids = dataset_ids,
      plan = plan
    ))
  }
  targets <- plan$output_release$targets
  if (
    !identical(plan$dataset_order, dataset_ids) ||
      !is.character(targets) ||
      anyNA(targets) ||
      any(!nzchar(targets)) ||
      anyDuplicated(targets)
  ) {
    return(.builder_rail_validation(
      FALSE,
      code = "invalid_owned_member_set",
      message = paste0(
        "The next frozen plan does not define the exact ordered datasets ",
        "and owned release members."
      ),
      dataset_ids = dataset_ids,
      plan = plan
    ))
  }
  .builder_rail_validation(
    TRUE,
    dataset_ids = dataset_ids,
    expected_members = targets,
    plan = plan
  )
}

.builder_rail_remove_event <- function(value) {
  if (is.character(value) && length(value) == 1L) {
    return(list(id = value, confirmed = FALSE))
  }
  if (!is.list(value) || is.object(value)) {
    return(NULL)
  }
  list(
    id = .subset2(value, "id"),
    confirmed = isTRUE(.subset2(value, "confirmed"))
  )
}

.builder_rail_dataset_id <- function(value, ids) {
  if (
    !is.character(value) ||
      is.object(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.null(attributes(value)) ||
      !nzchar(trimws(value)) ||
      !value %in% ids
  ) {
    return(NULL)
  }
  value
}

builder_dataset_rail_server <- function(
  input,
  session,
  store,
  validate_remove,
  on_select = function(...) invisible(NULL),
  on_remove = function(...) invisible(NULL),
  on_undo = function(...) invisible(NULL),
  on_validation = function(...) invisible(NULL)
) {
  stopifnot(
    is.function(store),
    is.function(validate_remove),
    is.function(on_select),
    is.function(on_remove),
    is.function(on_undo),
    is.function(on_validation)
  )
  validation <- shiny::reactiveVal(.builder_rail_validation(
    FALSE,
    code = "not_run",
    message = "No removal has been requested."
  ))

  shiny::observeEvent(input$pick, {
    state <- shiny::isolate(store())
    ids <- vapply(state$datasets, `[[`, character(1), "id")
    id <- .builder_rail_dataset_id(input$pick, ids)
    if (is.null(id)) {
      return()
    }
    store(builder_reduce_state(
      state,
      list(type = "select", id = id)
    ))
    on_select(id)
  })
  shiny::observeEvent(input$reorder_ds, {
    event <- input$reorder_ds
    state <- shiny::isolate(store())
    ids <- vapply(state$datasets, `[[`, character(1), "id")
    if (
      !is.list(event) ||
        is.object(event) ||
        !identical(names(event), c("id", "direction")) ||
        !is.character(event$id) ||
        length(event$id) != 1L ||
        is.na(event$id) ||
        !is.null(attributes(event$id)) ||
        !event$id %in% ids ||
        !is.character(event$direction) ||
        length(event$direction) != 1L ||
        is.na(event$direction) ||
        !is.null(attributes(event$direction)) ||
        !event$direction %in% c("up", "down")
    ) {
      return()
    }
    store(builder_reduce_state(
      state,
      list(type = "move", id = event$id, direction = event$direction)
    ))
  })
  shiny::observeEvent(input$drop_ds, {
    event <- .builder_rail_remove_event(input$drop_ds)
    if (is.null(event)) {
      return()
    }
    previous <- shiny::isolate(store())
    ids <- vapply(previous$datasets, `[[`, character(1), "id")
    id <- .builder_rail_dataset_id(event$id, ids)
    if (is.null(id)) {
      return()
    }
    entry <- previous$datasets[[match(id, ids)]]
    if (
      builder_dataset_remove_requires_confirmation(entry) &&
        !isTRUE(event$confirmed)
    ) {
      result <- .builder_rail_validation(
        FALSE,
        code = "confirmation_required",
        message = "Confirm removal before changing the frozen release."
      )
      validation(result)
      on_validation(result)
      return()
    }
    next_state <- builder_reduce_state(
      previous,
      list(type = "remove", id = id)
    )
    result <- validate_remove(next_state, id)
    if (!inherits(result, "builder_rail_validation")) {
      result <- .builder_rail_validation(
        FALSE,
        code = "invalid_plan_validation",
        message = "Removal validation returned an invalid result."
      )
    }
    validation(result)
    on_validation(result)
    if (!isTRUE(result$ok)) {
      return()
    }
    store(next_state)
    on_remove(previous, next_state, id, result)
  })
  shiny::observeEvent(input$undo_remove, {
    state <- shiny::isolate(store())
    if (!isTRUE(state$can_undo_remove)) {
      return()
    }
    store(builder_reduce_state(state, list(type = "undo_remove")))
    on_undo()
  })

  list(
    state = shiny::reactive(store()),
    validation = shiny::reactive(validation())
  )
}

builder_pending_dataset_files_ui <- function(files) {
  files <- Filter(function(file) isTRUE(file$visible %||% TRUE), files)
  if (!length(files)) {
    return(NULL)
  }
  shiny::div(
    class = "builder-file-list rail-pending-files",
    `aria-label` = "Files being added",
    `aria-live` = "polite",
    lapply(files, function(file) {
      filename <- builder_safe_file_name(file$filename, "Dataset file")
      detail <- paste(
        builder_file_type_label(filename, file$type),
        builder_file_human_size(file$size %||% NA_real_),
        sep = " · "
      )
      shiny::div(
        class = "builder-file-item rail-pending-file",
        shiny::div(
          class = "rail-pending-file-meta",
          shiny::strong(filename),
          shiny::span(class = "hint", detail)
        ),
        shiny::div(
          class = "builder-action-row",
          shiny::span(
            class = "builder-status builder-status--reading",
            "Reading…"
          ),
          shiny::tags$button(
            type = "button",
            class = "btn btn-remove-soft pending-upload-remove",
            `data-upload-id` = file$id,
            `aria-label` = paste("Cancel adding", filename),
            "Remove"
          )
        )
      )
    })
  )
}

builder_empty_workbench_ui <- function() {
  shiny::tags$section(
    class = "builder-stage builder-empty-state",
    `aria-labelledby` = "builder-empty-title",
    shiny::h2(id = "builder-empty-title", "Add a dataset to begin"),
    shiny::p(
      "Choose a local Seurat object or try one of the examples in the sidebar."
    )
  )
}

builder_loading_workbench_ui <- function(entry) {
  stopifnot(inherits(entry, "builder_import_entry"))
  failed <- identical(entry$load_state, "error")
  shiny::tags$section(
    class = paste(
      "builder-stage builder-loading-stage",
      if (failed) "is-error" else NULL
    ),
    `aria-live` = "polite",
    `aria-atomic` = "true",
    shiny::div(
      class = "builder-loading-copy",
      shiny::span(
        class = "builder-loading-kicker",
        if (failed) "Import stopped" else "Dataset import"
      ),
      shiny::h2(if (failed) "Could not load dataset" else "Loading dataset"),
      shiny::p(class = "builder-loading-name", entry$label),
      shiny::p(
        class = "builder-loading-status",
        if (failed) entry$error else entry$progress_label
      )
    ),
    if (!failed) {
      shiny::div(
        class = "builder-loading-progress",
        role = "progressbar",
        `aria-label` = entry$progress_label,
        `aria-valuetext` = entry$progress_label,
        shiny::span()
      )
    },
    shiny::div(
      class = "builder-action-row builder-loading-actions",
      if (failed) {
        shiny::tags$button(
          type = "button",
          class = "btn builder-retry-import",
          `data-import-id` = entry$id,
          "Retry"
        )
      },
      shiny::tags$button(
        type = "button",
        class = "btn btn-remove-soft builder-remove-import",
        `data-import-id` = entry$id,
        if (failed) "Remove dataset" else "Remove"
      )
    )
  )
}

builder_import_rail_ui <- function(entries, current = NULL) {
  if (!length(entries)) {
    return(NULL)
  }
  shiny::div(
    class = "builder-import-list",
    `aria-label` = "Datasets being added",
    lapply(entries, function(entry) {
      stopifnot(inherits(entry, "builder_import_entry"))
      active <- identical(entry$id, current)
      failed <- identical(entry$load_state, "error")
      importing <- !failed
      detail <- Filter(
        function(value) {
          is.character(value) && length(value) == 1L && nzchar(value)
        },
        list(entry$filename, entry$file_type)
      )
      shiny::div(
        class = paste(
          c(
            "ds ds--import",
            if (active) "is-active",
            if (importing) "is-importing",
            if (failed) "is-error"
          ),
          collapse = " "
        ),
        `data-import-id` = entry$id,
        `data-load-state` = entry$load_state,
        shiny::tags$button(
          type = "button",
          class = "ds-pick builder-pick-import",
          `data-import-id` = entry$id,
          `aria-label` = if (failed) {
            paste("Open failed import", entry$label)
          } else {
            paste("Open loading dataset", entry$label)
          },
          `aria-current` = if (active) "true" else NULL,
          shiny::span(class = "ds-state-dot", `aria-hidden` = "true"),
          shiny::span(
            class = "ds-body",
            shiny::span(class = "nm", entry$label),
            if (length(detail)) {
              shiny::span(
                class = "meta",
                paste(unlist(detail), collapse = " · ")
              )
            },
            shiny::span(
              class = paste(
                "builder-import-status",
                paste0("is-", entry$load_state)
              ),
              entry$progress_label
            )
          )
        ),
        shiny::div(
          class = "ds-actions",
          if (failed) {
            shiny::tags$button(
              type = "button",
              class = "ds-move builder-retry-import",
              `data-import-id` = entry$id,
              "Retry"
            )
          },
          shiny::tags$button(
            type = "button",
            class = "ds-del btn-remove-soft builder-remove-import",
            `data-import-id` = entry$id,
            `aria-label` = if (failed) {
              paste("Remove failed import", entry$label)
            } else {
              paste("Cancel loading", entry$label)
            },
            "Remove"
          )
        )
      )
    })
  )
}

builder_dataset_rail_ui <- function(state, current = state$current_dataset) {
  ids <- .builder_store_assert(state)
  if (!length(ids)) {
    return(shiny::div(
      class = "rail-empty",
      "No datasets yet. Add one below."
    ))
  }

  shiny::tagList(
    lapply(seq_along(state$datasets), function(index) {
      entry <- state$datasets[[index]]
      label <- .builder_rail_or(entry$settings$name, entry$id)
      cells <- .builder_rail_or(entry$profile$n_cells, 0L)
      confirm <- builder_dataset_remove_requires_confirmation(entry)
      active <- identical(entry$id, current)
      review_status <- builder_dataset_review_status(entry, active)

      shiny::div(
        class = paste(c("ds", if (active) "is-active"), collapse = " "),
        `data-ds` = entry$id,
        shiny::tags$button(
          class = "ds-pick builder-pick",
          id = paste0("pick_", entry$id),
          `data-ds` = entry$id,
          `aria-label` = paste0("Open ", label),
          `aria-current` = if (active) "true" else NULL,
          shiny::span(class = "ds-idx", index),
          shiny::span(
            class = "ds-body",
            shiny::span(class = "nm", label),
            shiny::span(
              class = if (identical(review_status$id, "needs-attention")) {
                "meta bad"
              } else {
                "meta"
              },
              sprintf(
                "%s cells \u00b7 %s",
                format(cells, big.mark = ","),
                entry$format
              ),
              shiny::span(
                class = paste("rail-review-status", review_status$id),
                `data-review-status` = review_status$id,
                review_status$label
              )
            )
          )
        ),
        shiny::div(
          class = "ds-actions",
          shiny::tags$button(
            class = "ds-move builder-reorder",
            title = "Move dataset up",
            `aria-label` = paste0("Move ", label, " up"),
            `data-ds` = entry$id,
            `data-direction` = "up",
            `data-icon` = "up",
            disabled = if (index == 1L) "disabled" else NULL,
            shiny::span(class = "ds-action-label", "Move up")
          ),
          shiny::tags$button(
            class = "ds-move builder-reorder",
            title = "Move dataset down",
            `aria-label` = paste0("Move ", label, " down"),
            `data-ds` = entry$id,
            `data-direction` = "down",
            `data-icon` = "down",
            disabled = if (index == length(ids)) "disabled" else NULL,
            shiny::span(class = "ds-action-label", "Move down")
          ),
          shiny::tags$button(
            class = "ds-del btn-remove-soft builder-drop",
            title = "Remove this dataset",
            `aria-label` = paste0("Remove ", label),
            `data-ds` = entry$id,
            `data-confirm` = if (confirm) "true" else "false",
            shiny::span(class = "ds-action-label", "Remove")
          )
        )
      )
    })
  )
}
