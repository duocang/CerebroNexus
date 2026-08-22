## Builder server: enhancements.

marker_dialog_mode <- reactiveVal("choice")

output[["enhance-marker_dialog_body"]] <- renderUI({
  if (identical(marker_dialog_mode(), "import")) {
    id <- current()
    req(id)
    entry <- isolate(entry_of(id))
    req(entry)
    groups <- entry$settings$included_groups %||%
      entry$settings$groups %||%
      names(entry$levels %||% list())
    builder_marker_import_ui(
      "enhance",
      groups = groups,
      draft = marker_import_draft_of(id)
    )
  } else {
    builder_marker_source_choice_ui("enhance")
  }
})

builder_show_marker_dialog <- function(mode = "choice") {
  marker_dialog_mode(mode)
  session$sendCustomMessage(
    "builder_marker_dialog",
    list(
      action = "open",
      title = if (identical(mode, "import")) {
        "Upload Marker gene results"
      } else {
        "Add Marker genes"
      }
    )
  )
}

builder_close_marker_dialog <- function() {
  session$sendCustomMessage(
    "builder_marker_dialog",
    list(action = "close")
  )
}

observeEvent(
  input[["enhance-analysis_marker_genes_action"]],
  {
    id <- current()
    req(id)
    entry <- entry_of(id)
    req(entry)
    selected <- entry$settings$analyses %||% character()
    imported <- entry$settings$marker_imports %||% list()
    if ("marker_genes" %in% selected || length(imported)) {
      entry$settings$analyses <- setdiff(selected, "marker_genes")
      entry$settings$marker_imports <- NULL
      replace_entry(entry)
      return()
    }
    builder_show_marker_dialog("choice")
  },
  ignoreInit = TRUE
)

observeEvent(
  input[["enhance-marker_genes_calculate"]],
  {
    id <- current()
    req(id)
    entry <- entry_of(id)
    req(entry)
    selected <- unique(c(
      entry$settings$analyses %||% character(),
      "marker_genes"
    ))
    entry$settings$analyses <- builder_normalize_analyses(
      selected,
      builder_profile_has(entry$profile, "marker_genes")
    )
    entry$settings$marker_imports <- NULL
    replace_entry(entry)
    builder_close_marker_dialog()
  },
  ignoreInit = TRUE
)

observeEvent(
  input[["enhance-marker_genes_upload"]],
  {
    id <- current()
    req(id)
    replace_marker_import_draft(id, NULL)
    builder_show_marker_dialog("import")
  },
  ignoreInit = TRUE
)

builder_marker_existing_methods <- function(entry) {
  imported <- entry$settings$marker_imports %||% list()
  imported_methods <- vapply(
    imported,
    function(record) as.character(record$method %||% ""),
    character(1)
  )
  content <- entry$dataset_profile$content$marker_genes %||%
    entry$profile$content$marker_genes %||%
    list()
  existing <- names(content$normalized %||% list()) %||% character()
  unique(c(existing, imported_methods[nzchar(imported_methods)]))
}

observeEvent(input[["enhance-marker_import_files"]], {
  id <- current()
  req(id)
  entry <- entry_of(id)
  req(entry)
  method <- trimws(as.character(
    input[["enhance-marker_import_method"]] %||% ""
  ))
  group <- as.character(input[["enhance-marker_import_group"]] %||% "")
  groups <- entry$settings$included_groups %||%
    entry$settings$groups %||%
    names(entry$levels %||% list())
  if (!nzchar(method) || !group %in% groups) {
    showNotification(
      "Enter a method name and choose Groups before adding files.",
      type = "error"
    )
    return()
  }
  uploads <- input[["enhance-marker_import_files"]]
  req(is.data.frame(uploads), nrow(uploads) > 0L)
  sources <- builder_marker_import_inventory(
    uploads$datapath,
    uploads$name,
    uploads$size
  )
  draft <- builder_marker_import_new_draft(
    id = paste0("marker-import-", id, "-", isolate(store()$revision)),
    method = method,
    group = group,
    sources = sources,
    known_levels = entry$levels[[group]] %||% character(),
    existing_methods = builder_marker_existing_methods(entry)
  )
  replace_marker_import_draft(id, draft)
})

observeEvent(input[["enhance-marker_source_mode"]], {
  id <- current()
  req(id)
  action <- input[["enhance-marker_source_mode"]]
  req(is.list(action), nzchar(action$id %||% ""))
  draft <- marker_import_draft_of(id)
  req(draft)
  index <- which(vapply(
    draft$sources,
    function(source) identical(source$id, action$id),
    logical(1)
  ))
  if (length(index) != 1L || !action$mode %in% c("single", "multiple")) {
    return()
  }
  source <- draft$sources[[index]]
  source$mapping <- action$mode
  source$table <- NULL
  source$levels <- character()
  source$confirmed <- FALSE
  source$status <- "mapping_required"
  source$error <- NULL
  if (identical(action$mode, "single") && is.null(source$cluster)) {
    source$cluster <- if (length(draft$known_levels)) {
      draft$known_levels[[1L]]
    } else {
      NULL
    }
  }
  if (identical(action$mode, "multiple") && is.null(source$cluster_column)) {
    source$cluster_column <- if (length(source$columns)) {
      source$columns[[1L]]
    } else {
      NULL
    }
  }
  draft$sources[[index]] <- source
  replace_marker_import_draft(
    id,
    builder_marker_import_refresh_draft(draft)
  )
})

observeEvent(input[["enhance-marker_source_confirm"]], {
  id <- current()
  req(id)
  action <- input[["enhance-marker_source_confirm"]]
  req(is.list(action), nzchar(action$id %||% ""))
  draft <- marker_import_draft_of(id)
  req(draft)
  mode <- as.character(
    input[[paste0("enhance-marker_source_mode_", action$id)]] %||% "single"
  )
  value <- if (identical(mode, "multiple")) {
    input[[paste0("enhance-marker_source_column_", action$id)]]
  } else {
    input[[paste0("enhance-marker_source_cluster_", action$id)]]
  }
  got <- try(
    builder_marker_import_confirm_source(draft, action$id, mode, value),
    silent = TRUE
  )
  if (inherits(got, "try-error")) {
    showNotification(
      "This source mapping could not be confirmed.",
      type = "error"
    )
    return()
  }
  replace_marker_import_draft(id, got)
})

observeEvent(input[["enhance-marker_import_save"]], {
  id <- current()
  req(id)
  entry <- entry_of(id)
  draft <- marker_import_draft_of(id)
  req(entry, draft)
  validation <- builder_marker_import_validate(
    draft$method,
    draft$group,
    draft$sources,
    draft$known_levels,
    builder_marker_existing_methods(entry)
  )
  if (!isTRUE(validation$ready)) {
    showNotification(
      "Resolve every Marker import source before saving.",
      type = "error"
    )
    return()
  }
  draft$validation <- validation
  draft$ready <- TRUE
  record <- builder_freeze_marker_imports(list(draft))[[1L]]
  imports <- entry$settings$marker_imports %||% list()
  imports[[record$id]] <- record
  entry$settings$marker_imports <- imports
  entry$settings$analyses <- setdiff(
    entry$settings$analyses %||% character(),
    "marker_genes"
  )
  replace_entry(entry)
  replace_marker_import_draft(id, NULL)
  builder_close_marker_dialog()
})

## -- supplementary tables -------------------------------------------------
enhance_table_ui_revision <- reactiveVal(0L)
refresh_enhance_tables <- function() {
  enhance_table_ui_revision(isolate(enhance_table_ui_revision()) + 1L)
}

observeEvent(input[["enhance-table_files"]], {
  id <- current()
  req(id)
  entry <- entry_of(id)
  req(entry)
  uploads <- input[["enhance-table_files"]]
  req(is.data.frame(uploads), nrow(uploads) > 0L)
  added <- list()
  for (index in seq_len(nrow(uploads))) {
    filename <- basename(uploads$name[[index]])
    records <- builder_table_inventory(
      uploads$datapath[[index]],
      filename = filename
    )
    if (!length(records) || !is.null(records$error)) {
      showNotification(
        records$error %||% paste0(filename, ": No worksheets were found."),
        type = "error",
        duration = 8
      )
      next
    }
    for (got in records) {
      got$name <- builder_table_unique_name(
        got$name,
        names(entry$settings$tables %||% list()) %||% character()
      )
      got$file_name <- filename
      got$file_type <- toupper(tools::file_ext(filename))
      got$file_size <- suppressWarnings(as.numeric(uploads$size[[index]]))
      got$source_path <- uploads$datapath[[index]]
      got$workbook_name <- got$workbook_name %||% filename
      got$sheet_name <- got$sheet_name %||% got$sheet %||% got$name
      got$display_name <- got$display_name %||% got$sheet_name
      entry$settings$tables[[got$name]] <- got
      added[[filename]] <- (added[[filename]] %||% 0L) + 1L
    }
  }
  if (isTRUE(replace_entry(entry))) {
    refresh_enhance_tables()
  }
  if (length(added)) {
    session$onFlushed(
      function() {
        session$sendCustomMessage(
          "enhance_tables_added",
          list(
            workbooks = unname(lapply(names(added), function(key) {
              list(key = key, count = unname(added[[key]]))
            }))
          )
        )
      },
      once = TRUE
    )
  }
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
    reopen_workbook <- NULL
    saved_attachment <- NULL
    structural_change <- FALSE
    workbook_rows <- vapply(
      tables,
      function(table) identical(table$file_name %||% "", action$key),
      logical(1)
    )
    if (identical(action$action, "remove_workbook")) {
      if (!any(workbook_rows)) {
        return()
      }
      tables <- tables[!workbook_rows]
      structural_change <- TRUE
    } else if (identical(action$action, "rename_workbook")) {
      new_name <- trimws(as.character(action$name %||% ""))
      other_names <- unique(vapply(
        tables[!workbook_rows],
        function(table) table$workbook_name %||% table$file_name %||% "",
        character(1)
      ))
      if (
        !any(workbook_rows) || !nzchar(new_name) || new_name %in% other_names
      ) {
        showNotification(
          "Workbook names must be non-empty and unique.",
          type = "error",
          duration = 5
        )
        return()
      }
      tables[workbook_rows] <- lapply(tables[workbook_rows], function(table) {
        table$workbook_name <- new_name
        table
      })
      reopen_workbook <- action$key
      saved_attachment <- list(
        kind = "workbook",
        key = action$key,
        name = new_name
      )
    } else if (!action$key %in% names(tables)) {
      return()
    } else if (identical(action$action, "remove")) {
      tables[[action$key]] <- NULL
      structural_change <- TRUE
    } else if (identical(action$action, "rename")) {
      new_name <- trimws(as.character(action$name %||% ""))
      if (!nzchar(new_name)) {
        showNotification(
          "Table names must be non-empty.",
          type = "error",
          duration = 5
        )
        return()
      }
      table <- tables[[action$key]]
      table$display_name <- new_name
      tables[[action$key]] <- table
      reopen_workbook <- table$file_name
      saved_attachment <- list(
        kind = "table",
        key = action$key,
        name = new_name
      )
    } else {
      return()
    }
    entry$settings$tables <- tables
    changed <- replace_entry(entry)
    if (!isTRUE(changed)) {
      return()
    }
    if (isTRUE(structural_change)) {
      refresh_enhance_tables()
    }
    if (!is.null(saved_attachment)) {
      session$sendCustomMessage(
        "enhance_attachment_saved",
        saved_attachment
      )
    }
    if (!is.null(reopen_workbook)) {
      session$onFlushed(
        function() {
          session$sendCustomMessage(
            "enhance_workbook_reopen",
            list(key = reopen_workbook)
          )
        },
        once = TRUE
      )
    }
  },
  ignoreInit = TRUE
)

alignment_server <- builder_spatial_alignment_server(
  input = input,
  output = output,
  session = session,
  current = current,
  entry_of = entry_of,
  entries = sets,
  worker = worker,
  enqueue = enqueue,
  commit_images = function(entry, images) {
    commit_enhance_images(entry, images, internal = TRUE)
  },
  alignment_preview = alignment_preview,
  spatial_previews = spatial_previews,
  spatial_coords = spatial_coords
)
active_slice <- alignment_server$active_section

observeEvent(
  input[["enhance-spatial_image_storage"]],
  {
    id <- current()
    entry <- if (is.null(id)) NULL else isolate(entry_of(id))
    storage <- input[["enhance-spatial_image_storage"]]
    if (
      is.null(entry) ||
        !is.character(storage) ||
        length(storage) != 1L ||
        !storage %in% c("external", "embedded") ||
        identical(entry$settings$spatial_image_storage, storage)
    ) {
      return()
    }
    entry$settings$spatial_image_storage <- storage
    replace_entry(entry)
  },
  ignoreInit = TRUE
)

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
observeEvent(
  input$retry_failed_analysis,
  {
    retry_confirmed_build()
  },
  priority = 100
)
observeEvent(
  input$remove_failed_analysis,
  {
    current_result <- isolate(result())
    dataset_id <- current_result$failed_dataset_id %||% NULL
    if (!builder_stage_has_text(dataset_id %||% "")) {
      showNotification(
        "The failed optional work could not be removed.",
        type = "error"
      )
      return()
    }
    failed <- current_result$retry_closure %||% character()
    entry <- isolate(entry_of(dataset_id))
    removable <- if (is.null(entry)) {
      character()
    } else {
      intersect(entry$settings$analyses %||% character(), failed)
    }
    if (!length(removable)) {
      showNotification(
        "The failed optional work could not be removed.",
        type = "error"
      )
      return()
    }
    entry$settings$analyses <- setdiff(entry$settings$analyses, removable)
    changed <- try(replace_entry(entry), silent = TRUE)
    if (inherits(changed, "try-error") || !isTRUE(changed)) {
      showNotification(
        "The failed optional work could not be removed.",
        type = "error"
      )
      return()
    }
    builder_build_recovery_needs_fresh_review(
      "Optional work removed. Review the updated plan before building."
    )
  },
  priority = 100
)
observeEvent(
  input$restart_worker,
  {
    current_result <- isolate(result())
    req(inherits(current_result, "builder_result"))
    req(isTRUE(current_result$restartable_worker))
    current_worker <- isolate(worker())
    current_protocol <- isolate(protocol())
    req(current_worker, current_protocol)
    restarted <- try(
      restart_worker_protocol(
        current_worker,
        current_protocol,
        "The worker was restarted from saved snapshots."
      ),
      silent = TRUE
    )
    if (inherits(restarted, "try-error") || !isTRUE(restarted)) {
      showNotification(
        "The background worker could not restart. Try again or restart this Builder session.",
        type = "error"
      )
      return()
    }
    if (isTRUE(builder_build_recovery_ready())) {
      result(NULL)
      build_flow(list(stage = "idle", plan = NULL))
    }
  },
  priority = 100
)
