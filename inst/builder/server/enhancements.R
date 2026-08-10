## Builder server: enhancements.

marker_dialog_mode <- reactiveVal("choice")

output[["enhance-marker_dialog_body"]] <- renderUI({
  if (identical(marker_dialog_mode(), "import")) {
    builder_marker_import_pending_ui()
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
    builder_show_marker_dialog("import")
  },
  ignoreInit = TRUE
)

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
