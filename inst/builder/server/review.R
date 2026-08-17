## Builder server: review.

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
  if (builder_mutations_locked(isolate(build_flow()), isolate(protocol()))) {
    return()
  }
  current_options <- isolate(review_options())
  values <- list(
    welcome_message = input[["build_welcome_message"]],
    initial_page = input[["build_initial_page"]],
    point_size = current_options$point_size,
    variable_to_compare = current_options$variable_to_compare,
    host = input[["build_host"]],
    port = input[["build_port"]],
    max_request_size = current_options$max_request_size,
    display_mode = current_options$display_mode,
    launch_browser = input[["build_launch_browser"]],
    show_upload_ui = input[["build_show_upload_ui"]]
  )
  if (any(vapply(values, is.null, logical(1)))) {
    return()
  }
  validate_review_inputs(values)
})

observeEvent(
  input[["build_require_login"]],
  {
    if (builder_mutations_locked(isolate(build_flow()), isolate(protocol()))) {
      return()
    }
    enabled <- isTRUE(input[["build_require_login"]])
    if (
      enabled && (!isTRUE(build_mode()) || !isTRUE(auth_capability$available))
    ) {
      auth_enabled(FALSE)
      auth_accounts(builder_auth_empty_accounts())
      auth_validation(list(
        ok = FALSE,
        error = "Login is unavailable for the current App settings."
      ))
      session$sendCustomMessage("builder_auth_reset", list(reset = TRUE))
      return()
    }
    auth_enabled(enabled)
    if (!enabled) {
      auth_accounts(builder_auth_empty_accounts())
      auth_validation(list(ok = TRUE, error = NULL))
      session$sendCustomMessage("builder_auth_reset", list(reset = TRUE))
      return()
    }
    parsed <- builder_auth_validate_payload(TRUE, isolate(auth_accounts()))
    auth_validation(list(ok = parsed$ok, error = parsed$error))
  },
  ignoreInit = TRUE
)

observeEvent(
  input$builder_auth_accounts,
  {
    if (builder_mutations_locked(isolate(build_flow()), isolate(protocol()))) {
      return()
    }
    payload <- input$builder_auth_accounts
    if (
      is.null(payload) ||
        !is.list(payload) ||
        is.object(payload) ||
        !identical(
          sort(names(payload)),
          c("accounts", "enabled", "nonce")
        ) ||
        !is.logical(payload$enabled) ||
        length(payload$enabled) != 1L ||
        is.na(payload$enabled) ||
        !is.numeric(payload$nonce) ||
        length(payload$nonce) != 1L ||
        is.na(payload$nonce) ||
        !is.finite(payload$nonce)
    ) {
      return()
    }
    if (
      isTRUE(payload$enabled) &&
        (!isTRUE(build_mode()) || !isTRUE(auth_capability$available))
    ) {
      auth_enabled(FALSE)
      auth_accounts(builder_auth_empty_accounts())
      auth_validation(list(
        ok = FALSE,
        error = "Login accounts could not be saved."
      ))
      session$sendCustomMessage(
        "builder_auth_status",
        list(
          ok = FALSE,
          message = "Login accounts could not be saved.",
          nonce = payload$nonce
        )
      )
      session$sendCustomMessage("builder_auth_reset", list(reset = TRUE))
      return()
    }
    parsed <- builder_auth_validate_payload(
      isTRUE(payload$enabled),
      payload$accounts
    )
    if (!isTRUE(parsed$ok)) {
      auth_validation(list(
        ok = FALSE,
        error = "Login accounts could not be saved."
      ))
      session$sendCustomMessage(
        "builder_auth_status",
        list(
          ok = FALSE,
          message = "Login accounts could not be saved.",
          nonce = payload$nonce
        )
      )
      return()
    }
    auth_validation(list(ok = TRUE, error = NULL))
    auth_enabled(isTRUE(payload$enabled))
    auth_accounts(parsed$accounts)
    session$sendCustomMessage(
      "builder_auth_status",
      list(
        ok = TRUE,
        account_count = length(parsed$accounts),
        nonce = payload$nonce
      )
    )
    if (!isTRUE(payload$enabled)) {
      session$sendCustomMessage("builder_auth_reset", list(reset = TRUE))
    }
  },
  ignoreInit = TRUE
)

observeEvent(
  build_mode(),
  {
    if (isTRUE(build_mode())) {
      return()
    }
    auth_enabled(FALSE)
    auth_accounts(builder_auth_empty_accounts())
    auth_validation(list(ok = TRUE, error = NULL))
    session$sendCustomMessage("builder_auth_reset", list(reset = TRUE))
  },
  ignoreInit = TRUE
)

freeze_plan_for_output <- function(
  out_dir,
  overwrite = FALSE,
  output_options = NULL,
  entries_override = NULL
) {
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
  all <- entries_override %||% sets()
  if (!length(all)) {
    return(builder_plan_error("No datasets yet.", "empty_release"))
  }
  explicit_output <- inherits(output_options, "builder_build_options")
  make_app <- if (explicit_output) {
    isTRUE(output_options$make_app)
  } else {
    builder_plan_requires_app(all)
  }
  validation <- review_validation()
  if (make_app && !isTRUE(validation$ok)) {
    return(builder_plan_error(
      validation$error %||% "Viewer App options are invalid.",
      "invalid_review_options"
    ))
  }
  login_enabled <- make_app && isTRUE(auth_enabled())
  parsed_auth <- builder_auth_validate_payload(
    login_enabled,
    if (login_enabled) auth_accounts() else builder_auth_empty_accounts()
  )
  if (!isTRUE(parsed_auth$ok)) {
    return(builder_plan_error(parsed_auth$error, "invalid_auth_accounts"))
  }
  app_options <- if (make_app && explicit_output) {
    builder_review_options_for_plan(
      output_options$app,
      initial_dataset = output_options$initial_dataset
    )
  } else {
    builder_review_options_for_plan(builder_review_options())
  }
  builder_freeze_plan(
    entries = all,
    out_dir = out_dir,
    make_app = make_app,
    overwrite = isTRUE(overwrite),
    app_options = app_options,
    app_auth = builder_auth_summary(login_enabled, parsed_auth$accounts)
  )
}

freeze_materialized_plan_for_output <- function(
  out_dir,
  overwrite = FALSE,
  output_options = NULL,
  notify = TRUE
) {
  materialized <- alignment_server$materialize_coordinate_drafts(
    notify = notify
  )
  if (!isTRUE(materialized$ok)) {
    return(builder_plan_error(
      materialized$error %||% "Coordinate settings could not be saved.",
      "coordinate_materialization_failed"
    ))
  }
  freeze_plan_for_output(
    out_dir = out_dir,
    overwrite = overwrite,
    output_options = output_options,
    entries_override = materialized$all_entries
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

configure_readiness <- reactive({
  if (length(imports()$entries)) {
    return(list(
      can_continue = FALSE,
      message = "Wait for all datasets to finish loading.",
      plan = NULL
    ))
  }
  plan <- frozen_review_plan()
  ready <- builder_review_can_build(plan)
  list(
    can_continue = ready,
    message = if (ready) {
      count <- length(plan$items)
      paste0(
        count,
        " dataset",
        if (identical(count, 1L)) "" else "s",
        " ready"
      )
    } else {
      plan$error %||% "Resolve the highlighted settings."
    },
    plan = plan
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
        analyses = entry$settings$analyses %||% character(),
        marker_imports = entry$settings$marker_imports %||% list()
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

output[["inspect_stage"]] <- renderUI({
  id <- current()
  req(id)
  entry <- entry_of(id)
  req(entry)
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
  builder_inspect_stage_ui(
    "inspect",
    builder_inspect_model(
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
  )
})

output$configure_actions <- renderUI({
  readiness <- configure_readiness()
  ids <- vapply(sets(), `[[`, character(1), "id")
  checked <- checked_dataset_ids()
  unchecked <- setdiff(ids, checked)
  current_checked <- current() %in% checked
  message <- if (length(unchecked)) {
    paste0(
      "Check all datasets before continuing · ",
      length(unchecked),
      " remaining"
    )
  } else {
    readiness$message
  }
  builder_configure_actions_ui(
    message,
    readiness$can_continue && !length(unchecked),
    dataset_checked = current_checked,
    remaining = length(unchecked)
  )
})

observeEvent(input$complete_dataset_check, {
  entries <- isolate(sets())
  ids <- vapply(entries, `[[`, character(1), "id")
  id <- isolate(current())
  index <- match(id, ids)
  if (is.na(index)) {
    return()
  }
  marks <- isolate(dataset_check_marks())
  marks[[id]] <- .builder_worker_identity(entries[[index]]$snapshot)
  dataset_check_marks(marks)
  unchecked <- setdiff(ids, names(marks))
  if (!length(unchecked)) {
    return()
  }
  after <- if (index < length(ids)) {
    ids[(index + 1L):length(ids)]
  } else {
    character()
  }
  before <- if (index > 1L) ids[seq_len(index - 1L)] else character()
  ordered <- c(after, before)
  target <- ordered[ordered %in% unchecked][[1L]]
  alignment_server$request_dataset_switch(target, function() {
    current(target)
    result(NULL)
  })
})

render_configure_workbench <- function() {
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
      immune_source_fact = entry$dataset_profile$content$immune_repertoire %||%
        entry$profile$content$immune_repertoire %||%
        list(),
      content_sources = settings$content_sources %||% list(),
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
  div(
    class = "builder-stage builder-stage-shell builder-stage-configure",
    `data-workflow-stage` = "configure",
    builder_stage_header_ui(
      "Data setup",
      "Choose data to include",
      "Define the content saved to each CRB file."
    ),
    uiOutput("inspect_stage"),
    builder_core_stage_ui("core", core_model),
    builder_enhance_stage_ui(
      "enhance",
      builder_enhance_model(
        id = entry$id,
        profile = entry$profile,
        state = if (inherits(state, "try-error")) list() else state,
        settings = entry$settings,
        modules = list(),
        active_section = shiny::isolate(active_slice())
      ),
      dynamic_modules = TRUE
    ),
    uiOutput("configure_actions")
  )
}

render_review_workbench <- function() {
  plan <- workflow()$review_plan
  req(builder_review_can_build(plan))
  tagAppendAttributes(
    builder_review_stage_ui(
      "review",
      builder_review_model(plan),
      footer = builder_review_confirmation_ui()
    ),
    `data-workflow-stage` = "review"
  )
}

observeEvent(input$back_to_settings, {
  workflow(builder_reduce_workflow(
    isolate(workflow()),
    list(type = "back_to_settings")
  ))
  session$onFlushed(
    function() {
      session$sendCustomMessage("builder_focus_stage", list(id = "configure"))
    },
    once = TRUE
  )
})

observeEvent(input$confirm_review, {
  state <- isolate(workflow())
  if (!identical(state$stage, "review")) {
    return()
  }
  reviewed <- state$review_plan
  live <- freeze_materialized_plan_for_output(
    file.path(tempdir(), "cerebro-builder-output-preview"),
    overwrite = FALSE
  )
  matches <- builder_review_can_build(live) &&
    identical(
      builder_review_plan_identity(reviewed),
      builder_review_plan_identity(live)
    )
  if (!isTRUE(matches)) {
    workflow(builder_reduce_workflow(state, list(type = "invalidate")))
    result(NULL)
    session$onFlushed(
      function() {
        session$sendCustomMessage("builder_focus_stage", list(id = "configure"))
      },
      once = TRUE
    )
    return()
  }
  if (isTRUE(live$make_app)) {
    build_mode(TRUE)
  }
  workflow(builder_reduce_workflow(
    state,
    list(type = "confirm_review", plan = live)
  ))
  session$onFlushed(
    function() {
      session$sendCustomMessage("builder_focus_stage", list(id = "build"))
    },
    once = TRUE
  )
})

observe({
  live <- frozen_review_plan()
  state <- isolate(workflow())
  if (is.null(state$review_plan) && is.null(state$confirmation)) {
    return()
  }
  if (isolate(build_flow())$stage %in% c("queued", "building")) {
    return()
  }
  matches <- builder_review_can_build(live) &&
    identical(
      builder_review_plan_identity(state$review_plan),
      builder_review_plan_identity(live)
    )
  if (!isTRUE(matches)) {
    workflow(builder_reduce_workflow(state, list(type = "invalidate")))
    build_flow(list(stage = "idle", plan = NULL))
    result(NULL)
    session$sendCustomMessage(
      "builder_build_dialog",
      list(action = "close")
    )
    if (state$stage %in% c("review", "build")) {
      showNotification(
        "Settings changed. Review the updated plan before building.",
        type = "warning",
        duration = 6
      )
    }
  }
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
