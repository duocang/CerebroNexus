## Builder server: datasets.

## -- the rail ------------------------------------------------------------
last_dataset_rail_patch <- reactiveVal(NULL)
observe({
  next_patch <- builder_dataset_rail_patch(
    store(),
    current(),
    checked_dataset_ids()
  )
  if (identical(next_patch, isolate(last_dataset_rail_patch()))) {
    return()
  }
  last_dataset_rail_patch(next_patch)
  session$sendCustomMessage("builder_dataset_rail_patch", next_patch)
})

observeEvent(
  input$builder_dataset_rail_sync,
  {
    session$sendCustomMessage(
      "builder_dataset_rail_patch",
      builder_dataset_rail_patch(store(), current(), checked_dataset_ids())
    )
  },
  ignoreInit = TRUE
)

last_import_rail_patch <- reactiveVal(NULL)
observe({
  next_patch <- builder_import_rail_patch(imports()$entries, active_import_id())
  if (identical(next_patch, isolate(last_import_rail_patch()))) {
    return()
  }
  last_import_rail_patch(next_patch)
  session$sendCustomMessage("builder_import_rail_patch", next_patch)
})

observeEvent(
  input$builder_import_rail_sync,
  {
    session$sendCustomMessage(
      "builder_import_rail_patch",
      builder_import_rail_patch(imports()$entries, active_import_id())
    )
  },
  ignoreInit = TRUE
)

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
      freezeReactiveValue(input, paste0("core-", field))
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
  profile <- entry$profile %||% list()
  stored_assay <- entry$settings$assay
  stored_layer <- entry$settings$layer
  stored_layers <- profile$assay_profiles[[stored_assay]]$layers %||%
    character()
  layer_missing <- builder_stage_has_text(stored_layer) &&
    !stored_layer %in% stored_layers
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
  if (
    layer_missing &&
      identical(values$assay, stored_assay) &&
      identical(values$layer, stored_layer)
  ) {
    next_settings$layer <- stored_layer
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
      percentage_cells_to_show = entry$settings[[
        "overview_percentage_cells_to_show"
      ]] %||%
        100,
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

lapply(
  c("immune_repertoire", "hla_tcr_motifs"),
  function(capability) {
    observeEvent(
      input[[paste0("core-immune_source_", capability)]],
      {
        id <- current()
        value <- input[[paste0("core-immune_source_", capability)]]
        if (
          is.null(id) ||
            !identical(input[["core-rendered_for"]], id) ||
            !builder_stage_has_text(value %||% "")
        ) {
          return()
        }
        entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
        req(entry)
        fact <- entry$dataset_profile$content$immune_repertoire %||%
          entry$profile$content$immune_repertoire %||%
          list()
        state <- try(builder_dataset_state(entry), silent = TRUE)
        if (inherits(state, "try-error")) {
          return()
        }
        selectors <- builder_immune_source_choices_model(
          list(
            immune_source_fact = fact,
            content_sources = entry$settings$content_sources %||% list()
          ),
          state$manifest %||% list()
        )
        matching <- Filter(
          function(selector) identical(selector$capability, capability),
          selectors
        )
        if (length(matching) != 1L) {
          return()
        }
        updated <- builder_apply_immune_source_choice(
          entry,
          matching[[1L]],
          value
        )
        if (!identical(updated$settings, entry$settings)) {
          replace_entry(updated)
        }
      },
      ignoreInit = TRUE
    )
  }
)

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

observeEvent(
  input[["core-percentage_cells_to_show"]],
  {
    id <- current()
    value <- suppressWarnings(as.numeric(
      input[["core-percentage_cells_to_show"]]
    ))
    if (
      is.null(id) ||
        !identical(input[["core-rendered_for"]], id) ||
        length(value) != 1L ||
        is.na(value) ||
        !is.finite(value) ||
        value < 10 ||
        value > 100
    ) {
      return()
    }
    entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
    req(entry)
    entry$settings$overview_percentage_cells_to_show <- value
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
  req(!identical(entry$load_state %||% "loaded", "artifact_ready"))
  catalog <- projection_catalog_for_entry(entry)
  ids <- names(catalog)[vapply(
    catalog,
    function(item) is.list(item) && isTRUE(item$available),
    logical(1)
  )]
  contract <- builder_projection_preview_contract(entry, ids)
  cache <- isolate(projection_previews())
  if (builder_preview_cache_hit(cache, id, contract)) {
    return()
  }
  if (!length(ids)) {
    projection_previews(builder_preview_cache_store(
      builder_preview_cache_begin(cache, id, contract),
      id,
      list()
    ))
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
  if (isTRUE(queued)) {
    projection_previews(builder_preview_cache_begin(cache, id, contract))
  }
})

observe({
  req(isTRUE(worker_available()))
  id <- current()
  req(id)
  entry <- builder_upgrade_viewer_content_entry(entry_of(id))
  req(entry)
  req(!identical(entry$load_state %||% "loaded", "artifact_ready"))
  trajectories <- selectable_trajectory_selection(
    trajectory_catalog_for_entry(entry)
  )
  contract <- builder_trajectory_preview_contract(entry, trajectories)
  cache <- isolate(trajectory_previews())
  if (builder_preview_cache_hit(cache, id, contract)) {
    return()
  }
  if (!length(trajectories)) {
    trajectory_previews(builder_preview_cache_store(
      builder_preview_cache_begin(cache, id, contract),
      id,
      list()
    ))
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
  if (isTRUE(queued)) {
    trajectory_previews(builder_preview_cache_begin(cache, id, contract))
  }
})

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

output[["core-projection_gallery"]] <- renderUI({
  id <- current()
  rendered_for <- input[["core-rendered_for"]]
  if (is.null(id) || !identical(rendered_for, id)) {
    return(NULL)
  }
  entry <- builder_upgrade_viewer_content_entry(entry_of(id))
  req(entry)
  frames <- builder_preview_cache_frames(projection_previews(), id)
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
    overview_percentage_cells_to_show = entry$settings[[
      "overview_percentage_cells_to_show"
    ]],
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
  frames <- builder_preview_cache_frames(trajectory_previews(), id)
  model <- builder_trajectory_catalog_model(list(
    trajectory_catalog = trajectory_catalog_for_entry(entry),
    included_trajectories = entry$settings$included_trajectories,
    default_trajectory = entry$settings$default_trajectory,
    trajectory_previews = frames
  ))
  builder_trajectory_catalog_ui("core", model)
})

## Assay-dependent controls above use the namespaced Core inputs.
analysis_checkbox_steps <- Filter(
  function(step) !identical(step$id, "marker_genes"),
  builder_analysis_steps()
)
invisible(lapply(analysis_checkbox_steps, function(step) {
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
