.builder_plan_alignment_outside_count <- function(record) {
  if (!is.list(record)) {
    return(NA_integer_)
  }
  if (is.null(record[["outside"]])) {
    return(0L)
  }
  value <- record[["outside"]]
  if (
    !is.numeric(value) ||
      is.object(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value < 0 ||
      value != floor(value) ||
      value > .Machine$integer.max
  ) {
    return(NA_integer_)
  }
  as.integer(value)
}

.builder_plan_preflight_entries <- function(entries) {
  for (entry in entries) {
    state_error <- tryCatch(
      {
        .builder_state_validate_entry(entry)
        .builder_state_validate_recommendations(entry)
        NULL
      },
      builder_state_error = function(error) error
    )
    if (!is.null(state_error)) {
      code <- if (
        state_error$code %in%
          c(
            "invalid_dataset_entry",
            "invalid_dataset_settings"
          )
      ) {
        "invalid_entries"
      } else {
        state_error$code
      }
      return(builder_plan_error(conditionMessage(state_error), code))
    }
  }

  for (entry in entries) {
    images <- entry$settings$images %||% list()
    unsaved <- names(images)[vapply(
      images,
      function(record) {
        is.list(record) && identical(record$saved, FALSE)
      },
      logical(1)
    )]
    if (length(unsaved)) {
      return(builder_plan_error(
        paste0(
          "Section “",
          unsaved[[1L]],
          "” has an image but no saved alignment. Save or remove it before building."
        ),
        "unsaved_spatial_alignment"
      ))
    }
    outside_counts <- vapply(
      images,
      .builder_plan_alignment_outside_count,
      integer(1)
    )
    invalid_outside <- names(images)[is.na(outside_counts)]
    if (length(invalid_outside)) {
      return(builder_plan_error(
        paste0(
          "Section “",
          invalid_outside[[1L]],
          "” has invalid image-coverage diagnostics. Re-open and save its alignment before building."
        ),
        "invalid_spatial_alignment_diagnostics"
      ))
    }
    outside <- names(images)[outside_counts > 0L]
    if (length(outside)) {
      return(builder_plan_error(
        paste0(
          "Section “",
          outside[[1L]],
          "” has cells outside its saved image bounds. Adjust the alignment ",
          "until every cell is covered before building."
        ),
        "spatial_alignment_outside"
      ))
    }
  }

  valid_names <- vapply(
    entries,
    function(entry) {
      builder_has_text(entry$settings$name)
    },
    logical(1)
  )
  if (!all(valid_names)) {
    return(builder_plan_error(
      "Every dataset needs a non-empty scalar name.",
      "invalid_dataset_name"
    ))
  }
  labels <- trimws(vapply(
    entries,
    function(entry) entry$settings$name,
    character(1)
  ))
  if (anyDuplicated(labels)) {
    return(builder_plan_error(
      "Dataset names must be unique.",
      "duplicate_dataset_name"
    ))
  }

  valid_analyses <- vapply(
    entries,
    function(entry) {
      analyses <- entry$settings$analyses
      isTRUE(tryCatch(
        {
          .builder_state_validate_analyses(analyses)
          TRUE
        },
        builder_state_error = function(error) FALSE
      ))
    },
    logical(1)
  )
  if (!all(valid_analyses)) {
    return(builder_plan_error(
      "Selected analyses must be unique supported analysis ids.",
      "invalid_analyses"
    ))
  }

  valid_core <- vapply(
    entries,
    function(entry) {
      settings <- entry$settings
      .builder_plan_character_set(settings$groups) &&
        length(settings$groups) > 0L &&
        .builder_plan_character_set(settings$reductions) &&
        length(settings$reductions) > 0L &&
        builder_has_text(settings$assay) &&
        builder_has_text(settings$layer)
    },
    logical(1)
  )
  if (!all(valid_core)) {
    return(builder_plan_error(
      "Every dataset needs an assay, layer, grouping variable and reduction.",
      "missing_core_selection"
    ))
  }

  valid_qc <- vapply(
    entries,
    function(entry) {
      settings <- entry$settings
      profile <- if (is.list(entry$profile)) entry$profile else list()
      builder_has_text(settings$nUMI %||% profile$nUMI) &&
        builder_has_text(settings$nGene %||% profile$nGene)
    },
    logical(1)
  )
  if (!all(valid_qc)) {
    return(builder_plan_error(
      "Every dataset needs explicit UMI/count and feature/gene fields.",
      "missing_qc_selection"
    ))
  }

  included_groups <- lapply(entries, .builder_state_included_groups)
  included_projections <- lapply(entries, function(entry) {
    settings <- entry$settings
    recommendations <- settings$recommendations
    settings$included_projections %||%
      recommendations$projections$included %||%
      settings$reductions
  })
  included_trajectories <- lapply(entries, function(entry) {
    if ("included_trajectories" %in% names(entry$settings)) {
      entry$settings$included_trajectories
    } else {
      NULL
    }
  })
  cell_cycle <- lapply(entries, function(entry) {
    entry$settings$cell_cycle_columns %||% character()
  })
  if (
    !all(vapply(
      included_groups,
      .builder_plan_character_set,
      logical(1)
    ))
  ) {
    return(builder_plan_error(
      "The final included group set is invalid.",
      "invalid_included_groups"
    ))
  }
  if (!all(vapply(cell_cycle, .builder_plan_character_set, logical(1)))) {
    return(builder_plan_error(
      "The selected cell-cycle annotation set is invalid.",
      "invalid_cell_cycle_selection"
    ))
  }
  if (
    !all(vapply(
      included_projections,
      .builder_plan_character_set,
      logical(1)
    ))
  ) {
    return(builder_plan_error(
      "The final included projection set is invalid.",
      "invalid_included_projections"
    ))
  }

  invalid_group_selection <- vapply(
    seq_along(entries),
    function(index) {
      !all(entries[[index]]$settings$groups %in% included_groups[[index]])
    },
    logical(1)
  )
  if (any(invalid_group_selection)) {
    return(builder_plan_error(
      "Selected groups must remain inside the final included group set.",
      "invalid_group_selection"
    ))
  }
  invalid_projection_selection <- vapply(
    seq_along(entries),
    function(index) {
      !all(
        entries[[index]]$settings$reductions %in%
          included_projections[[index]]
      )
    },
    logical(1)
  )
  if (any(invalid_projection_selection)) {
    return(builder_plan_error(
      paste0(
        "Selected projections must remain inside the final included ",
        "projection set."
      ),
      "invalid_projection_selection"
    ))
  }

  invalid_default_group <- vapply(
    entries,
    function(entry) {
      settings <- entry$settings
      value <- settings$default_group
      if (is.null(settings$recommendations) && is.null(value)) {
        return(FALSE)
      }
      !builder_has_text(value) || !value %in% settings$groups
    },
    logical(1)
  )
  if (any(invalid_default_group)) {
    return(builder_plan_error(
      "Every recommended dataset needs a valid selected default group.",
      "invalid_default_group"
    ))
  }
  invalid_default_projection <- vapply(
    entries,
    function(entry) {
      settings <- entry$settings
      value <- settings$default_projection
      if (is.null(settings$recommendations) && is.null(value)) {
        return(FALSE)
      }
      !builder_has_text(value) || !value %in% settings$reductions
    },
    logical(1)
  )
  if (any(invalid_default_projection)) {
    return(builder_plan_error(
      "Every recommended dataset needs a valid selected default projection.",
      "invalid_default_projection"
    ))
  }

  included_groups <- Map(
    function(values, entry) {
      builder_default_first(values, entry$settings$default_group)
    },
    included_groups,
    entries
  )
  included_projections <- Map(
    function(values, entry) {
      builder_default_first(values, entry$settings$default_projection)
    },
    included_projections,
    entries
  )
  included_trajectories <- Map(
    function(values, entry) {
      builder_trajectory_default_first(
        values,
        entry$settings$default_trajectory
      )
    },
    included_trajectories,
    entries
  )

  list(
    labels = labels,
    included_groups = included_groups,
    included_projections = included_projections,
    included_trajectories = included_trajectories,
    cell_cycle = cell_cycle
  )
}

.builder_plan_app_options_valid <- function(app_options) {
  if (!is.list(app_options) || is.object(app_options)) {
    return(FALSE)
  }
  option_names <- names(app_options)
  if (
    length(app_options) &&
      (is.null(option_names) ||
        anyNA(option_names) ||
        any(!nzchar(option_names)) ||
        anyDuplicated(option_names) ||
        length(setdiff(
          option_names,
          c(
            "show_upload_ui",
            "initial_dataset",
            "initial_page",
            "welcome_message",
            "point_size",
            "variable_to_compare",
            "host",
            "port",
            "max_request_size",
            "display_mode",
            "launch_browser"
          )
        )))
  ) {
    return(FALSE)
  }
  show_upload_ui_supplied <- "show_upload_ui" %in% option_names
  show_upload_ui <- app_options$show_upload_ui
  if (
    show_upload_ui_supplied &&
      (!is.logical(show_upload_ui) ||
        length(show_upload_ui) != 1L ||
        is.na(show_upload_ui))
  ) {
    return(FALSE)
  }
  initial_dataset_supplied <- "initial_dataset" %in% option_names
  initial_dataset <- app_options$initial_dataset
  if (initial_dataset_supplied && !builder_has_text(initial_dataset)) {
    return(FALSE)
  }
  if (
    "initial_page" %in%
      option_names &&
      (!builder_has_text(app_options$initial_page) ||
        !app_options$initial_page %in% builder_viewer_known_page_ids())
  ) {
    return(FALSE)
  }
  if (
    "welcome_message" %in%
      option_names &&
      !builder_has_text(app_options$welcome_message)
  ) {
    return(FALSE)
  }
  if ("point_size" %in% option_names) {
    point_size <- app_options$point_size
    point_names <- if (is.list(point_size)) names(point_size) else NULL
    if (
      !is.list(point_size) ||
        is.object(point_size) ||
        is.null(point_names) ||
        !identical(point_names, "overview_projection_point_size")
    ) {
      return(FALSE)
    }
    value <- point_size$overview_projection_point_size
    if (
      !is.numeric(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !is.finite(value) ||
        value < 0 ||
        value > 20
    ) {
      return(FALSE)
    }
  }
  if ("variable_to_compare" %in% option_names) {
    value <- app_options$variable_to_compare
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      return(FALSE)
    }
  }
  if ("host" %in% option_names && !builder_has_text(app_options$host)) {
    return(FALSE)
  }
  if ("port" %in% option_names) {
    value <- app_options$port
    if (
      !is.numeric(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !is.finite(value) ||
        value != floor(value) ||
        value < 1 ||
        value > 65535
    ) {
      return(FALSE)
    }
  }
  if ("max_request_size" %in% option_names) {
    value <- app_options$max_request_size
    if (
      !is.numeric(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !is.finite(value) ||
        value <= 0 ||
        !is.finite(value * 1024^2)
    ) {
      return(FALSE)
    }
  }
  if (
    "display_mode" %in%
      option_names &&
      (!is.character(app_options$display_mode) ||
        length(app_options$display_mode) != 1L ||
        is.na(app_options$display_mode) ||
        !app_options$display_mode %in% c("auto", "normal", "showcase"))
  ) {
    return(FALSE)
  }
  if ("launch_browser" %in% option_names) {
    value <- app_options$launch_browser
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      return(FALSE)
    }
  }
  TRUE
}
