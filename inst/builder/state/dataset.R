## Builder state: dataset.

.builder_state_load_state <- function(entry) {
  state <- .builder_state_or(entry$load_state, "loaded")
  if (!.builder_state_text(state)) {
    .builder_state_abort("invalid_load_state", "Dataset load state is invalid.")
  }
  state
}

.builder_state_page_manifest <- function(manifest, acknowledgements) {
  entries <- lapply(unname(manifest), function(entry) {
    action <- entry$required_action
    acknowledged <- identical(entry$status, "attention") &&
      is.list(action) &&
      identical(action$type, "acknowledge") &&
      action$token %in% acknowledgements
    if (acknowledged) {
      entry$status <- "valid"
    }
    entry
  })
  builder_content_manifest(entries)
}

.builder_state_apply_output_setup <- function(state) {
  entry <- state$entry
  if (
    is.null(entry$output_id) ||
      !identical(state$load_state, "loaded")
  ) {
    return(state)
  }
  settings <- entry$settings
  missing <- c(
    if (!length(settings$groups)) "settings_groups",
    if (!length(settings$reductions)) "settings_reductions",
    if (!.builder_state_text(settings$layer)) "settings_layer",
    if (!.builder_state_text(settings$nUMI)) "settings_nUMI",
    if (!.builder_state_text(settings$nGene)) "settings_nGene"
  )
  if (length(missing)) {
    state$readiness <- "blocked"
    state$blocking_ids <- unique(c(state$blocking_ids, missing))
    state$issue_count <- as.integer(sum(c(
      length(state$blocking_ids),
      length(state$attention_ids),
      length(state$checking_ids)
    )))
  }
  structure(state, class = c("builder_dataset_state", "list"))
}

#' Derive one dataset's rail and Review readiness from its manifest.
builder_dataset_state <- function(entry) {
  .builder_state_validate_entry(entry)
  entry <- builder_upgrade_viewer_content_entry(entry)
  .builder_state_validate_entry(entry)
  .builder_state_validate_viewer_content_settings(entry)
  .builder_state_validate_recommendations(entry)
  load_state <- .builder_state_load_state(entry)
  revision <- .builder_state_revision(entry$revision)
  base <- list(
    id = .builder_state_or(entry$id, NULL),
    revision = revision,
    load_state = load_state,
    entry = entry,
    manifest = NULL,
    readiness = load_state,
    blocking_ids = character(),
    attention_ids = character(),
    checking_ids = character(),
    issue_count = 0L,
    analyses = character(),
    metadata_policy = NULL,
    acknowledgements = .builder_state_acknowledgements(entry),
    page_expectations = NULL,
    error_code = NULL
  )
  if (load_state %in% c("loading", "reload_required")) {
    return(.builder_state_apply_output_setup(structure(
      base,
      class = c("builder_dataset_state", "list")
    )))
  }
  if (identical(load_state, "artifact_ready")) {
    item <- entry$project_artifact$plan_item %||% list()
    base$readiness <- "artifact_ready"
    base$analyses <- item$analyses %||% character()
    base$manifest <- item$manifest %||% list()
    base$metadata_policy <- item$metadata_policy %||% list()
    base$page_expectations <- item$viewer_page_expectations %||% list()
    return(structure(
      base,
      class = c("builder_dataset_state", "list")
    ))
  }
  if (!identical(load_state, "loaded")) {
    .builder_state_abort(
      "invalid_load_state",
      "Dataset load state is not supported."
    )
  }

  base$analyses <- .builder_state_selected_analyses(entry)
  .builder_state_validate_content_dispositions(entry)
  .builder_state_validate_analysis_dispositions(entry)
  .builder_state_validate_content_sources(entry)
  profile <- .builder_state_profile(entry)
  base$metadata_policy <- .builder_state_effective_metadata_policy(
    entry,
    profile
  )
  manifest <- entry$manifest
  if (is.null(manifest) && is.list(profile)) {
    manifest <- profile$manifest
  }
  modern <- is.list(profile)
  if (is.null(manifest) && modern) {
    base$readiness <- "blocked"
    base$blocking_ids <- "manifest"
    base$issue_count <- 1L
    base$error_code <- "missing_manifest"
    return(.builder_state_apply_output_setup(structure(
      base,
      class = c("builder_dataset_state", "list")
    )))
  }
  if (is.null(manifest)) {
    base$readiness <- "ready"
    return(.builder_state_apply_output_setup(structure(
      base,
      class = c("builder_dataset_state", "list")
    )))
  }

  manifest <- .builder_state_compile_manifest(
    entry,
    profile,
    manifest,
    base$metadata_policy
  )
  readiness <- builder_manifest_readiness(
    manifest,
    acknowledgements = base$acknowledgements
  )
  base$manifest <- manifest
  base$readiness <- readiness$state
  base$blocking_ids <- readiness$blocking_ids
  base$attention_ids <- readiness$attention_ids
  base$checking_ids <- readiness$checking_ids
  base$issue_count <- as.integer(sum(c(
    length(readiness$blocking_ids),
    length(readiness$attention_ids),
    length(readiness$checking_ids)
  )))
  base$page_expectations <- builder_viewer_page_contract(
    .builder_state_page_manifest(manifest, base$acknowledgements)
  )
  .builder_state_apply_output_setup(structure(
    base,
    class = c("builder_dataset_state", "list")
  ))
}

#' Apply a typed event to one pure dataset state.
builder_reduce_dataset <- function(state, action) {
  if (!inherits(state, "builder_dataset_state") || !is.list(state)) {
    .builder_state_abort(
      "invalid_dataset_state",
      "Expected a Builder dataset state."
    )
  }
  if (!is.list(action) || !.builder_state_text(action$type)) {
    .builder_state_abort(
      "invalid_dataset_action",
      "Dataset actions require a type."
    )
  }
  next_revision <- .builder_state_revision(state$revision) + 1L
  entry <- state$entry

  if (identical(action$type, "replace_manifest")) {
    entry$manifest <- action$manifest
    entry$revision <- next_revision
    return(builder_dataset_state(entry))
  }
  if (identical(action$type, "replace_entry")) {
    if (!is.list(action$entry)) {
      .builder_state_abort(
        "invalid_dataset_entry",
        "A replacement dataset entry is required."
      )
    }
    entry <- action$entry
    entry$revision <- next_revision
    return(builder_dataset_state(entry))
  }
  if (identical(action$type, "set_acknowledgements")) {
    entry$acknowledgements <- action$acknowledgements
    entry$revision <- next_revision
    return(builder_dataset_state(entry))
  }
  if (identical(action$type, "loading")) {
    entry$load_state <- "loading"
    entry$revision <- next_revision
    return(builder_dataset_state(entry))
  }
  if (identical(action$type, "reload_required")) {
    entry$load_state <- "reload_required"
    entry$revision <- next_revision
    return(builder_dataset_state(entry))
  }
  if (identical(action$type, "loaded")) {
    if (!is.list(action$entry)) {
      .builder_state_abort(
        "invalid_dataset_entry",
        "A loaded dataset entry is required."
      )
    }
    entry <- action$entry
    entry$load_state <- "loaded"
    entry$revision <- next_revision
    return(builder_dataset_state(entry))
  }
  .builder_state_abort(
    "unknown_dataset_action",
    "Dataset action type is not supported."
  )
}

.builder_store_validate_entry <- function(entry) {
  invisible(builder_dataset_state(entry))
  if (.builder_state_has_reference(entry)) {
    .builder_state_abort(
      "invalid_dataset_entry",
      "Dataset store entries must not contain deep mutable references."
    )
  }
  invisible(entry)
}

.builder_store_ids <- function(datasets) {
  if (!is.list(datasets) || is.object(datasets)) {
    .builder_state_abort(
      "invalid_builder_state",
      "Builder datasets must be an ordinary list."
    )
  }
  ids <- vapply(
    datasets,
    function(entry) {
      if (!is.list(entry) || !.builder_state_fact_text(entry$id)) {
        .builder_state_abort(
          "invalid_builder_state",
          "Every Builder dataset requires a stable id."
        )
      }
      entry$id
    },
    character(1)
  )
  if (anyDuplicated(ids)) {
    .builder_state_abort(
      "duplicate_dataset_id",
      "Builder dataset ids must be unique."
    )
  }
  invisible(lapply(datasets, .builder_store_validate_entry))
  ids
}

.builder_store_copy <- function(value) {
  unserialize(serialize(value, NULL, version = 3L))
}

#' Create the typed top-level state for the persistent dataset rail.
builder_state <- function(
  datasets = list(),
  current_dataset = NULL
) {
  # Validate the untrusted shape before upgrading it; upgrade is compatibility
  # logic, not a sanitizer for hostile values.
  invisible(.builder_store_ids(datasets))
  datasets <- lapply(datasets, builder_upgrade_viewer_content_entry)
  ids <- .builder_store_ids(datasets)
  if (
    !is.null(current_dataset) &&
      (!.builder_state_fact_text(current_dataset) || !current_dataset %in% ids)
  ) {
    .builder_state_abort(
      "invalid_current_dataset",
      "The current dataset must be present in the rail."
    )
  }
  state <- structure(
    list(
      datasets = datasets,
      current_dataset = if (length(ids)) {
        .builder_state_or(current_dataset, ids[[1L]])
      } else {
        NULL
      },
      last_removed = NULL,
      can_undo_remove = FALSE,
      revision = 0L
    ),
    class = c("builder_state", "list")
  )
  .builder_store_assert(state)
  state
}

.builder_store_assert <- function(state) {
  required_fields <- c(
    "datasets",
    "current_dataset",
    "last_removed",
    "can_undo_remove",
    "revision"
  )
  fixture <- isTRUE(state$.state_only_fixture)
  expected_fields <- c(required_fields, if (fixture) ".state_only_fixture")
  if (
    !identical(class(state), c("builder_state", "list")) ||
      !is.list(state) ||
      !setequal(names(state), expected_fields) ||
      length(names(state)) != length(expected_fields)
  ) {
    .builder_state_abort(
      "invalid_builder_state",
      "Expected a typed Builder state."
    )
  }
  ids <- .builder_store_ids(state$datasets)
  if (
    !is.null(state$current_dataset) &&
      (!.builder_state_fact_text(state$current_dataset) ||
        !state$current_dataset %in% ids)
  ) {
    .builder_state_abort(
      "invalid_current_dataset",
      "The current dataset must be present in the rail."
    )
  }
  if (
    !is.logical(state$can_undo_remove) ||
      length(state$can_undo_remove) != 1L ||
      is.na(state$can_undo_remove) ||
      !is.numeric(state$revision) ||
      length(state$revision) != 1L ||
      is.na(state$revision) ||
      !is.finite(state$revision) ||
      state$revision < 0 ||
      state$revision != floor(state$revision)
  ) {
    .builder_state_abort(
      "invalid_builder_state",
      "Builder state flags and revision must be typed scalar values."
    )
  }
  removed <- state$last_removed
  if (is.null(removed)) {
    if (isTRUE(state$can_undo_remove)) {
      .builder_state_abort(
        "invalid_builder_state",
        "Undo cannot be enabled without a removed dataset record."
      )
    }
  } else {
    required <- c(
      "id",
      "entry",
      "index",
      "before_id",
      "after_id",
      "restore_current",
      "fallback_current"
    )
    scalar_flag <- function(value) {
      is.logical(value) && length(value) == 1L && !is.na(value)
    }
    retained_id <- function(value) {
      is.null(value) || (.builder_state_fact_text(value) && value %in% ids)
    }
    valid_index <- is.numeric(removed$index) &&
      length(removed$index) == 1L &&
      !is.na(removed$index) &&
      is.finite(removed$index) &&
      removed$index >= 1 &&
      removed$index == floor(removed$index)
    if (
      !.builder_state_plain_record(removed) ||
        !all(required %in% names(removed)) ||
        length(names(removed)) != length(required) ||
        !isTRUE(state$can_undo_remove) ||
        !.builder_state_fact_text(removed$id) ||
        removed$id %in% ids ||
        !is.list(removed$entry) ||
        !identical(removed$entry$id, removed$id) ||
        !valid_index ||
        removed$index > length(ids) + 1L ||
        !retained_id(removed$before_id) ||
        !retained_id(removed$after_id) ||
        (!is.null(removed$before_id) &&
          identical(removed$before_id, removed$after_id)) ||
        !retained_id(removed$fallback_current) ||
        !scalar_flag(removed$restore_current)
    ) {
      .builder_state_abort(
        "invalid_builder_state",
        "The removed dataset record is malformed."
      )
    }
    .builder_store_validate_entry(removed$entry)
  }
  ids
}

#' Derive the generated App's initial dataset from dataset order.
builder_effective_initial_dataset <- function(state) {
  ids <- .builder_store_assert(state)
  if (!length(ids)) {
    return(list(id = NULL, mode = "automatic"))
  }
  list(id = ids[[1L]], mode = "automatic")
}

#' Adapt mutable Builder state to the frozen plan's App options.
builder_app_options_for_plan <- function(state) {
  .builder_store_assert(state)
  list()
}

#' Return the exact ordered dataset members consumed by the next BuildPlan.
builder_datasets_for_plan <- function(state) {
  .builder_store_assert(state)
  .builder_store_copy(state$datasets)
}

#' Apply a typed event to the persistent dataset rail state.
builder_reduce_state <- function(state, action) {
  ids <- .builder_store_assert(state)
  if (!is.list(action) || !.builder_state_text(action$type)) {
    .builder_state_abort(
      "invalid_builder_action",
      "Builder actions require a type."
    )
  }
  next_state <- .builder_store_copy(state)
  type <- action$type

  require_id <- function(id) {
    if (!.builder_state_fact_text(id) || !id %in% ids) {
      .builder_state_abort(
        "unknown_dataset_id",
        "The Builder action refers to an unknown dataset."
      )
    }
    match(id, ids)
  }

  if (identical(type, "add")) {
    .builder_store_validate_entry(action$entry)
    entry <- builder_upgrade_viewer_content_entry(action$entry)
    if (!is.list(entry) || !.builder_state_fact_text(entry$id)) {
      .builder_state_abort(
        "invalid_dataset_entry",
        "A dataset entry is required."
      )
    }
    if (entry$id %in% ids) {
      .builder_state_abort(
        "duplicate_dataset_id",
        "Builder dataset ids must be unique."
      )
    }
    .builder_store_validate_entry(entry)
    next_state$datasets[[length(next_state$datasets) + 1L]] <- entry
    if (is.null(next_state$current_dataset)) {
      next_state$current_dataset <- entry$id
    }
  } else if (identical(type, "replace")) {
    index <- require_id(action$id)
    .builder_store_validate_entry(action$entry)
    entry <- builder_upgrade_viewer_content_entry(action$entry)
    if (!is.list(entry) || !identical(entry$id, action$id)) {
      .builder_state_abort(
        "invalid_dataset_entry",
        "A replacement entry must retain its dataset id."
      )
    }
    .builder_store_validate_entry(entry)
    next_state$datasets[[index]] <- entry
  } else if (identical(type, "replace_all")) {
    invisible(.builder_store_ids(action$datasets))
    replacement <- lapply(
      action$datasets,
      builder_upgrade_viewer_content_entry
    )
    replacement_ids <- .builder_store_ids(replacement)
    next_state$datasets <- replacement
    if (
      is.null(next_state$current_dataset) ||
        !next_state$current_dataset %in% replacement_ids
    ) {
      next_state["current_dataset"] <- list(
        if (length(replacement_ids)) {
          replacement_ids[[1L]]
        } else {
          NULL
        }
      )
    }
  } else if (identical(type, "select")) {
    require_id(action$id)
    next_state$current_dataset <- action$id
    if (is.list(next_state$last_removed)) {
      next_state$last_removed$restore_current <- FALSE
    }
  } else if (identical(type, "remove")) {
    index <- require_id(action$id)
    restore_current <- identical(next_state$current_dataset, action$id)
    next_state$last_removed <- list(
      id = action$id,
      entry = next_state$datasets[[index]],
      index = as.integer(index),
      before_id = if (index > 1L) ids[[index - 1L]] else NULL,
      after_id = if (index < length(ids)) ids[[index + 1L]] else NULL,
      restore_current = restore_current
    )
    next_state$datasets <- next_state$datasets[-index]
    remaining <- ids[-index]
    if (identical(next_state$current_dataset, action$id)) {
      next_state["current_dataset"] <- list(
        if (length(remaining)) {
          remaining[[min(index, length(remaining))]]
        } else {
          NULL
        }
      )
    }
    next_state$can_undo_remove <- TRUE
    next_state$last_removed["fallback_current"] <-
      list(next_state$current_dataset)
  } else if (identical(type, "undo_remove")) {
    removed <- next_state$last_removed
    if (!isTRUE(next_state$can_undo_remove) || !is.list(removed)) {
      .builder_state_abort(
        "nothing_to_undo",
        "There is no removed dataset to restore."
      )
    }
    current_ids <- .builder_store_ids(next_state$datasets)
    if (removed$id %in% current_ids) {
      .builder_state_abort(
        "duplicate_dataset_id",
        "The removed dataset id is already in use."
      )
    }
    index <- if (
      !is.null(removed$before_id) && removed$before_id %in% current_ids
    ) {
      match(removed$before_id, current_ids) + 1L
    } else if (
      !is.null(removed$after_id) && removed$after_id %in% current_ids
    ) {
      match(removed$after_id, current_ids)
    } else {
      max(1L, min(removed$index, length(next_state$datasets) + 1L))
    }
    next_state$datasets <- append(
      next_state$datasets,
      list(removed$entry),
      after = index - 1L
    )
    if (
      isTRUE(removed$restore_current) &&
        identical(next_state$current_dataset, removed$fallback_current)
    ) {
      next_state$current_dataset <- removed$id
    }
    next_state["last_removed"] <- list(NULL)
    next_state$can_undo_remove <- FALSE
  } else if (identical(type, "reorder")) {
    order <- action$order
    if (
      !is.character(order) ||
        anyNA(order) ||
        anyDuplicated(order) ||
        !setequal(order, ids) ||
        length(order) != length(ids)
    ) {
      .builder_state_abort(
        "invalid_dataset_order",
        "Reorder must contain every dataset id exactly once."
      )
    }
    next_state$datasets <- next_state$datasets[match(order, ids)]
  } else if (identical(type, "move")) {
    index <- require_id(action$id)
    direction <- action$direction
    if (
      !is.character(direction) ||
        length(direction) != 1L ||
        is.na(direction) ||
        !is.null(attributes(direction)) ||
        !direction %in% c("up", "down")
    ) {
      .builder_state_abort(
        "invalid_move_direction",
        "Move direction must be exactly up or down."
      )
    }
    target <- index + if (identical(direction, "up")) -1L else 1L
    if (target >= 1L && target <= length(ids)) {
      order <- ids
      order[c(index, target)] <- order[c(target, index)]
      next_state$datasets <- next_state$datasets[match(order, ids)]
    }
  } else {
    .builder_state_abort(
      "unknown_builder_action",
      "Builder action type is not supported."
    )
  }

  next_state$revision <- .builder_state_revision(state$revision) + 1L
  next_state <- structure(next_state, class = c("builder_state", "list"))
  .builder_store_assert(next_state)
  next_state
}

#' Create the initial single-flight build state.
