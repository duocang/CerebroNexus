##----------------------------------------------------------------------------##
## Pure Builder dataset and build state.
##
## Readiness is always derived from the typed manifest. Callers may keep this
## state for a rail or Review screen, but they cannot set readiness directly.
##----------------------------------------------------------------------------##

.builder_state_or <- function(value, fallback) {
  if (is.null(value)) fallback else value
}

.builder_state_abort <- function(code, message) {
  condition <- structure(
    list(message = message, call = NULL, code = code),
    class = c("builder_state_error", "error", "condition")
  )
  stop(condition)
}

.builder_state_text <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(trimws(value))
}

.builder_state_has_reference <- function(value, depth = 0L) {
  if (depth > 50L) {
    return(TRUE)
  }
  if (
    is.environment(value) ||
      is.function(value) ||
      isS4(value) ||
      is.language(value) ||
      is.symbol(value) ||
      typeof(value) %in% c("externalptr", "weakref") ||
      inherits(value, "connection")
  ) {
    return(TRUE)
  }
  value_attributes <- attributes(value)
  if (
    !is.null(value_attributes) &&
      any(vapply(
        value_attributes,
        .builder_state_has_reference,
        logical(1),
        depth = depth + 1L
      ))
  ) {
    return(TRUE)
  }
  if (is.list(value) || is.pairlist(value)) {
    return(any(vapply(
      value,
      .builder_state_has_reference,
      logical(1),
      depth = depth + 1L
    )))
  }
  FALSE
}

.builder_state_plain_record <- function(value, recursive = TRUE) {
  if (!is.list(value) || is.object(value)) {
    return(FALSE)
  }
  value_attributes <- attributes(value)
  plain_attributes <- is.null(value_attributes) ||
    identical(names(value_attributes), "names")
  unsafe_reference <- if (isTRUE(recursive)) {
    .builder_state_has_reference(value)
  } else {
    !is.null(value_attributes) &&
      any(vapply(
        value_attributes,
        .builder_state_has_reference,
        logical(1)
      ))
  }
  if (
    !plain_attributes ||
      unsafe_reference
  ) {
    return(FALSE)
  }
  value_names <- attr(value, "names", exact = TRUE)
  !length(value) ||
    (is.character(value_names) &&
      length(value_names) == length(value) &&
      !anyNA(value_names) &&
      all(nzchar(value_names)) &&
      !anyDuplicated(value_names))
}

.builder_state_plain_list <- function(value) {
  if (!is.list(value) || is.object(value)) {
    return(FALSE)
  }
  value_attributes <- attributes(value)
  if (
    (!is.null(value_attributes) &&
      !identical(names(value_attributes), "names")) ||
      .builder_state_has_reference(value)
  ) {
    return(FALSE)
  }
  value_names <- attr(value, "names", exact = TRUE)
  is.null(value_names) ||
    (is.character(value_names) &&
      length(value_names) == length(value) &&
      !anyNA(value_names) &&
      all(nzchar(value_names)) &&
      !anyDuplicated(value_names))
}

.builder_state_viewer_ids <- function(value) {
  if (!is.character(value) || is.object(value)) {
    return(character())
  }
  value <- as.character(value)
  attributes(value) <- NULL
  unique(value[!is.na(value) & nzchar(trimws(value))])
}

.builder_state_viewer_catalog <- function(entry) {
  modern <- if (is.list(entry$dataset_profile)) {
    entry$dataset_profile
  } else if (inherits(entry$profile, "builder_dataset_profile")) {
    entry$profile
  } else {
    list()
  }
  legacy <- if (is.list(entry$profile)) entry$profile else list()
  viewer <- modern$viewer_content
  if (!is.list(viewer)) {
    viewer <- legacy$viewer_content
  }
  if (!is.list(viewer)) {
    viewer <- list()
  }

  metadata <- viewer$metadata
  groups <- if (is.list(metadata) && length(metadata)) {
    ids <- names(metadata)
    ids[vapply(
      metadata,
      function(column) is.list(column) && isTRUE(column$group_eligible),
      logical(1)
    )]
  } else {
    unname(legacy$group_candidates)
  }
  groups <- .builder_state_viewer_ids(groups)
  cell_cycle <- builder_cell_cycle_candidate_ids(metadata)

  projection_catalog <- viewer$projections
  projections <- if (
    is.list(projection_catalog) && length(projection_catalog)
  ) {
    ids <- names(projection_catalog)
    ids[vapply(
      projection_catalog,
      function(projection) {
        is.list(projection) && isTRUE(projection$available)
      },
      logical(1)
    )]
  } else {
    legacy$reductions
  }
  projections <- .builder_state_viewer_ids(projections)

  trajectory_catalog <- viewer$trajectories
  trajectories <- list()
  if (is.list(trajectory_catalog) && length(trajectory_catalog)) {
    for (record in trajectory_catalog) {
      if (
        !is.list(record) ||
          !isTRUE(record$selectable) ||
          !.builder_state_text(record$method) ||
          !.builder_state_text(record$name)
      ) {
        next
      }
      method <- as.character(record$method)
      name <- as.character(record$name)
      trajectories[[method]] <- unique(c(trajectories[[method]], name))
    }
  }
  list(
    groups = groups,
    cell_cycle = cell_cycle,
    projections = projections,
    trajectories = trajectories
  )
}

.builder_state_trajectory_selection <- function(value) {
  if (is.null(value) || !length(value)) {
    return(list())
  }
  if (!is.list(value) || is.object(value)) {
    return(list())
  }
  methods <- names(value)
  if (
    is.null(methods) ||
      anyNA(methods) ||
      any(!nzchar(methods)) ||
      anyDuplicated(methods)
  ) {
    return(list())
  }
  out <- list()
  for (method in methods) {
    names_for_method <- .builder_state_viewer_ids(value[[method]])
    if (length(names_for_method)) {
      out[[method]] <- names_for_method
    }
  }
  out
}

.builder_state_filter_trajectories <- function(selected, available) {
  if (!length(available)) {
    return(list())
  }
  selected <- .builder_state_trajectory_selection(selected)
  out <- list()
  for (method in names(available)) {
    kept <- intersect(
      .builder_state_viewer_ids(available[[method]]),
      .builder_state_viewer_ids(selected[[method]])
    )
    if (length(kept)) {
      out[[method]] <- kept
    }
  }
  out
}

.builder_state_first_trajectory <- function(included) {
  if (!length(included)) {
    return(NULL)
  }
  method <- names(included)[[1L]]
  names_for_method <- included[[method]]
  if (!length(names_for_method)) {
    return(NULL)
  }
  list(method = method, name = names_for_method[[1L]])
}

.builder_state_trajectory_included <- function(value, included) {
  is.list(value) &&
    identical(names(value), c("method", "name")) &&
    .builder_state_text(value$method) &&
    .builder_state_text(value$name) &&
    value$method %in% names(included) &&
    value$name %in% included[[value$method]]
}

#' Upgrade one dataset to the canonical Viewer-content settings shape.
#'
#' This is deliberately an import/restore boundary operation: it does not
#' increment the dataset revision, and current-schema settings are never
#' silently repaired after a user edit.
builder_upgrade_viewer_content_entry <- function(entry) {
  if (!is.list(entry) || !is.list(entry$settings)) {
    return(entry)
  }
  settings <- entry$settings
  if (identical(settings$viewer_content_schema_version, 1L)) {
    return(entry)
  }
  catalog <- .builder_state_viewer_catalog(entry)
  recommendations <- settings$recommendations
  group_recommendation <- if (
    is.list(recommendations) && is.list(recommendations$groups)
  ) {
    recommendations$groups
  } else {
    list()
  }
  projection_recommendation <- if (
    is.list(recommendations) && is.list(recommendations$projections)
  ) {
    recommendations$projections
  } else {
    list()
  }

  groups <- settings$included_groups
  if (is.null(groups)) {
    groups <- group_recommendation$included
  }
  if (is.null(groups)) {
    groups <- settings$groups
  }
  if (is.null(groups)) {
    groups <- catalog$groups
  }
  groups <- .builder_state_viewer_ids(groups)
  if (length(catalog$groups)) {
    groups <- intersect(catalog$groups, groups)
  }
  if (!length(groups) && length(catalog$groups)) {
    suggested <- group_recommendation$value
    groups <- if (
      .builder_state_text(suggested) && suggested %in% catalog$groups
    ) {
      suggested
    } else {
      catalog$groups[[1L]]
    }
  }
  default_group <- settings$default_group
  if (!.builder_state_text(default_group) || !default_group %in% groups) {
    suggested <- group_recommendation$value
    default_group <- if (
      .builder_state_text(suggested) && suggested %in% groups
    ) {
      suggested
    } else if (length(groups)) {
      groups[[1L]]
    } else {
      NULL
    }
  }

  cell_cycle <- settings$cell_cycle_columns
  if (is.null(cell_cycle)) {
    cell_cycle <- catalog$cell_cycle
  }
  cell_cycle <- intersect(
    catalog$cell_cycle,
    .builder_state_viewer_ids(cell_cycle)
  )

  projections <- settings$included_projections
  if (is.null(projections)) {
    projections <- projection_recommendation$included
  }
  if (is.null(projections)) {
    projections <- settings$reductions
  }
  if (is.null(projections)) {
    projections <- catalog$projections
  }
  projections <- .builder_state_viewer_ids(projections)
  if (length(catalog$projections)) {
    projections <- intersect(catalog$projections, projections)
  }
  if (!length(projections) && length(catalog$projections)) {
    suggested <- projection_recommendation$value
    projections <- if (
      .builder_state_text(suggested) && suggested %in% catalog$projections
    ) {
      suggested
    } else {
      catalog$projections[[1L]]
    }
  }
  default_projection <- settings$default_projection
  if (
    !.builder_state_text(default_projection) ||
      !default_projection %in% projections
  ) {
    suggested <- projection_recommendation$value
    default_projection <- if (
      .builder_state_text(suggested) && suggested %in% projections
    ) {
      suggested
    } else if (length(projections)) {
      projections[[1L]]
    } else {
      NULL
    }
  }

  included_trajectories <- if ("included_trajectories" %in% names(settings)) {
    .builder_state_filter_trajectories(
      settings$included_trajectories,
      catalog$trajectories
    )
  } else {
    catalog$trajectories
  }
  default_trajectory <- settings$default_trajectory
  if (
    !.builder_state_trajectory_included(
      default_trajectory,
      included_trajectories
    )
  ) {
    default_trajectory <- .builder_state_first_trajectory(
      included_trajectories
    )
  }

  overrides <- settings$group_color_overrides
  if (is.null(overrides)) {
    overrides <- settings$color_overrides
  }
  if (is.null(overrides)) {
    overrides <- settings$colors
  }
  if (!is.list(overrides) || is.object(overrides)) {
    overrides <- list()
  }
  point_size <- settings$overview_point_size
  if (
    !is.numeric(point_size) ||
      length(point_size) != 1L ||
      is.na(point_size) ||
      !is.finite(point_size) ||
      point_size < 0 ||
      point_size > 20
  ) {
    point_size <- 5
  }

  settings$viewer_content_schema_version <- 1L
  settings$included_groups <- groups
  settings["default_group"] <- list(default_group)
  settings$cell_cycle_columns <- cell_cycle
  settings$group_color_overrides <- overrides
  settings$included_projections <- projections
  settings["default_projection"] <- list(default_projection)
  settings$overview_point_size <- as.numeric(point_size)
  settings$included_trajectories <- included_trajectories
  settings["default_trajectory"] <- list(default_trajectory)
  # Keep the two legacy aliases synchronized while older planning code is
  # upgraded to consume the canonical fields.
  settings$groups <- groups
  settings$reductions <- projections
  entry$settings <- settings
  entry
}

.builder_state_validate_viewer_content_settings <- function(entry) {
  settings <- entry$settings
  if (!identical(settings$viewer_content_schema_version, 1L)) {
    .builder_state_abort(
      "invalid_viewer_content_settings",
      "Viewer content settings require schema version 1."
    )
  }
  validate_ids <- function(value, label) {
    if (
      !is.character(value) ||
        is.object(value) ||
        anyNA(value) ||
        any(!nzchar(trimws(value))) ||
        anyDuplicated(value)
    ) {
      .builder_state_abort(
        "invalid_viewer_content_settings",
        paste(label, "must contain unique stable names.")
      )
    }
    value
  }
  groups <- validate_ids(settings$included_groups, "Included groups")
  cell_cycle <- validate_ids(
    settings$cell_cycle_columns %||% character(),
    "Cell-cycle annotations"
  )
  available_cell_cycle <- .builder_state_viewer_catalog(entry)$cell_cycle
  if (length(setdiff(cell_cycle, available_cell_cycle))) {
    .builder_state_abort(
      "invalid_viewer_content_settings",
      "Cell-cycle annotations must belong to the detected phase catalog."
    )
  }
  projections <- validate_ids(
    settings$included_projections,
    "Included projections"
  )
  validate_default <- function(value, included, label) {
    if (!length(included)) {
      if (!is.null(value)) {
        .builder_state_abort(
          "invalid_viewer_content_default",
          paste(label, "must be empty when nothing is included.")
        )
      }
      return(invisible(NULL))
    }
    if (!.builder_state_fact_text(value) || !value %in% included) {
      .builder_state_abort(
        "invalid_viewer_content_default",
        paste(label, "must belong to its included set.")
      )
    }
    invisible(value)
  }
  validate_default(settings$default_group, groups, "Default group")
  validate_default(
    settings$default_projection,
    projections,
    "Default projection"
  )
  overrides <- settings$group_color_overrides
  if (!.builder_state_plain_list(overrides)) {
    .builder_state_abort(
      "invalid_viewer_content_settings",
      "Group color overrides must be an inert named list."
    )
  }
  for (group in names(overrides)) {
    values <- overrides[[group]]
    if (
      !is.character(values) ||
        is.null(names(values)) ||
        anyNA(names(values)) ||
        any(!nzchar(names(values))) ||
        anyDuplicated(names(values))
    ) {
      .builder_state_abort(
        "invalid_viewer_content_settings",
        "Each group color override requires stable level names."
      )
    }
  }
  point_size <- settings$overview_point_size
  if (
    !is.numeric(point_size) ||
      length(point_size) != 1L ||
      is.na(point_size) ||
      !is.finite(point_size) ||
      point_size < 0 ||
      point_size > 20
  ) {
    .builder_state_abort(
      "invalid_viewer_content_settings",
      "Initial point size must be between 0 and 20."
    )
  }

  trajectories <- settings$included_trajectories
  if (!.builder_state_plain_list(trajectories)) {
    .builder_state_abort(
      "invalid_viewer_content_settings",
      "Included trajectories must be an inert method catalog."
    )
  }
  methods <- names(trajectories)
  if (
    length(trajectories) &&
      (is.null(methods) ||
        anyNA(methods) ||
        any(!nzchar(methods)) ||
        anyDuplicated(methods))
  ) {
    .builder_state_abort(
      "invalid_viewer_content_settings",
      "Included trajectories require unique method names."
    )
  }
  invisible(lapply(
    trajectories,
    validate_ids,
    label = "Included trajectory names"
  ))
  default_trajectory <- settings$default_trajectory
  if (length(trajectories)) {
    if (!.builder_state_trajectory_included(default_trajectory, trajectories)) {
      .builder_state_abort(
        "invalid_viewer_content_default",
        "Default trajectory must belong to the included trajectories."
      )
    }
  } else if (!is.null(default_trajectory)) {
    .builder_state_abort(
      "invalid_viewer_content_default",
      "Default trajectory must be empty when no trajectory is included."
    )
  }
  invisible(entry)
}

.builder_state_validate_entry <- function(entry) {
  if (!is.list(entry) || is.object(entry)) {
    .builder_state_abort(
      "invalid_dataset_entry",
      "Dataset state requires one inert dataset entry."
    )
  }
  modern_profile <- .subset2(entry, "dataset_profile")
  legacy_profile <- .subset2(entry, "profile")
  invalid_profile <-
    (!is.null(modern_profile) && !is.list(modern_profile)) ||
    (!is.null(legacy_profile) && !is.list(legacy_profile))
  modern <- is.list(modern_profile) &&
    any(
      c(
        "schema_version",
        "identity",
        "metadata",
        "manifest",
        "content"
      ) %in%
        names(modern_profile)
    )
  typed_modern <- inherits(legacy_profile, "builder_dataset_profile")
  legacy <- is.list(legacy_profile) &&
    any(
      c(
        "default_assay",
        "assay_profiles",
        "nUMI",
        "nGene",
        "extras"
      ) %in%
        names(legacy_profile)
    )
  if (invalid_profile || !(modern || typed_modern || legacy)) {
    .builder_state_abort(
      "invalid_dataset_entry",
      "Dataset state requires a recognized modern or legacy profile."
    )
  }
  settings <- .subset2(entry, "settings")
  for (field in c("id", "source_id", "output_id", "selector_value")) {
    value <- .subset2(entry, field)
    if (!is.null(value) && !.builder_state_fact_text(value)) {
      .builder_state_abort(
        "invalid_dataset_entry",
        paste("Dataset", field, "must be plain scalar text.")
      )
    }
  }
  if (!.builder_state_plain_record(settings, recursive = FALSE)) {
    .builder_state_abort(
      "invalid_dataset_settings",
      "Dataset settings must be a plain inert record."
    )
  }
  text_vector <- function(value) {
    is.character(value) &&
      !is.object(value) &&
      !anyNA(value) &&
      is.null(attributes(value))
  }
  for (field in c("groups", "reductions")) {
    value <- .subset2(settings, field)
    if (!is.null(value) && !text_vector(value)) {
      .builder_state_abort(
        "invalid_dataset_settings",
        paste("Dataset setting", field, "must be an inert character vector.")
      )
    }
  }
  for (field in c("layer", "nUMI", "nGene")) {
    value <- .subset2(settings, field)
    if (!is.null(value) && !.builder_state_fact_text(value)) {
      .builder_state_abort(
        "invalid_dataset_settings",
        paste("Dataset setting", field, "must be one plain text value.")
      )
    }
  }
  if (!is.null(.subset2(entry, "output_id"))) {
    required <- c("name", "groups", "reductions", "layer", "nUMI", "nGene")
    if (!all(required %in% names(settings))) {
      .builder_state_abort(
        "invalid_dataset_settings",
        "An output dataset requires the complete core settings shape."
      )
    }
  }
  invisible(entry)
}

.builder_state_validate_recommendations <- function(entry) {
  settings <- .subset2(entry, "settings")
  recommendations <- .subset2(settings, "recommendations")
  if (is.null(recommendations)) {
    return(invisible(NULL))
  }
  if (!.builder_state_plain_record(recommendations)) {
    .builder_state_abort(
      "invalid_recommendations",
      "Dataset recommendations must be a plain inert record."
    )
  }
  for (id in c("groups", "projections")) {
    record <- .subset2(recommendations, id)
    if (is.null(record)) {
      next
    }
    if (!.builder_state_plain_record(record)) {
      .builder_state_abort(
        "invalid_recommendations",
        paste("The", id, "recommendation must be a plain inert record.")
      )
    }
    included <- .subset2(record, "included")
    if (
      !is.null(included) &&
        (!is.character(included) ||
          anyNA(included) ||
          any(!nzchar(trimws(included))) ||
          anyDuplicated(included))
    ) {
      .builder_state_abort(
        "invalid_recommendations",
        paste("The", id, "included set is invalid.")
      )
    }
  }
  invisible(recommendations)
}

.builder_state_revision <- function(value, default = 0L) {
  if (is.null(value)) {
    return(as.integer(default))
  }
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value < 0 ||
      value > .Machine$integer.max ||
      value != floor(value)
  ) {
    .builder_state_abort(
      "invalid_revision",
      "Builder revisions must be non-negative integers."
    )
  }
  as.integer(value)
}

.builder_state_profile <- function(entry) {
  profile <- entry$dataset_profile
  if (is.null(profile) && inherits(entry$profile, "builder_dataset_profile")) {
    profile <- entry$profile
  }
  profile
}

.builder_state_content_available <- function(entry, id) {
  profile <- .builder_state_profile(entry)
  if (is.list(profile)) {
    content <- .subset2(profile, "content")
    fact <- if (is.list(content)) .subset2(content, id) else NULL
    return(
      is.list(fact) &&
        isTRUE(.subset2(fact, "detected")) &&
        isTRUE(.subset2(fact, "valid"))
    )
  }
  any(vapply(
    .builder_state_or(entry$profile$extras, list()),
    function(value) {
      is.list(value) &&
        identical(value$key, id) &&
        isTRUE(value$found)
    },
    logical(1)
  ))
}

.builder_state_source <- function(entry, profile) {
  source <- .builder_state_or(profile$source, entry$source)
  if (
    !is.list(source) ||
      !.builder_state_text(source$type) ||
      !.builder_state_text(source$location)
  ) {
    return(list(
      type = "builder",
      location = .builder_state_or(entry$id, "dataset")
    ))
  }
  list(type = source$type, location = source$location)
}

.builder_state_acknowledgements <- function(entry) {
  acknowledgements <- .builder_state_or(
    entry$acknowledgements,
    .builder_state_or(entry$settings$acknowledgements, character())
  )
  if (!is.character(acknowledgements) || anyNA(acknowledgements)) {
    .builder_state_abort(
      "invalid_acknowledgements",
      "Dataset acknowledgements must be character tokens."
    )
  }
  unique(acknowledgements)
}

.builder_state_fact_logical <- function(value) {
  is.logical(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.null(attributes(value))
}

.builder_state_fact_text <- function(value) {
  .builder_state_text(value) &&
    !is.object(value) &&
    is.null(attributes(value))
}

.builder_state_fact_text_vector <- function(value) {
  is.character(value) &&
    !is.object(value) &&
    !anyNA(value) &&
    is.null(attributes(value))
}

.builder_state_fact_count <- function(value) {
  is.numeric(value) &&
    !is.object(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.finite(value) &&
    value >= 0 &&
    value == floor(value) &&
    is.null(attributes(value))
}

.builder_state_validate_fact_record <- function(fact, id, code) {
  required <- c(
    "detected",
    "valid",
    "normalized",
    "diagnostics",
    "requirements",
    "page_candidates"
  )
  if (
    !.builder_state_plain_record(fact) ||
      !all(required %in% names(fact))
  ) {
    .builder_state_abort(
      code,
      paste("Optional content evidence for", id, "must be an inert record.")
    )
  }
  for (field in c("detected", "valid")) {
    if (!.builder_state_fact_logical(.subset2(fact, field))) {
      .builder_state_abort(
        code,
        paste(
          "Optional content evidence for",
          id,
          "has an invalid",
          field,
          "flag."
        )
      )
    }
  }
  normalized <- .subset2(fact, "normalized")
  if (!is.null(normalized) && !.builder_state_plain_list(normalized)) {
    .builder_state_abort(
      code,
      paste("Optional content evidence for", id, "has invalid normalized data.")
    )
  }
  for (field in c("diagnostics", "requirements", "page_candidates")) {
    if (!.builder_state_fact_text_vector(.subset2(fact, field))) {
      .builder_state_abort(
        code,
        paste(
          "Optional content evidence for",
          id,
          "has invalid",
          field,
          "values."
        )
      )
    }
  }
  attention <- .subset2(fact, "attention")
  if (!is.null(attention) && !.builder_state_fact_logical(attention)) {
    .builder_state_abort(
      code,
      paste(
        "Optional content evidence for",
        id,
        "has an invalid attention flag."
      )
    )
  }
  invisible(fact)
}

.builder_state_validate_immune_candidates <- function(fact) {
  candidates <- .subset2(fact, "candidates")
  if (!.builder_state_plain_record(candidates)) {
    .builder_state_abort(
      "invalid_immune_candidates",
      "Immune-repertoire candidates must be inert named records."
    )
  }
  for (name in names(candidates)) {
    candidate <- .subset2(candidates, name)
    .builder_state_validate_fact_record(
      candidate,
      paste("immune candidate", name),
      "invalid_immune_candidates"
    )
    for (field in c("full_ir_ready", "hla_tcr_ready")) {
      if (!.builder_state_fact_logical(.subset2(candidate, field))) {
        .builder_state_abort(
          "invalid_immune_candidates",
          paste("Immune candidate", name, "has an invalid", field, "flag.")
        )
      }
    }
    source_kind <- .subset2(candidate, "source_kind")
    if (!is.null(source_kind) && !.builder_state_fact_text(source_kind)) {
      .builder_state_abort(
        "invalid_immune_candidates",
        paste("Immune candidate", name, "has an invalid source kind.")
      )
    }
  }
  invisible(candidates)
}

.builder_state_validate_immune_overlaps <- function(fact, candidate_names) {
  for (field in c(
    "source_overlaps",
    "full_source_overlaps",
    "motif_source_overlaps"
  )) {
    overlaps <- .subset2(fact, field)
    if (is.null(overlaps)) {
      next
    }
    if (!.builder_state_plain_list(overlaps)) {
      .builder_state_abort(
        "invalid_immune_overlaps",
        "Immune source overlaps must be inert record lists."
      )
    }
    for (overlap in overlaps) {
      if (!.builder_state_plain_record(overlap)) {
        .builder_state_abort(
          "invalid_immune_overlaps",
          "Each immune source overlap must be an inert record."
        )
      }
      left <- .subset2(overlap, "left")
      right <- .subset2(overlap, "right")
      if (
        !.builder_state_fact_text(left) ||
          !.builder_state_fact_text(right) ||
          identical(left, right) ||
          !left %in% candidate_names ||
          !right %in% candidate_names
      ) {
        .builder_state_abort(
          "invalid_immune_overlaps",
          "Immune source overlaps must identify two known sources."
        )
      }
      for (count in c("n_overlap", "n_divergent")) {
        if (!.builder_state_fact_count(.subset2(overlap, count))) {
          .builder_state_abort(
            "invalid_immune_overlaps",
            paste("Immune source overlap has an invalid", count, "count.")
          )
        }
      }
      equivalent <- .subset2(overlap, "equivalent")
      if (!is.null(equivalent) && !.builder_state_fact_logical(equivalent)) {
        .builder_state_abort(
          "invalid_immune_overlaps",
          "Immune source overlap has an invalid equivalent flag."
        )
      }
    }
  }
  invisible(fact)
}

.builder_state_validate_content_fact <- function(id, fact) {
  .builder_state_validate_fact_record(
    fact,
    id,
    "invalid_content_evidence"
  )
  if (identical(id, "immune_repertoire")) {
    candidates <- .builder_state_validate_immune_candidates(fact)
    .builder_state_validate_immune_overlaps(fact, names(candidates))
  }
  invisible(fact)
}

.builder_state_optional_evidence <- function(fact) {
  list(
    detected = isTRUE(.subset2(fact, "detected")),
    valid = isTRUE(.subset2(fact, "valid")),
    attention = isTRUE(.subset2(fact, "attention")),
    normalized = .builder_state_or(
      .subset2(fact, "normalized"),
      list()
    ),
    diagnostics = .builder_state_or(
      .subset2(fact, "diagnostics"),
      character()
    ),
    requirements = .builder_state_or(
      .subset2(fact, "requirements"),
      character()
    ),
    page_candidates = .builder_state_or(
      .subset2(fact, "page_candidates"),
      character()
    )
  )
}

.builder_state_optional_setting_record <- function(
  entry,
  field,
  code,
  label
) {
  settings <- .subset2(entry, "settings")
  record <- .subset2(settings, field)
  if (is.null(record)) {
    return(NULL)
  }
  if (!.builder_state_plain_record(record)) {
    .builder_state_abort(
      code,
      paste("Optional-content", label, "must be a plain inert record.")
    )
  }
  record
}

.builder_state_content_choice <- function(entry, id) {
  choices <- .builder_state_optional_setting_record(
    entry,
    "content_dispositions",
    "invalid_content_dispositions",
    "dispositions"
  )
  if (is.null(choices)) {
    return(NULL)
  }
  choice_names <- attr(choices, "names", exact = TRUE)
  if (
    is.null(choice_names) ||
      length(choice_names) != length(choices) ||
      anyNA(choice_names) ||
      any(!nzchar(choice_names)) ||
      anyDuplicated(choice_names)
  ) {
    .builder_state_abort(
      "invalid_content_dispositions",
      "Optional-content dispositions must have unique non-empty names."
    )
  }
  choice <- .subset2(choices, id)
  if (is.null(choice)) {
    return(NULL)
  }
  if (
    !.builder_state_fact_text(choice) ||
      !choice %in%
        c(
          "preserved",
          "generated",
          "converted",
          "attached",
          "filtered",
          "stored_only"
        )
  ) {
    .builder_state_abort(
      "invalid_content_disposition",
      "A selected content disposition is not supported."
    )
  }
  choice
}

.builder_state_validate_content_dispositions <- function(entry) {
  choices <- .builder_state_optional_setting_record(
    entry,
    "content_dispositions",
    "invalid_content_dispositions",
    "dispositions"
  )
  if (is.null(choices)) {
    return(invisible(NULL))
  }
  choice_ids <- attr(choices, "names", exact = TRUE)
  if (
    is.null(choice_ids) ||
      length(choice_ids) != length(choices) ||
      anyNA(choice_ids) ||
      any(!nzchar(choice_ids)) ||
      anyDuplicated(choice_ids) ||
      any(
        !choice_ids %in%
          c(
            .builder_profile_content_ids(),
            "hla_tcr_motifs"
          )
      )
  ) {
    .builder_state_abort(
      "invalid_content_dispositions",
      paste0(
        "Optional-content dispositions must have unique known ",
        "capability names."
      )
    )
  }
  for (id in choice_ids) {
    .builder_state_content_choice(entry, id)
  }
  invisible(choices)
}

.builder_state_analysis_ids <- function() {
  c(
    "percent_mt_ribo",
    "most_expressed",
    "marker_genes",
    "enriched_pathways"
  )
}

.builder_state_validate_analyses <- function(selected) {
  if (is.null(selected)) {
    return(character())
  }
  if (
    !is.character(selected) ||
      anyNA(selected) ||
      any(!nzchar(selected)) ||
      anyDuplicated(selected) ||
      any(!selected %in% .builder_state_analysis_ids())
  ) {
    .builder_state_abort(
      "invalid_analyses",
      "Selected analyses must be unique supported analysis ids."
    )
  }
  selected
}

.builder_state_normalize_analyses <- function(
  selected,
  has_marker_genes = FALSE
) {
  order <- .builder_state_analysis_ids()
  selected <- intersect(order, .builder_state_validate_analyses(selected))
  if (
    "enriched_pathways" %in%
      selected &&
      !"marker_genes" %in% selected &&
      !isTRUE(has_marker_genes)
  ) {
    selected <- setdiff(selected, "enriched_pathways")
  }
  selected
}

.builder_state_included_groups <- function(entry) {
  settings <- entry$settings
  recommendations <- settings$recommendations
  group_recommendation <- if (
    is.list(recommendations) &&
      !is.object(recommendations) &&
      is.list(recommendations$groups) &&
      !is.object(recommendations$groups)
  ) {
    recommendations$groups$included
  } else {
    NULL
  }
  .builder_state_or(
    settings$included_groups,
    .builder_state_or(group_recommendation, settings$groups)
  )
}

.builder_state_selected_analyses <- function(entry) {
  .builder_state_normalize_analyses(
    entry$settings$analyses,
    has_marker_genes = .builder_state_content_available(
      entry,
      "marker_genes"
    )
  )
}

.builder_state_generated_content <- function(entry) {
  selected <- .builder_state_selected_analyses(entry)
  map <- c(
    most_expressed = "most_expressed_genes",
    marker_genes = "marker_genes",
    enriched_pathways = "enriched_pathways"
  )
  unique(unname(map[intersect(names(map), selected)]))
}

.builder_state_validate_analysis_dispositions <- function(entry) {
  generated_content <- .builder_state_generated_content(entry)
  generation_capabilities <- c(
    "most_expressed_genes",
    "marker_genes",
    "enriched_pathways"
  )
  choices <- .builder_state_optional_setting_record(
    entry,
    "content_dispositions",
    "invalid_content_dispositions",
    "dispositions"
  )
  choice_ids <- if (is.null(choices)) {
    character()
  } else {
    attr(choices, "names", exact = TRUE)
  }
  for (id in union(generation_capabilities, choice_ids)) {
    choice <- .builder_state_content_choice(entry, id)
    generated <- id %in% generated_content
    if (
      !is.null(choice) &&
        (id %in% generation_capabilities || identical(choice, "generated")) &&
        !identical(generated, identical(choice, "generated"))
    ) {
      .builder_state_abort(
        "analysis_disposition_conflict",
        paste0(
          "Analysis execution and the ",
          id,
          " content disposition disagree."
        )
      )
    }
  }
  invisible(generated_content)
}

.builder_state_generated_page <- function(id) {
  pages <- c(
    most_expressed_genes = "most_expressed_genes",
    marker_genes = "marker_genes",
    enriched_pathways = "enriched_pathways"
  )
  unname(.builder_state_or(pages[[id]], character()))
}

.builder_state_attention_action <- function(id, evidence) {
  signals <- sort(
    unique(c(
      .builder_state_or(evidence$diagnostics, character()),
      .builder_state_or(evidence$attention_items, character())
    )),
    method = "radix"
  )
  signals <- signals[!is.na(signals) & nzchar(signals)]
  if (!length(signals)) {
    signals <- "review"
  }
  list(
    type = "acknowledge",
    token = paste(c("builder", id, "attention-v1", signals), collapse = ":")
  )
}

.builder_state_manifest_record <- function(
  id,
  source,
  status,
  disposition,
  pages,
  evidence,
  artifact_scope = "both",
  required_action = NULL,
  verifier = NULL
) {
  entry <- builder_manifest_entry(
    id = id,
    source = source,
    status = status,
    disposition = disposition,
    artifact_scope = artifact_scope,
    summary = paste("BuildPlan decision for", id),
    diagnostics = list(
      codes = evidence$diagnostics,
      requirements = evidence$requirements,
      normalized = evidence$normalized
    ),
    compatibility = list(
      viewer = status %in%
        c("valid", "attention") &&
        disposition %in% c("preserved", "generated", "converted", "attached")
    ),
    pages = pages,
    required_action = required_action,
    verifier = verifier
  )
  entry$evidence <- evidence
  entry
}

.builder_state_generic_content_entry <- function(entry, id, fact, source) {
  evidence <- .builder_state_optional_evidence(fact)
  choice <- .builder_state_content_choice(entry, id)
  generated <- id %in% .builder_state_generated_content(entry)

  if (!evidence$detected && !generated && is.null(choice)) {
    return(.builder_state_manifest_record(
      id,
      source,
      "not_applicable",
      NA_character_,
      character(),
      evidence,
      verifier = paste0("verify_", id)
    ))
  }

  disposition <- if (!is.null(choice)) {
    choice
  } else if (generated) {
    "generated"
  } else {
    "preserved"
  }
  filtered <- disposition %in% c("filtered", "stored_only")
  valid <- (evidence$detected && evidence$valid) || generated || filtered
  attention <- evidence$detected &&
    evidence$valid &&
    evidence$attention &&
    !filtered
  status <- if (attention) {
    "attention"
  } else if (valid) {
    "valid"
  } else {
    "blocking"
  }
  if (!valid) {
    disposition <- "rejected"
  }
  pages <- if (
    status %in%
      c("valid", "attention") &&
      disposition %in% c("preserved", "generated", "converted", "attached")
  ) {
    if (generated) {
      .builder_state_generated_page(id)
    } else {
      evidence$page_candidates
    }
  } else {
    character()
  }

  .builder_state_manifest_record(
    id,
    source,
    status,
    disposition,
    pages,
    evidence,
    required_action = if (attention) {
      .builder_state_attention_action(id, evidence)
    } else {
      NULL
    },
    verifier = paste0("verify_", id)
  )
}

.builder_state_immune_source <- function(names, candidates, fallback) {
  kinds <- vapply(
    seq_along(candidates),
    function(index) {
      .builder_state_or(
        .subset2(.subset2(candidates, index), "source_kind"),
        names[[index]]
      )
    },
    character(1)
  )
  records <- list(
    unified_misc = list(
      source = list(type = "seurat_slot", location = "@misc$immune_repertoire"),
      disposition = "preserved"
    ),
    metadata = list(
      source = list(type = "seurat_metadata", location = "@meta.data"),
      disposition = "converted"
    ),
    legacy_bcr = list(
      source = list(type = "seurat_slot", location = "@misc$bcr_data"),
      disposition = "converted"
    ),
    legacy_tcr = list(
      source = list(type = "seurat_slot", location = "@misc$tcr_data"),
      disposition = "converted"
    )
  )
  selected <- lapply(kinds, function(kind) {
    .builder_state_or(
      records[[kind]],
      list(source = fallback, disposition = "converted")
    )
  })
  locations <- unique(vapply(
    selected,
    function(record) record$source$location,
    character(1)
  ))
  source <- if (length(selected) == 1L) {
    selected[[1L]]$source
  } else {
    list(
      type = "seurat_slots",
      location = paste(locations, collapse = " + ")
    )
  }
  dispositions <- unique(vapply(
    selected,
    `[[`,
    character(1),
    "disposition"
  ))
  list(
    name = if (length(names) == 1L) names[[1L]] else NULL,
    names = names,
    kind = if (length(kinds) == 1L) kinds[[1L]] else kinds,
    candidate = if (length(candidates) == 1L) candidates[[1L]] else NULL,
    candidates = candidates,
    source = source,
    disposition = if (length(dispositions) == 1L) {
      dispositions[[1L]]
    } else {
      "converted"
    }
  )
}

.builder_state_immune_requested_source <- function(entry, id) {
  sources <- .builder_state_optional_setting_record(
    entry,
    "content_sources",
    "invalid_content_sources",
    "sources"
  )
  if (is.null(sources)) {
    return(NULL)
  }
  source <- .subset2(sources, id)
  if (is.null(source)) {
    return(NULL)
  }
  if (!.builder_state_fact_text(source)) {
    .builder_state_abort(
      "invalid_content_source",
      "A selected optional-content source is invalid."
    )
  }
  source
}

.builder_state_validate_content_sources <- function(entry) {
  sources <- .builder_state_optional_setting_record(
    entry,
    "content_sources",
    "invalid_content_sources",
    "sources"
  )
  if (is.null(sources)) {
    return(invisible(NULL))
  }
  source_ids <- attr(sources, "names", exact = TRUE)
  allowed_ids <- c("immune_repertoire", "hla_tcr_motifs")
  if (
    is.null(source_ids) ||
      length(source_ids) != length(sources) ||
      anyNA(source_ids) ||
      any(!nzchar(source_ids)) ||
      anyDuplicated(source_ids) ||
      any(!source_ids %in% allowed_ids)
  ) {
    .builder_state_abort(
      "invalid_content_sources",
      paste0(
        "Optional-content sources must be a named list of known ",
        "capabilities."
      )
    )
  }
  valid_values <- vapply(
    sources,
    .builder_state_fact_text,
    logical(1)
  )
  if (!all(valid_values)) {
    .builder_state_abort(
      "invalid_content_source",
      "A selected optional-content source is invalid."
    )
  }
  invisible(sources)
}

.builder_state_immune_selection <- function(entry, fact, gate, id, fallback) {
  candidates <- .subset2(fact, "candidates")
  requested <- .builder_state_immune_requested_source(entry, id)
  if (!is.list(candidates) || !length(candidates)) {
    if (!is.null(requested)) {
      return(structure(
        list(reason = "selected_source_is_not_ready"),
        class = "builder_invalid_immune_source"
      ))
    }
    return(NULL)
  }
  candidate_names <- names(candidates)
  if (
    is.null(candidate_names) ||
      anyNA(candidate_names) ||
      any(!nzchar(candidate_names)) ||
      anyDuplicated(candidate_names)
  ) {
    .builder_state_abort(
      "invalid_immune_candidates",
      "Immune-repertoire candidates must have stable source names."
    )
  }
  eligible <- candidate_names[vapply(
    candidates,
    function(candidate) {
      is.list(candidate) &&
        isTRUE(.subset2(candidate, "detected")) &&
        isTRUE(.subset2(candidate, gate)) &&
        (!identical(id, "hla_tcr_motifs") ||
          isTRUE(.subset2(candidate, "full_ir_ready")))
    },
    logical(1)
  )]
  if (!is.null(requested) && !requested %in% eligible) {
    return(structure(
      list(reason = "selected_source_is_not_ready"),
      class = "builder_invalid_immune_source"
    ))
  }
  overlaps <- if (identical(gate, "full_ir_ready")) {
    .builder_state_or(
      .subset2(fact, "full_source_overlaps"),
      .builder_state_or(.subset2(fact, "source_overlaps"), list())
    )
  } else {
    .builder_state_or(.subset2(fact, "motif_source_overlaps"), list())
  }
  divergent <- any(vapply(
    overlaps,
    function(overlap) {
      is.list(overlap) &&
        .subset2(overlap, "left") %in% eligible &&
        .subset2(overlap, "right") %in% eligible &&
        .subset2(overlap, "n_divergent") > 0
    },
    logical(1)
  ))
  incomplete_overlap <- identical(gate, "full_ir_ready") &&
    any(vapply(
      overlaps,
      function(overlap) {
        is.list(overlap) &&
          .subset2(overlap, "left") %in% eligible &&
          .subset2(overlap, "right") %in% eligible &&
          .subset2(overlap, "n_overlap") > 0L &&
          !isTRUE(.subset2(overlap, "equivalent"))
      },
      logical(1)
    ))
  sources_equivalent <- function() {
    eligible_overlaps <- Filter(
      function(overlap) {
        is.list(overlap) &&
          .subset2(overlap, "left") %in% eligible &&
          .subset2(overlap, "right") %in% eligible
      },
      overlaps
    )
    expected_pairs <- utils::combn(
      sort(eligible, method = "radix"),
      2L,
      simplify = FALSE
    )
    pair_key <- function(pair) {
      paste(sort(pair, method = "radix"), collapse = "\u001f")
    }
    observed_pairs <- unique(vapply(
      eligible_overlaps,
      function(overlap) {
        pair_key(c(
          .subset2(overlap, "left"),
          .subset2(overlap, "right")
        ))
      },
      character(1)
    ))
    expected_pair_keys <- vapply(expected_pairs, pair_key, character(1))
    length(observed_pairs) == length(expected_pair_keys) &&
      setequal(observed_pairs, expected_pair_keys) &&
      all(vapply(
        eligible_overlaps,
        function(overlap) isTRUE(.subset2(overlap, "equivalent")),
        logical(1)
      ))
  }
  complementary_legacy <- identical(gate, "full_ir_ready") &&
    setequal(eligible, c("legacy_bcr", "legacy_tcr"))
  unverified <- length(eligible) > 1L &&
    !complementary_legacy &&
    !sources_equivalent()
  decision_reason <- if (divergent) {
    "divergent_source_overlap"
  } else if (incomplete_overlap) {
    "incomplete_source_equivalence"
  } else if (unverified) {
    "unverified_source_equivalence"
  } else {
    NULL
  }
  if (!is.null(decision_reason) && is.null(requested)) {
    return(structure(
      list(reason = decision_reason),
      class = "builder_invalid_immune_source"
    ))
  }
  if (!is.null(decision_reason)) {
    selected <- requested
  } else {
    priority <- c(
      "attachment",
      "unified_misc",
      "metadata",
      "legacy_bcr",
      "legacy_tcr"
    )
    selected <- intersect(priority[seq_len(3L)], eligible)
    if (length(selected)) {
      selected <- selected[[1L]]
    } else {
      selected <- intersect(priority[4:5], eligible)
    }
    if (!length(selected) && length(eligible)) {
      selected <- eligible[[1L]]
    }
    if (!length(selected)) {
      return(NULL)
    }
  }
  .builder_state_immune_source(
    selected,
    candidates[selected],
    fallback
  )
}

.builder_state_immune_disposition <- function(choice, selection) {
  if (is.null(selection)) {
    return(NULL)
  }
  if (inherits(selection, "builder_invalid_immune_source")) {
    return("rejected")
  }
  if (is.null(choice)) {
    return(selection$disposition)
  }
  if (choice %in% c("filtered", "stored_only")) {
    return(choice)
  }
  if (identical(choice, selection$disposition)) {
    return(choice)
  }
  "rejected"
}

.builder_state_immune_evidence <- function(
  fact,
  selection,
  full_selection,
  motif_selection
) {
  evidence <- .builder_state_optional_evidence(fact)
  evidence$full_ir_ready <- !is.null(full_selection) &&
    !inherits(full_selection, "builder_invalid_immune_source")
  evidence$hla_tcr_ready <- !is.null(motif_selection) &&
    !inherits(motif_selection, "builder_invalid_immune_source")
  evidence$selected_sources <- if (
    is.null(selection) ||
      inherits(selection, "builder_invalid_immune_source")
  ) {
    character()
  } else {
    selection$names
  }
  evidence["selected_source"] <- list(
    if (length(evidence$selected_sources) == 1L) {
      evidence$selected_sources[[1L]]
    } else {
      NULL
    }
  )
  evidence$selected_candidates <- if (!length(evidence$selected_sources)) {
    list()
  } else {
    selection$candidates
  }
  selected_attention <- vapply(
    evidence$selected_candidates,
    function(candidate) isTRUE(.subset2(candidate, "attention")),
    logical(1)
  )
  evidence$attention <- any(selected_attention)
  evidence$attention_items <- unique(unlist(
    lapply(
      evidence$selected_candidates[selected_attention],
      function(candidate) {
        .builder_state_or(
          .subset2(candidate, "diagnostics"),
          character()
        )
      }
    ),
    use.names = FALSE
  ))
  evidence["selected_candidate"] <- list(
    if (length(evidence$selected_sources) == 1L) {
      evidence$selected_candidates[[1L]]
    } else {
      NULL
    }
  )
  if (inherits(selection, "builder_invalid_immune_source")) {
    evidence$diagnostics <- unique(c(
      evidence$diagnostics,
      selection$reason
    ))
  }
  evidence
}

.builder_state_immune_entries <- function(entry, fact, source) {
  full_selection <- .builder_state_immune_selection(
    entry,
    fact,
    "full_ir_ready",
    "immune_repertoire",
    source
  )
  motif_selection <- .builder_state_immune_selection(
    entry,
    fact,
    "hla_tcr_ready",
    "hla_tcr_motifs",
    source
  )
  immune_evidence <- .builder_state_immune_evidence(
    fact,
    full_selection,
    full_selection,
    motif_selection
  )
  motif_evidence <- .builder_state_immune_evidence(
    fact,
    motif_selection,
    full_selection,
    motif_selection
  )

  immune_choice <- .builder_state_content_choice(entry, "immune_repertoire")
  immune_filtered <- !is.null(immune_choice) &&
    immune_choice %in% c("filtered", "stored_only")
  motif_choice <- .builder_state_content_choice(entry, "hla_tcr_motifs")
  motif_filtered <- !is.null(motif_choice) &&
    motif_choice %in% c("filtered", "stored_only")
  candidates <- .builder_state_or(.subset2(fact, "candidates"), list())
  raw_motif_ready <- any(vapply(
    candidates,
    function(candidate) {
      is.list(candidate) &&
        isTRUE(.subset2(candidate, "detected")) &&
        isTRUE(.subset2(candidate, "hla_tcr_ready"))
    },
    logical(1)
  ))
  invalid_full_source <- inherits(
    full_selection,
    "builder_invalid_immune_source"
  )
  has_full_source <- !is.null(full_selection) && !invalid_full_source
  invalid_motif_source <- inherits(
    motif_selection,
    "builder_invalid_immune_source"
  )
  has_motif_source <- !is.null(motif_selection) && !invalid_motif_source
  motif_source_not_exportable <- raw_motif_ready &&
    !has_motif_source &&
    !invalid_motif_source
  if (motif_source_not_exportable) {
    motif_evidence$diagnostics <- unique(c(
      motif_evidence$diagnostics,
      "motif_source_not_exportable"
    ))
    if (!motif_filtered) {
      invalid_motif_source <- TRUE
    }
  }
  incompatible_sources <- has_full_source &&
    has_motif_source &&
    !immune_filtered &&
    !motif_filtered &&
    !all(motif_selection$names %in% full_selection$names)
  if (incompatible_sources) {
    immune_evidence$diagnostics <- unique(c(
      immune_evidence$diagnostics,
      "incompatible_immune_source_selection"
    ))
    motif_evidence$diagnostics <- unique(c(
      motif_evidence$diagnostics,
      "incompatible_immune_source_selection"
    ))
    invalid_full_source <- TRUE
    invalid_motif_source <- TRUE
  }
  full_payload_has_motif <- has_full_source &&
    any(vapply(
      full_selection$candidates,
      function(candidate) isTRUE(.subset2(candidate, "hla_tcr_ready")),
      logical(1)
    ))
  incompatible_pages <- (!immune_filtered &&
    motif_filtered &&
    full_payload_has_motif) ||
    (immune_filtered &&
      !motif_filtered &&
      has_motif_source)
  if (incompatible_pages) {
    immune_evidence$diagnostics <- unique(c(
      immune_evidence$diagnostics,
      "incompatible_immune_page_disposition"
    ))
    motif_evidence$diagnostics <- unique(c(
      motif_evidence$diagnostics,
      "incompatible_immune_page_disposition"
    ))
    invalid_full_source <- TRUE
    invalid_motif_source <- TRUE
  }
  immune_attention <- has_full_source &&
    isTRUE(immune_evidence$attention) &&
    !immune_filtered
  visible_immune_choice <- !is.null(immune_choice) && !immune_filtered
  immune_status <- if (
    invalid_full_source ||
      visible_immune_choice &&
        !has_full_source
  ) {
    "blocking"
  } else if (has_full_source || immune_filtered) {
    if (immune_attention) "attention" else "valid"
  } else if (!immune_evidence$detected || raw_motif_ready) {
    "not_applicable"
  } else {
    "blocking"
  }
  immune_disposition <- if (identical(immune_status, "not_applicable")) {
    NA_character_
  } else if (identical(immune_status, "blocking")) {
    "rejected"
  } else if (immune_filtered) {
    immune_choice
  } else {
    .builder_state_immune_disposition(immune_choice, full_selection)
  }
  if (identical(immune_disposition, "rejected")) {
    immune_status <- "blocking"
  }
  immune_visible <- immune_status %in%
    c("valid", "attention") &&
    immune_disposition %in% c("preserved", "converted", "attached")

  motif_attention <- has_motif_source &&
    isTRUE(motif_evidence$attention) &&
    !motif_filtered
  visible_motif_choice <- !is.null(motif_choice) && !motif_filtered
  motif_status <- if (
    invalid_motif_source ||
      visible_motif_choice &&
        !has_motif_source
  ) {
    "blocking"
  } else if (has_motif_source || motif_filtered) {
    if (motif_attention) "attention" else "valid"
  } else {
    "not_applicable"
  }
  motif_disposition <- if (identical(motif_status, "not_applicable")) {
    NA_character_
  } else if (identical(motif_status, "blocking")) {
    "rejected"
  } else if (motif_filtered) {
    motif_choice
  } else {
    .builder_state_immune_disposition(motif_choice, motif_selection)
  }
  if (identical(motif_disposition, "rejected")) {
    motif_status <- "blocking"
  }
  motif_visible <- motif_status %in%
    c("valid", "attention") &&
    motif_disposition %in% c("preserved", "converted", "attached")

  list(
    .builder_state_manifest_record(
      "immune_repertoire",
      if (has_full_source) full_selection$source else source,
      immune_status,
      immune_disposition,
      if (immune_visible) "immune_repertoire" else character(),
      immune_evidence,
      required_action = if (immune_attention) {
        .builder_state_attention_action("immune_repertoire", immune_evidence)
      } else {
        NULL
      },
      verifier = "verify_immune_repertoire"
    ),
    .builder_state_manifest_record(
      "hla_tcr_motifs",
      if (has_motif_source) motif_selection$source else source,
      motif_status,
      motif_disposition,
      if (motif_visible) "hla_tcr_motifs" else character(),
      motif_evidence,
      required_action = if (motif_attention) {
        .builder_state_attention_action("hla_tcr_motifs", motif_evidence)
      } else {
        NULL
      },
      verifier = "verify_hla_tcr_motifs"
    )
  )
}

.builder_state_metadata_policy_abort <- function(message) {
  .builder_state_abort("invalid_metadata_policy", message)
}

.builder_state_metadata_policy_ids <- function(value, label) {
  if (
    !is.character(value) ||
      anyNA(value) ||
      any(!nzchar(value)) ||
      anyDuplicated(value)
  ) {
    .builder_state_metadata_policy_abort(
      paste(label, "must contain unique non-empty column names.")
    )
  }
  value
}

.builder_state_missing_metadata_sentinel <- function(record, id) {
  zero_count <- function(value) {
    is.numeric(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      is.finite(value) &&
      identical(as.numeric(value), 0)
  }
  is.list(record) &&
    !is.object(record) &&
    !.builder_state_has_reference(record) &&
    identical(record$name, id) &&
    identical(record$class, "missing") &&
    isTRUE(record$required) &&
    identical(record$disposition, "blocking") &&
    identical(record$value, "blocking") &&
    identical(record$effective_included, FALSE) &&
    identical(record$requires_confirmation, TRUE) &&
    zero_count(record$non_missing) &&
    zero_count(record$unique_non_missing)
}

.builder_state_metadata_record_dependencies <- function(record) {
  dependencies <- record$dependency_ids
  if (is.null(dependencies)) {
    return(character())
  }
  if (
    !is.character(dependencies) ||
      anyNA(dependencies) ||
      any(!nzchar(dependencies)) ||
      anyDuplicated(dependencies)
  ) {
    .builder_state_metadata_policy_abort(
      "Metadata dependency ids must be unique non-empty strings."
    )
  }
  dependencies
}

.builder_state_metadata_required_columns <- function(
  entry,
  policy,
  recommendation
) {
  settings <- entry$settings
  legacy_profile <- if (is.list(entry$profile)) entry$profile else list()
  selected <- unlist(
    Filter(
      is.character,
      list(
        settings$groups,
        .builder_state_included_groups(entry),
        settings$default_group,
        settings$cell_cycle_columns,
        .builder_state_or(settings$nUMI, legacy_profile$nUMI),
        .builder_state_or(settings$nGene, legacy_profile$nGene)
      )
    ),
    use.names = FALSE
  )
  selected <- selected[!is.na(selected) & nzchar(selected)]
  dependent <- character()
  for (source in list(policy, recommendation)) {
    columns <- source$columns
    if (!is.list(columns)) {
      next
    }
    ids <- names(columns)
    if (is.null(ids)) {
      next
    }
    for (id in ids) {
      record <- columns[[id]]
      if (
        is.list(record) &&
          (isTRUE(record$required) ||
            length(.builder_state_metadata_record_dependencies(record)))
      ) {
        dependent <- c(dependent, id)
      }
    }
  }
  unique(c(selected, dependent))
}

.builder_state_validate_metadata_dependencies <- function(
  entry,
  policy,
  recommendation
) {
  required <- .builder_state_metadata_required_columns(
    entry,
    policy,
    recommendation
  )
  missing <- required[
    !vapply(
      required,
      function(id) {
        record <- policy$columns[[id]]
        is.list(record) &&
          (isTRUE(record$effective_included) ||
            identical(record$disposition, "blocking"))
      },
      logical(1)
    )
  ]
  if (length(missing)) {
    .builder_state_abort(
      "metadata_dependency_conflict",
      paste0(
        "Final metadata must include selected or dependency-bearing columns: ",
        paste(missing, collapse = ", "),
        "."
      )
    )
  }
  invisible(required)
}

.builder_state_validate_metadata_policy <- function(
  policy,
  profile,
  entry,
  recommendation = NULL,
  validate_dependencies = TRUE
) {
  if (
    !is.list(policy) ||
      is.object(policy) ||
      .builder_state_has_reference(policy) ||
      !is.list(policy$columns) ||
      is.object(policy$columns)
  ) {
    .builder_state_metadata_policy_abort(
      "The final metadata policy must be an inert record."
    )
  }

  column_ids <- names(policy$columns)
  if (is.null(column_ids)) {
    column_ids <- character()
  }
  column_ids <- .builder_state_metadata_policy_ids(
    column_ids,
    "Final metadata policy columns"
  )

  if (
    !is.list(profile) ||
      !is.list(profile$metadata) ||
      !is.list(profile$metadata$columns)
  ) {
    .builder_state_metadata_policy_abort(
      "The final metadata policy requires profiled metadata columns."
    )
  }
  source_columns <- profile$metadata$columns
  source_ids <- names(source_columns)
  if (is.null(source_ids)) {
    source_ids <- character()
  }
  source_ids <- .builder_state_metadata_policy_ids(
    source_ids,
    "Profiled metadata columns"
  )
  expected_ids <- unique(c(
    "cell_barcode",
    setdiff(source_ids, "cell_barcode")
  ))
  missing_ids <- setdiff(expected_ids, column_ids)
  extra_ids <- setdiff(column_ids, expected_ids)
  valid_extra_ids <- extra_ids[vapply(
    extra_ids,
    function(id) {
      .builder_state_missing_metadata_sentinel(
        policy$columns[[id]],
        id
      )
    },
    logical(1)
  )]
  if (
    length(missing_ids) ||
      !setequal(extra_ids, valid_extra_ids)
  ) {
    .builder_state_metadata_policy_abort(
      "The final metadata policy does not match the profiled column set."
    )
  }

  bucket_names <- c("included", "attention", "excluded", "blocking")
  buckets <- lapply(bucket_names, function(name) {
    .builder_state_metadata_policy_ids(
      policy[[name]],
      paste("Final metadata policy", name)
    )
  })
  names(buckets) <- bucket_names
  if (length(intersect(buckets$included, buckets$excluded))) {
    .builder_state_metadata_policy_abort(
      "Included metadata columns cannot also be excluded."
    )
  }

  dispositions <- character(length(column_ids))
  effective <- logical(length(column_ids))
  names(dispositions) <- column_ids
  names(effective) <- column_ids
  allowed <- c("included", "attention", "excluded", "blocking")
  for (id in column_ids) {
    record <- policy$columns[[id]]
    if (
      !is.list(record) ||
        is.object(record) ||
        .builder_state_has_reference(record) ||
        !identical(record$name, id) ||
        !.builder_state_text(record$disposition) ||
        !record$disposition %in% allowed ||
        !identical(record$value, record$disposition) ||
        !is.logical(record$effective_included) ||
        length(record$effective_included) != 1L ||
        is.na(record$effective_included) ||
        !is.logical(record$requires_confirmation) ||
        length(record$requires_confirmation) != 1L ||
        is.na(record$requires_confirmation)
    ) {
      .builder_state_metadata_policy_abort(
        paste("Final metadata policy column", id, "is malformed.")
      )
    }
    .builder_state_metadata_record_dependencies(record)
    expected_confirmation <- record$disposition %in%
      c("attention", "blocking")
    invalid_effective <-
      (identical(record$disposition, "included") &&
        !isTRUE(record$effective_included)) ||
      (identical(record$disposition, "excluded") &&
        isTRUE(record$effective_included))
    if (
      invalid_effective ||
        !identical(
          record$requires_confirmation,
          expected_confirmation
        )
    ) {
      .builder_state_metadata_policy_abort(
        paste("Final metadata policy column", id, "is inconsistent.")
      )
    }
    dispositions[[id]] <- record$disposition
    effective[[id]] <- record$effective_included
  }

  derived <- list(
    included = column_ids[effective],
    attention = column_ids[dispositions == "attention"],
    excluded = column_ids[dispositions == "excluded"],
    blocking = column_ids[dispositions == "blocking"]
  )
  if (
    !all(vapply(
      bucket_names,
      function(name) {
        identical(buckets[[name]], derived[[name]])
      },
      logical(1)
    ))
  ) {
    .builder_state_metadata_policy_abort(
      "Final metadata policy buckets do not match their column records."
    )
  }
  if (!identical(policy$value, buckets$included)) {
    .builder_state_metadata_policy_abort(
      "Final metadata policy value must equal its included columns."
    )
  }
  expected_confirmation <- length(buckets$attention) > 0L ||
    length(buckets$blocking) > 0L
  if (
    !is.logical(policy$requires_confirmation) ||
      length(policy$requires_confirmation) != 1L ||
      is.na(policy$requires_confirmation) ||
      !identical(
        policy$requires_confirmation,
        expected_confirmation
      )
  ) {
    .builder_state_metadata_policy_abort(
      "Final metadata policy confirmation state is inconsistent."
    )
  }

  barcode <- policy$columns$cell_barcode
  if (
    !is.list(barcode) ||
      !isTRUE(barcode$required) ||
      identical(barcode$disposition, "excluded") ||
      !isTRUE(barcode$effective_included) ||
      !"cell_barcode" %in% buckets$included
  ) {
    .builder_state_metadata_policy_abort(
      "The final metadata policy must include required cell barcodes."
    )
  }
  if (
    "cell_barcode" %in%
      source_ids &&
      !identical(barcode$disposition, "blocking")
  ) {
    .builder_state_metadata_policy_abort(
      "A reserved cell_barcode source collision cannot be downgraded."
    )
  }
  unsafe_included <- source_ids[vapply(
    source_ids,
    function(id) {
      fact <- source_columns[[id]]
      record <- policy$columns[[id]]
      classes <- if (is.list(fact)) fact$class else NULL
      unsafe <- !is.list(fact) ||
        !isTRUE(fact$supported) ||
        !is.character(classes) ||
        any(classes %in% c("list", "data.frame"))
      unsafe && isTRUE(record$effective_included)
    },
    logical(1)
  )]
  if (length(unsafe_included)) {
    .builder_state_metadata_policy_abort(
      paste0(
        "Unsupported metadata columns cannot be included: ",
        paste(unsafe_included, collapse = ", "),
        "."
      )
    )
  }
  if (isTRUE(validate_dependencies)) {
    .builder_state_validate_metadata_dependencies(
      entry,
      policy,
      recommendation
    )
  }
  invisible(policy)
}

.builder_state_effective_metadata_policy <- function(entry, profile) {
  recommendations <- entry$settings$recommendations
  if (
    !is.null(recommendations) &&
      (!is.list(recommendations) ||
        is.object(recommendations) ||
        .builder_state_has_reference(recommendations))
  ) {
    .builder_state_metadata_policy_abort(
      "Metadata recommendations must be an inert record."
    )
  }
  recommendation <- if (is.list(recommendations)) {
    recommendations$metadata
  } else {
    NULL
  }
  policy <- entry$settings$metadata_policy
  effective <- .builder_state_or(policy, recommendation)
  for (candidate in list(recommendation, policy)) {
    if (
      !is.null(candidate) &&
        (!is.list(candidate) ||
          is.object(candidate) ||
          .builder_state_has_reference(candidate))
    ) {
      .builder_state_metadata_policy_abort(
        "Metadata policy values must be inert records."
      )
    }
  }
  if (is.list(profile)) {
    if (!is.null(recommendation)) {
      .builder_state_validate_metadata_policy(
        recommendation,
        profile,
        entry,
        validate_dependencies = is.null(policy)
      )
    }
    if (!is.null(policy)) {
      .builder_state_validate_metadata_policy(
        policy,
        profile,
        entry,
        recommendation = recommendation
      )
    }
  }
  effective
}

.builder_state_metadata_entry <- function(policy, source) {
  recommendation <- policy
  if (is.null(recommendation)) {
    return(NULL)
  }
  if (
    !is.list(recommendation) ||
      !is.list(recommendation$columns) ||
      !is.character(recommendation$attention) ||
      anyNA(recommendation$attention) ||
      !is.character(recommendation$blocking) ||
      anyNA(recommendation$blocking) ||
      !is.logical(recommendation$requires_confirmation) ||
      length(recommendation$requires_confirmation) != 1L ||
      is.na(recommendation$requires_confirmation)
  ) {
    .builder_state_abort(
      "invalid_metadata_recommendation",
      "Metadata recommendations must use the production recommendation shape."
    )
  }
  attention <- sort(unique(recommendation$attention), method = "radix")
  blocking <- sort(unique(recommendation$blocking), method = "radix")
  included <- .builder_state_or(recommendation$included, character())
  excluded <- .builder_state_or(recommendation$excluded, character())
  if (
    !is.character(included) ||
      anyNA(included) ||
      !is.character(excluded) ||
      anyNA(excluded)
  ) {
    .builder_state_abort(
      "invalid_metadata_recommendation",
      "Metadata recommendations contain invalid included or excluded names."
    )
  }
  evidence <- list(
    detected = TRUE,
    valid = !length(blocking),
    attention = length(attention) > 0L &&
      isTRUE(recommendation$requires_confirmation),
    attention_items = c(
      paste0("review=", attention),
      paste0("include=", sort(unique(included), method = "radix")),
      paste0("exclude=", sort(unique(excluded), method = "radix"))
    ),
    normalized = list(
      included = included,
      excluded = excluded,
      attention = attention,
      blocking = blocking
    ),
    diagnostics = c(
      paste0("attention:", attention),
      paste0("blocking:", blocking)
    ),
    requirements = if (length(attention)) {
      "acknowledge_metadata_attention"
    } else {
      character()
    },
    page_candidates = character()
  )
  status <- if (length(blocking)) {
    "blocking"
  } else if (evidence$attention) {
    "attention"
  } else {
    "valid"
  }
  .builder_state_manifest_record(
    "metadata_policy",
    source,
    status,
    if (identical(status, "blocking")) "rejected" else "preserved",
    character(),
    evidence,
    required_action = if (identical(status, "attention")) {
      .builder_state_attention_action("metadata_policy", evidence)
    } else {
      NULL
    },
    verifier = "verify_metadata_policy"
  )
}

.builder_state_compile_manifest <- function(
  entry,
  profile,
  manifest,
  metadata_policy
) {
  content <- .subset2(profile, "content")
  source <- .builder_state_source(entry, profile)
  additions <- list()
  if (!is.null(content) && !is.list(content)) {
    .builder_state_abort(
      "invalid_content_evidence",
      "Optional content evidence must be a list of inert records."
    )
  }
  if (is.list(content) && length(content)) {
    content_ids <- attr(content, "names", exact = TRUE)
    if (
      is.null(content_ids) ||
        length(content_ids) != length(content) ||
        anyNA(content_ids) ||
        any(!nzchar(content_ids)) ||
        anyDuplicated(content_ids) ||
        any(!content_ids %in% .builder_profile_content_ids())
    ) {
      .builder_state_abort(
        "invalid_content_id",
        "Optional content evidence contains an unexpected capability."
      )
    }
    for (id in content_ids) {
      fact <- .subset2(content, id)
      .builder_state_validate_content_fact(id, fact)
      if (identical(id, "immune_repertoire")) {
        additions <- c(
          additions,
          .builder_state_immune_entries(entry, fact, source)
        )
        next
      }
      additions[[length(additions) + 1L]] <-
        .builder_state_generic_content_entry(entry, id, fact, source)
    }
  }
  metadata <- .builder_state_metadata_entry(metadata_policy, source)
  if (!is.null(metadata)) {
    additions[[length(additions) + 1L]] <- metadata
  }

  existing <- unname(manifest)
  addition_ids <- vapply(
    additions,
    function(value) value$id,
    character(1)
  )
  existing <- Filter(
    function(value) !value$id %in% addition_ids,
    existing
  )
  compiled <- builder_content_manifest(c(existing, additions))
  contract <- builder_viewer_page_contract(compiled)
  always <- contract$always$id
  for (id in names(compiled)) {
    pages <- compiled[[id]]$pages
    compiled[[id]]$page_visible <- any(pages %in% always) ||
      any(pages %in% contract$visible_conditional)
  }
  compiled
}

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
builder_build_state <- function() {
  structure(
    list(
      status = "idle",
      id = NULL,
      plan_revision = NULL,
      result = NULL,
      error = NULL,
      revision = 0L
    ),
    class = c("builder_build_state", "list")
  )
}

#' Translate a staged worker result into one explicit state transition.
builder_build_action <- function(result, id) {
  if (!is.list(result) || !.builder_state_text(result$state)) {
    .builder_state_abort(
      "invalid_build_result",
      "The worker returned an invalid build result."
    )
  }
  if (identical(result$state, "success")) {
    if (!isTRUE(result$publishable) && !isTRUE(result$published)) {
      .builder_state_abort(
        "invalid_build_result",
        "A successful build result must be publishable or already published."
      )
    }
    return(list(type = "succeed", id = id, result = result))
  }
  if (identical(result$state, "needs_decision")) {
    return(list(type = "needs_decision", id = id, result = result))
  }
  if (identical(result$state, "failure")) {
    error <- result$error
    if (!.builder_state_text(error)) {
      error <- "The staged build failed."
    }
    return(list(type = "fail", id = id, error = error))
  }
  .builder_state_abort(
    "invalid_build_result",
    "The worker returned an unknown build state."
  )
}

#' Apply a typed event to the pure single-flight build state.
builder_reduce_build <- function(state, action) {
  if (!inherits(state, "builder_build_state") || !is.list(state)) {
    .builder_state_abort(
      "invalid_build_state",
      "Expected a Builder build state."
    )
  }
  if (!is.list(action) || !.builder_state_text(action$type)) {
    .builder_state_abort(
      "invalid_build_action",
      "Build actions require a type."
    )
  }
  revision <- .builder_state_revision(state$revision) + 1L

  require_current_build <- function() {
    if (
      !.builder_state_fact_text(action$id) ||
        !identical(action$id, state$id)
    ) {
      .builder_state_abort(
        "stale_build_event",
        "The build event does not match the active build."
      )
    }
  }

  if (identical(action$type, "start")) {
    if (state$status %in% c("running", "cancelling")) {
      .builder_state_abort(
        "build_in_flight",
        "A Builder build is already in flight."
      )
    }
    if (!.builder_state_fact_text(action$id)) {
      .builder_state_abort("invalid_build_id", "A build id is required.")
    }
    state$status <- "running"
    state$id <- action$id
    state$plan_revision <- .builder_state_revision(action$revision)
    state$result <- NULL
    state$error <- NULL
  } else if (identical(action$type, "succeed")) {
    if (!state$status %in% c("running", "cancelling")) {
      .builder_state_abort("invalid_build_transition", "No build is running.")
    }
    require_current_build()
    state$status <- "success"
    state$result <- action$result
  } else if (identical(action$type, "fail")) {
    if (!state$status %in% c("running", "cancelling")) {
      .builder_state_abort("invalid_build_transition", "No build is running.")
    }
    require_current_build()
    state$status <- "failed"
    state$error <- action$error
  } else if (identical(action$type, "needs_decision")) {
    if (!state$status %in% c("running", "cancelling")) {
      .builder_state_abort("invalid_build_transition", "No build is running.")
    }
    require_current_build()
    state$status <- "needs_decision"
    state$result <- action$result
    state$error <- NULL
  } else if (identical(action$type, "cancel")) {
    if (!identical(state$status, "running")) {
      .builder_state_abort(
        "invalid_build_transition",
        "No build can be cancelled."
      )
    }
    require_current_build()
    state$status <- "cancelling"
  } else if (identical(action$type, "cancelled")) {
    if (!identical(state$status, "cancelling")) {
      .builder_state_abort(
        "invalid_build_transition",
        "Build is not cancelling."
      )
    }
    require_current_build()
    state$status <- "cancelled"
  } else if (identical(action$type, "reset")) {
    if (state$status %in% c("running", "cancelling")) {
      .builder_state_abort(
        "build_in_flight",
        "A running build cannot be reset."
      )
    }
    if (!identical(state$status, "idle")) {
      require_current_build()
    }
    state <- builder_build_state()
  } else {
    .builder_state_abort(
      "unknown_build_action",
      "Build action type is not supported."
    )
  }
  state$revision <- revision
  structure(state, class = c("builder_build_state", "list"))
}
