##----------------------------------------------------------------------------##
## Turn UI entries into a validated, deterministic build plan.
##
## Pure helpers only. The Shiny process creates the plan; the worker executes
## it without having to reinterpret UI state.
##----------------------------------------------------------------------------##

`%||%` <- function(a, b) if (is.null(a)) b else a

builder_safe_stem <- function(value, fallback = "dataset") {
  value <- iconv(value, to = "ASCII//TRANSLIT", sub = "")
  value <- tolower(trimws(value))
  value <- gsub("[^a-z0-9]+", "-", value)
  value <- gsub("(^-+|-+$)", "", value)
  if (!nzchar(value)) fallback else substr(value, 1L, 48L)
}

builder_item_filename <- function(item, index, total) {
  width <- max(2L, nchar(as.character(total)))
  paste0(
    sprintf(paste0("%0", width, "d"), index),
    "-",
    builder_safe_stem(item$settings$name),
    "-",
    builder_safe_stem(item$id, fallback = paste0("ds", index)),
    ".crb"
  )
}

builder_profile_has <- function(profile, key) {
  any(vapply(
    profile$extras %||% list(),
    function(x) identical(x$key, key) && isTRUE(x$found),
    logical(1)
  ))
}

builder_default_first <- function(values, default) {
  if (is.null(default) || !length(default) || !default %in% values) {
    return(values)
  }
  c(default, values[values != default])
}

builder_normalize_analyses <- function(selected, has_marker_genes = FALSE) {
  if (
    !exists(
      ".builder_state_normalize_analyses",
      mode = "function",
      inherits = TRUE
    )
  ) {
    stop("state_authority_unavailable", call. = FALSE)
  }
  .builder_state_normalize_analyses(selected, has_marker_genes)
}

builder_resolve_colors <- function(settings, levels) {
  groups <- settings$groups %||% character()
  palette <- settings$palette %||% "cerebro"
  overrides <- builder_settings_color_overrides(settings)
  out <- list()
  for (group in groups) {
    group_levels <- levels[[group]] %||% character()
    if (length(group_levels)) {
      out[[group]] <- builder_level_colors(
        group_levels,
        palette,
        overrides[[group]]
      )
    }
  }
  out
}

.builder_plan_partition_alignments <- function(images) {
  if (
    exists("builder_partition_alignments", mode = "function", inherits = TRUE)
  ) {
    return(builder_partition_alignments(images %||% list()))
  }
  images <- images %||% list()
  is_trekker <- vapply(
    names(images),
    function(name) {
      identical(name, "trekker") ||
        identical(images[[name]]$section_kind %||% "", "trekker")
    },
    logical(1)
  )
  list(
    spatial = images[!is_trekker],
    trekker = if (any(is_trekker)) images[[which(is_trekker)[[1L]]]] else NULL
  )
}

builder_default_settings <- function(profile, name, recommendations = NULL) {
  assay <- profile$default_assay
  assay_profile <- profile$assay_profiles[[assay]] %||%
    list(
      default_layer = profile$default_layer,
      nUMI = profile$nUMI,
      nGene = profile$nGene
    )
  settings <- list(
    name = name,
    organism = profile$organism_guess,
    assay = assay,
    layer = assay_profile$default_layer,
    nUMI = assay_profile$nUMI,
    nGene = assay_profile$nGene,
    groups = profile$group_preselect,
    reductions = profile$reduction_preselect,
    analyses = character(),
    tables = list(),
    images = list(),
    palette = "cerebro",
    color_overrides = list()
  )
  if (is.null(recommendations)) {
    return(settings)
  }
  settings$organism <- recommendations$organism$value
  settings$groups <- recommendations$groups$included %||% character()
  settings$reductions <- recommendations$projections$included %||% character()
  settings$default_group <- recommendations$groups$value
  settings$default_projection <- recommendations$projections$value
  settings$metadata_policy <- recommendations$metadata
  settings$nomenclature <- recommendations$nomenclature$value
  settings$expression_backend <- recommendations$backend$value
  settings$recommendations <- recommendations
  settings
}

builder_plan_error <- function(
  message,
  code = "invalid_plan",
  details = list()
) {
  structure(
    list(error = message, error_code = code, details = details),
    class = c("builder_plan_failure", "list")
  )
}

builder_has_text <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(trimws(value))
}

.builder_plan_has_reference <- function(value, depth = 0L) {
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
        .builder_plan_has_reference,
        logical(1),
        depth = depth + 1L
      ))
  ) {
    return(TRUE)
  }
  if (is.list(value) || is.pairlist(value)) {
    return(any(vapply(
      value,
      .builder_plan_has_reference,
      logical(1),
      depth = depth + 1L
    )))
  }
  FALSE
}

.builder_plan_deep_copy <- function(value) {
  if (.builder_plan_has_reference(value)) {
    stop("unsafe_reference", call. = FALSE)
  }
  unserialize(serialize(value, NULL, version = 3L))
}

.builder_plan_revision <- function(revision, entries) {
  if (is.null(revision)) {
    entry_revisions <- vapply(
      entries,
      function(entry) {
        value <- entry$revision
        if (
          is.numeric(value) &&
            length(value) == 1L &&
            !is.na(value) &&
            is.finite(value) &&
            value >= 0 &&
            value <= .Machine$integer.max &&
            value == floor(value)
        ) {
          as.integer(value)
        } else {
          0L
        }
      },
      integer(1)
    )
    return(max(c(1L, entry_revisions)))
  }
  if (
    !is.numeric(revision) ||
      length(revision) != 1L ||
      is.na(revision) ||
      !is.finite(revision) ||
      revision < 1 ||
      revision > .Machine$integer.max ||
      revision != floor(revision)
  ) {
    return(NULL)
  }
  as.integer(revision)
}

.builder_plan_character_set <- function(value) {
  is.character(value) &&
    !anyNA(value) &&
    all(nzchar(trimws(value))) &&
    !anyDuplicated(value)
}

.builder_plan_entry_source_identity <- function(entry) {
  profile <- entry$dataset_profile
  if (is.null(profile) && inherits(entry$profile, "builder_dataset_profile")) {
    profile <- entry$profile
  }
  source <- profile$source %||% entry$source
  if (is.list(source)) {
    source[c("type", "location", "fingerprint", "format")]
  } else {
    NULL
  }
}

.builder_plan_asset_claim <- function(source, target, artifact) {
  structure(
    list(
      source = source,
      target = target,
      artifact = artifact
    ),
    class = c("builder_asset_claim", "list")
  )
}

.builder_plan_normalize_asset_claim <- function(claim) {
  fields <- c("source", "target", "artifact")
  if (
    !inherits(claim, "builder_asset_claim") ||
      !is.list(claim) ||
      length(claim) != length(fields) ||
      .builder_plan_has_reference(claim)
  ) {
    return(NULL)
  }
  claim_names <- attr(claim, "names", exact = TRUE)
  if (
    !is.character(claim_names) ||
      length(claim_names) != length(fields) ||
      anyNA(claim_names) ||
      any(!nzchar(claim_names)) ||
      anyDuplicated(claim_names) ||
      !setequal(claim_names, fields)
  ) {
    return(NULL)
  }
  plain_claim <- claim
  attributes(plain_claim) <- list(names = claim_names)
  values <- plain_claim[fields]
  values <- lapply(values, function(value) {
    if (
      !is.character(value) ||
        length(value) != 1L ||
        is.na(value) ||
        .builder_plan_has_reference(value)
    ) {
      return(NULL)
    }
    attributes(value) <- NULL
    if (!nzchar(trimws(value))) {
      return(NULL)
    }
    value
  })
  if (any(vapply(values, is.null, logical(1)))) {
    return(NULL)
  }
  .builder_plan_asset_claim(
    values$source,
    values$target,
    values$artifact
  )
}

.builder_plan_asset_claim_valid <- function(claim) {
  !is.null(.builder_plan_normalize_asset_claim(claim))
}

.builder_plan_asset_set <- function(value) {
  if (is.null(value)) {
    value <- character()
  }
  if (is.character(value)) {
    valid <- .builder_plan_character_set(value)
    claims <- if (valid) {
      .builder_plan_legacy_asset_claims(value)
    } else {
      list()
    }
    return(list(valid = valid, targets = value, claims = claims))
  }
  if (inherits(value, "builder_asset_claim")) {
    value <- list(value)
  }
  if (!is.list(value)) {
    return(list(valid = FALSE, targets = character(), claims = list()))
  }
  if (!length(value)) {
    return(list(valid = TRUE, targets = character(), claims = list()))
  }
  claims <- lapply(value, .builder_plan_normalize_asset_claim)
  valid <- !any(vapply(claims, is.null, logical(1)))
  if (!valid) {
    return(list(valid = FALSE, targets = character(), claims = list()))
  }
  if (length(.builder_plan_dedupe_claims(claims)) != length(claims)) {
    return(list(valid = FALSE, targets = character(), claims = list()))
  }
  list(
    valid = TRUE,
    targets = vapply(claims, `[[`, character(1), "target"),
    claims = claims
  )
}

.builder_plan_internal_asset_claims <- function(paths, entry) {
  lapply(paths, function(path) {
    .builder_plan_asset_claim(
      paste0("builder_dataset:", entry$id),
      path,
      path
    )
  })
}

.builder_plan_legacy_asset_claims <- function(paths) {
  lapply(paths, function(path) {
    structure(
      list(
        source = NULL,
        target = path,
        artifact = NULL
      ),
      class = c("builder_asset_claim", "list")
    )
  })
}

.builder_plan_dedupe_claims <- function(claims) {
  out <- list()
  for (claim in claims) {
    duplicate <- any(vapply(out, identical, logical(1), y = claim))
    if (!duplicate) {
      out[[length(out) + 1L]] <- claim
    }
  }
  out
}

.builder_plan_claim_target_conflict <- function(claims) {
  targets <- vapply(claims, `[[`, character(1), "target")
  any(vapply(
    unique(targets),
    function(target) {
      matching <- claims[targets == target]
      if (length(matching) < 2L) {
        return(FALSE)
      }
      incomplete <- any(vapply(
        matching,
        function(claim) {
          !builder_has_text(claim$source) ||
            !builder_has_text(claim$artifact)
        },
        logical(1)
      ))
      incomplete ||
        any(!vapply(matching[-1L], identical, logical(1), y = matching[[1L]]))
    },
    logical(1)
  ))
}

.builder_plan_source_snapshot_identity <- function(entry) {
  source_identity <- .builder_plan_entry_source_identity(entry)
  candidates <- Filter(
    Negate(is.null),
    list(entry$snapshot, entry$snapshot_identity)
  )
  owned_available <- exists(
    ".builder_snapshot_owned",
    mode = "function",
    inherits = TRUE
  )
  if (owned_available) {
    for (snapshot in candidates) {
      valid_time <- is.list(snapshot) &&
        inherits(snapshot$created_at, "POSIXt") &&
        length(snapshot$created_at) == 1L &&
        !is.na(snapshot$created_at) &&
        is.finite(as.numeric(snapshot$created_at))
      valid_bytes <- is.list(snapshot) &&
        is.numeric(snapshot$closure_bytes) &&
        length(snapshot$closure_bytes) == 1L &&
        !is.na(snapshot$closure_bytes) &&
        is.finite(snapshot$closure_bytes) &&
        snapshot$closure_bytes >= 0
      valid_md5 <- is.list(snapshot) &&
        builder_has_text(snapshot$object_md5 %||% "") &&
        grepl("^[[:xdigit:]]{32}$", snapshot$object_md5)
      valid_shape <- is.list(snapshot) &&
        is.null(attr(snapshot, "class", exact = TRUE)) &&
        builder_has_text(snapshot$path %||% "") &&
        builder_has_text(snapshot$object_file %||% "") &&
        identical(
          snapshot$object_file,
          file.path(snapshot$path, "object.rds")
        ) &&
        builder_has_text(snapshot$owner_token %||% "") &&
        valid_time &&
        valid_md5 &&
        valid_bytes &&
        !.builder_plan_has_reference(snapshot)
      owned <- valid_shape &&
        isTRUE(tryCatch(
          .builder_snapshot_owned(snapshot),
          error = function(error) FALSE
        ))
      if (owned) {
        frozen <- .builder_plan_deep_copy(snapshot)
        return(c(
          list(
            available = TRUE,
            snapshot = frozen,
            source = source_identity
          ),
          frozen
        ))
      }
    }
  }
  list(
    available = FALSE,
    snapshot = NULL,
    source = source_identity,
    reason = paste0(
      "No Builder-owned snapshot with a matching owner marker and MD5 ",
      "was available to planning."
    )
  )
}

.builder_plan_backend <- function(settings, filename) {
  recommendations <- settings$recommendations
  backend <- settings$expression_backend
  if (is.null(backend) && is.null(recommendations)) {
    backend <- "embedded"
  }
  if (
    !builder_has_text(backend %||% "") ||
      !backend %in% c("embedded", "h5", "bpcells")
  ) {
    return(list(valid = FALSE, error_code = "invalid_expression_backend"))
  }
  stem <- tools::file_path_sans_ext(basename(filename))
  sidecars <- switch(
    backend,
    embedded = character(),
    h5 = paste0(stem, ".h5"),
    bpcells = paste0(stem, ".bpcells")
  )
  supplied <- settings$sidecars
  if (!is.null(supplied) && !identical(supplied, sidecars)) {
    return(list(valid = FALSE, error_code = "invalid_backend_sidecars"))
  }
  list(
    valid = TRUE,
    error_code = NULL,
    mode = backend,
    sidecars = sidecars,
    dataset_file = filename
  )
}

.builder_plan_analysis_graph <- function(analyses, has_marker_genes) {
  nodes <- lapply(analyses, function(id) {
    dependencies <- if (identical(id, "enriched_pathways")) {
      if ("marker_genes" %in% analyses || isTRUE(has_marker_genes)) {
        "marker_genes"
      } else {
        character()
      }
    } else {
      character()
    }
    list(id = id, dependencies = dependencies)
  })
  names(nodes) <- analyses
  nodes
}

.builder_plan_artifact_identity <- function(
  entry,
  included_groups,
  included_projections,
  analyses = character()
) {
  profile <- if (is.list(entry$dataset_profile)) {
    entry$dataset_profile
  } else {
    list()
  }
  identity <- if (is.list(profile$identity)) profile$identity else list()
  axis_ids <- function(axis) {
    record <- identity[[axis]]
    if (!is.list(record)) {
      return(character())
    }
    values <- record$canonical_ids %||% record$ids %||% character()
    if (!is.character(values) || anyNA(values)) {
      return(character())
    }
    unname(values)
  }
  levels <- if (is.list(entry$levels)) entry$levels else list()
  group_levels <- lapply(included_groups, function(group) {
    values <- levels[[group]] %||% character()
    if (!is.character(values) || anyNA(values)) character() else unname(values)
  })
  names(group_levels) <- included_groups
  settings <- if (is.list(entry$settings)) entry$settings else list()
  policy <- settings$metadata_policy
  source_metadata <- if (is.list(policy)) {
    policy$included %||% character()
  } else {
    character()
  }
  nUMI <- settings$nUMI %||% entry$profile$nUMI
  nGene <- settings$nGene %||% entry$profile$nGene
  additional_metadata <- setdiff(
    source_metadata,
    c("cell_barcode", included_groups, nUMI, nGene)
  )
  generated_metadata <- if ("percent_mt_ribo" %in% analyses) {
    c("percent_mt", "percent_ribo")
  } else {
    character()
  }
  metadata <- make.unique(c(
    "cell_barcode",
    included_groups,
    "nUMI",
    "nGene",
    additional_metadata,
    setdiff(generated_metadata, source_metadata)
  ))
  spatial <- if (is.list(profile$spatial)) profile$spatial else list()
  spatial_sections <- spatial$sections %||% character()
  if (!is.character(spatial_sections) || anyNA(spatial_sections)) {
    spatial_sections <- character()
  }
  list(
    schema_version = 2L,
    cells = axis_ids("cells"),
    features = axis_ids("features"),
    group_levels = group_levels,
    projections = unname(included_projections),
    source_metadata = unname(source_metadata),
    metadata = unname(metadata),
    spatial_sections = unname(spatial_sections)
  )
}

.builder_plan_release_manifest <- function(manifests) {
  ids <- unique(unlist(lapply(manifests, names), use.names = FALSE))
  out <- lapply(ids, function(id) {
    entries <- lapply(manifests, function(manifest) manifest[[id]])
    present <- !vapply(entries, is.null, logical(1))
    entries <- entries[present]
    dataset_ids <- names(manifests)[present]
    visible <- vapply(
      entries,
      function(entry) isTRUE(entry$page_visible),
      logical(1)
    )
    candidates <- if (any(visible)) which(visible) else seq_along(entries)
    rank <- c(
      blocking = 5L,
      checking = 4L,
      attention = 3L,
      valid = 2L,
      not_applicable = 1L
    )
    statuses <- vapply(entries, `[[`, "", "status")
    exemplar <- candidates[[which.max(unname(rank[statuses[candidates]]))]]
    record <- .builder_plan_deep_copy(entries[[exemplar]])
    record$page_visible <- any(visible)
    record$dataset_ids <- dataset_ids
    record$dataset_entries <- entries
    if (length(entries) > 1L) {
      record$evidence <- lapply(entries, `[[`, "evidence")
      names(record$evidence) <- dataset_ids
    }
    record
  })
  names(out) <- ids
  out
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

  list(
    labels = labels,
    included_groups = included_groups,
    included_projections = included_projections
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

builder_freeze_plan <- function(
  entries,
  out_dir,
  make_app = FALSE,
  overwrite = FALSE,
  revision = NULL,
  app_options = list(),
  expected_prior_identity = NULL
) {
  if (
    !exists(
      "builder_dataset_state",
      mode = "function",
      inherits = TRUE
    )
  ) {
    return(builder_plan_error(
      "The Builder dataset-state authority is unavailable.",
      "state_authority_unavailable"
    ))
  }
  if (!is.list(entries)) {
    return(builder_plan_error(
      "Datasets must be supplied as a list.",
      "invalid_entries"
    ))
  }
  legacy_asset_key <- vapply(
    entries,
    function(entry) {
      settings <- if (is.list(entry)) entry$settings else NULL
      is.list(settings) &&
        any(
          c("public_assets", "public_asset_claims") %in% names(settings)
        )
    },
    logical(1)
  )
  if (any(legacy_asset_key)) {
    return(builder_plan_error(
      "Dataset settings use a retired asset field.",
      "invalid_entries"
    ))
  }
  if (
    !is.logical(make_app) ||
      length(make_app) != 1L ||
      is.na(make_app)
  ) {
    return(builder_plan_error(
      "Generated-app selection must be one non-missing logical value.",
      "invalid_make_app"
    ))
  }
  if (
    !is.logical(overwrite) ||
      length(overwrite) != 1L ||
      is.na(overwrite)
  ) {
    return(builder_plan_error(
      "Overwrite selection must be one non-missing logical value.",
      "invalid_overwrite"
    ))
  }
  if (.builder_plan_has_reference(app_options)) {
    return(builder_plan_error(
      "BuildPlan values cannot contain mutable reference objects.",
      "unsafe_reference"
    ))
  }
  if (!.builder_plan_app_options_valid(app_options)) {
    return(builder_plan_error(
      "Generated-app options must be an inert list.",
      "invalid_app_options"
    ))
  }
  if (
    !is.null(expected_prior_identity) &&
      (!is.list(expected_prior_identity) ||
        is.object(expected_prior_identity))
  ) {
    return(builder_plan_error(
      "Expected prior identity must be an inert list.",
      "invalid_expected_prior_identity"
    ))
  }
  if (
    .builder_plan_has_reference(app_options) ||
      .builder_plan_has_reference(expected_prior_identity)
  ) {
    return(builder_plan_error(
      "BuildPlan values cannot contain mutable reference objects.",
      "unsafe_reference"
    ))
  }
  preflight <- .builder_plan_preflight_entries(entries)
  if (inherits(preflight, "builder_plan_failure")) {
    return(preflight)
  }
  plan_revision <- .builder_plan_revision(revision, entries)
  if (is.null(plan_revision)) {
    return(builder_plan_error(
      "Choose a valid positive BuildPlan revision.",
      "invalid_revision"
    ))
  }
  if (is.null(out_dir)) {
    return(builder_plan_error(
      "Choose an output directory.",
      "missing_output_directory"
    ))
  }
  if (
    !is.character(out_dir) ||
      length(out_dir) != 1L ||
      is.na(out_dir)
  ) {
    return(builder_plan_error(
      "The output directory must be one non-missing string.",
      "invalid_output_directory"
    ))
  }
  out_dir <- trimws(out_dir)
  if (!nzchar(out_dir)) {
    return(builder_plan_error(
      "Choose an output directory.",
      "missing_output_directory"
    ))
  }
  out_dir <- tryCatch(
    as.character(fs::path_norm(fs::path_abs(fs::path_expand(out_dir)))),
    error = function(error) NULL
  )
  if (is.null(out_dir)) {
    return(builder_plan_error(
      "The output directory could not be normalized.",
      "invalid_output_directory"
    ))
  }
  if (!length(entries)) {
    return(builder_plan_error(
      "Add at least one dataset.",
      "missing_dataset"
    ))
  }

  app_capability <- builder_app_capability()
  app_contract_version <- if (
    isTRUE(make_app) &&
      is.list(app_capability) &&
      identical(app_capability$version, 1L)
  ) {
    1L
  } else {
    0L
  }
  if (
    isTRUE(make_app) &&
      !(is.list(app_capability) &&
        isTRUE(app_capability$available) &&
        identical(app_contract_version, 1L))
  ) {
    reason <- if (
      is.list(app_capability) && builder_has_text(app_capability$reason)
    ) {
      app_capability$reason
    } else {
      builder_app_capability(0L)$reason
    }
    return(builder_plan_error(reason, "app_capability_unavailable"))
  }

  dataset_order <- vapply(
    entries,
    function(entry) {
      if (is.list(entry) && builder_has_text(entry$id %||% "")) {
        entry$id
      } else {
        ""
      }
    },
    character(1)
  )
  if (any(!nzchar(dataset_order)) || anyDuplicated(dataset_order)) {
    return(builder_plan_error(
      "Every dataset needs a unique stable id.",
      "invalid_dataset_order"
    ))
  }

  states <- lapply(entries, function(entry) {
    tryCatch(
      builder_dataset_state(entry),
      builder_state_error = function(error) error,
      builder_manifest_error = function(error) error
    )
  })
  state_errors <- vapply(states, inherits, logical(1), "condition")
  if (any(state_errors)) {
    error <- states[[which(state_errors)[[1L]]]]
    return(builder_plan_error(
      conditionMessage(error),
      error$code %||% "invalid_dataset_state"
    ))
  }
  names(states) <- dataset_order

  load_states <- vapply(states, `[[`, "", "load_state")
  if (any(load_states == "loading")) {
    return(builder_plan_error(
      "A dataset is still loading and cannot enter BuildPlan.",
      "dataset_loading"
    ))
  }
  if (any(load_states == "reload_required")) {
    return(builder_plan_error(
      "A dataset must be reloaded before BuildPlan can be frozen.",
      "dataset_reload_required"
    ))
  }
  missing_manifest <- vapply(
    states,
    function(state) identical(state$error_code, "missing_manifest"),
    logical(1)
  )
  if (any(missing_manifest)) {
    return(builder_plan_error(
      "A profiled dataset is missing its typed content manifest.",
      "missing_manifest"
    ))
  }
  readiness <- vapply(states, `[[`, "", "readiness")
  if (any(readiness == "blocked")) {
    index <- which(readiness == "blocked")[[1L]]
    ids <- states[[index]]$blocking_ids
    return(builder_plan_error(
      paste0(
        "Dataset ",
        dataset_order[[index]],
        " has a blocking capability",
        if (length(ids)) paste0(": ", paste(ids, collapse = ", ")) else "",
        "."
      ),
      "blocking_capability",
      list(dataset_id = dataset_order[[index]], capability_ids = ids)
    ))
  }
  if (any(readiness == "checking")) {
    return(builder_plan_error(
      "A dataset capability is still being checked.",
      "checking_capability"
    ))
  }
  if (any(readiness == "needs_attention")) {
    return(builder_plan_error(
      "A dataset capability still needs attention.",
      "attention_capability"
    ))
  }

  labels <- preflight$labels
  included_groups <- preflight$included_groups
  included_projections <- preflight$included_projections
  invalid_nomenclature <- vapply(
    entries,
    function(entry) {
      settings <- entry$settings
      if (
        is.null(settings$recommendations) ||
          is.null(settings$nomenclature)
      ) {
        return(FALSE)
      }
      tryCatch(
        {
          builder_validate_nomenclature(
            settings$organism,
            settings$nomenclature
          )
          FALSE
        },
        error = function(error) TRUE
      )
    },
    logical(1)
  )
  if (any(invalid_nomenclature)) {
    return(builder_plan_error(
      "The selected nomenclature is invalid for the dataset organism.",
      "invalid_nomenclature"
    ))
  }

  planned_filenames <- vapply(
    seq_along(entries),
    function(index) {
      builder_item_filename(entries[[index]], index, length(entries))
    },
    character(1)
  )
  backends <- lapply(seq_along(entries), function(index) {
    .builder_plan_backend(entries[[index]]$settings, planned_filenames[[index]])
  })
  invalid_backends <- !vapply(backends, `[[`, logical(1), "valid")
  if (any(invalid_backends)) {
    error_code <- backends[[which(invalid_backends)[[1L]]]]$error_code
    return(builder_plan_error(
      "Every dataset needs a supported expression backend and sidecar plan.",
      error_code %||% "invalid_expression_backend"
    ))
  }
  sidecar_claims <- unlist(
    lapply(backends, `[[`, "sidecars"),
    use.names = FALSE
  )
  if (
    anyDuplicated(sidecar_claims) ||
      any(sidecar_claims %in% planned_filenames)
  ) {
    return(builder_plan_error(
      "Backend sidecar targets cannot be claimed by another dataset.",
      "backend_sidecar_conflict"
    ))
  }

  viewer_bundle_asset_records <- lapply(entries, function(entry) {
    .builder_plan_asset_set(entry$settings$viewer_bundle_assets)
  })
  private_user_asset_records <- lapply(entries, function(entry) {
    .builder_plan_asset_set(entry$settings$private_assets)
  })
  valid_asset_sets <- all(vapply(
    c(viewer_bundle_asset_records, private_user_asset_records),
    `[[`,
    logical(1),
    "valid"
  ))
  if (!valid_asset_sets) {
    return(builder_plan_error(
      paste0(
        "Asset manifests must use unique non-empty legacy targets or ",
        "typed source/target/artifact claims."
      ),
      "invalid_asset_manifest"
    ))
  }
  viewer_bundle_asset_sets <- lapply(
    viewer_bundle_asset_records,
    `[[`,
    "targets"
  )
  private_asset_sets <- lapply(seq_along(entries), function(index) {
    c(
      planned_filenames[[index]],
      backends[[index]]$sidecars,
      private_user_asset_records[[index]]$targets
    )
  })
  viewer_bundle_asset_targets <- unlist(
    viewer_bundle_asset_sets,
    use.names = FALSE
  )
  private_asset_targets <- unlist(private_asset_sets, use.names = FALSE)
  viewer_bundle_asset_claim_sets <- lapply(seq_along(entries), function(index) {
    viewer_bundle_asset_records[[index]]$claims
  })
  private_asset_claim_sets <- lapply(seq_along(entries), function(index) {
    c(
      .builder_plan_internal_asset_claims(
        c(planned_filenames[[index]], backends[[index]]$sidecars),
        entries[[index]]
      ),
      private_user_asset_records[[index]]$claims
    )
  })
  all_viewer_bundle_asset_claims <- unlist(
    viewer_bundle_asset_claim_sets,
    recursive = FALSE,
    use.names = FALSE
  )
  all_private_asset_claims <- unlist(
    private_asset_claim_sets,
    recursive = FALSE,
    use.names = FALSE
  )
  if (
    .builder_plan_claim_target_conflict(all_viewer_bundle_asset_claims) ||
      .builder_plan_claim_target_conflict(all_private_asset_claims)
  ) {
    return(builder_plan_error(
      "One asset target is claimed by different sources or artifacts.",
      "asset_target_conflict"
    ))
  }
  if (length(intersect(viewer_bundle_asset_targets, private_asset_targets))) {
    return(builder_plan_error(
      "An asset cannot be both Viewer-bundle eligible and private.",
      "asset_scope_conflict"
    ))
  }

  items <- tryCatch(
    lapply(seq_along(entries), function(index) {
      entry <- entries[[index]]
      settings <- entry$settings
      filename <- planned_filenames[[index]]
      has_marker_genes <- .builder_state_content_available(
        entry,
        "marker_genes"
      )
      analyses <- states[[index]]$analyses
      analysis_dependency_graph <- .builder_plan_analysis_graph(
        analyses,
        has_marker_genes
      )
      artifact_identity <- .builder_plan_artifact_identity(
        entry,
        included_groups[[index]],
        included_projections[[index]],
        analyses
      )
      source_snapshot_identity <- .builder_plan_source_snapshot_identity(entry)
      alignments <- .builder_plan_partition_alignments(
        settings$images %||% list()
      )
      spatial_sections <- artifact_identity$spatial_sections %||% character()
      profile_extras <- entry$profile$extras %||% list()
      has_trekker <- any(vapply(
        profile_extras,
        function(extra) {
          identical(extra$key %||% "", "trekker") && isTRUE(extra$found)
        },
        logical(1)
      )) ||
        isTRUE(
          entry$dataset_profile$content$trekker$detected %||% FALSE
        )
      alignment_sections <- unique(c(
        spatial_sections,
        if (has_trekker) "trekker" else character()
      ))
      alignments$spatial <- alignments$spatial[
        intersect(names(alignments$spatial), spatial_sections)
      ]
      if (!has_trekker) {
        alignments$trekker <- NULL
      }
      image_sections <- names(alignments$spatial) %||% character()
      aligned_sections <- unique(c(
        image_sections,
        if (!is.null(alignments$trekker)) "trekker" else character()
      ))
      default_group <- settings$default_group %||% settings$groups[[1L]]
      default_group_levels <- entry$levels[[default_group]] %||% character()
      default_group_overrides <- builder_settings_color_overrides(settings)[[
        default_group
      ]] %||%
        character()
      custom_color_levels <- intersect(
        default_group_levels,
        names(default_group_overrides)
      )
      custom_color_levels <- custom_color_levels[vapply(
        default_group_overrides[custom_color_levels],
        function(value) !is.null(builder_normalize_hex_color(value)),
        logical(1)
      )]
      runtime_costs <- c(
        percent_mt_ribo = "seconds",
        most_expressed = "seconds",
        marker_genes = "minutes",
        enriched_pathways = "network-dependent"
      )
      item <- list(
        id = entry$id,
        name = labels[index],
        filename = filename,
        organism = settings$organism,
        assay = settings$assay,
        layer = settings$layer,
        groups = settings$groups,
        included_groups = included_groups[[index]],
        reductions = settings$reductions,
        included_projections = included_projections[[index]],
        analyses = analyses,
        analysis_dependency_graph = analysis_dependency_graph,
        artifact_identity = artifact_identity,
        cell_count = as.integer(
          entry$profile$n_cells %||%
            length(artifact_identity$cells %||% character())
        ),
        gene_count = as.integer(
          entry$profile$n_genes %||%
            length(artifact_identity$features %||% character())
        ),
        histology_coverage = list(
          sections = spatial_sections,
          with_histology = intersect(spatial_sections, image_sections),
          missing_histology = setdiff(spatial_sections, image_sections)
        ),
        spatial_alignment = list(
          section_count = as.integer(length(alignment_sections)),
          image_count = as.integer(length(aligned_sections)),
          saved_count = as.integer(length(aligned_sections)),
          points_only = setdiff(alignment_sections, aligned_sections)
        ),
        estimated_runtime = if (length(analyses)) {
          paste(unique(unname(runtime_costs[analyses])), collapse = ", ")
        } else {
          "no optional analysis runtime"
        },
        estimated_disk_bytes = as.double(
          source_snapshot_identity$closure_bytes %||% 0
        ),
        tables = settings$tables %||% list(),
        images = alignments$spatial,
        trekker_alignment = alignments$trekker,
        colors = builder_resolve_colors(settings, entry$levels %||% list()),
        color_custom_count = as.integer(length(custom_color_levels)),
        nUMI = settings$nUMI %||% entry$profile$nUMI,
        nGene = settings$nGene %||% entry$profile$nGene,
        default_group = default_group,
        default_projection = settings$default_projection %||%
          settings$reductions[[1L]],
        metadata_policy = states[[index]]$metadata_policy %||%
          list(
            included = unique(c(
              "cell_barcode",
              included_groups[[index]],
              settings$nUMI %||% entry$profile$nUMI,
              settings$nGene %||% entry$profile$nGene
            )),
            excluded = character()
          ),
        nomenclature = settings$nomenclature,
        expression_backend = backends[[index]]$mode,
        sidecars = backends[[index]]$sidecars,
        source_snapshot_identity = source_snapshot_identity,
        readiness = states[[index]]$readiness,
        manifest = states[[index]]$manifest,
        viewer_page_expectations = states[[index]]$page_expectations,
        acknowledgements = states[[index]]$acknowledgements,
        viewer_bundle_assets = viewer_bundle_asset_sets[[index]],
        private_assets = private_asset_sets[[index]],
        viewer_bundle_asset_claims = viewer_bundle_asset_claim_sets[[index]],
        private_asset_claims = private_asset_claim_sets[[index]]
      )
      if (!is.null(settings$recommendations)) {
        item$recommendations <- settings$recommendations
      }
      .builder_plan_deep_copy(item)
    }),
    error = function(error) error
  )
  if (inherits(items, "condition")) {
    code <- switch(
      conditionMessage(items),
      unsafe_reference = "unsafe_reference",
      invalid_snapshot_identity = "invalid_snapshot_identity",
      "invalid_frozen_value"
    )
    return(builder_plan_error(
      "BuildPlan values could not be frozen safely.",
      code
    ))
  }

  filenames <- vapply(items, `[[`, "", "filename")
  if (anyDuplicated(filenames)) {
    return(builder_plan_error(
      "Generated dataset filenames must be unique.",
      "duplicate_dataset_filename"
    ))
  }

  target_names <- unlist(
    lapply(items, function(item) {
      c(item$filename, item$sidecars)
    }),
    use.names = FALSE
  )
  targets <- file.path(out_dir, target_names)
  if (isTRUE(make_app)) {
    targets <- c(targets, file.path(out_dir, "cerebro_app"))
  }

  names(items) <- dataset_order
  manifests <- lapply(items, `[[`, "manifest")
  source_snapshot_identities <- lapply(
    items,
    `[[`,
    "source_snapshot_identity"
  )
  metadata_policy <- lapply(items, `[[`, "metadata_policy")
  backend_sidecars <- lapply(items, function(item) {
    list(mode = item$expression_backend, sidecars = item$sidecars)
  })
  analysis_dependency_graph <- lapply(
    items,
    `[[`,
    "analysis_dependency_graph"
  )
  viewer_page_expectations <- lapply(
    items,
    `[[`,
    "viewer_page_expectations"
  )
  acknowledgements <- lapply(items, `[[`, "acknowledgements")
  viewer_bundle_asset_claims <- .builder_plan_dedupe_claims(
    all_viewer_bundle_asset_claims
  )
  private_asset_claims <- .builder_plan_dedupe_claims(
    all_private_asset_claims
  )
  viewer_bundle_assets <- vapply(
    viewer_bundle_asset_claims,
    `[[`,
    character(1),
    "target"
  )
  private_assets <- vapply(
    private_asset_claims,
    `[[`,
    character(1),
    "target"
  )
  initial_dataset_supplied <- "initial_dataset" %in% names(app_options)
  default_app_options <- list(
    enabled = isTRUE(make_app),
    show_upload_ui = FALSE,
    initial_dataset = dataset_order[[1L]],
    initial_dataset_mode = "automatic",
    welcome_message = "Welcome to CerebroNexus!",
    point_size = list(overview_projection_point_size = 5),
    variable_to_compare = FALSE,
    host = "127.0.0.1",
    port = 8080L,
    max_request_size = 8000,
    display_mode = "normal",
    launch_browser = TRUE
  )
  frozen_app_options <- utils::modifyList(
    default_app_options,
    app_options,
    keep.null = TRUE
  )
  frozen_app_options$enabled <- isTRUE(make_app)
  frozen_app_options$initial_dataset_mode <- if (initial_dataset_supplied) {
    "explicit"
  } else {
    "automatic"
  }
  if (
    !builder_has_text(frozen_app_options$initial_dataset %||% "") ||
      !frozen_app_options$initial_dataset %in% dataset_order
  ) {
    return(builder_plan_error(
      "The generated-app initial dataset is not part of BuildPlan.",
      "invalid_app_options"
    ))
  }
  output_release <- list(
    directory = out_dir,
    overwrite = isTRUE(overwrite),
    replacement_policy = if (isTRUE(overwrite)) {
      "replace_existing_atomically"
    } else {
      "preserve_existing"
    },
    estimated_runtime = paste(
      unique(vapply(items, `[[`, character(1), "estimated_runtime")),
      collapse = "; "
    ),
    estimated_disk_bytes = sum(vapply(
      items,
      `[[`,
      numeric(1),
      "estimated_disk_bytes"
    )),
    targets = targets
  )

  plan <- list(
    error = NULL,
    error_code = NULL,
    details = list(),
    revision = plan_revision,
    readiness = "ready",
    dataset_order = dataset_order,
    out_dir = out_dir,
    make_app = isTRUE(make_app),
    app_contract_version = app_contract_version,
    overwrite = isTRUE(overwrite),
    items = unname(items),
    targets = targets,
    existing_targets = targets[file.exists(targets) | dir.exists(targets)],
    manifests = manifests,
    manifest = .builder_plan_release_manifest(manifests),
    source_snapshot_identities = source_snapshot_identities,
    metadata_policy = metadata_policy,
    backend_sidecars = backend_sidecars,
    analysis_dependency_graph = analysis_dependency_graph,
    viewer_page_expectations = viewer_page_expectations,
    viewer_bundle_assets = viewer_bundle_assets,
    private_assets = private_assets,
    viewer_bundle_asset_claims = viewer_bundle_asset_claims,
    private_asset_claims = private_asset_claims,
    acknowledgements = acknowledgements,
    app_options = frozen_app_options,
    output_release = output_release,
    expected_prior_identity = expected_prior_identity
  )
  frozen <- tryCatch(
    .builder_plan_deep_copy(plan),
    error = function(error) error
  )
  if (inherits(frozen, "condition")) {
    return(builder_plan_error(
      "BuildPlan values could not be frozen safely.",
      "unsafe_reference"
    ))
  }
  structure(frozen, class = c("builder_build_plan", "list"))
}

builder_make_plan <- function(
  entries,
  out_dir,
  make_app = FALSE,
  overwrite = FALSE,
  app_options = list()
) {
  builder_freeze_plan(
    entries = entries,
    out_dir = out_dir,
    make_app = make_app,
    overwrite = overwrite,
    app_options = app_options
  )
}
