##----------------------------------------------------------------------------##
## Versioned, structural facts about one Seurat data set.
##
## Every cell-associated component is compared with Cells(object) by barcode.
## Counts and row positions are diagnostics, never identity contracts.
##----------------------------------------------------------------------------##

# Source ----

.builder_profile_abort <- function(code, message) {
  condition <- structure(
    list(message = message, call = NULL, code = code),
    class = c("builder_profile_error", "error", "condition")
  )
  stop(condition)
}

.builder_profile_scalar_text <- function(value, allow_empty = FALSE) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    (isTRUE(allow_empty) || nzchar(value))
}

.builder_profile_plain_data <- function(value, depth = 0L) {
  if (depth > 20L) {
    return(FALSE)
  }
  if (
    is.object(value) ||
      is.function(value) ||
      is.environment(value) ||
      is.language(value) ||
      is.symbol(value) ||
      typeof(value) == "externalptr"
  ) {
    return(FALSE)
  }
  if (is.list(value)) {
    return(all(vapply(
      value,
      .builder_profile_plain_data,
      logical(1),
      depth = depth + 1L
    )))
  }
  is.null(value) || is.atomic(value)
}

builder_profile_source <- function(source) {
  if (
    !is.list(source) ||
      is.object(source) ||
      !.builder_profile_plain_data(source)
  ) {
    .builder_profile_abort(
      "invalid_source",
      "A dataset source must be an inert data record."
    )
  }
  if (
    !.builder_profile_scalar_text(source[["type"]]) ||
      !.builder_profile_scalar_text(source[["location"]])
  ) {
    .builder_profile_abort(
      "invalid_source",
      "A dataset source requires type and location strings."
    )
  }

  clean_text <- function(value) {
    value <- as.character(value)
    attributes(value) <- NULL
    value
  }

  optional_text <- function(name) {
    value <- source[[name]]
    if (is.null(value)) {
      return(NULL)
    }
    if (!.builder_profile_scalar_text(value)) {
      .builder_profile_abort(
        "invalid_source",
        paste("Dataset source", name, "must be a non-empty string.")
      )
    }
    clean_text(value)
  }

  list(
    type = clean_text(source[["type"]]),
    location = clean_text(source[["location"]]),
    fingerprint = optional_text("fingerprint"),
    format = optional_text("format")
  )
}

# Identity ----

.builder_profile_ids <- function(ids) {
  if (is.null(ids)) {
    return(character())
  }
  as.character(ids)
}

builder_identity_profile <- function(ids, expected = ids) {
  match <- builder_match_cells(ids, expected, mode = "exact")

  list(
    ids = match$ids,
    count = match$count,
    valid = match$valid,
    duplicates = match$duplicates,
    blanks = match$blanks,
    missing = match$missing,
    extra = match$extra,
    order_matches = match$order_matches,
    coverage = match$coverage,
    canonical_ids = match$canonical_ids,
    reorder_index = match$reorder_index
  )
}

builder_feature_profile <- function(ids, expected) {
  profile <- builder_identity_profile(ids, expected)
  usable_ids <- profile$ids[!is.na(profile$ids) & nzchar(profile$ids)]
  folded <- toupper(usable_ids)
  collision_keys <- unique(folded[duplicated(folded)])
  casefold_duplicates <- usable_ids[folded %in% collision_keys]
  structurally_usable <- profile$count > 0L &&
    !length(profile$blanks) &&
    !length(profile$duplicates) &&
    !length(casefold_duplicates) &&
    !length(profile$extra)
  relation <- if (!structurally_usable) {
    "invalid"
  } else if (profile$valid) {
    "complete"
  } else {
    "subset"
  }
  profile$casefold_duplicates <- casefold_duplicates
  profile$relation <- relation
  profile$usable <- !identical(relation, "invalid")
  profile
}

.builder_profile_identity_diagnostics <- function(identity) {
  diagnostics <- character()
  if (length(identity$blanks)) {
    diagnostics <- c(diagnostics, "blank_ids")
  }
  if (length(identity$duplicates)) {
    diagnostics <- c(diagnostics, "duplicate_ids")
  }
  if (length(identity$missing)) {
    diagnostics <- c(diagnostics, "missing_ids")
  }
  if (length(identity$extra)) {
    diagnostics <- c(diagnostics, "extra_ids")
  }
  if (identity$valid && !identity$order_matches) {
    diagnostics <- c(diagnostics, "reordered")
  }
  diagnostics
}

# Assays & layers ----

.builder_profile_layer_root <- function(layer) {
  if (startsWith(layer, "scale.data.")) {
    return("scale.data")
  }
  sub("[.].*$", "", layer)
}

.builder_profile_layer_api <- function() {
  namespace <- asNamespace("SeuratObject")
  required <- c("Layers", "Cells", "Features")
  if (
    !all(vapply(
      required,
      exists,
      logical(1),
      envir = namespace,
      inherits = FALSE
    ))
  ) {
    return(NULL)
  }
  list(
    layers = get("Layers", envir = namespace, inherits = FALSE),
    cells = get("Cells", envir = namespace, inherits = FALSE),
    features = get("Features", envir = namespace, inherits = FALSE)
  )
}

.builder_profile_legacy_layers <- function(assay) {
  candidates <- c("counts", "data", "scale.data")
  candidates[vapply(
    candidates,
    function(layer) {
      if (!layer %in% methods::slotNames(assay)) {
        return(FALSE)
      }
      value <- methods::slot(assay, layer)
      length(dim(value)) == 2L && all(dim(value) > 0L)
    },
    logical(1)
  )]
}

.builder_profile_layer_names <- function(assay, layer_api) {
  if (is.null(layer_api) || inherits(assay, "Assay")) {
    return(.builder_profile_legacy_layers(assay))
  }
  tryCatch(layer_api$layers(assay), error = function(error) character())
}

.builder_profile_layer_ids <- function(assay, layer, what, layer_api) {
  if (is.null(layer_api) || inherits(assay, "Assay")) {
    if (!layer %in% methods::slotNames(assay)) {
      return(character())
    }
    value <- methods::slot(assay, layer)
    return(if (identical(what, "cells")) colnames(value) else rownames(value))
  }
  method <- layer_api[[what]]
  tryCatch(method(assay, layer = layer), error = function(error) character())
}

.builder_profile_physical_layer <- function(
  assay,
  layer,
  expected_cells,
  expected_features,
  layer_api
) {
  cells <- builder_identity_profile(
    .builder_profile_layer_ids(assay, layer, "cells", layer_api),
    expected_cells
  )
  features <- builder_feature_profile(
    .builder_profile_layer_ids(assay, layer, "features", layer_api),
    expected_features
  )
  diagnostics <- c(
    .builder_profile_identity_diagnostics(cells),
    if (!features$count) "no_features" else character(),
    if (!features$usable) {
      "unusable_feature_ids"
    } else if (!features$valid) {
      "partial_feature_coverage"
    } else {
      character()
    }
  )
  list(
    name = layer,
    kind = "physical",
    members = layer,
    cells = cells,
    features = features,
    exportable = cells$valid && features$usable,
    diagnostics = unique(diagnostics)
  )
}

.builder_profile_find_partition <- function(
  expected_cells,
  memberships,
  max_solutions = 2L,
  max_nodes = 100000L,
  max_depth = 128L,
  max_conflict_work = 5000000
) {
  expected_cells <- as.character(expected_cells)
  if (
    !length(expected_cells) ||
      anyNA(expected_cells) ||
      any(!nzchar(expected_cells)) ||
      anyDuplicated(expected_cells)
  ) {
    return(list(
      status = "none",
      layers = character(),
      solutions = list(),
      strategy = "validation"
    ))
  }
  memberships <- lapply(memberships, as.character)
  usable <- vapply(
    memberships,
    function(ids) {
      length(ids) > 0L &&
        length(ids) < length(expected_cells) &&
        !anyNA(ids) &&
        !any(!nzchar(ids)) &&
        !anyDuplicated(ids) &&
        !length(setdiff(ids, expected_cells))
    },
    logical(1)
  )
  memberships <- memberships[usable]
  if (length(memberships) < 2L) {
    return(list(
      status = "none",
      layers = character(),
      solutions = list(),
      strategy = "linear"
    ))
  }
  memberships <- memberships[order(names(memberships), method = "radix")]
  expected_index <- seq_along(expected_cells)
  names(expected_index) <- expected_cells
  encoded <- lapply(memberships, function(ids) {
    unname(expected_index[ids])
  })
  claims <- unlist(encoded, use.names = FALSE)
  if (
    length(claims) == length(expected_cells) &&
      !anyDuplicated(claims) &&
      setequal(claims, seq_along(expected_cells))
  ) {
    solution <- names(memberships)
    return(list(
      status = "unique",
      layers = solution,
      solutions = list(solution),
      strategy = "linear"
    ))
  }

  layer_index <- seq_along(encoded)
  claims_by_layer <- rep.int(layer_index, lengths(encoded))
  cell_to_layers <- split(
    claims_by_layer,
    factor(claims, levels = seq_along(expected_cells))
  )
  conflict_work <- unname(sum(lengths(cell_to_layers)^2))
  if (conflict_work > max_conflict_work) {
    return(list(
      status = "budget_exceeded",
      layers = character(),
      solutions = list(),
      strategy = "search",
      budget_reason = "conflict_work",
      conflict_work = conflict_work,
      nodes = 0L
    ))
  }
  conflicts <- lapply(encoded, function(ids) {
    unique(unlist(cell_to_layers[ids], use.names = FALSE))
  })

  solutions <- list()
  nodes <- 0L
  budget_exceeded <- FALSE
  budget_reason <- NULL
  search <- function(covered, available, chosen, depth = 0L) {
    if (budget_exceeded) {
      return(invisible(NULL))
    }
    nodes <<- nodes + 1L
    if (nodes > max_nodes) {
      budget_exceeded <<- TRUE
      budget_reason <<- "nodes"
      return(invisible(NULL))
    }
    if (length(solutions) >= max_solutions) {
      return(invisible(NULL))
    }
    if (all(covered)) {
      if (length(chosen) >= 2L) {
        solutions[[length(solutions) + 1L]] <<- sort(
          names(memberships)[chosen],
          method = "radix"
        )
      }
      return(invisible(NULL))
    }
    if (depth >= max_depth) {
      budget_exceeded <<- TRUE
      budget_reason <<- "depth"
      return(invisible(NULL))
    }
    pivot <- which(!covered)[1L]
    choices <- intersect(cell_to_layers[[pivot]], available)
    for (choice in choices) {
      ids <- encoded[[choice]]
      remaining <- setdiff(available, conflicts[[choice]])
      next_covered <- covered
      next_covered[ids] <- TRUE
      search(
        next_covered,
        remaining,
        c(chosen, choice),
        depth + 1L
      )
      if (budget_exceeded || length(solutions) >= max_solutions) {
        break
      }
    }
    invisible(NULL)
  }
  search(
    rep(FALSE, length(expected_cells)),
    layer_index,
    integer()
  )
  if (budget_exceeded) {
    return(list(
      status = "budget_exceeded",
      layers = character(),
      solutions = solutions,
      strategy = "search",
      budget_reason = budget_reason,
      conflict_work = conflict_work,
      nodes = nodes
    ))
  }
  if (!length(solutions)) {
    return(list(
      status = "none",
      layers = character(),
      solutions = list(),
      strategy = "search"
    ))
  }
  keys <- vapply(solutions, paste, collapse = "\r", character(1))
  solutions <- solutions[!duplicated(keys)]
  if (length(solutions) == 1L) {
    return(list(
      status = "unique",
      layers = solutions[[1L]],
      solutions = solutions,
      strategy = "search"
    ))
  }
  list(
    status = "ambiguous",
    layers = character(),
    solutions = solutions,
    strategy = "search"
  )
}

.builder_profile_logical_layer <- function(
  root,
  members,
  physical,
  expected_cells,
  expected_features
) {
  direct <- members[
    !grepl(
      ".",
      substring(members, nchar(root) + 2L),
      fixed = TRUE
    )
  ]
  member_cells <- lapply(direct, function(member) {
    physical[[member]]$cells$ids
  })
  names(member_cells) <- direct
  partition <- .builder_profile_find_partition(expected_cells, member_cells)
  selected <- if (identical(partition$status, "unique")) {
    partition$layers
  } else {
    direct
  }
  cells <- builder_identity_profile(
    unlist(member_cells[selected], use.names = FALSE),
    expected_cells
  )
  feature_members <- lapply(selected, function(member) {
    physical[[member]]$features
  })
  names(feature_members) <- selected
  feature_members_usable <- length(feature_members) > 0L &&
    all(vapply(
      feature_members,
      function(features) features$usable,
      logical(1)
    ))
  feature_union <- unique(unlist(
    lapply(
      feature_members,
      function(features) features$ids
    ),
    use.names = FALSE
  ))
  features <- builder_feature_profile(feature_union, expected_features)
  features$usable <- feature_members_usable && features$usable
  feature_keys <- vapply(
    feature_members,
    function(features) paste(sort(features$ids), collapse = "\r"),
    character(1)
  )
  heterogeneous_features <- length(unique(feature_keys)) > 1L
  diagnostics <- .builder_profile_identity_diagnostics(cells)
  if (identical(partition$status, "ambiguous")) {
    diagnostics <- c(diagnostics, "ambiguous_partition")
  } else if (identical(partition$status, "budget_exceeded")) {
    diagnostics <- c(diagnostics, "partition_search_budget_exceeded")
  } else if (identical(partition$status, "none")) {
    diagnostics <- c(diagnostics, "incomplete_partition")
  }
  if (length(setdiff(members, direct))) {
    diagnostics <- c(diagnostics, "nested_candidates_deferred")
  }
  if (!features$usable) {
    diagnostics <- c(diagnostics, "unusable_feature_ids")
  } else if (heterogeneous_features) {
    diagnostics <- c(diagnostics, "incompatible_split_feature_sets")
  } else if (!features$valid) {
    diagnostics <- c(diagnostics, "partial_feature_coverage")
  }
  list(
    name = root,
    kind = "logical",
    members = selected,
    candidate_members = direct,
    nested_candidates = setdiff(members, direct),
    partition_status = partition$status,
    solutions = partition$solutions,
    cells = cells,
    features = features,
    feature_members = feature_members,
    heterogeneous_features = heterogeneous_features,
    exportable = identical(partition$status, "unique") &&
      cells$valid &&
      features$usable &&
      !heterogeneous_features,
    diagnostics = unique(diagnostics)
  )
}

builder_profile_assay <- function(
  assay,
  expected_cells,
  name = NULL,
  expected_features = NULL,
  layer_api = .builder_profile_layer_api()
) {
  if (is.null(expected_features)) {
    expected_features <- tryCatch(
      if (is.null(layer_api)) rownames(assay) else layer_api$features(assay),
      error = function(error) rownames(assay)
    )
  }
  layers <- .builder_profile_layer_names(assay, layer_api)
  physical <- lapply(layers, function(layer) {
    .builder_profile_physical_layer(
      assay,
      layer,
      expected_cells,
      expected_features,
      layer_api
    )
  })
  names(physical) <- layers

  roots <- unique(vapply(layers, .builder_profile_layer_root, character(1)))
  logical <- list()
  for (root in roots) {
    members <- layers[startsWith(layers, paste0(root, "."))]
    if (length(members) < 2L || root %in% layers) {
      next
    }
    logical[[root]] <- .builder_profile_logical_layer(
      root,
      members,
      physical,
      expected_cells,
      expected_features
    )
  }
  profiles <- c(physical, logical)
  exportable <- names(profiles)[vapply(
    profiles,
    function(layer) isTRUE(layer$exportable),
    logical(1)
  )]

  list(
    name = name,
    layers = profiles,
    exportable_layers = exportable,
    exportable = length(exportable) > 0L
  )
}

builder_profile_assays <- function(object, expected_cells) {
  assays <- names(object@assays)
  profiles <- lapply(assays, function(name) {
    assay <- object[[name]]
    expected_features <- tryCatch(
      SeuratObject::Features(assay),
      error = function(error) rownames(assay)
    )
    builder_profile_assay(
      assay,
      expected_cells,
      name,
      expected_features = expected_features
    )
  })
  names(profiles) <- assays
  profiles
}

# Metadata ----

.builder_profile_metadata_column <- function(values, name) {
  as_text <- if (is.factor(values)) {
    as.character(values)
  } else if (is.atomic(values)) {
    as.character(values)
  } else {
    rep(NA_character_, length(values))
  }
  list(
    name = name,
    class = class(values),
    storage_type = typeof(values),
    count = length(values),
    missing = sum(is.na(values)),
    blanks = sum(!is.na(as_text) & !nzchar(as_text)),
    unique = length(unique(values)),
    non_missing = sum(!is.na(values)),
    unique_non_missing = length(unique(values[!is.na(values)])),
    supported = is.atomic(values) && !is.list(values)
  )
}

.builder_profile_group_reason <- function(values, name, n_cells) {
  qc_pattern <- paste0(
    "^(nCount|nFeature|nUMI|nGene)_?|",
    "^percent[._]|[._]score$|^S\\.Score$|^G2M\\.Score$"
  )
  if (!is.atomic(values) || is.list(values)) {
    return("unsupported metadata type")
  }
  if (grepl(qc_pattern, name, ignore.case = TRUE)) {
    return("QC metric, not a group")
  }
  distinct <- length(unique(values[!is.na(values)]))
  if (distinct < 2L) {
    return("only one value")
  }
  if (distinct >= n_cells) {
    return("one value per cell")
  }
  if (distinct > 100L) {
    return(paste0(distinct, " values; too many"))
  }
  if (
    !(is.factor(values) ||
      is.character(values) ||
      is.logical(values) ||
      is.integer(values))
  ) {
    return("continuous numeric metadata requires review")
  }
  NULL
}

builder_profile_metadata <- function(meta, expected_cells) {
  identity <- builder_identity_profile(rownames(meta), expected_cells)
  column_names <- colnames(meta)
  column_identity <- builder_identity_profile(column_names, column_names)
  columns <- lapply(seq_along(column_names), function(index) {
    .builder_profile_metadata_column(meta[[index]], column_names[[index]])
  })
  names(columns) <- column_names

  candidates <- character()
  conversions <- character()
  rejected <- character()
  if (!column_identity$valid) {
    keys <- column_names
    invalid_key <- is.na(keys) | !nzchar(keys)
    keys[invalid_key] <- paste0("<column_", which(invalid_key), ">")
    keys <- make.unique(keys)
    rejected <- stats::setNames(
      rep("metadata column name is missing or duplicated", length(keys)),
      keys
    )
  } else {
    for (index in seq_along(column_names)) {
      name <- column_names[[index]]
      values <- meta[[index]]
      reason <- .builder_profile_group_reason(
        values,
        name,
        length(expected_cells)
      )
      if (!is.null(reason)) {
        rejected[[name]] <- reason
        next
      }
      candidates <- c(candidates, name)
      if (!is.factor(values)) {
        conversions[[name]] <- paste0(typeof(values), " to factor")
      }
    }
  }

  list(
    identity = identity,
    column_identity = column_identity,
    columns = columns,
    groups = list(
      candidates = candidates,
      conversions = conversions,
      rejected = rejected
    )
  )
}

# Reductions ----

builder_profile_embeddings <- function(embeddings, expected_cells, name) {
  matrix_like <- is.matrix(embeddings) || is.data.frame(embeddings)
  values <- if (matrix_like) as.matrix(embeddings) else NULL
  ids <- if (matrix_like) rownames(embeddings) else character()
  cells <- builder_identity_profile(ids, expected_cells)
  dimensions <- if (matrix_like) as.integer(ncol(embeddings)) else 0L
  numeric <- matrix_like && is.numeric(values)
  finite <- numeric && all(is.finite(values))
  diagnostics <- .builder_profile_identity_diagnostics(cells)
  if (!matrix_like) {
    diagnostics <- c(diagnostics, "missing_embeddings")
  }
  if (matrix_like && !numeric) {
    diagnostics <- c(diagnostics, "non_numeric")
  }
  if (numeric && !finite) {
    diagnostics <- c(diagnostics, "non_finite")
  }
  if (dimensions < 2L) {
    diagnostics <- c(diagnostics, "fewer_than_two_dimensions")
  }
  structurally_valid <- matrix_like &&
    numeric &&
    finite &&
    dimensions >= 2L &&
    cells$valid
  is_pca <- grepl("pca", name, ignore.case = TRUE)

  list(
    name = name,
    dimensions = dimensions,
    numeric = numeric,
    finite = finite,
    cells = cells,
    structurally_valid = structurally_valid,
    is_pca = is_pca,
    selection_role = if (is_pca) "fallback_only" else "normal",
    exportable = structurally_valid,
    diagnostics = unique(diagnostics)
  )
}

builder_profile_reduction <- function(reduction, expected_cells, name) {
  embeddings <- tryCatch(
    SeuratObject::Embeddings(reduction),
    error = function(error) NULL
  )
  builder_profile_embeddings(embeddings, expected_cells, name)
}

builder_profile_reductions <- function(object, expected_cells) {
  reduction_names <- names(object@reductions)
  profiles <- lapply(reduction_names, function(name) {
    builder_profile_reduction(object[[name]], expected_cells, name)
  })
  names(profiles) <- reduction_names
  profiles
}

builder_profile_organism <- function(features) {
  features <- .builder_profile_ids(features)
  features <- features[!is.na(features) & nzchar(features)]
  features <- utils::head(features, 200L)
  if (!length(features)) {
    return(list(
      code = "other",
      confidence = 0,
      reason = "No feature identifiers were available for inference."
    ))
  }

  human <- mean(features == toupper(features))
  mouse <- mean(grepl("^[A-Z][a-z]", features))
  if (isTRUE(human > 0.8)) {
    list(
      code = "hg",
      confidence = unname(human),
      reason = "Most sampled feature identifiers use human-style symbols."
    )
  } else if (isTRUE(mouse > 0.6)) {
    list(
      code = "mm",
      confidence = unname(mouse),
      reason = "Most sampled feature identifiers use mouse-style symbols."
    )
  } else {
    list(
      code = "other",
      confidence = unname(max(human, mouse)),
      reason = "Feature identifiers do not strongly match one supported style."
    )
  }
}

# Manifest ----

.builder_profile_manifest_source <- function(source) {
  list(type = source$type, location = source$location)
}

.builder_profile_manifest_entry <- function(
  id,
  source,
  valid,
  summary,
  diagnostics,
  disposition = "preserved",
  pages = character(),
  verifier = NULL
) {
  builder_manifest_entry(
    id = id,
    source = .builder_profile_manifest_source(source),
    status = if (isTRUE(valid)) "valid" else "blocking",
    disposition = if (isTRUE(valid)) disposition else "rejected",
    artifact_scope = "both",
    summary = summary,
    diagnostics = diagnostics,
    compatibility = list(viewer = isTRUE(valid)),
    pages = pages,
    verifier = verifier
  )
}

builder_profile_core_manifest <- function(
  source,
  identity,
  assays,
  metadata,
  reductions
) {
  identity_valid <- identity$cells$valid &&
    identity$cells$count > 0L &&
    identity$features$valid
  expression_valid <- identity_valid &&
    identity$features$count > 0L &&
    any(vapply(assays, function(assay) assay$exportable, logical(1)))
  metadata_valid <- metadata$identity$valid && metadata$column_identity$valid
  group_candidates <- metadata$groups$candidates
  groups_valid <- metadata_valid && length(group_candidates) > 0L
  has_conversion <- any(
    group_candidates %in% names(metadata$groups$conversions)
  )
  usable_reductions <- names(reductions)[vapply(
    reductions,
    function(reduction) reduction$exportable,
    logical(1)
  )]

  entries <- list(
    .builder_profile_manifest_entry(
      "dataset_identity",
      source,
      identity_valid,
      "Dataset cell and feature identities were checked.",
      identity,
      pages = "data_info",
      verifier = "verify_dataset_identity"
    ),
    .builder_profile_manifest_entry(
      "expression",
      source,
      expression_valid,
      "Expression layers were matched to dataset cell barcodes.",
      list(
        assays = names(assays),
        exportable = vapply(
          assays,
          function(assay) assay$exportable,
          logical(1)
        )
      ),
      pages = "gene_expression",
      verifier = "verify_expression"
    ),
    .builder_profile_manifest_entry(
      "metadata",
      source,
      metadata_valid,
      "Metadata rows were matched to dataset cell barcodes.",
      list(
        rows = metadata$identity,
        columns = metadata$column_identity
      ),
      pages = c("data_info", "groups"),
      verifier = "verify_metadata"
    ),
    .builder_profile_manifest_entry(
      "groups",
      source,
      groups_valid,
      "Grouping candidates were classified from metadata.",
      metadata$groups,
      disposition = if (has_conversion) "converted" else "preserved",
      pages = c("groups", "color_management"),
      verifier = "verify_groups"
    )
  )

  for (name in names(reductions)) {
    reduction <- reductions[[name]]
    if (reduction$exportable) {
      entries[[length(entries) + 1L]] <- .builder_profile_manifest_entry(
        paste0("reduction:", name),
        source,
        TRUE,
        if (reduction$is_pca) {
          "PCA is available only as a fallback projection selection."
        } else {
          "Reduction structure and cell barcodes were checked."
        },
        reduction,
        pages = "projection",
        verifier = "verify_reductions"
      )
    } else {
      entries[[length(entries) + 1L]] <- builder_manifest_entry(
        id = paste0("reduction:", name),
        source = .builder_profile_manifest_source(source),
        status = "valid",
        disposition = "rejected",
        artifact_scope = "both",
        summary = "This reduction is unsafe and is excluded from choices.",
        diagnostics = reduction,
        compatibility = list(viewer = FALSE),
        pages = character(),
        verifier = "verify_reductions"
      )
    }
  }

  entries[[length(entries) + 1L]] <- .builder_profile_manifest_entry(
    "projection",
    source,
    length(usable_reductions) > 0L,
    "At least one Viewer-compatible projection is required.",
    list(usable_reductions = usable_reductions),
    pages = "projection",
    verifier = "verify_projection"
  )
  builder_content_manifest(entries)
}

# Assembly ----

builder_dataset_profile <- function(object, source) {
  source <- builder_profile_source(source)
  if (!inherits(object, "Seurat")) {
    .builder_profile_abort(
      "unsupported_object",
      "Dataset profiles currently require a Seurat object."
    )
  }

  cells <- tryCatch(
    SeuratObject::Cells(object),
    error = function(error) character()
  )
  features <- tryCatch(
    SeuratObject::Features(object),
    error = function(error) character()
  )
  identity <- list(
    cells = builder_identity_profile(cells),
    features = builder_feature_profile(features, features),
    metadata = builder_identity_profile(rownames(object@meta.data), cells)
  )
  assays <- builder_profile_assays(object, cells)
  metadata <- builder_profile_metadata(object@meta.data, cells)
  reductions <- builder_profile_reductions(object, cells)
  assay_names <- names(assays)
  default_assay <- tryCatch(
    SeuratObject::DefaultAssay(object),
    error = function(error) if (length(assay_names)) assay_names[[1L]] else NULL
  )
  if (!default_assay %in% assay_names) {
    default_assay <- if (length(assay_names)) assay_names[[1L]] else NULL
  }
  content_context <- builder_profile_content_context(
    source = source,
    cells = cells,
    features = features,
    metadata = object@meta.data,
    assays = assays,
    default_assay = default_assay,
    groups = metadata$groups,
    reductions = reductions
  )
  content <- builder_profile_optional_content(object, content_context)
  object_version <- tryCatch(
    as.character(methods::slot(object, "version")),
    error = function(error) NA_character_
  )
  spatial_fact <- content$spatial$normalized
  spatial_sections <- vapply(
    spatial_fact$sections,
    function(section) section$name,
    character(1)
  )
  spatial_summary <- list(
    section_count = spatial_fact$section_count,
    sections = spatial_sections,
    sections_truncated = spatial_fact$sections_truncated,
    section_names_truncated = spatial_fact$section_names_truncated,
    deferred = TRUE
  )
  manifest <- builder_profile_core_manifest(
    source,
    identity,
    assays,
    metadata,
    reductions
  )

  structure(
    list(
      schema_version = 2L,
      source = source,
      object = list(class = class(object), version = object_version),
      identity = identity,
      assays = assays,
      default_assay = default_assay,
      metadata = list(
        identity = metadata$identity,
        column_identity = metadata$column_identity,
        columns = metadata$columns
      ),
      groups = metadata$groups,
      reductions = reductions,
      content = content,
      spatial = spatial_summary,
      organism = builder_profile_organism(features),
      manifest = manifest
    ),
    class = c("builder_dataset_profile", "list")
  )
}
