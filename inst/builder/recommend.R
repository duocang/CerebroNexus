##----------------------------------------------------------------------------##
## Conservative recommendations derived only from DatasetProfile v2 facts.
##
## Each decision has one authoritative `value`. Supporting fields explain why
## it was chosen and whether a person must confirm it before a build proceeds.
##----------------------------------------------------------------------------##

.builder_recommend_abort <- function(message) {
  stop(message, call. = FALSE)
}

.builder_recommend_scalar_text <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(trimws(value))
}

.builder_recommend_record <- function(
  value,
  reason,
  confidence,
  requires_confirmation,
  ...
) {
  if (!.builder_recommend_scalar_text(reason)) {
    .builder_recommend_abort("A recommendation reason must be one string.")
  }
  if (
    !is.numeric(confidence) ||
      length(confidence) != 1L ||
      is.na(confidence) ||
      !is.finite(confidence) ||
      confidence < 0 ||
      confidence > 1
  ) {
    .builder_recommend_abort(
      "Recommendation confidence must be between 0 and 1."
    )
  }
  if (
    !is.logical(requires_confirmation) ||
      length(requires_confirmation) != 1L ||
      is.na(requires_confirmation)
  ) {
    .builder_recommend_abort(
      "Recommendation confirmation must be one logical value."
    )
  }
  c(
    list(
      value = value,
      reason = trimws(reason),
      confidence = as.numeric(confidence),
      requires_confirmation = requires_confirmation
    ),
    list(...)
  )
}

.builder_recommend_profile <- function(profile) {
  if (
    !is.list(profile) ||
      !identical(profile$schema_version, 2L) ||
      !is.list(profile$identity) ||
      !is.list(profile$identity$cells) ||
      !is.list(profile$metadata) ||
      !is.list(profile$metadata$columns)
  ) {
    .builder_recommend_abort(
      "Recommendations require stable facts from DatasetProfile v2."
    )
  }
  count <- profile$identity$cells$count
  if (
    !is.numeric(count) ||
      length(count) != 1L ||
      is.na(count) ||
      !is.finite(count) ||
      count < 0 ||
      count > .Machine$integer.max ||
      count != floor(count)
  ) {
    .builder_recommend_abort("DatasetProfile v2 has a malformed cell count.")
  }
  profile
}

.builder_recommend_names <- function(values, label) {
  if (is.null(values)) {
    return(character())
  }
  if (
    !is.character(values) ||
      anyNA(values) ||
      any(!nzchar(trimws(values)))
  ) {
    .builder_recommend_abort(paste(label, "must contain non-empty names."))
  }
  unique(trimws(values))
}

.builder_recommend_normalize_name <- function(name) {
  name <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", name, perl = TRUE)
  name <- tolower(name)
  name <- gsub("[^a-z0-9]+", "_", name)
  gsub("(^_+|_+$)", "", name)
}

.builder_recommend_sensitive_name <- function(name) {
  normalized <- .builder_recommend_normalize_name(name)
  tokens <- strsplit(normalized, "_", fixed = TRUE)[[1L]]
  tokens <- tokens[nzchar(tokens)]
  sensitive_tokens <- c(
    "patient",
    "donor",
    "subject",
    "barcode",
    "email",
    "name",
    "mrn",
    "address",
    "phone",
    "clinical"
  )
  any(tokens %in% sensitive_tokens) ||
    grepl("sample.*id", gsub("_", "", normalized, fixed = TRUE))
}

.builder_recommend_category_name <- function(name) {
  normalized <- .builder_recommend_normalize_name(name)
  allowlist <- c(
    "cell_type",
    "celltype",
    "annotation",
    "seurat_clusters",
    "cluster",
    "cluster_id",
    "group",
    "condition",
    "treatment",
    "batch",
    "phase",
    "ident",
    "label"
  )
  normalized %in% allowlist
}

.builder_recommend_dependencies <- function(dependency_ids, name) {
  values <- dependency_ids[[name]]
  if (is.null(values)) {
    return(character())
  }
  values <- .builder_recommend_names(values, "Dependency ids")
  sort(values, method = "radix")
}

.builder_recommend_metadata_fact <- function(
  fact,
  name,
  n_cells,
  required,
  dependency_ids
) {
  if (
    !is.list(fact) ||
      !.builder_recommend_scalar_text(fact$name) ||
      !identical(fact$name, name) ||
      !is.character(fact$class) ||
      !length(fact$class) ||
      anyNA(fact$class) ||
      !is.logical(fact$supported) ||
      length(fact$supported) != 1L ||
      is.na(fact$supported)
  ) {
    .builder_recommend_abort("DatasetProfile v2 has a malformed metadata fact.")
  }
  counts <- c(fact$non_missing, fact$unique_non_missing)
  if (
    !is.numeric(counts) ||
      length(counts) != 2L ||
      anyNA(counts) ||
      any(!is.finite(counts)) ||
      any(counts < 0) ||
      any(counts > .Machine$integer.max) ||
      any(counts != floor(counts)) ||
      counts[[2L]] > counts[[1L]] ||
      counts[[1L]] > n_cells
  ) {
    .builder_recommend_abort("DatasetProfile v2 has a malformed metadata fact.")
  }

  class <- as.character(fact$class)
  non_missing <- as.integer(fact$non_missing)
  unique_non_missing <- as.integer(fact$unique_non_missing)
  is_required <- name %in% required
  sensitive <- .builder_recommend_sensitive_name(name)
  categorical_name <- .builder_recommend_category_name(name)
  safe_type <- any(class %in% c("factor", "ordered", "character", "logical"))
  numeric_type <- any(class %in% c("integer", "numeric", "double"))
  low_cardinality <- unique_non_missing >= 2L &&
    unique_non_missing <= 50L &&
    unique_non_missing <= n_cells * 0.05
  one_value_per_cell <- non_missing > 0L &&
    unique_non_missing >= non_missing

  disposition <- "excluded"
  effective_included <- FALSE
  confirmation <- FALSE
  preview_allowed <- FALSE
  group_eligible <- FALSE
  confidence <- 1
  reason <- "This column is not a safe low-cardinality export choice."

  if (!isTRUE(fact$supported)) {
    disposition <- if (is_required) "blocking" else "excluded"
    confirmation <- is_required
    reason <- if (is_required) {
      "This required column has an unsupported metadata type."
    } else {
      "This column has an unsupported metadata type."
    }
  } else if (sensitive) {
    disposition <- "attention"
    effective_included <- is_required
    confirmation <- TRUE
    confidence <- 0.5
    reason <- if (is_required) {
      "This required column looks sensitive and needs explicit confirmation."
    } else {
      "This column looks sensitive and is not included automatically."
    }
  } else if (numeric_type && categorical_name && low_cardinality) {
    disposition <- "included"
    effective_included <- TRUE
    preview_allowed <- TRUE
    group_eligible <- TRUE
    reason <- "This is an explicitly named low-cardinality category."
  } else if (is_required && safe_type && low_cardinality) {
    disposition <- "included"
    effective_included <- TRUE
    preview_allowed <- TRUE
    group_eligible <- TRUE
    reason <- "This required column is a safe categorical export."
  } else if (is_required) {
    disposition <- "attention"
    effective_included <- TRUE
    confirmation <- TRUE
    confidence <- 0.5
    reason <- paste0(
      "This required column is exportable but needs explicit confirmation."
    )
  } else if (unique_non_missing < 2L) {
    reason <- "This column does not contain two usable categories."
  } else if (one_value_per_cell) {
    reason <- "This column has one distinct value for every non-missing cell."
  } else if (!low_cardinality) {
    disposition <- "attention"
    confirmation <- TRUE
    confidence <- 0.5
    reason <- paste0(
      "This column has ",
      unique_non_missing,
      " values and needs confirmation beyond the automatic category limit."
    )
  } else if (safe_type) {
    disposition <- "included"
    effective_included <- TRUE
    preview_allowed <- TRUE
    group_eligible <- TRUE
    reason <- "This is a safe low-cardinality categorical column."
  } else if (numeric_type) {
    disposition <- "attention"
    confirmation <- TRUE
    confidence <- 0.5
    reason <- paste0(
      "This low-cardinality numeric column needs categorical confirmation."
    )
  } else {
    reason <- paste0(
      "Numeric metadata is excluded unless its name is explicitly categorical."
    )
  }

  .builder_recommend_record(
    value = disposition,
    reason = reason,
    confidence = confidence,
    requires_confirmation = confirmation,
    name = name,
    class = class,
    non_missing = non_missing,
    unique_non_missing = unique_non_missing,
    disposition = disposition,
    dependency_ids = .builder_recommend_dependencies(dependency_ids, name),
    preview_allowed = preview_allowed,
    effective_included = effective_included,
    group_eligible = group_eligible,
    sensitive = sensitive,
    required = is_required
  )
}

#' Recommend a conservative metadata export policy.
builder_recommend_metadata <- function(
  profile,
  required = character(),
  dependency_ids = list()
) {
  profile <- .builder_recommend_profile(profile)
  required <- .builder_recommend_names(required, "Required metadata")
  if (!is.list(dependency_ids) || is.object(dependency_ids)) {
    .builder_recommend_abort("Dependency ids must be a named list.")
  }
  if (length(dependency_ids)) {
    if (
      is.null(names(dependency_ids)) ||
        anyNA(names(dependency_ids)) ||
        any(!nzchar(names(dependency_ids)))
    ) {
      .builder_recommend_abort("Dependency ids must be a named list.")
    }
  }
  n_cells <- as.integer(profile$identity$cells$count)
  columns <- profile$metadata$columns
  column_names <- names(columns)
  if (is.null(column_names)) {
    if (length(columns)) {
      .builder_recommend_abort(
        "DatasetProfile v2 metadata columns must be named."
      )
    }
    column_names <- character()
  }
  if (
    anyNA(column_names) ||
      any(!nzchar(column_names)) ||
      anyDuplicated(column_names)
  ) {
    .builder_recommend_abort(
      "DatasetProfile v2 metadata columns are malformed."
    )
  }

  reserved_collision <- "cell_barcode" %in% column_names
  cell_barcode <- .builder_recommend_record(
    value = if (reserved_collision) "blocking" else "included",
    reason = if (reserved_collision) {
      paste0(
        "The metadata name cell_barcode is reserved for mandatory identity; ",
        "rename the metadata column before export."
      )
    } else {
      "Cell barcodes are required to preserve dataset identity."
    },
    confidence = 1,
    requires_confirmation = reserved_collision,
    name = "cell_barcode",
    class = "character",
    non_missing = n_cells,
    unique_non_missing = n_cells,
    disposition = if (reserved_collision) "blocking" else "included",
    dependency_ids = "core.cell_identity",
    preview_allowed = FALSE,
    effective_included = TRUE,
    group_eligible = FALSE,
    sensitive = FALSE,
    required = TRUE
  )
  metadata_names <- setdiff(column_names, "cell_barcode")
  records <- lapply(metadata_names, function(name) {
    .builder_recommend_metadata_fact(
      columns[[name]],
      name,
      n_cells,
      required,
      dependency_ids
    )
  })
  names(records) <- metadata_names

  missing_required <- setdiff(required, c("cell_barcode", column_names))
  for (name in missing_required) {
    records[[name]] <- .builder_recommend_record(
      value = "blocking",
      reason = "This required metadata column is missing.",
      confidence = 1,
      requires_confirmation = TRUE,
      name = name,
      class = "missing",
      non_missing = 0L,
      unique_non_missing = 0L,
      disposition = "blocking",
      dependency_ids = .builder_recommend_dependencies(dependency_ids, name),
      preview_allowed = FALSE,
      effective_included = FALSE,
      group_eligible = FALSE,
      sensitive = .builder_recommend_sensitive_name(name),
      required = TRUE
    )
  }
  records <- c(list(cell_barcode = cell_barcode), records)
  disposition <- vapply(records, `[[`, "", "disposition")
  effective <- vapply(records, `[[`, FALSE, "effective_included")
  included <- names(records)[effective]
  attention <- names(records)[disposition == "attention"]
  excluded <- names(records)[disposition == "excluded"]
  blocking <- names(records)[disposition == "blocking"]
  requires_confirmation <- length(attention) > 0L || length(blocking) > 0L

  .builder_recommend_record(
    value = included,
    reason = if (length(blocking)) {
      "Required metadata is missing or cannot be exported safely."
    } else if (length(attention)) {
      "Safe metadata is included; attention items need confirmation."
    } else {
      "Only safe low-cardinality metadata is included."
    },
    confidence = if (length(blocking)) {
      0
    } else if (length(attention)) {
      0.7
    } else {
      1
    },
    requires_confirmation = requires_confirmation,
    columns = records,
    included = included,
    attention = attention,
    excluded = excluded,
    blocking = blocking
  )
}

#' Recommend grouping columns from the effective metadata policy.
builder_recommend_groups <- function(profile, metadata_recommendation) {
  profile <- .builder_recommend_profile(profile)
  if (
    !is.list(metadata_recommendation) ||
      !is.list(metadata_recommendation$columns)
  ) {
    .builder_recommend_abort("Group recommendations require metadata policy.")
  }
  profile_order <- names(profile$metadata$columns)
  eligible <- profile_order[vapply(
    profile_order,
    function(name) {
      record <- metadata_recommendation$columns[[name]]
      is.list(record) &&
        isTRUE(record$effective_included) &&
        isTRUE(record$group_eligible) &&
        identical(record$disposition, "included")
    },
    logical(1)
  )]
  priority <- c(
    "cell_type",
    "celltype",
    "annotation",
    "seurat_clusters",
    "cluster",
    "condition",
    "treatment",
    "batch",
    "phase",
    "group"
  )
  normalized <- vapply(
    eligible,
    .builder_recommend_normalize_name,
    character(1)
  )
  priority_index <- match(priority, normalized)
  priority_index <- priority_index[!is.na(priority_index)]
  value <- if (length(priority_index)) {
    eligible[[priority_index[[1L]]]]
  } else if (length(eligible)) {
    eligible[[1L]]
  } else {
    NULL
  }
  .builder_recommend_record(
    value = value,
    reason = if (is.null(value)) {
      "No safe grouping column is available; choose or repair metadata."
    } else {
      paste("Selected", value, "from safe included metadata.")
    },
    confidence = if (is.null(value)) 0 else 1,
    requires_confirmation = is.null(value),
    included = eligible
  )
}

#' Recommend Viewer projection choices.
builder_recommend_projections <- function(profile) {
  profile <- .builder_recommend_profile(profile)
  reductions <- profile$reductions
  if (!is.list(reductions)) {
    .builder_recommend_abort("DatasetProfile v2 reductions are malformed.")
  }
  reduction_names <- names(reductions)
  if (is.null(reduction_names)) {
    reduction_names <- character()
  }
  valid <- vapply(
    reduction_names,
    function(name) {
      reduction <- reductions[[name]]
      is.list(reduction) &&
        is.logical(reduction$exportable) &&
        length(reduction$exportable) == 1L &&
        !is.na(reduction$exportable) &&
        isTRUE(reduction$exportable) &&
        is.logical(reduction$is_pca) &&
        length(reduction$is_pca) == 1L &&
        !is.na(reduction$is_pca)
    },
    logical(1)
  )
  exportable <- reduction_names[valid]
  pca <- exportable[vapply(
    exportable,
    function(name) isTRUE(reductions[[name]]$is_pca),
    logical(1)
  )]
  non_pca <- setdiff(exportable, pca)

  if (length(non_pca)) {
    normalized <- vapply(
      non_pca,
      .builder_recommend_normalize_name,
      character(1)
    )
    preferred <- match(c("umap", "tsne", "t_sne"), normalized)
    preferred <- preferred[!is.na(preferred)]
    value <- if (length(preferred)) {
      non_pca[[preferred[[1L]]]]
    } else {
      non_pca[[1L]]
    }
    return(.builder_recommend_record(
      value = value,
      reason = "Selected an exportable non-PCA Viewer projection.",
      confidence = 1,
      requires_confirmation = FALSE,
      included = non_pca,
      excluded = setdiff(reduction_names, non_pca)
    ))
  }
  if (length(pca) == 1L) {
    return(.builder_recommend_record(
      value = pca[[1L]],
      reason = "Only one exportable PCA fallback is available.",
      confidence = 0.5,
      requires_confirmation = TRUE,
      included = pca,
      excluded = setdiff(reduction_names, pca)
    ))
  }
  .builder_recommend_record(
    value = NULL,
    reason = if (length(pca)) {
      "Several PCA fallbacks are available; choose one explicitly."
    } else {
      "No exportable Viewer projection is available."
    },
    confidence = 0,
    requires_confirmation = TRUE,
    included = pca,
    excluded = setdiff(reduction_names, pca)
  )
}

#' Recommend an organism code while requiring confirmation of every inference.
builder_recommend_organism <- function(profile) {
  profile <- .builder_recommend_profile(profile)
  organism <- profile$organism
  valid <- is.list(organism) &&
    .builder_recommend_scalar_text(organism$code) &&
    organism$code %in% c("hg", "mm", "other") &&
    is.numeric(organism$confidence) &&
    length(organism$confidence) == 1L &&
    !is.na(organism$confidence) &&
    is.finite(organism$confidence) &&
    organism$confidence >= 0 &&
    organism$confidence <= 1 &&
    .builder_recommend_scalar_text(organism$reason)
  .builder_recommend_record(
    value = if (valid) organism$code else NULL,
    reason = if (valid) {
      organism$reason
    } else {
      "Organism facts are missing or malformed."
    },
    confidence = if (valid) organism$confidence else 0,
    requires_confirmation = TRUE
  )
}

#' Nomenclature choices supported for one confirmed organism.
builder_nomenclature_choices <- function(organism) {
  if (!.builder_recommend_scalar_text(organism)) {
    return(character())
  }
  switch(
    organism,
    hg = c("name", "ensembl", "gencode_v27"),
    mm = c("name", "ensembl", "gencode_vM16"),
    character()
  )
}

#' Validate an organism/nomenclature pair without cross-species coercion.
builder_validate_nomenclature <- function(organism, nomenclature) {
  if (
    !.builder_recommend_scalar_text(nomenclature) ||
      !nomenclature %in% builder_nomenclature_choices(organism)
  ) {
    .builder_recommend_abort(
      "The nomenclature is not valid for the selected organism."
    )
  }
  invisible(nomenclature)
}

.builder_recommend_organism_value <- function(organism) {
  if (is.list(organism)) {
    organism <- organism$value
  }
  if (!.builder_recommend_scalar_text(organism)) NULL else organism
}

#' Recommend symbol or Ensembl feature nomenclature from bounded profile ids.
builder_recommend_nomenclature <- function(profile, organism) {
  profile <- .builder_recommend_profile(profile)
  organism <- .builder_recommend_organism_value(organism)
  choices <- builder_nomenclature_choices(organism)
  features <- profile$identity$features$ids
  if (!is.character(features)) {
    features <- character()
  }
  features <- features[!is.na(features) & nzchar(features)]
  features <- utils::head(features, 500L)
  human_ensembl <- grepl("^ENSG[0-9]+([.][0-9]+)?$", features)
  mouse_ensembl <- grepl("^ENSMUSG[0-9]+([.][0-9]+)?$", features)
  symbol <- grepl("^[A-Za-z][A-Za-z0-9._-]*$", features)
  value <- NULL
  reason <- "Feature identifiers are empty or mixed; choose nomenclature."
  confidence <- 0
  confirmation <- TRUE
  if (length(features) && identical(organism, "hg") && all(human_ensembl)) {
    value <- "ensembl"
    reason <- "All sampled feature identifiers are human Ensembl ids."
    confidence <- 1
    confirmation <- FALSE
  } else if (
    length(features) &&
      identical(organism, "mm") &&
      all(mouse_ensembl)
  ) {
    value <- "ensembl"
    reason <- "All sampled feature identifiers are mouse Ensembl ids."
    confidence <- 1
    confirmation <- FALSE
  } else if (
    length(features) &&
      (identical(organism, "hg") || identical(organism, "mm")) &&
      all(symbol) &&
      !any(human_ensembl | mouse_ensembl)
  ) {
    value <- "name"
    reason <- "All sampled feature identifiers look like gene symbols."
    confidence <- 0.9
    confirmation <- FALSE
  } else if (!length(choices)) {
    reason <- "This organism has no automatic nomenclature choices."
  }
  .builder_recommend_record(
    value = value,
    reason = reason,
    confidence = confidence,
    requires_confirmation = confirmation,
    choices = choices
  )
}

#' Viewer gene-conversion table chosen only from confirmed organism identity.
builder_gene_conversion_initial_table <- function(
  organism,
  confirmed = FALSE
) {
  if (!isTRUE(confirmed) || !.builder_recommend_scalar_text(organism)) {
    return(NULL)
  }
  if (identical(organism, "hg")) {
    return("human")
  }
  if (identical(organism, "mm")) {
    return("mouse")
  }
  NULL
}

.builder_recommend_available <- function(available) {
  capabilities <- c("bpcells", "h5")
  blank <- stats::setNames(rep(FALSE, length(capabilities)), capabilities)
  if (!is.list(available)) {
    return(list(build = blank, viewer = blank))
  }
  normalize_side <- function(side) {
    values <- blank
    if (!is.list(side) && !(is.logical(side) && !is.null(names(side)))) {
      return(values)
    }
    for (name in capabilities) {
      value <- side[[name]]
      values[[name]] <- is.logical(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        isTRUE(value)
    }
    values
  }
  list(
    build = normalize_side(available$build),
    viewer = normalize_side(available$viewer)
  )
}

#' Recommend expression storage from bounded matrix facts and capabilities.
builder_recommend_backend <- function(matrix_summary, available) {
  normalized <- .builder_recommend_available(available)
  valid <- is.list(matrix_summary) &&
    is.numeric(matrix_summary$estimated_bytes) &&
    length(matrix_summary$estimated_bytes) == 1L &&
    !is.na(matrix_summary$estimated_bytes) &&
    is.finite(matrix_summary$estimated_bytes) &&
    matrix_summary$estimated_bytes >= 0 &&
    is.logical(matrix_summary$sparse) &&
    length(matrix_summary$sparse) == 1L &&
    !is.na(matrix_summary$sparse)
  if (!valid) {
    return(.builder_recommend_record(
      value = NULL,
      reason = "Matrix size or sparsity facts are missing or malformed.",
      confidence = 0,
      requires_confirmation = TRUE,
      available = normalized,
      blocking = "matrix_summary",
      dependency_actions = "Re-inspect the expression matrix."
    ))
  }
  bytes <- as.numeric(matrix_summary$estimated_bytes)
  sparse <- isTRUE(matrix_summary$sparse)
  threshold <- 256 * 1024^2
  if (bytes <= threshold) {
    return(.builder_recommend_record(
      value = "embedded",
      reason = "The estimated matrix size fits the embedded threshold.",
      confidence = 1,
      requires_confirmation = FALSE,
      available = normalized,
      blocking = character(),
      dependency_actions = character()
    ))
  }
  complete <- function(name) {
    isTRUE(normalized$build[[name]]) && isTRUE(normalized$viewer[[name]])
  }
  if (sparse && complete("bpcells")) {
    return(.builder_recommend_record(
      value = "bpcells",
      reason = "A large sparse matrix has complete BPCells support.",
      confidence = 1,
      requires_confirmation = FALSE,
      available = normalized,
      blocking = character(),
      dependency_actions = character()
    ))
  }
  if (complete("h5")) {
    return(.builder_recommend_record(
      value = "h5",
      reason = if (sparse) {
        "BPCells is incomplete, so complete H5 support is used."
      } else {
        "A large dense matrix requires complete H5 support."
      },
      confidence = 1,
      requires_confirmation = FALSE,
      available = normalized,
      blocking = character(),
      dependency_actions = character()
    ))
  }
  needed <- if (sparse) c("bpcells", "h5") else "h5"
  missing <- unlist(
    lapply(needed, function(name) {
      c(
        if (!isTRUE(normalized$build[[name]])) paste0("build.", name),
        if (!isTRUE(normalized$viewer[[name]])) paste0("viewer.", name)
      )
    }),
    use.names = FALSE
  )
  .builder_recommend_record(
    value = NULL,
    reason = "No non-embedded backend has complete build and Viewer support.",
    confidence = 0,
    requires_confirmation = TRUE,
    available = normalized,
    blocking = unique(missing),
    dependency_actions = paste("Enable", unique(missing), "support.")
  )
}

#' Assemble all recommendation records for one DatasetProfile v2.
builder_recommend_dataset <- function(
  profile,
  matrix_summary = NULL,
  available = NULL,
  required = character(),
  dependency_ids = list()
) {
  metadata <- builder_recommend_metadata(
    profile,
    required = required,
    dependency_ids = dependency_ids
  )
  organism <- builder_recommend_organism(profile)
  list(
    metadata = metadata,
    groups = builder_recommend_groups(profile, metadata),
    projections = builder_recommend_projections(profile),
    organism = organism,
    nomenclature = builder_recommend_nomenclature(profile, organism),
    backend = builder_recommend_backend(matrix_summary, available)
  )
}
