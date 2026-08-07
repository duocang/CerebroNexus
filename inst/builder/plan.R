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

builder_normalize_analyses <- function(selected, has_marker_genes = FALSE) {
  order <- c(
    "percent_mt_ribo",
    "most_expressed",
    "marker_genes",
    "enriched_pathways"
  )
  selected <- intersect(order, unique(as.character(selected %||% character())))
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

builder_resolve_colors <- function(settings, levels) {
  groups <- settings$groups %||% character()
  palette <- settings$palette %||% "cerebro"
  overrides <- settings$color_overrides %||% list()
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

builder_default_settings <- function(profile, name) {
  assay <- profile$default_assay
  assay_profile <- profile$assay_profiles[[assay]] %||%
    list(
      default_layer = profile$default_layer,
      nUMI = profile$nUMI,
      nGene = profile$nGene
    )
  list(
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
}

builder_plan_error <- function(message) {
  list(error = message)
}

builder_has_text <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(trimws(value))
}

builder_make_plan <- function(
  entries,
  out_dir,
  make_app = TRUE,
  overwrite = FALSE
) {
  out_dir <- trimws(out_dir %||% "")
  if (!nzchar(out_dir)) {
    return(builder_plan_error("Choose an output directory."))
  }
  if (!length(entries)) {
    return(builder_plan_error("Add at least one dataset."))
  }

  labels <- trimws(vapply(
    entries,
    function(entry) entry$settings$name %||% "",
    character(1)
  ))
  if (any(is.na(labels) | !nzchar(labels))) {
    return(builder_plan_error("Every dataset needs a non-empty name."))
  }
  if (anyDuplicated(labels)) {
    return(builder_plan_error("Dataset names must be unique."))
  }

  invalid <- vapply(
    entries,
    function(entry) {
      settings <- entry$settings
      !length(settings$groups %||% character()) ||
        !length(settings$reductions %||% character()) ||
        !builder_has_text(settings$assay %||% "") ||
        !builder_has_text(settings$layer %||% "")
    },
    logical(1)
  )
  if (any(invalid)) {
    return(builder_plan_error(
      "Every dataset needs an assay, layer, grouping variable and reduction."
    ))
  }
  invalid_qc <- vapply(
    entries,
    function(entry) {
      settings <- entry$settings
      !builder_has_text(settings$nUMI %||% entry$profile$nUMI) ||
        !builder_has_text(settings$nGene %||% entry$profile$nGene)
    },
    logical(1)
  )
  if (any(invalid_qc)) {
    return(builder_plan_error(
      "Every dataset needs explicit UMI/count and feature/gene fields."
    ))
  }

  items <- lapply(seq_along(entries), function(index) {
    entry <- entries[[index]]
    settings <- entry$settings
    filename <- builder_item_filename(entry, index, length(entries))
    list(
      id = entry$id,
      name = labels[index],
      filename = filename,
      organism = settings$organism,
      assay = settings$assay,
      layer = settings$layer,
      groups = settings$groups,
      reductions = settings$reductions,
      analyses = builder_normalize_analyses(
        settings$analyses,
        builder_profile_has(entry$profile, "marker_genes")
      ),
      tables = settings$tables %||% list(),
      images = settings$images %||% list(),
      colors = builder_resolve_colors(settings, entry$levels %||% list()),
      nUMI = settings$nUMI %||% entry$profile$nUMI,
      nGene = settings$nGene %||% entry$profile$nGene
    )
  })

  filenames <- vapply(items, `[[`, "", "filename")
  if (anyDuplicated(filenames)) {
    return(builder_plan_error("Generated dataset filenames must be unique."))
  }

  targets <- file.path(out_dir, filenames)
  if (isTRUE(make_app)) {
    targets <- c(targets, file.path(out_dir, "cerebro_app"))
  }

  list(
    error = NULL,
    out_dir = normalizePath(out_dir, mustWork = FALSE),
    make_app = isTRUE(make_app),
    overwrite = isTRUE(overwrite),
    items = items,
    targets = targets,
    existing_targets = targets[file.exists(targets) | dir.exists(targets)]
  )
}
