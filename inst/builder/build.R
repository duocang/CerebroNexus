##----------------------------------------------------------------------------##
## Frozen BuildPlan execution and staged-artifact verification.
##
## The coordinator creates the stage. This module never chooses a publication
## target and never moves an artifact into the final release directory.
##----------------------------------------------------------------------------##

.builder_build_text <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
}

.builder_build_stage <- function(stage) {
  if (!.builder_build_text(stage) || !dir.exists(stage)) {
    stop("Build execution requires an existing assigned stage.", call. = FALSE)
  }
  if (nzchar(Sys.readlink(stage))) {
    stop("The assigned stage cannot be a symbolic link.", call. = FALSE)
  }
  normalizePath(stage, winslash = "/", mustWork = TRUE)
}

.builder_build_path_within <- function(path, stage, must_exist = TRUE) {
  if (!.builder_build_text(path)) {
    return(FALSE)
  }
  canonical <- tryCatch(
    normalizePath(path, winslash = "/", mustWork = must_exist),
    error = function(error) NULL
  )
  if (is.null(canonical)) {
    return(FALSE)
  }
  identical(canonical, stage) || startsWith(canonical, paste0(stage, "/"))
}

.builder_build_safe_relative <- function(path) {
  if (!.builder_build_text(path) || grepl("\\", path, fixed = TRUE)) {
    return(FALSE)
  }
  if (
    startsWith(path, "/") ||
      startsWith(path, "//") ||
      grepl("^[A-Za-z]:", path)
  ) {
    return(FALSE)
  }
  components <- strsplit(path, "/", fixed = TRUE)[[1L]]
  length(components) > 0L &&
    all(nzchar(components)) &&
    !any(components %in% c(".", ".."))
}

.builder_build_failure <- function(message, failures = character()) {
  list(
    state = "failure",
    publishable = FALSE,
    error = message,
    failures = failures,
    built = character(),
    labels = character(),
    verifications = list(),
    analysis_log = character(),
    failed_analyses = character(),
    retry_closure = character(),
    app_dir = NULL,
    app_verification = NULL
  )
}

.builder_build_field <- function(object, name) {
  if (is.environment(object)) {
    return(get0(name, envir = object, inherits = FALSE))
  }
  if (is.list(object)) {
    return(object[[name]])
  }
  NULL
}

.builder_build_identity <- function(object, axis) {
  expression <- .builder_build_field(object, "expression")
  ids <- if (identical(axis, "cells")) {
    colnames(expression)
  } else {
    rownames(expression)
  }
  if (length(ids)) {
    return(as.character(ids))
  }
  fallback <- if (identical(axis, "cells")) {
    metadata <- .builder_build_field(object, "meta_data")
    if (
      is.data.frame(metadata) &&
        "cell_barcode" %in% colnames(metadata)
    ) {
      metadata[["cell_barcode"]]
    } else {
      rownames(metadata)
    }
  } else {
    rownames(.builder_build_field(object, "gene_data"))
  }
  as.character(fallback %||% character())
}

.builder_build_sidecar_path <- function(path, item, expected_type) {
  if (
    length(item$sidecars) != 1L ||
      !.builder_build_safe_relative(item$sidecars[[1L]]) ||
      !expected_type %in% c("file", "directory")
  ) {
    stop("The staged CRB sidecar does not match BuildPlan.", call. = FALSE)
  }
  root <- normalizePath(dirname(path), winslash = "/", mustWork = TRUE)
  sidecar <- file.path(dirname(path), item$sidecars[[1L]])
  info <- tryCatch(
    fs::file_info(sidecar, fail = TRUE, follow = FALSE),
    error = function(error) NULL
  )
  if (
    nzchar(Sys.readlink(sidecar)) ||
      is.null(info) ||
      !identical(as.character(info$type), expected_type) ||
      !.builder_build_path_within(sidecar, root, must_exist = TRUE)
  ) {
    stop("The staged CRB sidecar does not match BuildPlan.", call. = FALSE)
  }
  normalizePath(sidecar, winslash = "/", mustWork = TRUE)
}

.builder_build_h5_identities <- function(path, item) {
  if (!identical(item$expression_backend, "h5")) {
    return(NULL)
  }
  if (!requireNamespace("HDF5Array", quietly = TRUE)) {
    stop(
      "HDF5Array is required to verify the staged H5 sidecar.",
      call. = FALSE
    )
  }
  sidecar <- .builder_build_sidecar_path(path, item, "file")
  matrix <- tryCatch(
    DelayedArray::t(HDF5Array::TENxMatrix(sidecar, group = "expression")),
    error = function(error) NULL
  )
  if (is.null(matrix)) {
    stop("The staged H5 sidecar cannot be reopened.", call. = FALSE)
  }
  list(
    cells = as.character(colnames(matrix) %||% character()),
    features = as.character(rownames(matrix) %||% character())
  )
}

.builder_crb_visible_pages <- function(object) {
  present <- function(field) {
    value <- .builder_build_field(object, field)
    !is.null(value) && length(value) > 0L
  }
  extra <- .builder_build_field(object, "extra_material")
  immune <- .builder_build_field(object, "immune_repertoire")
  tcr_chains <- tryCatch(
    hla_detect_chains(immune),
    error = function(error) character()
  )
  pages <- c(
    if (present("marker_genes")) "marker_genes",
    if (present("most_expressed_genes")) "most_expressed_genes",
    if (present("enriched_pathways")) "enriched_pathways",
    if (!is.null(extra) && length(extra$tables) > 0L) "extra_material",
    if (present("immune_repertoire")) "immune_repertoire",
    if (present("trajectories")) "trajectory",
    if (present("spatial")) "spatial",
    if (present("trekker")) "trekker",
    if (any(tcr_chains %in% c("TRA", "TRB"))) {
      "hla_tcr_motifs"
    }
  )
  as.character(pages)
}

#' Reopen and compare one staged CRB with its frozen expectation.
builder_verify_crb <- function(path, item) {
  if (!file.exists(path) || dir.exists(path) || nzchar(Sys.readlink(path))) {
    stop("The staged CRB is missing or is not a regular file.", call. = FALSE)
  }
  object <- tryCatch(readRDS(path), error = function(error) error)
  if (inherits(object, "condition")) {
    stop(
      "The staged CRB cannot be reopened: ",
      conditionMessage(object),
      call. = FALSE
    )
  }
  expectation <- item$artifact_identity
  cells <- .builder_build_identity(object, "cells")
  features <- .builder_build_identity(object, "features")
  h5_identity <- .builder_build_h5_identities(path, item)
  if (!is.null(h5_identity)) {
    if (!identical(h5_identity$cells, expectation$cells)) {
      stop(
        "The staged H5 sidecar cell identity differs from BuildPlan.",
        call. = FALSE
      )
    }
    if (!identical(h5_identity$features, expectation$features)) {
      stop(
        "The staged H5 sidecar feature identity differs from BuildPlan.",
        call. = FALSE
      )
    }
  }
  if (!length(cells)) {
    cells <- h5_identity$cells %||% character()
  }
  if (!length(features)) {
    features <- h5_identity$features %||% character()
  }
  if (!identical(cells, expectation$cells)) {
    stop("The staged CRB cell identity differs from BuildPlan.", call. = FALSE)
  }
  if (!identical(features, expectation$features)) {
    stop(
      "The staged CRB feature identity differs from BuildPlan.",
      call. = FALSE
    )
  }
  groups_value <- .builder_build_field(object, "groups")
  groups <- names(groups_value)
  expected_groups <- names(expectation$group_levels)
  if (!identical(groups, expected_groups)) {
    stop(
      "The staged CRB grouping variables differ from BuildPlan.",
      call. = FALSE
    )
  }
  for (group in expected_groups) {
    if (
      !identical(
        as.character(groups_value[[group]]),
        expectation$group_levels[[group]]
      )
    ) {
      stop(
        "The staged CRB group levels differ from BuildPlan: ",
        group,
        call. = FALSE
      )
    }
  }
  projections <- names(.builder_build_field(object, "projections"))
  if (!identical(projections, expectation$projections)) {
    stop("The staged CRB projections differ from BuildPlan.", call. = FALSE)
  }
  metadata <- colnames(.builder_build_field(object, "meta_data"))
  if (!identical(metadata, expectation$metadata)) {
    stop("The staged CRB metadata differs from BuildPlan.", call. = FALSE)
  }
  spatial_sections <- names(.builder_build_field(object, "spatial"))
  spatial_sections <- spatial_sections %||% character()
  if (!identical(spatial_sections, expectation$spatial_sections)) {
    stop(
      "The staged CRB spatial sections differ from BuildPlan.",
      call. = FALSE
    )
  }
  expected_images <- item$images %||% list()
  spatial <- .builder_build_field(object, "spatial")
  for (section in names(expected_images)) {
    observed_image <- spatial[[section]]
    expected_image <- expected_images[[section]]
    if (
      !is.list(observed_image) ||
        !identical(observed_image$histology_image, expected_image$uri) ||
        !identical(
          observed_image$histology_image_bounds,
          expected_image$bounds
        )
    ) {
      stop(
        "The staged CRB histology image differs from BuildPlan: ",
        section,
        call. = FALSE
      )
    }
  }
  backend <- .builder_build_field(object, "expression_backend")
  backend_type <- if (is.null(backend)) "embedded" else backend$type
  if (!identical(backend_type, item$expression_backend)) {
    stop(
      "The staged CRB expression backend differs from BuildPlan.",
      call. = FALSE
    )
  }
  if (length(item$sidecars)) {
    expected_location <- item$sidecars[[1L]]
    if (!identical(backend$location, expected_location)) {
      stop(
        "The staged CRB sidecar location differs from BuildPlan.",
        call. = FALSE
      )
    }
    expected_directory <- identical(item$expression_backend, "bpcells")
    .builder_build_sidecar_path(
      path,
      item,
      if (expected_directory) "directory" else "file"
    )
  }
  visible <- .builder_crb_visible_pages(object)
  expected_visible <- item$viewer_page_expectations$visible_conditional %||%
    character()
  if (!setequal(visible, expected_visible)) {
    stop(
      "The staged CRB Viewer page contract differs from BuildPlan.",
      call. = FALSE
    )
  }
  list(
    valid = TRUE,
    path = path,
    cells = cells,
    features = features,
    groups = groups,
    projections = projections,
    metadata = metadata,
    spatial_sections = spatial_sections,
    image_sections = names(expected_images),
    backend = backend,
    page_contract = list(visible_conditional = visible)
  )
}

.builder_build_prepare_immune <- function(object, item) {
  if (!methods::is(object, "Seurat")) {
    return(object)
  }
  record <- item$manifest[["immune_repertoire"]]
  if (is.null(record)) {
    return(object)
  }
  clear_sources <- function(value) {
    value@misc$immune_repertoire <- NULL
    value@misc$bcr_data <- NULL
    value@misc$tcr_data <- NULL
    value
  }
  if (record$disposition %in% c("filtered", "stored_only")) {
    return(clear_sources(object))
  }
  selected <- record$evidence$selected_sources %||% character()
  if (!length(selected)) {
    return(object)
  }
  if (identical(selected, "unified_misc")) {
    object@misc$bcr_data <- NULL
    object@misc$tcr_data <- NULL
    return(object)
  }
  if (identical(selected, "metadata")) {
    candidate <- record$evidence$selected_candidates[["metadata"]]
    sample_column <- candidate$normalized$sample_column %||% NULL
    object <- clear_sources(object)
    return(CerebroNexus::addImmuneRepertoire(
      object,
      sample_col = sample_column,
      groups = item$included_groups,
      from_metadata = TRUE,
      verbose = FALSE
    ))
  }
  supported_legacy <- c("legacy_bcr", "legacy_tcr")
  if (length(setdiff(selected, supported_legacy))) {
    stop(
      "The frozen immune source is not supported at build time.",
      call. = FALSE
    )
  }
  bcr <- if ("legacy_bcr" %in% selected) object@misc$bcr_data else NULL
  tcr <- if ("legacy_tcr" %in% selected) object@misc$tcr_data else NULL
  object <- clear_sources(object)
  CerebroNexus::addImmuneRepertoire(
    object,
    tcr = tcr,
    bcr = bcr,
    from_metadata = FALSE,
    verbose = FALSE
  )
}

.builder_build_prepare <- function(object, item) {
  if (methods::is(object, "Seurat")) {
    object@reductions <- object@reductions[item$included_projections]
    object <- builder_prepare_export_layer(object, item$assay, item$layer)
    object <- .builder_build_prepare_immune(object, item)
    for (group in names(item$artifact_identity$group_levels)) {
      values <- object@meta.data[[group]]
      object@meta.data[[group]] <- factor(
        as.character(values),
        levels = item$artifact_identity$group_levels[[group]]
      )
    }
    object <- builder_attach_tables(object, item$tables %||% list())
  }
  object
}

.builder_build_apply_metadata_policy <- function(object, item) {
  if (!methods::is(object, "Seurat")) {
    return(object)
  }
  metadata <- setdiff(
    item$metadata_policy$included %||% character(),
    "cell_barcode"
  )
  if ("percent_mt_ribo" %in% item$analyses) {
    metadata <- unique(c(metadata, "percent_mt", "percent_ribo"))
  }
  missing_metadata <- setdiff(metadata, colnames(object@meta.data))
  if (length(missing_metadata)) {
    stop(
      "Frozen metadata columns are missing from the built object: ",
      paste(missing_metadata, collapse = ", "),
      call. = FALSE
    )
  }
  object@meta.data <- object@meta.data[, metadata, drop = FALSE]
  object
}

.builder_build_export <- function(object, item, path) {
  object <- .builder_build_apply_metadata_policy(object, item)
  CerebroNexus::exportFromSeurat(
    object = object,
    assay = item$assay,
    slot = item$layer,
    file = path,
    experiment_name = item$name,
    organism = item$organism,
    groups = item$included_groups,
    main_group = item$default_group,
    nUMI = item$nUMI,
    nGene = item$nGene,
    add_all_meta_data = TRUE,
    expression_matrix_mode = item$expression_backend,
    verbose = FALSE
  )
  path
}

.builder_build_attach_extras <- function(path, object, item) {
  trekker <- if (methods::is(object, "Seurat")) {
    tryCatch(object@misc$trekker, error = function(error) NULL)
  } else {
    NULL
  }
  result <- builder_attach_crb_extras(path, item$images %||% list(), trekker)
  if (!is.null(result$error)) {
    stop(result$error, call. = FALSE)
  }
  result
}

builder_build_hooks <- function() {
  list(
    open_snapshot = builder_open_snapshot,
    prepare = .builder_build_prepare,
    run_analyses = function(object, item) {
      settings <- item
      settings$groups <- item$included_groups
      builder_run_analyses(object, item$analyses, settings)
    },
    export = .builder_build_export,
    attach_extras = .builder_build_attach_extras,
    verify = builder_verify_crb,
    build_app = builder_build_app,
    verify_app = builder_verify_app
  )
}

#' Execute one frozen BuildPlan without publishing its outputs.
builder_execute_plan <- function(
  plan,
  stage,
  snapshots,
  hooks = builder_build_hooks()
) {
  if (!inherits(plan, "builder_build_plan") || !is.list(plan$items)) {
    stop("Build execution requires a frozen BuildPlan.", call. = FALSE)
  }
  stage <- .builder_build_stage(stage)
  if (isTRUE(plan$make_app) && !identical(plan$app_contract_version, 1L)) {
    return(.builder_build_failure(
      "Generated-app execution requires frozen contract version 1."
    ))
  }
  if (!is.list(snapshots)) {
    stop("Build execution requires a snapshot registry.", call. = FALSE)
  }
  required_hooks <- c(
    "open_snapshot",
    "prepare",
    "run_analyses",
    "export",
    "attach_extras",
    "verify"
  )
  if (!all(vapply(hooks[required_hooks], is.function, logical(1)))) {
    stop("Build execution hooks are incomplete.", call. = FALSE)
  }

  result <- list(
    state = "success",
    publishable = FALSE,
    error = NULL,
    failures = character(),
    built = character(),
    labels = character(),
    verifications = list(),
    analysis_log = character(),
    failed_analyses = character(),
    retry_closure = character(),
    app_dir = NULL,
    app_verification = NULL,
    stage = stage
  )
  for (item in plan$items) {
    snapshot <- snapshots[[item$id]]
    if (is.null(snapshot)) {
      return(.builder_build_failure(paste0(
        "The frozen snapshot is missing for ",
        item$name,
        "."
      )))
    }
    object <- tryCatch(hooks$open_snapshot(snapshot), error = function(error) {
      error
    })
    if (inherits(object, "condition")) {
      return(.builder_build_failure(paste0(
        item$name,
        ": ",
        conditionMessage(object)
      )))
    }
    object <- tryCatch(hooks$prepare(object, item), error = function(error) {
      error
    })
    if (inherits(object, "condition")) {
      return(.builder_build_failure(paste0(
        item$name,
        ": ",
        conditionMessage(object)
      )))
    }
    analyses <- tryCatch(
      hooks$run_analyses(object, item),
      error = function(error) error
    )
    if (inherits(analyses, "condition")) {
      return(.builder_build_failure(paste0(
        item$name,
        ": ",
        conditionMessage(analyses)
      )))
    }
    if (length(analyses$log)) {
      result$analysis_log <- c(
        result$analysis_log,
        paste0(item$name, ": ", analyses$log)
      )
    }
    if (length(analyses$failed)) {
      failed <- analyses$failed[[1L]]
      graph <- item$analysis_dependency_graph
      if (is.null(graph) || !failed %in% names(graph)) {
        graph <- builder_analysis_graph(item$analyses)
      }
      result$state <- "needs_decision"
      result$failed_analyses <- failed
      result$failed_dataset_id <- item$id
      result$retry_closure <- builder_retry_closure(graph, failed)
      result$failures <- paste0(item$name, ": analysis `", failed, "` failed.")
      return(result)
    }
    object <- analyses$object
    if (!.builder_build_safe_relative(item$filename)) {
      return(.builder_build_failure(
        "A build artifact target is outside the assigned stage."
      ))
    }
    target <- file.path(stage, item$filename)
    if (!.builder_build_path_within(target, stage, must_exist = FALSE)) {
      return(.builder_build_failure(
        "A build artifact target is outside the assigned stage."
      ))
    }
    exported <- tryCatch(
      hooks$export(object, item, target),
      error = function(error) error
    )
    if (inherits(exported, "condition")) {
      return(.builder_build_failure(paste0(
        item$name,
        ": ",
        conditionMessage(exported)
      )))
    }
    if (!.builder_build_path_within(exported, stage, must_exist = TRUE)) {
      return(.builder_build_failure(
        "A build artifact was written outside the assigned stage."
      ))
    }
    extras <- tryCatch(
      hooks$attach_extras(exported, object, item),
      error = function(error) error
    )
    if (inherits(extras, "condition")) {
      return(.builder_build_failure(paste0(
        item$name,
        ": ",
        conditionMessage(extras)
      )))
    }
    verified <- tryCatch(hooks$verify(exported, item), error = function(error) {
      error
    })
    if (inherits(verified, "condition") || !isTRUE(verified$valid)) {
      message <- if (inherits(verified, "condition")) {
        conditionMessage(verified)
      } else {
        "Artifact verification did not return a valid result."
      }
      return(.builder_build_failure(paste0(item$name, ": ", message)))
    }
    result$built <- c(result$built, stats::setNames(exported, item$name))
    result$labels <- c(result$labels, item$name)
    result$verifications[[item$id]] <- verified
  }
  if (isTRUE(plan$make_app)) {
    app_hooks <- c("build_app", "verify_app")
    if (!all(vapply(hooks[app_hooks], is.function, logical(1)))) {
      return(.builder_build_failure(
        "Generated-App build hooks are incomplete."
      ))
    }
    request <- tryCatch(
      builder_app_bundle_request(plan, result$built, result$labels),
      error = function(error) error
    )
    if (inherits(request, "condition")) {
      return(.builder_build_failure(conditionMessage(request)))
    }
    app_dir <- tryCatch(
      hooks$build_app(request, stage),
      error = function(error) error
    )
    if (inherits(app_dir, "condition")) {
      return(.builder_build_failure(conditionMessage(app_dir)))
    }
    app_verification <- tryCatch(
      hooks$verify_app(app_dir, request),
      error = function(error) error
    )
    valid_app_verification <-
      !inherits(app_verification, "condition") &&
      identical(typeof(app_verification), "list") &&
      identical(
        attr(app_verification, "class", exact = TRUE),
        c("builder_app_verification", "list")
      ) &&
      !.builder_app_has_reference(app_verification)
    plain_app_verification <- if (valid_app_verification) {
      .builder_app_plain_value(app_verification)
    } else {
      NULL
    }
    if (
      !valid_app_verification ||
        !isTRUE(plain_app_verification[["valid"]])
    ) {
      message <- if (inherits(app_verification, "condition")) {
        conditionMessage(app_verification)
      } else {
        "App verification did not return valid inert evidence."
      }
      return(.builder_build_failure(message))
    }
    result$app_dir <- app_dir
    result$app_verification <- app_verification
  }
  result$publishable <- length(result$built) == length(plan$items)
  result
}
