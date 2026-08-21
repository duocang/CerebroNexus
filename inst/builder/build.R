##----------------------------------------------------------------------------##
## Frozen BuildPlan execution and staged-artifact verification.
##
## The coordinator creates the stage. This module never chooses a publication
## target and never moves an artifact into the final release directory.
##----------------------------------------------------------------------------##

.builder_build_text <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
}

.builder_build_number_equal <- function(left, right, tolerance = 1e-12) {
  is.numeric(left) &&
    is.numeric(right) &&
    !is.object(left) &&
    !is.object(right) &&
    length(left) == 1L &&
    length(right) == 1L &&
    is.finite(left) &&
    is.finite(right) &&
    abs(as.numeric(left) - as.numeric(right)) <= tolerance
}

.builder_build_value_equal <- function(left, right, tolerance = 1e-12) {
  isTRUE(all.equal(
    left,
    right,
    tolerance = tolerance,
    check.attributes = TRUE
  ))
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

.builder_build_fingerprint_matches <- function(path, fingerprint) {
  if (
    !.builder_build_text(path) ||
      !file.exists(path) ||
      dir.exists(path) ||
      !is.list(fingerprint) ||
      !.builder_build_text(fingerprint$md5 %||% NULL)
  ) {
    return(FALSE)
  }
  observed <- tryCatch(
    unname(tools::md5sum(path)),
    error = function(error) NA_character_
  )
  length(observed) == 1L &&
    !is.na(observed) &&
    identical(as.character(observed), as.character(fingerprint$md5))
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
    spatial_images = list(),
    spatial_image_settings = list(),
    app_dir = NULL,
    app_verification = NULL
  )
}

.builder_build_cleanup_spatial_assets <- function(stage) {
  path <- file.path(stage, ".builder-spatial-assets")
  exists <- function(value) {
    file.exists(value) || dir.exists(value) || .builder_app_is_link(value)
  }
  if (!exists(path)) {
    return(invisible(TRUE))
  }
  if (
    .builder_app_is_link(path) ||
      !.builder_build_path_within(path, stage, must_exist = TRUE)
  ) {
    stop("Builder spatial asset staging is unsafe to clean.", call. = FALSE)
  }
  unlink(path, recursive = TRUE, force = TRUE)
  if (exists(path)) {
    stop("Builder spatial asset staging could not be cleaned.", call. = FALSE)
  }
  invisible(TRUE)
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
  if (!is.null(expectation$trajectories)) {
    trajectories <- .builder_build_field(object, "trajectories")
    trajectory_identity <- lapply(trajectories %||% list(), names)
    if (!identical(trajectory_identity, expectation$trajectories)) {
      stop(
        "The staged CRB trajectories differ from BuildPlan.",
        call. = FALSE
      )
    }
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
  expected_images <- if (identical(item$spatial_image_storage, "external")) {
    list()
  } else {
    item$images %||% list()
  }
  spatial <- .builder_build_field(object, "spatial")
  expected_coordinate_transforms <- item$spatial_coordinate_transforms %||%
    list()
  for (section in names(expected_coordinate_transforms)) {
    observed_spatial <- spatial[[section]]
    coordinates <- if (is.list(observed_spatial)) {
      observed_spatial$coordinates
    } else {
      NULL
    }
    valid_coordinates <- is.data.frame(coordinates) &&
      all(c("x", "y") %in% names(coordinates)) &&
      is.numeric(coordinates$x) &&
      is.numeric(coordinates$y) &&
      !is.object(coordinates$x) &&
      !is.object(coordinates$y) &&
      all(is.finite(coordinates$x)) &&
      all(is.finite(coordinates$y))
    if (!isTRUE(valid_coordinates)) {
      stop(
        "The staged CRB spatial coordinates are invalid for transformed FOV: ",
        section,
        call. = FALSE
      )
    }
    observed_transform <- observed_spatial$coordinate_transform
    expected_transform <- expected_coordinate_transforms[[section]]
    valid_pivot <- is.numeric(observed_transform$pivot) &&
      !is.object(observed_transform$pivot) &&
      identical(names(observed_transform$pivot), c("x", "y")) &&
      length(observed_transform$pivot) == 2L &&
      all(is.finite(observed_transform$pivot))
    observed_fingerprint <- tryCatch(
      .spx_coordinate_transform_fingerprint(coordinates),
      error = function(error) NULL
    )
    # A floating-point rotation is not losslessly invertible, so hashing the
    # inverse can reject an otherwise exact staged export. The source hash is
    # provenance; staged integrity is bound to the actual transformed values
    # below, while the frozen plan still binds the rotation and scale.
    valid_source_fingerprint <-
      is.character(observed_transform$source_coordinate_fingerprint) &&
      !is.object(observed_transform$source_coordinate_fingerprint) &&
      length(observed_transform$source_coordinate_fingerprint) == 1L &&
      !is.na(observed_transform$source_coordinate_fingerprint) &&
      grepl(
        "^[0-9a-f]{32}$",
        observed_transform$source_coordinate_fingerprint
      )
    if (
      !is.list(observed_transform) ||
        !identical(observed_transform$schema_version, 1L) ||
        !.builder_build_number_equal(
          observed_transform$rotation_degrees %||% NA_real_,
          expected_transform$rotation_degrees
        ) ||
        !.builder_build_number_equal(
          observed_transform$scale %||% NA_real_,
          expected_transform$scale
        ) ||
        !isTRUE(valid_pivot) ||
        !identical(observed_transform$pivot_method, "bounds_center") ||
        !identical(
          observed_transform$convention,
          "counterclockwise_degrees"
        ) ||
        !identical(
          observed_transform$transformed_coordinate_fingerprint,
          observed_fingerprint
        ) ||
        !isTRUE(valid_source_fingerprint)
    ) {
      stop(
        "The staged CRB spatial coordinate transform differs from BuildPlan: ",
        section,
        call. = FALSE
      )
    }
  }
  for (section in names(expected_images)) {
    observed_image <- spatial[[section]]
    expected_image <- expected_images[[section]]
    expected_records <- if (
      !is.null(builder_alignment_normalize(
        expected_image,
        section_id = section
      ))
    ) {
      list(expected_image)
    } else {
      expected_image
    }
    expected_record_labels <- names(expected_records)
    expected_payloads <- lapply(seq_along(expected_records), function(index) {
      payload <- builder_histology_image_payload(expected_records[[index]])
      if (
        !is.null(expected_record_labels) &&
          nzchar(expected_record_labels[[index]])
      ) {
        payload$histology_alignment$source <- expected_record_labels[[index]]
      }
      payload
    })
    observed_images <- if (is.list(observed_image)) {
      observed_image$histology_images %||% list()
    } else {
      list()
    }
    matching_image <- all(vapply(
      expected_payloads,
      function(expected_payload) {
        any(vapply(
          observed_images,
          .builder_build_value_equal,
          logical(1),
          right = expected_payload
        ))
      },
      logical(1)
    ))
    if (
      !is.list(observed_image) ||
        !matching_image
    ) {
      stop(
        "The staged CRB histology image differs from BuildPlan: ",
        section,
        call. = FALSE
      )
    }
    expected_alignment <- utils::tail(expected_records, 1L)[[1L]]
    expected_alignment_payload <- builder_alignment_payload(expected_alignment)
    if (!is.null(expected_record_labels) && length(expected_record_labels)) {
      expected_alignment_payload$source <- utils::tail(
        expected_record_labels,
        1L
      )[[1L]]
    }
    has_canonical_alignment <- any(
      c("dx", "rotation", "image_opacity", "point_opacity") %in%
        names(expected_alignment)
    )
    if (
      has_canonical_alignment &&
        !.builder_build_value_equal(
          observed_image$histology_alignment,
          expected_alignment_payload
        )
    ) {
      stop(
        "The staged CRB spatial alignment differs from BuildPlan: ",
        section,
        call. = FALSE
      )
    }
  }
  expected_trekker_alignment <- item$trekker_alignment %||% NULL
  if (!is.null(expected_trekker_alignment)) {
    observed_trekker <- .builder_build_field(object, "trekker")
    if (
      !is.list(observed_trekker) ||
        !identical(
          observed_trekker$histology_image,
          expected_trekker_alignment$uri
        ) ||
        !.builder_build_value_equal(
          observed_trekker$histology_image_bounds,
          expected_trekker_alignment$bounds
        ) ||
        !.builder_build_value_equal(
          observed_trekker$histology_alignment,
          builder_alignment_payload(expected_trekker_alignment)
        )
    ) {
      stop(
        "The staged CRB Trekker alignment differs from BuildPlan.",
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
  immune_record <- item$manifest[["immune_repertoire"]]
  motif_record <- item$manifest[["hla_tcr_motifs"]]
  if (is.null(immune_record) && is.null(motif_record)) {
    return(object)
  }
  clear_sources <- function(value) {
    value@misc$immune_repertoire <- NULL
    value@misc$bcr_data <- NULL
    value@misc$tcr_data <- NULL
    value
  }
  included_selection <- function(record) {
    if (
      !is.list(record) ||
        !(record$disposition %||% "") %in%
          c("preserved", "converted", "attached")
    ) {
      return(character())
    }
    record$evidence$selected_sources %||% character()
  }
  hidden <- function(record) {
    is.list(record) &&
      (record$disposition %||% "") %in% c("filtered", "stored_only")
  }
  selected_candidates <- function(record, selected) {
    candidates <- record$evidence$selected_candidates %||% list()
    if (!is.list(candidates)) {
      return(list())
    }
    candidates[intersect(selected, names(candidates))]
  }
  candidate_flag <- function(record, selected, flag) {
    candidates <- selected_candidates(record, selected)
    if (!length(selected) || length(candidates) != length(selected)) {
      return(NA)
    }
    flags <- vapply(
      candidates,
      function(candidate) isTRUE(candidate[[flag]]),
      logical(1)
    )
    any(flags)
  }
  full_selected <- included_selection(immune_record)
  motif_selected <- included_selection(motif_record)
  if (
    length(full_selected) &&
      length(motif_selected) &&
      !all(motif_selected %in% full_selected)
  ) {
    stop(
      "The frozen immune sources cannot be realized by one frozen immune payload.",
      call. = FALSE
    )
  }
  motif_exportable <- candidate_flag(
    motif_record,
    motif_selected,
    "full_ir_ready"
  )
  if (length(motif_selected) && !isTRUE(motif_exportable)) {
    stop(
      "The frozen motif source cannot be exported as one immune payload.",
      call. = FALSE
    )
  }
  if (length(motif_selected) && !length(full_selected)) {
    stop(
      "The frozen immune payload cannot hide only one Viewer page.",
      call. = FALSE
    )
  }
  if (
    length(motif_selected) &&
      hidden(immune_record)
  ) {
    stop(
      "The frozen immune payload cannot hide only one Viewer page.",
      call. = FALSE
    )
  }
  if (length(full_selected) && hidden(motif_record)) {
    full_has_motif <- candidate_flag(
      immune_record,
      full_selected,
      "hla_tcr_ready"
    )
    if (is.na(full_has_motif) || isTRUE(full_has_motif)) {
      stop(
        "The frozen immune payload cannot hide only one Viewer page.",
        call. = FALSE
      )
    }
  }
  selected <- if (length(full_selected)) full_selected else motif_selected
  selected_record <- if (length(full_selected)) immune_record else motif_record
  filtered <- vapply(
    Filter(Negate(is.null), list(immune_record, motif_record)),
    function(record) {
      (record$disposition %||% "") %in% c("filtered", "stored_only")
    },
    logical(1)
  )
  if (!length(selected) && length(filtered) && any(filtered)) {
    return(clear_sources(object))
  }
  if (!length(selected)) {
    return(object)
  }
  if (identical(selected, "unified_misc")) {
    object@misc$bcr_data <- NULL
    object@misc$tcr_data <- NULL
    return(object)
  }
  if (identical(selected, "metadata")) {
    candidate <- selected_record$evidence$selected_candidates[["metadata"]]
    sample_column <- candidate$normalized$sample_column %||% NULL
    object <- clear_sources(object)
    return(addImmuneRepertoire(
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
  addImmuneRepertoire(
    object,
    tcr = tcr,
    bcr = bcr,
    from_metadata = FALSE,
    verbose = FALSE
  )
}

.builder_build_select_trajectories <- function(
  trajectories,
  included,
  default = NULL
) {
  # A missing field identifies a legacy BuildPlan. Preserve its historical
  # payload; an explicit empty list means the user chose no trajectories.
  if (is.null(included)) {
    return(trajectories)
  }
  if (!is.list(trajectories) || !is.list(included)) {
    stop("The frozen trajectory selection is invalid.", call. = FALSE)
  }
  missing_methods <- setdiff(names(included), names(trajectories))
  missing_names <- unlist(
    lapply(names(included), function(method) {
      setdiff(included[[method]], names(trajectories[[method]]))
    }),
    use.names = FALSE
  )
  if (length(missing_methods) || length(missing_names)) {
    stop(
      "A frozen included trajectory is missing from the built object.",
      call. = FALSE
    )
  }
  if (
    is.list(default) &&
      .builder_build_text(default$method) &&
      .builder_build_text(default$name) &&
      default$method %in% names(included) &&
      default$name %in% included[[default$method]]
  ) {
    method <- default$method
    included[[method]] <- c(
      default$name,
      included[[method]][included[[method]] != default$name]
    )
    included <- c(included[method], included[names(included) != method])
  }
  selected <- lapply(names(included), function(method) {
    trajectories[[method]][included[[method]]]
  })
  names(selected) <- names(included)
  selected
}

.builder_build_prepare <- function(object, item) {
  if (methods::is(object, "Seurat")) {
    object@reductions <- object@reductions[item$included_projections]
    object@misc$trajectories <- .builder_build_select_trajectories(
      object@misc$trajectories %||% list(),
      item$included_trajectories,
      item$default_trajectory
    )
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
    if (length(item$marker_imports %||% list())) {
      object <- builder_attach_marker_imports(object, item$marker_imports)
    }
  }
  object
}

.builder_build_apply_metadata_policy <- function(object, item) {
  if (!methods::is(object, "Seurat")) {
    return(object)
  }
  object
}

.builder_build_export <- function(object, item, path) {
  object <- .builder_build_apply_metadata_policy(object, item)
  coordinate_transforms <- item$spatial_coordinate_transforms %||% NULL
  if (
    is.list(coordinate_transforms) &&
      !is.object(coordinate_transforms) &&
      !length(coordinate_transforms)
  ) {
    coordinate_transforms <- NULL
  }
  exportFromSeurat(
    object = object,
    assay = item$assay,
    slot = item$layer,
    file = path,
    experiment_name = item$name,
    organism = item$organism,
    groups = item$included_groups,
    main_group = item$default_group,
    cell_cycle = item$cell_cycle %||% NULL,
    nUMI = item$nUMI,
    nGene = item$nGene,
    add_all_meta_data = TRUE,
    projections = item$included_projections,
    expression_matrix_mode = item$expression_backend,
    spatial_coordinate_transforms = coordinate_transforms,
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
  if (!is.null(trekker) && length(trekker)) {
    trekker$builder_group <- item$default_group %||% NULL
    trekker$builder_colors <- item$colors[[item$default_group]] %||% NULL
    group <- item$default_group %||% NULL
    barcodes <- as.character(trekker$barcodes %||% character())
    if (
      !is.null(group) &&
        length(barcodes) &&
        group %in% colnames(object@meta.data) &&
        all(barcodes %in% rownames(object@meta.data))
    ) {
      trekker$builder_group_values <- as.character(
        object@meta.data[barcodes, group, drop = TRUE]
      )
    }
  }
  embedded_images <- if (identical(item$spatial_image_storage, "external")) {
    list()
  } else {
    item$images %||% list()
  }
  result <- builder_attach_crb_extras(
    path,
    embedded_images,
    trekker,
    item$trekker_alignment %||% NULL
  )
  if (!is.null(result$error)) {
    stop(result$error, call. = FALSE)
  }
  if (identical(item$spatial_image_storage, "external")) {
    external <- .builder_build_materialize_spatial_images(item, dirname(path))
    appearance <- builder_attach_external_spatial_appearance(
      path,
      item$images %||% list()
    )
    if (!is.null(appearance$error)) {
      stop(appearance$error, call. = FALSE)
    }
    result$external_images <- external$images
    result$external_settings <- external$settings
  } else {
    result$external_images <- list()
    result$external_settings <- list()
  }
  result
}

.builder_build_materialize_spatial_images <- function(item, stage) {
  safe_component <- function(value, fallback) {
    value <- tolower(iconv(
      as.character(value),
      to = "ASCII//TRANSLIT",
      sub = ""
    ))
    value <- gsub("[^a-z0-9]+", "-", value)
    value <- gsub("(^-+|-+$)", "", value)
    if (!nzchar(value)) fallback else substr(value, 1L, 48L)
  }
  images <- list()
  settings <- list()
  collection <- builder_image_collection_normalize(item$images %||% list())
  for (section_id in names(collection)) {
    section_dir <- file.path(
      stage,
      ".builder-spatial-assets",
      safe_component(item$id, "dataset"),
      safe_component(section_id, "section")
    )
    dir.create(
      section_dir,
      recursive = TRUE,
      mode = "0700",
      showWarnings = FALSE
    )
    for (label in names(collection[[section_id]])) {
      record <- collection[[section_id]][[label]]
      parsed <- builder_parse_image_uri(record$source_uri)
      extension <- switch(
        parsed$mime,
        `image/png` = "png",
        `image/jpeg` = "jpg",
        stop("Builder image URI has an unsupported MIME type.", call. = FALSE)
      )
      filename <- builder_safe_file_name(record$source$name, label)
      if (!nzchar(tools::file_ext(filename))) {
        filename <- paste0(filename, ".", extension)
      }
      existing_paths <- unlist(
        lapply(images[[item$name]][[section_id]] %||% list(), `[[`, "path"),
        use.names = FALSE
      )
      existing_names <- if (length(existing_paths)) {
        basename(existing_paths)
      } else {
        character()
      }
      filename <- utils::tail(
        make.unique(c(existing_names, filename)),
        1L
      )
      materialized <- builder_materialize_image_uri(
        record$source_uri,
        file.path(section_dir, filename)
      )
      images[[item$name]][[section_id]][[label]] <- list(
        path = materialized,
        bounds = unlist(record$base_bounds[c("xmin", "xmax", "ymin", "ymax")])
      )
      settings[[item$name]][[section_id]][[label]] <- list(
        flip_x = record$flip_x,
        flip_y = record$flip_y,
        scale_x = record$scale,
        scale_y = record$scale,
        offset_x = record$dx,
        offset_y = record$dy,
        rotation = record$rotation,
        image_opacity = record$image_opacity,
        point_opacity = record$point_opacity,
        point_size = record$point_size
      )
    }
  }
  list(images = images, settings = settings)
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
  hooks = builder_build_hooks(),
  auth_material = NULL,
  objects = list()
) {
  on.exit(auth_material <- NULL, add = TRUE)
  if (!inherits(plan, "builder_build_plan") || !is.list(plan$items)) {
    stop("Build execution requires a frozen BuildPlan.", call. = FALSE)
  }
  stage <- .builder_build_stage(stage)
  auth_enabled <- isTRUE(plan$app_auth$enabled)
  cleanup_complete <- !auth_enabled
  on.exit(
    {
      if (!cleanup_complete && auth_enabled) {
        cleaned <- try(
          .builder_auth_remove_partial_material(stage),
          silent = TRUE
        )
        if (inherits(cleaned, "try-error") || !isTRUE(cleaned)) {
          stop(
            "The authentication files could not be cleaned up.",
            call. = FALSE
          )
        }
      }
    },
    add = TRUE
  )
  if (auth_enabled) {
    auth_material <- tryCatch(
      builder_auth_validate_material(auth_material, stage),
      error = function(error) error
    )
    if (inherits(auth_material, "condition")) {
      return(.builder_build_failure(conditionMessage(auth_material)))
    }
  } else if (!is.null(auth_material)) {
    return(.builder_build_failure(
      "A public build cannot use authentication material."
    ))
  }
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
    reused <- item$reused_artifact %||% NULL
    if (is.list(reused)) {
      if (
        !.builder_build_safe_relative(item$filename) ||
          !is.character(reused$path) ||
          length(reused$path) != 1L ||
          is.na(reused$path) ||
          !file.exists(reused$path) ||
          dir.exists(reused$path) ||
          !.builder_build_fingerprint_matches(
            reused$path,
            reused$fingerprint %||% list()
          )
      ) {
        return(.builder_build_failure(paste0(
          item$name,
          ": the reusable CRB is unavailable or has changed."
        )))
      }
      target <- file.path(stage, item$filename)
      if (
        !.builder_build_path_within(target, stage, must_exist = FALSE) ||
          !file.copy(reused$path, target, overwrite = FALSE, copy.mode = TRUE)
      ) {
        return(.builder_build_failure(paste0(
          item$name,
          ": the reusable CRB could not be staged."
        )))
      }
      members <- reused$members %||% list()
      for (member in members) {
        member_source <- member$resolved_path %||% NULL
        member_target <- member$target %||% NULL
        if (
          !is.character(member_source) ||
            length(member_source) != 1L ||
            is.na(member_source) ||
            !file.exists(member_source) ||
            !.builder_build_fingerprint_matches(
              member_source,
              member$fingerprint %||% list()
            ) ||
            !.builder_build_safe_relative(member_target %||% "")
        ) {
          return(.builder_build_failure(paste0(
            item$name,
            ": a reusable CRB companion file is unavailable."
          )))
        }
        destination <- file.path(stage, member_target)
        dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
        if (
          !.builder_build_path_within(destination, stage, must_exist = FALSE) ||
            !file.copy(
              member_source,
              destination,
              overwrite = FALSE,
              copy.mode = TRUE
            )
        ) {
          return(.builder_build_failure(paste0(
            item$name,
            ": a reusable CRB companion file could not be staged."
          )))
        }
      }
      verified <- tryCatch(hooks$verify(target, item), error = function(error) {
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
      result$built <- c(result$built, stats::setNames(target, item$name))
      result$labels <- c(result$labels, item$name)
      result$verifications[[item$id]] <- verified
      next
    }
    snapshot <- snapshots[[item$id]]
    object <- objects[[item$id]] %||% NULL
    if (is.null(snapshot) && is.null(object)) {
      return(.builder_build_failure(paste0(
        "The frozen snapshot is missing for ",
        item$name,
        "."
      )))
    }
    object <- if (!is.null(object)) {
      object
    } else {
      tryCatch(hooks$open_snapshot(snapshot), error = function(error) {
        error
      })
    }
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
    if (length(extras$external_images %||% list())) {
      result$spatial_images[[item$name]] <- extras$external_images[[item$name]]
      result$spatial_image_settings[[item$name]] <-
        extras$external_settings[[item$name]]
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
    app_plan <- plan
    for (index in seq_along(app_plan$items)) {
      dataset <- app_plan$items[[index]]$name
      app_plan$items[[index]]$external_images <-
        result$spatial_images[[dataset]] %||% list()
      app_plan$items[[index]]$external_image_settings <-
        result$spatial_image_settings[[dataset]] %||% list()
    }
    request <- tryCatch(
      builder_app_bundle_request(app_plan, result$built, result$labels),
      error = function(error) error
    )
    if (inherits(request, "condition")) {
      return(.builder_build_failure(conditionMessage(request)))
    }
    app_dir <- tryCatch(
      hooks$build_app(request, stage, auth_material = auth_material),
      error = function(error) error
    )
    if (inherits(app_dir, "condition")) {
      return(.builder_build_failure(conditionMessage(app_dir)))
    }
    app_verification <- tryCatch(
      hooks$verify_app(
        app_dir,
        request,
        auth_env_file = if (auth_enabled) auth_material$env_file else NULL
      ),
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
    cleaned_assets <- tryCatch(
      .builder_build_cleanup_spatial_assets(stage),
      error = function(error) error
    )
    if (inherits(cleaned_assets, "condition")) {
      return(.builder_build_failure(conditionMessage(cleaned_assets)))
    }
  }
  if (auth_enabled) {
    auth_env_file <- auth_material$env_file
    cleaned <- try(
      builder_auth_cleanup_material(
        auth_material,
        stage,
        keep_env = TRUE
      ),
      silent = TRUE
    )
    if (inherits(cleaned, "try-error")) {
      return(.builder_build_failure(
        "The authentication files could not be cleaned up."
      ))
    }
    cleanup_complete <- TRUE
    result$auth_enabled <- TRUE
    result$auth_env_file <- auth_env_file
  } else {
    result$auth_enabled <- FALSE
    result$auth_env_file <- NULL
  }
  result$publishable <- length(result$built) == length(plan$items)
  result
}
