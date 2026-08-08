## -------------------------------------------------------------------------
## Builder planning contracts.
##
## The builder UI is only a front end for these decisions. Keeping them in
## small pure helpers makes the expensive worker predictable and lets us test
## edge cases without starting Shiny.
## -------------------------------------------------------------------------

builder_profile_source_runtime(globalenv())
builder_repo_source("prerequisite.R")
builder_repo_source("recommend.R", local = globalenv())
builder_repo_source("state.R", local = globalenv())

test_that("profiles expose safe layer choices for every assay", {
  skip_if_not_installed("SeuratObject")

  local({
    builder_repo_source("inspect.R")

    counts <- Matrix::Matrix(
      matrix(
        seq_len(40),
        nrow = 5,
        dimnames = list(
          paste0("G", seq_len(5)),
          paste0("cell", seq_len(8))
        )
      ),
      sparse = TRUE
    )
    object <- SeuratObject::CreateSeuratObject(counts)
    object[["ADT"]] <- SeuratObject::CreateAssay5Object(counts = counts)
    object[["RNA"]] <- split(
      object[["RNA"]],
      f = rep(c("sample1", "sample2"), each = 4)
    )
    object$nCount_ADT <- seq_len(8)
    object$nFeature_ADT <- seq_len(8) + 10

    profile <- describe_seurat(object)

    expect_setequal(names(profile$assay_profiles), c("RNA", "ADT"))
    expect_identical(profile$assay_profiles$RNA$layers, "counts")
    expect_identical(profile$assay_profiles$RNA$default_layer, "counts")
    expect_identical(profile$assay_profiles$ADT$layers, "counts")
    expect_identical(profile$assay_profiles$ADT$nUMI, "nCount_ADT")
    expect_identical(profile$assay_profiles$ADT$nGene, "nFeature_ADT")

    prepared <- builder_prepare_export_layer(object, "RNA", "counts")
    expect_identical(
      dim(SeuratObject::LayerData(prepared[["RNA"]], layer = "counts")),
      c(5L, 8L)
    )
  })
})

test_that("layer choices require exact cell identities", {
  skip_if_not_installed("SeuratObject")

  local({
    builder_repo_source("inspect.R")

    counts <- Matrix::Matrix(
      matrix(
        seq_len(40),
        nrow = 5,
        dimnames = list(
          paste0("G", seq_len(5)),
          paste0("cell", seq_len(8))
        )
      ),
      sparse = TRUE
    )
    object <- SeuratObject::CreateSeuratObject(counts)
    object[["RNA"]] <- split(
      object[["RNA"]],
      f = rep(c("sample1", "sample2"), each = 4)
    )

    choices <- builder_layer_choices(
      object[["RNA"]],
      expected_cells = SeuratObject::Cells(object)
    )

    expect_identical(choices, "counts")
    expect_false(any(grepl("^counts[.]", choices)))
    expect_error(
      builder_layer_choices(object[["RNA"]]),
      "expected_cells"
    )

    wrong <- builder_profile_wrong_assay()
    expect_identical(
      builder_layer_choices(
        wrong$assay,
        expected_cells = wrong$expected
      ),
      character()
    )
  })
})

test_that("build plans use collision-proof filenames and resolved colours", {
  local({
    builder_repo_source("prerequisite.R")
    builder_installed_app_contract_version <- function(namespace = NULL) 1L
    builder_repo_source("preview.R")
    builder_repo_source("plan.R")

    entries <- list(
      list(
        id = "ds1",
        profile = list(nUMI = "nCount_RNA", nGene = "nFeature_RNA"),
        levels = list(cluster = c("A", "B")),
        settings = list(
          name = "A/B",
          organism = "hg",
          assay = "RNA",
          layer = "data",
          nUMI = "nCount_RNA",
          nGene = "nFeature_RNA",
          groups = "cluster",
          reductions = "umap",
          analyses = character(),
          tables = list(),
          images = list(),
          palette = "okabe_ito",
          color_overrides = list(cluster = c(B = "#ff00aa"))
        )
      ),
      list(
        id = "ds2",
        profile = list(nUMI = "nCount_RNA", nGene = "nFeature_RNA"),
        levels = list(cluster = c("A", "B")),
        settings = list(
          name = "A:B",
          organism = "hg",
          assay = "RNA",
          layer = "data",
          nUMI = "nCount_RNA",
          nGene = "nFeature_RNA",
          groups = "cluster",
          reductions = "umap",
          analyses = character(),
          tables = list(),
          images = list(),
          palette = "cerebro",
          color_overrides = list()
        )
      )
    )

    out_dir <- withr::local_tempdir()
    plan <- builder_make_plan(
      entries,
      out_dir,
      make_app = TRUE
    )

    expect_null(plan$error)
    expect_length(unique(vapply(plan$items, `[[`, "", "filename")), 2)
    expect_match(plan$items[[1]]$filename, "^01-a-b-[a-z0-9]+[.]crb$")
    expect_match(plan$items[[2]]$filename, "^02-a-b-[a-z0-9]+[.]crb$")
    expect_identical(
      unname(plan$items[[1]]$colors$cluster[["B"]]),
      "#ff00aa"
    )

    sys.source(
      builder_profile_inst_path("builder", "app_bundle.R"),
      envir = environment()
    )
    built <- file.path(
      out_dir,
      vapply(plan$items, `[[`, character(1), "filename")
    )
    lapply(built, function(path) saveRDS(list(valid = TRUE), path))
    labels <- vapply(plan$items, `[[`, character(1), "name")
    names(built) <- labels

    expect_s3_class(
      builder_app_bundle_request(plan, built, labels),
      "builder_app_bundle_request"
    )
  })
})

test_that("plan validation rejects blank and duplicate labels", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("plan.R")

    entry <- function(id, name) {
      list(
        id = id,
        profile = list(nUMI = "nCount_RNA", nGene = "nFeature_RNA"),
        levels = list(cluster = c("A", "B")),
        settings = list(
          name = name,
          organism = "hg",
          assay = "RNA",
          layer = "data",
          groups = "cluster",
          reductions = "umap",
          analyses = character(),
          tables = list(),
          images = list(),
          palette = "cerebro",
          color_overrides = list()
        )
      )
    }

    blank <- builder_make_plan(list(entry("ds1", "  ")), tempdir())
    duplicate <- builder_make_plan(
      list(entry("ds1", "PBMC"), entry("ds2", " PBMC ")),
      tempdir()
    )

    expect_match(blank$error, "name", ignore.case = TRUE)
    expect_match(duplicate$error, "unique", ignore.case = TRUE)
  })
})

test_that("plan validation requires explicit QC fields", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("plan.R")

    entry <- list(
      id = "ds1",
      profile = list(nUMI = NA_character_, nGene = NA_character_),
      levels = list(cluster = c("A", "B")),
      settings = list(
        name = "PBMC",
        organism = "hg",
        assay = "RNA",
        layer = "data",
        groups = "cluster",
        reductions = "umap",
        analyses = character(),
        tables = list(),
        images = list(),
        palette = "cerebro",
        color_overrides = list()
      )
    )

    plan <- builder_make_plan(list(entry), tempdir())

    expect_match(plan$error, "UMI|count", ignore.case = TRUE)
  })
})

test_that("analysis dependencies are normalized before a build", {
  local({
    builder_repo_source("plan.R")

    expect_identical(
      builder_normalize_analyses(
        c("marker_genes", "enriched_pathways"),
        has_marker_genes = FALSE
      ),
      c("marker_genes", "enriched_pathways")
    )
    expect_identical(
      builder_normalize_analyses(
        "enriched_pathways",
        has_marker_genes = FALSE
      ),
      character()
    )
    expect_identical(
      builder_normalize_analyses(
        "enriched_pathways",
        has_marker_genes = TRUE
      ),
      "enriched_pathways"
    )
  })
})

builder_task6_fact <- function(
  detected = FALSE,
  valid = TRUE,
  page_candidates = character(),
  diagnostics = character(),
  ...
) {
  c(
    list(
      detected = detected,
      valid = valid,
      normalized = list(rows = if (detected) 1L else 0L),
      diagnostics = diagnostics,
      requirements = character(),
      page_candidates = page_candidates
    ),
    list(...)
  )
}

builder_task6_snapshot_identity <- function() {
  structure(
    list(
      path = "/private/builder/snapshot-a",
      object_file = "/private/builder/snapshot-a/object.rds",
      owner_token = "snapshot-owner",
      created_at = as.POSIXct("2026-08-04 12:00:00", tz = "UTC"),
      object_md5 = strrep("a", 32L),
      closure_bytes = 1024
    ),
    class = c("builder_snapshot_identity", "list")
  )
}

builder_task6_asset_claim <- function(source, target, artifact) {
  structure(
    list(
      source = source,
      target = target,
      artifact = artifact
    ),
    class = c("builder_asset_claim", "list")
  )
}

builder_task6_immune_candidate <- function(
  source_kind,
  full_ir_ready,
  hla_tcr_ready = FALSE
) {
  .builder_immune_record(
    detected = TRUE,
    valid = isTRUE(full_ir_ready),
    normalized = list(
      chains = if (hla_tcr_ready) "TRA" else "IGH"
    ),
    page_candidates = c(
      if (full_ir_ready) "immune_repertoire",
      if (hla_tcr_ready) "hla_tcr_motifs"
    ),
    source_kind = source_kind,
    full_ir_ready = isTRUE(full_ir_ready),
    hla_tcr_ready = isTRUE(hla_tcr_ready),
    parseable_tcr_chains = if (hla_tcr_ready) "TRA" else character(),
    parseable_tcr_row_count = if (hla_tcr_ready) 1L else 0L,
    preview_truncated_count = 0L,
    missing_columns = character(),
    expected_chains = character(),
    unexpected_chains = character()
  )
}

builder_task6_immune_fact <- function(candidates) {
  full_sources <- names(candidates)[vapply(
    candidates,
    function(candidate) isTRUE(candidate$full_ir_ready),
    logical(1)
  )]
  motif_sources <- names(candidates)[vapply(
    candidates,
    function(candidate) isTRUE(candidate$hla_tcr_ready),
    logical(1)
  )]
  .builder_immune_record(
    detected = length(candidates) > 0L,
    valid = length(full_sources) > 0L,
    normalized = list(
      available_sources = full_sources,
      available_tcr_sources = motif_sources
    ),
    requirements = c(
      "one_valid_source",
      "source_priority_is_deferred_to_build_plan"
    ),
    page_candidates = c(
      if (length(full_sources)) "immune_repertoire",
      if (length(motif_sources)) "hla_tcr_motifs"
    ),
    candidates = candidates,
    source_overlaps = list(),
    available_sources = full_sources,
    available_tcr_sources = motif_sources,
    selected_source = NULL
  )
}

builder_task6_final_metadata_policy <- function(policy, decisions = list()) {
  for (id in names(decisions)) {
    disposition <- decisions[[id]]
    record <- policy$columns[[id]]
    record$value <- disposition
    record$disposition <- disposition
    if (identical(disposition, "included")) {
      record$effective_included <- TRUE
    } else if (identical(disposition, "excluded")) {
      record$effective_included <- FALSE
    }
    record$requires_confirmation <- disposition %in%
      c("attention", "blocking")
    policy$columns[[id]] <- record
  }
  dispositions <- vapply(
    policy$columns,
    `[[`,
    character(1),
    "disposition"
  )
  effective <- vapply(
    policy$columns,
    `[[`,
    logical(1),
    "effective_included"
  )
  ids <- names(policy$columns)
  policy$included <- ids[effective]
  policy$attention <- ids[dispositions == "attention"]
  policy$excluded <- ids[dispositions == "excluded"]
  policy$blocking <- ids[dispositions == "blocking"]
  policy$value <- policy$included
  policy$requires_confirmation <- length(policy$attention) > 0L ||
    length(policy$blocking) > 0L
  policy
}

builder_task6_entry <- function(
  status = "valid",
  immune_detected = NULL,
  full_ir_ready = FALSE,
  hla_tcr_ready = FALSE,
  aggregate_valid = TRUE,
  hla_detected = FALSE,
  hla_valid = TRUE
) {
  if (is.null(immune_detected)) {
    immune_detected <- isTRUE(full_ir_ready) || isTRUE(hla_tcr_ready)
  }
  entry <- builder_minimal_entry("dataset-a", "Dataset A")
  source <- list(
    type = "example",
    location = "task-6",
    fingerprint = "example:task-6:v1",
    format = "Built-in example"
  )
  core <- builder_manifest_entry(
    id = "expression",
    source = source[c("type", "location")],
    status = status,
    disposition = if (identical(status, "blocking")) {
      "rejected"
    } else {
      "preserved"
    },
    artifact_scope = "both",
    pages = "gene_expression"
  )
  candidates <- if (immune_detected) {
    list(
      unified_misc = builder_task6_immune_candidate(
        "unified_misc",
        full_ir_ready,
        hla_tcr_ready
      )
    )
  } else {
    list()
  }
  immune <- builder_task6_immune_fact(candidates)
  immune$valid <- if (immune_detected) aggregate_valid else TRUE
  entry$dataset_profile <- list(
    schema_version = 2L,
    identity = list(cells = list(count = 100L)),
    metadata = list(
      columns = list(
        cluster = list(
          name = "cluster",
          class = "factor",
          supported = TRUE,
          non_missing = 100L,
          unique_non_missing = 2L
        ),
        nCount_RNA = list(
          name = "nCount_RNA",
          class = "numeric",
          supported = TRUE,
          non_missing = 100L,
          unique_non_missing = 80L
        ),
        nFeature_RNA = list(
          name = "nFeature_RNA",
          class = "integer",
          supported = TRUE,
          non_missing = 100L,
          unique_non_missing = 70L
        ),
        donor_id = list(
          name = "donor_id",
          class = "character",
          supported = TRUE,
          non_missing = 100L,
          unique_non_missing = 100L
        )
      )
    ),
    source = source,
    manifest = builder_content_manifest(list(core)),
    content = list(
      immune_repertoire = immune,
      hla = builder_task6_fact(
        detected = hla_detected,
        valid = hla_valid
      )
    )
  )
  entry$snapshot_identity <- NULL
  metadata_recommendation <- builder_recommend_metadata(
    entry$dataset_profile,
    required = c("nCount_RNA", "nFeature_RNA"),
    dependency_ids = list(
      nCount_RNA = "core.qc.nUMI",
      nFeature_RNA = "core.qc.nGene"
    )
  )
  metadata_policy <- builder_task6_final_metadata_policy(
    metadata_recommendation,
    list(
      nCount_RNA = "included",
      nFeature_RNA = "included",
      donor_id = "excluded"
    )
  )
  entry$settings$recommendations <- list(
    groups = list(included = "cluster"),
    projections = list(included = "umap"),
    metadata = metadata_recommendation
  )
  entry$settings$default_group <- "cluster"
  entry$settings$default_projection <- "umap"
  entry$settings$metadata_policy <- metadata_policy
  entry$settings$nomenclature <- "name"
  entry$settings$expression_backend <- "embedded"
  entry$settings$sidecars <- character()
  entry$settings$acknowledgements <- character()
  entry
}

test_that("rail Review and BuildPlan share manifest readiness", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    defaults <- builder_freeze_plan(
      list(builder_task6_entry()),
      tempdir(),
      make_app = TRUE
    )
    expect_identical(
      defaults$app_options$point_size,
      list(overview_projection_point_size = 5)
    )
    expect_identical(defaults$app_options$variable_to_compare, FALSE)

    blocking <- builder_task6_entry(status = "blocking")
    state <- builder_dataset_state(blocking)
    failed <- builder_make_plan(list(blocking), tempdir(), make_app = FALSE)

    expect_identical(state$readiness, "blocked")
    expect_identical(state$blocking_ids, "expression")
    expect_identical(failed$error_code, "blocking_capability")
    expect_match(failed$error, "blocking capability", ignore.case = TRUE)

    ready <- builder_task6_entry()
    ready_state <- builder_dataset_state(ready)
    plan <- builder_freeze_plan(list(ready), tempdir(), make_app = FALSE)
    expect_null(plan$error)
    expect_identical(plan$items[[1L]]$readiness, ready_state$readiness)
    expect_identical(plan$readiness, ready_state$readiness)
  })
})

test_that("final included sets own their default values", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry()
    entry$levels$batch <- c("one", "two")
    entry$dataset_profile$metadata$columns$batch <- list(
      name = "batch",
      class = "factor",
      supported = TRUE,
      non_missing = 100L,
      unique_non_missing = 2L
    )
    metadata <- builder_recommend_metadata(
      entry$dataset_profile,
      required = c("nCount_RNA", "nFeature_RNA"),
      dependency_ids = list(
        nCount_RNA = "core.qc.nUMI",
        nFeature_RNA = "core.qc.nGene"
      )
    )
    entry$settings$recommendations$metadata <- metadata
    entry$settings$metadata_policy <- builder_task6_final_metadata_policy(
      metadata,
      list(
        nCount_RNA = "included",
        nFeature_RNA = "included",
        donor_id = "excluded"
      )
    )
    entry$settings$groups <- c("cluster", "batch")
    entry$settings$default_group <- "batch"
    entry$settings$recommendations$groups$included <- c("cluster", "batch")
    plan <- builder_make_plan(list(entry), tempdir(), FALSE)

    expect_null(plan$error)
    expect_true(plan$items[[1L]]$default_group %in% plan$items[[1L]]$groups)
    expect_true(
      plan$items[[1L]]$default_projection %in%
        plan$items[[1L]]$reductions
    )

    invalid_group <- entry
    invalid_group$settings$default_group <- "removed_group"
    expect_identical(
      builder_make_plan(list(invalid_group), tempdir(), FALSE)$error_code,
      "invalid_default_group"
    )

    invalid_projection <- entry
    invalid_projection$settings$default_projection <- "removed_projection"
    expect_identical(
      builder_make_plan(list(invalid_projection), tempdir(), FALSE)$error_code,
      "invalid_default_projection"
    )

    invalid_selection <- entry
    invalid_selection$settings$groups <- c("cluster", "batch", "removed")
    expect_identical(
      builder_make_plan(list(invalid_selection), tempdir(), FALSE)$error_code,
      "invalid_group_selection"
    )
  })
})

test_that("modern drafts fail closed while legacy plan callers remain valid", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    missing_manifest <- builder_task6_entry()
    missing_manifest$dataset_profile$manifest <- NULL
    expect_identical(
      builder_make_plan(list(missing_manifest), tempdir(), FALSE)$error_code,
      "missing_manifest"
    )

    loading <- builder_task6_entry()
    loading$load_state <- "loading"
    expect_identical(
      builder_make_plan(list(loading), tempdir(), FALSE)$error_code,
      "dataset_loading"
    )

    reload <- builder_task6_entry()
    reload$load_state <- "reload_required"
    expect_identical(
      builder_make_plan(list(reload), tempdir(), FALSE)$error_code,
      "dataset_reload_required"
    )

    legacy <- builder_minimal_entry("legacy", "Legacy")
    legacy_plan <- builder_make_plan(list(legacy), tempdir(), FALSE)
    expect_null(legacy_plan$error)
    expect_false(
      legacy_plan$source_snapshot_identities[["legacy"]]$available
    )
    expect_null(
      legacy_plan$source_snapshot_identities[["legacy"]]$snapshot
    )
  })
})

test_that("Builder runtimes load dataset state before planning", {
  app <- readLines(
    builder_profile_inst_path("builder", "app.R"),
    warn = FALSE
  )
  session <- readLines(
    builder_profile_inst_path("builder", "worker.R"),
    warn = FALSE
  )

  app_state <- grep('source("state.R", local = TRUE)', app, fixed = TRUE)
  app_plan <- grep('source("plan.R", local = TRUE)', app, fixed = TRUE)
  worker_state <- grep(
    'source(file.path(dir, "state.R"))',
    session,
    fixed = TRUE
  )
  worker_plan <- grep(
    'source(file.path(dir, "plan.R"))',
    session,
    fixed = TRUE
  )

  expect_length(app_state, 1L)
  expect_length(app_plan, 1L)
  expect_lt(app_state, app_plan)
  expect_length(worker_state, 1L)
  expect_length(worker_plan, 1L)
  expect_lt(worker_state, worker_plan)
})

test_that("frozen plans own every reviewed value", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    draft <- builder_task6_entry()
    app_options <- list(show_upload_ui = FALSE)
    prior <- list(entries = list(list(path = "old.crb", md5 = "old-md5")))
    plan <- builder_freeze_plan(
      list(draft),
      tempdir(),
      FALSE,
      revision = 8L,
      app_options = app_options,
      expected_prior_identity = prior
    )

    expect_null(plan$error)
    expect_s3_class(plan, "builder_build_plan")
    expect_true(all(
      c(
        "revision",
        "dataset_order",
        "source_snapshot_identities",
        "metadata_policy",
        "backend_sidecars",
        "analysis_dependency_graph",
        "viewer_page_expectations",
        "viewer_bundle_assets",
        "private_assets",
        "viewer_bundle_asset_claims",
        "private_asset_claims",
        "acknowledgements",
        "app_options",
        "output_release",
        "expected_prior_identity"
      ) %in%
        names(plan)
    ))
    expect_identical(plan$revision, 8L)
    expect_identical(plan$dataset_order, "dataset-a")
    expect_false(
      plan$source_snapshot_identities[["dataset-a"]]$available
    )
    frozen_snapshot_decision <-
      plan$source_snapshot_identities[["dataset-a"]]
    expect_true(plan$items[[1L]]$filename %in% plan$private_assets)
    expect_false(
      plan$items[[1L]]$filename %in% plan$viewer_bundle_assets
    )

    draft$settings$name <- "Changed"
    draft$settings$metadata_policy$included <- "changed"
    draft$snapshot_identity <- builder_task6_snapshot_identity()
    prior$entries[[1L]]$md5 <- "changed"

    expect_identical(plan$items[[1L]]$name, "Dataset A")
    expect_identical(
      plan$metadata_policy[["dataset-a"]]$included,
      c(
        "cell_barcode",
        "cluster",
        "nCount_RNA",
        "nFeature_RNA"
      )
    )
    expect_identical(
      plan$source_snapshot_identities[["dataset-a"]],
      frozen_snapshot_decision
    )
    expect_identical(
      plan$expected_prior_identity$entries[[1L]]$md5,
      "old-md5"
    )
    expect_identical(
      plan$viewer_page_expectations[["dataset-a"]],
      builder_viewer_page_contract(plan$manifests[["dataset-a"]])
    )

    spoofed_app <- builder_freeze_plan(
      list(builder_task6_entry()),
      tempdir(),
      FALSE,
      app_options = list(enabled = TRUE)
    )
    expect_identical(spoofed_app$error_code, "invalid_app_options")
    inert_welcome <- builder_freeze_plan(
      list(builder_task6_entry()),
      tempdir(),
      FALSE,
      app_options = list(welcome = "Hello")
    )
    expect_identical(inert_welcome$error_code, "invalid_app_options")

    invalid_initial_dataset <- builder_freeze_plan(
      list(builder_task6_entry()),
      tempdir(),
      FALSE,
      app_options = list(initial_dataset = "missing")
    )
    expect_identical(
      invalid_initial_dataset$error_code,
      "invalid_app_options"
    )

    automatic <- builder_freeze_plan(
      list(builder_task6_entry()),
      tempdir(),
      FALSE
    )
    expect_identical(automatic$app_options$initial_dataset, "dataset-a")
    expect_identical(
      automatic$app_options$initial_dataset_mode,
      "automatic"
    )
    explicit_first <- builder_freeze_plan(
      list(builder_task6_entry()),
      tempdir(),
      FALSE,
      app_options = list(initial_dataset = "dataset-a")
    )
    expect_identical(
      explicit_first$app_options$initial_dataset_mode,
      "explicit"
    )

    unsafe <- builder_freeze_plan(
      list(builder_task6_entry()),
      tempdir(),
      FALSE,
      app_options = list(callback = new.env(parent = emptyenv()))
    )
    expect_identical(unsafe$error_code, "unsafe_reference")

    attributed_reference <- structure(
      list(entries = list()),
      mutable = new.env(parent = emptyenv())
    )
    unsafe_attribute <- builder_freeze_plan(
      list(builder_task6_entry()),
      tempdir(),
      FALSE,
      expected_prior_identity = attributed_reference
    )
    expect_identical(unsafe_attribute$error_code, "unsafe_reference")
  })
})

test_that("Review App options are typed, range checked, and frozen", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    options <- list(
      show_upload_ui = TRUE,
      initial_dataset = "dataset-a",
      welcome_message = "Welcome, team!",
      point_size = list(overview_projection_point_size = 6),
      variable_to_compare = FALSE,
      host = "0.0.0.0",
      port = 4242L,
      max_request_size = 512,
      display_mode = "showcase",
      launch_browser = FALSE
    )
    plan <- builder_freeze_plan(
      list(builder_task6_entry()),
      tempdir(),
      make_app = TRUE,
      app_options = options
    )

    expect_null(plan$error)
    expect_identical(
      plan$app_options[names(options)],
      options
    )
    options$welcome_message <- "mutated"
    options$point_size$overview_projection_point_size <- 20
    expect_identical(plan$app_options$welcome_message, "Welcome, team!")
    expect_identical(
      plan$app_options$point_size$overview_projection_point_size,
      6
    )
    wrapped <- builder_make_plan(
      list(builder_task6_entry()),
      tempdir(),
      make_app = TRUE,
      app_options = options
    )
    expect_null(wrapped$error)
    expect_identical(wrapped$app_options$welcome_message, "mutated")

    invalid <- list(
      welcome_message = list(NA_character_, c("a", "b"), 1),
      point_size = list(
        list(overview_projection_point_size = -1),
        list(overview_projection_point_size = 21),
        list(overview_projection_point_size = NULL),
        list(hidden = 4),
        4
      ),
      variable_to_compare = list(NULL, NA, c(TRUE, FALSE), "cluster"),
      host = list("", NA_character_, c("a", "b"), 1),
      port = list(0, 65536, 1.5, NA, "8080"),
      max_request_size = list(0, Inf, NA, "8000"),
      display_mode = list("fullscreen", NA_character_, c("auto", "normal")),
      launch_browser = list(NA, c(TRUE, FALSE), 1)
    )
    for (field in names(invalid)) {
      for (value in invalid[[field]]) {
        failure <- builder_freeze_plan(
          list(builder_task6_entry()),
          tempdir(),
          make_app = TRUE,
          app_options = stats::setNames(list(value), field)
        )
        expect_identical(
          failure$error_code,
          "invalid_app_options",
          info = field
        )
      }
    }

    unknown <- builder_freeze_plan(
      list(builder_task6_entry()),
      tempdir(),
      make_app = TRUE,
      app_options = list(hidden_future_option = TRUE)
    )
    expect_identical(unknown$error_code, "invalid_app_options")
  })
})

test_that("BuildPlan freezes exact artifact identities for read-back", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry()
    entry$dataset_profile$identity <- list(
      cells = list(
        ids = c("cell-b", "cell-a"),
        canonical_ids = c("cell-b", "cell-a"),
        count = 2L
      ),
      features = list(
        ids = c("Gene2", "Gene1"),
        canonical_ids = c("Gene2", "Gene1"),
        count = 2L
      )
    )
    entry$levels$cluster <- c("B", "A")
    entry$settings$analyses <- "percent_mt_ribo"
    entry$dataset_profile$spatial <- list(
      section_count = 2L,
      sections = c("slice-b", "slice-a"),
      sections_truncated = FALSE,
      section_names_truncated = FALSE
    )

    plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_null(plan$error)
    expectation <- plan$items[[1L]]$artifact_identity
    expect_identical(expectation$schema_version, 2L)
    expect_identical(expectation$cells, c("cell-b", "cell-a"))
    expect_identical(expectation$features, c("Gene2", "Gene1"))
    expect_identical(expectation$group_levels$cluster, c("B", "A"))
    expect_identical(expectation$projections, "umap")
    expect_identical(
      expectation$metadata,
      c(
        "cell_barcode",
        "cluster",
        "nUMI",
        "nGene",
        "percent_mt",
        "percent_ribo"
      )
    )
    expect_identical(expectation$spatial_sections, c("slice-b", "slice-a"))

    entry$dataset_profile$identity$cells$canonical_ids[[1L]] <- "changed"
    entry$levels$cluster[[1L]] <- "changed"
    expect_identical(expectation$cells, c("cell-b", "cell-a"))
    expect_identical(expectation$group_levels$cluster, c("B", "A"))
  })
})

test_that("release manifests select a coherent visible dataset entry", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    absent <- builder_task6_entry(immune_detected = FALSE)
    visible <- builder_task6_entry(full_ir_ready = TRUE)
    visible$id <- "dataset-b"
    visible$settings$name <- "Dataset B"
    plan <- builder_freeze_plan(list(absent, visible), tempdir(), FALSE)

    expect_null(plan$error)
    expect_identical(
      plan$manifest[["immune_repertoire"]]$status,
      "valid"
    )
    expect_identical(
      plan$manifest[["immune_repertoire"]]$disposition,
      "preserved"
    )
    expect_contains(
      plan$manifest[["immune_repertoire"]]$pages,
      "immune_repertoire"
    )
    expect_s3_class(
      builder_content_manifest(unname(plan$manifest)),
      "builder_content_manifest"
    )

    filtered <- builder_task6_entry(full_ir_ready = TRUE)
    filtered$settings$content_dispositions <- list(
      immune_repertoire = "filtered"
    )
    visible_after_filtered <- builder_task6_entry(full_ir_ready = TRUE)
    visible_after_filtered$id <- "dataset-b"
    visible_after_filtered$settings$name <- "Dataset B"
    filtered_plan <- builder_freeze_plan(
      list(filtered, visible_after_filtered),
      tempdir(),
      FALSE
    )
    expect_identical(
      filtered_plan$manifest[["immune_repertoire"]]$disposition,
      "preserved"
    )
    expect_contains(
      filtered_plan$manifest[["immune_repertoire"]]$pages,
      "immune_repertoire"
    )
  })
})

test_that("immune repertoire and motif readiness compile independently", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    motif_only <- builder_task6_entry(
      full_ir_ready = FALSE,
      hla_tcr_ready = TRUE,
      aggregate_valid = TRUE
    )
    motif_plan <- builder_freeze_plan(list(motif_only), tempdir(), FALSE)
    expect_null(motif_plan$error)
    expect_false(
      motif_plan$manifest[["immune_repertoire"]]$page_visible
    )
    expect_true(
      motif_plan$manifest[["hla_tcr_motifs"]]$page_visible
    )
    expect_false(
      motif_plan$manifest[["immune_repertoire"]]$evidence$full_ir_ready
    )
    expect_true(
      motif_plan$manifest[["hla_tcr_motifs"]]$evidence$hla_tcr_ready
    )

    bcr_only <- builder_task6_entry(
      full_ir_ready = TRUE,
      hla_tcr_ready = FALSE,
      aggregate_valid = FALSE
    )
    bcr_plan <- builder_freeze_plan(
      list(bcr_only),
      tempdir(),
      FALSE
    )
    expect_null(bcr_plan$error)
    expect_true(
      bcr_plan$manifest[["immune_repertoire"]]$page_visible
    )
    expect_false(
      bcr_plan$manifest[["hla_tcr_motifs"]]$page_visible
    )

    full_tcr <- builder_task6_entry(
      full_ir_ready = TRUE,
      hla_tcr_ready = TRUE,
      aggregate_valid = FALSE
    )
    full_tcr_plan <- builder_freeze_plan(list(full_tcr), tempdir(), FALSE)
    expect_null(full_tcr_plan$error)
    expect_true(
      full_tcr_plan$manifest[["immune_repertoire"]]$page_visible
    )
    expect_true(
      full_tcr_plan$manifest[["hla_tcr_motifs"]]$page_visible
    )

    hla_only <- builder_task6_entry(
      immune_detected = FALSE,
      hla_detected = TRUE,
      hla_valid = TRUE,
      aggregate_valid = FALSE
    )
    hla_plan <- builder_freeze_plan(list(hla_only), tempdir(), FALSE)
    expect_null(hla_plan$error)
    expect_false(
      hla_plan$manifest[["immune_repertoire"]]$page_visible
    )
    expect_false(
      hla_plan$manifest[["hla_tcr_motifs"]]$page_visible
    )
    expect_identical(hla_plan$manifest[["hla"]]$status, "valid")

    filtered_motif <- builder_task6_entry(
      full_ir_ready = FALSE,
      hla_tcr_ready = TRUE
    )
    filtered_motif$settings$content_dispositions <- list(
      hla_tcr_motifs = "filtered"
    )
    filtered_motif_plan <- builder_freeze_plan(
      list(filtered_motif),
      tempdir(),
      FALSE
    )
    expect_identical(
      filtered_motif_plan$manifest[["hla_tcr_motifs"]]$disposition,
      "filtered"
    )
    expect_false(
      filtered_motif_plan$manifest[["hla_tcr_motifs"]]$page_visible
    )
  })
})

test_that("modern marker evidence owns enrichment normalization", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry()
    entry$dataset_profile$content$marker_genes <- .builder_table_record(
      detected = TRUE,
      valid = TRUE,
      normalized = list(methods = "wilcox"),
      page_candidates = "marker_genes"
    )
    entry$dataset_profile$content$enriched_pathways <-
      .builder_table_record()
    entry$settings$analyses <- "enriched_pathways"

    state <- builder_dataset_state(entry)
    plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)

    expect_null(plan$error)
    expect_identical(plan$items[[1L]]$analyses, "enriched_pathways")
    expect_identical(
      plan$items[[1L]]$analysis_dependency_graph$enriched_pathways$dependencies,
      "marker_genes"
    )
    expect_identical(
      state$manifest[["enriched_pathways"]]$disposition,
      "generated"
    )
    expect_identical(
      state$manifest[["enriched_pathways"]]$pages,
      "enriched_pathways"
    )
    expect_true(state$manifest[["enriched_pathways"]]$page_visible)
  })
})

test_that("generated analyses open their explicit Viewer pages", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry()
    entry$dataset_profile$content$most_expressed_genes <-
      .builder_table_record()
    entry$dataset_profile$content$marker_genes <- .builder_table_record()
    entry$dataset_profile$content$enriched_pathways <-
      .builder_table_record()
    entry$settings$analyses <- c(
      "most_expressed",
      "marker_genes",
      "enriched_pathways"
    )

    plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_null(plan$error)
    expected <- c(
      most_expressed_genes = "most_expressed_genes",
      marker_genes = "marker_genes",
      enriched_pathways = "enriched_pathways"
    )
    for (id in names(expected)) {
      record <- plan$items[[1L]]$manifest[[id]]
      expect_identical(record$disposition, "generated", info = id)
      expect_identical(record$pages, unname(expected[[id]]), info = id)
      expect_true(record$page_visible, info = id)
    }
  })
})

test_that("analysis execution and content dispositions have one authority", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    for (disposition in c(
      "filtered",
      "stored_only",
      "preserved",
      "converted",
      "attached"
    )) {
      conflicting <- builder_task6_entry()
      conflicting$dataset_profile$content$marker_genes <-
        .builder_table_record()
      conflicting$settings$analyses <- "marker_genes"
      conflicting$settings$content_dispositions <- list(
        marker_genes = disposition
      )
      expect_identical(
        builder_freeze_plan(
          list(conflicting),
          tempdir(),
          FALSE
        )$error_code,
        "analysis_disposition_conflict",
        info = disposition
      )
    }

    explicit <- builder_task6_entry()
    explicit$dataset_profile$content$marker_genes <-
      .builder_table_record()
    explicit$settings$analyses <- "marker_genes"
    explicit$settings$content_dispositions <- list(
      marker_genes = "generated"
    )
    plan <- builder_freeze_plan(list(explicit), tempdir(), FALSE)
    expect_null(plan$error)
    expect_identical(plan$items[[1L]]$analyses, "marker_genes")
    expect_identical(
      plan$items[[1L]]$manifest[["marker_genes"]]$disposition,
      "generated"
    )
    expect_identical(
      plan$items[[1L]]$manifest[["marker_genes"]]$pages,
      "marker_genes"
    )

    unselected <- builder_task6_entry()
    unselected$settings$content_dispositions <- list(
      marker_genes = "generated"
    )
    expect_identical(
      builder_freeze_plan(list(unselected), tempdir(), FALSE)$error_code,
      "analysis_disposition_conflict"
    )

    for (disposition in c("preserved", "converted", "attached")) {
      absent <- builder_task6_entry()
      absent$dataset_profile$content$marker_genes <-
        .builder_table_record(detected = FALSE, valid = TRUE)
      absent$settings$content_dispositions <- setNames(
        list(disposition),
        "marker_genes"
      )
      expect_identical(
        builder_freeze_plan(list(absent), tempdir(), FALSE)$error_code,
        "blocking_capability",
        info = disposition
      )
    }

    unsupported <- builder_task6_entry(
      hla_detected = TRUE,
      hla_valid = TRUE
    )
    unsupported$settings$content_dispositions <- list(hla = "generated")
    expect_identical(
      builder_freeze_plan(list(unsupported), tempdir(), FALSE)$error_code,
      "analysis_disposition_conflict"
    )

    stale <- builder_task6_entry()
    stale$dataset_profile$content$marker_genes <-
      .builder_table_record()
    stale$settings$analyses <- "marker_genes"
    stale_marker <- builder_manifest_entry(
      id = "marker_genes",
      source = list(type = "stale", location = "caller-manifest"),
      status = "valid",
      disposition = "preserved",
      artifact_scope = "both",
      pages = character()
    )
    stale$dataset_profile$manifest <- builder_content_manifest(c(
      unname(stale$dataset_profile$manifest),
      list(stale_marker)
    ))
    stale_plan <- builder_freeze_plan(list(stale), tempdir(), FALSE)
    expect_null(stale_plan$error)
    expect_identical(
      stale_plan$items[[1L]]$manifest[["marker_genes"]]$disposition,
      "generated"
    )
    expect_identical(
      stale_plan$items[[1L]]$manifest[["marker_genes"]]$pages,
      "marker_genes"
    )

    forged_core <- builder_task6_entry()
    forged_core$dataset_profile$content$expression <-
      builder_task6_fact()
    expect_identical(
      builder_freeze_plan(
        list(forged_core),
        tempdir(),
        FALSE
      )$error_code,
      "invalid_content_id"
    )
  })
})

test_that("production HLA attention requires a stable acknowledgement", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry()
    entry$dataset_profile$content$hla <- .builder_immune_record(
      detected = TRUE,
      valid = TRUE,
      diagnostics = "unknown_provenance",
      requirements = "explicit_provenance_for_association",
      attention = TRUE,
      provenance = list(has_unknown = TRUE),
      page_gate = list(role = "supporting", opens = FALSE)
    )

    state <- builder_dataset_state(entry)
    repeated <- builder_dataset_state(entry)
    expect_identical(state$readiness, "needs_attention")
    expect_identical(state$attention_ids, "hla")
    expect_identical(state$manifest[["hla"]]$status, "attention")
    expect_identical(
      state$manifest[["hla"]]$required_action$type,
      "acknowledge"
    )
    token <- state$manifest[["hla"]]$required_action$token
    expect_true(builder_has_text(token))
    expect_identical(
      repeated$manifest[["hla"]]$required_action$token,
      token
    )
    expect_identical(
      builder_freeze_plan(list(entry), tempdir(), FALSE)$error_code,
      "attention_capability"
    )

    entry$settings$acknowledgements <- token
    acknowledged <- builder_dataset_state(entry)
    plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_identical(acknowledged$readiness, "ready")
    expect_null(plan$error)
  })
})

test_that("production metadata attention requires acknowledgement", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    profile <- list(
      schema_version = 2L,
      identity = list(cells = list(count = 100L)),
      metadata = list(
        columns = list(
          donor_id = list(
            name = "donor_id",
            class = "character",
            supported = TRUE,
            non_missing = 100L,
            unique_non_missing = 80L
          )
        )
      )
    )
    metadata <- builder_recommend_metadata(profile, required = "donor_id")
    expect_true(metadata$requires_confirmation)
    expect_identical(metadata$attention, "donor_id")

    entry <- builder_task6_entry()
    entry$dataset_profile$identity <- profile$identity
    entry$dataset_profile$metadata <- profile$metadata
    entry$levels$donor_id <- c("donor-a", "donor-b")
    entry$settings$groups <- "donor_id"
    entry$settings$default_group <- "donor_id"
    entry$settings$nUMI <- "donor_id"
    entry$settings$nGene <- "donor_id"
    entry$settings$recommendations$groups$included <- "donor_id"
    entry$settings$recommendations$metadata <- metadata
    entry$settings$metadata_policy <- metadata

    state <- builder_dataset_state(entry)
    repeated <- builder_dataset_state(entry)
    expect_identical(state$readiness, "needs_attention")
    expect_identical(state$attention_ids, "metadata_policy")
    action <- state$manifest[["metadata_policy"]]$required_action
    expect_identical(action$type, "acknowledge")
    expect_true(builder_has_text(action$token))
    expect_identical(
      repeated$manifest[["metadata_policy"]]$required_action$token,
      action$token
    )
    expect_identical(
      builder_freeze_plan(list(entry), tempdir(), FALSE)$error_code,
      "attention_capability"
    )

    entry$settings$acknowledgements <- action$token
    expect_identical(builder_dataset_state(entry)$readiness, "ready")
    expect_null(builder_freeze_plan(list(entry), tempdir(), FALSE)$error)
  })
})

test_that("reserved barcode collisions remain valid blocking policies", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    profile <- list(
      schema_version = 2L,
      identity = list(cells = list(count = 4L)),
      metadata = list(
        columns = list(
          cell_barcode = list(
            name = "cell_barcode",
            class = "character",
            supported = TRUE,
            non_missing = 4L,
            unique_non_missing = 4L
          )
        )
      )
    )
    policy <- builder_recommend_metadata(profile)
    expect_identical(
      policy$columns$cell_barcode$disposition,
      "blocking"
    )
    expect_true(policy$columns$cell_barcode$effective_included)

    entry <- builder_task6_entry()
    entry$dataset_profile$identity <- profile$identity
    entry$dataset_profile$metadata <- profile$metadata
    entry$settings$groups <- "cell_barcode"
    entry$settings$default_group <- "cell_barcode"
    entry$settings$nUMI <- "cell_barcode"
    entry$settings$nGene <- "cell_barcode"
    entry$settings$recommendations$groups$included <- "cell_barcode"
    entry$settings$recommendations$metadata <- policy
    entry$settings$metadata_policy <- policy
    state <- builder_dataset_state(entry)
    plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)

    expect_identical(state$readiness, "blocked")
    expect_identical(state$blocking_ids, "metadata_policy")
    expect_identical(plan$error_code, "blocking_capability")
  })
})

test_that("final metadata cannot weaken profiled hard blockers", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    collision_profile <- list(
      schema_version = 2L,
      identity = list(cells = list(count = 4L)),
      metadata = list(
        columns = list(
          cell_barcode = list(
            name = "cell_barcode",
            class = "character",
            supported = TRUE,
            non_missing = 4L,
            unique_non_missing = 4L
          )
        )
      )
    )
    collision <- builder_recommend_metadata(collision_profile)
    weakened <- builder_task6_final_metadata_policy(
      collision,
      list(cell_barcode = "attention")
    )
    collision_entry <- builder_task6_entry()
    collision_entry$dataset_profile$identity <- collision_profile$identity
    collision_entry$dataset_profile$metadata <- collision_profile$metadata
    collision_entry$settings$groups <- "cell_barcode"
    collision_entry$settings$default_group <- "cell_barcode"
    collision_entry$settings$nUMI <- "cell_barcode"
    collision_entry$settings$nGene <- "cell_barcode"
    collision_entry$settings$recommendations$groups$included <-
      "cell_barcode"
    collision_entry$settings$recommendations$metadata <- collision
    collision_entry$settings$metadata_policy <- weakened
    expect_identical(
      builder_freeze_plan(
        list(collision_entry),
        tempdir(),
        FALSE
      )$error_code,
      "invalid_metadata_policy"
    )

    for (bad_class in c("character", "list", "data.frame")) {
      unsafe <- builder_task6_entry()
      unsafe$dataset_profile$metadata$columns$unsafe_column <- list(
        name = "unsafe_column",
        class = bad_class,
        supported = FALSE,
        non_missing = 100L,
        unique_non_missing = 2L
      )
      recommendation <- builder_recommend_metadata(
        unsafe$dataset_profile,
        required = c("nCount_RNA", "nFeature_RNA"),
        dependency_ids = list(
          nCount_RNA = "core.qc.nUMI",
          nFeature_RNA = "core.qc.nGene"
        )
      )
      included <- builder_task6_final_metadata_policy(
        recommendation,
        list(
          nCount_RNA = "included",
          nFeature_RNA = "included",
          donor_id = "excluded",
          unsafe_column = "included"
        )
      )
      unsafe$settings$recommendations$metadata <- recommendation
      unsafe$settings$metadata_policy <- included
      expect_identical(
        builder_freeze_plan(list(unsafe), tempdir(), FALSE)$error_code,
        "invalid_metadata_policy",
        info = bad_class
      )

      fallback <- unsafe
      fallback$settings$metadata_policy <- NULL
      fallback$settings$recommendations$metadata <- included
      expect_identical(
        builder_freeze_plan(list(fallback), tempdir(), FALSE)$error_code,
        "invalid_metadata_policy",
        info = paste("fallback", bad_class)
      )
    }
  })
})

test_that("final metadata covers selections and declared dependencies", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry()
    entry$settings$recommendations$metadata$columns$donor_id$dependency_ids <-
      "content.hla_association"
    entry$settings$metadata_policy$columns$donor_id$dependency_ids <-
      "content.hla_association"
    entry$settings$metadata_policy <- builder_task6_final_metadata_policy(
      entry$settings$metadata_policy,
      list(donor_id = "included")
    )
    expect_null(builder_freeze_plan(list(entry), tempdir(), FALSE)$error)

    included_group <- builder_task6_entry()
    included_group$settings$included_groups <- c("cluster", "donor_id")
    expect_identical(
      builder_freeze_plan(
        list(included_group),
        tempdir(),
        FALSE
      )$error_code,
      "metadata_dependency_conflict"
    )
    included_group$settings$metadata_policy <-
      builder_task6_final_metadata_policy(
        included_group$settings$metadata_policy,
        list(donor_id = "included")
      )
    expect_null(
      builder_freeze_plan(
        list(included_group),
        tempdir(),
        FALSE
      )$error
    )

    for (id in c("cluster", "nCount_RNA", "nFeature_RNA", "donor_id")) {
      missing <- entry
      missing$settings$metadata_policy <-
        builder_task6_final_metadata_policy(
          missing$settings$metadata_policy,
          setNames(list("excluded"), id)
        )
      expect_identical(
        builder_freeze_plan(list(missing), tempdir(), FALSE)$error_code,
        "metadata_dependency_conflict",
        info = id
      )
    }

    fallback <- entry
    fallback$settings$recommendations$metadata <-
      builder_task6_final_metadata_policy(
        fallback$settings$recommendations$metadata,
        list(
          nCount_RNA = "included",
          nFeature_RNA = "included",
          donor_id = "excluded"
        )
      )
    fallback$settings$metadata_policy <- NULL
    expect_identical(
      builder_freeze_plan(list(fallback), tempdir(), FALSE)$error_code,
      "metadata_dependency_conflict"
    )

    required <- builder_task6_entry()
    required_recommendation <- builder_recommend_metadata(
      required$dataset_profile,
      required = c("nCount_RNA", "nFeature_RNA", "donor_id"),
      dependency_ids = list(
        nCount_RNA = "core.qc.nUMI",
        nFeature_RNA = "core.qc.nGene"
      )
    )
    required$settings$recommendations$metadata <- required_recommendation
    required$settings$metadata_policy <-
      builder_task6_final_metadata_policy(
        required_recommendation,
        list(
          nCount_RNA = "included",
          nFeature_RNA = "included",
          donor_id = "excluded"
        )
      )
    required$settings$metadata_policy$columns$donor_id$required <- FALSE
    expect_identical(
      builder_freeze_plan(list(required), tempdir(), FALSE)$error_code,
      "metadata_dependency_conflict"
    )
  })
})

test_that("missing required metadata remains a valid blocking policy", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    profile <- list(
      schema_version = 2L,
      identity = list(cells = list(count = 4L)),
      metadata = list(columns = list())
    )
    policy <- builder_recommend_metadata(
      profile,
      required = "missing_required"
    )
    sentinel <- policy$columns$missing_required
    expect_identical(sentinel$class, "missing")
    expect_true(sentinel$required)
    expect_identical(sentinel$disposition, "blocking")
    expect_false(sentinel$effective_included)

    entry <- builder_task6_entry()
    entry$dataset_profile$identity <- profile$identity
    entry$dataset_profile$metadata <- profile$metadata
    entry$settings$groups <- "cell_barcode"
    entry$settings$default_group <- "cell_barcode"
    entry$settings$nUMI <- "cell_barcode"
    entry$settings$nGene <- "cell_barcode"
    entry$settings$recommendations$groups$included <- "cell_barcode"
    entry$settings$recommendations$metadata <- policy
    entry$settings$metadata_policy <- policy
    state <- builder_dataset_state(entry)
    plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)

    expect_identical(state$readiness, "blocked")
    expect_identical(state$blocking_ids, "metadata_policy")
    expect_identical(plan$error_code, "blocking_capability")
  })
})

test_that("final metadata policy owns review and frozen output", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    selected <- builder_task6_entry()
    recommendation <- selected$settings$recommendations$metadata
    final_policy <- selected$settings$metadata_policy
    expect_identical(
      recommendation$columns$donor_id$disposition,
      "attention"
    )
    expect_identical(
      final_policy$columns$donor_id$disposition,
      "excluded"
    )
    expect_false(identical(final_policy, recommendation))
    state <- builder_dataset_state(selected)
    plan <- builder_freeze_plan(
      list(selected),
      tempdir(),
      FALSE
    )
    expect_identical(state$readiness, "ready")
    expect_identical(
      state$manifest[["metadata_policy"]]$evidence$normalized$excluded,
      final_policy$excluded
    )
    expect_null(plan$error)
    expect_identical(
      plan$items[[1L]]$metadata_policy,
      final_policy
    )

    recommendation_only <- builder_task6_entry()
    recommendation_only$settings$metadata_policy <- NULL
    recommendation_action <- builder_dataset_state(
      recommendation_only
    )$manifest[["metadata_policy"]]$required_action
    recommendation_only$settings$acknowledgements <-
      recommendation_action$token
    recommendation_plan <- builder_freeze_plan(
      list(recommendation_only),
      tempdir(),
      FALSE
    )
    expect_null(recommendation_plan$error)
    expect_identical(
      recommendation_plan$items[[1L]]$metadata_policy,
      recommendation
    )

    final_attention <- builder_task6_entry()
    final_attention$settings$metadata_policy <- recommendation
    attention_state <- builder_dataset_state(final_attention)
    expect_identical(attention_state$readiness, "needs_attention")
    expect_identical(
      builder_freeze_plan(
        list(final_attention),
        tempdir(),
        FALSE
      )$error_code,
      "attention_capability"
    )
    final_attention$settings$acknowledgements <- attention_state$manifest[[
      "metadata_policy"
    ]]$required_action$token
    attention_plan <- builder_freeze_plan(
      list(final_attention),
      tempdir(),
      FALSE
    )
    expect_null(attention_plan$error)
    expect_identical(
      attention_plan$items[[1L]]$metadata_policy,
      recommendation
    )
  })
})

test_that("final metadata policies must prove their own consistency", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry()
    policy <- entry$settings$metadata_policy

    without_recommendation <- entry
    without_recommendation$settings$recommendations$metadata <- NULL
    expect_null(
      builder_freeze_plan(
        list(without_recommendation),
        tempdir(),
        FALSE
      )$error
    )

    unknown_column <- policy
    unknown_column$columns$invented <- unknown_column$columns$donor_id
    unknown_column$columns$invented$name <- "invented"
    unknown_column <- builder_task6_final_metadata_policy(
      unknown_column
    )

    forged_missing_sentinel <- policy
    forged_missing_sentinel$columns$invented <-
      forged_missing_sentinel$columns$donor_id
    forged_missing_sentinel$columns$invented$name <- "invented"
    forged_missing_sentinel$columns$invented$class <- "missing"
    forged_missing_sentinel$columns$invented$required <- TRUE
    forged_missing_sentinel$columns$invented$non_missing <- 1L
    forged_missing_sentinel$columns$invented$unique_non_missing <- 0L
    forged_missing_sentinel <- builder_task6_final_metadata_policy(
      forged_missing_sentinel,
      list(invented = "blocking")
    )

    missing_source_column <- policy
    missing_source_column$columns$donor_id <- NULL
    missing_source_column <- builder_task6_final_metadata_policy(
      missing_source_column
    )

    overlapping_sets <- policy
    overlapping_sets$included <- c(
      overlapping_sets$included,
      "donor_id"
    )
    overlapping_sets$value <- overlapping_sets$included

    missing_barcode <- policy
    missing_barcode$columns$cell_barcode <- NULL
    missing_barcode <- builder_task6_final_metadata_policy(
      missing_barcode
    )

    excluded_barcode <- builder_task6_final_metadata_policy(
      policy,
      list(cell_barcode = "excluded")
    )

    disposition_drift <- policy
    disposition_drift$columns$donor_id$disposition <- "included"

    effective_drift <- policy
    effective_drift$columns$donor_id$value <- "included"
    effective_drift$columns$donor_id$disposition <- "included"
    effective_drift$columns$donor_id$effective_included <- FALSE
    effective_drift$excluded <- setdiff(
      effective_drift$excluded,
      "donor_id"
    )

    record_value_drift <- policy
    record_value_drift$columns$donor_id$value <- "included"

    record_confirmation_drift <- policy
    record_confirmation_drift$columns$donor_id$requires_confirmation <- TRUE

    aggregate_drift <- policy
    aggregate_drift$excluded <- character()

    policy_value_drift <- policy
    policy_value_drift$value <- "donor_id"

    policy_confirmation_drift <- policy
    policy_confirmation_drift$requires_confirmation <- TRUE

    mutable_record <- policy
    mutable_record$columns$donor_id$mutable <- new.env(parent = emptyenv())

    invalid <- list(
      unknown_column = unknown_column,
      forged_missing_sentinel = forged_missing_sentinel,
      missing_source_column = missing_source_column,
      overlapping_sets = overlapping_sets,
      missing_barcode = missing_barcode,
      excluded_barcode = excluded_barcode,
      disposition_drift = disposition_drift,
      effective_drift = effective_drift,
      record_value_drift = record_value_drift,
      record_confirmation_drift = record_confirmation_drift,
      aggregate_drift = aggregate_drift,
      policy_value_drift = policy_value_drift,
      policy_confirmation_drift = policy_confirmation_drift,
      mutable_record = mutable_record
    )
    for (label in names(invalid)) {
      malformed <- entry
      malformed$settings$metadata_policy <- invalid[[label]]
      expect_identical(
        builder_freeze_plan(
          list(malformed),
          tempdir(),
          FALSE
        )$error_code,
        "invalid_metadata_policy",
        info = label
      )
    }
  })
})

test_that("immune source and gate decisions remain truthful", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    sources <- list(
      unified_misc = list(
        disposition = "preserved",
        location = "@misc$immune_repertoire"
      ),
      metadata = list(
        disposition = "converted",
        location = "@meta.data"
      ),
      legacy_bcr = list(
        disposition = "converted",
        location = "@misc$bcr_data"
      ),
      legacy_tcr = list(
        disposition = "converted",
        location = "@misc$tcr_data"
      )
    )
    for (source_kind in names(sources)) {
      entry <- builder_task6_entry(immune_detected = FALSE)
      candidate <- builder_task6_immune_candidate(
        source_kind,
        full_ir_ready = TRUE,
        hla_tcr_ready = identical(source_kind, "legacy_tcr")
      )
      entry$dataset_profile$content$immune_repertoire <-
        builder_task6_immune_fact(setNames(list(candidate), source_kind))
      plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)
      record <- plan$items[[1L]]$manifest[["immune_repertoire"]]

      expect_null(plan$error, info = source_kind)
      expect_identical(
        record$evidence$selected_source,
        source_kind,
        info = source_kind
      )
      expect_identical(
        record$disposition,
        sources[[source_kind]]$disposition,
        info = source_kind
      )
      expect_identical(
        record$source$location,
        sources[[source_kind]]$location,
        info = source_kind
      )
    }

    mixed <- builder_task6_entry(immune_detected = FALSE)
    mixed$dataset_profile$content$immune_repertoire <-
      builder_task6_immune_fact(list(
        unified_misc = builder_task6_immune_candidate(
          "unified_misc",
          full_ir_ready = FALSE,
          hla_tcr_ready = TRUE
        ),
        metadata = builder_task6_immune_candidate(
          "metadata",
          full_ir_ready = TRUE,
          hla_tcr_ready = FALSE
        )
      ))
    mixed_plan <- builder_freeze_plan(list(mixed), tempdir(), FALSE)
    expect_null(mixed_plan$error)
    expect_identical(
      mixed_plan$items[[1L]]$manifest[[
        "immune_repertoire"
      ]]$evidence$selected_source,
      "metadata"
    )
    expect_identical(
      mixed_plan$items[[1L]]$manifest[["immune_repertoire"]]$disposition,
      "converted"
    )
    expect_identical(
      mixed_plan$items[[1L]]$manifest[[
        "hla_tcr_motifs"
      ]]$evidence$selected_source,
      "unified_misc"
    )
    expect_identical(
      mixed_plan$items[[1L]]$manifest[["hla_tcr_motifs"]]$disposition,
      "preserved"
    )

    motif_only <- builder_task6_entry(immune_detected = FALSE)
    motif_only$dataset_profile$content$immune_repertoire <-
      builder_task6_immune_fact(list(
        unified_misc = builder_task6_immune_candidate(
          "unified_misc",
          full_ir_ready = FALSE,
          hla_tcr_ready = TRUE
        )
      ))
    state <- builder_dataset_state(motif_only)
    expect_identical(
      state$manifest[["immune_repertoire"]]$status,
      "not_applicable"
    )
    expect_true(state$manifest[["hla_tcr_motifs"]]$page_visible)

    motif_only$settings$content_dispositions <- list(
      immune_repertoire = "preserved"
    )
    spoofed <- builder_freeze_plan(list(motif_only), tempdir(), FALSE)
    expect_identical(spoofed$error_code, "blocking_capability")
  })
})

test_that("stale explicit immune sources block after candidates disappear", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry(immune_detected = FALSE)
    entry$settings$content_sources <- list(
      immune_repertoire = "unified_misc"
    )

    state <- builder_dataset_state(entry)
    expect_identical(state$readiness, "blocked")
    expect_true("immune_repertoire" %in% state$blocking_ids)
    expect_true(
      "selected_source_is_not_ready" %in%
        state$manifest[["immune_repertoire"]]$evidence$diagnostics
    )

    plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_identical(plan$error_code, "blocking_capability")
    expect_true(
      "immune_repertoire" %in% plan$details$capability_ids
    )
  })
})

test_that("malformed immune candidate and overlap evidence fails typed", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    capture_state <- function(entry) {
      tryCatch(
        builder_dataset_state(entry),
        error = function(error) error
      )
    }
    capture_plan <- function(entry) {
      tryCatch(
        builder_freeze_plan(list(entry), tempdir(), FALSE),
        error = function(error) error
      )
    }
    expect_typed <- function(entry, code) {
      state <- capture_state(entry)
      plan <- capture_plan(entry)
      expect_s3_class(state, "builder_state_error")
      expect_identical(state$code, code)
      expect_s3_class(plan, "builder_plan_failure")
      expect_identical(plan$error_code, code)
    }

    candidate_cases <- list(
      source_kind = c("unified_misc", "metadata"),
      detected = c(TRUE, FALSE),
      full_ir_ready = "yes",
      hla_tcr_ready = NA
    )
    for (field in names(candidate_cases)) {
      malformed <- builder_task6_entry(full_ir_ready = TRUE)
      malformed$dataset_profile$content$immune_repertoire$candidates$unified_misc[[
        field
      ]] <- candidate_cases[[field]]
      expect_typed(malformed, "invalid_immune_candidates")
    }

    overlap_entry <- builder_task6_entry(immune_detected = FALSE)
    overlap_fact <- builder_task6_immune_fact(list(
      unified_misc = builder_task6_immune_candidate(
        "unified_misc",
        full_ir_ready = TRUE
      ),
      metadata = builder_task6_immune_candidate(
        "metadata",
        full_ir_ready = TRUE
      )
    ))
    overlap_fact$source_overlaps <- list(list(
      left = c("unified_misc", "metadata"),
      right = "metadata",
      n_overlap = 1L,
      n_divergent = 1L,
      equivalent = FALSE
    ))
    overlap_entry$dataset_profile$content$immune_repertoire <- overlap_fact
    expect_typed(overlap_entry, "invalid_immune_overlaps")
  })
})

test_that("divergent immune sources require an explicit source choice", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry(immune_detected = FALSE)
    fact <- builder_task6_immune_fact(list(
      unified_misc = builder_task6_immune_candidate(
        "unified_misc",
        full_ir_ready = TRUE
      ),
      metadata = builder_task6_immune_candidate(
        "metadata",
        full_ir_ready = TRUE
      )
    ))
    fact$source_overlaps <- list(list(
      left = "unified_misc",
      right = "metadata",
      n_overlap = 2L,
      n_exact = 0L,
      n_divergent = 2L,
      overlap_preview = c("cell-a", "cell-b"),
      divergent_preview = c("cell-a", "cell-b"),
      preview_truncated_count = 0L
    ))
    entry$dataset_profile$content$immune_repertoire <- fact

    state <- builder_dataset_state(entry)
    blocked <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_identical(state$readiness, "blocked")
    expect_identical(state$blocking_ids, "immune_repertoire")
    expect_identical(blocked$error_code, "blocking_capability")

    entry$settings$content_sources <- list(
      immune_repertoire = "unified_misc"
    )
    selected <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_null(selected$error)
    expect_identical(
      selected$items[[1L]]$manifest[[
        "immune_repertoire"
      ]]$evidence$selected_source,
      "unified_misc"
    )
  })
})

test_that("selected immune filtering requires a stable acknowledgement", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry(immune_detected = FALSE)
    candidate <- builder_task6_immune_candidate(
      "unified_misc",
      full_ir_ready = TRUE,
      hla_tcr_ready = TRUE
    )
    candidate$attention <- TRUE
    candidate$diagnostics <- c(
      "empty_sample_table",
      "barcodes_outside_dataset"
    )
    entry$dataset_profile$content$immune_repertoire <-
      builder_task6_immune_fact(list(unified_misc = candidate))

    state <- builder_dataset_state(entry)
    expect_identical(state$readiness, "needs_attention")
    expect_identical(
      state$attention_ids,
      c("immune_repertoire", "hla_tcr_motifs")
    )
    actions <- lapply(
      state$manifest[state$attention_ids],
      `[[`,
      "required_action"
    )
    expect_true(all(vapply(
      actions,
      function(action) identical(action$type, "acknowledge"),
      logical(1)
    )))

    blocked <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_identical(blocked$error_code, "attention_capability")

    entry$settings$acknowledgements <- unique(vapply(
      actions,
      `[[`,
      character(1),
      "token"
    ))
    plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_null(plan$error)
    expect_setequal(
      plan$items[[1L]]$acknowledgements,
      entry$settings$acknowledgements
    )
    expect_setequal(
      intersect(
        plan$items[[1L]]$viewer_page_expectations$visible_conditional,
        c("immune_repertoire", "hla_tcr_motifs")
      ),
      c("immune_repertoire", "hla_tcr_motifs")
    )
  })
})

test_that("legacy BCR and TCR freeze as one conversion source set", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry(immune_detected = FALSE)
    entry$dataset_profile$content$immune_repertoire <-
      builder_task6_immune_fact(list(
        legacy_bcr = builder_task6_immune_candidate(
          "legacy_bcr",
          full_ir_ready = TRUE,
          hla_tcr_ready = FALSE
        ),
        legacy_tcr = builder_task6_immune_candidate(
          "legacy_tcr",
          full_ir_ready = TRUE,
          hla_tcr_ready = TRUE
        )
      ))

    plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_null(plan$error)
    immune <- plan$items[[1L]]$manifest[["immune_repertoire"]]
    motif <- plan$items[[1L]]$manifest[["hla_tcr_motifs"]]
    expect_identical(
      immune$evidence$selected_sources,
      c("legacy_bcr", "legacy_tcr")
    )
    expect_null(immune$evidence$selected_source)
    expect_named(
      immune$evidence$selected_candidates,
      c("legacy_bcr", "legacy_tcr")
    )
    expect_identical(immune$disposition, "converted")
    expect_match(immune$source$location, "bcr_data")
    expect_match(immune$source$location, "tcr_data")
    expect_identical(motif$evidence$selected_source, "legacy_tcr")
  })
})

test_that("production complementary legacy chains are not source conflicts", {
  skip_if_not_installed("SeuratObject")

  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    object <- builder_immune_fixture_object()
    cells <- SeuratObject::Cells(object)
    object@misc$bcr_data <- list(
      sample_a = builder_immune_fixture_table(
        cells[1:2],
        chain = "IGH",
        suffix = "legacy-bcr"
      )
    )
    object@misc$tcr_data <- list(
      sample_a = builder_immune_fixture_table(
        cells[1:2],
        chain = "TRB",
        suffix = "legacy-tcr"
      )
    )
    fact <- builder_profile_immune_content(
      object,
      builder_immune_fixture_context(object)
    )$immune_repertoire

    expect_true(fact$candidates$legacy_bcr$full_ir_ready)
    expect_true(fact$candidates$legacy_tcr$full_ir_ready)
    expect_false(any(vapply(
      fact$full_source_overlaps,
      function(overlap) overlap$n_divergent > 0L,
      logical(1)
    )))

    entry <- builder_task6_entry(immune_detected = FALSE)
    entry$dataset_profile$content$immune_repertoire <- fact
    plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)

    expect_null(plan$error)
    immune <- plan$items[[1L]]$manifest[["immune_repertoire"]]
    expect_identical(
      immune$evidence$selected_sources,
      c("legacy_bcr", "legacy_tcr")
    )
    expect_identical(immune$disposition, "converted")
  })
})

test_that("production same-chain disagreements require source selection", {
  skip_if_not_installed("SeuratObject")

  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    object <- builder_immune_fixture_object()
    cells <- SeuratObject::Cells(object)
    object@misc$immune_repertoire <- list(
      sample_a = builder_immune_fixture_table(
        cells[1:2],
        chain = "TRB",
        suffix = "unified"
      )
    )
    object@misc$tcr_data <- list(
      sample_a = builder_immune_fixture_table(
        cells[1:2],
        chain = "TRB",
        suffix = "legacy"
      )
    )
    fact <- builder_profile_immune_content(
      object,
      builder_immune_fixture_context(object)
    )$immune_repertoire
    expect_true(any(vapply(
      fact$full_source_overlaps,
      function(overlap) {
        identical(overlap$left, "unified_misc") &&
          identical(overlap$right, "legacy_tcr") &&
          overlap$n_divergent > 0L
      },
      logical(1)
    )))

    entry <- builder_task6_entry(immune_detected = FALSE)
    entry$dataset_profile$content$immune_repertoire <- fact
    blocked <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_identical(blocked$error_code, "blocking_capability")

    entry$settings$content_sources <- list(
      immune_repertoire = "unified_misc",
      hla_tcr_motifs = "unified_misc"
    )
    selected <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_null(selected$error)
    expect_identical(
      selected$items[[1L]]$manifest[[
        "immune_repertoire"
      ]]$evidence$selected_source,
      "unified_misc"
    )
  })
})

test_that("production partial full-IR overlap requires source selection", {
  skip_if_not_installed("SeuratObject")

  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    object <- builder_immune_fixture_object()
    cells <- SeuratObject::Cells(object)
    object@misc$immune_repertoire <- list(
      sample_a = builder_immune_fixture_table(
        cells[1:2],
        chain = "IGH",
        suffix = "shared"
      )
    )
    object@misc$bcr_data <- list(
      sample_a = builder_immune_fixture_table(
        cells[1:3],
        chain = "IGH",
        suffix = "shared"
      )
    )
    fact <- builder_profile_immune_content(
      object,
      builder_immune_fixture_context(object)
    )$immune_repertoire
    partial <- Filter(
      function(overlap) {
        identical(overlap$left, "unified_misc") &&
          identical(overlap$right, "legacy_bcr")
      },
      fact$full_source_overlaps
    )[[1L]]
    expect_gt(partial$n_overlap, 0L)
    expect_identical(partial$n_divergent, 0L)
    expect_false(partial$equivalent)

    entry <- builder_task6_entry(immune_detected = FALSE)
    entry$dataset_profile$content$immune_repertoire <- fact
    blocked <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_identical(blocked$error_code, "blocking_capability")

    entry$settings$content_sources <- list(
      immune_repertoire = "unified_misc"
    )
    selected <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_null(selected$error)
    expect_identical(
      selected$items[[1L]]$manifest[[
        "immune_repertoire"
      ]]$evidence$selected_source,
      "unified_misc"
    )

    disjoint <- builder_immune_fixture_object()
    disjoint_cells <- SeuratObject::Cells(disjoint)
    disjoint@misc$immune_repertoire <- list(
      sample_a = builder_immune_fixture_table(
        disjoint_cells[[1L]],
        chain = "IGH",
        suffix = "unified"
      )
    )
    disjoint@misc$bcr_data <- list(
      sample_a = builder_immune_fixture_table(
        disjoint_cells[[2L]],
        chain = "IGH",
        suffix = "legacy"
      )
    )
    disjoint_fact <- builder_profile_immune_content(
      disjoint,
      builder_immune_fixture_context(disjoint)
    )$immune_repertoire
    disjoint_entry <- builder_task6_entry(immune_detected = FALSE)
    disjoint_entry$dataset_profile$content$immune_repertoire <-
      disjoint_fact
    disjoint_plan <- builder_freeze_plan(
      list(disjoint_entry),
      tempdir(),
      FALSE
    )
    expect_identical(disjoint_plan$error_code, "blocking_capability")

    disjoint_entry$settings$content_sources <- list(
      immune_repertoire = "unified_misc"
    )
    disjoint_plan <- builder_freeze_plan(
      list(disjoint_entry),
      tempdir(),
      FALSE
    )
    expect_null(disjoint_plan$error)
    expect_identical(
      disjoint_plan$items[[1L]]$manifest[[
        "immune_repertoire"
      ]]$evidence$selected_source,
      "unified_misc"
    )
  })
})

test_that("equivalent full-IR sources auto-select by priority", {
  skip_if_not_installed("SeuratObject")

  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    object <- builder_immune_fixture_object()
    cells <- SeuratObject::Cells(object)
    common <- builder_immune_fixture_table(
      cells[1:2],
      chain = "TRB",
      suffix = "same"
    )
    object@misc$immune_repertoire <- list(sample_a = common)
    object@misc$tcr_data <- list(sample_a = common)
    fact <- builder_profile_immune_content(
      object,
      builder_immune_fixture_context(object)
    )$immune_repertoire
    exact <- Filter(
      function(overlap) {
        identical(overlap$left, "unified_misc") &&
          identical(overlap$right, "legacy_tcr")
      },
      fact$full_source_overlaps
    )[[1L]]
    expect_true(exact$equivalent)

    entry <- builder_task6_entry(immune_detected = FALSE)
    entry$dataset_profile$content$immune_repertoire <- fact
    selected <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_null(selected$error)
    expect_identical(
      selected$items[[1L]]$manifest[[
        "immune_repertoire"
      ]]$evidence$selected_source,
      "unified_misc"
    )
  })
})

test_that("production motif-only sources need equivalence or selection", {
  skip_if_not_installed("SeuratObject")

  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    motif_table <- function(barcodes, aa) {
      data.frame(
        barcode = barcodes,
        CTgene = rep("TRBV1.TRBJ1", length(barcodes)),
        CTaa = aa,
        stringsAsFactors = FALSE
      )
    }

    divergent <- builder_immune_fixture_object()
    cells <- SeuratObject::Cells(divergent)
    divergent@misc$immune_repertoire <- list(
      sample_a = motif_table(cells[1:2], c("CASS_A", "CASS_B"))
    )
    divergent@meta.data$CTgene <- NA_character_
    divergent@meta.data$CTaa <- NA_character_
    divergent@meta.data[cells[1:2], "CTgene"] <- "TRBV1.TRBJ1"
    divergent@meta.data[cells[1:2], "CTaa"] <- c(
      "DIFFERENT_A",
      "CASS_B"
    )
    divergent_fact <- builder_profile_immune_content(
      divergent,
      builder_immune_fixture_context(divergent)
    )$immune_repertoire
    expect_true(divergent_fact$candidates$unified_misc$hla_tcr_ready)
    expect_true(divergent_fact$candidates$metadata$hla_tcr_ready)
    expect_true(any(vapply(
      divergent_fact$motif_source_overlaps,
      function(overlap) overlap$n_divergent > 0L,
      logical(1)
    )))

    entry <- builder_task6_entry(immune_detected = FALSE)
    entry$dataset_profile$content$immune_repertoire <- divergent_fact
    blocked <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_identical(blocked$error_code, "blocking_capability")

    entry$settings$content_sources <- list(
      hla_tcr_motifs = "unified_misc"
    )
    selected <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_null(selected$error)
    expect_identical(
      selected$items[[1L]]$manifest[[
        "hla_tcr_motifs"
      ]]$evidence$selected_source,
      "unified_misc"
    )

    disjoint <- builder_immune_fixture_object()
    disjoint$orig.ident <- "sample_a"
    disjoint$sample <- "sample_a"
    disjoint_cells <- SeuratObject::Cells(disjoint)
    disjoint@misc$immune_repertoire <- list(
      sample_a = motif_table(disjoint_cells[[1L]], "CASS_UNIFIED")
    )
    disjoint@meta.data$CTgene <- NA_character_
    disjoint@meta.data$CTaa <- NA_character_
    disjoint@meta.data[disjoint_cells[[2L]], "CTgene"] <-
      "TRBV1.TRBJ1"
    disjoint@meta.data[disjoint_cells[[2L]], "CTaa"] <- "CASS_METADATA"
    disjoint_fact <- builder_profile_immune_content(
      disjoint,
      builder_immune_fixture_context(disjoint)
    )$immune_repertoire
    disjoint_entry <- builder_task6_entry(immune_detected = FALSE)
    disjoint_entry$dataset_profile$content$immune_repertoire <-
      disjoint_fact

    disjoint_plan <- builder_freeze_plan(
      list(disjoint_entry),
      tempdir(),
      FALSE
    )
    expect_identical(disjoint_plan$error_code, "blocking_capability")
    disjoint_entry$settings$content_sources <- list(
      hla_tcr_motifs = "metadata"
    )
    expect_null(
      builder_freeze_plan(
        list(disjoint_entry),
        tempdir(),
        FALSE
      )$error
    )
  })
})

test_that("optional-content facts require inert typed evidence", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    capture_state <- function(entry) {
      tryCatch(
        builder_dataset_state(entry),
        error = function(error) error
      )
    }
    capture_plan <- function(entry) {
      tryCatch(
        builder_freeze_plan(list(entry), tempdir(), FALSE),
        error = function(error) error
      )
    }
    expect_invalid_fact <- function(entry) {
      state <- capture_state(entry)
      plan <- capture_plan(entry)
      expect_s3_class(state, "builder_state_error")
      expect_identical(state$code, "invalid_content_evidence")
      expect_s3_class(plan, "builder_plan_failure")
      expect_identical(plan$error_code, "invalid_content_evidence")
    }

    accessor_called <- FALSE
    method_env <- environment(builder_dataset_state)
    method_name <- "$.builder_hostile_fact"
    had_method <- exists(method_name, envir = method_env, inherits = FALSE)
    if (had_method) {
      prior_method <- get(method_name, envir = method_env, inherits = FALSE)
    }
    assign(
      method_name,
      function(value, name) {
        accessor_called <<- TRUE
        stop("HOSTILE_FACT_ACCESSOR_EXECUTED", call. = FALSE)
      },
      envir = method_env
    )
    for (id in c("hla", "marker_genes")) {
      hostile <- builder_task6_entry()
      if (is.null(hostile$dataset_profile$content[[id]])) {
        hostile$dataset_profile$content[[id]] <- builder_task6_fact()
      }
      hostile$dataset_profile$content[[id]] <- structure(
        hostile$dataset_profile$content[[id]],
        class = c("builder_hostile_fact", "list")
      )
      expect_invalid_fact(hostile)
    }
    if (had_method) {
      assign(method_name, prior_method, envir = method_env)
    } else {
      rm(list = method_name, envir = method_env)
    }
    expect_false(accessor_called)

    malformed_fields <- list(
      detected = c(TRUE, FALSE),
      valid = 1,
      normalized = "rows",
      diagnostics = list("boom"),
      requirements = NA_character_,
      page_candidates = list("hla"),
      attention = c(TRUE, FALSE)
    )
    for (field in names(malformed_fields)) {
      malformed <- builder_task6_entry()
      malformed$dataset_profile$content$hla[[field]] <-
        malformed_fields[[field]]
      expect_invalid_fact(malformed)
    }
  })
})

test_that("malformed optional-content settings fail as plan records", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    sources <- builder_task6_entry(full_ir_ready = TRUE)
    sources$settings$content_sources <- "unified_misc"
    expect_identical(
      builder_freeze_plan(list(sources), tempdir(), FALSE)$error_code,
      "invalid_content_sources"
    )

    invalid_names <- list(
      unnamed = list("unified_misc"),
      blank = setNames(list("unified_misc"), ""),
      duplicate = structure(
        list("unified_misc", "metadata"),
        names = c("immune_repertoire", "immune_repertoire")
      ),
      unknown = list(unknown_capability = "unified_misc"),
      known_but_not_selectable = list(marker_genes = "table")
    )
    for (label in names(invalid_names)) {
      malformed <- builder_task6_entry(full_ir_ready = TRUE)
      malformed$settings$content_sources <- invalid_names[[label]]
      expect_identical(
        builder_freeze_plan(
          list(malformed),
          tempdir(),
          FALSE
        )$error_code,
        "invalid_content_sources",
        info = label
      )
    }

    invalid_value <- builder_task6_entry()
    invalid_value$settings$content_sources <- list(
      immune_repertoire = ""
    )
    expect_identical(
      builder_freeze_plan(
        list(invalid_value),
        tempdir(),
        FALSE
      )$error_code,
      "invalid_content_source"
    )

    for (value in c("preserved", "filtered", "stored_only")) {
      unknown <- builder_task6_entry()
      unknown$settings$content_dispositions <- list(
        unknown_capability = value
      )
      state_error <- tryCatch(
        builder_dataset_state(unknown),
        builder_state_error = function(error) error
      )
      expect_s3_class(
        state_error,
        "builder_state_error"
      )
      expect_identical(
        state_error$code,
        "invalid_content_dispositions",
        info = value
      )
      expect_identical(
        builder_freeze_plan(
          list(unknown),
          tempdir(),
          FALSE
        )$error_code,
        "invalid_content_dispositions",
        info = value
      )
    }

    dispositions <- builder_task6_entry()
    dispositions$settings$content_dispositions <- "preserved"
    expect_identical(
      builder_freeze_plan(list(dispositions), tempdir(), FALSE)$error_code,
      "invalid_content_dispositions"
    )
  })
})

test_that("optional-content setting records are inert and non-dispatching", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    capture_state <- function(entry) {
      tryCatch(
        builder_dataset_state(entry),
        error = function(error) error
      )
    }
    capture_plan <- function(entry) {
      tryCatch(
        builder_freeze_plan(list(entry), tempdir(), FALSE),
        error = function(error) error
      )
    }
    expect_invalid <- function(entry, code, info) {
      state <- capture_state(entry)
      plan <- capture_plan(entry)
      expect_s3_class(state, "builder_state_error")
      expect_identical(state$code, code, info = info)
      expect_s3_class(plan, "builder_plan_failure")
      expect_identical(plan$error_code, code, info = info)
    }

    accessor_called <- FALSE
    method_env <- environment(builder_dataset_state)
    method_name <- "[[.evil_choices"
    had_method <- exists(method_name, envir = method_env, inherits = FALSE)
    if (had_method) {
      prior_method <- get(method_name, envir = method_env, inherits = FALSE)
    }
    assign(
      method_name,
      function(value, name, ...) {
        accessor_called <<- TRUE
        stop("EVIL_CHOICES_ACCESSOR_EXECUTED", call. = FALSE)
      },
      envir = method_env
    )

    cases <- list(
      content_dispositions = list(
        value = list(hla = "filtered"),
        code = "invalid_content_dispositions"
      ),
      content_sources = list(
        value = list(immune_repertoire = "unified_misc"),
        code = "invalid_content_sources"
      )
    )
    for (field in names(cases)) {
      classed <- builder_task6_entry(full_ir_ready = TRUE)
      classed$settings[[field]] <- structure(
        cases[[field]]$value,
        class = c("evil_choices", "list")
      )
      expect_invalid(classed, cases[[field]]$code, paste(field, "class"))
    }
    expect_false(accessor_called)

    if (had_method) {
      assign(method_name, prior_method, envir = method_env)
    } else {
      rm(list = method_name, envir = method_env)
    }

    for (field in names(cases)) {
      attributed <- builder_task6_entry(full_ir_ready = TRUE)
      record <- cases[[field]]$value
      attr(record, "probe") <- new.env(parent = emptyenv())
      attributed$settings[[field]] <- record
      expect_invalid(
        attributed,
        cases[[field]]$code,
        paste(field, "reference attribute")
      )
    }

    scalar_cases <- list(
      content_dispositions = list(
        id = "hla",
        value = "filtered",
        code = "invalid_content_disposition"
      ),
      content_sources = list(
        id = "immune_repertoire",
        value = "unified_misc",
        code = "invalid_content_source"
      )
    )
    for (field in names(scalar_cases)) {
      scalar_case <- scalar_cases[[field]]
      for (shape in c("class", "attribute")) {
        malformed <- builder_task6_entry(full_ir_ready = TRUE)
        value <- scalar_case$value
        if (identical(shape, "class")) {
          class(value) <- "evil_choice_value"
        } else {
          attr(value, "note") <- "not-plain"
        }
        malformed$settings[[field]] <- setNames(
          list(value),
          scalar_case$id
        )
        expect_invalid(
          malformed,
          scalar_case$code,
          paste(field, "scalar", shape)
        )
      }
    }
  })
})

test_that("plan input boundaries always return structured failures", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    expect_failure <- function(result, code, info = NULL) {
      expect_s3_class(result, "builder_plan_failure")
      expect_identical(result$error_code, code, info = info)
    }
    capture <- function(...) {
      tryCatch(
        builder_freeze_plan(...),
        error = function(error) error
      )
    }
    capture_state <- function(entry) {
      tryCatch(
        builder_dataset_state(entry),
        error = function(error) error
      )
    }

    invalid_entry <- capture(list(1), tempdir(), FALSE)
    expect_failure(invalid_entry, "invalid_entries", "entry")

    invalid_settings <- builder_task6_entry()
    invalid_settings$settings <- "settings"
    expect_failure(
      capture(list(invalid_settings), tempdir(), FALSE),
      "invalid_entries",
      "settings"
    )

    recommendation_entry <- builder_task6_entry()
    valid_recommendations <- recommendation_entry$settings$recommendations
    recommendation_cases <- list(
      outer_atomic = "recommendations",
      outer_classed = structure(
        valid_recommendations,
        class = "builder_recommendations"
      ),
      groups_atomic = utils::modifyList(
        valid_recommendations,
        list(groups = "groups")
      ),
      projections_atomic = utils::modifyList(
        valid_recommendations,
        list(projections = "projections")
      ),
      groups_included = utils::modifyList(
        valid_recommendations,
        list(groups = list(included = list("cluster")))
      ),
      projections_included = utils::modifyList(
        valid_recommendations,
        list(projections = list(included = 1))
      ),
      groups_classed = valid_recommendations
    )
    recommendation_cases$groups_classed$groups <- structure(
      list(included = "cluster"),
      class = "builder_group_recommendation"
    )
    reference_recommendations <- valid_recommendations
    reference_recommendations$groups$mutable <-
      new.env(parent = emptyenv())
    recommendation_cases$groups_reference <- reference_recommendations
    for (label in names(recommendation_cases)) {
      invalid_recommendations <- builder_task6_entry()
      invalid_recommendations$settings$recommendations <-
        recommendation_cases[[label]]
      state_result <- capture_state(invalid_recommendations)
      expect_s3_class(state_result, "builder_state_error")
      expect_identical(
        state_result$code,
        "invalid_recommendations",
        info = paste("state", label)
      )
      expect_failure(
        capture(list(invalid_recommendations), tempdir(), FALSE),
        "invalid_recommendations",
        paste("plan", label)
      )
    }

    invalid_name <- builder_task6_entry()
    invalid_name$settings$name <- c("Dataset", "Other")
    expect_failure(
      capture(list(invalid_name), tempdir(), FALSE),
      "invalid_dataset_name",
      "name"
    )

    null_analyses <- builder_task6_entry()
    null_analyses$settings$analyses <- NULL
    null_state <- builder_dataset_state(null_analyses)
    expect_identical(null_state$analyses, character())
    null_plan <- builder_freeze_plan(
      list(null_analyses),
      tempdir(),
      FALSE
    )
    expect_null(null_plan$error)
    expect_identical(null_plan$items[[1L]]$analyses, character())

    invalid_analyses <- list(
      list("marker_genes"),
      NA_character_,
      "unknown_analysis",
      c("marker_genes", "marker_genes"),
      new.env(parent = emptyenv())
    )
    for (index in seq_along(invalid_analyses)) {
      entry <- builder_task6_entry()
      entry$settings$analyses <- invalid_analyses[[index]]
      state_result <- tryCatch(
        builder_dataset_state(entry),
        builder_state_error = function(error) error,
        error = function(error) error
      )
      expect_s3_class(
        state_result,
        "builder_state_error"
      )
      expect_identical(
        state_result$code,
        "invalid_analyses",
        info = paste("state analyses", index)
      )
      expect_failure(
        capture(list(entry), tempdir(), FALSE),
        "invalid_analyses",
        paste("analyses", index)
      )
    }

    for (value in list(1, NA, c(TRUE, FALSE))) {
      expect_failure(
        capture(list(builder_task6_entry()), tempdir(), value),
        "invalid_make_app",
        "make_app"
      )
      expect_failure(
        capture(
          list(builder_task6_entry()),
          tempdir(),
          FALSE,
          overwrite = value
        ),
        "invalid_overwrite",
        "overwrite"
      )
    }

    expect_failure(
      capture(
        list(builder_task6_entry()),
        tempdir(),
        FALSE,
        app_options = structure(list(), class = "options")
      ),
      "invalid_app_options",
      "app_options"
    )
    for (value in list(NULL, 1, NA, c(TRUE, FALSE))) {
      expect_failure(
        capture(
          list(builder_task6_entry()),
          tempdir(),
          FALSE,
          app_options = list(show_upload_ui = value)
        ),
        "invalid_app_options",
        "show_upload_ui"
      )
    }
    for (value in list(
      NULL,
      1,
      NA_character_,
      c("a", "b"),
      ""
    )) {
      expect_failure(
        capture(
          list(builder_task6_entry()),
          tempdir(),
          FALSE,
          app_options = list(initial_dataset = value)
        ),
        "invalid_app_options",
        "initial_dataset"
      )
    }
    expect_failure(
      capture(
        list(builder_task6_entry()),
        tempdir(),
        FALSE,
        expected_prior_identity = "prior"
      ),
      "invalid_expected_prior_identity",
      "prior_identity"
    )

    malformed_metadata <- builder_task6_entry()
    malformed_metadata$settings$recommendations$metadata <- "metadata"
    expect_failure(
      capture(list(malformed_metadata), tempdir(), FALSE),
      "invalid_metadata_policy",
      "metadata_recommendation"
    )
  })
})

test_that("backends derive exact private sidecars and release targets", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    out_dir <- file.path(tempdir(), "task6-sidecar-targets")
    h5 <- builder_task6_entry()
    h5$settings$expression_backend <- "h5"
    h5$settings$sidecars <- NULL
    bpcells <- builder_task6_entry()
    bpcells$id <- "dataset-b"
    bpcells$settings$name <- "Dataset B"
    bpcells$settings$expression_backend <- "bpcells"
    bpcells$settings$sidecars <- NULL

    plan <- builder_freeze_plan(list(h5, bpcells), out_dir, FALSE)
    expect_null(plan$error)
    expected_sidecars <- c(
      paste0(
        tools::file_path_sans_ext(plan$items[[1L]]$filename),
        ".h5"
      ),
      paste0(
        tools::file_path_sans_ext(plan$items[[2L]]$filename),
        ".bpcells"
      )
    )
    expect_identical(plan$items[[1L]]$sidecars, expected_sidecars[[1L]])
    expect_identical(plan$items[[2L]]$sidecars, expected_sidecars[[2L]])
    expected_targets <- file.path(
      plan$out_dir,
      c(
        plan$items[[1L]]$filename,
        expected_sidecars[[1L]],
        plan$items[[2L]]$filename,
        expected_sidecars[[2L]]
      )
    )
    expect_identical(plan$targets, expected_targets)
    expect_identical(plan$output_release$targets, expected_targets)
    expect_true(all(expected_sidecars %in% plan$private_assets))

    embedded_mismatch <- builder_task6_entry()
    embedded_mismatch$settings$sidecars <- "forged.h5"
    expect_identical(
      builder_freeze_plan(
        list(embedded_mismatch),
        out_dir,
        FALSE
      )$error_code,
      "invalid_backend_sidecars"
    )

    h5_mismatch <- builder_task6_entry()
    h5_mismatch$settings$expression_backend <- "h5"
    h5_mismatch$settings$sidecars <- "wrong-dataset.h5"
    expect_identical(
      builder_freeze_plan(list(h5_mismatch), out_dir, FALSE)$error_code,
      "invalid_backend_sidecars"
    )
  })
})

test_that("output directories are scalar absolute release authorities", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    for (invalid in list(
      NA_character_,
      c(tempdir(), tempdir()),
      character(),
      42
    )) {
      failed <- builder_freeze_plan(
        list(builder_task6_entry()),
        invalid,
        FALSE
      )
      expect_s3_class(failed, "builder_plan_failure")
      expect_identical(
        failed$error_code,
        "invalid_output_directory"
      )
    }

    blank <- builder_freeze_plan(
      list(builder_task6_entry()),
      "  ",
      FALSE
    )
    expect_identical(blank$error_code, "missing_output_directory")

    root <- withr::local_tempdir()
    dir.create(file.path(root, "segment"))
    dir.create(file.path(root, "release"))
    supplied <- file.path(root, "segment", "..", "release")
    expected <- as.character(fs::path_norm(fs::path_abs(
      fs::path_expand(supplied)
    )))
    plan <- builder_freeze_plan(
      list(builder_task6_entry()),
      supplied,
      FALSE
    )

    expect_null(plan$error)
    expect_identical(plan$out_dir, expected)
    expect_identical(plan$output_release$directory, expected)
    expect_true(all(startsWith(
      plan$targets,
      paste0(expected, .Platform$file.sep)
    )))
    expect_identical(plan$targets, plan$output_release$targets)
  })
})

test_that("asset manifests encode dedupe and target conflicts", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    duplicate <- builder_task6_entry()
    duplicate$settings$viewer_bundle_assets <- c("readme.html", "readme.html")
    expect_identical(
      builder_freeze_plan(list(duplicate), tempdir(), FALSE)$error_code,
      "invalid_asset_manifest"
    )

    for (legacy_key in c("public_assets", "public_asset_claims")) {
      legacy <- builder_task6_entry()
      legacy$settings[[legacy_key]] <- "legacy.html"
      expect_identical(
        builder_freeze_plan(list(legacy), tempdir(), FALSE)$error_code,
        "invalid_entries"
      )
    }

    malformed <- builder_task6_entry()
    malformed$settings$private_assets <- c("", NA_character_)
    expect_identical(
      builder_freeze_plan(list(malformed), tempdir(), FALSE)$error_code,
      "invalid_asset_manifest"
    )

    generated_public <- builder_task6_entry()
    generated_public$settings$viewer_bundle_assets <- builder_item_filename(
      generated_public,
      1L,
      1L
    )
    expect_identical(
      builder_freeze_plan(list(generated_public), tempdir(), FALSE)$error_code,
      "asset_scope_conflict"
    )

    intersection <- builder_task6_entry()
    intersection$settings$viewer_bundle_assets <- "shared.txt"
    intersection$settings$private_assets <- "shared.txt"
    scope_conflict <- builder_freeze_plan(
      list(intersection),
      tempdir(),
      FALSE
    )
    expect_identical(scope_conflict$error_code, "asset_scope_conflict")
    expect_match(scope_conflict$error, "Viewer-bundle")
    expect_false(grepl("public", scope_conflict$error, fixed = TRUE))

    shared_claim <- builder_task6_asset_claim(
      source = "/source/histology.png",
      target = "shared.css",
      artifact = "spatial_image"
    )
    first <- builder_task6_entry()
    first$settings$viewer_bundle_assets <- list(shared_claim)
    second <- builder_task6_entry()
    second$id <- "dataset-b"
    second$settings$name <- "Dataset B"
    second$dataset_profile$source$location <- "another-dataset"
    second$dataset_profile$source$fingerprint <- "another-dataset:v1"
    second$settings$viewer_bundle_assets <- list(shared_claim)
    shared <- builder_freeze_plan(list(first, second), tempdir(), FALSE)
    expect_null(shared$error)
    expect_identical(shared$viewer_bundle_assets, "shared.css")
    expect_length(shared$viewer_bundle_asset_claims, 1L)
    expect_identical(
      shared$viewer_bundle_asset_claims[[1L]]$source,
      "/source/histology.png"
    )
    expect_identical(
      shared$viewer_bundle_asset_claims[[1L]]$target,
      "shared.css"
    )
    expect_identical(
      shared$viewer_bundle_asset_claims[[1L]]$artifact,
      "spatial_image"
    )

    conflicting <- second
    conflicting$settings$viewer_bundle_assets <- list(builder_task6_asset_claim(
      source = "/other/histology.png",
      target = "shared.css",
      artifact = "spatial_image"
    ))
    expect_identical(
      builder_freeze_plan(
        list(first, conflicting),
        tempdir(),
        FALSE
      )$error_code,
      "asset_target_conflict"
    )

    within_dataset <- builder_task6_entry()
    within_dataset$settings$viewer_bundle_assets <- list(
      shared_claim,
      builder_task6_asset_claim(
        source = "/other/histology.png",
        target = "shared.css",
        artifact = "spatial_image"
      )
    )
    expect_identical(
      builder_freeze_plan(
        list(within_dataset),
        tempdir(),
        FALSE
      )$error_code,
      "asset_target_conflict"
    )

    legacy_first <- builder_task6_entry()
    legacy_first$settings$viewer_bundle_assets <- "legacy.css"
    legacy_second <- builder_task6_entry()
    legacy_second$id <- "dataset-b"
    legacy_second$settings$name <- "Dataset B"
    legacy_second$settings$viewer_bundle_assets <- "legacy.css"
    expect_identical(
      builder_freeze_plan(
        list(legacy_first, legacy_second),
        tempdir(),
        FALSE
      )$error_code,
      "asset_target_conflict"
    )

    valid <- builder_task6_entry()
    valid$settings$viewer_bundle_assets <- "readme.html"
    valid$settings$private_assets <- "audit.json"
    plan <- builder_freeze_plan(list(valid), tempdir(), FALSE)
    expect_null(plan$error)
    expect_identical(plan$viewer_bundle_assets, "readme.html")
    expect_length(plan$viewer_bundle_asset_claims, 1L)
    expect_true(length(plan$private_asset_claims) >= 2L)
    expect_true(all(
      c("audit.json", plan$items[[1L]]$filename) %in%
        plan$private_assets
    ))
    expect_length(intersect(plan$viewer_bundle_assets, plan$private_assets), 0L)

    recursive_names <- function(value) {
      if (!is.list(value)) {
        return(character())
      }
      c(names(value), unlist(lapply(value, recursive_names), use.names = FALSE))
    }
    expect_false(any(
      c("public_assets", "public_asset_claims") %in% recursive_names(plan)
    ))
  })
})

test_that("typed asset claims normalize before comparison", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    canonical <- builder_task6_asset_claim(
      source = "/source/histology.png",
      target = "shared.css",
      artifact = "spatial_image"
    )
    attributed_artifact <- structure(
      "spatial_image",
      label = "display-only"
    )
    reordered <- structure(
      list(
        artifact = attributed_artifact,
        source = structure(
          "/source/histology.png",
          names = "display-only"
        ),
        target = structure("shared.css", label = "display-only")
      ),
      note = "display-only",
      class = c("builder_asset_claim", "list")
    )

    first <- builder_task6_entry()
    first$settings$viewer_bundle_assets <- list(canonical)
    second <- builder_task6_entry()
    second$id <- "dataset-b"
    second$settings$name <- "Dataset B"
    second$dataset_profile$source$location <- "another-dataset"
    second$dataset_profile$source$fingerprint <- "another-dataset:v1"
    second$settings$viewer_bundle_assets <- list(reordered)

    plan <- builder_freeze_plan(list(first, second), tempdir(), FALSE)
    expect_null(plan$error)
    expect_identical(plan$viewer_bundle_assets, "shared.css")
    expect_length(plan$viewer_bundle_asset_claims, 1L)
    expect_identical(plan$viewer_bundle_asset_claims[[1L]], canonical)

    unsafe <- builder_task6_entry()
    unsafe_claim <- canonical
    attr(unsafe_claim, "mutable") <- new.env(parent = emptyenv())
    unsafe$settings$viewer_bundle_assets <- list(unsafe_claim)
    expect_identical(
      builder_freeze_plan(list(unsafe), tempdir(), FALSE)$error_code,
      "invalid_asset_manifest"
    )

    extra <- builder_task6_entry()
    extra_claim <- canonical
    extra_claim$note <- "unexpected"
    extra$settings$viewer_bundle_assets <- list(extra_claim)
    expect_identical(
      builder_freeze_plan(list(extra), tempdir(), FALSE)$error_code,
      "invalid_asset_manifest"
    )
  })
})

test_that("only owned production snapshots are available to plans", {
  skip_if_not_installed("SeuratObject")

  local({
    builder_repo_source("io.R")
    builder_repo_source("adapters.R")
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    root <- withr::local_tempdir()
    snapshot <- builder_snapshot_seurat(
      builder_immune_fixture_object(),
      file.path(root, "real.snapshot"),
      available_bytes = 2^40
    )
    withr::defer(.builder_snapshot_release(snapshot))
    expect_type(snapshot, "list")
    expect_false(inherits(snapshot, "builder_snapshot_identity"))

    real <- builder_task6_entry()
    real$snapshot <- snapshot
    real_plan <- builder_freeze_plan(list(real), tempdir(), FALSE)
    expect_null(real_plan$error)
    expect_true(
      real_plan$source_snapshot_identities[["dataset-a"]]$available
    )
    expect_identical(
      real_plan$source_snapshot_identities[["dataset-a"]]$object_md5,
      snapshot$object_md5
    )

    forged <- snapshot
    forged$owner_token <- paste0(forged$owner_token, "-forged")
    forged <- structure(
      forged,
      class = c("builder_snapshot_identity", "list")
    )
    forged_entry <- builder_task6_entry()
    forged_entry$snapshot <- forged
    forged_plan <- builder_freeze_plan(
      list(forged_entry),
      tempdir(),
      FALSE
    )
    expect_null(forged_plan$error)
    expect_false(
      forged_plan$source_snapshot_identities[["dataset-a"]]$available
    )

    missing <- builder_task6_entry()
    missing$snapshot_identity <- builder_task6_snapshot_identity()
    missing_plan <- builder_freeze_plan(list(missing), tempdir(), FALSE)
    expect_null(missing_plan$error)
    expect_false(
      missing_plan$source_snapshot_identities[["dataset-a"]]$available
    )

    shadowed <- builder_task6_entry()
    shadowed$snapshot_identity <- builder_task6_snapshot_identity()
    shadowed$snapshot <- snapshot
    shadowed_plan <- builder_freeze_plan(
      list(shadowed),
      tempdir(),
      FALSE
    )
    expect_true(
      shadowed_plan$source_snapshot_identities[["dataset-a"]]$available
    )
    expect_null(
      shadowed_plan$error
    )
    expect_identical(
      shadowed_plan$source_snapshot_identities[["dataset-a"]]$object_md5,
      snapshot$object_md5
    )
  })
})
