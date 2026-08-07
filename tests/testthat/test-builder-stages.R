builder_repo_source("ui/inspect_stage.R")
builder_repo_source("ui/core_stage.R")
builder_repo_source("ui/enhance_stage.R")
builder_repo_source("ui/review_stage.R")
builder_repo_source("ui/build_status.R")

builder_stage_html <- function(value) {
  htmltools::renderTags(value)$html
}

test_that("Inspect leads with attention and hides complete diagnostics", {
  model <- list(
    summary = c("1,200 cells", "4,500 genes"),
    attention = c("Metadata column sample has missing values"),
    blockers = c("Projection coordinates are incomplete"),
    detected = c("RNA assay", "UMAP projection"),
    diagnostics = c("source fingerprint abc123")
  )

  html <- builder_stage_html(builder_inspect_stage_ui("inspect", model))

  expect_match(html, "Needs attention", fixed = TRUE)
  expect_match(html, model$attention[[1L]], fixed = TRUE)
  expect_match(html, model$blockers[[1L]], fixed = TRUE)
  expect_match(html, "View all detected content", fixed = TRUE)
  expect_match(html, "Technical diagnostics", fixed = TRUE)
  expect_match(html, "<details", fixed = TRUE)
  expect_false(grepl("export|Build App", html, ignore.case = TRUE))
})

test_that("Inspect detected content comes from manifest readiness", {
  model <- builder_inspect_model(
    profile = list(n_cells = 1200L, n_genes = 4500L, internal_cache = TRUE),
    state = list(
      attention_ids = character(),
      blocking_ids = character(),
      manifest = list(
        expression = list(
          id = "expression",
          status = "valid",
          summary = "Expression matrix is Viewer-ready."
        ),
        spatial = list(
          id = "spatial",
          status = "not_applicable",
          summary = "No spatial sections were detected."
        )
      )
    ),
    format = "rds",
    dataset_id = "dataset-a"
  )
  html <- builder_stage_html(builder_inspect_stage_ui("inspect", model))

  expect_match(html, "expression — ready", fixed = TRUE)
  expect_match(html, "spatial — not applicable", fixed = TRUE)
  expect_match(html, "No spatial sections were detected", fixed = TRUE)
  expect_false(grepl("internal_cache", html, fixed = TRUE))
})

test_that("Core keeps technical controls advanced and metadata visible", {
  model <- list(
    id = "dataset-a",
    name = "PBMC",
    organism = "hg",
    organism_choices = c("hg", "mm"),
    default_group = "cluster",
    group_choices = c("cluster", "sample"),
    default_projection = "umap",
    projection_choices = c("umap", "pca"),
    assay = "RNA",
    assay_choices = c("RNA", "SCT"),
    layer = "data",
    layer_choices = c("data", "counts"),
    nUMI = "nCount_RNA",
    nUMI_choices = "nCount_RNA",
    nGene = "nFeature_RNA",
    nGene_choices = "nFeature_RNA",
    backend = "embedded",
    backend_choices = c("embedded", "h5"),
    metadata_attention = "sample contains missing values"
  )

  html <- builder_stage_html(builder_core_stage_ui("core", model))

  expect_match(html, "Dataset name", fixed = TRUE)
  expect_match(html, "Organism", fixed = TRUE)
  expect_match(html, "Default group", fixed = TRUE)
  expect_match(html, "Default projection", fixed = TRUE)
  expect_match(html, model$metadata_attention, fixed = TRUE)
  expect_match(html, "Advanced technical settings", fixed = TRUE)
  expect_match(html, "Assay", fixed = TRUE)
  expect_match(html, "Expression backend", fixed = TRUE)
  expect_match(html, 'id="core-rendered_for"', fixed = TRUE)
  expect_match(html, 'value="dataset-a"', fixed = TRUE)
})

test_that("Enhance renders only relevant opt-in modules and consequences", {
  model <- list(
    id = "dataset-a",
    modules = list(
      list(
        id = "marker_genes",
        label = "Marker genes",
        relevant = TRUE,
        blocked = FALSE,
        selected = FALSE,
        enabled_pages = "marker genes",
        replacement_policy = "Replace the existing marker result.",
        skip_consequence = "The marker page stays unavailable.",
        consequence = "Adds ranked marker tables to the Viewer.",
        cost = "Can take several minutes and increases release size.",
        network = "No network access.",
        prerequisite = "Requires a supported differential-expression method."
      ),
      list(
        id = "enriched_pathways",
        label = "Enriched pathways",
        relevant = TRUE,
        blocked = TRUE,
        blocked_reason = "Select Marker genes first",
        selected = FALSE,
        enabled_pages = "enriched pathways",
        replacement_policy = "Replace the existing enrichment result.",
        skip_consequence = "The enrichment page stays unavailable.",
        consequence = "Adds pathway enrichment.",
        cost = "Network-dependent",
        network = "Network access is required.",
        prerequisite = "Requires marker_genes first."
      )
    ),
    attachments = list(
      tables = list(
        label = "Supplementary tables",
        enabled_pages = "extra material",
        cost = "Reads one bounded delimited file.",
        network = "No network access.",
        prerequisite = "Requires a readable CSV or TSV file.",
        selected = "Differential expression",
        replacement_policy = "Replace a table only when its display name matches.",
        skip_consequence = "Skipped tables will not appear in Extra material."
      ),
      histology = list(
        label = "Histology images",
        enabled_pages = "spatial",
        relevant = TRUE,
        cost = "Image encoding and alignment.",
        network = "No network access.",
        prerequisite = "Requires spatial sections and coordinates.",
        sections = c("section-a", "section-b"),
        selected = "section-a",
        replacement_policy = "One saved image per tissue section.",
        skip_consequence = "Sections without an image keep points-only spatial views."
      )
    ),
    auto_retained = list(
      list(
        id = "immune_repertoire",
        label = "Immune repertoire",
        enabled_pages = "immune repertoire",
        replacement_policy = "Existing validated content is retained.",
        skip_consequence = "Not optional: frozen valid content stays in the CRB."
      )
    )
  )

  html <- builder_stage_html(builder_enhance_stage_ui("enhance", model))

  expect_match(html, "Marker genes", fixed = TRUE)
  expect_match(html, "Adds ranked marker tables", fixed = TRUE)
  expect_match(html, "several minutes", fixed = TRUE)
  expect_match(html, "No network access", fixed = TRUE)
  expect_match(html, "Requires a supported", fixed = TRUE)
  expect_match(html, "Enabled page: marker genes", fixed = TRUE)
  expect_match(html, "Replace the existing marker result", fixed = TRUE)
  expect_match(html, "The marker page stays unavailable", fixed = TRUE)
  expect_match(html, "Select Marker genes first", fixed = TRUE)
  expect_match(html, "disabled", fixed = TRUE)
  expect_match(html, "Optional attachments", fixed = TRUE)
  expect_match(html, "Supplementary tables", fixed = TRUE)
  expect_match(html, "Histology images", fixed = TRUE)
  expect_match(html, "Enabled page: extra material", fixed = TRUE)
  expect_match(html, "Enabled page: spatial", fixed = TRUE)
  expect_match(html, "Replacement policy", fixed = TRUE)
  expect_match(html, "Skipped tables will not appear", fixed = TRUE)
  expect_match(html, "Auto-retained content", fixed = TRUE)
  expect_match(html, "Immune repertoire", fixed = TRUE)
  expect_match(html, 'id="enhance-table_path"', fixed = TRUE)
  expect_match(html, 'id="enhance-add_table"', fixed = TRUE)
  expect_match(html, 'id="enhance-tables_to_retain"', fixed = TRUE)
  expect_match(html, 'id="enhance-active_slice"', fixed = TRUE)
  expect_match(html, 'id="enhance-attach_image"', fixed = TRUE)
  expect_match(html, 'id="enhance-histology_to_retain"', fixed = TRUE)
})

test_that("Enhance model derives attachments and retained content from state", {
  model <- builder_enhance_model(
    id = "dataset-a",
    profile = list(images = c("section-a", "section-b")),
    state = list(
      manifest = list(
        immune = list(
          id = "immune_repertoire",
          status = "valid",
          disposition = "preserved",
          summary = "Immune repertoire",
          pages = "immune_repertoire"
        ),
        absent = list(
          id = "spatial",
          status = "not_applicable",
          disposition = "rejected",
          summary = "No spatial data",
          pages = character()
        )
      )
    ),
    settings = list(
      tables = list(markers = list(table = data.frame())),
      images = list(`section-a` = list(uri = "data:image/png;base64,AA=="))
    ),
    modules = list()
  )

  expect_identical(model$attachments$tables$selected, "markers")
  expect_identical(
    model$attachments$histology$sections,
    c("section-a", "section-b")
  )
  expect_identical(model$attachments$histology$selected, "section-a")
  expect_identical(
    vapply(model$auto_retained, `[[`, character(1), "id"),
    "immune_repertoire"
  )
  expect_match(
    model$auto_retained[[1L]]$replacement_policy,
    "preserved",
    fixed = TRUE
  )

  settings <- list(
    tables = list(first = 1, second = 2),
    images = list(`section-a` = 1, `section-b` = 2)
  )
  settings <- builder_enhance_retain(settings, "tables", "second")
  settings <- builder_enhance_retain(settings, "images", "section-a")
  expect_identical(names(settings$tables), "second")
  expect_identical(names(settings$images), "section-a")

  no_spatial <- builder_enhance_model(
    id = "dataset-b",
    profile = list(images = character()),
    state = list(
      manifest = list(
        spatial = list(
          id = "spatial",
          status = "not_applicable",
          disposition = "rejected"
        )
      )
    ),
    settings = list(tables = list(), images = list()),
    modules = list()
  )
  expect_false(no_spatial$attachments$histology$relevant)
  expect_false(grepl(
    "Histology images",
    builder_stage_html(builder_enhance_stage_ui("enhance", no_spatial)),
    fixed = TRUE
  ))
})

test_that("Enhance distinguishes intrinsic absence from dependency blocking", {
  percent <- list(id = "percent_mt_ribo")
  enrichr <- list(id = "enriched_pathways")

  intrinsic <- builder_enhance_analysis_applicability(
    percent,
    organism = "other",
    blocked_reason = "Human and mouse only"
  )
  dependency <- builder_enhance_analysis_applicability(
    enrichr,
    organism = "hg",
    blocked_reason = "Select Marker genes first"
  )
  current_other <- builder_enhance_analysis_profile(
    list(organism_guess = "hg"),
    "other"
  )
  current_hg <- builder_enhance_analysis_profile(
    list(organism_guess = "other"),
    "hg"
  )

  expect_false(intrinsic$relevant)
  expect_false(intrinsic$blocked)
  expect_true(dependency$relevant)
  expect_true(dependency$blocked)
  expect_identical(dependency$blocked_reason, "Select Marker genes first")
  expect_identical(current_other$organism_guess, "other")
  expect_identical(current_hg$organism_guess, "hg")

  html <- builder_stage_html(builder_enhance_stage_ui(
    "enhance",
    list(
      id = "dataset-a",
      modules = list(
        c(list(id = "percent_mt_ribo", label = "Percent MT/Ribo"), intrinsic),
        c(list(id = "enriched_pathways", label = "Enrichr"), dependency)
      ),
      attachments = list(
        tables = list(relevant = TRUE),
        histology = list(relevant = FALSE)
      ),
      auto_retained = list()
    )
  ))
  expect_false(grepl("Percent MT/Ribo", html, fixed = TRUE))
  expect_match(html, "Enrichr", fixed = TRUE)
  expect_match(html, "Select Marker genes first", fixed = TRUE)
  expect_match(html, "disabled", fixed = TRUE)
})

test_that("saved histology Remove targets the mounted Enhance observer", {
  html <- builder_stage_html(builder_enhance_saved_image_ui(
    "enhance",
    "section-a",
    2L
  ))
  expect_match(html, 'id="enhance-drop_image"', fixed = TRUE)

  app <- readLines(
    builder_profile_inst_path("builder", "app.R"),
    warn = FALSE
  )
  expect_true(any(grepl(
    'observeEvent(input[["enhance-drop_image"]]',
    app,
    fixed = TRUE
  )))
  expect_false(any(grepl(
    'actionButton("drop_image"',
    app,
    fixed = TRUE
  )))
})

builder_stage_frozen_plan <- function(make_app = TRUE) {
  structure(
    list(
      revision = 17L,
      readiness = "ready",
      dataset_order = c("dataset-b", "dataset-a"),
      make_app = make_app,
      app_contract_version = if (make_app) 1L else NULL,
      items = list(
        list(
          id = "dataset-b",
          name = "Dataset B",
          filename = "01-dataset-b.crb",
          organism = "hg",
          assay = "RNA",
          layer = "data",
          groups = c("cluster", "sample"),
          included_groups = c("cluster", "sample"),
          reductions = c("umap", "pca"),
          included_projections = "umap",
          analyses = "marker_genes",
          analysis_dependency_graph = list(
            marker_genes = list(id = "marker_genes", dependencies = character())
          ),
          artifact_identity = list(
            schema_version = 2L,
            cells = c("cell-a", "cell-b"),
            features = c("gene-a", "gene-b", "gene-c"),
            spatial_sections = "section-a"
          ),
          cell_count = 2L,
          gene_count = 3L,
          histology_coverage = list(
            sections = "section-a",
            with_histology = "section-a",
            missing_histology = character()
          ),
          estimated_runtime = "minutes",
          estimated_disk_bytes = 4096,
          tables = list(differential_expression = list()),
          images = list(section_a = list()),
          colors = list(cluster = c(A = "#111111")),
          nUMI = "nCount_RNA",
          nGene = "nFeature_RNA",
          default_group = "cluster",
          default_projection = "umap",
          expression_backend = "h5",
          sidecars = "01-dataset-b.h5",
          metadata_policy = list(
            included = c("cluster", "sample"),
            excluded = "patient_name"
          ),
          nomenclature = "HGNC",
          readiness = "ready",
          acknowledgements = "Marker genes replace the existing method"
        ),
        list(
          id = "dataset-a",
          name = "Dataset A",
          filename = "02-dataset-a.crb",
          organism = "mm",
          groups = "cell_type",
          reductions = "umap",
          analyses = character(),
          colors = list(cell_type = c(T = "#222222")),
          expression_backend = "embedded",
          sidecars = character(),
          metadata_policy = list(included = "cell_type")
        )
      ),
      manifest = list(immune_repertoire = list(status = "valid")),
      viewer_page_expectations = list(
        list(pages = "overview"),
        list(pages = "overview")
      ),
      viewer_bundle_assets = c("spatial-assets/image.png"),
      private_assets = c("private-data/01-dataset-b.crb"),
      acknowledgements = list("Filtered orphan repertoire rows"),
      app_options = list(
        enabled = make_app,
        show_upload_ui = FALSE,
        initial_dataset = "dataset-b",
        initial_dataset_mode = "automatic",
        welcome_message = "Welcome, lab team!",
        point_size = list(overview_projection_point_size = 4),
        variable_to_compare = TRUE,
        host = "127.0.0.1",
        port = 8080L,
        max_request_size = 8000,
        display_mode = "normal",
        launch_browser = TRUE
      ),
      output_release = list(
        directory = "/private/host/output",
        overwrite = FALSE,
        replacement_policy = "preserve_existing",
        estimated_runtime = "minutes",
        estimated_disk_bytes = 4096,
        targets = c(
          "/private/host/output/01-dataset-b.crb",
          "/private/host/output/01-dataset-b.h5",
          "/private/host/output/02-dataset-a.crb",
          "/private/host/output/cerebro_app"
        )
      )
    ),
    class = c("builder_build_plan", "list")
  )
}

test_that("Review model is an exact frozen-plan projection", {
  plan <- builder_stage_frozen_plan()
  model <- builder_review_model(plan)

  expect_identical(model$revision, plan$revision)
  expect_identical(model$private_assets, plan$private_assets)
  expect_identical(model$viewer_bundle_assets, plan$viewer_bundle_assets)
  expect_identical(model$dataset_order, plan$dataset_order)
  expect_identical(model$release_members, basename(plan$output_release$targets))

  plan$revision <- 99L
  plan$private_assets <- "changed"
  expect_identical(model$revision, 17L)
  expect_identical(model$private_assets, "private-data/01-dataset-b.crb")
})

test_that("Review distinguishes CRBs from private App contract", {
  crbs <- builder_review_model(builder_stage_frozen_plan(FALSE))
  expect_identical(crbs$artifact_mode, "crbs_only")

  app <- builder_review_model(builder_stage_frozen_plan(TRUE))
  html <- builder_stage_html(builder_review_stage_ui("review", app))

  expect_identical(app$artifact_mode, "crbs_and_private_app")
  expect_match(html, "App contract 1", fixed = TRUE)
  expect_match(html, "Dataset B → Dataset A", fixed = TRUE)
  expect_match(html, "Dataset B", fixed = TRUE)
  expect_match(html, "automatic", fixed = TRUE)
  expect_match(html, "Uploads disabled", fixed = TRUE)
  expect_match(html, "Palettes", fixed = TRUE)
  expect_match(html, "Welcome, lab team!", fixed = TRUE)
  expect_match(html, "Point size: 4", fixed = TRUE)
  expect_match(html, "Variable comparison: enabled", fixed = TRUE)
  expect_match(html, "Host: 127.0.0.1", fixed = TRUE)
  expect_match(html, "Port: 8080", fixed = TRUE)
  expect_match(html, "Request limit: 8000 MB", fixed = TRUE)
  expect_match(html, "Display mode: normal", fixed = TRUE)
  expect_match(html, "Launch browser: enabled", fixed = TRUE)
  expect_match(html, "Planned payload members", fixed = TRUE)
  expect_no_match(html, "Exact release members", fixed = TRUE)
  expect_match(html, "01-dataset-b.crb", fixed = TRUE)
  expect_match(html, "01-dataset-b.h5", fixed = TRUE)
  expect_match(html, "immune_repertoire", fixed = TRUE)
  expect_match(html, "overview", fixed = TRUE)
  expect_match(html, "#111111", fixed = TRUE)
  expect_match(html, "assay: RNA", fixed = TRUE)
  expect_match(html, "layer: data", fixed = TRUE)
  expect_match(html, "default_group: cluster", fixed = TRUE)
  expect_match(html, "default_projection: umap", fixed = TRUE)
  expect_match(html, "nUMI: nCount_RNA", fixed = TRUE)
  expect_match(html, "nGene: nFeature_RNA", fixed = TRUE)
  expect_match(html, "metadata_policy / excluded: patient_name", fixed = TRUE)
  expect_match(html, "analyses: marker_genes", fixed = TRUE)
  expect_match(html, "Cell count: 2", fixed = TRUE)
  expect_match(html, "Gene count: 3", fixed = TRUE)
  expect_match(html, "Analysis dependency graph", fixed = TRUE)
  expect_match(html, "Artifact identity", fixed = TRUE)
  expect_match(html, "Histology coverage", fixed = TRUE)
  expect_match(html, "Output directory: /private/host/output", fixed = TRUE)
  expect_match(html, "Replacement policy: preserve_existing", fixed = TRUE)
  expect_match(html, "Estimated runtime: minutes", fixed = TRUE)
  expect_match(html, "Estimated disk: 4096 bytes", fixed = TRUE)
  expect_match(html, "Acknowledged warnings", fixed = TRUE)
  expect_match(html, "Marker genes replace the existing method", fixed = TRUE)
  expect_match(html, "spatial-assets/image.png", fixed = TRUE)
  expect_match(html, "not directly downloadable", fixed = TRUE)
  expect_match(html, "duplicated", ignore.case = TRUE)
  expect_match(html, "not directly downloadable", fixed = TRUE)
  expect_match(html, "no HTTP-public asset class", fixed = TRUE)
})

test_that("Build status has four top-level types and warning Success variant", {
  success_result <- builder_result_success(
    published = TRUE,
    built = "/release/dataset.crb",
    warnings = "One optional analysis was skipped"
  )
  decision_result <- builder_result_needs_decision("Choose whether to retry.")
  failure_result <- builder_result_failure("failed")
  recovery_result <- builder_result_recovery_required(
    "Restore the backup manually."
  )
  success <- builder_build_status_model(success_result)
  decision <- builder_build_status_model(decision_result)
  failure <- builder_build_status_model(failure_result)
  recovery <- builder_build_status_model(recovery_result)

  expect_s3_class(success_result, "builder_result_success")
  expect_identical(success_result$state, "success")
  expect_identical(success$type, "success")
  expect_identical(success$variant, "warnings")
  expect_identical(decision$type, "needs_decision")
  expect_identical(failure$type, "failure")
  expect_identical(recovery$type, "recovery_required")
  expect_error(builder_build_status_model(list(error = "legacy")), "typed")
})

test_that("release recovery evidence produces a recovery-required result", {
  recovery <- list(
    state = "recovery_required",
    message = "Restore the preserved backup before retrying.",
    backup = "release.backup"
  )
  mapped <- builder_release_error_result(
    "Publishing failed.",
    "/release",
    .recovery = function(target) {
      expect_identical(target, "/release")
      recovery
    }
  )
  ordinary <- builder_release_error_result(
    "Validation failed.",
    "/release",
    .recovery = function(target) list(state = "ready")
  )

  expect_s3_class(mapped, "builder_result_recovery_required")
  expect_identical(mapped$state, "recovery_required")
  expect_identical(mapped$recovery, recovery)
  expect_s3_class(ordinary, "builder_result_failure")
})

test_that("a real publish restore failure maps to recovery required", {
  local({
    builder_repo_source("publish.R")
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old", file.path(target, "dataset.crb"))
    handle <- builder_prepare_release(
      target,
      "build-result-recovery",
      builder_release_identity(target)
    )
    writeLines("new", file.path(handle$stage, "dataset.crb"))
    moves <- 0L
    fail_publish_and_restore <- function(from, to) {
      moves <<- moves + 1L
      if (moves %in% c(2L, 3L)) {
        return(FALSE)
      }
      file.rename(from, to)
    }
    failure <- tryCatch(
      builder_publish_release(handle, .move = fail_publish_and_restore),
      error = function(error) error
    )

    mapped <- builder_release_error_result(
      conditionMessage(failure),
      target,
      .recovery = builder_discover_recovery
    )

    expect_s3_class(mapped, "builder_result_recovery_required")
    expect_identical(mapped$recovery$state, "recovery_required")
    expect_true(dir.exists(mapped$recovery$backup))
  })
})

test_that("Open App requires a verified published final App directory", {
  no_app <- builder_build_status_ui(builder_result_success(
    published = TRUE,
    built = "/release/dataset.crb"
  ))
  verified_app <- builder_build_status_ui(builder_result_success(
    published = TRUE,
    built = "/release/dataset.crb",
    app_dir = "/release/cerebro_app",
    app_verified = TRUE,
    report_path = "/release/build-report.json"
  ))

  expect_false(grepl("Open App", builder_stage_html(no_app), fixed = TRUE))
  verified_html <- builder_stage_html(verified_app)
  expect_match(verified_html, "Open App", fixed = TRUE)
  expect_match(verified_html, "Reveal Folder", fixed = TRUE)
  expect_match(verified_html, "Copy Path", fixed = TRUE)
  expect_match(verified_html, "Copy Report", fixed = TRUE)
  expect_match(verified_html, 'data-path="/release/cerebro_app"', fixed = TRUE)
  expect_match(
    verified_html,
    'data-report="/release/build-report.json"',
    fixed = TRUE
  )
})

test_that("result actions execute through injected platform boundaries", {
  opened <- revealed <- copied <- character()
  app <- builder_result_success(
    published = TRUE,
    built = "/release/dataset.crb",
    app_dir = "/release/cerebro_app",
    app_verified = TRUE,
    report_path = "/release/build-report.json"
  )

  expect_true(builder_open_final_app(app, .open = function(path) {
    opened <<- path
    TRUE
  }))
  expect_true(builder_reveal_release(app, .reveal = function(path) {
    revealed <<- path
    TRUE
  }))
  expect_true(builder_copy_result_path(app, "release", .copy = function(value) {
    copied <<- value
    TRUE
  }))
  expect_identical(opened, "/release/cerebro_app")
  expect_identical(revealed, "/release")
  expect_identical(copied, "/release")
  expect_error(
    builder_open_final_app(builder_result_success(published = TRUE)),
    "verified final App"
  )
})

test_that("typed Review controls expose only accepted App options", {
  options <- builder_review_options(
    welcome_message = "Welcome, team!",
    point_size = 5,
    variable_to_compare = TRUE,
    host = "127.0.0.1",
    port = 4242L,
    max_request_size = 512,
    display_mode = "normal",
    launch_browser = FALSE,
    show_upload_ui = FALSE
  )

  expect_s3_class(options, "builder_review_options")
  frozen <- builder_review_options_for_plan(
    options,
    initial_dataset = "dataset-a"
  )
  expect_identical(
    names(frozen),
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
  )
  expect_identical(
    frozen$point_size,
    list(overview_projection_point_size = 5)
  )
  expect_error(builder_review_options(port = 0), "Review options")

  html <- builder_stage_html(builder_review_controls_ui("review", options))
  for (label in c(
    "Welcome message",
    "Point size",
    "Variable to compare",
    "Host",
    "Port",
    "Request size",
    "Display mode",
    "Launch browser",
    "Allow uploads"
  )) {
    expect_match(html, label, fixed = TRUE)
  }
})

test_that("build completion preserves decisions and always has an idle ack path", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)

  release <- list(handle = list(target = "/release"))
  decision <- list(
    state = "needs_decision",
    publishable = FALSE,
    error = "Choose analyses to retry.",
    failed_analyses = "marker_genes",
    retry_closure = c("marker_genes", "enriched_pathways")
  )
  settled <- app_env$builder_app_settle_release(
    release,
    decision,
    .abort = function(handle) list(aborted = TRUE)
  )
  expect_s3_class(settled, "builder_result_needs_decision")
  expect_identical(settled$failed_analyses, "marker_genes")
  expect_identical(
    settled$retry_closure,
    c("marker_genes", "enriched_pathways")
  )
  expect_identical(
    app_env$builder_app_build_action(settled, "build-a")$type,
    "needs_decision"
  )

  cleanup_failure <- app_env$builder_app_settle_release(
    release,
    decision,
    .abort = function(handle) stop("stage cleanup failed"),
    .release_error = function(message, target) {
      expect_match(message, "cleanup failed", fixed = TRUE)
      expect_identical(target, "/release")
      app_env$builder_result_recovery_required(
        "Restore the preserved stage.",
        recovery = list(state = "recovery_required")
      )
    }
  )
  expect_s3_class(cleanup_failure, "builder_result_recovery_required")

  recovery <- app_env$builder_result_recovery_required("Restore the backup.")
  recovery_action <- app_env$builder_app_build_action(recovery, "build-a")
  expect_identical(recovery_action$type, "fail")
  expect_match(recovery_action$error, "Restore the backup", fixed = TRUE)

  protocol <- app_env$builder_request_protocol("worker-a")
  queued <- app_env$builder_enqueue(
    protocol,
    app_env$builder_command(
      "build",
      "session",
      payload = list(id = "build-a")
    )
  )
  dispatched <- app_env$builder_protocol_dispatch(queued)
  completed <- app_env$builder_protocol_complete(
    dispatched$protocol,
    app_env$builder_worker_response(
      dispatched$request,
      list(state = "recovery_required", error = "Restore the backup.")
    )
  )
  expect_length(completed$protocol$awaiting_ack, 1L)
  acknowledged <- app_env$builder_app_acknowledge_build(
    completed$protocol,
    dispatched$request$request_id
  )
  expect_length(acknowledged$awaiting_ack, 0L)
  expect_identical(acknowledged$build_status, "idle")

  app <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
  expect_match(app, "on.exit(", fixed = TRUE)
  expect_match(app, "builder_app_acknowledge_build", fixed = TRUE)
})

test_that("Review inputs fail explicitly and recover without rebuilding inputs", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }

  shiny::testServer(app_env$server, {
    invalid <- list(
      welcome_message = "Welcome",
      point_size = 5,
      variable_to_compare = FALSE,
      host = "127.0.0.1",
      port = 0,
      max_request_size = 8000,
      display_mode = "normal",
      launch_browser = TRUE,
      show_upload_ui = FALSE
    )
    expect_false(validate_review_inputs(invalid))
    expect_false(review_validation()$ok)
    expect_match(review_validation()$error, "Review options", fixed = TRUE)
    invalid_plan <- frozen_review_plan()
    expect_identical(invalid_plan$error_code, "invalid_review_options")
    expect_false(app_env$builder_review_can_build(invalid_plan))

    invalid$port <- 8080L
    expect_true(validate_review_inputs(invalid))
    expect_true(review_validation()$ok)
    expect_s3_class(review_options(), "builder_review_options")
    expect_identical(frozen_review_plan()$error_code, "empty_release")
  })

  lines <- readLines("app.R", warn = FALSE)
  app_block <- function(start, finish) {
    first <- grep(start, lines, fixed = TRUE)[1L]
    last <- grep(finish, lines, fixed = TRUE)
    last <- last[last > first][1L]
    paste(lines[first:(last - 1L)], collapse = "\n")
  }
  workbench <- app_block(
    "output$workbench <- renderUI({",
    "output$review_stage <- renderUI({"
  )
  review_stage <- app_block(
    "output$review_stage <- renderUI({",
    "output$actionbar <- renderUI({"
  )
  actionbar <- app_block(
    "output$actionbar <- renderUI({",
    "output$review_action_summary <- renderUI({"
  )
  expect_match(workbench, "entry <- isolate(entry_of(id))", fixed = TRUE)
  expect_false(grepl("frozen_review_plan()", workbench, fixed = TRUE))
  expect_match(workbench, 'uiOutput("review_stage")', fixed = TRUE)
  expect_match(workbench, "builder_review_controls_ui", fixed = TRUE)
  expect_match(review_stage, "frozen_review_plan()", fixed = TRUE)
  expect_false(grepl("builder_review_controls_ui", review_stage, fixed = TRUE))
  expect_match(actionbar, 'uiOutput("review_action_summary"', fixed = TRUE)
  expect_false(grepl("review_report()", actionbar, fixed = TRUE))
})

test_that("workbench identity ignores settings writes but tracks selection", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }

  shiny::testServer(app_env$server, {
    entry <- function(id) {
      list(
        id = id,
        revision = 0L,
        snapshot = list(
          path = paste0("/private/", id),
          owner_token = paste0("owner-", id),
          object_md5 = strrep(substr(id, nchar(id), nchar(id)), 32L)
        ),
        profile = list(marker = id),
        settings = list(name = id)
      )
    }
    use_state_only_fixture(list(entry("dataset-a"), entry("dataset-b")))
    session$flushReact()

    renders <- 0L
    tracker <- observe({
      current()
      renders <<- renders + 1L
    })
    withr::defer(tracker$destroy())
    session$flushReact()
    baseline <- renders

    changed <- sets()[[1L]]
    changed$settings$name <- "Dataset A renamed"
    expect_true(replace_entry(changed))
    session$flushReact()
    expect_identical(renders, baseline)

    use_state_only_fixture(list(entry("dataset-b"), entry("dataset-a")))
    session$flushReact()
    expect_identical(renders, baseline + 1L)
  })
})

test_that("dynamic Core and Enhance contracts update only their owned controls", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }
  select_updates <- list()
  retain_updates <- list()
  app_env$updateSelectInput <- function(
    session,
    inputId,
    label = NULL,
    choices = NULL,
    selected = NULL
  ) {
    select_updates[[inputId]] <<- list(
      choices = choices,
      selected = selected
    )
  }
  app_env$updateCheckboxGroupInput <- function(
    session,
    inputId,
    label = NULL,
    choices = NULL,
    selected = NULL,
    inline = FALSE
  ) {
    retain_updates[[inputId]] <<- list(
      choices = choices,
      selected = selected
    )
  }
  table_path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("sample,value", "a,1"), table_path)

  shiny::testServer(app_env$server, {
    entry <- list(
      id = "dataset-a",
      revision = 0L,
      snapshot = list(
        path = "/private/dataset-a",
        owner_token = "owner-a",
        object_md5 = strrep("a", 32L)
      ),
      profile = list(
        organism_guess = "hg",
        assays = c("RNA", "SCT"),
        layers = c("data", "counts"),
        default_layer = "data",
        nUMI = "nCount_RNA",
        nGene = "nFeature_RNA",
        assay_profiles = list(
          RNA = list(
            layers = c("data", "counts"),
            default_layer = "data",
            nUMI_choices = "nCount_RNA",
            nGene_choices = "nFeature_RNA",
            nUMI = "nCount_RNA",
            nGene = "nFeature_RNA"
          ),
          SCT = list(
            layers = c("scale.data", "counts"),
            default_layer = "scale.data",
            nUMI_choices = "nCount_SCT",
            nGene_choices = "nFeature_SCT",
            nUMI = "nCount_SCT",
            nGene = "nFeature_SCT"
          )
        ),
        extras = list(),
        images = character()
      ),
      settings = list(
        name = "Dataset A",
        organism = "hg",
        default_group = "cluster",
        default_projection = "umap",
        assay = "RNA",
        layer = "data",
        nUMI = "nCount_RNA",
        nGene = "nFeature_RNA",
        expression_backend = "embedded",
        analyses = character(),
        tables = list(),
        images = list()
      )
    )
    use_state_only_fixture(list(entry))
    session$flushReact()

    top_level_runs <- 0L
    tracker <- observe({
      current()
      top_level_runs <<- top_level_runs + 1L
    })
    withr::defer(tracker$destroy())
    session$flushReact()
    baseline <- top_level_runs

    session$setInputs(
      `core-rendered_for` = "dataset-a",
      `core-name` = "Dataset A",
      `core-organism` = "hg",
      `core-default_group` = "cluster",
      `core-default_projection` = "umap",
      `core-assay` = "SCT",
      `core-layer` = "data",
      `core-nUMI` = "nCount_RNA",
      `core-nGene` = "nFeature_RNA",
      `core-backend` = "embedded"
    )
    session$flushReact()
    expect_identical(top_level_runs, baseline)
    expect_identical(
      names(select_updates),
      c("core-layer", "core-nUMI", "core-nGene")
    )
    expect_identical(
      select_updates[["core-layer"]]$choices,
      c("scale.data", "counts")
    )
    expect_identical(
      select_updates[["core-layer"]]$selected,
      "scale.data"
    )
    expect_identical(
      select_updates[["core-nUMI"]],
      list(choices = "nCount_SCT", selected = "nCount_SCT")
    )
    expect_identical(
      select_updates[["core-nGene"]],
      list(choices = "nFeature_SCT", selected = "nFeature_SCT")
    )
    expect_identical(sets()[[1L]]$settings$assay, "SCT")
    expect_identical(sets()[[1L]]$settings$layer, "scale.data")
    expect_identical(sets()[[1L]]$settings$nUMI, "nCount_SCT")
    expect_identical(sets()[[1L]]$settings$nGene, "nFeature_SCT")

    session$setInputs(`core-organism` = "other")
    session$flushReact()
    other_html <- paste(
      as.character(output[["enhance-analysis_modules"]]),
      collapse = ""
    )
    expect_false(grepl("percent_mt_ribo", other_html, fixed = TRUE))
    expect_identical(top_level_runs, baseline)

    session$setInputs(`core-organism` = "hg")
    session$flushReact()
    blocked_html <- paste(
      as.character(output[["enhance-analysis_modules"]]),
      collapse = ""
    )
    expect_match(blocked_html, "Select Marker genes first", fixed = TRUE)
    session$setInputs(
      `enhance-rendered_for` = "dataset-a",
      `enhance-analysis_marker_genes` = TRUE
    )
    session$flushReact()
    enabled_html <- paste(
      as.character(output[["enhance-analysis_modules"]]),
      collapse = ""
    )
    expect_false(grepl(
      "Select Marker genes first",
      enabled_html,
      fixed = TRUE
    ))
    expect_identical(top_level_runs, baseline)

    session$setInputs(
      `enhance-table_path` = table_path,
      `enhance-table_name` = "Clinical",
      `enhance-add_table` = 1L
    )
    session$flushReact()
    expect_identical(
      retain_updates[["enhance-tables_to_retain"]]$choices,
      "Clinical"
    )
    expect_identical(
      retain_updates[["enhance-tables_to_retain"]]$selected,
      "Clinical"
    )
    expect_identical(top_level_runs, baseline)

    alignment <- list(uri = "data:image/png;base64,AA==")
    saved <- sets()[[1L]]
    commit_enhance_images(saved, list(`section-a` = alignment))
    expect_identical(
      retain_updates[["enhance-histology_to_retain"]],
      list(choices = "section-a", selected = "section-a")
    )
    expect_identical(
      names(sets()[[1L]]$settings$images),
      "section-a"
    )
    expect_identical(top_level_runs, baseline)

    picture <- list(
      uri = "data:image/png;base64,AA==",
      bytes = 2,
      width = 10,
      height = 10,
      source_width = 10,
      source_height = 10,
      extent_width = 10,
      extent_height = 10,
      display_width = 10,
      display_height = 10
    )
    per_section <- list(
      `section-a` = list(
        bounds = c(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
        cover = list(outside = 0L, total = 2L)
      ),
      `section-b` = list(
        bounds = c(xmin = 10, xmax = 20, ymin = 10, ymax = 20),
        cover = list(outside = 0L, total = 2L)
      )
    )
    apply_section_bounds("dataset-a", per_section, picture)
    expect_identical(
      retain_updates[["enhance-histology_to_retain"]],
      list(
        choices = c("section-a", "section-b"),
        selected = c("section-a", "section-b")
      )
    )
    expect_identical(
      names(sets()[[1L]]$settings$images),
      c("section-a", "section-b")
    )
    expect_identical(top_level_runs, baseline)

    active_slice("section-a")
    session$setInputs(`enhance-drop_image` = 1L)
    session$flushReact()
    expect_identical(
      retain_updates[["enhance-histology_to_retain"]],
      list(choices = "section-b", selected = "section-b")
    )
    expect_identical(
      names(sets()[[1L]]$settings$images),
      "section-b"
    )
    expect_identical(top_level_runs, baseline)
  })

  app <- paste(
    readLines(builder_profile_inst_path("builder", "app.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(
    app,
    "imgs[[nm]] <- a\n    commit_enhance_images(e, imgs)",
    fixed = TRUE
  )
  expect_match(
    app,
    "paired <- builder_pair_sections(a, per_section)",
    fixed = TRUE
  )
  expect_match(
    app,
    "imgs[[nm]] <- NULL\n    }\n    commit_enhance_images(e, imgs)",
    fixed = TRUE
  )
})

test_that("app composes stage modules in deterministic order", {
  app <- readLines(
    builder_profile_inst_path("builder", "app.R"),
    warn = FALSE
  )
  sources <- vapply(
    c(
      "inspect_stage.R",
      "core_stage.R",
      "enhance_stage.R",
      "review_stage.R",
      "build_status.R"
    ),
    function(file) which(grepl(file, app, fixed = TRUE)),
    integer(1)
  )
  expect_true(all(diff(sources) > 0L))
  expect_true(any(grepl("builder_inspect_stage_ui(", app, fixed = TRUE)))
  expect_true(any(grepl("builder_core_stage_ui(", app, fixed = TRUE)))
  expect_true(any(grepl("builder_enhance_stage_ui(", app, fixed = TRUE)))
  expect_true(any(grepl("builder_review_stage_ui(", app, fixed = TRUE)))
  expect_true(any(grepl("builder_review_can_build(", app, fixed = TRUE)))
  expect_true(any(grepl('name = "core-name"', app, fixed = TRUE)))
  expect_true(any(grepl("input[[input_id]]", app, fixed = TRUE)))
  expect_true(any(grepl(
    'paste0("enhance-analysis_", step$id)',
    app,
    fixed = TRUE
  )))
  expect_true(any(grepl('input[["enhance-add_table"]]', app, fixed = TRUE)))
  expect_true(any(grepl(
    'input[["enhance-tables_to_retain"]]',
    app,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    'input[["enhance-histology_to_retain"]]',
    app,
    fixed = TRUE
  )))
  expect_true(sum(grepl("builder_enhance_retain", app, fixed = TRUE)) >= 2L)
  expect_true(any(grepl(
    "builder_enhance_analysis_profile",
    app,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "settings$organism",
    app,
    fixed = TRUE
  )))
  expect_true(any(grepl("builder_open_final_app", app, fixed = TRUE)))
  expect_true(any(grepl("builder_reveal_release", app, fixed = TRUE)))
  expect_true(any(grepl("builder_copy_result_path", app, fixed = TRUE)))
  expect_true(any(grepl("builder_release_error_result", app, fixed = TRUE)))
  expect_true(any(grepl(
    "plan$output_release$directory",
    app,
    fixed = TRUE
  )))
  expect_true(any(grepl("release$handle$target", app, fixed = TRUE)))
  expect_true(any(grepl("abort_release_result", app, fixed = TRUE)))
  status_source <- readLines(
    builder_profile_inst_path("builder", "ui", "build_status.R"),
    warn = FALSE
  )
  expect_true(any(grepl(
    "builder_result_recovery_required",
    status_source,
    fixed = TRUE
  )))
  expect_false(any(grepl("detected = names(entry$profile)", app, fixed = TRUE)))
  expect_false(any(grepl('uiOutput("detail")', app, fixed = TRUE)))
  expect_false(any(grepl("result(list(error =", app, fixed = TRUE)))
  expect_false(any(grepl("ready_report <- reactive", app, fixed = TRUE)))
})

test_that("guided workbench has no unreachable legacy detail handlers", {
  app <- paste(
    readLines(
      builder_profile_inst_path("builder", "app.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  legacy_contracts <- c(
    "output$detail <- renderUI",
    "\n  setting_inputs <- c(",
    "observeEvent(\n    input$assay",
    "observeEvent(input$analyses",
    "output$preview_plot <- plotly::renderPlotly",
    "output$palette_note <- renderUI",
    "output$color_swatches <- renderUI",
    "output$analysis_choices <- renderUI",
    "observeEvent(input$drop_table",
    "output$table_list <- renderUI"
  )

  for (contract in legacy_contracts) {
    expect_false(
      grepl(contract, app, fixed = TRUE),
      info = paste("legacy Builder contract remains reachable:", contract)
    )
  }
})
