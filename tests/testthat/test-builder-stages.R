builder_repo_source("ui/inspect_stage.R")
builder_repo_source("preview.R")
builder_repo_source("ui/core_stage.R")
builder_repo_source("ui/enhance_stage.R")
builder_repo_source("ui/review_stage.R")
builder_repo_source("ui/build_status.R")
builder_repo_source("extras.R")

builder_stage_html <- function(value) {
  htmltools::renderTags(value)$html
}

test_that("Review bounds large atomic identifier vectors without leaking tails", {
  ids <- sprintf("CELL-%05d", seq_len(1000L))
  lines <- builder_review_bounded_lines(list(cells = list(ids = ids)))
  text <- paste(lines, collapse = "\n")

  expect_match(text, "CELL-00001", fixed = TRUE)
  expect_match(text, "1000 values", fixed = TRUE)
  expect_match(text, "more values not shown", fixed = TRUE)
  expect_false(grepl("CELL-01000", text, fixed = TRUE))
  expect_lt(max(nchar(lines)), 240L)
})

test_that("Inspect leads with attention and compact detected-content tags", {
  model <- list(
    summary = c("1,200 cells", "4,500 genes"),
    attention = c("Metadata column sample has missing values"),
    blockers = c("Projection coordinates are incomplete"),
    content_tags = list(
      list(label = "Expression", tone = "core"),
      list(label = "UMAP", tone = "projection")
    ),
    diagnostics = c("source fingerprint abc123")
  )

  html <- builder_stage_html(builder_inspect_stage_ui("inspect", model))

  expect_match(html, "Needs attention", fixed = TRUE)
  expect_match(html, model$attention[[1L]], fixed = TRUE)
  expect_match(html, model$blockers[[1L]], fixed = TRUE)
  expect_match(html, "Detected content", fixed = TRUE)
  expect_match(html, "Expression", fixed = TRUE)
  expect_match(html, "UMAP", fixed = TRUE)
  expect_match(html, "builder-content-tag", fixed = TRUE)
  expect_false(grepl("View all detected content", html, fixed = TRUE))
  expect_false(grepl("Technical diagnostics", html, fixed = TRUE))
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

  expect_match(html, "Expression", fixed = TRUE)
  expect_false(grepl("spatial", html, fixed = TRUE))
  expect_false(grepl("No spatial sections were detected", html, fixed = TRUE))
  expect_false(grepl("internal_cache", html, fixed = TRUE))
})

test_that("Inspect content tags use readable colour families instead of a gray catch-all", {
  ids <- c(
    "marker_genes",
    "trajectory",
    "immune_repertoire",
    "extra_material"
  )
  tones <- vapply(
    ids,
    function(id) {
      builder_inspect_content_tag(list(id = id, status = "valid"))$tone
    },
    character(1)
  )

  expect_identical(
    unname(tones),
    c("analysis", "trajectory", "immune", "extra")
  )
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
  expect_match(html, 'class="builder-form-grid"', fixed = TRUE)
  expect_equal(
    lengths(regmatches(
      html,
      gregexpr('class="builder-field[^\"]*"', html, perl = TRUE)
    )),
    4L
  )
  expect_match(html, 'class="builder-group-colors-slot"', fixed = TRUE)
  expect_match(html, 'class="builder-disclosure"', fixed = TRUE)
  expect_match(html, "Advanced settings", fixed = TRUE)
  expect_false(grepl("Advanced technical settings", html, fixed = TRUE))
  expect_match(html, "Assay", fixed = TRUE)
  expect_match(html, "Expression backend", fixed = TRUE)
  expect_match(html, 'id="core-rendered_for"', fixed = TRUE)
  expect_match(html, 'value="dataset-a"', fixed = TRUE)
  expect_match(html, 'id="core-organism"', fixed = TRUE)
  expect_match(html, '"create":true', fixed = TRUE)
})

test_that("Core restores accessible group colors after Default group", {
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
    assay_choices = "RNA",
    layer = "data",
    layer_choices = "data",
    nUMI = "nCount_RNA",
    nUMI_choices = "nCount_RNA",
    nGene = "nFeature_RNA",
    nGene_choices = "nFeature_RNA",
    backend = "embedded",
    backend_choices = "embedded"
  )
  colors <- builder_group_colors_model(
    model$default_group,
    c("0", "1", "N/A"),
    "cerebro",
    list(cluster = c(`1` = "#e76f51"))
  )

  core <- builder_stage_html(builder_core_stage_ui("core", model))
  html <- builder_stage_html(builder_group_colors_ui("core", colors))

  expect_lt(
    regexpr("core-default_group", core, fixed = TRUE)[[1L]],
    regexpr("core-group_colors", core, fixed = TRUE)[[1L]]
  )
  expect_lt(
    regexpr("core-group_colors", core, fixed = TRUE)[[1L]],
    regexpr("core-default_projection", core, fixed = TRUE)[[1L]]
  )
  expect_match(html, "Group colors", fixed = TRUE)
  expect_match(html, "Coloring by:", fixed = TRUE)
  expect_match(html, "cluster", fixed = TRUE)
  expect_match(html, 'type="color"', fixed = TRUE)
  expect_match(html, 'aria-label="Color for cluster 0"', fixed = TRUE)
  expect_match(html, "Missing", fixed = TRUE)
  expect_match(html, "#E76F51", fixed = TRUE)
  expect_match(html, "Reset colors", fixed = TRUE)
  expect_false(grepl("UMAP colors|PCA colors", html))
})

test_that("Group colors bounds large groups and exposes local search", {
  twenty <- builder_group_colors_model(
    "cluster",
    sprintf("value-%02d", seq_len(20L)),
    "cerebro",
    list()
  )
  fifty <- builder_group_colors_model(
    "cluster",
    sprintf("long-category-name-%02d", seq_len(50L)),
    "cerebro",
    list()
  )
  twenty_html <- builder_stage_html(builder_group_colors_ui("core", twenty))
  fifty_html <- builder_stage_html(builder_group_colors_ui("core", fifty))

  expect_match(twenty_html, "Show all 20 colors", fixed = TRUE)
  expect_match(twenty_html, "Show fewer", fixed = TRUE)
  expect_match(twenty_html, 'data-visible-limit="12"', fixed = TRUE)
  expect_false(grepl("Find a group value", twenty_html, fixed = TRUE))
  expect_match(fifty_html, "Find a group value", fixed = TRUE)
  expect_match(fifty_html, "Show all 50 colors", fixed = TRUE)
  expect_match(fifty_html, 'title="long-category-name-50"', fixed = TRUE)
})

test_that("Group colors has a short empty state for invalid groups", {
  model <- builder_group_colors_model("", character(), "cerebro", list())
  html <- builder_stage_html(builder_group_colors_ui("core", model))

  expect_match(
    html,
    "Choose a categorical default group to set initial colors.",
    fixed = TRUE
  )
  expect_false(grepl('type="color"', html, fixed = TRUE))
})

test_that("Inspect does not repeat the verified cell and gene summary", {
  model <- list(
    summary = c("24 cells", "40 genes"),
    statistics = list(cells = 24L, genes = 40L),
    attention = character(),
    blockers = character(),
    content_tags = list()
  )

  html <- builder_stage_html(builder_inspect_stage_ui("inspect", model))

  expect_match(html, "24 cells", fixed = TRUE)
  expect_match(html, "40 genes", fixed = TRUE)
  expect_false(grepl("Verified profile", html, fixed = TRUE))
  expect_false(grepl("were verified", html, fixed = TRUE))
})

test_that("Organism keeps a custom current value in the editable choices", {
  model <- list(
    id = "dataset-a",
    name = "PBMC",
    organism = "danio_rerio",
    organism_choices = c(
      "Human (hg)" = "hg",
      "Mouse (mm)" = "mm",
      Other = "other"
    ),
    default_group = "cluster",
    group_choices = "cluster",
    default_projection = "umap",
    projection_choices = "umap",
    assay = "RNA",
    assay_choices = "RNA",
    layer = "data",
    layer_choices = "data",
    nUMI = "nCount_RNA",
    nUMI_choices = "nCount_RNA",
    nGene = "nFeature_RNA",
    nGene_choices = "nFeature_RNA",
    backend = "embedded",
    backend_choices = c(Embedded = "embedded")
  )

  html <- builder_stage_html(builder_core_stage_ui("core", model))

  expect_match(html, 'value="danio_rerio" selected', fixed = TRUE)
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
        selected = TRUE,
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
        label = "Spatial alignment",
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
  expect_match(html, 'class="enhance-module-select"', fixed = TRUE)
  expect_match(
    html,
    'class="enhance-module-checkbox visually-hidden shiny-input-checkbox"',
    fixed = TRUE
  )
  expect_match(html, 'class="enhance-module-title"', fixed = TRUE)
  expect_match(html, 'id="enhance-analysis_marker_genes"', fixed = TRUE)
  expect_match(html, 'checked="checked"', fixed = TRUE)
  expect_match(html, 'class="enhance-info-button"', fixed = TRUE)
  expect_match(
    html,
    '</label>\\s*<button type="button" class="enhance-info-button"',
    perl = TRUE
  )
  expect_match(
    html,
    'aria-label="More information about Marker genes"',
    fixed = TRUE
  )
  expect_match(html, 'data-title="Marker genes"', fixed = TRUE)
  expect_match(
    html,
    'data-description="Adds ranked marker tables to the Viewer."',
    fixed = TRUE
  )
  expect_match(html, 'data-pages="marker genes"', fixed = TRUE)
  expect_match(
    html,
    'data-cost="Can take several minutes and increases release size."',
    fixed = TRUE
  )
  expect_match(html, 'data-network="No network access."', fixed = TRUE)
  expect_match(
    html,
    'data-prerequisite="Requires a supported differential-expression method."',
    fixed = TRUE
  )
  expect_match(
    html,
    'data-replacement="Replace the existing marker result."',
    fixed = TRUE
  )
  expect_match(
    html,
    'data-skip="The marker page stays unavailable."',
    fixed = TRUE
  )
  expect_false(grepl("What this changes", html, fixed = TRUE))
  expect_false(grepl("Enabled page: marker genes", html, fixed = TRUE))
  expect_match(html, "Select Marker genes first", fixed = TRUE)
  expect_match(html, "disabled", fixed = TRUE)
  expect_match(html, "enhance-module is-blocked", fixed = TRUE)
  expect_match(html, "Optional attachments", fixed = TRUE)
  expect_match(html, "Tables for Extra material", fixed = TRUE)
  expect_match(
    html,
    "Add optional CSV or TSV tables to the generated app’s Extra material page.",
    fixed = TRUE
  )
  expect_match(html, "Spatial alignment", fixed = TRUE)
  expect_match(
    html,
    "Compare transcriptome and physical space, then align an optional tissue image.",
    fixed = TRUE
  )
  expect_false(grepl("Enabled page: extra material", html, fixed = TRUE))
  expect_false(grepl("Enabled page: spatial", html, fixed = TRUE))
  expect_false(grepl("Replacement policy", html, fixed = TRUE))
  expect_false(grepl("Skipped tables will not appear", html, fixed = TRUE))
  expect_false(grepl("Auto-retained content", html, fixed = TRUE))
  expect_false(grepl("Tables to retain", html, fixed = TRUE))
  expect_false(grepl(
    "Dataset cell and feature identities were checked",
    html,
    fixed = TRUE
  ))
  expect_false(grepl("BuildPlan decision for", html, fixed = TRUE))
  expect_match(html, 'id="enhance-table_files"', fixed = TRUE)
  expect_match(html, 'type="file"', fixed = TRUE)
  expect_match(html, 'multiple="multiple"', fixed = TRUE)
  expect_match(html, 'accept=".csv,.tsv,.txt"', fixed = TRUE)
  expect_match(
    html,
    'class="enhance-table-file-control builder-file-picker builder-file-picker--content"',
    fixed = TRUE
  )
  expect_match(
    html,
    'class="enhance-table-file-button builder-file-trigger"',
    fixed = TRUE
  )
  expect_match(html, "+ Add tables…", fixed = TRUE)
  expect_false(grepl('id="enhance-table_path"', html, fixed = TRUE))
  expect_false(grepl('id="enhance-table_name"', html, fixed = TRUE))
  expect_false(grepl('id="enhance-add_table"', html, fixed = TRUE))
  expect_false(grepl('id="enhance-tables_to_retain"', html, fixed = TRUE))
  expect_match(html, 'id="enhance-active_section"', fixed = TRUE)
  expect_match(html, 'id="enhance-tissue_image_file"', fixed = TRUE)
  expect_match(html, 'accept=".png,.jpg,.jpeg"', fixed = TRUE)
  expect_match(
    html,
    'class="enhance-tissue-file-control builder-file-picker builder-file-picker--compact"',
    fixed = TRUE
  )
  expect_match(html, "+ Add tissue image…", fixed = TRUE)
  expect_false(grepl('id="enhance-image_path"', html, fixed = TRUE))
  expect_false(grepl('id="enhance-attach_image"', html, fixed = TRUE))
  expect_false(grepl('id="enhance-histology_to_retain"', html, fixed = TRUE))
  expect_match(html, "Transcriptome space", fixed = TRUE)
  expect_match(html, "Spatial space", fixed = TRUE)
  expect_match(html, 'id="enhance-alignment_transcriptome_plot"', fixed = TRUE)
  expect_match(html, 'id="enhance-alignment_spatial_plot"', fixed = TRUE)
  expect_match(html, 'id="enhance-alignment_legend"', fixed = TRUE)
  expect_match(html, 'aria-label="Transcriptome-space cell plot"', fixed = TRUE)
  expect_match(html, 'aria-label="Spatial-space cell plot"', fixed = TRUE)
  expect_match(html, "Position", fixed = TRUE)
  expect_match(html, "Scale &amp; orientation", fixed = TRUE)
  expect_match(html, "Appearance", fixed = TRUE)
  expect_match(html, "Image opacity", fixed = TRUE)
  expect_match(html, "Point opacity", fixed = TRUE)
  expect_match(html, 'data-postfix="%"', fixed = TRUE)
  expect_match(html, "has_image", fixed = TRUE)
  expect_match(html, 'data-ns-prefix="enhance-"', fixed = TRUE)
  expect_match(html, "Point size", fixed = TRUE)
  expect_match(html, "Save alignment", fixed = TRUE)
  expect_match(html, "Apply transform to all sections", fixed = TRUE)
  expect_match(html, "Reset alignment", fixed = TRUE)
  expect_match(html, 'id="enhance-alignment_status"', fixed = TRUE)
  expect_false(grepl("Remove image", html, fixed = TRUE))
  expect_match(html, 'class="spatial-alignment-layout"', fixed = TRUE)
  expect_match(
    html,
    'class="spatial-alignment-sidebar builder-controls-grid"',
    fixed = TRUE
  )
  expect_match(
    html,
    'class="spatial-alignment-plots builder-preview-grid"',
    fixed = TRUE
  )
  expect_match(
    html,
    'class="spatial-alignment-actions builder-action-row"',
    fixed = TRUE
  )
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
    "Spatial alignment",
    builder_stage_html(builder_enhance_stage_ui("enhance", no_spatial)),
    fixed = TRUE
  ))

  trekker <- builder_enhance_model(
    id = "dataset-trekker",
    profile = list(
      images = character(),
      extras = list(list(
        key = "trekker",
        label = "Trekker spatial mapping",
        found = TRUE
      ))
    ),
    state = list(manifest = list()),
    settings = list(tables = list(), images = list()),
    modules = list()
  )
  expect_true(trekker$attachments$histology$relevant)
  expect_identical(trekker$attachments$histology$sections, "trekker")
  expect_match(
    builder_stage_html(builder_enhance_stage_ui("enhance", trekker)),
    "Spatial alignment",
    fixed = TRUE
  )
})

test_that("table uploads derive a display name from the client filename", {
  expect_identical(
    builder_table_default_name("clinical-results.CSV"),
    "clinical-results"
  )
  expect_identical(builder_table_default_name("nested.name.tsv"), "nested.name")
})

test_that("table read failures never expose a server-side upload path", {
  directory <- withr::local_tempdir()

  got <- builder_read_table(
    directory,
    filename = "supplement.csv"
  )

  expect_identical(
    got$error,
    "Could not read this table. Check that it is a valid CSV, TSV or TXT file."
  )
  expect_false(grepl(directory, got$error, fixed = TRUE))
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

test_that("tissue image metadata and Remove share the bounded file-list UI", {
  html <- builder_stage_html(builder_tissue_image_file_ui(
    "enhance",
    list(
      source = list(
        name = "/private/upload/section-a.png",
        type = "image/png",
        size = 2048
      ),
      saved = TRUE
    )
  ))
  expect_match(html, "builder-file-list", fixed = TRUE)
  expect_match(html, "builder-file-item", fixed = TRUE)
  expect_match(html, "section-a.png", fixed = TRUE)
  expect_match(html, "PNG · 2 KB", fixed = TRUE)
  expect_match(html, "Ready", fixed = TRUE)
  expect_false(grepl("/private/upload", html, fixed = TRUE))
  expect_match(html, 'id="enhance-drop_image"', fixed = TRUE)
  expect_match(html, "btn-remove-soft", fixed = TRUE)

  server <- readLines(
    builder_profile_inst_path("builder", "spatial_alignment_server.R"),
    warn = FALSE
  )
  expect_true(any(grepl(
    'observeEvent(input[["enhance-drop_image"]]',
    server,
    fixed = TRUE
  )))
  expect_false(any(grepl(
    'actionButton("drop_image"',
    server,
    fixed = TRUE
  )))
})

test_that("Apply to all sections requires an explicit confirmation", {
  server <- paste(
    readLines(
      builder_profile_inst_path("builder", "spatial_alignment_server.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(server, "enhance-confirm_apply_align_all", fixed = TRUE)
  expect_match(
    server,
    "Apply transform to all image-bearing sections?",
    fixed = TRUE
  )
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
            group_levels = list(cluster = c("A", "B")),
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
          color_custom_count = 1L,
          nUMI = "nCount_RNA",
          nGene = "nFeature_RNA",
          default_group = "cluster",
          default_projection = "umap",
          viewer_page_expectations = list(
            always = builder_viewer_page_catalog()$always,
            conditional = builder_viewer_page_catalog()$conditional,
            visible_conditional = c("marker_genes", "extra_material"),
            hidden_conditional = c("spatial", "trajectory")
          ),
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

test_that("Review model translates a frozen plan into user language", {
  plan <- builder_stage_frozen_plan()
  model <- builder_review_model(plan)

  expect_null(model$revision)
  expect_null(model$contract)
  expect_null(model$manifest)
  expect_null(model$app$host)
  expect_null(model$app$port)
  expect_null(model$app$max_request_size)
  expect_identical(model$dataset_count, 2L)
  expect_identical(model$output_label, "CRB files + private App")
  expect_identical(model$datasets[[1L]]$name, "Dataset B")
  expect_identical(model$datasets[[1L]]$group_count, 2L)
  expect_identical(model$datasets[[1L]]$projection_count, 1L)
  expect_identical(model$app$initial_dataset, "Dataset B")
  expect_identical(model$app$dataset_order, c("Dataset B", "Dataset A"))
  expect_identical(model$output$existing_files, "Keep existing files")
  expect_identical(model$output$estimated_size, "8 KB")
  expect_identical(model$output$estimated_time, "A few minutes")
  expect_true(all(
    c("Data info", "Projection", "Marker genes") %in% model$pages
  ))
  expect_false(any(c("marker_genes", "spatial") %in% model$pages))
  expect_length(model$warnings, 0L)
  expect_true(model$can_build)
})

test_that("Review presents datasets, App experience, pages, and output", {
  crbs <- builder_review_model(builder_stage_frozen_plan(FALSE))
  expect_identical(crbs$output_label, "CRB files")

  app <- builder_review_model(builder_stage_frozen_plan(TRUE))
  html <- builder_stage_html(builder_review_stage_ui("review", app))

  expect_match(
    html,
    "Check your datasets and output before building.",
    fixed = TRUE
  )
  expect_match(html, "2 datasets", fixed = TRUE)
  expect_match(html, "Creates CRB files + private App", fixed = TRUE)
  expect_match(html, "Datasets", fixed = TRUE)
  expect_match(html, "Dataset B", fixed = TRUE)
  expect_match(html, "2 cells · 3 genes", fixed = TRUE)
  expect_match(html, "2 groups · 1 projection", fixed = TRUE)
  expect_match(html, "Opens with", fixed = TRUE)
  expect_match(html, "UMAP", fixed = TRUE)
  expect_match(html, "Grouped by", fixed = TRUE)
  expect_match(html, "cluster", fixed = TRUE)
  expect_match(html, "Output file:", fixed = TRUE)
  expect_match(html, "01-dataset-b.crb", fixed = TRUE)
  expect_match(html, "App experience", fixed = TRUE)
  expect_match(html, "Dataset order", fixed = TRUE)
  expect_match(html, "Visitor uploads", fixed = TRUE)
  expect_match(html, "Off", fixed = TRUE)
  expect_match(html, "Welcome, lab team!", fixed = TRUE)
  expect_match(html, "Point size", fixed = TRUE)
  expect_match(html, "Variable comparison", fixed = TRUE)
  expect_match(html, "Pages in the App", fixed = TRUE)
  expect_match(html, "Data info", fixed = TRUE)
  expect_match(html, "Marker genes", fixed = TRUE)
  expect_match(html, "Output", fixed = TRUE)
  expect_match(html, "Folder", fixed = TRUE)
  expect_match(html, "/private/host/output", fixed = TRUE)
  expect_false(grepl("Existing files", html, fixed = TRUE))
  expect_false(grepl("Keep existing files", html, fixed = TRUE))
  expect_match(html, "8 KB", fixed = TRUE)
  expect_match(html, "A few minutes", fixed = TRUE)
  expect_match(html, "Private App", fixed = TRUE)
  expect_match(html, "not offered as public downloads", fixed = TRUE)
  forbidden <- c(
    "Plan revision",
    "App contract",
    "Artifact mode",
    "automatic",
    "Planned payload members",
    "Replacement policy",
    "Technical plan details",
    "Viewer page expectations",
    "Expected after build",
    "Private assets",
    "BuildPlan",
    "manifest",
    "Host:",
    "Port:",
    "Request limit",
    "Display mode",
    "Launch browser",
    "sidecars",
    "HTTP-public",
    "barcode",
    "backend"
  )
  expect_false(any(vapply(
    forbidden,
    grepl,
    logical(1),
    x = html,
    fixed = TRUE
  )))
  expect_false(grepl("Needs attention", html, fixed = TRUE))
})

test_that("Review summarizes saved and points-only spatial sections", {
  plan <- builder_stage_frozen_plan(TRUE)
  plan$items[[1L]]$spatial_alignment <- list(
    section_count = 2L,
    image_count = 1L,
    saved_count = 1L,
    points_only = "section-b"
  )
  model <- builder_review_model(plan)
  html <- builder_stage_html(builder_review_stage_ui("review", model))

  expect_identical(model$datasets[[1L]]$spatial_alignment$saved_count, 1L)
  expect_match(html, "Spatial alignment", fixed = TRUE)
  expect_match(html, "1 of 2 sections has a saved tissue image", fixed = TRUE)
  expect_match(html, "1 section remains points-only", fixed = TRUE)
  expect_false(grepl("section-b", html, fixed = TRUE))
  expect_false(grepl("histology_image_bounds", html, fixed = TRUE))
})

test_that("Review keeps group colors compact and distinguishes custom colors", {
  plan <- builder_stage_frozen_plan(TRUE)
  plan$items[[1L]]$colors$cluster <- c(
    A = "#111111",
    B = "#222222",
    C = "#333333",
    D = "#444444",
    E = "#555555",
    F = "#666666",
    G = "#777777",
    H = "#888888"
  )
  plan$items[[1L]]$color_custom_count <- 3L
  plan$items[[2L]]$default_group <- "cell_type"
  plan$items[[2L]]$color_custom_count <- 0L

  model <- builder_review_model(plan)
  html <- builder_stage_html(builder_review_stage_ui("review", model))

  expect_identical(model$datasets[[1L]]$group_colors$group, "cluster")
  expect_identical(model$datasets[[1L]]$group_colors$custom_count, 3L)
  expect_lte(length(model$datasets[[1L]]$group_colors$preview), 5L)
  expect_match(html, "Group colors", fixed = TRUE)
  expect_match(html, "cluster · 3 custom colors", fixed = TRUE)
  expect_match(html, "cell_type · Using default colors", fixed = TRUE)
  expect_match(html, "+3", fixed = TRUE)
  expect_false(grepl(">#111111<", html, fixed = TRUE))
  expect_false(grepl("palettes are frozen", html, ignore.case = TRUE))
})

test_that("Review translates policies, bounds pages, and names actionable issues", {
  expect_identical(
    vapply(
      c("preserve_existing", "overwrite", "error_if_exists"),
      builder_review_existing_files,
      character(1)
    ),
    c(
      preserve_existing = "Keep existing files",
      overwrite = "Replace existing files",
      error_if_exists = "Stop if files already exist"
    )
  )
  expect_match(builder_review_human_size(514285), "KB", fixed = TRUE)
  expect_match(builder_review_human_size(5 * 1024^2), "MB", fixed = TRUE)

  plan <- builder_stage_frozen_plan(TRUE)
  plan$existing_targets <- "/private/host/output/01-dataset-b.crb"
  plan$overwrite <- FALSE
  plan$required_settings <- "dataset-b: choose a different output folder."
  model <- builder_review_model(plan)
  html <- builder_stage_html(builder_review_stage_ui("review", model))

  expect_false(model$can_build)
  expect_match(model$warnings[[1L]], "Dataset B", fixed = TRUE)
  expect_false(grepl("dataset-b:", model$warnings[[1L]], fixed = TRUE))
  expect_match(html, "Needs attention", fixed = TRUE)
  expect_match(html, "Show 1 more", fixed = TRUE)
})

test_that("Review translates network-dependent runtime into user language", {
  plan <- builder_stage_frozen_plan(TRUE)
  plan$output_release$estimated_runtime <- "network-dependent"

  expect_identical(
    builder_review_model(plan)$output$estimated_time,
    "Depends on network response"
  )
})

test_that("Review gives a useful next step when the plan is not ready", {
  html <- builder_stage_html(builder_review_blocked_ui(
    "review",
    "Choose a valid output folder."
  ))

  expect_match(html, "Review", fixed = TRUE)
  expect_match(html, "Needs attention", fixed = TRUE)
  expect_match(html, "Choose a valid output folder.", fixed = TRUE)
  expect_match(html, "Correct the highlighted settings", fixed = TRUE)
  expect_false(grepl(
    "Choose the required dataset settings before building.",
    html,
    fixed = TRUE
  ))
})

test_that("Review handles one dataset and long output folders", {
  plan <- builder_stage_frozen_plan(TRUE)
  plan$items <- plan$items[1L]
  plan$dataset_order <- "dataset-b"
  plan$output_release$directory <- paste0(
    "/private/host/",
    paste(rep("long-folder-name", 8L), collapse = "/")
  )
  model <- builder_review_model(plan)
  html <- builder_stage_html(builder_review_stage_ui("review", model))

  expect_match(html, "1 dataset", fixed = TRUE)
  expect_false(grepl("1 datasets", html, fixed = TRUE))
  expect_match(html, plan$output_release$directory, fixed = TRUE)
  expect_identical(model$app$dataset_order, "Dataset B")
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

test_that("build pipeline only renders server-known states", {
  queued <- builder_stage_html(builder_build_pipeline_ui("queued"))
  building <- builder_stage_html(builder_build_pipeline_ui("building"))
  complete <- builder_stage_html(builder_build_pipeline_ui("complete"))
  failure <- builder_stage_html(builder_build_pipeline_ui("failure"))

  expect_match(queued, 'data-pipeline-state="queued"', fixed = TRUE)
  expect_match(building, 'data-pipeline-state="building"', fixed = TRUE)
  expect_match(complete, 'data-pipeline-state="complete"', fixed = TRUE)
  expect_match(failure, 'data-pipeline-state="failure"', fixed = TRUE)
  expect_false(grepl("Verify", paste(queued, building, failure)))

  decision <- builder_stage_html(
    builder_build_status_ui(builder_result_needs_decision("Choose one."))
  )
  expect_false(grepl("builder-build-pipeline", decision, fixed = TRUE))
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

test_that("recovery actions require explicit typed evidence", {
  undecidable <- builder_stage_html(builder_build_status_ui(
    builder_result_needs_decision("Choose one.", retry_closure = "marker_genes")
  ))
  targeted <- builder_stage_html(builder_build_status_ui(
    builder_result_needs_decision(
      "Choose one.",
      retry_closure = "marker_genes",
      failed_dataset_id = "ds1"
    )
  ))
  ordinary_failure <- builder_stage_html(builder_build_status_ui(
    builder_result_failure("Export failed.")
  ))
  worker_failure <- builder_stage_html(builder_build_status_ui(
    builder_result_failure("Worker stopped.", restartable_worker = TRUE)
  ))

  expect_match(undecidable, "Retry optional work", fixed = TRUE)
  expect_false(grepl("Remove and rebuild", undecidable, fixed = TRUE))
  expect_match(targeted, "Remove and rebuild", fixed = TRUE)
  expect_false(grepl("Restart worker", ordinary_failure, fixed = TRUE))
  expect_match(worker_failure, "Restart worker", fixed = TRUE)
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
    "Allow uploads"
  )) {
    expect_match(html, label, fixed = TRUE)
  }
  for (label in c(
    "Host",
    "Port",
    "Request size",
    "Display mode",
    "Launch browser"
  )) {
    expect_false(grepl(label, html, fixed = TRUE))
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
  table_path <- withr::local_tempfile()
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
      levels = list(cluster = c("A", "B"), sample = c("one", "two")),
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
        images = list(),
        palette = "cerebro",
        color_overrides = list(sample = c(one = "#123456"))
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

    before_color <- sets()[[1L]]$revision
    marked <- isolate(store())
    marked$datasets[[1L]]$reviewed_revision <- before_color
    store(marked)
    session$setInputs(
      `core-group_color` = list(
        group = "cluster",
        level = "B",
        color = "#e76f51",
        nonce = 1
      )
    )
    session$flushReact()
    colored <- sets()[[1L]]
    expect_identical(
      colored$settings$color_overrides$cluster[["B"]],
      "#E76F51"
    )
    expect_identical(
      colored$settings$color_overrides$sample[["one"]],
      "#123456"
    )
    expect_identical(colored$settings$default_projection, "umap")
    expect_gt(colored$revision, before_color)
    expect_false(identical(colored$reviewed_revision, colored$revision))

    before_reset <- colored$revision
    session$setInputs(`core-reset_colors` = 1L)
    session$flushReact()
    reset <- sets()[[1L]]
    expect_null(reset$settings$color_overrides$cluster)
    expect_identical(reset$settings$color_overrides$sample[["one"]], "#123456")
    expect_gt(reset$revision, before_reset)
    expect_identical(top_level_runs, baseline)

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
      `enhance-table_files` = data.frame(
        name = "clinical-results.csv",
        size = file.info(table_path)$size,
        type = "text/csv",
        datapath = table_path,
        stringsAsFactors = FALSE
      )
    )
    session$flushReact()
    table_list_html <- paste(
      as.character(output[["enhance-table_list"]]),
      collapse = ""
    )
    expect_match(table_list_html, "Added tables", fixed = TRUE)
    expect_match(table_list_html, "Table name", fixed = TRUE)
    expect_match(table_list_html, "CSV", fixed = TRUE)
    expect_match(table_list_html, "bytes", fixed = TRUE)
    expect_match(table_list_html, "Ready", fixed = TRUE)
    expect_match(table_list_html, "builder-file-list", fixed = TRUE)
    expect_match(table_list_html, "builder-file-item", fixed = TRUE)
    expect_false(grepl(table_path, table_list_html, fixed = TRUE))
    expect_false(grepl("fakepath", table_list_html, fixed = TRUE))
    expect_false(grepl("Tables to retain", table_list_html, fixed = TRUE))
    session$setInputs(
      `enhance-table_action` = list(
        action = "rename",
        key = "clinical-results",
        name = "Clinical results",
        nonce = 1
      )
    )
    session$flushReact()
    expect_identical(names(sets()[[1L]]$settings$tables), "Clinical results")
    expect_identical(
      sets()[[1L]]$settings$tables[[1L]]$file_name,
      "clinical-results.csv"
    )
    session$setInputs(
      `enhance-table_action` = list(
        action = "remove",
        key = "Clinical results",
        nonce = 2
      )
    )
    session$flushReact()
    expect_length(sets()[[1L]]$settings$tables, 0L)
    expect_identical(top_level_runs, baseline)

    alignment <- list(uri = "data:image/png;base64,AA==")
    saved <- sets()[[1L]]
    commit_enhance_images(saved, list(`section-a` = alignment))
    expect_null(retain_updates[["enhance-histology_to_retain"]])
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
    expect_null(retain_updates[["enhance-histology_to_retain"]])
    expect_identical(
      names(sets()[[1L]]$settings$images),
      c("section-a", "section-b")
    )
    expect_identical(top_level_runs, baseline)

    active_slice("section-a")
    session$setInputs(`enhance-drop_image` = 1L)
    session$flushReact()
    expect_null(retain_updates[["enhance-histology_to_retain"]])
    expect_identical(
      names(sets()[[1L]]$settings$images),
      "section-b"
    )
    expect_identical(top_level_runs, baseline)
  })

  server <- paste(
    readLines(
      builder_profile_inst_path("builder", "spatial_alignment_server.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    server,
    "commit_section(entry, section, record)",
    fixed = TRUE
  )
  expect_match(
    server,
    "builder_alignment_apply_transform_to_all",
    fixed = TRUE
  )
  expect_match(
    server,
    "commit_section(entry, section, NULL)",
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
  expect_true(any(grepl('input[["enhance-table_files"]]', app, fixed = TRUE)))
  expect_false(any(grepl('input[["enhance-add_table"]]', app, fixed = TRUE)))
  expect_false(any(grepl('input[["enhance-table_path"]]', app, fixed = TRUE)))
  expect_false(any(grepl(
    'input[["enhance-tables_to_retain"]]',
    app,
    fixed = TRUE
  )))
  expect_false(any(grepl(
    'input[["enhance-histology_to_retain"]]',
    app,
    fixed = TRUE
  )))
  expect_false(any(grepl("builder_enhance_retain", app, fixed = TRUE)))
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
