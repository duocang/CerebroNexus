builder_stage_contract_source_runtime(environment())

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

  expect_match(html, "builder-stage-section", fixed = TRUE)
  expect_false(grepl("builder-card", html, fixed = TRUE))
  expect_match(html, "Marker genes", fixed = TRUE)
  expect_match(html, "Adds ranked marker tables", fixed = TRUE)
  expect_match(
    html,
    'class="[^"]*marker-genes-action[^"]*is-selected[^"]*"',
    perl = TRUE
  )
  expect_match(
    html,
    'class="enhance-module-checkbox visually-hidden shiny-input-checkbox"',
    fixed = TRUE
  )
  expect_match(html, 'class="enhance-module-title"', fixed = TRUE)
  expect_match(
    html,
    'id="enhance-analysis_marker_genes_action"',
    fixed = TRUE
  )
  expect_match(html, 'aria-pressed="true"', fixed = TRUE)
  expect_false(grepl(
    'id="enhance-analysis_marker_genes" type="checkbox"',
    html,
    fixed = TRUE
  ))
  expect_match(
    html,
    'id="enhance-analysis_enriched_pathways" type="checkbox"',
    fixed = TRUE
  )
  expect_match(html, 'class="enhance-info-button"', fixed = TRUE)
  expect_match(
    html,
    '</button>\\s*<button type="button" class="enhance-info-button"',
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
  expect_length(
    gregexpr('class="enhance-group ', html, fixed = TRUE)[[1L]],
    2L
  )
  expect_match(
    html,
    'class="enhance-group enhance-group--analyses"',
    fixed = TRUE
  )
  expect_match(html, "<h3>Optional enhancements</h3>", fixed = TRUE)
  expect_match(html, "<h4>Optional analyses</h4>", fixed = TRUE)
  expect_match(html, "<h4>Optional attachments</h4>", fixed = TRUE)
  expect_match(
    html,
    'id="enhance-stage" class="builder-enhancement-stack"',
    fixed = TRUE
  )
  expect_match(
    html,
    paste(
      'class="builder-stage-section builder-stage-spatial',
      'spatial-alignment-workbench"'
    ),
    fixed = TRUE
  )
  expect_match(
    html,
    'class="enhance-group enhance-group--attachments"',
    fixed = TRUE
  )
  expect_match(html, "Tables for Extra material", fixed = TRUE)
  expect_match(
    html,
    "Add optional CSV or TSV tables to the CRB’s Extra material content.",
    fixed = TRUE
  )
  expect_match(html, "Spatial alignment", fixed = TRUE)
  expect_match(
    html,
    "Align tissue images with the spatial coordinates for each FOV or section.",
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
  expect_match(
    html,
    '<input id="enhance-rendered_for" type="text"[^>]*hidden="hidden"',
    perl = TRUE
  )
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
  expect_match(html, 'id="enhance-add_image_label"', fixed = TRUE)
  expect_false(grepl('id="enhance-image_path"', html, fixed = TRUE))
  expect_false(grepl('id="enhance-attach_image"', html, fixed = TRUE))
  expect_false(grepl('id="enhance-histology_to_retain"', html, fixed = TRUE))
  expect_match(html, "Spatial space", fixed = TRUE)
  expect_false(grepl("Transcriptome space", html, fixed = TRUE))
  expect_false(grepl(
    'id="enhance-alignment_transcriptome_plot"',
    html,
    fixed = TRUE
  ))
  expect_match(html, 'id="enhance-alignment_spatial_plot"', fixed = TRUE)
  expect_match(html, 'id="enhance-alignment_legend"', fixed = TRUE)
  expect_match(html, 'aria-label="Spatial-space cell plot"', fixed = TRUE)
  expect_match(
    html,
    'class="enhance-attachment-block enhance-attachment-block--tables"',
    fixed = TRUE
  )
  expect_false(grepl("enhance-attachment-block--spatial", html, fixed = TRUE))
  expect_false(grepl(
    'class="enhance-attachment[^\"]*builder-subcard',
    html,
    perl = TRUE
  ))
  expect_length(
    gregexpr('class="spatial-alignment-figure"', html, fixed = TRUE)[[1L]],
    1L
  )
  expect_false(grepl("Transcriptome space", html, fixed = TRUE))
  expect_false(grepl(
    'class="spatial-alignment-plot-card builder-subcard"',
    html,
    fixed = TRUE
  ))
  expect_match(
    html,
    'class="spatial-alignment-figure-header"',
    fixed = TRUE
  )
  expect_match(
    html,
    '<h5>Tables for Extra material</h5>',
    fixed = TRUE
  )
  expect_match(
    html,
    '<h3 class="spatial-alignment-title">Spatial alignment</h3>',
    fixed = TRUE
  )
  expect_match(html, "Position", fixed = TRUE)
  expect_match(html, "Scale &amp; orientation", fixed = TRUE)
  expect_match(html, "Image appearance", fixed = TRUE)
  expect_match(html, "Image opacity", fixed = TRUE)
  expect_match(html, "Point opacity", fixed = TRUE)
  expect_match(html, 'data-postfix="%"', fixed = TRUE)
  expect_match(html, "has_image", fixed = TRUE)
  expect_match(html, 'data-ns-prefix="enhance-"', fixed = TRUE)
  expect_match(html, "Point size", fixed = TRUE)
  expect_no_match(html, "Save alignment", fixed = TRUE)
  expect_match(html, "Apply transform to matching image label", fixed = TRUE)
  expect_match(html, "Reset alignment", fixed = TRUE)
  expect_match(html, 'id="enhance-alignment_status"', fixed = TRUE)
  expect_false(grepl("Remove image", html, fixed = TRUE))
  expect_match(html, 'class="spatial-alignment-layout"', fixed = TRUE)
  expect_match(
    html,
    'class="spatial-alignment-sidebar builder-controls-grid"',
    fixed = TRUE
  )
  expect_match(html, 'class="spatial-alignment-sidebar-primary"', fixed = TRUE)
  expect_match(html, 'class="spatial-alignment-sidebar-scroll"', fixed = TRUE)
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

test_that("Marker import workbench exposes explicit per-source confirmation", {
  source <- list(
    id = "source-001",
    source_name = "T_cells",
    file_name = "markers.xlsx",
    sheet = "T_cells",
    rows = 12L,
    columns = c("gene", "score", "cluster"),
    raw_table = data.frame(gene = "CD3D"),
    table = NULL,
    mapping = "single",
    cluster_column = NULL,
    cluster = "T cells",
    levels = "T cells",
    confirmed = FALSE,
    status = "confirmation_required",
    error = NULL
  )
  draft <- list(
    id = "marker-import-1",
    method = "Scanpy Wilcoxon",
    group = "cell_type",
    known_levels = c("T cells", "B cells", "NK"),
    sources = list(source),
    validation = list(
      ready = FALSE,
      errors = "unresolved_sources",
      coverage = list(
        covered = character(),
        missing = c("T cells", "B cells", "NK")
      ),
      warnings = "No imported rows for: T cells, B cells, NK"
    )
  )

  html <- builder_stage_html(builder_marker_import_ui(
    "enhance",
    groups = "cell_type",
    draft = draft
  ))

  expect_match(html, "Scanpy Wilcoxon", fixed = TRUE)
  expect_match(html, "markers.xlsx", fixed = TRUE)
  expect_match(html, "T_cells", fixed = TRUE)
  expect_match(html, "12 rows", fixed = TRUE)
  expect_match(html, 'id="enhance-marker_source_mode_source-001"', fixed = TRUE)
  expect_match(
    html,
    'id="enhance-marker_source_cluster_source-001"',
    fixed = TRUE
  )
  expect_match(html, 'data-source-id="source-001"', fixed = TRUE)
  expect_match(html, "Confirm mapping", fixed = TRUE)
  expect_match(html, 'aria-live="polite"', fixed = TRUE)
  expect_match(html, 'id="enhance-marker_import_save"', fixed = TRUE)
  expect_match(
    html,
    '<button[^>]+disabled[^>]+id="enhance-marker_import_save"',
    perl = TRUE
  )
  expect_match(html, "No imported rows for", fixed = TRUE)
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
      spatial_coordinate_transforms = list(
        `section-b` = list(rotation_degrees = 27.5, scale = 1)
      ),
      images = list(
        `section-a` = list(
          uri = "data:image/png;base64,AA==",
          bounds = list(xmin = 0, xmax = 1, ymin = 0, ymax = 1)
        )
      )
    ),
    modules = list(),
    active_section = "section-b"
  )

  expect_identical(model$attachments$tables$selected, "markers")
  expect_identical(
    model$attachments$histology$sections,
    c("section-a", "section-b")
  )
  expect_identical(model$attachments$histology$selected, "section-a")
  expect_identical(model$attachments$histology$active_section, "section-b")
  expect_identical(model$attachments$histology$coordinate_rotation, 27.5)
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

test_that("XLSX Extra material imports every non-empty worksheet", {
  skip_if_not_installed("writexl")
  path <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    list(
      Clinical = data.frame(patient = c("A", "B"), score = c(1, 2)),
      Empty = data.frame(),
      Results = data.frame(feature = "CD3D", value = 4.2)
    ),
    path
  )

  got <- builder_read_tables(path, filename = "supplement.xlsx")

  expect_named(got, c("Clinical", "Results"))
  expect_identical(
    unname(vapply(got, `[[`, character(1), "name")),
    c("supplement · Clinical", "supplement · Results")
  )
  expect_identical(got$Clinical$table$patient, c("A", "B"))
  expect_identical(got$Results$table$feature, "CD3D")
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

test_that("tissue image metadata owns the single rename and remove actions", {
  html <- builder_stage_html(builder_tissue_image_file_ui(
    "enhance",
    list(
      source = list(
        name = "/private/upload/section-a.png",
        type = "image/png",
        size = 2048
      ),
      section_id = "section-a"
    )
  ))
  expect_match(html, "builder-file-list", fixed = TRUE)
  expect_match(html, "builder-file-item", fixed = TRUE)
  expect_match(html, "section-a.png", fixed = TRUE)
  expect_match(html, "PNG · 2 KB", fixed = TRUE)
  expect_match(html, "Added", fixed = TRUE)
  expect_false(grepl("/private/upload", html, fixed = TRUE))
  expect_match(html, 'id="enhance-drop_image"', fixed = TRUE)
  expect_match(html, 'id="enhance-rename_image"', fixed = TRUE)
  expect_lt(
    regexpr('id="enhance-rename_image"', html, fixed = TRUE)[[1L]],
    regexpr('id="enhance-drop_image"', html, fixed = TRUE)[[1L]]
  )
  expect_match(html, "btn-remove-soft", fixed = TRUE)

  stage_html <- builder_stage_html(builder_spatial_alignment_ui(
    "enhance",
    list(label = "Spatial alignment", sections = "section-a")
  ))
  expect_false(grepl('id="enhance-remove_image"', stage_html, fixed = TRUE))
  expect_false(grepl('id="enhance-rename_image"', stage_html, fixed = TRUE))

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
    'observeEvent(input[["enhance-remove_image"]]',
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
    "Apply transform to matching image label?",
    fixed = TRUE
  )
})
