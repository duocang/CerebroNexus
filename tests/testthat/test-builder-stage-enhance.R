builder_stage_contract_source_runtime(environment())

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

test_that("table inventory failures never expose a server-side upload path", {
  directory <- withr::local_tempdir()

  got <- builder_table_inventory(
    directory,
    filename = "supplement.csv"
  )

  expect_identical(got$error, "File not found.")
  expect_false(grepl(directory, got$error, fixed = TRUE))
})

test_that("background table inventory returns metadata without source paths", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("sample,value", "A,1"), path)

  got <- builder_table_inventory_metadata(path, "supplement.csv")

  expect_named(got, "supplement")
  expect_null(got$supplement$source_path)
  expect_identical(got$supplement$workbook_name, "supplement.csv")
  expect_identical(got$supplement$sheet_name, "supplement")
})

test_that("XLSX Extra material inventories worksheets without reading them", {
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

  got <- builder_table_inventory(path, filename = "supplement.xlsx")

  expect_named(got, c("Clinical", "Empty", "Results"))
  expect_identical(
    unname(vapply(got, `[[`, character(1), "name")),
    c("supplement · Clinical", "supplement · Empty", "supplement · Results")
  )
  expect_identical(
    unname(vapply(got, `[[`, character(1), "workbook_name")),
    rep("supplement.xlsx", 3L)
  )
  expect_identical(
    unname(vapply(got, `[[`, character(1), "sheet_name")),
    c("Clinical", "Empty", "Results")
  )
  expect_identical(
    unname(vapply(got, `[[`, character(1), "display_name")),
    c("Clinical", "Empty", "Results")
  )
  expect_null(got$Clinical$table)
  expect_identical(
    builder_read_table_source(got$Results)$table$feature,
    "CD3D"
  )
})

test_that("Extra material retains workbook and source-sheet labels for Viewer", {
  skip_if_not_installed("SeuratObject")
  object <- SeuratObject::pbmc_small
  tables <- list(
    list(
      name = "30mins",
      workbook_name = "All samples",
      file_name = "all-samples.xlsx",
      sheet_name = "30mins",
      display_name = "30mins",
      table = data.frame(value = 1)
    )
  )

  attached <- builder_attach_tables(object, tables)

  expect_identical(attached@misc$extra_material$tables$`30mins`$value, 1)
  expect_identical(
    attached@misc$extra_material$table_index$`30mins`,
    list(
      workbook_name = "All samples",
      file_name = "all-samples.xlsx",
      sheet_name = "30mins",
      display_name = "30mins"
    )
  )
})

test_that("legacy XLS Extra material inventories every worksheet", {
  path <- readxl::readxl_example("datasets.xls")

  got <- builder_table_inventory(path, filename = "legacy.xls")

  expect_named(got, readxl::excel_sheets(path))
  expect_true(all(vapply(
    got,
    function(record) !is.null(record$source_path),
    logical(1)
  )))
  expect_true(all(startsWith(
    vapply(got, `[[`, character(1), "name"),
    "legacy · "
  )))
})

test_that("XLSM Extra material reads a selected sheet only when needed", {
  skip_if_not_installed("writexl")
  xlsx <- withr::local_tempfile(fileext = ".xlsx")
  xlsm <- withr::local_tempfile(fileext = ".xlsm")
  writexl::write_xlsx(list(Data = data.frame(value = c(1, 2))), xlsx)
  file.copy(xlsx, xlsm, overwrite = TRUE)

  got <- builder_table_inventory(xlsm, filename = "macro.xlsm")

  expect_named(got, "Data")
  expect_identical(got$Data$name, "macro · Data")
  expect_identical(builder_read_table_source(got$Data)$table$value, c(1, 2))
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
