trekker_viewer_path <- function(...) {
  source <- testthat::test_path("../../inst/viewer", ...)
  if (file.exists(source)) {
    return(source)
  }
  file.path(system.file("viewer", package = "CerebroNexus"), ...)
}

trekker_r_path <- function(file) {
  source <- testthat::test_path("../../R", file)
  testthat::skip_if_not(
    file.exists(source),
    "R/ source tree not present (installed-package layout)"
  )
  source
}

trekker_helpers <- new.env(parent = baseenv())
trekker_helpers$`%||%` <- function(x, y) if (is.null(x)) y else x
sys.source(
  trekker_viewer_path("trekker", "helpers.R"),
  envir = trekker_helpers
)

test_that("Trekker entity labels are explicit and safe", {
  expect_identical(
    trekker_helpers$trekker_entity_label(list(entity_type = "nucleus")),
    "nucleus"
  )
  expect_identical(
    trekker_helpers$trekker_entity_label(
      list(entity_type = "nucleus"),
      plural = TRUE
    ),
    "nuclei"
  )
  expect_identical(
    trekker_helpers$trekker_entity_label(list(), plural = TRUE),
    "observations"
  )
})

test_that("official metric aliases produce semantic summaries", {
  metrics <- data.frame(
    Metrics = c(
      "Total_nuclei_from_single-nuclei_sequencing_library",
      "Nuclei_from_single-nuclei_sequencing_library_found_in_Trekker_library",
      paste0(
        "Nuclei_from_single-nuclei_sequencing_library_found_in_",
        "Trekker_library_with_valid_spatial_barcodes"
      ),
      "Total_nuclei_positioned",
      "Total_nuclei_positioned_with_1_spatial_location",
      "Pct_readpairs_with_proper_structure",
      paste0(
        "Pct_readpairs_matched_to_single_nuclei_barcodes_with_",
        "valid_spatial_barcodes"
      ),
      "Pct_nuclei_positioned",
      "Median_useful_reads_per_nuclei",
      "Nuclei_0",
      "Nuclei_1",
      "Nuclei_>=4"
    ),
    Value = c(
      "10,000",
      "9,000",
      "8,000",
      "7,000",
      "6,500",
      "91.5%",
      "73.2",
      "70",
      "1,149",
      "50",
      "6,500",
      "25"
    ),
    stringsAsFactors = FALSE
  )

  summary <- trekker_helpers$trekker_positioning_summary(metrics)
  expect_identical(
    summary$funnel$stage,
    c(
      "Input nuclei",
      "Found in Trekker",
      "Valid spatial barcodes",
      "Positioned",
      "Confidently positioned"
    )
  )
  expect_equal(summary$funnel$value, c(10000, 9000, 8000, 7000, 6500))
  expect_identical(summary$distribution$bucket, c("0", "1", "4+"))
  expect_equal(summary$qc$value, c(91.5, 73.2, 70, 1149))

  missing <- trekker_helpers$trekker_positioning_summary(metrics[1, ])
  expect_equal(nrow(missing$funnel), 1L)
  expect_equal(nrow(missing$distribution), 0L)
  expect_false(any(missing$qc$value == 0))
})

test_that("official metrics expand into focused run-QC groups", {
  metrics <- data.frame(
    Metrics = c(
      "Total_readpairs_in_Trekker_library",
      "Readpairs_with_proper_structure",
      "Readpairs_matched_single_nuclei_barcodes",
      paste0(
        "Readpairs_matched_to_single_nuclei_barcodes_with_",
        "valid_spatial_barcodes"
      ),
      "Pct_readpairs_with_proper_structure",
      "Pct_useful_reads",
      "Median_reads_per_nuclei",
      "Median_useful_reads_per_nuclei",
      "Nuclei_o_2",
      "Nuclei_salvaged_2"
    ),
    Value = c(1000, 900, 700, 600, 90, 60, 120, 80, 40, 25),
    stringsAsFactors = FALSE
  )

  details <- trekker_helpers$trekker_run_qc_details(metrics)
  expect_identical(
    details$reads$stage,
    c(
      "Total read pairs",
      "Proper structure",
      "Matched nucleus barcodes",
      "Matched + valid spatial barcodes"
    )
  )
  expect_equal(details$rates$value, c(90, 60))
  expect_setequal(details$depth$measure, c("Reads", "Useful reads"))
  expect_identical(
    details$positioning$stage,
    c("Initially 2", "Salvaged from 2")
  )
})

test_that("spatial QC derives display values from nucleus metadata", {
  metadata <- data.frame(
    nCount_RNA = c(9, 99),
    nFeature_RNA = c(4, 49),
    percent.mt = c(1, 5),
    row.names = c("a", "b")
  )
  coordinates <- data.frame(
    barcode = c("b", "a"),
    x = c(2, 1),
    y = c(4, 3)
  )

  qc <- trekker_helpers$trekker_spatial_qc(metadata, coordinates)
  expect_setequal(
    unique(qc$metric),
    c("Mitochondrial UMI (%)", "log10 RNA UMI", "log10 detected genes")
  )
  expect_equal(
    qc$value[qc$barcode == "b" & qc$metric == "log10 RNA UMI"],
    2
  )
  expect_identical(unique(qc$barcode), c("b", "a"))
})

test_that("marker dot summaries retain cluster and expression evidence", {
  markers <- data.frame(
    gene = c("A", "B", "C", "A"),
    cluster = c("0", "0", "1", "1"),
    p_val_adj = c(0.01, 0.02, 0.01, 0.03),
    avg_log2FC = c(2, 1, 3, 0.5),
    pct.1 = c(0.8, 0.7, 0.9, 0.6),
    pct.2 = c(0.2, 0.3, 0.1, 0.4),
    stringsAsFactors = FALSE
  )
  expression <- matrix(
    c(2, 1, 0, 0, 0, 2, 1, 0, 0, 0, 3, 4),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(c("A", "B", "C"), c("a", "b", "c", "d"))
  )

  dot <- trekker_helpers$trekker_marker_dot(
    markers,
    expression,
    groups = c("0", "0", "1", "1"),
    n_per_cluster = 1L
  )
  expect_setequal(unique(dot$gene), c("A", "C"))
  expect_setequal(unique(dot$cluster), c("0", "1"))
  expect_equal(nrow(dot), 4L)
  expect_true(all(dot$percent >= 0 & dot$percent <= 100))
  expect_true(all(is.finite(dot$scaled_average)))
})

test_that("top spatial genes follow official Moran rank and availability", {
  moran <- data.frame(
    gene = c("B", "A", "C"),
    moransi.spatially.variable.rank = c(2, 1, 3),
    stringsAsFactors = FALSE
  )
  expect_identical(
    trekker_helpers$trekker_top_spatial_genes(
      moran,
      available_genes = c("B", "C"),
      n = 2L
    ),
    c("B", "C")
  )
})

test_that("marker and Moran results remain linked by every marker row", {
  markers <- data.frame(
    gene = c("A", "A", "B"),
    cluster = c("1", "2", "1"),
    p_val = c(0.1, 0.2, 0.3),
    avg_log2FC = c(2, 1, 3),
    pct.1 = c(0.8, 0.7, 0.9),
    pct.2 = c(0.2, 0.4, 0.1),
    p_val_adj = c(0.01, 0.02, 0.03),
    stringsAsFactors = FALSE
  )
  moran <- data.frame(
    gene = c("A", "C"),
    MoransI_observed = c(0.4, 0.2),
    MoransI_p.value = c(0.001, 0.2),
    moransi.spatially.variable = c(TRUE, FALSE),
    moransi.spatially.variable.rank = c(1, 2),
    stringsAsFactors = FALSE
  )

  joined <- trekker_helpers$trekker_marker_moran(markers, moran)
  expect_equal(nrow(joined), 2L)
  expect_identical(joined$cluster, c("1", "2"))
  expect_equal(joined$pct_delta, c(0.6, 0.3))

  top <- trekker_helpers$trekker_top_markers(markers, "1", n = 1L)
  expect_identical(top$gene, "A")
  complete <- trekker_helpers$trekker_top_markers(markers, "1", n = NULL)
  expect_equal(nrow(complete), 2L)
})

test_that("active cohort QC is separated from displayed QC", {
  metadata <- data.frame(
    nCount_RNA = c(10, 20, 30),
    nFeature_RNA = c(3, 5, 9),
    percent.mt = c(1, 2, 8),
    row.names = c("a", "b", "c")
  )
  result <- trekker_helpers$trekker_cohort_qc(metadata, c("b", "c"))
  expect_setequal(unique(result$cohort), c("Displayed", "Active cohort"))
  expect_equal(nrow(result[result$field == "nCount_RNA", ]), 5L)
  expect_equal(
    result$value[
      result$field == "nCount_RNA" & result$cohort == "Active cohort"
    ],
    c(20, 30)
  )

  no_selection <- trekker_helpers$trekker_cohort_qc(metadata, character())
  expect_identical(unique(no_selection$cohort), "Displayed")
  expect_equal(nrow(no_selection), 9L)
})

test_that("QC ranges resolve directly to nucleus barcodes", {
  metadata <- data.frame(
    nCount_RNA = c(1, 2, 3, 4, NA),
    row.names = letters[1:5]
  )

  expect_identical(
    trekker_helpers$trekker_qc_range(metadata, "nCount_RNA", c(2, 3)),
    c("b", "c")
  )
  expect_identical(
    trekker_helpers$trekker_qc_range(
      metadata,
      "nCount_RNA",
      c(2, 3),
      outside = TRUE
    ),
    c("a", "d")
  )
})

test_that("gene evidence joins expression, markers, and Moran results", {
  expression <- matrix(
    c(1, 0, 3, 1, 4, 4, 0, 0),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("A", "B"), letters[1:4])
  )
  markers <- data.frame(
    gene = c("A", "A", "B"),
    cluster = c("0", "1", "0"),
    avg_log2FC = c(2, 1, 3),
    stringsAsFactors = FALSE
  )
  moran <- data.frame(
    gene = c("A", "B"),
    MoransI_observed = c(0.4, 0.2),
    stringsAsFactors = FALSE
  )

  result <- trekker_helpers$trekker_gene_evidence(
    "A",
    expression,
    c("0", "0", "1", "1"),
    markers,
    moran
  )

  expect_equal(result$distribution$average, c(0.5, 2))
  expect_equal(result$distribution$percent, c(50, 100))
  expect_equal(nrow(result$markers), 2L)
  expect_identical(result$moran$gene, "A")
})

test_that("multi-gene controls do not rebuild from their own inputs", {
  server <- paste(
    readLines(
      trekker_viewer_path("trekker", "server.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  controls <- strsplit(
    server,
    'output[["trekker_multigene_controls"]] <- renderUI({',
    fixed = TRUE
  )[[1L]][[2L]]
  controls <- strsplit(
    controls,
    "trekker_multigene_results <- reactive({",
    fixed = TRUE
  )[[1L]][[1L]]

  expect_false(
    grepl('input[["trekker_score_genes"]]', controls, fixed = TRUE)
  )
  expect_false(
    grepl('input[["trekker_multigene_mode"]]', controls, fixed = TRUE)
  )
  expect_match(controls, '"Mean expression" = "mean"', fixed = TRUE)
  expect_match(controls, '"Separate panels" = "separate"', fixed = TRUE)
  expect_match(controls, '"RGB co-expression" = "rgb"', fixed = TRUE)
  expect_match(controls, "maxItems = 6", fixed = TRUE)
})

test_that("multi-gene plot implements mean, separate, and RGB modes", {
  server <- paste(
    readLines(
      trekker_viewer_path("trekker", "server.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  block <- strsplit(
    server,
    'output[["trekker_multigene_plot"]] <- plotly::renderPlotly({',
    fixed = TRUE
  )[[1L]][[2L]]
  block <- strsplit(
    block,
    "trekker_spatial_gallery_results <- reactive({",
    fixed = TRUE
  )[[1L]][[1L]]

  expect_match(block, 'identical(result$mode, "separate")', fixed = TRUE)
  expect_match(block, 'identical(result$mode, "rgb")', fixed = TRUE)
  expect_match(block, "colMeans(result$expression", fixed = TRUE)
  expect_match(block, "blend_genes_to_rgb(", fixed = TRUE)
})

test_that("cluster spatial profiles quantify footprint and mixing", {
  coordinates <- data.frame(
    barcode = letters[1:6],
    x = c(0, 1, 0, 10, 11, 10),
    y = c(0, 0, 1, 10, 10, 11)
  )
  profiles <- trekker_helpers$trekker_spatial_profiles(
    coordinates,
    rep(c("A", "B"), each = 3L)
  )

  expect_identical(profiles$group, c("A", "B"))
  expect_equal(profiles$nuclei, c(3L, 3L))
  expect_equal(profiles$hull_area, c(0.5, 0.5))
  expect_equal(profiles$nearest_same_group, c(1, 1))
})

test_that("local neighbourhood scores retain nucleus identity", {
  coordinates <- data.frame(
    barcode = letters[1:6],
    x = c(0, 1, 2, 20, 21, 22),
    y = 0
  )
  local <- trekker_helpers$trekker_local_neighbourhood(
    coordinates,
    rep(c("A", "B"), each = 3L),
    target = "A",
    k = 1L
  )

  expect_identical(local$barcode, letters[1:6])
  expect_equal(local$target_share, c(1, 1, 1, 0, 0, 0))
  expect_true(all(is.finite(local$enrichment)))
})

test_that("section summaries compare composition and available QC", {
  coordinates <- data.frame(
    barcode = letters[1:4],
    section = rep(c("slice1", "slice2"), each = 2L),
    x = 1:4,
    y = 1:4
  )
  metadata <- data.frame(
    nCount_RNA = c(10, 20, 30, 50),
    percent.mt = c(1, 3, 2, 4),
    row.names = letters[1:4]
  )
  summary <- trekker_helpers$trekker_section_summary(
    coordinates,
    metadata,
    c("A", "B", "A", "B")
  )

  expect_equal(nrow(summary$composition), 4L)
  expect_equal(summary$composition$share, rep(0.5, 4L))
  expect_setequal(unique(summary$qc$field), c("nCount_RNA", "percent.mt"))
  expect_equal(
    summary$qc$value[
      summary$qc$section == "slice2" & summary$qc$field == "nCount_RNA"
    ],
    40
  )
})

test_that("coordinate audits recognise the official Y-axis inversion", {
  coordinates <- data.frame(
    barcode = c("a", "b"),
    section = "slice1",
    x = c(1, 2),
    y = c(3, 4)
  )
  spatial <- cbind(SPATIAL_1 = c(1, 2), SPATIAL_2 = c(-3, -4))
  rownames(spatial) <- c("a", "b")

  audit <- trekker_helpers$trekker_coordinate_audit(
    coordinates,
    metadata_keys = c("a", "b"),
    spatial = spatial
  )

  expect_true(all(audit$status == "Pass"))
  expect_match(
    audit$detail[audit$check == "SPATIAL coordinate agreement"],
    "Y inverted",
    fixed = TRUE
  )
})

test_that("KNN overlap compares two layouts on shared barcodes", {
  first <- cbind(x = c(0, 1, 2, 20), y = c(0, 0, 0, 0))
  rownames(first) <- letters[1:4]
  result <- trekker_helpers$trekker_knn_overlap(
    first,
    first,
    groups = c("A", "A", "A", "B"),
    k = 1L
  )

  expect_equal(result$per_nucleus$overlap, rep(1, 4L))
  expect_equal(result$summary$mean_overlap, c(1, 1))
})

test_that("neighbourhood permutations are deterministic after sampling", {
  coordinates <- data.frame(
    x = seq_len(1200L),
    y = seq_len(1200L) %% 17L
  )
  groups <- rep(c("A", "B", "C"), length.out = 1200L)
  first <- trekker_helpers$trekker_neighbourhood(
    coordinates,
    groups,
    k = 2L,
    maximum = 1000L,
    permutations = 9L,
    seed = 7L
  )
  second <- trekker_helpers$trekker_neighbourhood(
    coordinates,
    groups,
    k = 2L,
    maximum = 1000L,
    permutations = 9L,
    seed = 7L
  )

  expect_equal(first$n_sample, 1000L)
  expect_true(first$sampled)
  expect_true(all(is.finite(first$adjacency$p_value)))
  expect_equal(first$adjacency$p_value, second$adjacency$p_value)
  expect_equal(first$adjacency$p_adj, second$adjacency$p_adj)
})

test_that("Trekker selection payloads resolve to nucleus barcodes", {
  expect_identical(
    trekker_helpers$trekker_selection_ids(list(
      x = c(1, 2),
      y = c(3, 4),
      ids = c("a", "b")
    )),
    c("a", "b")
  )
  expect_identical(trekker_helpers$trekker_selection_ids(NULL), character())
})

test_that("registered numeric groups remain categorical", {
  metadata <- data.frame(
    cluster = c(1, 2),
    score = c(0.1, 0.2),
    row.names = c("a", "b")
  )
  cluster <- trekker_helpers$trekker_categorical_values(
    metadata,
    c("b", "a"),
    "meta:cluster",
    registered_groups = "cluster"
  )
  expect_identical(cluster, c("2", "1"))
  expect_null(trekker_helpers$trekker_categorical_values(
    metadata,
    c("a", "b"),
    "meta:score"
  ))
})

test_that("density bins preserve counts and coordinate limits", {
  coordinates <- data.frame(x = c(0, 1, 2, 3), y = c(0, 1, 2, 3))
  density <- trekker_helpers$trekker_density_bins(coordinates, bins = 2L)
  expect_equal(sum(density$count), 4L)
  expect_equal(range(c(density$x0, density$x1)), c(0, 3))
  expect_equal(range(c(density$y0, density$y1)), c(0, 3))
})

test_that("six-neighbour summaries are deterministic and symmetric", {
  expect_identical(
    trekker_helpers$trekker_sample_indices(2000L, 1000L),
    trekker_helpers$trekker_sample_indices(2000L, 1000L)
  )
  expect_length(
    trekker_helpers$trekker_sample_indices(2000L, 1000L),
    1000L
  )

  coordinates <- data.frame(
    x = c(0:3, 20:23),
    y = c(0, 0, 1, 1, 0, 0, 1, 1)
  )
  result <- trekker_helpers$trekker_neighbourhood(
    coordinates,
    rep(c("A", "B"), each = 4L),
    k = 2L
  )
  ab <- result$adjacency[
    result$adjacency$group_a == "A" &
      result$adjacency$group_b == "B",
  ]
  ba <- result$adjacency[
    result$adjacency$group_a == "B" &
      result$adjacency$group_b == "A",
  ]
  expect_equal(ab$observed, ba$observed)
  expect_equal(ab$enrichment, ba$enrichment)
  expect_equal(result$n_sample, 8L)
  expect_false(result$sampled)
  expect_setequal(unique(result$composition$group), c("A", "B"))
})

test_that("numeric neighbourhood groups retain natural cluster order", {
  coordinates <- data.frame(x = 0:17, y = (0:17) %% 3)
  result <- trekker_helpers$trekker_neighbourhood(
    coordinates,
    as.character(0:17),
    k = 1L
  )

  expect_identical(unique(result$adjacency$group_a), as.character(0:17))
  expect_identical(
    unique(result$composition$selected_group),
    as.character(0:17)
  )
})

test_that("Trekker import records the entity type", {
  source <- paste(
    readLines(trekker_r_path("trekker-data.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(source, 'entity_type = "nucleus"', fixed = TRUE)
})

test_that("schema-v1 Trekker payloads declare positioned nuclei", {
  contract <- new.env(parent = baseenv())
  contract$`%||%` <- function(x, y) if (is.null(x)) y else x
  sys.source(trekker_r_path("trekker-data.R"), envir = contract)
  payload <- list(
    schema_version = 1L,
    source = list(),
    files = list(
      location = list(
        category = "location",
        name = "location.csv",
        path = "sample.trekker/location.csv",
        sha256 = paste(rep("a", 64L), collapse = ""),
        size = 1
      )
    ),
    coordinates = data.frame(
      barcode = "a",
      section = "slice1",
      x = 1,
      y = 2
    )
  )
  expect_error(
    contract$.validate_trekker_payload(payload),
    "entity_type"
  )
  payload$entity_type <- "nucleus"
  expect_invisible(contract$.validate_trekker_payload(payload))
})

test_that("Trekker positioning evidence aligns vendor barcodes to CRB cells", {
  contract <- new.env(parent = baseenv())
  contract$`%||%` <- function(x, y) if (is.null(x)) y else x
  sys.source(trekker_r_path("trekker-data.R"), envir = contract)

  location <- tempfile(fileext = "_Location_ConfPositionedNuclei.csv")
  positioning <- tempfile(pattern = "coords_", fileext = ".txt")
  utils::write.csv(
    data.frame(SPATIAL_1 = c(1, 2), SPATIAL_2 = c(3, 4)),
    location,
    row.names = c("AA-1", "BB-1")
  )
  utils::write.table(
    data.frame(
      cell_bc = c("BB", "AA"),
      number_clusters_o = c(2, 1),
      SB_total = c(20, 10),
      proportion_SB_noise = c(0.2, 0.1),
      rep_status = c(1, 0),
      number_clusters = c(1, 1)
    ),
    positioning,
    row.names = FALSE,
    quote = FALSE
  )

  payload <- contract$.build_trekker_payload(
    list(location = location, positioning = positioning),
    cells = c("AA-1", "BB-1"),
    sidecar_name = "sample.trekker",
    declared_by = "test"
  )

  expect_identical(payload$positioning$barcode, c("AA-1", "BB-1"))
  expect_equal(payload$positioning$SB_total, c(10, 20))
  expect_identical(payload$files$positioning$category, "positioning")
  expect_invisible(contract$.validate_trekker_payload(payload))
})

test_that("Trekker positioning evidence rejects ambiguous barcode suffixes", {
  contract <- new.env(parent = baseenv())
  contract$`%||%` <- function(x, y) if (is.null(x)) y else x
  sys.source(trekker_r_path("trekker-data.R"), envir = contract)

  location <- tempfile(fileext = "_Location_ConfPositionedNuclei.csv")
  positioning <- tempfile(pattern = "coords_", fileext = ".txt")
  utils::write.csv(
    data.frame(SPATIAL_1 = 1, SPATIAL_2 = 2),
    location,
    row.names = "AA-3"
  )
  utils::write.table(
    data.frame(
      cell_bc = c("AA-1", "AA-2"),
      number_clusters = c(1, 2)
    ),
    positioning,
    row.names = FALSE,
    quote = FALSE
  )

  expect_error(
    contract$.build_trekker_payload(
      list(location = location, positioning = positioning),
      cells = "AA-3",
      sidecar_name = "sample.trekker",
      declared_by = "test"
    ),
    "ambiguous"
  )
})

test_that("Shiny app enrichment can explicitly replace positioning evidence", {
  contract <- new.env(parent = baseenv())
  contract$`%||%` <- function(x, y) if (is.null(x)) y else x
  contract$setNames <- stats::setNames
  sys.source(trekker_r_path("trekker-data.R"), envir = contract)

  replacement <- contract$.normalize_trekker_replace(
    "positioning",
    "dataset"
  )

  expect_identical(replacement$dataset, "positioning")
})

test_that("Trekker app enrichment accepts omitted image lists", {
  contract <- new.env(parent = baseenv())
  contract$`%||%` <- function(x, y) if (is.null(x)) y else x
  sys.source(trekker_r_path("trekker-data.R"), envir = contract)

  expect_identical(
    contract$.trekker_named_list(list(), "`trekker_images`"),
    list()
  )
})

test_that("positioning evidence becomes readable per-nucleus metadata", {
  positioning <- data.frame(
    barcode = c("a", "b"),
    source_barcode = c("a", "b"),
    number_clusters_o = c(2, 1),
    SB_total = c(100, 80),
    proportion_SB_noise = c(0.2, 0.1),
    proportion_SB_UMI_top_cluster = c(0.7, 0.8),
    rep_status = c(1, 0),
    number_clusters = c(1, 1),
    stringsAsFactors = FALSE
  )

  metadata <- trekker_helpers$trekker_positioning_metadata(positioning)
  expect_identical(rownames(metadata), c("a", "b"))
  expect_true(all(
    c(
      "Trekker: Candidate locations (initial)",
      "Trekker: Spatial barcodes",
      "Trekker: Noise barcode fraction",
      "Trekker: Top-location UMI fraction",
      "Trekker: Recovery status"
    ) %in%
      names(metadata)
  ))
  expect_identical(
    as.character(metadata[["Trekker: Recovery status"]]),
    c("Bead-removal recovery", "Original positioning")
  )

  record <- trekker_helpers$trekker_positioning_record(positioning, "a")
  expect_true(all(c("Field", "Value") %in% names(record)))
  expect_identical(
    record$Value[record$Field == "Recovery status"],
    "Bead-removal recovery"
  )
})

test_that("Viewer exposes evidence filtering with honest layout wording", {
  read_viewer <- function(...) {
    paste(
      readLines(trekker_viewer_path(...), warn = FALSE),
      collapse = "\n"
    )
  }
  ui <- read_viewer("trekker", "UI.R")
  server <- read_viewer("trekker", "server.R")

  expect_match(ui, "trekker_positioning_filter_controls", fixed = TRUE)
  expect_match(server, "trekker_positioning_metadata", fixed = TRUE)
  expect_match(server, "trekker_positioning_record", fixed = TRUE)
  expect_match(server, "trekker_select_positioning_range", fixed = TRUE)
  expect_match(server, "trekker_clear_positioning_filter", fixed = TRUE)
  expect_match(server, "trekker_positioning_cohort", fixed = TRUE)
  expect_match(server, "trekker_analysis_coordinates", fixed = TRUE)
  expect_match(
    ui,
    "UMAP-layout vs physical neighbourhood overlap",
    fixed = TRUE
  )
  expect_match(server, "exploratory layout agreement", fixed = TRUE)
  expect_no_match(ui, "UMAP–physical neighbourhood agreement", fixed = TRUE)
})

test_that("cluster spatial profiles bound quadratic nearest-neighbour work", {
  coordinates <- data.frame(x = seq_len(120L), y = seq_len(120L) %% 7L)
  groups <- rep(c("A", "B"), 60L)
  result <- trekker_helpers$trekker_spatial_profiles(
    coordinates,
    groups,
    maximum = 40L
  )

  expect_identical(attr(result, "n_sample"), 40L)
  expect_true(attr(result, "sampled"))
  expect_equal(sum(result$nuclei), 40L)
})

test_that("Trekker analytics keep the primary map above linked tabs", {
  read_viewer <- function(...) {
    paste(
      readLines(
        trekker_viewer_path(...),
        warn = FALSE
      ),
      collapse = "\n"
    )
  }
  ui <- read_viewer("trekker", "UI.R")
  server <- read_viewer("trekker", "server.R")
  css <- read_viewer("www", "trekker.css")

  for (tab in c(
    '"Overview"',
    '"Cluster markers"',
    '"Spatial genes"',
    '"Neighbourhood"',
    '"Files"'
  )) {
    expect_match(ui, tab, fixed = TRUE)
  }
  expect_lt(
    regexpr('cerebroCellViewOutput("trekker_projection")', ui, fixed = TRUE),
    regexpr('id = "trekker_analysis_tabs"', ui, fixed = TRUE)
  )
  for (output in c(
    "trekker_positioning_funnel",
    "trekker_position_distribution",
    "trekker_run_qc_details_plot",
    "trekker_spatial_qc_plot",
    "trekker_marker_plot",
    "trekker_marker_dot_plot",
    "trekker_moran_plot",
    "trekker_spatial_gene_gallery",
    "trekker_marker_moran_plot",
    "trekker_cohort_qc_plot",
    "trekker_adjacency_plot",
    "trekker_neighbour_composition"
  )) {
    expect_match(ui, output, fixed = TRUE)
    expect_match(server, output, fixed = TRUE)
  }
  expect_match(server, "trekker_entity_label", fixed = TRUE)
  expect_match(server, 'source = "trekker_markers"', fixed = TRUE)
  expect_match(server, 'source = "trekker_moran"', fixed = TRUE)
  expect_match(server, 'source = "trekker_evidence"', fixed = TRUE)
  expect_gte(
    lengths(regmatches(
      server,
      gregexpr("req(nrow(table) > 0L)", server, fixed = TRUE)
    )),
    2L
  )
  expect_match(server, "Categorical cluster metadata", fixed = TRUE)
  expect_match(css, ".tk-analysis-tabs", fixed = TRUE)
  expect_false(grepl(".plotly.html-widget-output:empty", css, fixed = TRUE))
})

test_that("marker-spatial evidence uses a horizontal legend below the plot", {
  server <- paste(
    readLines(
      trekker_viewer_path("trekker", "server.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  block <- strsplit(
    server,
    'output[["trekker_marker_moran_plot"]] <-',
    fixed = TRUE
  )[[1L]][[2L]]
  block <- strsplit(block, "observeEvent(", fixed = TRUE)[[1L]][[1L]]

  expect_match(block, 'orientation = "h"', fixed = TRUE)
  expect_match(block, 'xanchor = "center"', fixed = TRUE)
  expect_match(block, "y = -0.22", fixed = TRUE)
})

test_that("neighbourhood heatmap keeps numeric cluster labels categorical", {
  server <- paste(
    readLines(
      trekker_viewer_path("trekker", "server.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  block <- strsplit(
    server,
    'output[["trekker_adjacency_plot"]] <-',
    fixed = TRUE
  )[[1L]][[2L]]
  block <- strsplit(block, "observeEvent(", fixed = TRUE)[[1L]][[1L]]

  expect_gte(
    lengths(regmatches(
      block,
      gregexpr('type = "category"', block, fixed = TRUE)
    )),
    2L
  )
  expect_gte(
    lengths(regmatches(
      block,
      gregexpr("categoryarray = groups", block, fixed = TRUE)
    )),
    2L
  )

  composition <- strsplit(
    server,
    'output[["trekker_neighbour_composition"]] <-',
    fixed = TRUE
  )[[1L]][[2L]]
  composition <- strsplit(
    composition,
    "observeEvent(",
    fixed = TRUE
  )[[1L]][[1L]]
  expect_match(composition, 'type = "category"', fixed = TRUE)
  expect_match(composition, "categoryarray = groups", fixed = TRUE)
})

test_that("Trekker density is an optional Canvas layer in Settings", {
  read_viewer <- function(...) {
    paste(
      readLines(trekker_viewer_path(...), warn = FALSE),
      collapse = "\n"
    )
  }
  ui <- read_viewer("coordinated_views", "UI.R")
  server <- read_viewer("trekker", "server.R")
  utility <- read_viewer("utility_functions.R")
  javascript <- read_viewer("www", "cell_views.js")

  expect_match(ui, '"cv-trekker-density"', fixed = TRUE)
  expect_match(server, 'input[["cv-trekker-density"]]', fixed = TRUE)
  expect_match(server, "trekker_density_bins", fixed = TRUE)
  expect_match(
    utility,
    "panel$density <- lapply(panel$density, wire_array)",
    fixed = TRUE
  )
  expect_match(javascript, "drawDensity", fixed = TRUE)
  expect_match(javascript, "panel.density", fixed = TRUE)
})

test_that("Trekker advanced analyses are wired into the Viewer", {
  read_viewer <- function(...) {
    paste(
      readLines(trekker_viewer_path(...), warn = FALSE),
      collapse = "\n"
    )
  }
  ui <- read_viewer("trekker", "UI.R")
  server <- read_viewer("trekker", "server.R")
  javascript <- read_viewer("www", "cell_views.js")

  outputs <- c(
    "trekker_qc_filter_controls",
    "trekker_selection_export",
    "trekker_gene_evidence_maps",
    "trekker_gene_evidence_distribution",
    "trekker_gene_evidence_table",
    "trekker_multigene_plot",
    "trekker_spatial_profile_plot",
    "trekker_spatial_profile_table",
    "trekker_local_neighbourhood_plot",
    "trekker_knn_overlap_plot",
    "trekker_section_composition",
    "trekker_section_qc",
    "trekker_audit_table"
  )
  for (output in outputs) {
    expect_match(ui, output, fixed = TRUE)
    expect_match(server, output, fixed = TRUE)
  }
  expect_match(ui, '"Sections"', fixed = TRUE)
  expect_match(server, "trekker_download_selection", fixed = TRUE)
  expect_match(server, '"cell_view_set_selection"', fixed = TRUE)
  expect_match(
    javascript,
    "Shiny.addCustomMessageHandler('cell_view_set_selection'",
    fixed = TRUE
  )
})

test_that("Trekker selection export stays inside the active cohort bar", {
  ui <- paste(
    readLines(
      trekker_viewer_path("trekker", "UI.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  shared_ui <- paste(
    readLines(
      trekker_viewer_path("shiny_UI.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    ui,
    paste0(
      "cerebroSelectionStatus\\([\\s\\S]{0,300}",
      "extra_actions = uiOutput\\(\"trekker_selection_export\"\\)"
    ),
    perl = TRUE
  )
  expect_match(shared_ui, "extra_actions = NULL", fixed = TRUE)
  expect_match(
    shared_ui,
    paste0(
      "class = \"cerebro-selection-actions\"[\\s\\S]{0,120}",
      "extra_actions"
    ),
    perl = TRUE
  )
})

test_that("neighbourhood heatmap reports permutation evidence", {
  server <- paste(
    readLines(
      trekker_viewer_path("trekker", "server.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  block <- strsplit(
    server,
    'output[["trekker_adjacency_plot"]] <-',
    fixed = TRUE
  )[[1L]][[2L]]
  block <- strsplit(block, "observeEvent(", fixed = TRUE)[[1L]][[1L]]

  expect_match(block, "Empirical P", fixed = TRUE)
  expect_match(block, "BH-adjusted P", fixed = TRUE)
  expect_match(server, "exploratory summaries", fixed = TRUE)
})

test_that("local neighbourhood waits for a complete reactive result", {
  server <- paste(
    readLines(
      trekker_viewer_path("trekker", "server.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  block <- strsplit(
    server,
    'output[["trekker_local_neighbourhood_plot"]] <-',
    fixed = TRUE
  )[[1L]][[2L]]
  block <- strsplit(block, 'trekker_knn_overlap_results <-', fixed = TRUE)[[
    1L
  ]][[1L]]

  expect_match(
    block,
    'all(c("barcode", "group", "target_share", "enrichment") %in% names(table))',
    fixed = TRUE
  )
})

test_that("Trekker composite plots keep titles and scales readable", {
  server <- paste(
    readLines(
      trekker_viewer_path("trekker", "server.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  gene_maps <- strsplit(
    server,
    'output[["trekker_gene_evidence_maps"]] <-',
    fixed = TRUE
  )[[1L]][[2L]]
  gene_maps <- strsplit(
    gene_maps,
    'output[["trekker_gene_evidence_distribution"]] <-',
    fixed = TRUE
  )[[1L]][[1L]]
  spatial_profiles <- strsplit(
    server,
    'output[["trekker_spatial_profile_plot"]] <-',
    fixed = TRUE
  )[[1L]][[2L]]
  spatial_profiles <- strsplit(
    spatial_profiles,
    'output[["trekker_spatial_profile_table"]] <-',
    fixed = TRUE
  )[[1L]][[1L]]
  section_qc <- strsplit(
    server,
    'output[["trekker_section_qc"]] <-',
    fixed = TRUE
  )[[1L]][[2L]]
  section_qc <- strsplit(
    section_qc,
    "trekker_current_crb <-",
    fixed = TRUE
  )[[1L]][[1L]]

  expect_match(gene_maps, "annotations = trekker_subplot_titles", fixed = TRUE)
  expect_match(spatial_profiles, 'textposition = "none"', fixed = TRUE)
  expect_match(section_qc, "plotly::subplot", fixed = TRUE)
  expect_match(section_qc, 'textposition = "none"', fixed = TRUE)
  expect_false(grepl("split = ~field", section_qc, fixed = TRUE))
})

test_that("Trekker QC fields have readable labels", {
  expect_equal(
    trekker_helpers$trekker_metric_label(
      c("nUMI", "nGene", "percent.mt")
    ),
    c("RNA UMI", "Detected genes", "Mitochondrial UMI (%)")
  )
})
