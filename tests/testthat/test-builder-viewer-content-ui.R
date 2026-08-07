builder_repo_source("profile.R")
builder_repo_source("spatial.R")
builder_repo_source("preview.R")
builder_repo_source("ui/core_stage.R")

builder_viewer_test_object <- function(n = 40L) {
  skip_if_not_installed("SeuratObject")
  counts <- Matrix::Matrix(
    matrix(
      seq_len(6L * n),
      nrow = 6L,
      dimnames = list(
        paste0("gene", seq_len(6L)),
        paste0("cell", seq_len(n))
      )
    ),
    sparse = TRUE
  )
  object <- SeuratObject::CreateSeuratObject(counts = counts)
  object$cell_type <- rep(c("B", "T"), length.out = n)
  embeddings <- cbind(
    UMAP_1 = seq_len(n),
    UMAP_2 = sin(seq_len(n) / 4)
  )
  rownames(embeddings) <- colnames(object)
  object[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings = embeddings,
    key = "UMAP_",
    assay = SeuratObject::DefaultAssay(object)
  )
  pca_embeddings <- embeddings[, 2:1, drop = FALSE]
  colnames(pca_embeddings) <- c("PC_1", "PC_2")
  object[["pca"]] <- SeuratObject::CreateDimReducObject(
    embeddings = pca_embeddings,
    key = "PC_",
    assay = SeuratObject::DefaultAssay(object)
  )
  object
}

test_that("projection preview catalogs are deterministic, bounded coordinate-only data", {
  object <- builder_viewer_test_object(60L)

  first <- builder_projection_preview_catalog(
    object,
    c("umap", "pca"),
    group = "cell_type",
    max_cells = 12L
  )
  second <- builder_projection_preview_catalog(
    object,
    c("umap", "pca"),
    group = "cell_type",
    max_cells = 12L
  )

  expect_identical(names(first), c("umap", "pca"))
  expect_identical(first, second)
  expect_true(all(vapply(first, nrow, integer(1)) <= 12L))
  expect_identical(
    unname(lapply(first, names)),
    rep(list(c("cell_barcode", "x", "y", "group")), 2L)
  )
})

test_that("projection cards use real names, shared colors, and native choices", {
  frames <- list(
    umap = structure(
      data.frame(
        cell_barcode = c("a", "b"),
        x = c(0, 1),
        y = c(1, 0),
        group = c("B", "T")
      ),
      reduction = "umap"
    ),
    pca = structure(
      data.frame(
        cell_barcode = c("a", "b"),
        x = c(-2, 2),
        y = c(-1, 1),
        group = c("B", "T")
      ),
      reduction = "pca"
    )
  )
  model <- builder_projection_catalog_model(list(
    projection_catalog = list(
      umap = list(
        id = "umap",
        name = "umap",
        kind = "umap",
        dimensions = 2L,
        cell_count = 80L,
        available = TRUE
      ),
      pca = list(
        id = "pca",
        name = "pca",
        kind = "pca",
        dimensions = 20L,
        cell_count = 80L,
        available = TRUE,
        is_pca = TRUE
      )
    ),
    included_projections = c("umap", "pca"),
    default_projection = "umap",
    overview_point_size = 6,
    projection_previews = frames,
    preview_colors = c(B = "#D97706", T = "#7C3AED")
  ))
  html <- htmltools::renderTags(
    builder_projection_catalog_ui("core", model)
  )$html

  expect_identical(
    vapply(model$items, `[[`, character(1), "id"),
    c(
      "umap",
      "pca"
    )
  )
  expect_match(html, "UMAP", fixed = TRUE)
  expect_match(html, "PCA", fixed = TRUE)
  expect_match(html, "viewer-projection-preview", fixed = TRUE)
  expect_match(html, "<svg", fixed = TRUE)
  expect_match(html, "Initial point size", fixed = TRUE)
  expect_match(html, "Set default", fixed = TRUE)
  expect_match(html, "Default", fixed = TRUE)
  expect_false(grepl("Opens first", html, fixed = TRUE))
  expect_match(html, "type=\"checkbox\"", fixed = TRUE)
  expect_match(html, "type=\"radio\"", fixed = TRUE)
  expect_match(html, "#D97706", fixed = TRUE)
  expect_match(html, "data-projection=\"pca\"", fixed = TRUE)
  expect_match(html, "<h4>PCA</h4>", fixed = TRUE)
})

test_that("trajectory previews use trajectory coordinates and never expression", {
  object <- builder_viewer_test_object(20L)
  cells <- colnames(object)[seq_len(8L)]
  object@misc$trajectories <- list(
    monocle2 = list(
      lineage_a = list(
        meta = data.frame(
          DR_1 = seq_along(cells),
          DR_2 = rev(seq_along(cells)),
          pseudotime = seq(0, 1, length.out = length(cells)),
          state = rep(c("1", "2"), length.out = length(cells)),
          row.names = cells
        ),
        edges = data.frame(
          source = "a",
          target = "b",
          weight = 1,
          source_dim_1 = 1,
          source_dim_2 = 8,
          target_dim_1 = 8,
          target_dim_2 = 1
        )
      )
    )
  )

  previews <- builder_trajectory_preview_catalog(
    object,
    list(monocle2 = "lineage_a"),
    max_cells = 5L
  )

  expect_identical(names(previews), "monocle2::lineage_a")
  expect_lte(nrow(previews[[1L]]$points), 5L)
  expect_identical(
    names(previews[[1L]]$points),
    c("cell_barcode", "x", "y", "group", "pseudotime")
  )
  expect_identical(
    names(previews[[1L]]$edges),
    c("x", "y", "xend", "yend")
  )
})

test_that("trajectory cards separate selectable and unsupported methods", {
  previews <- list(
    "monocle2::lineage_a" = list(
      points = data.frame(
        cell_barcode = c("a", "b"),
        x = c(0, 1),
        y = c(1, 0),
        group = c("1", "2"),
        pseudotime = c(0, 1)
      ),
      edges = data.frame(x = 0, y = 1, xend = 1, yend = 0)
    )
  )
  model <- builder_trajectory_catalog_model(list(
    trajectory_catalog = list(
      list(
        method = "monocle2",
        name = "lineage_a",
        supported = TRUE,
        valid = TRUE,
        selectable = TRUE,
        cell_count = 24L,
        coverage = 0.8,
        state_count = 2L,
        edge_count = 1L
      ),
      list(
        method = "slingshot",
        name = "lineage_b",
        supported = FALSE,
        valid = FALSE,
        selectable = FALSE,
        cell_count = 0L,
        coverage = 0,
        state_count = 0L,
        edge_count = 0L,
        reason = "Not supported by this Viewer version."
      )
    ),
    included_trajectories = list(monocle2 = "lineage_a"),
    default_trajectory = list(method = "monocle2", name = "lineage_a"),
    trajectory_previews = previews
  ))
  html <- htmltools::renderTags(
    builder_trajectory_catalog_ui("core", model)
  )$html

  expect_equal(model$included_count, 1L)
  expect_match(html, "lineage_a", fixed = TRUE)
  expect_match(html, "monocle2", fixed = TRUE)
  expect_match(html, "24 cells", fixed = TRUE)
  expect_match(html, "2 states", fixed = TRUE)
  expect_match(html, "80% coverage", fixed = TRUE)
  expect_match(html, "lineage_b", fixed = TRUE)
  expect_match(html, "Not supported by this Viewer version.", fixed = TRUE)
  expect_match(html, "disabled", fixed = TRUE)
  expect_match(html, "viewer-trajectory-preview", fixed = TRUE)
  expect_match(html, "<line", fixed = TRUE)
})

test_that("analysis results form a compact bounded Viewer catalog", {
  manifest <- list(
    marker_genes = list(
      id = "marker_genes",
      status = "valid",
      disposition = "preserved",
      pages = "marker_genes",
      evidence = list(
        detected = TRUE,
        valid = TRUE,
        normalized = list(
          wilcox = list(
            cell_type = list(kind = "table", valid = TRUE, rows = 20L),
            sample = list(kind = "empty_result", valid = TRUE, rows = 0L)
          ),
          presto = list(
            cell_type = list(kind = "table", valid = TRUE, rows = 15L)
          )
        )
      )
    ),
    most_expressed_genes = list(
      id = "most_expressed_genes",
      status = "valid",
      disposition = "generated",
      pages = "most_expressed_genes",
      evidence = list(detected = FALSE, valid = TRUE, normalized = list())
    ),
    mean_expression = list(
      id = "mean_expression",
      status = "not_applicable",
      disposition = NA_character_,
      pages = character(),
      evidence = list(detected = FALSE, valid = TRUE, normalized = list())
    ),
    enriched_pathways = list(
      id = "enriched_pathways",
      status = "blocking",
      disposition = "rejected",
      pages = character(),
      evidence = list(
        detected = TRUE,
        valid = FALSE,
        diagnostics = "unsupported_payload",
        normalized = list()
      )
    )
  )

  model <- builder_analysis_results_model(list(analysis_manifest = manifest))
  html <- htmltools::renderTags(builder_analysis_results_ui(model))$html

  expect_identical(
    vapply(model$items, `[[`, character(1), "id"),
    c("marker_genes", "most_expressed_genes", "enriched_pathways")
  )
  expect_identical(model$items[[1L]]$method_count, 2L)
  expect_identical(model$items[[1L]]$group_count, 2L)
  expect_identical(model$items[[1L]]$table_count, 2L)
  expect_identical(model$existing_count, 1L)
  expect_identical(model$generated_count, 1L)
  expect_identical(model$attention_count, 1L)
  expect_match(html, "Marker genes", fixed = TRUE)
  expect_match(html, "2 methods · 2 groups · 2 tables", fixed = TRUE)
  expect_match(html, "Existing", fixed = TRUE)
  expect_match(html, "Will be generated", fixed = TRUE)
  expect_match(html, "Created during build.", fixed = TRUE)
  expect_match(html, "Needs attention", fixed = TRUE)
  expect_match(
    html,
    "page stays unavailable until this is fixed.",
    fixed = TRUE
  )
  expect_match(html, "Shown on the Marker genes page.", fixed = TRUE)
  expect_false(grepl("unsupported_payload", html, fixed = TRUE))
  expect_false(grepl("diagnostic", html, ignore.case = TRUE))
})

test_that("acknowledged analysis attention is summarized as existing", {
  manifest <- list(
    marker_genes = list(
      id = "marker_genes",
      status = "attention",
      disposition = "preserved",
      pages = "marker_genes",
      required_action = list(type = "acknowledge", token = "ack-marker"),
      evidence = list(
        detected = TRUE,
        valid = TRUE,
        normalized = list(
          wilcox = list(
            cell_type = list(kind = "table", valid = TRUE, rows = 10L)
          )
        )
      )
    )
  )

  model <- builder_analysis_results_model(list(
    analysis_manifest = manifest,
    analysis_acknowledgements = "ack-marker"
  ))

  expect_identical(model$existing_count, 1L)
  expect_identical(model$attention_count, 0L)
  expect_identical(model$items[[1L]]$status, "existing")
})

test_that("specialized content becomes a compact bounded Viewer summary", {
  manifest <- list(
    spatial = list(
      id = "spatial",
      status = "valid",
      disposition = "preserved",
      pages = "spatial",
      evidence = list(
        detected = TRUE,
        valid = TRUE,
        diagnostics = "internal_spatial_code",
        normalized = list(
          section_count = 2L,
          valid_section_count = 2L,
          invalid_section_count = 0L,
          sections = list(
            list(
              name = "section_a",
              raster = list(present = TRUE, valid = TRUE)
            ),
            list(
              name = "section_b",
              raster = list(present = FALSE, valid = TRUE)
            )
          )
        )
      )
    ),
    trekker = list(
      id = "trekker",
      status = "valid",
      disposition = "preserved",
      pages = "trekker",
      evidence = list(
        detected = TRUE,
        valid = TRUE,
        normalized = list(
          cell_count = 24L,
          barcode_coverage = 1,
          cluster_count = 3L,
          field_count = 2L,
          moran_count = 4L,
          evidence_count = 2L
        )
      )
    ),
    immune_repertoire = list(
      id = "immune_repertoire",
      status = "valid",
      disposition = "preserved",
      pages = "immune_repertoire",
      evidence = list(
        detected = TRUE,
        valid = TRUE,
        normalized = list(chains = c("TRA", "TRB", "IGH")),
        selected_candidate = list(
          normalized = list(n_rows = 120L, n_samples = 2L)
        )
      )
    ),
    hla_tcr_motifs = list(
      id = "hla_tcr_motifs",
      status = "valid",
      disposition = "preserved",
      pages = "hla_tcr_motifs",
      evidence = list(
        detected = TRUE,
        valid = TRUE,
        hla_tcr_ready = TRUE,
        normalized = list()
      )
    ),
    hla = list(
      id = "hla",
      status = "valid",
      disposition = "preserved",
      pages = character(),
      evidence = list(
        detected = TRUE,
        valid = TRUE,
        normalized = list(n_samples = 2L, n_loci = 3L, n_alleles = 4L)
      )
    ),
    extra_material = list(
      id = "extra_material",
      status = "valid",
      disposition = "preserved",
      pages = "extra_material",
      evidence = list(
        detected = TRUE,
        valid = TRUE,
        normalized = list(
          tables = list(
            cell_summary = list(kind = "table", valid = TRUE, rows = 24L),
            qc = list(kind = "table", valid = TRUE, rows = 2L)
          ),
          plots = list(
            overview = list(kind = "plot", valid = TRUE, recognized = TRUE)
          )
        )
      )
    )
  )

  model <- builder_specialized_content_model(list(content_manifest = manifest))
  html <- htmltools::renderTags(builder_specialized_content_ui(model))$html

  expect_identical(
    vapply(model$items, `[[`, character(1), "id"),
    c("spatial", "trekker", "immune_repertoire", "hla", "extra_material")
  )
  expect_identical(
    model$items[[1L]]$metrics,
    c("2 sections", "2 ready", "1 tissue image")
  )
  expect_identical(
    model$items[[2L]]$metrics,
    c(
      "24 cells",
      "3 clusters",
      "2 fields",
      "100% coverage",
      "4 Moran results",
      "2 evidence images"
    )
  )
  expect_identical(
    model$items[[3L]]$metrics,
    c("120 records", "2 samples", "3 chains")
  )
  expect_identical(
    model$items[[4L]]$metrics,
    c("2 samples", "3 loci", "4 alleles")
  )
  expect_identical(
    model$items[[5L]]$metrics,
    c("2 tables", "1 plot")
  )
  expect_match(html, "Spatial", fixed = TRUE)
  expect_match(
    html,
    "Paired transcriptome and physical coordinates",
    fixed = TRUE
  )
  expect_match(html, "HLA &amp; TCR motifs", fixed = TRUE)
  expect_match(html, "Tables: Cell summary, QC", fixed = TRUE)
  expect_match(html, "Plots: Overview", fixed = TRUE)
  expect_false(grepl("internal_spatial_code", html, fixed = TRUE))
  expect_false(grepl("section_a", html, fixed = TRUE))
  expect_false(grepl("diagnostic", html, ignore.case = TRUE))
  expect_false(grepl("disposition", html, ignore.case = TRUE))
})

test_that("immune identity problems use a short actionable message", {
  manifest <- list(
    immune_repertoire = list(
      id = "immune_repertoire",
      status = "blocking",
      disposition = "rejected",
      pages = character(),
      evidence = list(
        detected = TRUE,
        valid = FALSE,
        diagnostics = c(
          "no_dataset_barcode_overlap",
          "internal_immune_code"
        ),
        normalized = list(
          n_rows = 20L,
          n_samples = 1L,
          chains = "TRB"
        )
      )
    )
  )

  model <- builder_specialized_content_model(list(content_manifest = manifest))
  html <- htmltools::renderTags(builder_specialized_content_ui(model))$html

  expect_identical(model$attention_count, 1L)
  expect_match(html, "cell barcodes do not match this dataset", fixed = TRUE)
  expect_match(html, "Check barcode prefixes", fixed = TRUE)
  expect_false(grepl("no_dataset_barcode_overlap", html, fixed = TRUE))
  expect_false(grepl("internal_immune_code", html, fixed = TRUE))
})

test_that("specialized content distinguishes included and unavailable pages", {
  detected <- function(normalized = list()) {
    list(detected = TRUE, valid = TRUE, normalized = normalized)
  }
  manifest <- list(
    spatial = list(
      status = "valid",
      disposition = "preserved",
      pages = "spatial",
      evidence = detected(list(
        section_count = 1L,
        valid_section_count = 1L,
        sections = list()
      ))
    ),
    trekker = list(
      status = "blocking",
      disposition = "rejected",
      pages = character(),
      evidence = detected(list(
        cell_count = 12L,
        cluster_count = 2L,
        field_count = 1L,
        barcode_coverage = 0.5
      ))
    ),
    immune_repertoire = list(
      status = "not_applicable",
      disposition = NA_character_,
      pages = character(),
      evidence = detected(list(chains = "TRB"))
    ),
    hla_tcr_motifs = list(
      status = "valid",
      disposition = "filtered",
      pages = "hla_tcr_motifs",
      evidence = detected()
    ),
    hla = list(
      status = "valid",
      disposition = "preserved",
      pages = character(),
      evidence = detected(list(n_samples = 1L, n_loci = 1L, n_alleles = 2L))
    )
  )

  model <- builder_specialized_content_model(list(content_manifest = manifest))
  html <- htmltools::renderTags(builder_specialized_content_ui(model))$html

  expect_identical(model$included_count, 2L)
  expect_identical(model$attention_count, 1L)
  expect_identical(model$excluded_count, 1L)
  expect_identical(
    model$summary,
    "2 included · 1 needs attention · 1 not included"
  )
  expect_match(
    html,
    "Supporting HLA typing will be preserved with this dataset.",
    fixed = TRUE
  )
  expect_false(grepl(
    "Supports the HLA &amp; TCR motifs page.",
    html,
    fixed = TRUE
  ))
})
