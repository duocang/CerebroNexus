builder_repo_source("ui/inspect_stage.R")
builder_repo_source("extras.R")
builder_repo_source("ui/core_stage.R")
builder_repo_source("ui/workflow.R")
builder_repo_source("ui/review_stage.R")

builder_viewer_review_html <- function(value) {
  htmltools::renderTags(value)$html
}

builder_viewer_review_plan <- function(with_trajectory = TRUE) {
  item <- list(
    id = "dataset-a",
    name = "PBMC atlas",
    filename = "01-pbmc-atlas.crb",
    organism = "hg",
    cell_count = 120L,
    gene_count = 240L,
    included_groups = c("cell_type", "sample", "condition"),
    default_group = "cell_type",
    group_color_overrides = list(
      cell_type = stats::setNames(rep("#CC5500", 6L), paste0("type", 1:6)),
      sample = c(sample_a = "#D97706", sample_b = "#F59E0B")
    ),
    included_projections = c("umap", "tsne"),
    default_projection = "umap",
    overview_point_size = 5,
    cell_cycle = "Phase",
    colors = list(cell_type = c(T = "#CC5500")),
    expression_backend = "embedded",
    spatial_image_storage = "external",
    spatial_alignment = list(
      section_count = 6L,
      image_count = 7L,
      saved_count = 7L,
      points_only = character()
    ),
    metadata_policy = list(
      retained = paste0("metadata_", seq_len(12L)),
      excluded = character()
    ),
    viewer_page_expectations = list(visible_conditional = "projection")
  )
  if (isTRUE(with_trajectory)) {
    item$included_trajectories <- list(monocle2 = "trajectory_1")
    item$default_trajectory <- list(
      method = "monocle2",
      name = "trajectory_1"
    )
  }
  structure(
    list(
      readiness = "ready",
      revision = 1L,
      make_app = TRUE,
      dataset_order = "dataset-a",
      items = list(item),
      app_options = list(
        enabled = TRUE,
        initial_dataset = "dataset-a",
        show_upload_ui = FALSE,
        welcome_message = "Welcome!",
        point_size = list(overview_projection_point_size = 4),
        variable_to_compare = FALSE
      ),
      app_auth = list(
        enabled = FALSE,
        account_count = 0L,
        timeout_minutes = 15L
      ),
      output_release = list(
        directory = "/tmp/output",
        replacement_policy = "preserve_existing",
        overwrite = FALSE,
        estimated_disk_bytes = 1024,
        estimated_runtime = "seconds"
      ),
      viewer_page_expectations = list(),
      existing_targets = character(),
      overwrite = FALSE
    ),
    class = c("builder_build_plan", "list")
  )
}

test_that("Review summarizes frozen Viewer content without technical payloads", {
  model <- builder_review_model(builder_viewer_review_plan())
  dataset <- model$datasets[[1L]]

  expect_identical(dataset$viewer_content$groups$included_count, 3L)
  expect_identical(dataset$viewer_content$groups$default, "Cell type")
  expect_identical(dataset$viewer_content$groups$custom_color_count, 8L)
  expect_identical(
    dataset$viewer_content$projections$included,
    c("UMAP", "t-SNE")
  )
  expect_identical(dataset$viewer_content$projections$default, "UMAP")
  expect_identical(dataset$viewer_content$projections$point_size, 5)
  expect_identical(dataset$viewer_content$trajectories$included_count, 1L)
  expect_identical(dataset$viewer_content$cell_cycle$included, "Phase")
  expect_identical(dataset$spatial_alignment$section_count, 6L)
  expect_identical(dataset$spatial_alignment$image_count, 7L)
  expect_identical(
    dataset$spatial_alignment$storage,
    "External spatial-assets"
  )
  expect_identical(
    dataset$viewer_content$trajectories$default,
    "trajectory_1"
  )

  html <- builder_viewer_review_html(builder_review_stage_ui("review", model))
  expect_match(html, "Groups", fixed = TRUE)
  expect_match(html, "12 retained · 0 excluded", fixed = TRUE)
  expect_match(html, "3 included · Default: Cell type", fixed = TRUE)
  expect_match(html, "8 colors customized", fixed = TRUE)
  expect_match(html, "Projections", fixed = TRUE)
  expect_match(html, "UMAP, t-SNE", fixed = TRUE)
  expect_match(html, "Default: UMAP", fixed = TRUE)
  expect_match(html, "Point size 5", fixed = TRUE)
  expect_identical(
    sum(gregexpr("Point size", html, fixed = TRUE)[[1L]] > 0L),
    1L
  )
  expect_match(html, "Trajectories", fixed = TRUE)
  expect_match(html, "Cell cycle", fixed = TRUE)
  expect_match(html, "Expression storage", fixed = TRUE)
  expect_match(html, "Embedded", fixed = TRUE)
  expect_match(
    html,
    "6 sections · 7 images · External spatial-assets",
    fixed = TRUE
  )
  expect_match(html, "Phase", fixed = TRUE)
  expect_match(html, "1 included · Default: trajectory_1", fixed = TRUE)
  expect_false(grepl("manifest", html, ignore.case = TRUE))
  expect_false(grepl("diagnostic", html, ignore.case = TRUE))
  expect_false(grepl("raw", html, ignore.case = TRUE))
})

test_that("Review remains compatible with legacy items and omits empty trajectories", {
  plan <- builder_viewer_review_plan(FALSE)
  plan$items[[1L]]$included_groups <- NULL
  plan$items[[1L]]$groups <- c("cluster", "sample")
  plan$items[[1L]]$included_projections <- NULL
  plan$items[[1L]]$reductions <- "pca"
  plan$items[[1L]]$default_group <- "cluster"
  plan$items[[1L]]$default_projection <- "pca"

  model <- builder_review_model(plan)
  html <- builder_viewer_review_html(builder_review_stage_ui("review", model))

  expect_identical(
    model$datasets[[1L]]$viewer_content$groups$included_count,
    2L
  )
  expect_identical(
    model$datasets[[1L]]$viewer_content$projections$included,
    "PCA"
  )
  expect_null(model$datasets[[1L]]$viewer_content$trajectories)
  expect_false(grepl("Trajectories", html, fixed = TRUE))
})


test_that("Review summarizes analysis results without exposing diagnostics", {
  plan <- builder_viewer_review_plan()
  plan$items[[1L]]$acknowledgements <- c(
    "ack-metadata",
    "ack-enrichment"
  )
  plan$items[[1L]]$metadata_policy <- list(
    columns = list(
      cell_barcode = list(
        disposition = "included",
        effective_included = TRUE,
        retain_in_crb = TRUE
      ),
      cell_type = list(
        disposition = "included",
        effective_included = TRUE,
        retain_in_crb = TRUE
      ),
      score = list(
        disposition = "excluded",
        effective_included = FALSE,
        retain_in_crb = FALSE
      ),
      donor = list(
        disposition = "attention",
        effective_included = TRUE,
        retain_in_crb = TRUE
      )
    )
  )
  plan$items[[1L]]$manifest <- list(
    metadata_policy = list(
      id = "metadata_policy",
      status = "attention",
      disposition = "preserved",
      required_action = list(type = "acknowledge", token = "ack-metadata")
    ),
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
            cell_type = list(kind = "table", valid = TRUE, rows = 20L)
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
    enriched_pathways = list(
      id = "enriched_pathways",
      status = "attention",
      disposition = "preserved",
      pages = "enriched_pathways",
      required_action = list(
        type = "acknowledge",
        token = "ack-enrichment"
      ),
      evidence = list(
        detected = TRUE,
        valid = TRUE,
        diagnostics = "unsafe_container",
        normalized = list(
          enrichr = list(
            cell_type = list(kind = "table", valid = TRUE, rows = 12L)
          )
        )
      )
    ),
    mean_expression = list(
      id = "mean_expression",
      status = "valid",
      disposition = "filtered",
      pages = character(),
      evidence = list(
        detected = TRUE,
        valid = TRUE,
        normalized = list(
          cell_type = list(kind = "table", valid = TRUE, rows = 20L)
        )
      )
    )
  )

  model <- builder_review_model(plan)
  analysis <- model$datasets[[1L]]$viewer_content$analysis_results
  html <- builder_viewer_review_html(builder_review_stage_ui("review", model))

  expect_identical(analysis$existing_count, 2L)
  expect_identical(analysis$generated_count, 1L)
  expect_identical(analysis$attention_count, 0L)
  expect_identical(analysis$excluded_count, 1L)
  expect_match(html, "Analysis results", fixed = TRUE)
  expect_match(
    html,
    "2 existing · 1 will be generated · 1 not included",
    fixed = TRUE
  )
  expect_match(html, "Marker genes", fixed = TRUE)
  expect_match(html, "Metadata", fixed = TRUE)
  expect_match(html, "2 retained · 1 excluded", fixed = TRUE)
  expect_false(grepl("needs attention", html, ignore.case = TRUE))
  expect_false(grepl("unsafe_container", html, fixed = TRUE))
})

test_that("Review counts acknowledged omitted metadata as excluded", {
  policy <- list(
    columns = list(
      cell_barcode = list(
        disposition = "included",
        effective_included = TRUE
      ),
      cell_type = list(
        disposition = "included",
        effective_included = TRUE
      ),
      sensitive_note = list(
        disposition = "attention",
        effective_included = FALSE
      )
    )
  )
  manifest <- list(
    metadata_policy = list(
      status = "attention",
      required_action = list(type = "acknowledge", token = "ack-metadata")
    )
  )

  summary <- builder_review_metadata_model(
    policy,
    manifest,
    acknowledgements = "ack-metadata"
  )

  expect_identical(summary$kept_count, 1L)
  expect_identical(summary$excluded_count, 1L)
  expect_identical(summary$attention_count, 0L)
})

test_that("Review lists specialized Viewer content without internal payloads", {
  plan <- builder_viewer_review_plan()
  plan$items[[1L]]$manifest <- list(
    spatial = list(
      status = "valid",
      disposition = "preserved",
      pages = "spatial",
      evidence = list(
        detected = TRUE,
        valid = TRUE,
        normalized = list(
          section_count = 2L,
          valid_section_count = 2L,
          sections = list()
        )
      )
    ),
    trekker = list(
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
          field_count = 2L
        )
      )
    ),
    immune_repertoire = list(
      status = "valid",
      disposition = "preserved",
      pages = "immune_repertoire",
      evidence = list(
        detected = TRUE,
        valid = TRUE,
        normalized = list(chains = c("TRA", "TRB")),
        selected_candidate = list(
          normalized = list(n_rows = 24L, n_samples = 1L)
        )
      )
    ),
    hla = list(
      status = "valid",
      disposition = "preserved",
      pages = character(),
      evidence = list(
        detected = TRUE,
        valid = TRUE,
        normalized = list(n_samples = 1L, n_loci = 2L, n_alleles = 3L)
      )
    ),
    extra_material = list(
      status = "valid",
      disposition = "preserved",
      pages = "extra_material",
      evidence = list(
        detected = TRUE,
        valid = TRUE,
        diagnostics = "internal_extra_code",
        normalized = list(
          tables = list(summary = list(valid = TRUE)),
          plots = list(overview = list(valid = TRUE))
        )
      )
    )
  )

  model <- builder_review_model(plan)
  specialized <- model$datasets[[1L]]$viewer_content$specialized
  html <- builder_viewer_review_html(builder_review_stage_ui("review", model))

  expect_identical(specialized$total_count, 5L)
  expect_match(html, "Specialized content", fixed = TRUE)
  expect_match(html, "5 included", fixed = TRUE)
  expect_match(
    html,
    "Spatial, Trekker, Immune repertoire, HLA, Extra material",
    fixed = TRUE
  )
  expect_false(grepl("internal_extra_code", html, fixed = TRUE))
  expect_false(grepl("diagnostic", html, ignore.case = TRUE))
  expect_false(grepl("disposition", html, ignore.case = TRUE))
})
