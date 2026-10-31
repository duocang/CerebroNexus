# test-enriched_pathways.R — Tests for enriched pathways module

example_crb <- system.file(
  "extdata/examples/example.crb",
  package = "CerebroNexus"
)

test_that("every optional page UI defines its tabName", {
  tabs <- list(
    enriched_pathways = c("tab_enriched_pathways", "enrichedPathways"),
    extra_material = c("tab_extra_material", "extra_material"),
    immune_repertoire = c("tab_immune_repertoire", "immune_repertoire"),
    most_expressed_genes = c("tab_most_expressed_genes", "mostExpressedGenes"),
    spatial = c("tab_spatial", "spatial"),
    trajectory = c("tab_trajectory", "trajectory")
  )
  ## Pages whose content the server renders into one container.
  containers <- c(
    enriched_pathways = "enriched_pathways_select_method_and_table_UI",
    extra_material = "extra_material_select_category_and_content_UI"
  )
  for (page in names(tabs)) {
    scope <- viewer_ui_test_env()
    sys.source(viewer_test_path(page, "UI.R"), scope)
    ui <- htmltools::tagQuery(scope[[tabs[[page]][[1L]]]])
    expect_equal(
      ui$filter(paste0("#shiny-tab-", tabs[[page]][[2L]]))$length(),
      1L,
      info = page
    )
    if (page %in% names(containers)) {
      expect_equal(
        ui$find(paste0("#", containers[[page]]))$length(),
        1L,
        info = page
      )
    }
  }
})

test_that("example.crb enriched_pathways class methods work", {
  crb <- readCerebro(example_crb)
  methods <- crb$getMethodsForEnrichedPathways()
  expect_true(is.character(methods))
  expect_true(length(methods) >= 2)
})

test_that("enriched_pathways seurat_enrichr returns real data", {
  crb <- readCerebro(example_crb)
  gse_groups <- crb$getGroupsWithEnrichedPathways("cerebro_seurat_enrichr")
  expect_true(is.character(gse_groups))
  expect_true(length(gse_groups) > 0)
  result <- crb$getEnrichedPathways("cerebro_seurat_enrichr", "seurat_clusters")
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
})
