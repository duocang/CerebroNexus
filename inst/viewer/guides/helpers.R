viewerGuideCatalogue <- function() {
  data.frame(
    section = c(
      rep("Getting started", 6L),
      rep("Analysis", 10L),
      rep("Reference", 7L)
    ),
    title = c(
      "Seurat workflow",
      "Create a self-contained app",
      "Launch with pre-loaded data",
      "Load multiple data sets",
      "Host on shinyapps.io",
      "Control access",
      "Most expressed genes",
      "Enriched pathways",
      "Extra material",
      "Immune repertoire",
      "Trajectory analysis",
      "Spatial analysis",
      "Trekker spatial mapping",
      "HLA and TCR motifs",
      "HLA bulk TCR associations",
      "HLA antigen-selected TCRs",
      "Cerebro object overview",
      "Data integrity contracts",
      "Seurat v5 layered assays",
      "External expression matrices",
      "Expression backend benchmark",
      "Export to SingleCellExperiment",
      "Custom tables and plots"
    ),
    slug = c(
      "cerebronexus_workflow_seurat",
      "create_a_self_contained_shiny_app",
      "launch_cerebro_with_pre-loaded_data_set",
      "multi_crb",
      "host_cerebro_on_shinyapps",
      "control_access_to_cerebro_with_a_login_page",
      "most_expressed_genes",
      "enriched_pathways",
      "extra_material",
      "immune_repertoire_analysis",
      "trajectory_analysis",
      "spatial_analysis",
      "trekker_spatial_mapping",
      "hla_tcr_motifs",
      "hla_bulk_tcr_associations",
      "hla_tcr_antigen_selected",
      "overview_of_cerebro_class",
      "data_integrity_contracts",
      "seurat_v5_layered_assays",
      "create_expression_matrix_in_h5_format",
      "expression_backend_benchmark",
      "export_a_data_set_in_SCE_format",
      "export_and_visualize_custom_tables_and_plots"
    ),
    stringsAsFactors = FALSE
  )
}

viewerGuideHref <- function(slug, cerebro_root, resource_prefix) {
  file <- file.path(
    cerebro_root,
    "viewer",
    "www",
    "guides",
    paste0(slug, ".html")
  )
  if (
    is.character(resource_prefix) &&
      length(resource_prefix) == 1L &&
      !is.na(resource_prefix) &&
      nzchar(resource_prefix) &&
      file.exists(file)
  ) {
    return(paste0(resource_prefix, "/guides/", slug, ".html"))
  }
  paste0(
    "https://mihem.github.io/CerebroNexus/articles/",
    slug,
    ".html"
  )
}
