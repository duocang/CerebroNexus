builder_repo_source <- function(file, local = parent.frame()) {
  path <- testthat::test_path("..", "..", "inst", "builder", file)
  if (!file.exists(path)) {
    path <- system.file(file.path("builder", file), package = "CerebroNexus")
  }
  source(path, local = local)
}

builder_minimal_entry <- function(id = "ds1", name = "PBMC") {
  list(
    id = id,
    profile = list(nUMI = "nCount_RNA", nGene = "nFeature_RNA"),
    levels = list(cluster = c("A", "B")),
    settings = list(
      name = name,
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
}
