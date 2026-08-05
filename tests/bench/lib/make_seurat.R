# Wrap a counts matrix into the minimum Seurat object exportFromSeurat() needs.
#
# The grouping variables and the projection are synthetic on purpose: this
# benchmark measures how the three expression backends behave as the matrix
# grows, and a real clustering/UMAP would add tens of minutes per tier without
# changing a single number that is being measured. Everything that IS measured
# - the counts, the dimensions, the sparsity - comes from the real file.

suppressPackageStartupMessages({
  library(Seurat)
  library(SeuratObject)
})

bench_make_seurat <- function(m, n_groups = 6L, seed = 42L) {
  set.seed(seed)
  n <- ncol(m)

  obj <- SeuratObject::CreateSeuratObject(
    counts = m,
    min.cells = 0,
    min.features = 0,
    assay = "RNA"
  )

  # Cerebro requires at least one grouping variable and uses them for every
  # per-group aggregation; two levels of granularity mirror sample + cluster.
  obj$sample <- factor(paste0(
    "sample_",
    sample.int(n_groups, n, replace = TRUE)
  ))
  obj$cluster <- factor(paste0(
    "cluster_",
    sample.int(n_groups * 2, n, replace = TRUE)
  ))
  obj$nUMI <- Matrix::colSums(m)
  obj$nGene <- Matrix::colSums(m > 0)

  # A dummy 2-D embedding stands in for UMAP. Cerebro only reads the
  # coordinates, so random ones exercise the same code paths.
  emb <- matrix(
    stats::rnorm(2 * n),
    ncol = 2,
    dimnames = list(colnames(m), c("UMAP_1", "UMAP_2"))
  )
  obj[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings = emb,
    key = "UMAP_",
    assay = "RNA"
  )
  obj
}
