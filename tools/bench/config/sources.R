# Remote data source registry for the expression-backend benchmark.
#
# Every source is a public HDF5 file. Metadata inspection uses the rhdf5 ROS3
# virtual file driver and transfers only the ranges it needs. A benchmark run
# downloads the complete file into its marked scratch directory so all timed
# source reads are local and network throughput is excluded from measurements.
#
# Numbers in the comments were measured with src/01_inspect_data.R on 2026-07-30.

BENCH_SOURCES <- list(
  # 10x Genomics 1.3 M mouse brain cells (E18), the canonical large-scale
  # scRNA-seq reference. Raw integer counts, CSC over cells.
  # 1,306,127 cells x 27,998 genes, nnz 2,624,828,308, 3.93 GB remote.
  mouse_brain_e18 = list(
    label = "10x mouse brain E18",
    kind = "tenx",
    url = paste0(
      "https://cf.10xgenomics.com/samples/cell-exp/1.3.0/1M_neurons/",
      "1M_neurons_filtered_gene_bc_matrices_h5.h5"
    ),
    group = "mm10",
    organism = "mm10",
    slot = "counts",
    expected_bytes = 4216018749,
    # 2010 nnz/cell. A dgCMatrix costs 12 B per non-zero and assembling one
    # peaks at roughly twice that (the slot assignment copies), so budget
    # ~48 kB of peak RAM per cell: a 32 GB host runs out somewhere past 400k
    # cells. The last tier is deliberately past that wall.
    tiers = c(50e3, 150e3, 400e3, 800e3),
    comparison_tiers = c(50e3, 150e3)
  ),

  # CELLxGENE Discover: population-scale cross-disorder atlas of the human
  # prefrontal cortex (HBCC cohort). Normalised float values, CSR over cells.
  # 1,486,324 cells x 34,176 genes, nnz 6,111,732,728, 14.15 GB remote.
  human_pfc_hbcc = list(
    label = "human PFC cross-disorder (HBCC)",
    kind = "h5ad",
    url = paste0(
      "https://datasets.cellxgene.cziscience.com/",
      "d27fb144-f105-46c2-b36f-f51421f74e4e.h5ad"
    ),
    organism = "hg38",
    slot = "data",
    expected_bytes = 14150526668,
    # 4112 nnz/cell, so ~98 kB of peak RAM per cell: this source hits the same
    # 32 GB wall at less than half the cell count of the mouse fixture even
    # though the two files hold a comparable number of cells.
    tiers = c(50e3, 150e3, 300e3),
    comparison_tiers = 50e3
  ),

  # Same collection, MSSM cohort: 4,140,453 cells, 33.6 GB remote. Opt-in via
  # BENCH_SOURCES_EXTRA=human_pfc_mssm because probing it alone streams more
  # than the other two sources combined.
  human_pfc_mssm = list(
    label = "human PFC cross-disorder (MSSM)",
    kind = "h5ad",
    url = paste0(
      "https://datasets.cellxgene.cziscience.com/",
      "0e853475-e298-4b09-881a-ed0b60d5a8c9.h5ad"
    ),
    organism = "hg38",
    slot = "data",
    expected_bytes = 36077725286,
    tiers = c(50e3, 200e3),
    comparison_tiers = 50e3,
    opt_in = TRUE
  )
)

# Which sources run by default.
bench_active_sources <- function() {
  extra <- strsplit(Sys.getenv("BENCH_SOURCES_EXTRA", ""), "[,[:space:]]+")[[1]]
  extra <- extra[nzchar(extra)]
  keep <- vapply(BENCH_SOURCES, function(s) !isTRUE(s$opt_in), logical(1))
  active <- names(BENCH_SOURCES)[keep | names(BENCH_SOURCES) %in% extra]
  only <- strsplit(Sys.getenv("BENCH_SOURCES_ONLY", ""), "[,[:space:]]+")[[1]]
  only <- only[nzchar(only)]
  if (!length(only)) {
    return(active)
  }
  unknown <- setdiff(only, names(BENCH_SOURCES))
  if (length(unknown)) {
    stop("unknown BENCH_SOURCES_ONLY value: ", paste(unknown, collapse = ", "))
  }
  intersect(active, only)
}
