#!/usr/bin/env Rscript
# ============================================================================
# Build the OMNIBUS demo (.crb) — every modality in one object
#   -> inst/extdata/examples/demo_omnibus.crb
# ============================================================================
# This data set is FULLY SYNTHETIC and self-contained: it downloads nothing and
# depends on no external fixture, so `Rscript data-raw/build_omnibus_demo.R`
# reproduces it byte-stably from set.seed() alone. It exists to light up EVERY
# feature the app ships from a single object, and — deliberately — to expose one
# design conflict that no shipped demo can currently trigger.
#
# WHAT IT CONTAINS
#   * scRNA expression (sparse) + UMAP / tSNE / PCA projections
#   * categorical groups: sample, cell_type, cluster, region, dextramer_*
#   * per-cell-type marker gene tables
#   * TCR immune repertoire on the T-cell compartment, built with CONVERGENT
#     CDR3 families so the motif network is legible (an unselected repertoire's
#     is not)
#   * HLA typing for three donors (one homozygous locus, one half-called locus,
#     so the three-state restriction_in_genotype column is exercised)
#   * spatial: THREE tissue sections (one per donor), each with its own embedded
#     synthetic H&E image  ->  multiple histology images
#   * Trekker: a positioned single-cell subset with confidence, per-cell fields,
#     positioning evidence, Moran's I, and vendor QC
#
# THE SPATIAL x TREKKER COEXISTENCE IT EXERCISES
#   Linked views gives every space (umap / spatial / trekker / clone) its own
#   panel and tiles them to fill the screen (2x2 when all four exist). This object
#   carries BOTH a `spatial` slot and a `trekker` slot — the only demo that does —
#   so it is the regression test that the two coexist as separate panels and that
#   neither swallows the other. (One previously DID: cv_build_bundle exposed
#   Trekker as the spatial panel only `if (is.null(sp))`, so a standard spatial
#   silently dropped the Trekker mapping. Fixed 2026-07-23 by giving Trekker its
#   own space id "trekker"; this data set is what makes that coexistence testable.)
#
# USAGE
#   Rscript data-raw/build_omnibus_demo.R
# ============================================================================

suppressPackageStartupMessages({
  library(Matrix)
})
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".", quiet = TRUE)
} else {
  pkgload::load_all(".", quiet = TRUE)
}
stopifnot(requireNamespace("base64enc", quietly = TRUE))

set.seed(20260723)
`%||%` <- function(a, b) if (is.null(a)) b else a

OUT <- "inst/extdata/examples/demo_omnibus.crb"

## ---- 0. dimensions ------------------------------------------------------- ##
SAMPLES <- c("donorA", "donorB", "donorC")
N_PER_SAMPLE <- 1000L
N <- length(SAMPLES) * N_PER_SAMPLE # 3000

## Cell types and their rough abundances. The T-cell compartment (CD8/CD4) is
## the only one that carries a receptor; it is kept sizeable so the clonal axis
## and the motif network have something to draw.
CELL_TYPES <- c(
  "Epithelial",
  "Fibroblast",
  "Endothelial",
  "Macrophage",
  "CD8 T",
  "CD4 T",
  "B cell"
)
TYPE_PROB <- c(0.24, 0.16, 0.08, 0.14, 0.18, 0.14, 0.06)

## Recognisable marker symbols per type (fills to N_MARK with synthetic symbols),
## so the gene-expression and marker-genes tabs show real-looking biology.
REAL_MARKERS <- list(
  "Epithelial" = c("EPCAM", "KRT8", "KRT18", "CDH1", "KRT19"),
  "Fibroblast" = c("COL1A1", "COL1A2", "DCN", "LUM", "PDGFRB"),
  "Endothelial" = c("PECAM1", "VWF", "CDH5", "CLDN5", "FLT1"),
  "Macrophage" = c("CD68", "LYZ", "CD163", "CSF1R", "ITGAM"),
  "CD8 T" = c("CD8A", "CD8B", "GZMB", "NKG7", "CCL5"),
  "CD4 T" = c("CD4", "IL7R", "CCR7", "CD3D", "CD3E"),
  "B cell" = c("CD19", "MS4A1", "CD79A", "CD79B", "IGHM")
)
N_MARK <- 25L # marker genes per type
N_BG <- 400L # background (non-marker) genes

## ---- 1. per-cell assignment (sample, cell_type, cluster) ----------------- ##
cat("== 1. cell assignment ==\n")
sample_vec <- rep(SAMPLES, each = N_PER_SAMPLE)
cell_type <- sample(CELL_TYPES, N, replace = TRUE, prob = TYPE_PROB)

## Clusters: a finer partition than cell_type (each type split into 1-2 sub-
## clusters). Stored 0-based; celltype-by-cluster lookup kept for Trekker.
subsplit <- c(
  "Epithelial" = 2L,
  "Fibroblast" = 2L,
  "Endothelial" = 1L,
  "Macrophage" = 2L,
  "CD8 T" = 2L,
  "CD4 T" = 1L,
  "B cell" = 1L
)
cluster_defs <- do.call(
  rbind,
  lapply(CELL_TYPES, function(ct) {
    data.frame(
      cell_type = ct,
      sub = seq_len(subsplit[[ct]]),
      stringsAsFactors = FALSE
    )
  })
)
cluster_defs$cluster_id <- seq_len(nrow(cluster_defs)) - 1L # 0-based
CELLTYPE_BY_CLUSTER <- cluster_defs$cell_type # index = cluster_id + 1
cluster_of_cell <- integer(N)
for (ct in CELL_TYPES) {
  ix <- which(cell_type == ct)
  ids <- cluster_defs$cluster_id[cluster_defs$cell_type == ct]
  cluster_of_cell[ix] <- sample(ids, length(ix), replace = TRUE)
}
n_clusters <- nrow(cluster_defs)

## Region: a spatial-layer label derived from cell type (used as a group and to
## lay out the tissue). Immune cells scatter through an "Immune-infiltrate".
REGION_OF_TYPE <- c(
  "Epithelial" = "Epithelial-zone",
  "Fibroblast" = "Stroma",
  "Endothelial" = "Perivascular",
  "Macrophage" = "Immune-infiltrate",
  "CD8 T" = "Immune-infiltrate",
  "CD4 T" = "Immune-infiltrate",
  "B cell" = "Immune-infiltrate"
)
region <- unname(REGION_OF_TYPE[cell_type])

barcodes <- sprintf(
  "%s_%s-1",
  sample_vec,
  vapply(
    seq_len(N),
    function(i) {
      paste0(sample(c(LETTERS, 0:9), 12, replace = TRUE), collapse = "")
    },
    character(1)
  )
)
barcodes <- make.unique(barcodes)

## ---- 2. expression (sparse, marker-driven) ------------------------------- ##
cat("== 2. expression ==\n")
## Build the gene panel: N_MARK markers per type (real symbols + synthetic) plus
## a shared background set.
gene_panel <- list()
for (ct in CELL_TYPES) {
  reals <- REAL_MARKERS[[ct]]
  syn <- sprintf(
    "%sMk%02d",
    gsub("[^A-Za-z0-9]", "", ct),
    seq_len(N_MARK - length(reals))
  )
  gene_panel[[ct]] <- c(reals, syn)
}
bg_genes <- sprintf("Gene%04d", seq_len(N_BG))
all_genes <- c(unlist(gene_panel, use.names = FALSE), bg_genes)
n_genes <- length(all_genes)

## Lambda matrix (genes x cells): a cell's own-type markers are elevated; every
## gene has a low baseline. Poisson draw -> counts; ~90% zeros keeps it sparse.
base_lambda <- 0.15
mark_lambda <- 6.0
lambda <- matrix(base_lambda, nrow = n_genes, ncol = N)
rownames(lambda) <- all_genes
for (ct in CELL_TYPES) {
  cells_ct <- which(cell_type == ct)
  g <- gene_panel[[ct]]
  lambda[g, cells_ct] <- mark_lambda *
    runif(length(g) * length(cells_ct), 0.5, 1.5)
}
## background genes get mild per-cell variation so PCA/UMAP are not degenerate
lambda[bg_genes, ] <- lambda[bg_genes, ] *
  matrix(runif(N_BG * N, 0.3, 2.2), nrow = N_BG)

counts <- matrix(rpois(n_genes * N, lambda), nrow = n_genes)
rownames(counts) <- all_genes
colnames(counts) <- barcodes

## Library-size normalise + log1p (the "data" layer the app colours by). Zeros
## stay zeros, so the block stays sparse.
libsize <- pmax(colSums(counts), 1)
norm <- log1p(sweep(counts, 2, libsize, "/") * 1e4)
expression <- methods::as(Matrix::Matrix(norm, sparse = TRUE), "CsparseMatrix")
cat(sprintf(
  "   expression: %d genes x %d cells, %.1f%% zeros\n",
  nrow(expression),
  ncol(expression),
  100 * (1 - Matrix::nnzero(expression) / prod(dim(expression)))
))

## ---- 3. projections: umap / tsne / pca ----------------------------------- ##
cat("== 3. projections ==\n")
## UMAP/tSNE as per-cluster gaussian blobs (consistent with the marker structure
## by construction); PCA from the expression block itself.
blob_centers <- function(k, radius, seed) {
  set.seed(seed)
  ang <- seq(0, 2 * pi, length.out = k + 1)[seq_len(k)] + runif(1, 0, pi)
  cbind(radius * cos(ang), radius * sin(ang)) +
    matrix(rnorm(k * 2, 0, 0.6), ncol = 2)
}
mk_embedding <- function(centers, spread, seed, nm) {
  set.seed(seed)
  co <- centers[cluster_of_cell + 1, , drop = FALSE] +
    matrix(rnorm(N * 2, 0, spread), ncol = 2)
  df <- data.frame(round(co, 3))
  colnames(df) <- paste0(nm, c("_1", "_2"))
  rownames(df) <- barcodes
  df
}
## Cluster centres on a SPHERE for the 3-D embedding. A ring lifted into 3-D
## still reads as a ring from most angles; points spread over a sphere only
## resolve once the cloud is turned, which is what a 3-D demo has to show.
## Fibonacci spiral, so they spread evenly instead of bunching at the poles.
blob_centers_3d <- function(k, radius, seed) {
  set.seed(seed)
  i <- seq_len(k) - 0.5
  phi <- acos(1 - 2 * i / k)
  theta <- pi * (1 + sqrt(5)) * i
  cbind(
    radius * cos(theta) * sin(phi),
    radius * sin(theta) * sin(phi),
    radius * cos(phi)
  ) +
    matrix(rnorm(k * 3, 0, 0.6), ncol = 3)
}
mk_embedding_3d <- function(centers, spread, seed, nm) {
  set.seed(seed)
  co <- centers[cluster_of_cell + 1, , drop = FALSE] +
    matrix(rnorm(N * 3, 0, spread), ncol = 3)
  df <- data.frame(round(co, 3))
  colnames(df) <- paste0(nm, c("_1", "_2", "_3"))
  rownames(df) <- barcodes
  df
}
umap <- mk_embedding(blob_centers(n_clusters, 10, 101), 0.9, 11, "UMAP")
tsne <- mk_embedding(blob_centers(n_clusters, 14, 202), 1.3, 22, "tSNE")
## A genuine 3-D reduction — what RunUMAP(n.components = 3) produces. Until this
## existed no demo carried one, so the Linked views rotate path had nothing real
## to run on.
umap_3d <- mk_embedding_3d(
  blob_centers_3d(n_clusters, 10, 303),
  0.9,
  33,
  "UMAP3D"
)
## FIVE components, not two: a PCA is normally exported with far more than three
## (RunPCA defaults to 50), and Linked views renders the first three and says so
## — "3-D of 5". Keeping this at 2 left that path untested by any demo.
pca_fit <- stats::prcomp(t(as.matrix(expression)), rank. = 5, center = TRUE)
pca <- data.frame(round(pca_fit$x[, 1:5], 3))
colnames(pca) <- paste0("PC_", 1:5)
rownames(pca) <- barcodes

## ---- 4. immune repertoire: convergent TCR on the T compartment ----------- ##
cat("== 4. immune repertoire (TCR) ==\n")
t_cells <- which(cell_type %in% c("CD8 T", "CD4 T"))
AA <- strsplit("ACDEFGHIKLMNPQRSTVWY", "")[[1]]
hamming1 <- function(seqv, n_variants) {
  ## expand a germline CDR3 into n_variants at Hamming distance 1 (convergence)
  chars <- strsplit(seqv, "")[[1]]
  vapply(
    seq_len(n_variants),
    function(i) {
      p <- sample(2:(length(chars) - 1), 1) # keep conserved C.../...F ends
      c2 <- chars
      c2[p] <- sample(AA, 1)
      paste(c2, collapse = "")
    },
    character(1)
  )
}
## a handful of germline beta CDR3s; each seeds a convergent family
germ_b <- c(
  "CASSLGTGELFF",
  "CASSIRSSYEQYF",
  "CASSPGQGAYEQYF",
  "CASSFAGGTDTQYF",
  "CASSLAPGATNEKLFF",
  "CASSQEGVGNTIYF"
)
germ_a <- c(
  "CAVRDTGGFKTIF",
  "CAGHTGNQFYF",
  "CAVNPGGTSYGKLTF",
  "CALSEAGGTSYGKLTF",
  "CAASIGFGNVLHC",
  "CAVMDSNYQLIW"
)
TRBV <- c("TRBV5-1", "TRBV20-1", "TRBV28", "TRBV19", "TRBV7-9", "TRBV12-3")
TRAV <- c("TRAV12-1", "TRAV1-2", "TRAV8-6", "TRAV38-1", "TRAV21", "TRAV26-1")

## Build a clone table: convergent families, power-law sizes (few large clones).
n_families <- length(germ_b)
clone_rows <- list()
cid <- 0L
for (fam in seq_len(n_families)) {
  n_clones_fam <- sample(6:12, 1)
  vb <- hamming1(germ_b[fam], n_clones_fam)
  va <- hamming1(germ_a[fam], n_clones_fam)
  for (j in seq_len(n_clones_fam)) {
    cid <- cid + 1L
    clone_rows[[cid]] <- data.frame(
      clone = cid,
      trbv = TRBV[fam],
      trav = TRAV[fam],
      cdr3b = vb[j],
      cdr3a = va[j],
      stringsAsFactors = FALSE
    )
  }
}
clone_tab <- do.call(rbind, clone_rows)
n_clones <- nrow(clone_tab)
## power-law clone sizes (expansion): a few big, a long tail of singletons
clone_weight <- (seq_len(n_clones)^(-1.1))
clone_weight <- clone_weight / sum(clone_weight)
clone_assign <- sample(
  seq_len(n_clones),
  length(t_cells),
  replace = TRUE,
  prob = clone_weight
)

ctgene <- sprintf(
  "%s.TRAJ33.TRAC_%s.TRBJ2-1.TRBD2.TRBC2",
  clone_tab$trav[clone_assign],
  clone_tab$trbv[clone_assign]
)
ctaa <- sprintf(
  "%s_%s",
  clone_tab$cdr3a[clone_assign],
  clone_tab$cdr3b[clone_assign]
)
ctstrict <- sprintf(
  "%s;%s_%s;%s",
  clone_tab$trav[clone_assign],
  clone_tab$cdr3a[clone_assign],
  clone_tab$trbv[clone_assign],
  clone_tab$cdr3b[clone_assign]
)
ctnt <- vapply(
  seq_along(t_cells),
  function(i) {
    paste0(sample(c("A", "C", "G", "T"), 40, replace = TRUE), collapse = "")
  },
  character(1)
)
ir_df <- data.frame(
  barcode = barcodes[t_cells],
  sample = sample_vec[t_cells],
  CTgene = ctgene,
  CTnt = ctnt,
  CTaa = ctaa,
  CTstrict = ctstrict,
  stringsAsFactors = FALSE
)
immune_repertoire <- lapply(split(ir_df, ir_df$sample), function(x) {
  x$sample <- NULL
  rownames(x) <- NULL
  x
})
cat(sprintf(
  "   %d T cells, %d clones in %d convergent families\n",
  length(t_cells),
  n_clones,
  n_families
))

## ---- 5. HLA typing + the dextramer honesty columns ----------------------- ##
cat("== 5. HLA typing ==\n")
## Three donors. donorB is homozygous A*03:01; donorC's HLA-B is half-called
## (single copy) so a B-restricted binder it carries is UNDECIDABLE, not "no".
DONOR_HLA <- read.csv(
  text = "sample,copy,allele
donorA,1,HLA-A*02:01
donorA,2,HLA-A*11:01
donorA,1,HLA-B*35:01
donorA,2,HLA-B*07:02
donorB,1,HLA-A*03:01
donorB,2,HLA-A*03:01
donorB,1,HLA-B*08:01
donorB,2,HLA-B*44:03
donorC,1,HLA-A*24:02
donorC,2,HLA-A*29:02
donorC,1,HLA-B*35:02",
  stringsAsFactors = FALSE
)
donor_typing <- data.frame(
  sample = DONOR_HLA$sample,
  donor_id = DONOR_HLA$sample,
  allele = DONOR_HLA$allele,
  copy = as.integer(DONOR_HLA$copy),
  stringsAsFactors = FALSE
)

## Give each T cell a synthetic dextramer reagent (antigen + restricting allele),
## then compute restriction_in_genotype vs the donor's published-style genotype:
## yes = donor carries the allele; no = does not and the locus is fully called;
## unknown = does not, but the locus is half-called (donorC's HLA-B).
REAGENTS <- data.frame(
  antigen = c("Flu-MP", "EBV-BMLF1", "CMV-pp65", "MART1"),
  peptide = c("GILGFVFTL", "GLCTLVAML", "NLVPMVATV", "ELAGIGILTV"),
  allele = c("HLA-A*02:01", "HLA-A*02:01", "HLA-A*02:01", "HLA-A*02:01"),
  stringsAsFactors = FALSE
)
## widen the allele pool so cross-reactivity (off-genotype) shows up
REAGENTS$allele <- c("HLA-A*02:01", "HLA-A*03:01", "HLA-B*07:02", "HLA-A*24:02")
dex_idx <- sample(seq_len(nrow(REAGENTS)), length(t_cells), replace = TRUE)
dex_antigen <- rep(NA_character_, N)
dex_peptide <- rep(NA_character_, N)
dex_allele <- rep(NA_character_, N)
dex_antigen[t_cells] <- REAGENTS$antigen[dex_idx]
dex_peptide[t_cells] <- REAGENTS$peptide[dex_idx]
dex_allele[t_cells] <- REAGENTS$allele[dex_idx]

## locus completeness per donor (two copies = complete)
locus_of <- function(a) sub("^HLA-([A-Z]+)\\*.*", "\\1", a)
carried <- paste(donor_typing$sample, donor_typing$allele)
donor_locus_n <- table(paste(
  donor_typing$sample,
  locus_of(donor_typing$allele)
))
restriction_in_genotype <- rep(NA_character_, N)
for (i in t_cells) {
  key <- paste(sample_vec[i], dex_allele[i])
  lk <- paste(sample_vec[i], locus_of(dex_allele[i]))
  complete <- !is.na(donor_locus_n[lk]) && donor_locus_n[lk] >= 2
  restriction_in_genotype[i] <- if (key %in% carried) {
    "yes"
  } else if (complete) {
    "no"
  } else {
    "unknown"
  }
}

## ---- 6. meta + groups ---------------------------------------------------- ##
cat("== 6. meta + groups ==\n")
meta <- data.frame(
  cell_barcode = barcodes,
  sample = sample_vec,
  cell_type = cell_type,
  cluster = as.character(cluster_of_cell),
  region = region,
  dextramer_antigen = dex_antigen,
  dextramer_allele = dex_allele,
  restriction_in_genotype = restriction_in_genotype,
  nCount_RNA = as.integer(colSums(counts)),
  nFeature_RNA = as.integer(colSums(counts > 0)),
  stringsAsFactors = FALSE
)
rownames(umap) <- meta$cell_barcode
rownames(tsne) <- meta$cell_barcode
rownames(umap_3d) <- meta$cell_barcode
rownames(pca) <- meta$cell_barcode

groups <- list(
  sample = sort(unique(meta$sample)),
  cell_type = sort(unique(meta$cell_type)),
  cluster = as.character(sort(unique(cluster_of_cell))),
  region = sort(unique(meta$region)),
  dextramer_antigen = sort(unique(stats::na.omit(meta$dextramer_antigen))),
  restriction_in_genotype = sort(unique(stats::na.omit(
    meta$restriction_in_genotype
  )))
)

## ---- 7. marker gene tables (per cell_type) ------------------------------- ##
cat("== 7. marker genes ==\n")
marker_df <- do.call(
  rbind,
  lapply(CELL_TYPES, function(ct) {
    g <- gene_panel[[ct]]
    data.frame(
      cell_type = ct,
      p_val = signif(runif(length(g), 1e-30, 1e-8), 3),
      avg_log2FC = round(runif(length(g), 1.5, 5), 3),
      pct.1 = round(runif(length(g), 0.6, 0.98), 3),
      pct.2 = round(runif(length(g), 0.02, 0.25), 3),
      p_val_adj = signif(runif(length(g), 1e-28, 1e-6), 3),
      gene = g,
      stringsAsFactors = FALSE
    )
  })
)

## ---- 8. spatial: three DISTINCT tissue sections, each with its own H&E ---- ##
cat("== 8. spatial (3 distinct sections + embedded H&E) ==\n")
## Each donor is a genuinely different-looking slide, so the three read at a
## glance as three real, separate tissue sections rather than one scatter
## recoloured three times: donorA is a round section (cells fill a disc, zones
## radial), donorB a layered strip (cortex-like horizontal bands), donorC a
## triangular wedge (denser toward the apex). Each H&E matches its outline + a
## different tint (rosy / purple-layered / pale-tan). Coordinates in [~40, 960]
## so the embedded image (drawn in [0,1]) overlays the cells.
spatial_layout <- function(i, reg, nn) {
  if (i == 1) {
    ## donorA — round section: uniform disc, region sets radius from the centre.
    rad <- c(
      "Perivascular" = 0.22,
      "Stroma" = 0.5,
      "Immune-infiltrate" = 0.6,
      "Epithelial-zone" = 0.86
    )[reg]
    rad[is.na(rad)] <- 0.5
    ang <- runif(nn, 0, 2 * pi)
    r <- pmin(pmax(rad + rnorm(nn, 0, 0.08), 0.02), 0.99)
    x <- 0.5 + r * cos(ang) * 0.47
    y <- 0.5 + r * sin(ang) * 0.47
  } else if (i == 2) {
    ## donorB — layered strip: region = a horizontal band (cortical layers).
    band <- c(
      "Epithelial-zone" = 0.85,
      "Immune-infiltrate" = 0.6,
      "Stroma" = 0.4,
      "Perivascular" = 0.17
    )[reg]
    band[is.na(band)] <- 0.5
    x <- runif(nn, 0.03, 0.97)
    y <- pmin(pmax(band + rnorm(nn, 0, 0.045), 0.03), 0.97)
  } else {
    ## donorC — triangular wedge (y in [0, x]), a clearly different outline.
    px <- runif(nn)
    py <- runif(nn) * px
    x <- 0.04 + px * 0.92
    y <- 0.04 + py * 0.92
  }
  data.frame(x = round(x * 1000, 2), y = round(y * 1000, 2))
}
he_png_donor <- function(i, w = 320L, h = 320L, palette = "rose") {
  set.seed(700 + i)
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = w, height = h, bg = "#ffffff")
  op <- graphics::par(mar = c(0, 0, 0, 0))
  plot.new()
  graphics::plot.window(c(0, 1), c(0, 1))
  if (i == 1) {
    ## rosy disc
    tissue_color <- if (identical(palette, "blue")) "#c8e3f5" else "#f3c9de"
    nucleus_color <- if (identical(palette, "blue")) "#245b8a" else "#6a2c74"
    th <- seq(0, 2 * pi, length.out = 90)
    graphics::polygon(
      0.5 + 0.47 * cos(th),
      0.5 + 0.47 * sin(th),
      col = tissue_color,
      border = NA
    )
    nn <- 520
    ang <- runif(nn, 0, 2 * pi)
    r <- sqrt(runif(nn)) * 0.46
    graphics::points(
      0.5 + r * cos(ang),
      0.5 + r * sin(ang),
      pch = 19,
      cex = runif(nn, 0.4, 1.2),
      col = grDevices::adjustcolor(nucleus_color, 0.5)
    )
  } else if (i == 2) {
    ## purple layered strip — horizontal bands of tint + dense nuclei
    bands <- c("#efd9f0", "#e3c3ec", "#edd0e6", "#dcb6e6")
    for (k in 0:3) {
      graphics::rect(
        0.02,
        0.03 + k * 0.235,
        0.98,
        0.03 + (k + 1) * 0.235,
        col = bands[k + 1],
        border = NA
      )
    }
    nn <- 640
    graphics::points(
      runif(nn, 0.03, 0.97),
      runif(nn, 0.04, 0.96),
      pch = 19,
      cex = runif(nn, 0.4, 1.1),
      col = grDevices::adjustcolor("#3f1d5e", 0.48)
    )
  } else {
    ## pale-tan wedge — triangular tissue, sparse, with a couple of dense foci
    graphics::polygon(
      c(0.04, 0.96, 0.96),
      c(0.04, 0.04, 0.92),
      col = "#f4e6cf",
      border = NA
    )
    nn <- 360
    px <- runif(nn)
    py <- runif(nn) * px
    graphics::points(
      0.04 + px * 0.92,
      0.04 + py * 0.88,
      pch = 19,
      cex = runif(nn, 0.4, 1.0),
      col = grDevices::adjustcolor("#7a3b2a", 0.5)
    )
    for (f in 1:2) {
      cx <- runif(1, 0.45, 0.85)
      cy <- runif(1, 0.08, cx * 0.75)
      graphics::points(
        rnorm(60, cx, 0.04),
        rnorm(60, cy, 0.04),
        pch = 19,
        cex = 0.7,
        col = grDevices::adjustcolor("#7a3b2a", 0.6)
      )
    }
  }
  graphics::par(op)
  grDevices::dev.off()
  paste0("data:image/png;base64,", base64enc::base64encode(tmp))
}
spatial_bank <- list()
for (dn in SAMPLES) {
  i <- which(SAMPLES == dn)
  ix <- which(sample_vec == dn)
  set.seed(300 + i)
  co <- spatial_layout(i, region[ix], length(ix))
  rownames(co) <- barcodes[ix]
  xr <- range(co$x)
  yr <- range(co$y)
  embedded_image <- he_png_donor(i)
  spatial_entry <- list(
    coordinates = co,
    expression = expression[, ix, drop = FALSE],
    histology_image = embedded_image,
    histology_image_bounds = list(
      xmin = xr[1],
      xmax = xr[2],
      ymin = yr[1],
      ymax = yr[2]
    )
  )
  if (i == 1L) {
    spatial_entry$histology_images <- list(
      "Rose H&E" = list(
        histology_image = embedded_image,
        histology_image_bounds = c(
          xmin = xr[1],
          xmax = xr[2],
          ymin = yr[1],
          ymax = yr[2]
        )
      ),
      "Blue H&E" = list(
        histology_image = he_png_donor(i, palette = "blue"),
        histology_image_bounds = c(
          xmin = xr[1],
          xmax = xr[2],
          ymin = yr[1],
          ymax = yr[2]
        )
      )
    )
  }
  spatial_bank[[dn]] <- spatial_entry
}

## ---- 9. Trekker: positioned single-cell subset --------------------------- ##
cat("== 9. trekker ==\n")
mk_field <- function(v, label, desc, scale = "unit", by_type = NULL) {
  if (identical(scale, "minmax")) {
    mn <- min(v, na.rm = TRUE)
    mx <- max(v, na.rm = TRUE)
    rng <- if (mx > mn) mx - mn else 1
    out <- list(
      v = as.integer(round((v - mn) / rng * 255)),
      min = round(mn, 3),
      max = round(mx, 3),
      label = label,
      desc = desc
    )
  } else {
    out <- list(
      v = as.integer(round(pmin(pmax(v, 0), 1) * 255)),
      max = 1,
      label = label,
      desc = desc
    )
  }
  if (!is.null(by_type)) {
    out$by_type <- by_type
  }
  out
}
## ~70% of cells are "positioned"; the rest are unplaced (as on a real Trekker run)
pos <- sort(sample(seq_len(N), round(N * 0.7)))
np <- length(pos)
## Physical coordinates in µm, spanning a realistic ~4 mm tissue (like a real
## Trekker run, whose sections are thousands of µm across). The niche-radius
## slider is 50-500 µm, so the tissue must be this big for those radii to read as
## SMALL neighbourhoods — a 120 µm toy tissue made a 50 µm niche cover ~40% of it.
tk_x <- runif(np, 0, 4000)
tk_y <- runif(np, 0, 4000)
purity <- pmin(pmax(rbeta(np, 3, 2), 0), 1)
concord <- pmin(pmax(rbeta(np, 2, 2), 0), 1)
prop_top <- pmin(pmax(rbeta(np, 5, 2), 0), 1)
prop_noise <- pmin(pmax(1 - prop_top - runif(np, 0, 0.1), 0), 1)
purity_by_type <- tapply(purity, cell_type[pos], function(z) round(mean(z), 3))

## a few positioning-evidence panels (synthetic bead-cloud thumbnails)
evi_png <- function(seed) {
  set.seed(seed)
  tmp <- tempfile(fileext = ".jpg")
  grDevices::jpeg(tmp, width = 140, height = 140, quality = 70)
  op <- graphics::par(mar = c(0, 0, 0, 0))
  plot.new()
  graphics::plot.window(c(0, 1), c(0, 1))
  graphics::rect(0, 0, 1, 1, col = "#111318", border = NA)
  cx0 <- runif(1, 0.3, 0.7)
  cy0 <- runif(1, 0.3, 0.7)
  graphics::points(
    rnorm(200, cx0, 0.08),
    rnorm(200, cy0, 0.08),
    pch = 19,
    cex = 0.5,
    col = grDevices::adjustcolor("#f97316", 0.7)
  )
  graphics::par(op)
  grDevices::dev.off()
  paste0("data:image/jpeg;base64,", base64enc::base64encode(tmp))
}
n_evi <- 6L
evi_pos <- sort(sample(seq_len(np), n_evi))
evidence <- lapply(seq_len(n_evi), function(i) {
  list(
    cell = evi_pos[i] - 1L,
    bc = barcodes[pos][evi_pos[i]],
    img = evi_png(seed = 900 + i)
  )
})
moran_genes <- unlist(lapply(CELL_TYPES, function(ct) gene_panel[[ct]][1]))
moran <- lapply(seq_along(moran_genes), function(i) {
  list(
    rank = i,
    gene = moran_genes[i],
    I = round(0.75 - 0.05 * i + runif(1, -0.02, 0.02), 4)
  )
})
## Complete vendor-style QC. The Trekker page + the Linked-views info modal read
## the FULL set through CerebroTrekker.buildStatsGrid / buildPositionTable /
## buildSalvFlag / buildProvenanceDl — a missing field throws on `.toFixed()`.
## Kept internally consistent: n_1 = o_1 + salv_2 + salv_3.
qc_n0 <- N - np
qc_n1 <- round(np * 0.82)
qc_n2 <- round(np * 0.12)
qc_n3 <- round(np * 0.04)
qc_n4p <- np - qc_n1 - qc_n2 - qc_n3
qc_o1 <- round(qc_n1 * 0.85)
qc_salv2 <- round(qc_n1 * 0.10)
qc_salv3 <- qc_n1 - qc_o1 - qc_salv2
qc_inlib <- round(N * 0.985)
qc_conf <- sum(prop_top > 0.6)
qc <- list(
  sample_id = "omnibus-synthetic",
  assay = "synthetic Trekker",
  tile_id = "tile-001",
  eps = 30,
  min_sb = 3,
  total_nuclei = N,
  in_lib = qc_inlib,
  pct_in_lib = round(100 * qc_inlib / N, 2),
  pct_valid_sb = 97.4,
  positioned = np,
  pct_positioned = round(100 * np / N, 2),
  conf = qc_conf,
  pct_conf = round(100 * qc_conf / np, 2),
  pct_2plus = round(100 * (qc_n2 + qc_n3 + qc_n4p) / np, 2),
  o_1 = qc_o1,
  salv_2 = qc_salv2,
  salv_3 = qc_salv3,
  pct_salv = round(100 * (qc_salv2 + qc_salv3) / qc_n1, 2),
  n_0 = qc_n0,
  n_1 = qc_n1,
  n_2 = qc_n2,
  n_3 = qc_n3,
  n_4p = qc_n4p
)
qc_examples <- lapply(0:2, function(k) {
  list(
    class = k,
    n = sum(c(qc$n_1, qc$n_2, qc$n_3)[k + 1]),
    img = evi_png(seed = 1200 + k)
  )
})
trekker <- list(
  meta = list(
    n_cells_full = N,
    n_cells = np,
    n_genes_obj = n_genes,
    unit = "um",
    coord_source = "synthetic",
    r = 25,
    seurat = FALSE,
    generated = as.character(Sys.time())
  ),
  qc = qc,
  barcodes = unname(barcodes[pos]),
  x = unname(round(tk_x, 2)),
  y = unname(round(tk_y, 2)),
  ux = unname(round(umap[pos, 1], 3)),
  uy = unname(round(umap[pos, 2], 3)),
  clusters = unname(cluster_of_cell[pos]),
  celltype = unname(CELLTYPE_BY_CLUSTER),
  fields = list(
    spatial_purity = mk_field(
      purity,
      "Spatial purity",
      "Fraction of a nucleus's spatial neighbours sharing its cell type.",
      by_type = purity_by_type
    ),
    xspace_concordance = mk_field(
      concord,
      "Cross-space concordance",
      "Overlap of a nucleus's expression-kNN and physical-kNN neighbourhoods."
    ),
    position_confidence = mk_field(
      prop_top,
      "Position confidence",
      "Share of a nucleus's beads supporting its top candidate location.",
      scale = "minmax"
    )
  ),
  conf = list(
    prop_top = unname(round(prop_top, 3)),
    prop_noise = unname(round(prop_noise, 3)),
    sb_total = unname(as.integer(round(runif(np, 50, 400)))),
    sb_umi_top = unname(as.integer(round(prop_top * runif(np, 50, 400))))
  ),
  moran = moran,
  evidence = evidence,
  qc_examples = qc_examples
)

## ---- 10. assemble the Cerebro object ------------------------------------- ##
cat("== 10. assemble .crb ==\n")
crb <- Cerebro$new()
crb$expression <- expression
crb$setMetaData(meta)
crb$projections <- list(
  umap = umap,
  tsne = tsne,
  umap_3d = umap_3d,
  pca = pca
)
crb$groups <- groups
crb$addMarkerGenes("cerebro_seurat", "cell_type", marker_df)
crb$immune_repertoire <- immune_repertoire
crb$experiment <- list(
  experiment_name = "Omnibus synthetic multi-modal demo",
  organism = "hg",
  date_of_export = Sys.Date()
)
crb$technical_info <- list(
  observation_unit = "cell",
  receptor_key = "v_gene+cdr3",
  tcr_selection = "antigen-selected",
  tcr_selection_detail = paste(
    "FULLY SYNTHETIC demo. The TCR repertoire is built with convergent CDR3",
    "families so the motif network is legible; the per-cell dextramer_* columns",
    "and HLA genotypes are fabricated to exercise the app, not real biology."
  ),
  lineage_column = "cell_type"
)
crb$addHLATyping(
  donor_typing,
  source_type = "genotyped",
  typing_method = "synthetic (fabricated genotypes for the omnibus demo)",
  source_reference = "data-raw/build_omnibus_demo.R"
)
for (dn in SAMPLES) {
  crb$addSpatialData(sprintf("%s tissue", dn), spatial_bank[[dn]])
}
crb$addTrekker(trekker)

## ---- 11. stage + verification gate --------------------------------------- ##
cat("== 11. stage + verify ==\n")
dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
staged <- paste0(OUT, ".staged")
on.exit(unlink(staged), add = TRUE)
saveRDS(crb, staged, compress = "xz")
cat(sprintf("   staged %.1f MB\n", file.info(staged)$size / 1024^2))

check <- readRDS(staged)
m <- check$getMetaData()
stopifnot(
  "meta lost rows" = nrow(m) == N,
  "cell_barcode column missing" = "cell_barcode" %in% colnames(m),
  "expression not sparse" = methods::is(check$expression, "CsparseMatrix"),
  "expression not aligned" = identical(
    colnames(check$expression),
    m$cell_barcode
  ),
  "projections missing" = setequal(
    check$availableProjections(),
    c("umap", "tsne", "umap_3d", "pca")
  ),
  ## the two shapes Linked views treats differently from a plain 2-D embedding
  "umap_3d not 3 columns" = ncol(check$projections$umap_3d) == 3,
  "pca not 5 columns" = ncol(check$projections$pca) == 5,
  "umap not aligned" = identical(
    rownames(check$projections$umap),
    m$cell_barcode
  ),
  "groups missing" = all(
    c("sample", "cell_type", "cluster", "region") %in% check$getGroups()
  ),
  "markers missing" = !is.null(check$getMarkerGenes(
    "cerebro_seurat",
    "cell_type"
  )),
  "IR missing" = length(check$getImmuneRepertoire()) == length(SAMPLES),
  "HLA missing" = nrow(check$getHLATyping()) == nrow(donor_typing),
  "spatial count wrong" = length(check$availableSpatial()) == length(SAMPLES),
  "trekker missing" = !is.null(check$getTrekker())
)
## every spatial section must carry an embedded image + x/y coordinates
for (nm in check$availableSpatial()) {
  sd <- check$getSpatialData(nm)
  first_image <- sd$histology_images[[1L]]
  stopifnot(
    "spatial coords need x/y" = all(c("x", "y") %in% colnames(sd$coordinates)),
    "spatial image missing prefix" = grepl(
      "^data:image/",
      first_image$histology_image %||% ""
    ),
    "spatial bounds missing" = all(
      c("xmin", "xmax", "ymin", "ymax") %in%
        names(
          first_image$histology_image_bounds
        )
    )
  )
}
stopifnot(
  "donorA should expose two embedded background variants" = length(
    check$getSpatialData("donorA tissue")$histology_images
  ) ==
    2L
)
## the three-state restriction column must actually show all three states
stopifnot(
  "restriction_in_genotype must be yes/no/unknown" = all(
    stats::na.omit(m$restriction_in_genotype) %in% c("yes", "no", "unknown")
  ),
  "off-genotype (no) calls vanished" = any(
    m$restriction_in_genotype == "no",
    na.rm = TRUE
  ),
  "undecidable (unknown) calls vanished" = any(
    m$restriction_in_genotype == "unknown",
    na.rm = TRUE
  )
)
## trekker arrays must be equal-length and unnamed (position-indexed on the client)
tk <- check$getTrekker()
stopifnot(
  "trekker arrays misaligned" = length(tk$barcodes) == length(tk$x) &&
    length(tk$x) == length(tk$y) &&
    length(tk$x) == length(tk$conf$prop_top),
  "trekker barcodes must be unnamed" = is.null(names(tk$x)),
  "trekker fields empty" = length(tk$fields) > 0,
  "trekker field v must be 0-255" = {
    v <- tk$fields[[1]]$v
    all(v >= 0 & v <= 255)
  },
  ## The Trekker page + the Linked-views info modal call .toFixed() on these QC
  ## fields; a missing one throws in the browser (not here), so assert them now.
  "trekker qc is missing a field the UI reads" = all(
    c(
      "total_nuclei",
      "in_lib",
      "pct_in_lib",
      "pct_valid_sb",
      "positioned",
      "pct_positioned",
      "conf",
      "pct_conf",
      "pct_2plus",
      "n_0",
      "n_1",
      "n_2",
      "n_3",
      "n_4p",
      "o_1",
      "salv_2",
      "salv_3",
      "assay",
      "sample_id",
      "tile_id",
      "eps",
      "min_sb"
    ) %in%
      names(tk$qc)
  )
)
cat("   all gates passed.\n")

## ---- 12. publish --------------------------------------------------------- ##
if (!file.rename(staged, OUT)) {
  stop("could not publish staged object to ", OUT, call. = FALSE)
}
cat(sprintf("   PUBLISHED %s (%.1f MB)\n", OUT, file.info(OUT)$size / 1024^2))
cat("done.\n")
