#!/usr/bin/env Rscript
## Freeze the biological main case from the shipped real HLA/TCR object.
## This script does not download data and does not choose cells from a plot.

suppressPackageStartupMessages({
  library(jsonlite)
  library(igraph)
})

root <- normalizePath(".", winslash = "/", mustWork = TRUE)
pkgload::load_all(root, quiet = TRUE)

crb_path <- file.path(
  root,
  "inst",
  "extdata",
  "examples",
  "demo_hla_tcr_dextramer.crb"
)
out_dir <- dirname(crb_path)
stopifnot(file.exists(crb_path))

## Reuse the exact parser and the exact Linked views config contract used by the
## app. clone_contract.R is sourced by the real Shiny server before bundle.R.
source(file.path(root, "inst", "viewer", "clone_contract.R"), local = TRUE)
source(
  file.path(root, "inst", "viewer", "coordinated_views", "bundle.R"),
  local = TRUE
)
source(
  file.path(root, "inst", "viewer", "coordinated_views", "config.R"),
  local = TRUE
)

crb <- readRDS(crb_path)
meta <- crb$getMetaData()
ir <- crb$getImmuneRepertoire()
trb <- CerebroNexus:::hla_parse_ir_segments(ir, "TRB")
tra <- CerebroNexus:::hla_parse_ir_segments(ir, "TRA")

meta_index <- match(trb$barcode, meta$cell_barcode)
stopifnot(!anyNA(meta_index))
trb$sample <- meta$sample[meta_index]
trb$dextramer_antigen <- meta$dextramer_antigen[meta_index]
trb$dextramer_peptide <- meta$dextramer_peptide[meta_index]
trb$dextramer_allele <- meta$dextramer_allele[meta_index]
trb$restriction_in_genotype <- meta$restriction_in_genotype[meta_index]

TARGET <- list(
  v_gene = "TRBV19",
  j_gene = "TRBJ1-5",
  cdr3 = "CASSIYSNQPQHF"
)
target <- trb[
  trb$v_gene == TARGET$v_gene &
    trb$j_gene == TARGET$j_gene &
    trb$cdr3 == TARGET$cdr3,
  ,
  drop = FALSE
]
stopifnot("golden clonotype must be present" = nrow(target) == 10L)

count_list <- function(values, levels = NULL) {
  if (!is.null(levels)) {
    values <- factor(as.character(values), levels = levels)
  }
  counts <- table(values)
  as.list(as.integer(counts)) |> setNames(names(counts))
}

target_barcodes <- sort(unique(target$barcode), method = "radix")
stopifnot(
  "one stable barcode per selected cell" = length(target_barcodes) ==
    nrow(target),
  "golden clonotype is shared by two donors" = identical(
    count_list(target$sample),
    list(donor1 = 2L, donor2 = 8L)
  )
)

## The motif graph is the same Hamming-1 graph that the HLA & TCR Motifs page
## renders. A graph node is keyed by (V gene, CDR3), so J is retained in the
## golden clonotype contract but does not silently redefine motif membership.
motif_graph <- CerebroNexus:::hla_build_motif_graph(
  trb,
  by_v = TRUE,
  min_nodes = 2L
)
target_node <- paste(TARGET$v_gene, TARGET$cdr3, sep = "::")
vertex <- igraph::vertex_attr(motif_graph)
target_vertex <- match(target_node, vertex$node_id)
stopifnot(
  "golden clonotype must be in a rendered motif" = !is.na(target_vertex)
)
target_motif <- vertex$motif_group[[target_vertex]]
motif_vertices <- which(vertex$motif_group == target_motif)
motif_nodes <- vertex$node_id[motif_vertices]
trb$node_id <- paste(trb$v_gene, trb$cdr3, sep = "::")
motif_cells <- trb[trb$node_id %in% motif_nodes, , drop = FALSE]
stopifnot(
  "motif cell count drifted" = nrow(motif_cells) == 30L,
  "motif node count drifted" = length(motif_nodes) == 15L,
  "motif must recur in three donors" = identical(
    count_list(motif_cells$sample),
    list(donor1 = 6L, donor2 = 22L, donor3 = 2L)
  )
)

umap <- crb$projections$umap
target_umap <- umap[
  match(target_barcodes, rownames(umap)),
  c("UMAP_1", "UMAP_2"),
  drop = FALSE
]
stopifnot(!anyNA(target_umap))
umap_summary <- list(
  projection = "umap",
  x_min = unname(min(target_umap[, "UMAP_1"])),
  x_max = unname(max(target_umap[, "UMAP_1"])),
  y_min = unname(min(target_umap[, "UMAP_2"])),
  y_max = unname(max(target_umap[, "UMAP_2"])),
  x_mean = unname(mean(target_umap[, "UMAP_1"])),
  y_mean = unname(mean(target_umap[, "UMAP_2"]))
)

alpha <- tra[match(target_barcodes, tra$barcode), , drop = FALSE]
stopifnot(
  "golden clonotype must have paired alpha and beta" = nrow(alpha) ==
    length(target_barcodes),
  all(!is.na(alpha$cdr3))
)

fingerprint <- cv_config_cell_fingerprint(meta$cell_barcode)
sha256 <- character()
if (nzchar(Sys.which("sha256sum"))) {
  sha256 <- sub(
    "[[:space:]].*$",
    "",
    system2("sha256sum", crb_path, stdout = TRUE)
  )
} else {
  sha256 <- sub(
    "[[:space:]].*$",
    "",
    system2("shasum", c("-a", "256", crb_path), stdout = TRUE)
  )
}
stopifnot(nchar(sha256) == 64L)

## The polygon is only a visual record of the UMAP selection. The barcode list
## remains authoritative and is what makes the case robust to plot geometry.
pad <- 0.25
polygon <- list(
  c(umap_summary$x_min - pad, umap_summary$y_min - pad),
  c(umap_summary$x_max + pad, umap_summary$y_min - pad),
  c(umap_summary$x_max + pad, umap_summary$y_max + pad),
  c(umap_summary$x_min - pad, umap_summary$y_max + pad)
)

config <- list(
  schema = CV_CONFIG_SCHEMA,
  version = CV_CONFIG_VERSION,
  created_at = "2026-08-21T00:00:00Z",
  dataset = list(
    cell_count = nrow(meta),
    cell_fingerprint = fingerprint
  ),
  selection = list(
    cells = target_barcodes,
    source = "golden-clonotype-barcode-list",
    geometry = list(
      space = "umap",
      mode = "box",
      polygon = polygon
    )
  ),
  view = list(
    colour = list(
      mode = "sample",
      gene = NULL,
      rgb_genes = character(),
      clip = 0
    ),
    projections = "umap",
    spatial_sections = character(),
    active_spatial = NULL,
    filters = structure(list(), names = character()),
    hidden_levels = list(),
    display = list(
      percentage_cells = 100,
      point_size = 5,
      point_opacity = 0.8,
      group_labels = FALSE,
      selection_mode = "box",
      clone_layout = "stack"
    ),
    lenses = list(
      list(
        space = "umap",
        viewport = list(
          cx = mean(target_umap[, "UMAP_1"]),
          cy = mean(target_umap[, "UMAP_2"]),
          span = max(
            diff(range(target_umap[, "UMAP_1"])) + 2 * pad,
            diff(range(target_umap[, "UMAP_2"])) + 2 * pad
          )
        ),
        rotation = NULL
      ),
      list(
        space = "clone",
        viewport = list(cx = 0, cy = 0, span = 100),
        rotation = NULL
      )
    ),
    spatial_backgrounds = list(),
    trekker = list(
      dissolve_percentage = 0,
      evidence = FALSE,
      niche_radius = 100
    )
  )
)
normalized_config <- cv_config_normalize(config, cells = meta$cell_barcode)
config_json <- cv_config_encode(normalized_config)

case <- list(
  schema = "cerebronexus-hla-tcr-end-to-end-case",
  version = 1L,
  case_id = "hla-tcr-antigen-selected-golden-clonotype-v1",
  dataset = list(
    path = "inst/extdata/examples/demo_hla_tcr_dextramer.crb",
    cell_count = nrow(meta),
    donor_count = length(unique(meta$sample)),
    sha256 = sha256,
    cell_fingerprint = fingerprint,
    source = "10x Genomics CD8+ T cells of Healthy Donor 1-4 (2019), Zhang et al. Sci Adv 2021, CC BY 4.0",
    build_command = "Rscript data-raw/build_hla_tcr_dextramer_demo.R",
    case_command = "Rscript data-raw/prepare_hla_tcr_end_to_end_case.R"
  ),
  golden_clonotype = list(
    chain = "TRB",
    v_gene = TARGET$v_gene,
    j_gene = TARGET$j_gene,
    cdr3 = TARGET$cdr3,
    clone_key = paste(TARGET$v_gene, TARGET$j_gene, TARGET$cdr3, sep = ";"),
    cell_count = nrow(target),
    donor_counts = count_list(target$sample),
    cell_barcodes = target_barcodes,
    paired_alpha_v_genes = sort(unique(alpha$v_gene)),
    paired_alpha_j_genes = sort(unique(alpha$j_gene)),
    paired_alpha_cdr3s = sort(unique(alpha$cdr3)),
    dextramer_antigen_counts = count_list(target$dextramer_antigen),
    dextramer_peptide_counts = count_list(target$dextramer_peptide),
    dextramer_allele_counts = count_list(target$dextramer_allele),
    restriction_in_genotype_counts = count_list(
      target$restriction_in_genotype,
      levels = c("yes", "no", "unknown")
    ),
    umap = umap_summary
  ),
  motif = list(
    chain = "TRB",
    group = target_motif,
    consensus = vertex$motif_consensus[[target_vertex]],
    max_mismatch = vertex$motif_max_mismatch[[target_vertex]],
    node_count = length(motif_nodes),
    cell_count = nrow(motif_cells),
    donor_counts = count_list(motif_cells$sample),
    nodes = setNames(
      as.list(vertex$cdr3[motif_vertices]),
      vertex$node_id[motif_vertices]
    )
  ),
  viewer_clone_contract = list(
    clone_call = "CTgene",
    selection_authority = "barcode-list",
    note = paste(
      "The Viewer clone workspace uses CTgene as its clone call.",
      "The strict TRB V/J/CDR3 case selection spans two CTgene calls",
      "because one paired alpha gene call differs; use the barcode list or",
      "Linked views JSON as the authoritative 10-cell selection."
    )
  ),
  claims = list(
    can_say = c(
      "The fixed TRB clonotype is expanded in donor1 and donor2.",
      "Its nearby TRB CDR3 motif family contains 15 nodes and cells from donor1, donor2, and donor3.",
      "The selected cells can be located in the real UMAP and shared by stable barcode."
    ),
    cannot_say = c(
      "A dextramer binder call is not validated peptide-level antigen specificity.",
      "The four donors are not enough for a statistical HLA association claim.",
      "The reagent restriction does not prove that an HLA allele caused this TCR."
    )
  )
)

write_atomic <- function(path, write_fun) {
  staged <- paste0(path, ".staged")
  on.exit(unlink(staged), add = TRUE)
  write_fun(staged)
  stopifnot(file.rename(staged, path))
}

figure_path <- file.path(
  root,
  "inst",
  "extdata",
  "examples",
  "demo_hla_tcr_dextramer.golden-clone-umap.png"
)
dir.create(dirname(figure_path), recursive = TRUE, showWarnings = FALSE)
write_atomic(figure_path, function(path) {
  all_umap <- umap[meta$cell_barcode, c("UMAP_1", "UMAP_2"), drop = FALSE]
  selected <- meta$cell_barcode %in% target_barcodes
  grDevices::png(path, width = 1100, height = 850, res = 140)
  on.exit(grDevices::dev.off(), add = TRUE)
  plot(
    all_umap,
    type = "n",
    asp = 1,
    xlab = "UMAP 1",
    ylab = "UMAP 2",
    main = "Golden TRB clonotype in real CD8 T-cell UMAP"
  )
  points(
    all_umap[!selected, , drop = FALSE],
    pch = 16,
    cex = 0.28,
    col = "#cbd5e1"
  )
  points(
    all_umap[selected, , drop = FALSE],
    pch = 21,
    cex = 1.15,
    bg = ifelse(meta$sample[selected] == "donor1", "#2563eb", "#dc2626"),
    col = "#111827",
    lwd = 0.7
  )
  legend(
    "topright",
    legend = c("all cells", "golden clone · donor1", "golden clone · donor2"),
    pch = c(16, 21, 21),
    pt.cex = c(0.8, 1.1, 1.1),
    col = c("#cbd5e1", "#111827", "#111827"),
    pt.bg = c(NA, "#2563eb", "#dc2626"),
    bty = "n"
  )
})

write_atomic(
  file.path(out_dir, "demo_hla_tcr_dextramer.case.json"),
  function(path) {
    jsonlite::write_json(
      case,
      path,
      auto_unbox = TRUE,
      pretty = TRUE,
      digits = NA,
      null = "null"
    )
  }
)
write_atomic(
  file.path(out_dir, "demo_hla_tcr_dextramer.golden-clone.barcodes.tsv"),
  function(path) {
    writeLines(c("cell_barcode", target_barcodes), path, useBytes = TRUE)
  }
)
write_atomic(
  file.path(out_dir, "demo_hla_tcr_dextramer.linked-views.json"),
  function(path) writeLines(config_json, path, useBytes = TRUE)
)
write_atomic(
  file.path(out_dir, "demo_hla_tcr_dextramer.sha256"),
  function(path) {
    writeLines(paste(sha256, basename(crb_path)), path, useBytes = TRUE)
  }
)

cat(sprintf(
  "case: %s (%d cells; %s)\n",
  TARGET$cdr3,
  nrow(target),
  paste(
    names(count_list(target$sample)),
    unlist(count_list(target$sample)),
    sep = ":",
    collapse = ", "
  )
))
cat(sprintf(
  "motif: %s (%d nodes; %d cells; donors %s)\n",
  target_motif,
  length(motif_nodes),
  nrow(motif_cells),
  paste(
    names(count_list(motif_cells$sample)),
    unlist(count_list(motif_cells$sample)),
    sep = ":",
    collapse = ", "
  )
))
cat(sprintf("wrote case artifacts to %s\n", out_dir))
