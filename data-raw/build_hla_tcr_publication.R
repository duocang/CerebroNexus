#!/usr/bin/env Rscript

## Regenerate every machine-readable result used by the HLA/TCR articles.
## The default, CI-safe path starts from the tracked Cerebro object. Use
## --from-raw only for a release audit of the pinned 10x inputs.

args <- commandArgs(trailingOnly = TRUE)
mode_flags <- intersect(args, c("--from-crb", "--from-raw"))
if (length(mode_flags) > 1L) {
  stop("choose only one of --from-crb or --from-raw", call. = FALSE)
}
mode <- if (identical(mode_flags, "--from-raw")) "from-raw" else "from-crb"
verify_only <- "--verify" %in% args
screenshots <- "--screenshots" %in% args
unknown <- setdiff(
  args,
  c("--from-crb", "--from-raw", "--verify", "--screenshots")
)
if (length(unknown)) {
  stop("unknown argument: ", unknown[[1L]], call. = FALSE)
}

command <- commandArgs(trailingOnly = FALSE)
file_argument <- grep("^--file=", command, value = TRUE)
script_file <- if (length(file_argument)) {
  sub("^--file=", "", file_argument[[1L]])
} else {
  "data-raw/build_hla_tcr_publication.R"
}
root <- normalizePath(file.path(dirname(script_file), ".."), mustWork = TRUE)
if (!file.exists(file.path(root, "DESCRIPTION"))) {
  stop("run this script from a CerebroNexus source checkout", call. = FALSE)
}

if (identical(mode, "from-raw")) {
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    file.path(root, "data-raw/build_hla_tcr_dextramer_demo.R")
  )
  if (!identical(status, 0L)) {
    stop("raw HLA/TCR object rebuild failed", call. = FALSE)
  }
}
suppressPackageStartupMessages({
  library(igraph)
  library(jsonlite)
})
pkgload::load_all(root, quiet = TRUE)

config_environment <- new.env(parent = baseenv())
sys.source(
  file.path(root, "inst/viewer/coordinated_views/config.R"),
  envir = config_environment
)

out_dir <- file.path(root, "inst/extdata/examples")
crb_path <- file.path(out_dir, "demo_hla_tcr_dextramer.crb")
stopifnot("tracked Cerebro object is missing" = file.exists(crb_path))

sha256_file <- function(path) {
  if (nzchar(Sys.which("sha256sum"))) {
    output <- system2("sha256sum", path, stdout = TRUE)
    hash <- sub("[[:space:]].*$", "", output[[1L]])
  } else if (nzchar(Sys.which("shasum"))) {
    output <- system2("shasum", c("-a", "256", path), stdout = TRUE)
    hash <- sub("[[:space:]].*$", "", output[[1L]])
  } else if (nzchar(Sys.which("openssl"))) {
    output <- system2("openssl", c("dgst", "-sha256", path), stdout = TRUE)
    hash <- sub("^.*= ", "", output[[1L]])
  } else {
    stop("sha256sum, shasum, or openssl is required", call. = FALSE)
  }
  stopifnot("invalid SHA-256 output" = grepl("^[0-9a-fA-F]{64}$", hash))
  tolower(hash)
}

counts_list <- function(values, levels = NULL) {
  if (!is.null(levels)) {
    values <- factor(as.character(values), levels = levels)
  }
  counts <- table(values)
  stats::setNames(as.list(as.integer(counts)), names(counts))
}

counts_integer <- function(values) {
  counts <- table(values)
  stats::setNames(as.integer(counts), names(counts))
}

annotate_repertoire <- function(crb) {
  metadata <- crb$getMetaData()
  repertoire <- crb$getImmuneRepertoire()
  Map(function(frame, sample) {
    index <- match(frame$barcode, metadata$cell_barcode)
    stopifnot("repertoire barcodes are not aligned" = !anyNA(index))
    additions <- setdiff(colnames(metadata), c("cell_barcode", colnames(frame)))
    for (column in additions) {
      frame[[column]] <- metadata[[column]][index]
    }
    frame$sample <- sample
    frame
  }, repertoire, names(repertoire))
}

crb <- readRDS(crb_path)
metadata <- crb$getMetaData()
cells <- as.character(metadata$cell_barcode)
repertoire <- annotate_repertoire(crb)
repertoire_rows <- do.call(rbind, repertoire)
trb <- CerebroNexus:::hla_parse_ir_segments(repertoire, "TRB")
tra <- CerebroNexus:::hla_parse_ir_segments(repertoire, "TRA")
umap <- crb$projections$umap

strict_key <- list(
  v_gene = "TRBV19",
  j_gene = "TRBJ1-5",
  cdr3 = "CASSIYSNQPQHF"
)
strict <- trb[
  trb$v_gene == strict_key$v_gene &
    trb$j_gene == strict_key$j_gene &
    trb$cdr3 == strict_key$cdr3,
  ,
  drop = FALSE
]
strict_cells <- sort(unique(strict$barcode), method = "radix")

strict_graph <- CerebroNexus:::hla_build_motif_graph(
  trb,
  by_v = TRUE,
  min_nodes = 2L
)
strict_vertices <- igraph::vertex_attr(strict_graph)
strict_node <- paste(strict_key$v_gene, strict_key$cdr3, sep = "::")
strict_anchor <- match(strict_node, strict_vertices$node_id)
strict_group <- strict_vertices$motif_group[[strict_anchor]]
strict_member_index <- which(strict_vertices$motif_group == strict_group)
strict_nodes <- strict_vertices$node_id[strict_member_index]
trb$node_id <- paste(trb$v_gene, trb$cdr3, sep = "::")
strict_motif <- trb[trb$node_id %in% strict_nodes, , drop = FALSE]

viewer_ctgene <- paste0(
  "TRAV27.TRAJ42.TRAC_",
  "TRBV19.None.TRBJ2-7.TRBC2"
)
viewer_cells <- cells[cells %in% repertoire_rows$barcode[
  repertoire_rows$CTgene == viewer_ctgene
]]
viewer_metadata <- metadata[match(viewer_cells, metadata$cell_barcode), , drop = FALSE]
viewer_trb <- trb[trb$barcode %in% viewer_cells, , drop = FALSE]
viewer_cdr3 <- sort(table(viewer_trb$cdr3), decreasing = TRUE)
viewer_anchor_cdr3 <- "CASSIRSSYEQYF"

viewer_graph <- CerebroNexus:::hla_build_motif_graph(
  trb,
  by_v = FALSE,
  min_nodes = 2L,
  show_isolated = FALSE,
  meta_cols = c("sample", "dextramer_antigen", "restriction_in_genotype")
)
viewer_vertices <- as.data.frame(igraph::vertex.attributes(viewer_graph))
viewer_anchor <- viewer_vertices[
  viewer_vertices$cdr3 == viewer_anchor_cdr3,
  ,
  drop = FALSE
]
viewer_cluster <- viewer_anchor$cluster[[1L]]
viewer_members <- sort(viewer_vertices$cdr3[
  viewer_vertices$cluster == viewer_cluster
])
viewer_motif <- trb[trb$cdr3 %in% viewer_members, , drop = FALSE]

stopifnot(
  "strict clonotype count drifted" = nrow(strict) == 10L,
  "strict motif node count drifted" = length(strict_nodes) == 15L,
  "strict motif cell count drifted" = nrow(strict_motif) == 30L,
  "Viewer selection count drifted" = length(viewer_cells) == 293L,
  "Viewer motif node count drifted" = length(viewer_members) == 34L,
  "Viewer motif cell count drifted" = nrow(viewer_motif) == 627L
)

dataset_fingerprint <- config_environment$cv_config_cell_fingerprint(cells)
dataset_sha256 <- sha256_file(crb_path)

selection_config <- function(selected_cells, source, geometry, focus_space,
                             point_size, group_labels, projection_viewport,
                             clone_viewport) {
  config <- list(
    schema = config_environment$CV_CONFIG_SCHEMA,
    version = config_environment$CV_CONFIG_VERSION,
    created_at = "2026-08-21T00:00:00Z",
    dataset = list(
      cell_count = length(cells),
      cell_fingerprint = dataset_fingerprint
    ),
    selection = list(
      cells = unname(selected_cells),
      source = source,
      geometry = geometry
    ),
    view = list(
      colour = list(
        mode = "sample",
        genes = character(),
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
        point_size = point_size,
        point_opacity = 0.8,
        group_labels = group_labels,
        cell_borders = FALSE,
        selection_mode = "box",
        clone_layout = "stack",
        keep_square = FALSE
      ),
      focus_space = focus_space,
      lenses = list(
        list(
          space = "projection::umap",
          viewport = projection_viewport,
          rotation = NULL
        ),
        list(
          space = "clone",
          viewport = clone_viewport,
          rotation = NULL
        )
      ),
      spatial_backgrounds = list(),
      trekker = list(
        dissolve_percentage = 0,
        evidence = FALSE,
        niche_radius = if (identical(focus_space, "clone")) 250 else 100
      )
    )
  )
  config_environment$cv_config_normalize(config, cells = cells)
}

strict_umap <- umap[
  match(strict_cells, rownames(umap)),
  c("UMAP_1", "UMAP_2"),
  drop = FALSE
]
strict_umap_summary <- list(
  projection = "umap",
  x_min = unname(min(strict_umap[, "UMAP_1"])),
  x_max = unname(max(strict_umap[, "UMAP_1"])),
  y_min = unname(min(strict_umap[, "UMAP_2"])),
  y_max = unname(max(strict_umap[, "UMAP_2"])),
  x_mean = unname(mean(strict_umap[, "UMAP_1"])),
  y_mean = unname(mean(strict_umap[, "UMAP_2"]))
)
pad <- 0.25
strict_polygon <- list(
  c(min(strict_umap[, 1]) - pad, min(strict_umap[, 2]) - pad),
  c(max(strict_umap[, 1]) + pad, min(strict_umap[, 2]) - pad),
  c(max(strict_umap[, 1]) + pad, max(strict_umap[, 2]) + pad),
  c(min(strict_umap[, 1]) - pad, max(strict_umap[, 2]) + pad)
)
strict_config <- selection_config(
  strict_cells,
  "golden-clonotype-barcode-list",
  list(space = "projection::umap", mode = "box", polygon = strict_polygon),
  "projection::umap",
  5,
  FALSE,
  list(
    cx = strict_umap_summary$x_mean,
    cy = strict_umap_summary$y_mean,
    span = max(
      strict_umap_summary$x_max - strict_umap_summary$x_min + 2 * pad,
      strict_umap_summary$y_max - strict_umap_summary$y_min + 2 * pad
    )
  ),
  list(cx = 0, cy = 0, span = 100)
)

clone_sizes <- sort(table(repertoire_rows$CTgene), decreasing = TRUE)
viewer_rank <- match(viewer_ctgene, names(clone_sizes))
viewer_x <- viewer_rank - 1L
viewer_config <- selection_config(
  viewer_cells,
  "Clonal expansion (TCR)",
  list(
    space = "clone",
    mode = "box",
    polygon = list(
      c(viewer_x - 0.45, -0.5),
      c(viewer_x + 0.45, -0.5),
      c(viewer_x + 0.45, length(viewer_cells) - 0.5),
      c(viewer_x - 0.45, length(viewer_cells) - 0.5)
    )
  ),
  "clone",
  3,
  TRUE,
  list(cx = 0.5, cy = 0.5, span = 1),
  list(cx = 0.5, cy = 0.5, span = 1)
)

encode_config <- function(config) {
  text <- config_environment$cv_config_encode_document(
    config_environment$cv_config_json_document(config)
  )
  sub("\n$", "", text)
}

strict_alpha <- tra[match(strict_cells, tra$barcode), , drop = FALSE]
strict_case <- list(
  schema = "cerebronexus-hla-tcr-end-to-end-case",
  version = 1L,
  case_id = "hla-tcr-antigen-selected-golden-clonotype-v1",
  dataset = list(
    path = "inst/extdata/examples/demo_hla_tcr_dextramer.crb",
    cell_count = length(cells),
    donor_count = length(unique(metadata$sample)),
    sha256 = dataset_sha256,
    cell_fingerprint = dataset_fingerprint,
    source = paste(
      "10x Genomics CD8+ T cells of Healthy Donor 1-4 (2019),",
      "Zhang et al. Sci Adv 2021, CC BY 4.0"
    ),
    build_command = "Rscript data-raw/build_hla_tcr_dextramer_demo.R",
    case_command = paste(
      "Rscript data-raw/build_hla_tcr_publication.R --from-crb"
    )
  ),
  golden_clonotype = list(
    chain = "TRB",
    v_gene = strict_key$v_gene,
    j_gene = strict_key$j_gene,
    cdr3 = strict_key$cdr3,
    clone_key = paste(unlist(strict_key), collapse = ";"),
    cell_count = nrow(strict),
    donor_counts = counts_list(strict$sample),
    cell_barcodes = strict_cells,
    paired_alpha_v_genes = sort(unique(strict_alpha$v_gene)),
    paired_alpha_j_genes = sort(unique(strict_alpha$j_gene)),
    paired_alpha_cdr3s = sort(unique(strict_alpha$cdr3)),
    dextramer_antigen_counts = counts_list(strict$dextramer_antigen),
    dextramer_peptide_counts = counts_list(strict$dextramer_peptide),
    dextramer_allele_counts = counts_list(strict$dextramer_allele),
    restriction_in_genotype_counts = counts_list(
      strict$restriction_in_genotype,
      c("yes", "no", "unknown")
    ),
    umap = strict_umap_summary
  ),
  motif = list(
    chain = "TRB",
    group = strict_group,
    consensus = strict_vertices$motif_consensus[[strict_anchor]],
    max_mismatch = strict_vertices$motif_max_mismatch[[strict_anchor]],
    node_count = length(strict_nodes),
    cell_count = nrow(strict_motif),
    donor_counts = counts_list(strict_motif$sample),
    nodes = setNames(
      as.list(strict_vertices$cdr3[strict_member_index]),
      strict_nodes
    )
  ),
  viewer_clone_contract = list(
    clone_call = "CTgene",
    selection_authority = "barcode-list",
    note = paste(
      "The Viewer clone workspace uses CTgene as its clone call. The strict",
      "TRB V/J/CDR3 case selection spans two CTgene calls because one paired",
      "alpha gene call differs; use the barcode list or Linked views JSON as",
      "the authoritative 10-cell selection."
    )
  ),
  claims = list(
    can_say = c(
      "The fixed TRB clonotype is expanded in donor1 and donor2.",
      paste(
        "Its nearby TRB CDR3 motif family contains 15 nodes and cells from",
        "donor1, donor2, and donor3."
      ),
      "The selected cells can be located in the real UMAP and shared by stable barcode."
    ),
    cannot_say = c(
      "A dextramer binder call is not validated peptide-level antigen specificity.",
      "The four donors are not enough for a statistical HLA association claim.",
      "The reagent restriction does not prove that an HLA allele caused this TCR."
    )
  )
)

viewer_case <- list(
  schema = "cerebronexus-biological-main-case",
  version = 1L,
  case_id = "hla-tcr-expanded-clone",
  title = "From an expanded gene-defined TCR clone to a shareable selection",
  source_dataset = list(
    file = basename(crb_path),
    cell_count = length(cells),
    cell_fingerprint = dataset_fingerprint,
    receptor = "TCR",
    selection_context = "antigen-selected"
  ),
  selection = list(
    clone_call = "gene",
    clone_column = "CTgene",
    ctgene = viewer_ctgene,
    clone_rank = viewer_rank,
    cell_count = length(viewer_cells),
    cells = unname(viewer_cells),
    donor_counts = counts_list(viewer_trb$sample),
    binder_counts = counts_list(viewer_metadata$dextramer_antigen),
    restriction_counts = counts_list(viewer_metadata$restriction_in_genotype),
    distinct_trb_cdr3 = length(viewer_cdr3),
    dominant_trb_cdr3 = names(viewer_cdr3)[[1L]],
    dominant_trb_cdr3_cells = unname(as.integer(viewer_cdr3[[1L]]))
  ),
  motif = list(
    definition = "equal-length TRB CDR3, Hamming distance 1",
    anchor_cdr3 = viewer_anchor_cdr3,
    consensus = viewer_anchor$motif_consensus[[1L]],
    node_count = length(viewer_members),
    cell_count = nrow(viewer_motif),
    donor_counts = counts_list(viewer_motif$sample),
    member_cdr3 = unname(viewer_members)
  ),
  interpretation = list(
    claims = c(
      "The CTgene-defined clone is expanded across donor1 and donor2.",
      "Its ten TRB CDR3 sequences belong to one anchored Hamming-1 family.",
      "The anchored family is observed across donors in this selected cohort."
    ),
    non_claims = c(
      "CTgene is not an exact sequence-defined clonotype.",
      "A raw dextramer binder call does not prove peptide specificity.",
      "The motif does not prove HLA restriction or population association.",
      "The antigen-selected four-donor cohort is not an unbiased repertoire."
    )
  )
)

donors <- paste0("donor", 1:4)
count_for <- function(donor, frame) sum(frame$sample == donor)
restriction_count <- function(donor, status) {
  sum(metadata$sample == donor & metadata$restriction_in_genotype == status)
}
cohort <- data.frame(
  donor = donors,
  total_cells = vapply(donors, function(x) sum(metadata$sample == x), integer(1)),
  strict_clonotype_cells = vapply(donors, count_for, integer(1), frame = strict),
  strict_motif_cells = vapply(donors, count_for, integer(1), frame = strict_motif),
  viewer_clone_cells = vapply(donors, count_for, integer(1), frame = viewer_trb),
  viewer_motif_cells = vapply(donors, count_for, integer(1), frame = viewer_motif),
  restriction_yes = vapply(donors, restriction_count, integer(1), status = "yes"),
  restriction_no = vapply(donors, restriction_count, integer(1), status = "no"),
  restriction_unknown = vapply(
    donors,
    restriction_count,
    integer(1),
    status = "unknown"
  ),
  stringsAsFactors = FALSE
)

hla <- crb$getHLATyping()
hla <- hla[order(hla$sample, hla$locus, hla$copy, hla$allele), c(
  "sample", "donor_id", "locus", "allele", "copy", "source_type",
  "typing_method", "source_reference"
)]
rownames(hla) <- NULL

sequence_rows <- function(frame, case_id) {
  keys <- unique(frame[c("v_gene", "j_gene", "cdr3")])
  keys <- keys[do.call(order, keys), , drop = FALSE]
  keys$cell_count <- vapply(seq_len(nrow(keys)), function(index) {
    sum(
      frame$v_gene == keys$v_gene[[index]] &
        frame$j_gene == keys$j_gene[[index]] &
        frame$cdr3 == keys$cdr3[[index]]
    )
  }, integer(1))
  data.frame(case_id = case_id, keys, stringsAsFactors = FALSE)
}
sequences <- rbind(
  sequence_rows(strict, "strict-clonotype"),
  sequence_rows(viewer_trb, "viewer-expanded-clone")
)
rownames(sequences) <- NULL

stage_dir <- tempfile(".hla-tcr-publication-", tmpdir = out_dir)
dir.create(stage_dir)
on.exit(unlink(stage_dir, recursive = TRUE), add = TRUE)

write_json <- function(value, filename) {
  jsonlite::write_json(
    value,
    file.path(stage_dir, filename),
    auto_unbox = TRUE,
    pretty = TRUE,
    digits = NA,
    null = "null",
    na = "null"
  )
}
write_text <- function(value, filename) {
  writeLines(value, file.path(stage_dir, filename), useBytes = TRUE)
}
write_table <- function(value, filename) {
  write.csv(value, file.path(stage_dir, filename), row.names = FALSE, na = "")
}

strict_case_name <- "demo_hla_tcr_dextramer.case.json"
strict_barcodes_name <- "demo_hla_tcr_dextramer.golden-clone.barcodes.tsv"
strict_config_name <- "demo_hla_tcr_dextramer.linked-views.json"
sha_name <- "demo_hla_tcr_dextramer.sha256"
viewer_case_name <- "demo_hla_tcr_main_case.expectations.json"
viewer_config_name <- "demo_hla_tcr_main_case.linked-view.json"
cohort_name <- "demo_hla_tcr_publication.cohort.csv"
hla_name <- "demo_hla_tcr_publication.hla.csv"
sequences_name <- "demo_hla_tcr_publication.sequences.csv"
screenshot_metadata_name <- "demo_hla_tcr_publication.screenshots.json"

write_json(strict_case, strict_case_name)
write_text(c("cell_barcode", strict_cells), strict_barcodes_name)
write_text(encode_config(strict_config), strict_config_name)
write_text(paste(dataset_sha256, basename(crb_path)), sha_name)
write_json(viewer_case, viewer_case_name)
write_text(encode_config(viewer_config), viewer_config_name)
write_table(cohort, cohort_name)
write_table(hla, hla_name)
write_table(sequences, sequences_name)

screenshot_metadata_path <- file.path(out_dir, screenshot_metadata_name)
if (file.exists(screenshot_metadata_path)) {
  copied <- file.copy(
    screenshot_metadata_path,
    file.path(stage_dir, screenshot_metadata_name),
    overwrite = TRUE
  )
  stopifnot("could not stage screenshot metadata" = copied)
}

with_svg <- function(filename, width, height, draw) {
  path <- file.path(stage_dir, filename)
  grDevices::svg(path, width = width, height = height, pointsize = 10)
  old_par <- graphics::par(
    family = "sans",
    mar = c(4.2, 4.2, 2.6, 0.8),
    mgp = c(2.4, 0.7, 0),
    tcl = -0.25
  )
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  })
  draw()
}

umap_figure <- "hla_tcr_publication_umap.svg"
with_svg(umap_figure, 10, 4.8, function() {
  graphics::par(mfrow = c(1, 2))
  all_umap <- umap[cells, c("UMAP_1", "UMAP_2"), drop = FALSE]
  context_cells <- unlist(lapply(split(cells, metadata$sample), function(x) {
    x[unique(round(seq(1, length(x), length.out = 500L)))]
  }), use.names = FALSE)
  context <- match(context_cells, cells)
  draw_selection <- function(selected, colour, title, label) {
    chosen <- cells %in% selected
    graphics::plot(
      all_umap,
      type = "n",
      xlab = "UMAP 1",
      ylab = "UMAP 2",
      main = title,
      asp = 1
    )
    graphics::points(
      all_umap[context, , drop = FALSE],
      pch = 16,
      cex = 0.28,
      col = "#d7dde5"
    )
    graphics::points(
      all_umap[chosen, , drop = FALSE],
      pch = 21,
      cex = 0.65,
      bg = colour,
      col = "#172033",
      lwd = 0.35
    )
    graphics::legend(
      "bottomleft",
      legend = c("cohort context", label),
      pch = c(16, 21),
      col = c("#d7dde5", "#172033"),
      pt.bg = c(NA, colour),
      bty = "n",
      cex = 0.8
    )
  }
  draw_selection(
    strict_cells,
    "#d9485f",
    "A  Strict sequence-defined clonotype",
    "10 cells"
  )
  draw_selection(
    viewer_cells,
    "#2374ab",
    "B  Secondary CTgene Viewer case",
    "293 cells"
  )
})

motif_figure <- "hla_tcr_publication_motifs.svg"
with_svg(motif_figure, 10, 4.8, function() {
  graphics::par(mfrow = c(1, 2), mar = c(1, 1, 2.6, 1))
  draw_motif <- function(graph, keep, anchor, title) {
    index <- which(igraph::V(graph)$name %in% keep)
    subgraph <- igraph::induced_subgraph(graph, vids = index)
    coordinates <- cbind(
      as.numeric(igraph::V(subgraph)$layout_x),
      as.numeric(igraph::V(subgraph)$layout_y)
    )
    is_anchor <- igraph::V(subgraph)$name == anchor
    igraph::plot.igraph(
      subgraph,
      layout = coordinates,
      vertex.size = 4 + sqrt(as.numeric(igraph::V(subgraph)$clone_count)) * 1.5,
      vertex.color = ifelse(is_anchor, "#d9485f", "#7cb7d6"),
      vertex.frame.color = "#172033",
      vertex.label = ifelse(is_anchor, igraph::V(subgraph)$cdr3, NA_character_),
      vertex.label.cex = 0.7,
      vertex.label.color = "#172033",
      vertex.label.dist = 1.4,
      vertex.label.degree = pi / 2,
      edge.color = "#9aa7b8",
      edge.width = 0.8,
      main = title,
      margin = 0.12
    )
  }
  draw_motif(
    strict_graph,
    strict_nodes,
    strict_node,
    "A  Strict motif: 15 nodes / 30 cells"
  )
  draw_motif(
    viewer_graph,
    viewer_members,
    viewer_anchor_cdr3,
    "B  Viewer motif: 34 nodes / 627 cells"
  )
})

restriction_figure <- "hla_tcr_publication_restriction.svg"
with_svg(restriction_figure, 7.2, 4.8, function() {
  values <- t(as.matrix(cohort[c(
    "restriction_yes",
    "restriction_no",
    "restriction_unknown"
  )]))
  colnames(values) <- cohort$donor
  proportions <- sweep(values, 2, colSums(values), "/") * 100
  graphics::barplot(
    proportions,
    col = c("#2a9d8f", "#d9485f", "#d4a72c"),
    border = NA,
    ylim = c(0, 108),
    ylab = "Cells (%)",
    xlab = "Published donor genotype",
    main = "Dextramer restriction allele in donor genotype"
  )
  graphics::legend(
    "topright",
    legend = c("carried", "not carried", "unknown locus copy"),
    fill = c("#2a9d8f", "#d9485f", "#d4a72c"),
    border = NA,
    bty = "n",
    horiz = TRUE,
    cex = 0.78
  )
})

artifact_names <- c(
  strict_case_name, strict_barcodes_name, strict_config_name, sha_name,
  viewer_case_name, viewer_config_name, cohort_name, hla_name, sequences_name
)
if (file.exists(screenshot_metadata_path)) {
  artifact_names <- c(artifact_names, screenshot_metadata_name)
}
artifact_entries <- lapply(artifact_names, function(filename) {
  path <- file.path(stage_dir, filename)
  list(
    file = filename,
    bytes = unname(file.info(path)$size),
    sha256 = sha256_file(path)
  )
})
figure_names <- c(umap_figure, motif_figure, restriction_figure)
figure_destinations <- file.path("vignettes/img", figure_names)
figure_entries <- Map(function(filename, destination) {
  path <- file.path(stage_dir, filename)
  list(
    file = destination,
    width_in = switch(
      filename,
      hla_tcr_publication_restriction.svg = 7.2,
      10
    ),
    height_in = 4.8,
    sha256 = sha256_file(path)
  )
}, figure_names, figure_destinations)

png_dimensions <- function(path) {
  header <- readBin(path, what = "raw", n = 24L)
  stopifnot(
    "invalid PNG header" = length(header) == 24L && identical(
      as.integer(header[1:8]),
      c(137L, 80L, 78L, 71L, 13L, 10L, 26L, 10L)
    )
  )
  decode <- function(bytes) {
    sum(as.integer(bytes) * 256^(3:0))
  }
  c(width = decode(header[17:20]), height = decode(header[21:24]))
}

screenshot_source_commit <- NULL
screenshot_entries <- list()
if (file.exists(screenshot_metadata_path)) {
  screenshot_metadata <- jsonlite::fromJSON(
    screenshot_metadata_path,
    simplifyVector = FALSE
  )
  stopifnot(
    "screenshot metadata has the wrong schema" = identical(
      screenshot_metadata$schema,
      "cerebronexus-hla-tcr-screenshots"
    ),
    "screenshot dataset fingerprint drifted" = identical(
      screenshot_metadata$dataset_fingerprint,
      dataset_fingerprint
    )
  )
  screenshot_source_commit <- screenshot_metadata$source_commit
  screenshot_entries <- lapply(screenshot_metadata$screenshots, function(entry) {
    path <- file.path(root, entry$file)
    stopifnot("publication screenshot is missing" = file.exists(path))
    dimensions <- png_dimensions(path)
    stopifnot(
      "publication screenshot width drifted" = identical(
        as.integer(entry$width),
        as.integer(dimensions[["width"]])
      ),
      "publication screenshot height drifted" = identical(
        as.integer(entry$height),
        as.integer(dimensions[["height"]])
      )
    )
    entry$sha256 <- sha256_file(path)
    entry
  })
}

manifest <- list(
  schema = "cerebronexus-hla-tcr-publication",
  version = 1L,
  release_date = "2026-09-08",
  dataset = list(
    file = basename(crb_path),
    cell_count = length(cells),
    donor_count = length(donors),
    expression_features = nrow(crb$expression),
    sha256 = dataset_sha256,
    cell_fingerprint = dataset_fingerprint
  ),
  strict_case = list(
    definition = strict_key,
    cell_count = nrow(strict),
    donor_counts = counts_list(strict$sample),
    motif = list(
      node_count = length(strict_nodes),
      cell_count = nrow(strict_motif),
      donor_counts = counts_list(strict_motif$sample)
    )
  ),
  viewer_case = list(
    definition = list(CTgene = viewer_ctgene),
    cell_count = length(viewer_cells),
    donor_counts = counts_list(viewer_trb$sample),
    motif = list(
      anchor_cdr3 = viewer_anchor_cdr3,
      node_count = length(viewer_members),
      cell_count = nrow(viewer_motif),
      donor_counts = counts_list(viewer_motif$sample)
    )
  ),
  scientific_scope = list(
    primary = "strict sequence-defined TRB clonotype",
    secondary = "CTgene-defined Viewer workflow",
    limitation = paste(
      "Four antigen-selected donors support reproducible case studies, not",
      "population-level HLA association or validated peptide specificity."
    )
  ),
  artifacts = artifact_entries,
  figures = figure_entries,
  screenshot_source_commit = screenshot_source_commit,
  screenshots = screenshot_entries
)
manifest_name <- "demo_hla_tcr_publication.manifest.json"
write_json(manifest, manifest_name)
published_names <- c(artifact_names, manifest_name)

for (entry in artifact_entries) {
  staged <- file.path(stage_dir, entry$file)
  stopifnot(
    "staged artifact byte count drifted" = identical(
      unname(file.info(staged)$size),
      entry$bytes
    ),
    "staged artifact hash drifted" = identical(
      sha256_file(staged),
      entry$sha256
    )
  )
}

if (verify_only) {
  differences <- published_names[vapply(published_names, function(filename) {
    published <- file.path(out_dir, filename)
    !file.exists(published) || !identical(
      sha256_file(file.path(stage_dir, filename)),
      sha256_file(published)
    )
  }, logical(1))]
  if (length(differences)) {
    stop(
      "publication artifacts differ: ",
      paste(differences, collapse = ", "),
      call. = FALSE
    )
  }
  figure_differences <- figure_names[vapply(seq_along(figure_names), function(i) {
    published <- file.path(root, figure_destinations[[i]])
    !file.exists(published) || !identical(
      sha256_file(file.path(stage_dir, figure_names[[i]])),
      sha256_file(published)
    )
  }, logical(1))]
  if (length(figure_differences)) {
    stop(
      "publication figures differ: ",
      paste(figure_differences, collapse = ", "),
      call. = FALSE
    )
  }
  message(
    "Verified ", length(published_names), " artifacts and ",
    length(figure_names), " figures."
  )
} else {
  for (filename in published_names) {
    staged <- file.path(stage_dir, filename)
    destination <- file.path(out_dir, filename)
    if (file.exists(destination)) {
      unlink(destination)
    }
    if (!file.rename(staged, destination)) {
      stop("could not publish ", filename, call. = FALSE)
    }
  }
  for (i in seq_along(figure_names)) {
    staged <- file.path(stage_dir, figure_names[[i]])
    destination <- file.path(root, figure_destinations[[i]])
    if (file.exists(destination)) {
      unlink(destination)
    }
    if (!file.rename(staged, destination)) {
      stop("could not publish ", figure_destinations[[i]], call. = FALSE)
    }
  }
  message(
    "Published ", length(published_names), " artifacts and ",
    length(figure_names), " figures: ",
    nrow(strict), " strict cells; ", length(viewer_cells), " Viewer cells."
  )
}
unlink(stage_dir, recursive = TRUE)

if (screenshots) {
  old_capture_root <- Sys.getenv("CEREBRONEXUS_CAPTURE_HLA_TCR_ROOT", unset = NA)
  on.exit({
    if (is.na(old_capture_root)) {
      Sys.unsetenv("CEREBRONEXUS_CAPTURE_HLA_TCR_ROOT")
    } else {
      Sys.setenv(CEREBRONEXUS_CAPTURE_HLA_TCR_ROOT = old_capture_root)
    }
  }, add = TRUE)
  Sys.setenv(CEREBRONEXUS_CAPTURE_HLA_TCR_ROOT = root)
  test_expression <- paste0(
    "devtools::test(filter=\"hla-tcr-publication-browser\", ",
    "stop_on_failure=TRUE)"
  )
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("-e", shQuote(test_expression)),
    stdout = "",
    stderr = ""
  )
  if (!identical(status, 0L)) {
    stop("Viewer screenshot capture failed", call. = FALSE)
  }
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(file.path(root, "data-raw/build_hla_tcr_publication.R"), "--from-crb")
  )
  if (!identical(status, 0L)) {
    stop("could not refresh manifest after screenshots", call. = FALSE)
  }
  message("Captured and indexed 8 Viewer screenshots.")
}
