# test-hla-tcr-publication-data.R — independent biological recomputation.

publication_data_inst_candidates <- c(
  normalizePath(testthat::test_path("../../inst"), mustWork = FALSE),
  system.file(package = "CerebroNexus")
)
publication_data_inst <- publication_data_inst_candidates[file.exists(file.path(
  publication_data_inst_candidates,
  "extdata/examples/demo_hla_tcr_dextramer.crb"
))][1]
publication_data_crb <- readRDS(file.path(
  publication_data_inst,
  "extdata/examples/demo_hla_tcr_dextramer.crb"
))

publication_annotate_repertoire <- function(crb) {
  metadata <- crb$getMetaData()
  repertoire <- crb$getImmuneRepertoire()
  Map(function(frame, sample) {
    index <- match(frame$barcode, metadata$cell_barcode)
    frame$sample <- sample
    frame$dextramer_antigen <- metadata$dextramer_antigen[index]
    frame$restriction_in_genotype <- metadata$restriction_in_genotype[index]
    frame
  }, repertoire, names(repertoire))
}

publication_counts <- function(x) {
  counts <- table(x)
  stats::setNames(as.integer(counts), names(counts))
}

test_that("the publication object contains aligned real single-cell data", {
  crb <- publication_data_crb
  metadata <- crb$getMetaData()
  expression <- crb$expression
  umap <- crb$projections$umap

  expect_s4_class(expression, "CsparseMatrix")
  expect_identical(dim(expression), c(2000L, 12000L))
  expect_identical(colnames(expression), metadata$cell_barcode)
  expect_identical(rownames(umap), metadata$cell_barcode)
  expect_identical(dim(umap), c(12000L, 2L))
  expect_identical(
    publication_counts(metadata$sample),
    stats::setNames(rep(3000L, 4), paste0("donor", 1:4))
  )

  repertoire <- crb$getImmuneRepertoire()
  expect_identical(names(repertoire), paste0("donor", 1:4))
  expect_true(all(vapply(repertoire, nrow, integer(1)) == 3000L))
  expect_true(all(grepl("^[^_]+_[^_]+$", unlist(lapply(
    repertoire,
    `[[`,
    "CTaa"
  )))))
})

test_that("the strict sequence-defined case is rederived from the CRB", {
  trb <- CerebroNexus:::hla_parse_ir_segments(
    publication_annotate_repertoire(publication_data_crb),
    "TRB"
  )
  strict <- trb[
    trb$v_gene == "TRBV19" &
      trb$j_gene == "TRBJ1-5" &
      trb$cdr3 == "CASSIYSNQPQHF",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(strict), 10L)
  expect_identical(
    publication_counts(strict$sample),
    c(donor1 = 2L, donor2 = 8L)
  )

  graph <- CerebroNexus:::hla_build_motif_graph(
    trb,
    by_v = TRUE,
    min_nodes = 2L
  )
  vertices <- igraph::vertex_attr(graph)
  anchor <- match("TRBV19::CASSIYSNQPQHF", vertices$node_id)
  members <- vertices$node_id[vertices$motif_group == vertices$motif_group[[anchor]]]
  trb$node_id <- paste(trb$v_gene, trb$cdr3, sep = "::")
  motif <- trb[trb$node_id %in% members, , drop = FALSE]
  expect_equal(length(members), 15L)
  expect_equal(nrow(motif), 30L)
  expect_identical(
    publication_counts(motif$sample),
    c(donor1 = 6L, donor2 = 22L, donor3 = 2L)
  )
})

test_that("the secondary Viewer case is rederived from the CRB", {
  repertoire <- publication_annotate_repertoire(publication_data_crb)
  rows <- do.call(rbind, repertoire)
  target <- paste0(
    "TRAV27.TRAJ42.TRAC_",
    "TRBV19.None.TRBJ2-7.TRBC2"
  )
  selected <- rows$barcode[rows$CTgene == target]
  expect_equal(length(selected), 293L)
  expect_identical(publication_counts(rows$sample[rows$CTgene == target]), c(
    donor1 = 142L,
    donor2 = 151L
  ))

  trb <- CerebroNexus:::hla_parse_ir_segments(repertoire, "TRB")
  graph <- CerebroNexus:::hla_build_motif_graph(
    trb,
    by_v = FALSE,
    min_nodes = 2L,
    show_isolated = FALSE
  )
  vertices <- as.data.frame(igraph::vertex.attributes(graph))
  anchor <- vertices[vertices$cdr3 == "CASSIRSSYEQYF", , drop = FALSE]
  members <- vertices$cdr3[vertices$cluster == anchor$cluster[[1L]]]
  motif <- trb[trb$cdr3 %in% members, , drop = FALSE]
  expect_equal(length(members), 34L)
  expect_equal(nrow(motif), 627L)
  expect_identical(
    publication_counts(motif$sample),
    c(donor1 = 308L, donor2 = 318L, donor4 = 1L)
  )
})

test_that("published HLA genotypes retain copies and provenance", {
  typing <- publication_data_crb$getHLATyping()
  expected <- read.csv(text = "sample,copy,allele
donor1,1,HLA-A*02:01
donor1,2,HLA-A*11:01
donor1,1,HLA-B*35:01
donor2,1,HLA-A*02:01
donor2,2,HLA-A*01:01
donor2,1,HLA-B*08:01
donor3,1,HLA-A*24:02
donor3,2,HLA-A*29:02
donor3,1,HLA-B*35:02
donor3,2,HLA-B*44:03
donor4,1,HLA-A*03:01
donor4,2,HLA-A*03:01
donor4,1,HLA-B*07:02
donor4,2,HLA-B*57:01", stringsAsFactors = FALSE)
  observed <- typing[, c("sample", "copy", "allele")]
  rownames(observed) <- NULL
  expect_equal(observed, expected)
  expect_identical(unique(typing$source_type), "genotyped")
  expect_true(all(nzchar(typing$typing_method)))
  expect_true(all(nzchar(typing$source_reference)))
})
