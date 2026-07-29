## Tests for addImmuneRepertoire(), the entry point for putting TCR/BCR data
## into a Seurat object before export.
##
## The logic that builds the slot already existed, but only inside
## convertSeuratToCerebro() and only reachable by running that whole pipeline.
## Anyone using exportFromSeurat() directly was told, in a vignette, to assign
## `@misc$immune_repertoire` by hand -- a convention with no function behind it
## and nothing checking the result.

skip_if_not_installed("Seurat")
skip_if_not_installed("SeuratObject")

## ---------------------------------------------------------------------------
## Fixtures
## ---------------------------------------------------------------------------

make_seurat <- function(n_genes = 30, n_cells = 8, with_columns = FALSE) {
  set.seed(3)
  counts <- matrix(
    rpois(n_genes * n_cells, 3),
    nrow = n_genes,
    ncol = n_cells,
    dimnames = list(
      paste0("g", seq_len(n_genes)),
      paste0("cell", seq_len(n_cells))
    )
  )
  obj <- Seurat::CreateSeuratObject(
    counts = methods::as(counts, "CsparseMatrix")
  )
  obj <- Seurat::NormalizeData(obj, verbose = FALSE)
  obj$orig.ident <- rep(c("donorA", "donorB"), length.out = n_cells)
  obj$sample <- obj$orig.ident
  obj[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings = matrix(
      rnorm(n_cells * 2),
      ncol = 2,
      dimnames = list(colnames(obj), c("UMAP_1", "UMAP_2"))
    ),
    key = "UMAP_",
    assay = "RNA"
  )

  if (with_columns) {
    ## what scRepertoire::combineExpression() leaves behind
    obj$CTgene <- "TRAV1.TRAJ2.TRAC_TRBV3.TRBJ1.TRBC1"
    obj$CTaa <- paste0(
      "CAVR",
      seq_len(n_cells),
      "F_CASSL",
      seq_len(n_cells),
      "F"
    )
    obj$CTnt <- NA_character_
  }
  obj
}

## what combineTCR() returns: a list named by sample
make_combined_list <- function(object) {
  lapply(split(colnames(object), object$orig.ident), function(bc) {
    data.frame(
      barcode = bc,
      CTgene = "TRAV1.TRAJ2.TRAC_TRBV3.TRBJ1.TRBC1",
      CTaa = paste0("CAVR", seq_along(bc), "F_CASSL", seq_along(bc), "F"),
      stringsAsFactors = FALSE
    )
  })
}

## ---------------------------------------------------------------------------
## Metadata extraction
## ---------------------------------------------------------------------------

test_that("repertoire columns already in meta.data are picked up", {
  object <- make_seurat(with_columns = TRUE)

  result <- addImmuneRepertoire(object, verbose = FALSE)
  repertoire <- result@misc$immune_repertoire

  expect_type(repertoire, "list")
  expect_setequal(names(repertoire), unique(object$orig.ident))
  expect_true(all(vapply(repertoire, is.data.frame, logical(1))))
  expect_true(all(vapply(
    repertoire,
    function(df) "barcode" %in% names(df),
    logical(1)
  )))
  ## the barcodes have to be the object's cell names, or nothing joins
  expect_true(all(
    unlist(lapply(repertoire, `[[`, "barcode")) %in% colnames(object)
  ))
})

test_that("an object with no repertoire columns is returned untouched", {
  object <- make_seurat(with_columns = FALSE)
  result <- addImmuneRepertoire(object, verbose = FALSE)
  expect_null(result@misc$immune_repertoire)
})

test_that("from_metadata = FALSE does not scan meta.data", {
  object <- make_seurat(with_columns = TRUE)
  result <- addImmuneRepertoire(object, from_metadata = FALSE, verbose = FALSE)
  expect_null(result@misc$immune_repertoire)
})

## ---------------------------------------------------------------------------
## Passing repertoire data in
## ---------------------------------------------------------------------------

test_that("a combineTCR-style list is stored as given", {
  object <- make_seurat()
  combined <- make_combined_list(object)

  result <- addImmuneRepertoire(object, tcr = combined, verbose = FALSE)
  repertoire <- result@misc$immune_repertoire

  expect_setequal(names(repertoire), names(combined))
  expect_equal(repertoire[["donorA"]], combined[["donorA"]])

  ## and the chain detector, which the HLA page depends on, sees the chains
  expect_true(all(c("TRA", "TRB") %in% hla_detect_chains(repertoire)))
})

test_that("TCR and BCR for the same samples are row-bound, not concatenated", {
  ## A name identifies one biological sample, not one receptor type. Simply
  ## concatenating the two lists gives two entries called `donorA`, and
  ## `x[["donorA"]]` returns the first -- so the B cells would vanish while the
  ## app went on reading the names as sample identifiers.
  object <- make_seurat()
  tcr <- make_combined_list(object)
  bcr <- lapply(make_combined_list(object), function(df) {
    df$CTgene <- "IGHV1.IGHJ4.IGHM_IGKV3.IGKJ1.IGKC"
    df
  })

  result <- addImmuneRepertoire(object, tcr = tcr, bcr = bcr, verbose = FALSE)
  repertoire <- result@misc$immune_repertoire

  expect_setequal(names(repertoire), names(tcr))
  expect_false(anyDuplicated(names(repertoire)) > 0)

  donor_a <- repertoire[["donorA"]]
  expect_equal(nrow(donor_a), nrow(tcr[["donorA"]]) + nrow(bcr[["donorA"]]))
  ## both receptor types survive, which is the whole point
  expect_true(any(grepl("^TRAV", donor_a$CTgene)))
  expect_true(any(grepl("^IGHV", donor_a$CTgene)))
  expect_true(all(
    c("TRA", "TRB", "IGH", "IGK") %in% hla_detect_chains(repertoire)
  ))
})

test_that("a sample present in only one receptor list is carried through", {
  object <- make_seurat()
  tcr <- make_combined_list(object)
  bcr <- make_combined_list(object)["donorA"]
  bcr[["donorA"]]$CTgene <- "IGHV1.IGHJ4.IGHM_IGKV3.IGKJ1.IGKC"

  result <- addImmuneRepertoire(object, tcr = tcr, bcr = bcr, verbose = FALSE)
  repertoire <- result@misc$immune_repertoire

  expect_setequal(names(repertoire), c("donorA", "donorB"))
  expect_equal(nrow(repertoire[["donorB"]]), nrow(tcr[["donorB"]]))
})

test_that("a sample with no rows survives the merge", {
  ## A sample can legitimately end up empty -- every receptor filtered out --
  ## and filling a missing column on a zero-row data.frame is an error unless
  ## the replacement is sized to the frame.
  object <- make_seurat()

  ## Only the TCR side carries CTstrict, so the column has to be filled on the
  ## BCR side -- which is the empty one.
  tcr <- make_combined_list(object)
  tcr[["donorA"]]$CTstrict <- "TRAV1.TRAJ2_TRBV3.TRBJ1"
  bcr <- make_combined_list(object)
  bcr[["donorA"]] <- bcr[["donorA"]][0, , drop = FALSE]

  expect_no_error(
    result <- addImmuneRepertoire(object, tcr = tcr, bcr = bcr, verbose = FALSE)
  )
  repertoire <- result@misc$immune_repertoire

  expect_setequal(names(repertoire), names(tcr))
  ## the empty side contributes no rows but does not lose the other side
  expect_equal(nrow(repertoire[["donorA"]]), nrow(tcr[["donorA"]]))
  expect_true("CTstrict" %in% names(repertoire[["donorA"]]))
  expect_false(anyNA(repertoire[["donorA"]]$CTstrict))
})

test_that("an .rds path is read", {
  object <- make_seurat()
  combined <- make_combined_list(object)
  path <- tempfile(fileext = ".rds")
  saveRDS(combined, path)

  result <- addImmuneRepertoire(object, tcr = path, verbose = FALSE)
  expect_setequal(names(result@misc$immune_repertoire), names(combined))
})

test_that("a bad shape is rejected at the entry point, not at export", {
  object <- make_seurat()
  flat <- do.call(rbind, make_combined_list(object))

  expect_error(
    addImmuneRepertoire(object, tcr = flat, verbose = FALSE),
    regexp = "named list"
  )
})

## ---------------------------------------------------------------------------
## End to end
## ---------------------------------------------------------------------------

test_that("the slot survives export as samples, not as column names", {
  object <- make_seurat()
  combined <- make_combined_list(object)
  object <- addImmuneRepertoire(object, tcr = combined, verbose = FALSE)

  crb_path <- tempfile(fileext = ".crb")
  exportFromSeurat(
    object = object,
    file = crb_path,
    experiment_name = "repertoire api",
    organism = "hg",
    groups = "sample",
    nUMI = "nCount_RNA",
    nGene = "nFeature_RNA",
    verbose = FALSE
  )

  loaded <- readRDS(crb_path)$getImmuneRepertoire()
  expect_setequal(names(loaded), names(combined))
  ## the failure this guards against: column names standing in for samples
  expect_false(any(c("barcode", "CTgene", "CTaa") %in% names(loaded)))
})

## ---------------------------------------------------------------------------
## Cell Ranger contigs
## ---------------------------------------------------------------------------

test_that("filtered_contig_annotations.csv paths are assembled into clones", {
  skip_if_not_installed("scRepertoire")

  object <- make_seurat()
  contig_dir <- withr::local_tempdir()
  barcodes <- split(colnames(object), object$orig.ident)

  paths <- vapply(
    names(barcodes),
    function(sample_name) {
      bc <- barcodes[[sample_name]]
      contigs <- do.call(
        rbind,
        lapply(c("TRA", "TRB"), function(chain) {
          data.frame(
            barcode = bc,
            is_cell = "True",
            contig_id = paste0(bc, "_", chain),
            high_confidence = "True",
            length = 500L,
            chain = chain,
            v_gene = if (chain == "TRA") "TRAV1" else "TRBV3",
            d_gene = "None",
            j_gene = if (chain == "TRA") "TRAJ2" else "TRBJ1",
            c_gene = if (chain == "TRA") "TRAC" else "TRBC1",
            full_length = "True",
            productive = "True",
            cdr3 = if (chain == "TRA") "CAVRF" else "CASSLF",
            cdr3_nt = if (chain == "TRA") "TGTGCC" else "TGTGCA",
            reads = 100L,
            umis = 5L,
            raw_clonotype_id = paste0("clonotype", seq_along(bc)),
            raw_consensus_id = paste0(
              "clonotype",
              seq_along(bc),
              "_consensus_1"
            ),
            stringsAsFactors = FALSE
          )
        })
      )
      path <- file.path(contig_dir, paste0(sample_name, ".csv"))
      utils::write.csv(contigs, path, row.names = FALSE)
      path
    },
    character(1)
  )

  ## `combineTCR(samples = )` prefixes every barcode with the sample name, so
  ## the cell names have to carry the same prefix -- the trap both vignettes
  ## warn about. Do it, so this exercises the path that actually works rather
  ## than accepting a repertoire that reaches no cell.
  object <- SeuratObject::RenameCells(
    object,
    new.names = paste0(object$orig.ident, "_", colnames(object))
  )

  result <- addImmuneRepertoire(
    object,
    tcr = paths,
    sample_names = names(barcodes),
    verbose = FALSE
  )
  repertoire <- result@misc$immune_repertoire

  expect_true(length(repertoire) > 0)
  expect_true(all(vapply(repertoire, is.data.frame, logical(1))))
  expect_true(all(vapply(
    repertoire,
    function(df) all(c("barcode", "CTgene", "CTaa") %in% names(df)),
    logical(1)
  )))

  ## the assertion that matters: the barcodes reach the cells. Well-named
  ## columns over an empty join are exactly the failure this entry point exists
  ## to prevent.
  repertoire_barcodes <- unique(unlist(
    lapply(repertoire, `[[`, "barcode"),
    use.names = FALSE
  ))
  expect_true(all(repertoire_barcodes %in% colnames(object)))
  expect_gt(length(repertoire_barcodes), 0)
})

test_that("a repertoire whose barcodes reach no cell is refused at the entry", {
  ## The same CSV path without RenameCells: combineTCR prefixed the barcodes
  ## and nothing on the object matches them.
  skip_if_not_installed("scRepertoire")

  object <- make_seurat()
  contig_dir <- withr::local_tempdir()
  barcodes <- split(colnames(object), object$orig.ident)

  paths <- vapply(
    names(barcodes),
    function(sample_name) {
      bc <- barcodes[[sample_name]]
      contigs <- do.call(
        rbind,
        lapply(c("TRA", "TRB"), function(chain) {
          data.frame(
            barcode = bc,
            is_cell = "True",
            contig_id = paste0(bc, "_", chain),
            high_confidence = "True",
            length = 500L,
            chain = chain,
            v_gene = if (chain == "TRA") "TRAV1" else "TRBV3",
            d_gene = "None",
            j_gene = if (chain == "TRA") "TRAJ2" else "TRBJ1",
            c_gene = if (chain == "TRA") "TRAC" else "TRBC1",
            full_length = "True",
            productive = "True",
            cdr3 = if (chain == "TRA") "CAVRF" else "CASSLF",
            cdr3_nt = if (chain == "TRA") "TGTGCC" else "TGTGCA",
            reads = 100L,
            umis = 5L,
            raw_clonotype_id = paste0("clonotype", seq_along(bc)),
            raw_consensus_id = paste0(
              "clonotype",
              seq_along(bc),
              "_consensus_1"
            ),
            stringsAsFactors = FALSE
          )
        })
      )
      path <- file.path(contig_dir, paste0(sample_name, ".csv"))
      utils::write.csv(contigs, path, row.names = FALSE)
      path
    },
    character(1)
  )

  err <- tryCatch(
    addImmuneRepertoire(
      object,
      tcr = paths,
      sample_names = names(barcodes),
      verbose = FALSE
    ),
    error = function(e) conditionMessage(e)
  )
  expect_true(grepl("shares no barcode", err, fixed = TRUE))
  expect_true(grepl("RenameCells", err, fixed = TRUE))
})

## ---------------------------------------------------------------------------
## Export contract
## ---------------------------------------------------------------------------

test_that("addImmuneRepertoire is part of the package's public surface", {
  ## The tests above call it unqualified, which works inside the package
  ## whether or not it is exported. Users cannot, so pin the export itself.
  ##
  ## Look for the NAMESPACE in both layouts. Under R CMD check the tests run
  ## against an installed copy and `../../NAMESPACE` does not exist -- which
  ## would skip this check exactly where it is worth making.
  candidates <- c(
    system.file("NAMESPACE", package = "CerebroNexus"),
    testthat::test_path("../../NAMESPACE")
  )
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  skip_if(length(candidates) == 0, "no NAMESPACE in either layout")

  expect_true(
    any(grepl(
      "^export\\(addImmuneRepertoire\\)$",
      readLines(candidates[[1]], warn = FALSE)
    ))
  )
})

## ---------------------------------------------------------------------------
## Sample names derived from contig file paths
## ---------------------------------------------------------------------------

test_that("a lone Cell Ranger file is named after its directory", {
  ## Cell Ranger calls every sample's file the same thing, so the file name
  ## identifies nothing. Deriving it from the stem produced a sample called
  ## "filtered_contig_annotations", which scRepertoire then prefixed onto every
  ## barcode -- a reliable way to end up matching no cell.
  paths <- file.path("root", "donorA", "filtered_contig_annotations.csv")
  expect_identical(.deriveContigSampleNames(paths), "donorA")
})

test_that("several Cell Ranger files are named after their directories", {
  paths <- file.path(
    "root",
    c("donorA", "donorB"),
    "filtered_contig_annotations.csv"
  )
  expect_identical(.deriveContigSampleNames(paths), c("donorA", "donorB"))
})

test_that("the unfiltered Cell Ranger name is treated the same way", {
  paths <- file.path("root", "donorA", "all_contig_annotations.csv")
  expect_identical(.deriveContigSampleNames(paths), "donorA")
})

test_that("a renamed file keeps its own stem", {
  ## Renaming is the only signal the user gave about what the sample is called.
  paths <- file.path("root", "run1", "donorA_tcr.csv")
  expect_identical(.deriveContigSampleNames(paths), "donorA_tcr")
})

test_that("renamed and standard files are decided per path, not all or none", {
  paths <- c(
    file.path("root", "run1", "donorA_tcr.csv"),
    file.path("root", "donorB", "filtered_contig_annotations.csv")
  )
  expect_identical(.deriveContigSampleNames(paths), c("donorA_tcr", "donorB"))
})

test_that("colliding renamed files fall back to their directories", {
  paths <- c(
    file.path("root", "donorA", "contigs.csv"),
    file.path("root", "donorB", "contigs.csv")
  )
  expect_identical(.deriveContigSampleNames(paths), c("donorA", "donorB"))
})

test_that("names that cannot be told apart ask for sample_names", {
  paths <- c(
    file.path("root", "same", "filtered_contig_annotations.csv"),
    file.path("root", "same", "filtered_contig_annotations.csv")
  )
  expect_error(.deriveContigSampleNames(paths), "sample_names")
})
