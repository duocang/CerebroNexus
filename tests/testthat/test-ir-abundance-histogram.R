ir_histogram_scope <- function() {
  ir_scope(c(
    "ir_row_values",
    "ir_bind_clone_histograms",
    "ir_abundance_histogram"
  ))
}

ir_histogram_fixture <- function() {
  list(
    s1 = data.frame(
      barcode = paste0("a", 1:7),
      CTgene = c(
        "TRAV1.TRAJ1_TRBV2.TRBJ1",
        "TRAV1.TRAJ1_TRBV2.TRBJ1",
        "TRAV3.TRAJ1_TRBV2.TRBJ1",
        "TRAV3.TRAJ1_NA",
        "NA_TRBV9.TRBJ1",
        "IGHV1.IGHJ1.IGHM_IGKV2.IGKJ1.IGKC",
        "TRAV5.TRAJ1_TRBV2.TRBJ1;TRBV4.TRBJ1"
      ),
      CTaa = c(
        "CAA_CBB",
        "CAA_CBB",
        "CAC_CBB",
        "CAC_NA",
        "NA_CDD",
        "CHH_CKK",
        "CAE_CBB;CBF"
      ),
      group = c("x", "y", "x", "y", "x", "x", NA),
      stringsAsFactors = FALSE
    ),
    s2 = data.frame(
      barcode = paste0("b", 1:4),
      CTgene = c(
        "TRAV1.TRAJ1_TRBV2.TRBJ1",
        "TRAV1.TRAJ1_TRBV2.TRBJ1",
        "TRAV1.TRAJ1_TRBV2.TRBJ1",
        "TRAV7.TRAJ1_TRBV8.TRBJ1"
      ),
      CTaa = c("CAA_CBB", "CAA_CBB", "CAA_CBB", "CAG_CBH"),
      group = c("x", "x", "y", "y"),
      stringsAsFactors = FALSE
    )
  )
}

## clonalAbundance(exportTable = TRUE) as a histogram: one row per (unit,
## abundance) with the number of clones, NA clones left out as the app does.
ir_scrrepertoire_histogram <- function(data, chain, group_by) {
  table <- scRepertoire::clonalAbundance(
    data,
    cloneCall = "aa",
    chain = chain,
    group.by = group_by,
    exportTable = TRUE
  )
  unit <- if (is.null(group_by)) table$values else table[[group_by]]
  keep <- !is.na(table$CTaa) & !is.na(unit)
  counts <- as.data.frame(
    table(sample = unit[keep], abundance = table$Abundance[keep]),
    stringsAsFactors = FALSE
  )
  counts <- counts[counts$Freq > 0L, ]
  out <- data.frame(
    sample = counts$sample,
    abundance = as.integer(counts$abundance),
    n_clones = as.integer(counts$Freq)
  )
  out[order(out$sample, out$abundance), , drop = FALSE]
}

test_that("the Abundance histogram matches scRepertoire for chains and groups", {
  skip_if_not_installed("scRepertoire")
  scope <- ir_histogram_scope()
  data <- ir_histogram_fixture()
  for (chain in c("both", "TRA", "TRB")) {
    for (group_by in list(NULL, "group")) {
      encoded <- scope$ir_encode_repertoire(data)
      actual <- scope$ir_abundance_histogram(
        encoded,
        "CTaa",
        chain,
        scope$ir_row_values(encoded, group_by, NULL)
      )
      actual <- actual[order(actual$sample, actual$abundance), , drop = FALSE]
      expected <- suppressWarnings(
        ir_scrrepertoire_histogram(data, chain, group_by)
      )
      expect_equal(
        unname(as.list(actual)),
        unname(as.list(expected)),
        info = paste(chain, if (is.null(group_by)) "samples" else group_by)
      )
    }
  }
})

test_that("isotype counts are the cells per group and isotype", {
  scope <- ir_scope("bcr_isotype_counts", "server.R", ir_histogram_scope())
  data <- list(
    d1 = data.frame(
      CTgene = c(
        "IGHV1.IGHJ1.IGHM_IGKV1.IGKJ1.IGKC",
        "IGHV1.IGHJ1.IGHG1_IGKV1.IGKJ1.IGKC",
        "IGHV1.IGHJ1.IGHM_NA",
        "TRAV1.TRAJ1_TRBV1.TRBJ1",
        "IGHV2.IGHJ1.IGHA1_NA"
      ),
      patient = c("p1", "p2", "p1", "p1", NA),
      stringsAsFactors = FALSE
    ),
    d2 = data.frame(
      CTgene = c("IGHV1.IGHJ1.IGHM_NA", "IGHV3.IGHJ1.IGHD_NA"),
      patient = c("p2", "p2"),
      stringsAsFactors = FALSE
    )
  )
  encoded <- scope$ir_encode_repertoire(data)
  expect_identical(
    scope$bcr_isotype_counts(
      encoded,
      scope$ir_row_values(encoded, "sample", NULL),
      "sample"
    ),
    data.frame(
      sample = c("d1", "d1", "d1", "d2", "d2"),
      isotype = c("IGHM", "IGHG1", "IGHA1", "IGHM", "IGHD"),
      n = c(2L, 1L, 1L, 1L, 1L)
    )
  )
  expect_identical(
    scope$bcr_isotype_counts(
      encoded,
      scope$ir_row_values(encoded, "patient", NULL),
      "patient"
    ),
    data.frame(
      patient = c("p1", "p2", "p2", "p2"),
      isotype = c("IGHM", "IGHG1", "IGHM", "IGHD"),
      n = c(2L, 1L, 1L, 1L)
    )
  )
  ## A TCR-only repertoire has no isotype to draw.
  expect_null(scope$bcr_isotype_counts(
    scope$ir_encode_repertoire(list(
      d = data.frame(
        CTgene = "TRAV1.TRAJ1_TRBV1.TRBJ1"
      )
    )),
    "d"
  ))
})
