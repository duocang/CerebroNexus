hla_inst_file <- function(...) {
  installed <- system.file(..., package = "CerebroNexus")
  if (nzchar(installed) && file.exists(installed)) {
    return(installed)
  }
  testthat::test_path("../../inst", ...)
}

test_that("core shim binds locally without polluting globalenv", {
  local_env <- new.env(parent = globalenv())
  # The shim reads Cerebro.options[["cerebro_root"]] to locate its bundled core/
  # directory; supply it so the source path resolves in this isolated env.
  local_env$Cerebro.options <- list(cerebro_root = hla_inst_file())
  global_names <- c("hla_detect_chains", "hla_descriptive_feature_overlap")
  for (nm in global_names) {
    if (exists(nm, envir = .GlobalEnv, inherits = FALSE)) {
      rm(list = nm, envir = .GlobalEnv)
    }
  }

  source(
    hla_inst_file("viewer/hla_tcr_motifs/core_shim.R"),
    local = local_env
  )

  expect_true(exists("hla_detect_chains", envir = local_env, inherits = FALSE))
  expect_true(exists(
    "hla_descriptive_feature_overlap",
    envir = local_env,
    inherits = FALSE
  ))
  expect_false(exists(
    "hla_detect_chains",
    envir = .GlobalEnv,
    inherits = FALSE
  ))
  expect_false(exists(
    "hla_descriptive_feature_overlap",
    envir = .GlobalEnv,
    inherits = FALSE
  ))
})

test_that("bundled core shim resolves without an installed package", {
  repo_root <- normalizePath(testthat::test_path("../.."), mustWork = TRUE)
  app_root <- file.path(repo_root, "inst")
  shim_path <- file.path(
    app_root,
    "viewer/hla_tcr_motifs/core_shim.R"
  )
  # The shim sources its bundled core/ copies with no package on the search
  # path -- the exact condition of a createShinyApp bundle. Run it in a --vanilla
  # subprocess to prove no CerebroNexus install is needed. Under R CMD check
  # the source `inst/` tree is not at this path, so there is nothing to test --
  # skip rather than fail on the missing file.
  testthat::skip_if_not(
    file.exists(shim_path),
    "source tree not present (installed-package layout)"
  )
  expression <- paste0(
    "e <- new.env(parent = globalenv()); ",
    "e$Cerebro.options <- list(cerebro_root = ",
    deparse(app_root),
    "); ",
    "sys.source(",
    deparse(shim_path),
    ", envir = e); ",
    "stopifnot(",
    "exists('hla_distinct_colors', envir = e, inherits = FALSE), ",
    "exists('hla_descriptive_feature_overlap', envir = e, inherits = FALSE)",
    ")"
  )
  output <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", "-e", shQuote(expression)),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(output, "status")
  if (is.null(status)) {
    status <- 0L
  }

  expect_equal(status, 0L, info = paste(output, collapse = "\n"))
})

## ---- shipped demo contracts ------------------------------------------- ##
## One demo ships: real single cells, real paired TCR, real published donor
## genotypes. It makes claims the UI depends on, so if a rebuild drops one the
## page silently changes meaning -- donor-level counting reverts to
## sample-level, or the antigen-selection disclosure disappears while the
## carrier contrast remains on screen.

hla_sc_demo <- function() {
  path <- hla_inst_file("extdata/examples/demo_hla_tcr_dextramer.crb")
  testthat::skip_if_not(file.exists(path), "single-cell demo not built")
  readCerebro(path)
}

test_that("shipped demo declares antigen selection, cells and its receptor key", {
  ti <- hla_sc_demo()$technical_info
  # The repertoire was sorted for dextramer binding: not an unbiased sample of
  # the donors' repertoires, and the page prints so above the Associations.
  expect_equal(ti$tcr_selection, "antigen-selected")
  expect_true(nzchar(ti$tcr_selection_detail))
  expect_equal(ti$observation_unit, "cell")
  # This source identifies a receptor by V gene AND CDR3; CDR3-only nodes would
  # fuse receptors the source counts separately.
  expect_equal(ti$receptor_key, "v_gene+cdr3")
  # Declared, so class-based filtering never has to infer the lineage column.
  expect_equal(ti$lineage_column, "cell_type")
})

test_that("shipped demo HLA is genotyped and covers every sample", {
  crb <- hla_sc_demo()
  ht <- crb$getHLATyping()
  # Published (table S1), measured independently of these cells -- which is what
  # lets a carrier / non-carrier contrast mean anything here.
  expect_true(all(ht$source_type == "genotyped"))
  expect_setequal(unique(ht$sample), names(crb$getImmuneRepertoire()))
  # Sorted CD8+ T cells, so Class I only. The Class I x Class II pair scope is
  # gated on hla_pair_available() and therefore stays hidden on this demo.
  expect_setequal(unique(ht$locus), c("HLA-A", "HLA-B"))
})

test_that("shipped demo carries donor ids, so counting is donor-level", {
  ht <- hla_sc_demo()$getHLATyping()
  expect_false(any(is.na(ht$donor_id)))
  units <- hla_analysis_unit_map(ht, unique(ht$sample))
  expect_equal(unique(units$unit_type), "donor")
})

test_that("shipped demo yields a readable TRB motif network on real sequences", {
  # The whole argument for the page: a Hamming-1 CDR3 network is legible on an
  # ANTIGEN-SELECTED repertoire. An unselected one is sparse -- the real-sequence
  # predecessor gave 4 nodes in 2 motifs. Measured on the shipped random 2,000
  # cells: 38 nodes in 8 motifs, largest 12 (157 / 31 / 36 on all 12,000).
  # Assert well under those so a rebuild is not brittle, but far above the
  # near-empty scatter this replaced.
  crb <- hla_sc_demo()
  seg <- hla_parse_ir_segments(crb$getImmuneRepertoire(), "TRB")
  nodes <- hla_aggregate_cdr3_nodes(seg, by_v = TRUE)
  m <- hla_build_motif_groups(nodes, by_v = TRUE)$motif_df
  in_motif <- m[m$motif_size >= 2L, ]
  expect_gt(nrow(in_motif), 25L)
  expect_gte(length(unique(in_motif$motif_group)), 6L)
  expect_gte(max(in_motif$motif_size), 5L)
  # Isolated CDR3s must still dominate: a repertoire where everything clusters
  # would be its own kind of lie.
  expect_gt(nrow(m) - nrow(in_motif), nrow(in_motif))
})

test_that("shipped demo measures real genes for every cell", {
  crb <- hla_sc_demo()
  # Unlike the bulk cohort this replaced, these are sequenced cells: the matrix
  # must be non-empty and aligned to the metadata.
  expect_gt(nrow(crb$expression), 0L)
  expect_equal(ncol(crb$expression), nrow(crb$meta_data))
})

test_that("shipped demo keeps its expression block sparse", {
  # Normalized single-cell expression is ~90% zeros, and every other demo this
  # package ships is a dgCMatrix. A dense block here cost 184 MiB of session
  # memory and 4.5 MiB of installed package for nothing, so it is a size
  # regression worth failing on rather than a style preference.
  crb <- hla_sc_demo()
  expect_s4_class(crb$expression, "CsparseMatrix")
})

test_that("every shipped observation is paired alpha/beta", {
  # The vignette and NEWS both call this demo paired ab. combineTCR() writes
  # CTaa as "<alpha>_<beta>" and puts the literal string NA on a side it could
  # not resolve, so a non-empty CTaa is NOT evidence of pairing -- an earlier
  # build filtered on nzchar() alone and shipped 1,493 single-chain cells under
  # a "paired" label.
  ir <- hla_sc_demo()$getImmuneRepertoire()
  ctaa <- unlist(lapply(ir, function(x) x$CTaa), use.names = FALSE)
  parts <- strsplit(ifelse(is.na(ctaa), "", ctaa), "_", fixed = TRUE)
  paired <- vapply(
    parts,
    function(p) {
      length(p) == 2L && all(nzchar(p)) && !any(p %in% c("NA", "None"))
    },
    logical(1)
  )
  expect_true(all(paired))
})

test_that("shipped demo labels dextramer calls as reagent calls, not specificity", {
  # 10x's binarized flags are RAW BINDER CALLS. Naming them `antigen` /
  # `restricting_allele` asserts a validated peptide specificity and a presenting
  # allele that this data does not establish -- for most cells the bound
  # reagent's restriction is not even in the donor's published genotype.
  md <- hla_sc_demo()$getMetaData()
  expect_true(all(
    c(
      "dextramer_antigen",
      "dextramer_peptide",
      "dextramer_allele",
      "restriction_in_genotype"
    ) %in%
      colnames(md)
  ))
  # The over-claiming names must not come back.
  expect_false(any(c("antigen", "restricting_allele") %in% colnames(md)))
})

test_that("shipped demo exposes its cross-reactivity instead of describing it", {
  # restriction_in_genotype is the evidence column that keeps the binder calls
  # honest INSIDE the app: colour the projection by it and the noise is visible.
  # It is also declared as a group, so it reaches the network and its table.
  crb <- hla_sc_demo()
  md <- crb$getMetaData()
  expect_true(all(md$restriction_in_genotype %in% c("yes", "no", "unknown")))
  # If this ever came out clean, the calls would have stopped being raw 10x
  # calls and the documentation would need rewriting -- so assert the caveat.
  expect_gt(sum(md$restriction_in_genotype == "no"), 0L)
  expect_true("restriction_in_genotype" %in% crb$getGroups())
  # And the caveat travels with the object, not only in the vignette.
  expect_match(
    crb$technical_info$tcr_selection_detail,
    "RAW BINDER CALLS",
    fixed = TRUE
  )
})

test_that("an incompletely called locus is unknown, never a confirmed negative", {
  # Table S1 publishes ONE HLA-B allele for donors 1 and 2, so their second B
  # copy could be anything -- a B-restricted binder call there is undecidable,
  # not off-genotype. A two-state column would have to call it "no", inventing a
  # confirmed negative out of missing data. Same rule the carrier logic uses:
  # only a locus called at two copies can rule an allele out.
  crb <- hla_sc_demo()
  md <- crb$getMetaData()
  ht <- crb$getHLATyping()
  expect_gt(sum(md$restriction_in_genotype == "unknown"), 0L)

  for (i in seq_len(nrow(md))) {
    if (!identical(md$restriction_in_genotype[i], "no")) {
      next
    }
    locus <- hla_allele_locus(md$dextramer_allele[i])
    n_copies <- sum(ht$sample == md$sample[i] & ht$locus == locus)
    # every "no" must stand on a completely called locus
    expect_gte(n_copies, 2L)
  }
  # and every "unknown" must be a call the genotype genuinely cannot settle
  unknown <- md[md$restriction_in_genotype == "unknown", , drop = FALSE]
  if (nrow(unknown) > 0) {
    carried <- paste(ht$sample, ht$allele)
    expect_false(any(
      paste(unknown$sample, unknown$dextramer_allele) %in% carried
    ))
  }
})

test_that("the shipped demo's selection is a caveat the page can actually show", {
  crb <- hla_sc_demo()
  shiny::testServer(
    function(input, output, session) {
      data_set <- function() crb
      viewer_server_load(
        viewer_server_statements("hla_tcr_motifs", "data.R"),
        environment(),
        names = c("HLA_SELECTION_CAVEATS", "hla_selection_caveat")
      )
    },
    {
      caveat <- hla_selection_caveat()
      expect_true(length(caveat) > 0)
      expect_match(
        paste(unlist(caveat), collapse = " "),
        "ascertainment",
        ignore.case = TRUE
      )
    }
  )
})

test_that("the shipped demo ships its CC-BY attribution beside the data", {
  # data-raw/DATASETS.md holds the provenance but is .Rbuildignore'd, so an
  # installed user would otherwise receive the CC-BY data with no licensing
  # record. The attribution file lives in extdata so it installs with the demo.
  att <- hla_inst_file("extdata/examples/demo_hla_tcr_dextramer.ATTRIBUTION.md")
  expect_true(file.exists(att))
  txt <- viewer_test_text(att)
  expect_match(txt, "CC-BY", fixed = TRUE)
  expect_match(txt, "abf5835", fixed = TRUE) # the source paper
})

## ---- core_shim sources every core file, byte-for-byte ------------------ ##
## The shim sys.source()s a hardcoded list of the bundled core/ files -- no
## namespace fallback, so the bundle never names CerebroNexus. A core file
## present in R/ but missing from that list is never sourced, so its functions
## surface as "could not find function" in a running app while unit tests (which
## reach R/ directly) stay green. Pin the file list here, and pin the bundled
## copies byte-for-byte against R/ in the next test -- together they guarantee
## every R/hla_*.R function is reachable at runtime, in every launch mode.

test_that("core_shim sources every R/hla_*.R core file", {
  expected <- new.env(parent = globalenv())
  core <- hla_inst_file("viewer/hla_tcr_motifs/core")
  for (file in list.files(core, pattern = "[.]R$", full.names = TRUE)) {
    sys.source(file, expected)
  }
  actual <- new.env(parent = globalenv())
  actual$Cerebro.options <- list(cerebro_root = hla_inst_file())
  sys.source(hla_inst_file("viewer/hla_tcr_motifs/core_shim.R"), actual)
  expect_true(all(ls(expected) %in% ls(actual)))
})

test_that("bundled HLA core/ is byte-identical to the R/ source", {
  r_dir <- testthat::test_path("../../R")
  core_dir <- hla_inst_file("viewer/hla_tcr_motifs/core")
  # The shim sources these copies at runtime; they exist only so the module is
  # self-contained in a createShinyApp bundle (no R/, no package). If they drift
  # from R/, the bundle silently runs stale core code. This guard needs the
  # package R/ source tree, absent under the installed-package test layout --
  # skip there rather than fail.
  testthat::skip_if_not(
    dir.exists(r_dir) &&
      length(list.files(r_dir, pattern = "^hla_.*[.]R$")) > 0,
    "R/ source tree not present (installed-package layout)"
  )
  core_files <- c(
    "hla_typing.R",
    "hla_motif_core.R",
    "hla_association_core.R",
    "hla_visual_helpers.R",
    "hla_export.R"
  )
  for (f in core_files) {
    inst_copy <- file.path(core_dir, f)
    r_src <- file.path(r_dir, f)
    expect_true(
      file.exists(inst_copy),
      info = paste("missing bundled core copy:", f)
    )
    expect_identical(
      readLines(inst_copy, warn = FALSE),
      readLines(r_src, warn = FALSE),
      info = paste0(
        "inst/viewer/hla_tcr_motifs/core/",
        f,
        " drifted from R/",
        f,
        " -- re-copy R/hla_*.R into the module's core/ directory."
      )
    )
  }
})

test_that("the network caption states area encoding and the cap", {
  shiny::testServer(
    function(input, output, session) {
      hla_require_active_page <- function() invisible(NULL)
      hla_has_deps <- function() TRUE
      hla_segments <- function() data.frame(cdr3 = c("AAA", "AAB"))
      hla_motif_graph <- function() igraph::make_ring(2)
      hla_unit_noun <- function() "cell"
      hla_param <- function(name, default) default
      hla_motif_n_clusters <- function() 1L
      viewer_server_load(
        viewer_server_statements("hla_tcr_motifs", "visualizations.R"),
        environment(),
        names = c("HLA_MOTIF_MAX_LEGEND_CLUSTERS", "output$hla_motif_note")
      )
    },
    {
      text <- output$hla_motif_note$html
      expect_match(text, "Node AREA", fixed = TRUE)
      expect_match(text, as.character(HLA_NODE_MAX_EXACT), fixed = TRUE)
      expect_match(text, "same size", fixed = TRUE)
    }
  )
})

test_that("the page's nav gate scans every sample, like the core does", {
  # The IR module's detect_chains() stops after three samples. The HLA page is
  # gated on chains being present, and it is also the only route to its own
  # Data & QC tab -- so a cohort whose TCR happens to start at sample four would
  # be locked out of a page that could analyse it.
  late <- list(
    s1 = data.frame(CTgene = NA_character_, stringsAsFactors = FALSE),
    s2 = data.frame(CTgene = NA_character_, stringsAsFactors = FALSE),
    s3 = data.frame(CTgene = NA_character_, stringsAsFactors = FALSE),
    s4 = data.frame(
      CTgene = "TRAV1-2.TRAJ33.TRAC_TRBV19.TRBD1.TRBJ2-7.TRBC2",
      stringsAsFactors = FALSE
    )
  )
  expect_setequal(hla_detect_chains(late), c("TRA", "TRB"))

  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), scope)
  expect_true(scope$viewerHasTcrRepertoire(late))
})

test_that("every scrolling table keeps its cells on one line", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      typing <- hla_normalize_typing(
        data.frame(
          sample = c("s1", "s2"),
          locus = "HLA-A",
          allele = c("HLA-A*02:01", "HLA-A*01:01"),
          donor_id = c("d1", "d2")
        ),
        source_type = "genotyped"
      )
      hla_active_typing <- function() typing
      hla_has_typing <- function() TRUE
      hla_ir_samples <- function() c("s1", "s2")
      hla_unit_noun <- function() "cell"
      viewer_server_load(
        viewer_server_statements("hla_tcr_motifs", "data_qc.R"),
        scope,
        names = c(
          "output$hla_coverage_table",
          "output$hla_normalized_preview",
          "output$hla_donor_mapping_preview",
          "output$hla_download_normalized"
        )
      )
      viewer_server_load(
        viewer_server_statements("hla_tcr_motifs", "associations.R"),
        scope,
        names = c(
          "hla_overlap_breadth_skeleton",
          "output$hla_overlap_summary",
          "output$hla_overlap_table",
          "output$hla_allele_matrix"
        )
      )
    },
    {
      for (id in c(
        "hla_coverage_table",
        "hla_normalized_preview",
        "hla_donor_mapping_preview",
        "hla_overlap_summary",
        "hla_overlap_table",
        "hla_allele_matrix"
      )) {
        table <- jsonlite::fromJSON(output[[id]])$x
        expect_true(table$options$scrollX, info = id)
        expect_match(table$container, "nowrap", fixed = TRUE, info = id)
      }
      downloaded <- read.csv(
        output$hla_download_normalized,
        check.names = FALSE,
        na.strings = ""
      )
      expect_equal(downloaded$allele, scope$typing$allele)
      expect_equal(downloaded$donor_id, c("d1", "d2"))
    }
  )
})

test_that("Viewer Pack first frames bypass cold HLA preparation", {
  withr::local_package("magrittr")
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      available_crb_files <- list(selected = "tiny")
      data_set <- function() TRUE
      hla_active_chain <- function() "TRB"
      first <- list(
        filter_levels = list(sample = "s1"),
        filter_groups = "sample",
        initial_samples = "s1",
        by_v = FALSE,
        node_meta_cols = "sample",
        lineage_col = NULL,
        available_cols = "sample",
        segments = data.frame(sample = "s1", cdr3 = "AAA"),
        graph_raw = igraph::make_ring(3)
      )
      viewerPackCurrent <- function() "packed"
      viewerPackHlaFirstFrame <- function(pack, chain) {
        stopifnot(identical(pack, "packed"), identical(chain, "TRB"))
        if (isTRUE(input$corrupt)) NULL else first
      }
      viewerPackHlaSegments <- function(pack, chain) first$segments
      hla_ir_annotated <- function() stop("cold metadata hydration")
      hla_unfiltered_segments <- function() stop("cold segment parsing")
      hla_segments_fallback <- function() {
        data.frame(sample = "s2", cdr3 = "BBB")
      }
      hla_node_meta_cols <- function() "sample"
      hla_celltype_col <- function() NA_character_
      hla_default_min_nodes <- function() 2L
      hla_params_ready <- function() TRUE
      hla_motif_graph_cached <- function() igraph::make_empty_graph()
      viewer_server_load(
        viewer_server_statements("hla_tcr_motifs", "data.R"),
        scope,
        names = c(
          "hla_bindCache",
          "hla_param",
          "hla_first_frame",
          "hla_packed_segments",
          "hla_filter_groups",
          "hla_filter_levels",
          "hla_initial_samples",
          "hla_default_filter_selections",
          "hla_filter_input_id",
          "hla_filter_selections",
          "hla_first_frame_filters_match",
          "hla_by_v_default",
          "hla_segments",
          "hla_first_frame_graph_matches",
          "hla_motif_graph"
        )
      )
    },
    {
      expect_equal(hla_filter_levels(), list(sample = "s1"))
      expect_equal(hla_packed_segments(), scope$first$segments)
      expect_equal(hla_segments(), scope$first$segments)
      expect_equal(igraph::vcount(hla_motif_graph()), 3L)
      session$setInputs(hla_group_filter_TRB_sample = "s2")
      expect_equal(hla_segments()$cdr3, "BBB")
      expect_equal(igraph::vcount(hla_motif_graph()), 0L)
    }
  )
})
