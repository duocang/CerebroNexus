test_that("the HLA/TCR main case ships a frozen biological answer", {
  case_file <- test_path(
    "..",
    "..",
    "inst",
    "extdata",
    "examples",
    "demo_hla_tcr_dextramer.case.json"
  )
  expect_true(file.exists(case_file))

  case <- jsonlite::fromJSON(case_file, simplifyVector = FALSE)
  expect_equal(case$schema, "cerebronexus-hla-tcr-end-to-end-case")
  expect_equal(case$version, 1)
  expect_equal(case$dataset$cell_count, 12000)
  expect_equal(case$golden_clonotype$chain, "TRB")
  expect_equal(case$golden_clonotype$v_gene, "TRBV19")
  expect_equal(case$golden_clonotype$j_gene, "TRBJ1-5")
  expect_equal(case$golden_clonotype$cdr3, "CASSIYSNQPQHF")
  expect_equal(case$golden_clonotype$cell_count, 10)
  expect_equal(case$golden_clonotype$donor_counts, list(donor1 = 2, donor2 = 8))
  expect_length(case$golden_clonotype$cell_barcodes, 10)
  expect_equal(
    case$golden_clonotype$restriction_in_genotype_counts,
    list(yes = 10, no = 0, unknown = 0)
  )
  expect_equal(case$motif$group, "TRBV19::M13_1")
  expect_equal(case$motif$node_count, 15)
  expect_equal(case$motif$cell_count, 30)
  expect_equal(
    case$motif$donor_counts,
    list(donor1 = 6, donor2 = 22, donor3 = 2)
  )
  expect_equal(case$viewer_clone_contract$clone_call, "CTgene")
  expect_equal(case$viewer_clone_contract$selection_authority, "barcode-list")
  expect_true(grepl(
    "two CTgene calls",
    case$viewer_clone_contract$note,
    fixed = TRUE
  ))
  cannot_say <- unlist(case$claims$cannot_say, use.names = FALSE)
  expect_true(any(grepl(
    "not validated peptide-level antigen specificity",
    cannot_say,
    ignore.case = TRUE
  )))
  expect_true(any(grepl("four donors", cannot_say, ignore.case = TRUE)))
})

test_that("the frozen case configuration selects stable barcodes in the shipped dataset", {
  root <- test_path("..", "..")
  case_dir <- file.path(root, "inst", "extdata", "examples")
  case <- jsonlite::fromJSON(
    file.path(case_dir, "demo_hla_tcr_dextramer.case.json"),
    simplifyVector = FALSE
  )
  config <- jsonlite::fromJSON(
    file.path(case_dir, "demo_hla_tcr_dextramer.linked-views.json"),
    simplifyVector = FALSE
  )
  source(
    file.path(root, "inst", "viewer", "coordinated_views", "config.R"),
    local = TRUE
  )
  crb <- readRDS(file.path(case_dir, "demo_hla_tcr_dextramer.crb"))
  normalized <- cv_config_decode(
    paste(
      readLines(file.path(
        case_dir,
        "demo_hla_tcr_dextramer.linked-views.json"
      )),
      collapse = "\n"
    ),
    cells = crb$getMetaData()$cell_barcode
  )
  barcode_file <- file.path(
    case_dir,
    "demo_hla_tcr_dextramer.golden-clone.barcodes.tsv"
  )
  expect_true(file.exists(barcode_file))
  barcodes <- readLines(barcode_file, warn = FALSE)
  expect_identical(barcodes[[1L]], "cell_barcode")
  expect_identical(
    sort(barcodes[-1L]),
    sort(unlist(case$golden_clonotype$cell_barcodes, use.names = FALSE))
  )
  expect_identical(config$schema, "cerebronexus-linked-view")
  expect_equal(config$version, 1)
  expect_equal(config$dataset$cell_count, case$dataset$cell_count)
  expect_identical(
    sort(unlist(config$selection$cells, use.names = FALSE)),
    sort(unlist(case$golden_clonotype$cell_barcodes, use.names = FALSE))
  )
  expect_identical(config$selection$source, "golden-clonotype-barcode-list")
  expect_identical(config$view$projections, list("umap"))
  expect_identical(config$view$colour$mode, "sample")
  expect_true(any(vapply(
    config$view$lenses,
    function(x) x$space == "projection::umap",
    logical(1)
  )))
  expect_identical(config$view$focus_space, "projection::umap")
  expect_true(any(vapply(
    config$view$lenses,
    function(x) x$space == "clone",
    logical(1)
  )))
  expect_identical(
    normalized$selection$cells,
    unname(unlist(config$selection$cells))
  )
})

test_that("the shipped case records source bytes and reproducible build commands", {
  root <- test_path("..", "..")
  case_dir <- file.path(root, "inst", "extdata", "examples")
  case <- jsonlite::fromJSON(
    file.path(case_dir, "demo_hla_tcr_dextramer.case.json"),
    simplifyVector = FALSE
  )
  crb <- file.path(case_dir, "demo_hla_tcr_dextramer.crb")
  digest <- if (nzchar(Sys.which("sha256sum"))) {
    sub("[[:space:]].*", "", system2("sha256sum", crb, stdout = TRUE))
  } else {
    sub(
      "[[:space:]].*",
      "",
      system2("shasum", c("-a", "256", crb), stdout = TRUE)
    )
  }
  sha <- trimws(readLines(file.path(case_dir, "demo_hla_tcr_dextramer.sha256")))
  expect_match(sha, digest, fixed = FALSE)
  expect_equal(case$dataset$sha256, sub("[[:space:]].*", "", sha))
  expect_true(file.exists(file.path(
    root,
    "data-raw",
    "build_hla_tcr_dextramer_demo.R"
  )))
  expect_true(file.exists(file.path(
    root,
    "data-raw",
    "prepare_hla_tcr_end_to_end_case.R"
  )))
  expect_true(file.exists(file.path(
    case_dir,
    "demo_hla_tcr_dextramer.golden-clone-umap.png"
  )))
  expect_true(file.exists(file.path(
    root,
    "docs",
    "hla-tcr-end-to-end-case.md"
  )))
})
