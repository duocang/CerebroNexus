viewer_pack_fixture <- function(path, n = 3L, immune = FALSE) {
  object <- Cerebro$new()
  cells <- sprintf("cell-%03d", seq_len(n))
  object$setMetaData(data.frame(
    cell_barcode = cells,
    group = factor(rep(c("A", "B"), length.out = n)),
    score = seq_len(n) / 10,
    stringsAsFactors = FALSE
  ))
  object$addProjection(
    "umap",
    data.frame(
      x = seq_len(n),
      y = -seq_len(n),
      row.names = cells
    )
  )
  if (isTRUE(immune)) {
    object$addImmuneRepertoire(list(
      sample_1 = data.frame(
        barcode = cells,
        CTgene = rep("TRBV1.TRBJ1", n),
        CTaa = rep("CASSQ", n),
        CTnt = rep("TGTGCC", n),
        CTstrict = rep("TRBV1.TRBJ1;CASSQ", n),
        stringsAsFactors = FALSE
      )
    ))
  }
  saveCerebro(object, path, codec = "rds")
  path
}

test_that("Viewer Pack gating is dataset-level", {
  root <- tempfile("viewer-pack-gating-")
  dir.create(root)
  crb <- viewer_pack_fixture(file.path(root, "dataset.crb"), n = 3L)

  expect_null(buildViewerPack(
    crb,
    viewer_binary = "auto",
    viewer_binary_threshold = 4L
  ))
  expect_false(dir.exists(file.path(root, "dataset.viewer")))
  expect_null(buildViewerPack(
    crb,
    viewer_binary = "never",
    viewer_binary_threshold = 1L
  ))

  pack <- buildViewerPack(
    crb,
    viewer_binary = "auto",
    viewer_binary_threshold = 3L
  )
  expect_identical(
    normalizePath(pack, winslash = "/", mustWork = FALSE),
    normalizePath(
      file.path(root, "dataset.viewer"),
      winslash = "/",
      mustWork = FALSE
    )
  )
  expect_true(validateViewerPack(crb, pack)$valid)

  unlink(pack, recursive = TRUE)
  expect_identical(
    normalizePath(
      buildViewerPack(
        crb,
        viewer_binary = "always",
        viewer_binary_threshold = 999L
      ),
      winslash = "/",
      mustWork = FALSE
    ),
    normalizePath(pack, winslash = "/", mustWork = FALSE)
  )
})

test_that("Viewer Pack manifest validates canonical identity and assets", {
  root <- tempfile("viewer-pack-manifest-")
  dir.create(root)
  crb <- viewer_pack_fixture(
    file.path(root, "dataset.crb"),
    n = 4L,
    immune = TRUE
  )
  pack <- buildViewerPack(crb, viewer_binary = "always")
  manifest <- jsonlite::read_json(
    file.path(pack, "manifest.json"),
    simplifyVector = TRUE
  )

  expect_identical(manifest$schema_version, 1L)
  expect_identical(manifest$n_cells, 4L)
  expect_match(manifest$dataset_fingerprint, "^md5-crb-v1:[0-9a-f]{32}$")
  expect_match(
    manifest$cell_order_fingerprint,
    "^md5-cell-order-v1:[0-9a-f]{32}$"
  )
  expect_true(all(
    c(
      "common",
      "projections",
      "metadata",
      "immune",
      "hla_tcr"
    ) %in%
      manifest$modules
  ))
  expect_identical(manifest$immune_receptors, "TCR")
  expect_true(length(manifest$assets) >= 4L)
  expect_true(all(file.exists(file.path(pack, manifest$assets$path))))
  expect_true(all(manifest$assets$bytes > 0))
  expect_true(all(grepl("^[0-9a-f]{32}$", manifest$assets$checksum)))
  expect_true(validateViewerPack(crb, pack, full = TRUE)$valid)
})

test_that("Viewer Pack rejects corruption and preserves the CRB fallback", {
  root <- tempfile("viewer-pack-corrupt-")
  dir.create(root)
  crb <- viewer_pack_fixture(file.path(root, "dataset.crb"), n = 3L)
  pack <- buildViewerPack(crb, viewer_binary = "always")
  manifest <- jsonlite::read_json(
    file.path(pack, "manifest.json"),
    simplifyVector = TRUE
  )
  asset <- file.path(pack, manifest$assets$path[[1L]])
  writeBin(as.raw(1:3), asset)

  checked <- validateViewerPack(crb, pack, full = TRUE)
  expect_false(checked$valid)
  expect_match(checked$reason, "checksum|size")
  expect_null(readViewerPack(crb, pack))

  expect_error(
    buildViewerPack(crb, viewer_binary = "always"),
    "already exists"
  )
  rebuilt <- buildViewerPack(
    crb,
    viewer_binary = "always",
    overwrite = TRUE
  )
  expect_true(validateViewerPack(crb, rebuilt, full = TRUE)$valid)
})

test_that("Viewer Pack runtime loads HLA assets with exact fallback", {
  root <- tempfile("viewer-pack-runtime-")
  dir.create(root)
  crb <- viewer_pack_fixture(
    file.path(root, "dataset.crb"),
    n = 4L,
    immune = TRUE
  )
  pack <- buildViewerPack(crb, viewer_binary = "always")
  object <- readCerebro(crb)
  runtime <- new.env(parent = globalenv())
  sys.source(
    testthat::test_path("..", "..", "inst", "viewer", "core", "viewer_pack.R"),
    envir = runtime
  )

  descriptor <- runtime$viewerPackOpen(crb, object)
  expect_type(descriptor, "list")
  expect_false(exists(
    ".cell_order_valid",
    envir = descriptor$cache,
    inherits = FALSE
  ))
  expected_annotated <- hla_annotate_ir_metadata(
    object$getImmuneRepertoire(),
    object$getMetaData()
  )
  expect_identical(
    runtime$viewerPackHlaSegments(descriptor, "TRB"),
    hla_parse_ir_segments(expected_annotated, "TRB")
  )
  expect_true(get(
    ".cell_order_valid",
    envir = descriptor$cache,
    inherits = FALSE
  ))

  misaligned <- runtime$viewerPackOpen(crb, object)
  misaligned$cells <- rev(misaligned$cells)
  expect_null(runtime$viewerPackHlaSegments(misaligned, "TRB"))

  immune <- runtime$viewerPackImmuneIndex(descriptor, "TCR")
  expect_identical(immune$cell_index, 1:4)
  expect_identical(immune$clone, rep("TRBV1.TRBJ1", 4L))
  expect_identical(immune$ctaa, rep("CASSQ", 4L))
  expect_identical(immune$expansion, rep(2L, 4L))

  abundance <- runtime$viewerPackImmuneAbundance(descriptor, "CTgene")
  expect_identical(
    abundance,
    data.frame(
      sample = "sample_1",
      abundance = 4L,
      n_clones = 1L,
      stringsAsFactors = FALSE
    )
  )

  manifest <- jsonlite::read_json(
    file.path(pack, "manifest.json"),
    simplifyVector = TRUE
  )
  trb <- manifest$assets$path[grepl("hla_tcr/TRB[.]qs2$", manifest$assets$path)]
  writeBin(as.raw(1:3), file.path(pack, trb))
  expect_null(runtime$viewerPackHlaSegments(
    runtime$viewerPackOpen(crb, object),
    "TRB"
  ))
})

test_that("Viewer Pack runtime fails closed on incompatible identity", {
  root <- tempfile("viewer-pack-identity-")
  dir.create(root)
  crb <- viewer_pack_fixture(file.path(root, "dataset.crb"), n = 3L)
  pack <- buildViewerPack(crb, viewer_binary = "always")
  object <- readCerebro(crb)
  runtime <- new.env(parent = globalenv())
  sys.source(
    testthat::test_path("..", "..", "inst", "viewer", "core", "viewer_pack.R"),
    envir = runtime
  )
  manifest_path <- file.path(pack, "manifest.json")
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)

  manifest$schema_version <- 999L
  jsonlite::write_json(
    manifest,
    manifest_path,
    auto_unbox = TRUE,
    dataframe = "rows"
  )
  expect_null(runtime$viewerPackOpen(crb, object))

  manifest$schema_version <- 1L
  manifest$dataset_fingerprint <- "md5-crb-v1:00000000000000000000000000000000"
  jsonlite::write_json(
    manifest,
    manifest_path,
    auto_unbox = TRUE,
    dataframe = "rows"
  )
  expect_null(runtime$viewerPackOpen(crb, object))
})

test_that("Viewer wires valid HLA assets behind the CRB fallback", {
  server <- paste(
    readLines(testthat::test_path(
      "..",
      "..",
      "inst",
      "viewer",
      "shiny_server.R"
    )),
    collapse = "\n"
  )
  data_layer <- paste(
    readLines(testthat::test_path(
      "..",
      "..",
      "inst",
      "viewer",
      "hla_tcr_motifs",
      "data.R"
    )),
    collapse = "\n"
  )

  expect_match(server, "/viewer/core/viewer_pack.R", fixed = TRUE)
  expect_match(server, "viewerPackOpen(dataset_to_load, data)", fixed = TRUE)
  expect_match(
    data_layer,
    "viewerPackHlaSegments(viewerPackCurrent(), hla_active_chain())",
    fixed = TRUE
  )
  expect_match(data_layer, "data <- getImmuneRepertoire()", fixed = TRUE)
  expect_match(data_layer, "hla_parse_ir_segments(data, chain)", fixed = TRUE)

  immune_data <- paste(
    readLines(testthat::test_path(
      "..",
      "..",
      "inst",
      "viewer",
      "immune_repertoire",
      "data.R"
    )),
    collapse = "\n"
  )
  linked <- paste(
    readLines(testthat::test_path(
      "..",
      "..",
      "inst",
      "viewer",
      "coordinated_views",
      "bundle.R"
    )),
    collapse = "\n"
  )
  expect_match(
    immune_data,
    "viewerPackImmuneIndex(viewerPackCurrent()",
    fixed = TRUE
  )
  expect_match(
    immune_data,
    "viewerPackImmuneAbundance(viewerPackCurrent()",
    fixed = TRUE
  )
  expect_match(linked, "viewerPackImmuneIndex(pack,", fixed = TRUE)
})
