demo_compressed_script <- function() {
  testthat::test_path(
    "..",
    "..",
    "data-raw",
    "large-demo",
    "h5_compressed_variant.R"
  )
}

test_that("the compressed sidecar holds the same counts as the original", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("rhdf5")
  skip_if_not(file.exists(demo_compressed_script()), "repository-only script")
  env <- new.env(parent = globalenv())
  sys.source(demo_compressed_script(), envir = env)

  set.seed(3)
  genes_by_cells <- abs(Matrix::rsparsematrix(40L, 700L, 0.1))
  genes_by_cells@x <- round(genes_by_cells@x * 10) + 1
  dimnames(genes_by_cells) <- list(paste0("g", 1:40), paste0("c", 1:700))
  original <- withr::local_tempfile(fileext = ".h5")
  HDF5Array::writeTENxMatrix(
    methods::as(Matrix::t(genes_by_cells), "CsparseMatrix"),
    original,
    group = "expression",
    level = 0L
  )

  ## a step shorter than the data exercises the piecewise copy
  compressed <- withr::local_tempfile(fileext = ".h5")
  env$.demoRecompressH5(
    original,
    compressed,
    level = 3L,
    chunk = 1024L,
    step = 1000,
    verbose = FALSE
  )
  for (dataset in c("data", "indices", "indptr", "shape", "genes", "barcodes")) {
    expect_identical(
      as.vector(rhdf5::h5read(compressed, paste0("expression/", dataset))),
      as.vector(rhdf5::h5read(original, paste0("expression/", dataset))),
      info = dataset
    )
  }

  ## the two long datasets are chunked at the requested length
  handle <- rhdf5::H5Fopen(compressed)
  withr::defer(rhdf5::H5Fclose(handle))
  values <- rhdf5::H5Dopen(handle, "expression/data")
  withr::defer(rhdf5::H5Dclose(values))
  plist <- rhdf5::H5Dget_create_plist(values)
  withr::defer(rhdf5::H5Pclose(plist))
  expect_identical(as.numeric(rhdf5::H5Pget_chunk(plist)), 1024)
})

test_that("the compressed variant lives next to the CRB under a fixed name", {
  skip_if_not(file.exists(demo_compressed_script()), "repository-only script")
  env <- new.env(parent = globalenv())
  sys.source(demo_compressed_script(), envir = env)

  paths <- env$.demoCompressedPaths("/cache/ren/cerebro-v2/ren.crb")
  expect_identical(paths$crb, "/cache/ren/cerebro-v2/ren_compressed.crb")
  expect_identical(paths$h5, "/cache/ren/cerebro-v2/ren_compressed.h5")
  expect_identical(paths$source_h5, "/cache/ren/cerebro-v2/ren.h5")
  ## the Viewer pairs a CRB with the H5 of the same stem
  expect_identical(sub("[.]crb$", ".h5", paths$crb), paths$h5)
})

test_that("a compressed variant is reused only when its stamp matches", {
  skip_if_not(file.exists(demo_compressed_script()), "repository-only script")
  env <- new.env(parent = globalenv())
  sys.source(demo_compressed_script(), envir = env)

  root <- withr::local_tempdir()
  crb <- file.path(root, "demo.crb")
  writeLines("source", file.path(root, "demo.h5"))
  paths <- env$.demoCompressedPaths(crb)
  stamp <- env$.demoCompressedStamp(paths, level = 3L, chunk = 65536L)
  expect_false(env$.demoCompressedComplete(paths, stamp))

  file.create(paths$crb, paths$h5)
  expect_false(env$.demoCompressedComplete(paths, stamp))
  writeLines(stamp, paths$stamp)
  expect_true(env$.demoCompressedComplete(paths, stamp))

  ## other deflate settings, or a changed source, rebuild it
  expect_false(env$.demoCompressedComplete(
    paths,
    env$.demoCompressedStamp(paths, level = 6L, chunk = 65536L)
  ))
  writeLines("a longer source", file.path(root, "demo.h5"))
  expect_false(env$.demoCompressedComplete(
    paths,
    env$.demoCompressedStamp(paths, level = 3L, chunk = 65536L)
  ))
})

test_that("the compression settings come from the environment and are checked", {
  skip_if_not(file.exists(demo_compressed_script()), "repository-only script")
  env <- new.env(parent = globalenv())
  sys.source(demo_compressed_script(), envir = env)

  withr::local_envvar(c(CEREBRO_DEMO_H5_LEVEL = NA, CEREBRO_DEMO_H5_CHUNK = NA))
  expect_identical(env$.demoCompressionLevel(), 3L)
  expect_identical(env$.demoCompressionChunk(), 65536L)
  withr::local_envvar(c(CEREBRO_DEMO_H5_LEVEL = "6", CEREBRO_DEMO_H5_CHUNK = "4096"))
  expect_identical(env$.demoCompressionLevel(), 6L)
  expect_identical(env$.demoCompressionChunk(), 4096L)
  withr::local_envvar(c(CEREBRO_DEMO_H5_LEVEL = "0"))
  expect_error(env$.demoCompressionLevel(), "from 1 to 9")
  withr::local_envvar(c(CEREBRO_DEMO_H5_CHUNK = "10"))
  expect_error(env$.demoCompressionChunk(), "at least 1024")
})

test_that("the Viewer menu lists the compressed twin when it exists", {
  env <- new.env(parent = baseenv())
  sys.source(viewer_test_path("million_cell_demo.R"), envir = env)
  original <- list(
    crb_file_to_load = c(Small = "small.crb"),
    point_size = c(Small = 5),
    point_opacity = c(Small = 1),
    percentage_cells_to_show = 100
  )
  root <- withr::local_tempdir()
  crb <- file.path(root, "ren.crb")
  file.create(crb, file.path(root, "ren.h5"))
  label <- "Ren et al. COVID-19 atlas (1.46M + TCR/BCR)"

  alone <- env$viewerAddRenDemo(original, crb)
  expect_identical(names(alone$crb_file_to_load), c("Small", label))

  ## a twin whose H5 is missing is not offered
  file.create(file.path(root, "ren_compressed.crb"))
  half <- env$viewerAddRenDemo(original, crb)
  expect_identical(names(half$crb_file_to_load), c("Small", label))

  file.create(file.path(root, "ren_compressed.h5"))
  both <- env$viewerAddRenDemo(original, crb)
  twin <- paste(label, "[compressed]")
  expect_identical(names(both$crb_file_to_load), c("Small", label, twin))
  expect_identical(
    unname(both$crb_file_to_load[twin]),
    normalizePath(file.path(root, "ren_compressed.crb"))
  )
  ## the twin is drawn like the data set it copies
  for (key in c("point_size", "point_opacity", "percentage_cells_to_show")) {
    expect_identical(
      unname(both[[key]][twin]),
      unname(both[[key]][label]),
      info = key
    )
  }
})
