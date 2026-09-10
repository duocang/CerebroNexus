write_large_example_artifact <- function(file, sidecar) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  writeBin(as.raw(1), file)
  dir.create(sidecar, recursive = TRUE, showWarnings = FALSE)
  writeBin(as.raw(1), file.path(sidecar, "matrix"))
}

test_that("large example sizes resolve to fixed public sources", {
  expect_null(.largeExampleSpec(NULL))
  expect_null(.prepareLargeExample(NULL))

  pbmc <- .largeExampleSpec("50K")
  brain <- .largeExampleSpec("1m")
  expect_identical(c(pbmc$size, brain$size), c("50k", "1m"))
  expect_identical(c(pbmc$n_cells, brain$n_cells), c(50000L, 1000000L))
  expect_identical(c(pbmc$organism, brain$organism), c("Human", "Mouse"))
  expect_match(pbmc$matrix$url, "fresh_68k_pbmc_donor_a", fixed = TRUE)
  expect_match(brain$matrix$url, "1M_neurons", fixed = TRUE)
})

test_that("large example sizes reject unsupported values", {
  expect_error(.largeExampleSpec(character()), "one of NULL, '50k', or '1m'")
  expect_error(.largeExampleSpec(50000), "one of NULL, '50k', or '1m'")
  expect_error(.largeExampleSpec("500k"), "one of NULL, '50k', or '1m'")
})

test_that("large examples prepare multiple unique sizes in order", {
  root <- withr::local_tempdir()
  prepared <- character()
  build <- function(spec, paths) {
    prepared <<- c(prepared, spec$size)
    write_large_example_artifact(paths$seurat, paths$seurat_sidecar)
  }
  convert <- function(spec, paths) {
    write_large_example_artifact(paths$crb, paths$crb_sidecar)
    paths$crb
  }
  testthat::local_mocked_bindings(
    .requireLargeExamplePackages = function(...) invisible(TRUE),
    .downloadLargeExample = function(...) NULL,
    .extractLargeExampleArchive = function(...) NULL,
    .buildLargeExampleSeurat = build,
    .convertLargeExample = convert,
    .package = "CerebroNexus"
  )

  result <- .prepareLargeExample(c("1m", "50K"), root)

  expect_identical(prepared, c("1m", "50k"))
  expect_identical(
    names(result),
    c("1M mouse brain example", "50K PBMC example")
  )
  expect_error(.prepareLargeExample(character(), root), "non-empty")
  expect_error(.prepareLargeExample(c("50k", "50K"), root), "unique")
  expect_error(.prepareLargeExample(c("50k", NA_character_), root), "non-NA")
})

test_that("large example cache defaults outside the package", {
  cache <- .largeExampleCacheDir(NULL)

  expect_true(nzchar(cache))
  expect_match(cache, "CerebroNexus")
  expect_match(cache, "large-examples")
  expect_false(startsWith(
    normalizePath(cache, mustWork = FALSE),
    normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  ))

  custom <- file.path(tempdir(), "custom-large-examples")
  expect_identical(.largeExampleCacheDir(custom), custom)
})

test_that("large examples fail before download when build packages are missing", {
  available <- function(package, quietly = TRUE) package != "BPCells"

  expect_error(
    .requireLargeExamplePackages(available = available),
    "BPCells"
  )
})

test_that("large example downloads reuse an existing local file", {
  root <- withr::local_tempdir()
  dest <- file.path(root, "source.bin")
  writeBin(as.raw(1:4), dest)
  calls <- 0L
  downloader <- function(url, target) {
    calls <<- calls + 1L
    writeBin(as.raw(1:4), target)
  }

  expect_identical(
    .downloadLargeExample(
      "https://example.invalid/source",
      dest,
      downloader = downloader
    ),
    dest
  )
  expect_identical(calls, 0L)
})

test_that("large example downloads never publish partial files", {
  root <- withr::local_tempdir()
  dest <- file.path(root, "source.bin")
  downloader <- function(url, target) {
    writeBin(as.raw(1:2), target)
    stop("download failed")
  }

  expect_error(
    .downloadLargeExample(
      "https://example.invalid/source",
      dest,
      downloader = downloader
    ),
    "download failed"
  )
  expect_false(file.exists(dest))
  expect_length(list.files(root, pattern = "[.]part-", full.names = TRUE), 0L)
})

test_that("large example selection is exact and reproducible", {
  cells <- paste0("cell", seq_len(10))
  first <- .selectLargeExampleCells(cells, 5L)
  second <- .selectLargeExampleCells(cells, 5L)

  expect_identical(first, second)
  expect_length(first, 5L)
  expect_identical(first[c(1L, 5L)], cells[c(1L, 10L)])
})

test_that("large example selection rejects insufficient or duplicate cells", {
  expect_error(
    .selectLargeExampleCells(c("cell1", "cell2"), 3L),
    "source matrix has 2 cells"
  )
  expect_error(
    .selectLargeExampleCells(c("cell1", "cell1"), 1L),
    "must be unique"
  )
})

test_that("large example preparation reuses a complete CRB cache", {
  root <- withr::local_tempdir()
  spec <- .largeExampleSpec("50k")
  paths <- .largeExamplePaths(spec, root)
  write_large_example_artifact(paths$crb, paths$crb_sidecar)

  result <- .prepareLargeExample("50k", root)

  expect_identical(unname(result), paths$crb)
  expect_identical(names(result), spec$label)
})

test_that("large example preparation acquires, builds, and converts once", {
  root <- withr::local_tempdir()
  calls <- character()
  build <- function(spec, paths) {
    calls <<- c(calls, "build")
    write_large_example_artifact(paths$seurat, paths$seurat_sidecar)
    invisible(paths$seurat)
  }
  convert <- function(spec, paths) {
    calls <<- c(calls, "convert")
    write_large_example_artifact(paths$crb, paths$crb_sidecar)
    invisible(paths$crb)
  }
  testthat::local_mocked_bindings(
    .requireLargeExamplePackages = function(...) invisible(TRUE),
    .downloadLargeExample = function(...) calls <<- c(calls, "download"),
    .buildLargeExampleSeurat = build,
    .convertLargeExample = convert,
    .package = "CerebroNexus"
  )

  first <- .prepareLargeExample("1m", root)
  second <- .prepareLargeExample("1m", root)

  expect_identical(calls, c("download", "build", "convert"))
  expect_identical(first, second)
})

test_that("large example Seurat build keeps real metadata on disk", {
  skip_if_not_installed("SeuratObject")
  skip_if_not_installed("BPCells")
  root <- withr::local_tempdir()
  spec <- .largeExampleSpec("50k")
  spec$n_cells <- 5L
  paths <- .largeExamplePaths(spec, root)
  cells <- paste0("cell", seq_len(8))
  counts <- Matrix::sparseMatrix(
    i = rep(1:3, 8),
    j = rep(seq_len(8), each = 3),
    x = rep(1:3, 8),
    dims = c(3, 8),
    dimnames = list(paste0("gene", 1:3), cells)
  )
  input_dir <- file.path(root, "input.bpcells")
  BPCells::write_matrix_dir(
    methods::as(counts, "IterableMatrix"),
    dir = input_dir
  )
  counts <- BPCells::open_matrix_dir(input_dir)
  analyze <- function(object) {
    selected <- colnames(object)
    object$cluster_full <- rep(c("1", "2"), length.out = length(selected))
    embedding <- cbind(
      UMAP_1 = seq_along(selected),
      UMAP_2 = -seq_along(selected)
    )
    rownames(embedding) <- selected
    object[["full.umap"]] <- SeuratObject::CreateDimReducObject(
      embeddings = embedding,
      key = "fullUMAP_",
      assay = "RNA"
    )
    object
  }

  expect_identical(
    .buildLargeExampleSeurat(
      spec,
      paths,
      read_matrix = function(...) counts,
      analyze = analyze
    ),
    paths$seurat
  )

  object <- readRDS(paths$seurat)
  expect_s4_class(object, "Seurat")
  expect_equal(ncol(object), 5L)
  expect_true("umap" %in% names(object@reductions))
  expect_true(all(object$seurat_clusters %in% c("1", "2")))
  expect_identical(
    rownames(object@meta.data),
    rownames(SeuratObject::Embeddings(object, "umap"))
  )
  expect_true(.completeLargeExampleArtifact(
    paths$seurat,
    paths$seurat_sidecar
  ))
})

test_that("large example conversion uses counts and BPCells", {
  root <- withr::local_tempdir()
  spec <- .largeExampleSpec("50k")
  paths <- .largeExamplePaths(spec, root)
  dir.create(dirname(paths$seurat), recursive = TRUE)
  writeBin(as.raw(1), paths$seurat)
  captured <- NULL
  converter <- function(...) {
    captured <<- list(...)
    write_large_example_artifact(paths$crb, paths$crb_sidecar)
  }

  expect_identical(
    .convertLargeExample(spec, paths, converter = converter),
    paths$crb
  )
  expect_identical(captured$seurat_file, paths$seurat)
  expect_identical(captured$slot, "counts")
  expect_identical(captured$groups, c("sample", "seurat_clusters"))
  expect_identical(captured$expression_matrix_mode, "bpcells")
  expect_false(captured$add_most_expressed_genes)
})

test_that("launchCerebro appends a requested large example", {
  example <- system.file(
    "extdata/examples/example.crb",
    package = "CerebroNexus"
  )
  testthat::local_mocked_bindings(
    .prepareLargeExample = function(size, cache_dir) {
      expect_identical(size, c("50k", "1m"))
      expect_identical(cache_dir, "custom-cache")
      stats::setNames(
        rep(example, 2L),
        c("50K PBMC example", "1M mouse brain example")
      )
    },
    .package = "CerebroNexus"
  )
  had_options <- exists("Cerebro.options", envir = .GlobalEnv, inherits = FALSE)
  previous <- if (had_options) {
    get("Cerebro.options", envir = .GlobalEnv)
  } else {
    NULL
  }
  on.exit(
    {
      if (had_options) {
        assign("Cerebro.options", previous, envir = .GlobalEnv)
      } else if (
        exists("Cerebro.options", envir = .GlobalEnv, inherits = FALSE)
      ) {
        rm("Cerebro.options", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )

  app <- launchCerebro(
    crb_file_to_load = c("Existing" = example),
    large_example = c("50k", "1m"),
    large_example_cache_dir = "custom-cache",
    mode = "closed"
  )

  expect_s3_class(app, "shiny.appobj")
  expect_identical(
    names(get("Cerebro.options", envir = .GlobalEnv)$crb_file_to_load),
    c("Existing", "50K PBMC example", "1M mouse brain example")
  )
})

test_that("createShinyApp can build an example-only app", {
  example <- system.file(
    "extdata/examples/example.crb",
    package = "CerebroNexus"
  )
  source_dir <- withr::local_tempdir()
  examples <- file.path(source_dir, c("50k.crb", "1m.crb"))
  file.copy(example, examples)
  testthat::local_mocked_bindings(
    .prepareLargeExample = function(size, cache_dir) {
      expect_identical(size, c("50k", "1m"))
      expect_identical(cache_dir, "custom-cache")
      stats::setNames(
        examples,
        c("50K PBMC example", "1M mouse brain example")
      )
    },
    .package = "CerebroNexus"
  )
  output <- file.path(withr::local_tempdir(), "app")

  expect_identical(
    createShinyApp(
      cerebro_data = NULL,
      result_dir = output,
      large_example = c("50k", "1m"),
      large_example_cache_dir = "custom-cache",
      launch_browser = FALSE,
      verbose = FALSE
    ),
    output
  )
  config <- readRDS(file.path(output, "cerebro_config.rds"))
  expect_identical(
    names(config$crb_file_to_load),
    c("50K PBMC example", "1M mouse brain example")
  )
})
