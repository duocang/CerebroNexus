## Tests for the external-backend sibling that `createShinyApp()` has to carry
## into the generated bundle.
##
## A `.crb` written with `expression_matrix_mode = "h5"` or `"bpcells"` holds no
## expression matrix. It holds a backend tag naming a sibling file or directory,
## which the running app resolves relative to the `.crb`. Bundling the `.crb`
## without its sibling produces an app that only fails once a user opens it.

skip_if_not_installed("Seurat")
skip_if_not_installed("SeuratObject")
skip_if_not_installed("HDF5Array")

## ---------------------------------------------------------------------------
## Fixture
## ---------------------------------------------------------------------------

## Export a small object with an external h5 backend. Returns the .crb path;
## the sibling sits next to it, named after the .crb's stem.
write_h5_crb <- function(dir, stem = "ds", n_genes = 40, n_cells = 12) {
  set.seed(7)
  counts <- matrix(
    rpois(n_genes * n_cells, 3),
    nrow = n_genes,
    ncol = n_cells,
    dimnames = list(
      paste0("g", seq_len(n_genes)),
      paste0("c", seq_len(n_cells))
    )
  )
  obj <- Seurat::CreateSeuratObject(
    counts = methods::as(counts, "CsparseMatrix")
  )
  obj <- Seurat::NormalizeData(obj, verbose = FALSE)
  obj$sample <- rep(c("s1", "s2"), length.out = n_cells)
  obj$cluster <- factor(rep(c("c1", "c2"), length.out = n_cells))
  obj[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings = matrix(
      rnorm(n_cells * 2),
      ncol = 2,
      dimnames = list(colnames(obj), c("UMAP_1", "UMAP_2"))
    ),
    key = "UMAP_",
    assay = "RNA"
  )

  crb <- file.path(dir, paste0(stem, ".crb"))
  exportFromSeurat(
    object = obj,
    file = crb,
    experiment_name = "sibling test",
    organism = "mm",
    groups = c("sample", "cluster"),
    nUMI = "nCount_RNA",
    nGene = "nFeature_RNA",
    expression_matrix_mode = "h5",
    verbose = FALSE
  )
  crb
}

## ---------------------------------------------------------------------------
## Tests
## ---------------------------------------------------------------------------

test_that("the sibling named by the backend tag is bundled with the .crb", {
  root <- withr::local_tempdir()
  crb <- write_h5_crb(root)
  expect_true(file.exists(file.path(root, "ds.h5")))

  app_dir <- file.path(root, "app")
  expect_no_error(createShinyApp(
    cerebro_data = c("Dataset" = crb),
    result_dir = app_dir,
    launch_browser = FALSE,
    verbose = FALSE
  ))

  ## The sibling has to keep the name the backend tag holds: the app resolves
  ## it relative to the .crb by that name, whatever the .crb ends up called.
  location <- readRDS(crb)$getExpressionBackend()$location
  expect_equal(location, "ds.h5")
  expect_true(file.exists(file.path(app_dir, "data", location)))
})

test_that("renaming the .crb does not lose its sibling", {
  ## The sibling used to be located by guessing `<crb stem>.h5`. Renaming the
  ## .crb -- an ordinary thing to do when giving datasets readable names --
  ## made the guess miss, while the sibling sat right there under the name the
  ## backend tag records. The miss was silent: the bundle was written without
  ## the matrix and failed only when someone opened the app.
  root <- withr::local_tempdir()
  crb <- write_h5_crb(root)
  renamed <- file.path(root, "readable_name.crb")
  expect_true(file.rename(crb, renamed))

  app_dir <- file.path(root, "app")
  expect_no_error(createShinyApp(
    cerebro_data = c("Dataset" = renamed),
    result_dir = app_dir,
    launch_browser = FALSE,
    verbose = FALSE
  ))

  ## copied under the tag's name, which is what the app resolves, and not
  ## under the .crb's new stem
  expect_true(file.exists(file.path(app_dir, "data", "ds.h5")))
  expect_false(file.exists(file.path(app_dir, "data", "readable_name.h5")))
})

test_that("a sibling that is genuinely absent stops the bundle being built", {
  ## Copying the .crb on its own is the easy mistake: it looks like one file.
  ## The generated app would start and then fail on the first expression query.
  root <- withr::local_tempdir()
  crb <- write_h5_crb(root)
  expect_true(file.remove(file.path(root, "ds.h5")))

  expect_error(
    createShinyApp(
      cerebro_data = c("Dataset" = crb),
      result_dir = file.path(root, "app"),
      launch_browser = FALSE,
      verbose = FALSE
    ),
    regexp = "h5"
  )
})

test_that("the failure names the file it looked for and the .crb it came from", {
  root <- withr::local_tempdir()
  crb <- write_h5_crb(root)
  file.remove(file.path(root, "ds.h5"))

  err <- tryCatch(
    createShinyApp(
      cerebro_data = c("Dataset" = crb),
      result_dir = file.path(root, "app"),
      launch_browser = FALSE,
      verbose = FALSE
    ),
    error = function(e) conditionMessage(e)
  )
  expect_true(grepl("ds.h5", err, fixed = TRUE))
  expect_true(grepl("ds.crb", err, fixed = TRUE))
  ## and points at the way out, matching the runtime's own wording
  expect_true(grepl("expression_matrix_h5", err, fixed = TRUE))
})

test_that("an absolute override in cerebro_options is not treated as missing", {
  ## `Cerebro.options[["expression_matrix_h5"]]` outranks sibling resolution at
  ## runtime, so a bundle configured that way is expected to have no sibling
  ## next to the .crb. Demanding one would reject a working configuration.
  root <- withr::local_tempdir()
  crb <- write_h5_crb(root)
  elsewhere <- file.path(root, "elsewhere")
  dir.create(elsewhere)
  moved <- file.path(elsewhere, "matrix.h5")
  expect_true(file.rename(file.path(root, "ds.h5"), moved))

  expect_no_error(createShinyApp(
    cerebro_data = c("Dataset" = crb),
    result_dir = file.path(root, "app"),
    launch_browser = FALSE,
    cerebro_options = list(expression_matrix_h5 = moved),
    verbose = FALSE
  ))
})

test_that("two data sets claiming one sibling name are refused, not overwritten", {
  ## Exporting `ds.crb` twice in different directories is enough to give two
  ## data sets the same location tag, and every sibling lands in the one
  ## `data/`. The second copy would overwrite the first and both .crb files
  ## would then read the same matrix -- silently, and with the right dimensions
  ## whenever the two data sets happen to be the same size.
  root <- withr::local_tempdir()
  first_dir <- file.path(root, "first")
  second_dir <- file.path(root, "second")
  dir.create(first_dir)
  dir.create(second_dir)

  first <- write_h5_crb(first_dir)
  second <- write_h5_crb(second_dir, n_cells = 16)

  expect_equal(readRDS(first)$getExpressionBackend()$location, "ds.h5")
  expect_equal(readRDS(second)$getExpressionBackend()$location, "ds.h5")

  err <- tryCatch(
    createShinyApp(
      cerebro_data = c("First" = first, "Second" = second),
      result_dir = file.path(root, "app"),
      launch_browser = FALSE,
      verbose = FALSE
    ),
    error = function(e) conditionMessage(e)
  )
  expect_true(grepl("ds.h5", err, fixed = TRUE))
  expect_true(grepl("overwrite", err, fixed = TRUE))
})

test_that("the same destination spelt two ways is still a collision", {
  ## `m.h5` and `./m.h5` name one file in the bundle but are two different
  ## strings. Comparing the strings is what the check does, so without
  ## normalising first, two data sets could claim one destination under
  ## different spellings and one would quietly overwrite the other -- the same
  ## data mix-up as above, reached by a different spelling.
  root <- withr::local_tempdir()
  first_dir <- file.path(root, "first")
  second_dir <- file.path(root, "second")
  dir.create(first_dir)
  dir.create(second_dir)

  first <- write_h5_crb(first_dir)
  second <- write_h5_crb(second_dir, n_cells = 16)

  aliased <- readRDS(second)
  aliased$setExpressionBackend(type = "h5", location = "./ds.h5")
  saveRDS(aliased, second)

  err <- tryCatch(
    createShinyApp(
      cerebro_data = c("First" = first, "Second" = second),
      result_dir = file.path(root, "app"),
      launch_browser = FALSE,
      verbose = FALSE
    ),
    error = function(e) conditionMessage(e)
  )
  expect_true(grepl("overwrite", err, fixed = TRUE))
})

test_that("destination aliases follow the bundle file system's case rules", {
  ## A string comparison cannot decide whether `ds.h5` and `DS.h5` are one
  ## destination: APFS and NTFS normally say yes, while a Linux file system
  ## normally says no. Exercise the collision only where the bundle's actual
  ## file system says the two spellings are aliases.
  root <- withr::local_tempdir()
  probe <- file.path(root, "case-probe")
  writeLines("probe", probe)
  skip_if_not(
    file.exists(file.path(root, "CASE-PROBE")),
    "bundle file system is case-sensitive"
  )

  first_dir <- file.path(root, "first")
  second_dir <- file.path(root, "second")
  dir.create(first_dir)
  dir.create(second_dir)

  first <- write_h5_crb(first_dir)
  second <- write_h5_crb(second_dir, n_cells = 16)

  aliased <- readRDS(second)
  aliased$setExpressionBackend(type = "h5", location = "DS.h5")
  saveRDS(aliased, second)

  ## POSIX realpath() is not required to return the directory entry's actual
  ## case. Simulate a case-insensitive volume where file.exists() recognises
  ## the alias but normalizePath() preserves the spelling supplied by its
  ## caller. Existence, not canonicalised text, must be the overwrite guard.
  copy_sibling <- .copyExpressionBackendSibling
  helper_env <- new.env(parent = environment(copy_sibling))
  helper_env$normalizePath <- function(path, winslash = "/", mustWork = NA) {
    resolved <- base::normalizePath(
      path,
      winslash = winslash,
      mustWork = mustWork
    )
    if (identical(basename(path), "DS.h5")) {
      return(file.path(dirname(resolved), "DS.h5"))
    }
    resolved
  }
  environment(copy_sibling) <- helper_env

  data_dir <- file.path(root, "data")
  dir.create(data_dir)
  claimed <- new.env(parent = emptyenv())
  copy_sibling(
    first,
    data_dir,
    claimed = claimed,
    verbose = FALSE
  )

  expect_error(
    copy_sibling(
      second,
      data_dir,
      claimed = claimed,
      verbose = FALSE
    ),
    regexp = "overwrite"
  )
})

test_that("a BPCells destination appearing during copy is never overwritten", {
  ## The existence check and recursive directory copy are separate operations.
  ## Simulate another writer creating the destination between them: recursive
  ## file.copy() defaults overwrite to TRUE, so the call itself also has to
  ## refuse replacement rather than relying only on the earlier check.
  root <- withr::local_tempdir()
  matrix_dir <- file.path(root, "matrix.bpcells")
  dir.create(matrix_dir)
  writeLines("SECOND", file.path(matrix_dir, "payload"))
  writeLines("SECOND", file.path(matrix_dir, "new-fragment"))

  crb <- file.path(root, "dataset.crb")
  saveRDS(
    list(
      getExpressionBackend = function() {
        list(type = "bpcells", location = "matrix.bpcells")
      }
    ),
    crb
  )

  data_dir <- file.path(root, "data")
  dir.create(data_dir)
  destination <- file.path(data_dir, "matrix.bpcells")

  copy_sibling <- .copyExpressionBackendSibling
  helper_env <- new.env(parent = environment(copy_sibling))
  helper_env$file.copy <- function(
    from,
    to,
    overwrite = recursive,
    recursive = FALSE,
    ...
  ) {
    if (!dir.exists(destination)) {
      dir.create(destination)
      writeLines("FIRST", file.path(destination, "payload"))
    }
    base::file.copy(
      from,
      to,
      overwrite = overwrite,
      recursive = recursive,
      ...
    )
  }
  environment(copy_sibling) <- helper_env

  expect_error(
    copy_sibling(crb, data_dir, verbose = FALSE),
    regexp = "Failed to copy"
  )
  expect_equal(readLines(file.path(destination, "payload")), "FIRST")
  expect_false(file.exists(file.path(destination, "new-fragment")))
})

test_that("overwrite FALSE replaces a complete existing sibling", {
  root <- withr::local_tempdir()
  first_dir <- file.path(root, "first")
  second_dir <- file.path(root, "second")
  dir.create(first_dir)
  dir.create(second_dir)

  first <- write_h5_crb(first_dir)
  second <- write_h5_crb(second_dir, n_cells = 16)
  app_dir <- file.path(root, "app")

  expect_no_error(createShinyApp(
    cerebro_data = c("Dataset" = first),
    result_dir = app_dir,
    launch_browser = FALSE,
    verbose = FALSE
  ))
  expect_no_error(createShinyApp(
    cerebro_data = c("Dataset" = second),
    result_dir = app_dir,
    overwrite = FALSE,
    launch_browser = FALSE,
    verbose = FALSE
  ))

  bundled <- file.path(app_dir, "data", "ds.h5")
  expect_identical(
    unname(tools::md5sum(bundled)),
    unname(tools::md5sum(file.path(second_dir, "ds.h5")))
  )
})

test_that("an empty override does not switch the sibling check off", {
  ## `!is.null("")` is TRUE, so a typo used to buy a free pass: the bundle was
  ## written without a matrix and the failure moved back to run time.
  root <- withr::local_tempdir()
  crb <- write_h5_crb(root)
  file.remove(file.path(root, "ds.h5"))

  expect_error(
    createShinyApp(
      cerebro_data = c("Dataset" = crb),
      result_dir = file.path(root, "app"),
      launch_browser = FALSE,
      cerebro_options = list(expression_matrix_h5 = ""),
      verbose = FALSE
    ),
    regexp = "single non-empty path"
  )
})

test_that("several external data sets sharing one global override warn", {
  ## `Cerebro.options` is one list for the whole app, so the override is not
  ## per-dataset: both .crb files would read the same matrix.
  root <- withr::local_tempdir()
  first_dir <- file.path(root, "first")
  second_dir <- file.path(root, "second")
  dir.create(first_dir)
  dir.create(second_dir)
  first <- write_h5_crb(first_dir)
  second <- write_h5_crb(second_dir)

  elsewhere <- file.path(root, "shared.h5")
  file.copy(file.path(first_dir, "ds.h5"), elsewhere)

  expect_warning(
    createShinyApp(
      cerebro_data = c("First" = first, "Second" = second),
      result_dir = file.path(root, "app"),
      launch_browser = FALSE,
      cerebro_options = list(expression_matrix_h5 = elsewhere),
      verbose = FALSE
    ),
    regexp = "same expression matrix"
  )
})

test_that("a location tag that climbs out of the bundle is refused", {
  ## The runtime resolves the tag relative to the .crb, so it has to stay
  ## relative. An absolute one is mis-joined here anyway --
  ## file.path("data", "/abs/m.h5") is "data//abs/m.h5".
  root <- withr::local_tempdir()
  crb <- write_h5_crb(root)

  escaped <- readRDS(crb)
  escaped$setExpressionBackend(type = "h5", location = "../outside.h5")
  saveRDS(escaped, crb)

  expect_error(
    createShinyApp(
      cerebro_data = c("Dataset" = crb),
      result_dir = file.path(root, "app"),
      launch_browser = FALSE,
      verbose = FALSE
    ),
    regexp = "not a path inside"
  )
})

test_that("an embedded .crb is bundled without looking for a sibling", {
  root <- withr::local_tempdir()
  crb <- write_h5_crb(root, stem = "embedded_source")

  ## rebuild the same data as an embedded .crb so the backend tag says so
  set.seed(7)
  obj <- readRDS(crb)
  expect_equal(obj$getExpressionBackend()$type, "h5")

  embedded_dir <- withr::local_tempdir()
  counts <- matrix(
    rpois(40 * 12, 3),
    nrow = 40,
    dimnames = list(paste0("g", 1:40), paste0("c", 1:12))
  )
  seurat <- Seurat::CreateSeuratObject(
    counts = methods::as(counts, "CsparseMatrix")
  )
  seurat <- Seurat::NormalizeData(seurat, verbose = FALSE)
  seurat$sample <- rep(c("s1", "s2"), length.out = 12)
  seurat[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings = matrix(
      rnorm(24),
      ncol = 2,
      dimnames = list(colnames(seurat), c("UMAP_1", "UMAP_2"))
    ),
    key = "UMAP_",
    assay = "RNA"
  )
  embedded_crb <- file.path(embedded_dir, "plain.crb")
  exportFromSeurat(
    object = seurat,
    file = embedded_crb,
    experiment_name = "embedded",
    organism = "mm",
    groups = "sample",
    nUMI = "nCount_RNA",
    nGene = "nFeature_RNA",
    verbose = FALSE
  )

  expect_no_error(createShinyApp(
    cerebro_data = c("Dataset" = embedded_crb),
    result_dir = file.path(embedded_dir, "app"),
    launch_browser = FALSE,
    verbose = FALSE
  ))
  expect_true(file.exists(file.path(embedded_dir, "app", "data", "plain.crb")))
})
