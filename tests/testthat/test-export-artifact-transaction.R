## Tests for the export artifact transaction helpers
## (R/utils-export-artifacts.R)
##
## Filesystem mechanics only -- no Seurat, no matrix backends, no Cerebro
## object. The exporter-level behaviour is covered in test-exportFromSeurat.R.

skip_if_not_installed("withr")

## Build a layout plus a staged .crb (and sibling, when the mode has one) so
## each test starts from "ordinary export work has finished".
stage_fixture <- function(root, mode, crb_text = "new", sibling_text = "new") {
  layout <- .newExportArtifactLayout(file.path(root, "dataset.crb"), mode)
  writeLines(crb_text, layout$staged_crb)
  if (!is.null(layout$staged_sibling)) {
    if (identical(mode, "bpcells")) {
      dir.create(layout$staged_sibling, showWarnings = FALSE)
      writeLines(sibling_text, file.path(layout$staged_sibling, "matrix"))
    } else {
      writeLines(sibling_text, layout$staged_sibling)
    }
  }
  layout
}

## Put a previous export in place, as a user would already have on disk.
place_previous <- function(
  layout,
  mode,
  crb_text = "old",
  sibling_text = "old"
) {
  writeLines(crb_text, layout$target_crb)
  if (!is.null(layout$target_sibling)) {
    if (identical(mode, "bpcells")) {
      dir.create(layout$target_sibling, showWarnings = FALSE)
      writeLines(sibling_text, file.path(layout$target_sibling, "matrix"))
    } else {
      writeLines(sibling_text, layout$target_sibling)
    }
  }
}

## ---------------------------------------------------------------------------
## Layout construction
## ---------------------------------------------------------------------------

test_that("layout validates its inputs", {
  root <- withr::local_tempdir()
  expect_error(
    .newExportArtifactLayout(c("a", "b"), "h5"),
    "single non-empty"
  )
  expect_error(
    .newExportArtifactLayout(NA_character_, "h5"),
    "single non-empty"
  )
  expect_error(
    .newExportArtifactLayout(file.path(root, "x.crb"), "zarr"),
    "embedded"
  )
})

test_that("layout creates a multi-level output directory", {
  root <- withr::local_tempdir()
  nested <- file.path(root, "a", "b", "c")
  layout <- .newExportArtifactLayout(file.path(nested, "dataset.crb"), "h5")
  expect_true(dir.exists(nested))
  expect_true(dir.exists(layout$stage_dir))
  expect_true(dir.exists(layout$backup_dir))
  ## staging must sit next to the target so publishing is a rename
  expect_identical(dirname(layout$stage_dir), nested)
  expect_identical(dirname(layout$backup_dir), nested)
})

test_that("layout names the sibling from the .crb stem, per mode", {
  root <- withr::local_tempdir()
  target <- file.path(root, "dataset.crb")

  embedded <- .newExportArtifactLayout(target, "embedded")
  expect_null(embedded$sibling_name)
  expect_null(embedded$target_sibling)

  h5 <- .newExportArtifactLayout(target, "h5")
  expect_identical(h5$sibling_name, "dataset.h5")
  expect_identical(h5$target_sibling, file.path(root, "dataset.h5"))

  bpcells <- .newExportArtifactLayout(target, "bpcells")
  expect_identical(bpcells$sibling_name, "dataset.bpcells")
})

test_that("two layouts in one directory do not collide", {
  root <- withr::local_tempdir()
  a <- .newExportArtifactLayout(file.path(root, "dataset.crb"), "h5")
  b <- .newExportArtifactLayout(file.path(root, "dataset.crb"), "h5")
  expect_false(identical(a$stage_dir, b$stage_dir))
  expect_false(identical(a$backup_dir, b$backup_dir))
})

## ---------------------------------------------------------------------------
## Backup / publish / restore
## ---------------------------------------------------------------------------

test_that("rollback restores a previous file pair", {
  root <- withr::local_tempdir()
  layout <- stage_fixture(root, "h5")
  place_previous(layout, "h5")

  .backupExportArtifacts(layout)
  expect_false(file.exists(layout$target_crb))
  expect_false(file.exists(layout$target_sibling))

  ## publish the sibling, then fail before the .crb goes out
  .publishStagedArtifact(layout$staged_sibling, layout$target_sibling)
  expect_identical(readLines(layout$target_sibling), "new")

  .restoreExportArtifacts(layout)
  expect_identical(readLines(layout$target_crb), "old")
  expect_identical(readLines(layout$target_sibling), "old")
})

test_that("rollback restores a previous directory sibling", {
  root <- withr::local_tempdir()
  layout <- stage_fixture(root, "bpcells")
  place_previous(layout, "bpcells")

  .backupExportArtifacts(layout)
  .publishStagedArtifact(layout$staged_sibling, layout$target_sibling)
  expect_identical(
    readLines(file.path(layout$target_sibling, "matrix")),
    "new"
  )

  .restoreExportArtifacts(layout)
  expect_true(dir.exists(layout$target_sibling))
  expect_identical(
    readLines(file.path(layout$target_sibling, "matrix")),
    "old"
  )
  expect_identical(readLines(layout$target_crb), "old")
})

test_that("rollback removes a partial first export", {
  root <- withr::local_tempdir()
  layout <- stage_fixture(root, "h5")
  ## nothing on disk beforehand
  .backupExportArtifacts(layout)
  .publishStagedArtifact(layout$staged_sibling, layout$target_sibling)
  expect_true(file.exists(layout$target_sibling))

  .restoreExportArtifacts(layout)
  expect_false(file.exists(layout$target_sibling))
  expect_false(file.exists(layout$target_crb))
})

test_that("publishing refuses to overwrite an unbacked-up target", {
  root <- withr::local_tempdir()
  layout <- stage_fixture(root, "h5")
  place_previous(layout, "h5")

  ## deliberately skip the backup step
  expect_error(
    .publishStagedArtifact(layout$staged_sibling, layout$target_sibling),
    "not backed up"
  )
  expect_identical(readLines(layout$target_sibling), "old")
})

test_that("publishing errors when nothing was staged", {
  root <- withr::local_tempdir()
  layout <- .newExportArtifactLayout(file.path(root, "dataset.crb"), "h5")
  expect_error(
    .publishStagedArtifact(layout$staged_crb, layout$target_crb),
    "Nothing staged"
  )
})

test_that("a full successful publish leaves the new pair in place", {
  root <- withr::local_tempdir()
  layout <- stage_fixture(root, "h5")
  place_previous(layout, "h5")

  .backupExportArtifacts(layout)
  .publishStagedArtifact(layout$staged_sibling, layout$target_sibling)
  .publishStagedArtifact(layout$staged_crb, layout$target_crb)
  .cleanupExportArtifactLayout(layout)

  expect_identical(readLines(layout$target_crb), "new")
  expect_identical(readLines(layout$target_sibling), "new")
  expect_false(dir.exists(layout$stage_dir))
  expect_false(dir.exists(layout$backup_dir))
})

test_that("restore never raises, so it cannot mask the original error", {
  root <- withr::local_tempdir()
  layout <- stage_fixture(root, "h5")
  place_previous(layout, "h5")
  .backupExportArtifacts(layout)

  ## make restoring the .crb impossible by removing the backup out from
  ## under it, and confirm we get a warning rather than an error
  unlink(layout$backup_crb)
  expect_no_error(.restoreExportArtifacts(layout))
})

test_that("cleanup is idempotent", {
  root <- withr::local_tempdir()
  layout <- .newExportArtifactLayout(file.path(root, "dataset.crb"), "bpcells")
  expect_no_error(.cleanupExportArtifactLayout(layout))
  expect_no_error(.cleanupExportArtifactLayout(layout))
  expect_no_error(.cleanupExportArtifactLayout(NULL))
  expect_false(dir.exists(layout$stage_dir))
})

test_that("embedded mode has no sibling to move", {
  root <- withr::local_tempdir()
  layout <- stage_fixture(root, "embedded")
  place_previous(layout, "embedded")

  .backupExportArtifacts(layout)
  expect_false(file.exists(layout$target_crb))
  .publishStagedArtifact(layout$staged_crb, layout$target_crb)
  expect_identical(readLines(layout$target_crb), "new")

  .cleanupExportArtifactLayout(layout)
  expect_false(dir.exists(layout$backup_dir))
})
