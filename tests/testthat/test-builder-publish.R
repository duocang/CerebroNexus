builder_publish_source <- function(local = parent.frame()) {
  root <- testthat::test_path("..", "..", "inst", "builder")
  if (!dir.exists(root)) {
    root <- system.file("builder", package = "CerebroNexus")
  }
  source(file.path(root, "core", "bundle_path_contract.R"), local = local)
  source(file.path(root, "publish.R"), local = local)
}

test_that("release identity is complete, stable, and rejects links", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(file.path(target, "matrix"), recursive = TRUE)
    writeLines("crb", file.path(target, "dataset.crb"))
    writeLines("matrix", file.path(target, "matrix", "part.bin"))

    first <- builder_release_identity(target)
    second <- builder_release_identity(target)
    expect_identical(first, second)
    expect_identical(
      vapply(first$entries, `[[`, character(1), "path"),
      c("dataset.crb", "matrix", "matrix/part.bin")
    )
    expect_identical(
      vapply(first$entries, `[[`, character(1), "type"),
      c("file", "directory", "file")
    )

    linked <- file.path(target, "linked")
    linked_ok <- tryCatch(
      file.symlink("dataset.crb", linked),
      error = function(e) FALSE
    )
    if (isTRUE(linked_ok)) {
      expect_error(builder_release_identity(target), "symbolic link")
    }
  })
})

test_that("compare-and-swap rejects a release changed after Review", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("reviewed", file.path(target, "dataset.crb"))
    expected <- builder_release_identity(target)
    handle <- builder_prepare_release(target, "build-cas", expected)
    writeLines("new", file.path(handle$stage, "dataset.crb"))
    writeLines("changed", file.path(target, "dataset.crb"))

    expect_error(
      builder_publish_release(handle),
      "changed after Review"
    )
    expect_identical(readLines(file.path(target, "dataset.crb")), "changed")
    expect_true(dir.exists(handle$stage))
    builder_abort_release(handle)
  })
})

test_that("process death between renames leaves exact recoverable evidence", {
  skip_if_not_installed("callr")
  local({
    root_path <- testthat::test_path("..", "..", "inst", "builder")
    if (!dir.exists(root_path)) {
      root_path <- system.file("builder", package = "CerebroNexus")
    }
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old", file.path(target, "dataset.crb"))
    crash <- callr::r_bg(
      function(builder_root, target) {
        source(file.path(builder_root, "core", "bundle_path_contract.R"))
        source(file.path(builder_root, "publish.R"))
        expected <- builder_release_identity(target)
        handle <- builder_prepare_release(target, "build-crash", expected)
        writeLines("new", file.path(handle$stage, "dataset.crb"))
        builder_publish_release(
          handle,
          .after_phase = function(phase) {
            if (identical(phase, "old_moved")) {
              quit(save = "no", status = 17L, runLast = FALSE)
            }
          }
        )
      },
      list(root_path, target)
    )
    crash$wait(timeout = 10000)
    expect_identical(crash$get_exit_status(), 17L)

    builder_publish_source()
    recovery <- builder_discover_recovery(target)
    expect_identical(recovery$state, "recovery_required")
    expect_true(dir.exists(recovery$backup))
    expect_false(dir.exists(target))

    restored <- builder_recover_release(target, action = "restore")
    expect_true(restored$recovered)
    expect_identical(readLines(file.path(target, "dataset.crb")), "old")
  })
})

test_that("process death after the new release can still restore the prior one", {
  skip_if_not_installed("callr")
  local({
    root_path <- testthat::test_path("..", "..", "inst", "builder")
    if (!dir.exists(root_path)) {
      root_path <- system.file("builder", package = "CerebroNexus")
    }
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old", file.path(target, "dataset.crb"))
    crash <- callr::r_bg(
      function(builder_root, target) {
        source(file.path(builder_root, "core", "bundle_path_contract.R"))
        source(file.path(builder_root, "publish.R"))
        expected <- builder_release_identity(target)
        handle <- builder_prepare_release(target, "build-new-crash", expected)
        writeLines("new", file.path(handle$stage, "dataset.crb"))
        builder_publish_release(
          handle,
          .after_phase = function(phase) {
            if (identical(phase, "new_published")) {
              quit(save = "no", status = 18L, runLast = FALSE)
            }
          }
        )
      },
      list(root_path, target)
    )
    crash$wait(timeout = 10000)
    expect_identical(crash$get_exit_status(), 18L)

    builder_publish_source()
    recovery <- builder_discover_recovery(target)
    expect_identical(recovery$phase, "new_published")
    expect_identical(readLines(file.path(target, "dataset.crb")), "new")
    restored <- builder_recover_release(target, action = "restore")
    expect_true(restored$recovered)
    expect_identical(readLines(file.path(target, "dataset.crb")), "old")
  })
})

test_that("restoration failure preserves the exact backup for recovery", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old", file.path(target, "dataset.crb"))
    handle <- builder_prepare_release(
      target,
      "build-restore-failure",
      builder_release_identity(target)
    )
    writeLines("new", file.path(handle$stage, "dataset.crb"))
    moves <- 0L
    fail_publish_and_restore <- function(from, to) {
      moves <<- moves + 1L
      if (moves %in% c(2L, 3L)) {
        return(FALSE)
      }
      file.rename(from, to)
    }

    failure <- tryCatch(
      builder_publish_release(handle, .move = fail_publish_and_restore),
      error = function(error) error
    )
    expect_s3_class(failure, "error")
    expect_true(grepl(handle$backup, conditionMessage(failure), fixed = TRUE))
    recovery <- builder_discover_recovery(target)
    expect_identical(recovery$state, "recovery_required")
    expect_identical(recovery$backup, handle$backup)
    expect_true(dir.exists(handle$backup))
    expect_identical(readLines(file.path(handle$backup, "dataset.crb")), "old")
  })
})

test_that("a dead completed owner cannot strand the release lock", {
  skip_if_not_installed("callr")
  local({
    root_path <- testthat::test_path("..", "..", "inst", "builder")
    if (!dir.exists(root_path)) {
      root_path <- system.file("builder", package = "CerebroNexus")
    }
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    crash <- callr::r_bg(
      function(builder_root, target) {
        source(file.path(builder_root, "core", "bundle_path_contract.R"))
        source(file.path(builder_root, "publish.R"))
        handle <- builder_prepare_release(target, "build-complete-crash")
        writeLines("new", file.path(handle$stage, "dataset.crb"))
        builder_publish_release(
          handle,
          .after_phase = function(phase) {
            if (identical(phase, "complete")) {
              quit(save = "no", status = 19L, runLast = FALSE)
            }
          }
        )
      },
      list(root_path, target)
    )
    crash$wait(timeout = 10000)
    expect_identical(crash$get_exit_status(), 19L)

    builder_publish_source()
    expect_true(dir.exists(file.path(
      builder_release_control_path(target),
      "lock"
    )))
    next_handle <- builder_prepare_release(
      target,
      "build-after-complete",
      builder_release_identity(target)
    )
    expect_s3_class(next_handle, "builder_release_handle")
    expect_true(builder_abort_release(next_handle)$aborted)
  })
})

test_that("a crash after the stage rename but before journaling is recoverable", {
  skip_if_not_installed("callr")
  local({
    root_path <- testthat::test_path("..", "..", "inst", "builder")
    if (!dir.exists(root_path)) {
      root_path <- system.file("builder", package = "CerebroNexus")
    }
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old", file.path(target, "dataset.crb"))
    crash <- callr::r_bg(
      function(builder_root, target) {
        source(file.path(builder_root, "core", "bundle_path_contract.R"))
        source(file.path(builder_root, "publish.R"))
        handle <- builder_prepare_release(
          target,
          "build-rename-crash",
          builder_release_identity(target)
        )
        writeLines("new", file.path(handle$stage, "dataset.crb"))
        builder_publish_release(
          handle,
          .after_move = function(move) {
            if (identical(move, "new_to_target")) {
              quit(save = "no", status = 20L, runLast = FALSE)
            }
          }
        )
      },
      list(root_path, target)
    )
    crash$wait(timeout = 10000)
    expect_identical(crash$get_exit_status(), 20L)

    builder_publish_source()
    recovery <- builder_discover_recovery(target)
    expect_identical(recovery$phase, "old_moved")
    expect_identical(readLines(file.path(target, "dataset.crb")), "new")
    expect_true(builder_recover_release(target, "restore")$recovered)
    expect_identical(readLines(file.path(target, "dataset.crb")), "old")
  })
})

test_that("foreign or unverifiable locks remain fail closed", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    control <- builder_release_control_path(target)
    dir.create(control, mode = "0700")
    lock <- file.path(control, "lock")
    dir.create(lock, mode = "0700")
    owner <- list(
      schema_version = 1L,
      token = "foreign-token",
      host = "another-host",
      pid = 999999L,
      acquired_at = "unknown"
    )
    saveRDS(owner, file.path(lock, "owner.rds"))
    journal <- list(
      schema_version = 1L,
      target = .builder_release_path(target),
      control = control,
      stage = file.path(control, "stages", "foreign"),
      backup = file.path(control, "backup"),
      journal = file.path(control, "journal.rds"),
      lock = lock,
      build_id = "foreign-build",
      token = "foreign-token",
      phase = "old_moved"
    )
    saveRDS(journal, file.path(control, "journal.rds"))

    expect_error(
      builder_recover_release(target, "restore"),
      "manual inspection"
    )
    expect_true(dir.exists(lock))
    expect_identical(readRDS(file.path(lock, "owner.rds")), owner)
  })
})

test_that("foreign control occupants are preserved and fail closed", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    control <- builder_release_control_path(target)
    dir.create(control, mode = "0700")
    foreign <- file.path(control, "do-not-delete.txt")
    writeLines("foreign", foreign)

    expect_error(
      builder_prepare_release(target, "build-foreign"),
      "unknown control"
    )
    expect_identical(readLines(foreign), "foreign")
  })
})
