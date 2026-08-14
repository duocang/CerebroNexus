builder_publish_source <- function(local = parent.frame()) {
  root <- testthat::test_path("..", "..", "inst", "builder")
  if (!dir.exists(root)) {
    root <- system.file("builder", package = "CerebroNexus")
  }
  source(file.path(root, "core", "bundle_path_contract.R"), local = local)
  source(file.path(root, "publish.R"), local = local)
}

write_builder_release_record_fixture <- function(root, members) {
  lines <- c(
    "CEREBRO_BUILDER_RELEASE_V1",
    vapply(
      members,
      function(member) {
        paste(member$type, member$path, sep = "\t")
      },
      ""
    )
  )
  writeLines(
    lines,
    file.path(root, ".cerebro-builder-release-v1"),
    useBytes = TRUE
  )
}

test_that("release existence includes dangling lexical entries", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "dangling")
    outside <- file.path(root, "missing")
    linked <- tryCatch(
      file.symlink(outside, target),
      error = function(error) FALSE
    )
    skip_if_not(isTRUE(linked), "Symbolic links are unavailable")

    expect_true(.builder_release_exists(target))
    expect_identical(Sys.readlink(target), outside)
    expect_false(file.exists(outside))
  })
})

test_that("release ownership records require canonical recursive members", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(file.path(release, "cerebro_app", "www"), recursive = TRUE)
    writeLines("crb", file.path(release, "01-a.crb"))
    writeLines("app", file.path(release, "cerebro_app", "app.R"))
    writeLines("asset", file.path(release, "cerebro_app", "www", "asset.txt"))
    members <- list(
      list(type = "D", path = "cerebro_app"),
      list(type = "D", path = "cerebro_app/www"),
      list(type = "F", path = "01-a.crb"),
      list(type = "F", path = "cerebro_app/app.R"),
      list(type = "F", path = "cerebro_app/www/asset.txt")
    )
    write_builder_release_record_fixture(release, members)

    record <- .builder_release_read_record(release)

    expect_identical(record$members, members)
    expect_type(record$bytes, "raw")
    expect_identical(record$identity, builder_release_identity(release))
  })
})

test_that("ownership records round-trip sorted UTF-8 paths", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    paths <- enc2utf8(c("a.crb", "é.crb", "中文.crb"))
    for (path in paths) {
      writeLines(path, file.path(release, path), useBytes = TRUE)
    }
    identity <- builder_release_identity(release)

    record <- .builder_release_write_record(
      release,
      identity,
      token = "utf8-round-trip"
    )

    expected_lines <- sort(enc2utf8(paste0("F\t", paths)), method = "radix")
    expected_paths <- sub("^F\t", "", expected_lines)
    record_paths <- vapply(record$members, `[[`, character(1), "path")
    expect_identical(record_paths, expected_paths)
    expect_true(all(validUTF8(record_paths)))
    non_ascii <- grepl("[^ -~]", record_paths)
    expect_true(all(Encoding(record_paths[non_ascii]) == "UTF-8"))
    expect_identical(
      vapply(.builder_release_read_record(release)$members, `[[`, "", "path"),
      expected_paths
    )
  })
})

test_that("malformed ownership records fail closed", {
  local({
    builder_publish_source()
    cases <- list(
      header = c("CEREBRO_BUILDER_RELEASE_V2", "F\tdataset.crb"),
      blank = c("CEREBRO_BUILDER_RELEASE_V1", ""),
      tag = c("CEREBRO_BUILDER_RELEASE_V1", "X\tdataset.crb"),
      delimiter = c("CEREBRO_BUILDER_RELEASE_V1", "F dataset.crb"),
      dot = c("CEREBRO_BUILDER_RELEASE_V1", "F\t./dataset.crb"),
      parent = c("CEREBRO_BUILDER_RELEASE_V1", "F\ta/../dataset.crb"),
      duplicate = c(
        "CEREBRO_BUILDER_RELEASE_V1",
        "F\tdataset.crb",
        "F\tdataset.crb"
      ),
      absolute = c("CEREBRO_BUILDER_RELEASE_V1", "F\t/tmp/dataset.crb"),
      device = c("CEREBRO_BUILDER_RELEASE_V1", "F\tCON"),
      control = c("CEREBRO_BUILDER_RELEASE_V1", "F\tbad\001name")
    )
    for (case in cases) {
      root <- withr::local_tempdir()
      release <- file.path(root, "release")
      dir.create(release)
      writeLines("crb", file.path(release, "dataset.crb"))
      writeLines(
        case,
        file.path(release, ".cerebro-builder-release-v1"),
        useBytes = TRUE
      )
      expect_error(
        .builder_release_read_record(release),
        "ownership record",
        info = paste(case, collapse = " | ")
      )
    }
  })
})

test_that("ownership records reject extra blank lines", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    writeBin(
      charToRaw("CEREBRO_BUILDER_RELEASE_V1\n\n"),
      file.path(release, ".cerebro-builder-release-v1")
    )

    expect_error(
      .builder_release_read_record(release),
      "blank member"
    )
  })
})

test_that("ownership records reject trailing member delimiters", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    writeLines("crb", file.path(release, "dataset.crb"))
    writeBin(
      charToRaw(
        "CEREBRO_BUILDER_RELEASE_V1\nF\tdataset.crb\t\n"
      ),
      file.path(release, ".cerebro-builder-release-v1")
    )

    expect_error(
      .builder_release_read_record(release),
      "malformed member"
    )
  })
})

test_that("ownership records reject identity mismatches and links", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()

    missing <- file.path(root, "missing")
    dir.create(missing)
    write_builder_release_record_fixture(
      missing,
      list(list(type = "F", path = "missing.crb"))
    )
    expect_error(.builder_release_read_record(missing), "ownership record")

    wrong_type <- file.path(root, "wrong-type")
    dir.create(file.path(wrong_type, "dataset.crb"), recursive = TRUE)
    write_builder_release_record_fixture(
      wrong_type,
      list(list(type = "F", path = "dataset.crb"))
    )
    expect_error(.builder_release_read_record(wrong_type), "ownership record")

    hidden <- file.path(root, "hidden")
    dir.create(hidden)
    writeLines("crb", file.path(hidden, "dataset.crb"))
    writeLines("foreign", file.path(hidden, ".unknown"))
    write_builder_release_record_fixture(
      hidden,
      list(list(type = "F", path = "dataset.crb"))
    )
    expect_error(.builder_release_read_record(hidden), "ownership record")

    linked <- file.path(root, "linked")
    dir.create(linked)
    writeLines("crb", file.path(linked, "dataset.crb"))
    link_ok <- tryCatch(
      file.symlink("dataset.crb", file.path(linked, "alias.crb")),
      error = function(error) FALSE
    )
    if (isTRUE(link_ok)) {
      write_builder_release_record_fixture(
        linked,
        list(
          list(type = "F", path = "alias.crb"),
          list(type = "F", path = "dataset.crb")
        )
      )
      expect_error(.builder_release_read_record(linked), "symbolic link")
    }
  })
})

test_that("ownership record replacement is atomic", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    writeLines("crb", file.path(release, "dataset.crb"))
    identity <- builder_release_identity(release)

    expect_error(
      .builder_release_write_record(
        release,
        identity,
        token = "atomic-test",
        .move = function(from, to) FALSE
      ),
      "atomically"
    )
    expect_false(file.exists(file.path(
      release,
      ".cerebro-builder-release-v1"
    )))
    expect_identical(readLines(file.path(release, "dataset.crb")), "crb")
    expect_length(list.files(release, all.files = TRUE, no.. = TRUE), 1L)
  })
})

test_that("record temp creation never follows a dangling symlink", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    writeLines("crb", file.path(release, "dataset.crb"))
    identity <- builder_release_identity(release)
    token <- "dangling-test"
    temporary <- file.path(
      release,
      paste0("..cerebro-builder-release-v1.", token, ".tmp")
    )
    outside <- file.path(root, "outside.txt")
    link_ok <- tryCatch(
      file.symlink(outside, temporary),
      error = function(error) FALSE
    )
    skip_if_not(isTRUE(link_ok))

    expect_error(
      .builder_release_write_record(release, identity, token),
      "temporary"
    )
    expect_false(file.exists(outside))
    expect_true(.builder_release_link(temporary))
  })
})

test_that("record temp replacement cannot write through an outside link", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    writeLines("crb", file.path(release, "dataset.crb"))
    identity <- builder_release_identity(release)
    outside <- file.path(root, "outside.txt")
    writeLines("keep me", outside)
    replaced <- FALSE

    expect_error(
      .builder_release_write_record(
        release,
        identity,
        token = "replacement-test",
        .after_create = function(temporary) {
          unlink(temporary, force = TRUE)
          replaced <<- isTRUE(file.symlink(outside, temporary))
        }
      ),
      "temporary.*unsafe"
    )
    expect_true(replaced)
    expect_identical(readLines(outside), "keep me")
  })
})

test_that("record publication rejects a hard-link swap during rename", {
  skip_if(identical(.Platform$OS.type, "windows"))
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    writeLines("crb", file.path(release, "dataset.crb"))
    identity <- builder_release_identity(release)
    outside <- file.path(root, "outside-record")

    expect_error(
      .builder_release_write_record(
        release,
        identity,
        token = "hard-link-swap",
        .move = function(from, to) {
          file.copy(from, outside)
          unlink(from, force = TRUE)
          linked <- file.link(outside, from)
          isTRUE(linked) && file.rename(from, to)
        }
      ),
      "changed during publication"
    )
    expect_false(file.exists(file.path(
      release,
      ".cerebro-builder-release-v1"
    )))
    expect_true(file.exists(outside))
  })
})

test_that("record publication rejects a hard-link swap during final parsing", {
  skip_if(identical(.Platform$OS.type, "windows"))
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    writeLines("crb", file.path(release, "dataset.crb"))
    identity <- builder_release_identity(release)
    outside <- file.path(root, "outside-record")
    original_identity <- builder_release_identity
    swapped <- FALSE
    builder_release_identity <- function(target, ...) {
      record <- file.path(target, ".cerebro-builder-release-v1")
      if (!swapped && file.exists(record)) {
        file.copy(record, outside)
        unlink(record, force = TRUE)
        swapped <<- isTRUE(file.link(outside, record))
      }
      original_identity(target, ...)
    }

    expect_error(
      .builder_release_write_record(
        release,
        identity,
        token = "hard-link-final-parse"
      ),
      "changed during publication"
    )
    expect_true(swapped)
    expect_false(file.exists(file.path(
      release,
      ".cerebro-builder-release-v1"
    )))
    expect_true(file.exists(outside))
  })
})

test_that("failed final record parsing removes the unpublished record", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    writeLines("crb", file.path(release, "dataset.crb"))
    identity <- builder_release_identity(release)
    original_identity <- builder_release_identity
    corrupted <- FALSE
    builder_release_identity <- function(target, ...) {
      record <- file.path(target, ".cerebro-builder-release-v1")
      if (!corrupted && file.exists(record)) {
        writeLines("bad", record)
        corrupted <<- TRUE
      }
      original_identity(target, ...)
    }

    expect_error(
      .builder_release_write_record(
        release,
        identity,
        token = "failed-final-parse"
      ),
      "ownership record|changed during publication"
    )
    expect_true(corrupted)
    expect_false(file.exists(file.path(
      release,
      ".cerebro-builder-release-v1"
    )))
  })
})

test_that("ownership records remain owner-only under a permissive umask", {
  skip_if(identical(.Platform$OS.type, "windows"))
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    writeLines("crb", file.path(release, "dataset.crb"))
    identity <- builder_release_identity(release)
    old_umask <- Sys.umask("0000")
    withr::defer(Sys.umask(old_umask))

    .builder_release_write_record(
      release,
      identity,
      token = "owner-only"
    )

    expect_identical(
      as.octmode(
        file.info(file.path(
          release,
          ".cerebro-builder-release-v1"
        ))$mode
      ),
      as.octmode("600")
    )
  })
})

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

test_that("release identity rejects a file replaced by a directory", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    slot <- file.path(target, "slot")
    hidden <- file.path(slot, "hidden.txt")
    dir.create(target)
    writeLines("ordinary file", slot)
    calls <- 0L
    swap_after_first_list <- function(path) {
      calls <<- calls + 1L
      if (identical(calls, 2L)) {
        unlink(slot)
        dir.create(slot)
        writeLines("hidden", hidden)
      }
      .builder_release_list_directory(path)
    }

    attempt <- tryCatch(
      builder_release_identity(
        target,
        .list_directory = swap_after_first_list
      ),
      error = function(error) error
    )
    if (!inherits(attempt, "error")) {
      paths <- vapply(attempt$entries, `[[`, character(1), "path")
      expect_identical(paths, "slot")
      expect_false("slot/hidden.txt" %in% paths)
    }
    expect_s3_class(attempt, "error")
    if (inherits(attempt, "error")) {
      expect_match(conditionMessage(attempt), "changed after enumeration")
    }
    expect_true(file.exists(hidden))
  })
})

test_that("release identity rejects a same-name directory replacement", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    slot <- file.path(target, "slot")
    hidden <- file.path(slot, "hidden.txt")
    replacement <- file.path(root, "replacement")
    replacement_hidden <- file.path(replacement, "hidden.txt")
    dir.create(slot, recursive = TRUE)
    writeLines("old", hidden)
    original <- fs::file_info(slot, follow = FALSE)
    dir.create(replacement)
    writeLines("new", replacement_hidden)
    replacement_info <- fs::file_info(replacement, follow = FALSE)
    expect_false(identical(
      as.double(original$inode),
      as.double(replacement_info$inode)
    ))
    calls <- 0L
    replace_before_confirmation <- function(path) {
      calls <<- calls + 1L
      if (identical(calls, 3L)) {
        unlink(slot, recursive = TRUE)
        if (!file.rename(replacement, slot)) {
          stop("The replacement fixture could not be moved.")
        }
      }
      .builder_release_list_directory(path)
    }

    attempt <- tryCatch(
      builder_release_identity(
        target,
        .list_directory = replace_before_confirmation
      ),
      error = function(error) error
    )
    if (!inherits(attempt, "error")) {
      paths <- vapply(attempt$entries, `[[`, character(1), "path")
      expect_identical(paths, c("slot", "slot/hidden.txt"))
    }
    expect_s3_class(attempt, "error")
    if (inherits(attempt, "error")) {
      expect_match(conditionMessage(attempt), "changed after enumeration")
    }
    expect_identical(readLines(hidden), "new")
  })
})

test_that("release identity rejects special filesystem nodes without blocking", {
  skip_if_not_installed("callr")
  skip_if(Sys.which("mkfifo") == "")
  local({
    builder_root <- testthat::test_path("..", "..", "inst", "builder")
    if (!dir.exists(builder_root)) {
      builder_root <- system.file("builder", package = "CerebroNexus")
    }
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    expect_identical(
      system2("mkfifo", file.path(target, ".cerebro-builder-release-v1")),
      0L
    )
    process <- callr::r_bg(
      function(builder_root, target) {
        source(file.path(builder_root, "core", "bundle_path_contract.R"))
        source(file.path(builder_root, "publish.R"))
        tryCatch(
          builder_release_state(target),
          error = function(error) conditionMessage(error)
        )
      },
      list(builder_root, target)
    )
    on.exit(if (process$is_alive()) process$kill(), add = TRUE)
    process$wait(timeout = 1500)
    finished <- !process$is_alive()
    if (!finished) {
      process$kill()
    }

    expect_true(finished, info = "release identity blocked on a FIFO")
    if (finished) {
      expect_match(process$get_result(), "unsupported filesystem entry")
    }
  })
})

test_that("release identity rejects an unreadable release root", {
  skip_if(identical(.Platform$OS.type, "windows"))
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    writeLines("foreign", file.path(release, "foreign.txt"))
    Sys.chmod(release, mode = "0000")
    withr::defer(Sys.chmod(release, mode = "0700"))
    skip_if(file.access(release, mode = 5L) == 0L)

    expect_error(
      builder_release_identity(release),
      "enumerate"
    )
  })
})

test_that("release identity rejects unreadable nested directories", {
  skip_if(identical(.Platform$OS.type, "windows"))
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    secret <- file.path(release, "secret")
    dir.create(secret, recursive = TRUE)
    writeLines("foreign", file.path(secret, "foreign.txt"))
    Sys.chmod(secret, mode = "0000")
    withr::defer(Sys.chmod(secret, mode = "0700"))
    skip_if(file.access(secret, mode = 5L) == 0L)

    expect_error(
      builder_release_identity(release),
      "enumerate"
    )
  })
})

test_that("release identity rejects an invalid payload hash", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    writeLines("payload", file.path(release, "dataset.crb"))

    expect_error(
      builder_release_identity(
        release,
        .hash_file = function(path) NA_character_
      ),
      "read safely"
    )
  })
})

test_that("release identity rejects payload changes during hashing", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    payload <- file.path(release, "dataset.crb")
    dir.create(release)
    writeLines("before", payload)
    mutate_while_hashing <- function(path) {
      md5 <- .builder_release_payload_md5(path)
      writeLines("changed after hashing", path)
      md5
    }

    expect_error(
      builder_release_identity(release, .hash_file = mutate_while_hashing),
      "changed while its identity was read"
    )
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

test_that("payload verification requires exact TRUE and restores the prior release", {
  local({
    builder_publish_source()
    for (result in list(FALSE, NULL, structure("boom", class = "error"))) {
      root <- withr::local_tempdir()
      target <- file.path(root, "release")
      dir.create(target)
      writeLines("old", file.path(target, "dataset.crb"))
      handle <- builder_prepare_release(
        target,
        "guard",
        builder_release_identity(target)
      )
      writeLines("new", file.path(handle$stage, "dataset.crb"))
      callback <- if (inherits(result, "error")) {
        function(...) stop("private callback detail")
      } else {
        force(result)
        function(...) result
      }
      expect_error(
        builder_publish_release(handle, .verify_payload = callback),
        "verification failed"
      )
      expect_identical(readLines(file.path(target, "dataset.crb")), "old")
      expect_true(dir.exists(handle$stage))
    }
  })
})

test_that("post-rename payload verification restores the old release", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old", file.path(target, "dataset.crb"))
    handle <- builder_prepare_release(
      target,
      "post-rename-guard",
      builder_release_identity(target)
    )
    writeLines("new", file.path(handle$stage, "dataset.crb"))
    expect_error(
      builder_publish_release(
        handle,
        .verify_payload = function(root, phase) {
          if (identical(phase, "after_rename")) FALSE else TRUE
        }
      ),
      "verification failed"
    )
    expect_identical(readLines(file.path(target, "dataset.crb")), "old")
    expect_true(dir.exists(handle$stage))
  })
})
