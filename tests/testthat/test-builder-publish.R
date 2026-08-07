builder_publish_source <- function(local = parent.frame()) {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "publish.R"
  )
  if (!file.exists(path)) {
    path <- system.file(
      file.path("builder", "publish.R"),
      package = "CerebroNexus"
    )
  }
  source(path, local = local)
}

test_that("publishing refuses existing targets without acknowledgement", {
  local({
    builder_publish_source()

    root <- withr::local_tempdir()
    stage <- file.path(root, "stage")
    final <- file.path(root, "final")
    dir.create(stage)
    writeLines("new", file.path(stage, "dataset.crb"))
    writeLines("old", final)

    result <- builder_publish_batch(
      staged = file.path(stage, "dataset.crb"),
      targets = final,
      overwrite = FALSE
    )

    expect_false(is.null(result$error))
    expect_identical(readLines(final), "old")
    expect_true(file.exists(file.path(stage, "dataset.crb")))
  })
})

test_that("publishing replaces a batch only after every staged item exists", {
  local({
    builder_publish_source()

    root <- withr::local_tempdir()
    stage <- file.path(root, "stage")
    dir.create(stage)
    first <- file.path(stage, "one.crb")
    second <- file.path(stage, "two.crb")
    writeLines("one", first)

    targets <- file.path(root, c("one.crb", "two.crb"))
    failed <- builder_publish_batch(
      staged = c(first, second),
      targets = targets,
      overwrite = TRUE
    )

    expect_false(is.null(failed$error))
    expect_false(any(file.exists(targets)))

    writeLines("two", second)
    done <- builder_publish_batch(
      staged = c(first, second),
      targets = targets,
      overwrite = TRUE
    )

    expect_null(done$error)
    expect_identical(readLines(targets[1]), "one")
    expect_identical(readLines(targets[2]), "two")
  })
})

test_that("a mid-publication failure restores every previous output", {
  local({
    builder_publish_source()

    root <- withr::local_tempdir()
    stage <- file.path(root, "stage")
    dir.create(stage)
    staged <- file.path(stage, c("one.crb", "two.crb"))
    targets <- file.path(root, c("one.crb", "two.crb"))
    writeLines("new one", staged[1])
    writeLines("new two", staged[2])
    writeLines("old one", targets[1])
    writeLines("old two", targets[2])

    calls <- 0L
    fail_fourth_move <- function(from, to) {
      calls <<- calls + 1L
      if (calls == 4L) {
        return(FALSE)
      }
      file.rename(from, to)
    }

    result <- builder_publish_batch(
      staged,
      targets,
      overwrite = TRUE,
      .move = fail_fourth_move
    )

    expect_false(is.null(result$error))
    expect_identical(readLines(targets[1]), "old one")
    expect_identical(readLines(targets[2]), "old two")
  })
})
