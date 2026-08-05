versionless_inst_dir <- function() {
  installed <- system.file(package = "CerebroNexus")
  if (nzchar(installed) && file.exists(file.path(installed, "app.R"))) {
    return(installed)
  }
  testthat::test_path("..", "..", "inst")
}

test_that("runtime and example directories do not encode a product version", {
  inst_dir <- versionless_inst_dir()
  legacy_viewer_version <- paste0("v1", ".4")

  expect_true(dir.exists(file.path(inst_dir, "viewer")))
  expect_true(dir.exists(file.path(inst_dir, "extdata", "examples")))
  expect_false(dir.exists(file.path(inst_dir, "shiny", legacy_viewer_version)))
  expect_false(dir.exists(file.path(
    inst_dir,
    "extdata",
    legacy_viewer_version
  )))

  relative_directories <- list.dirs(
    inst_dir,
    recursive = TRUE,
    full.names = FALSE
  )
  path_segments <- unlist(strsplit(relative_directories, "/", fixed = TRUE))

  expect_false(any(grepl("^v[0-9]+(?:\\.[0-9]+)+$", path_segments)))
})

test_that("the package exposes one versionless launcher", {
  namespace_file <- testthat::test_path("..", "..", "NAMESPACE")
  namespace <- readLines(namespace_file, warn = FALSE)
  r_files <- basename(list.files(testthat::test_path("..", "..", "R")))

  expect_true("export(launchCerebro)" %in% namespace)
  expect_false(any(grepl("^export\\(launchCerebroV[0-9]", namespace)))
  expect_false(any(grepl("^launchCerebroV[0-9].*[.]R$", r_files)))
})

test_that("current code and documentation do not describe a legacy viewer version", {
  root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  roots <- file.path(root, c("R", "man", "vignettes", "tests"))
  files <- unlist(lapply(
    roots,
    list.files,
    pattern = "[.](R|Rd|Rmd)$",
    recursive = TRUE,
    full.names = TRUE
  ))
  legacy_viewer_version <- paste0("v1", ".4")
  occurrences <- vapply(
    files,
    function(path) {
      any(grepl(
        legacy_viewer_version,
        tolower(readLines(path, warn = FALSE)),
        fixed = TRUE
      ))
    },
    logical(1)
  )

  expect_false(
    any(occurrences),
    info = paste(files[occurrences], collapse = "\n")
  )
})
