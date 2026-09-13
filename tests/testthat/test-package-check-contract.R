source_file <- function(...) {
  testthat::test_path("..", "..", ...)
}

skip_if_not_source_tree <- function() {
  skip_if_not(
    file.exists(source_file(".Rbuildignore")),
    "static source-tree contract"
  )
}

test_that("development-only directories are excluded from package builds", {
  skip_if_not_source_tree()
  ignores <- readLines(source_file(".Rbuildignore"), warn = FALSE)
  expected <- c(
    "^\\.claude$",
    "^\\.loci$",
    "^\\.playwright-mcp$",
    "^\\.sisyphus$",
    "^\\.superpowers$"
  )

  expect_true(all(expected %in% ignores))
})

test_that("large development demos stay out of CRAN package builds", {
  skip_if_not_source_tree()
  ignores <- readLines(source_file(".Rbuildignore"), warn = FALSE)
  expected <- c(
    "^inst/extdata/examples/demo_.*[.]crb$",
    "^inst/extdata/examples/demo_spatial_visium_he[.]png$",
    "^inst/extdata/examples/spatial/xenium$",
    "^vignettes$"
  )

  expect_true(all(expected %in% ignores))
  expect_true(file.exists(source_file(
    "inst",
    "extdata",
    "examples",
    "example.crb"
  )))
})

test_that("DESCRIPTION is ready for CRAN incoming checks", {
  skip_if_not_source_tree()
  description <- read.dcf(source_file("DESCRIPTION"))[1L, ]

  expect_false(any(
    c("Author", "Maintainer", "Remotes", "LazyData") %in%
      colnames(description)
  ))
  expect_identical(
    unname(description[["Title"]]),
    "Interactive Visualization of Single-Cell Transcriptomics"
  )
  expect_false(startsWith(description[["Description"]], "CerebroNexus"))
  expect_match(
    description[["URL"]],
    "https://www.mheming.com/CerebroNexus/",
    fixed = TRUE
  )
  expect_match(
    description[["Additional_repositories"]],
    "https://bnprks.r-universe.dev",
    fixed = TRUE
  )
  expect_false("VignetteBuilder" %in% colnames(description))
})

test_that("optional analysis dependencies are suggested", {
  skip_if_not_source_tree()
  description <- read.dcf(source_file("DESCRIPTION"))[1L, ]
  imports <- trimws(strsplit(description[["Imports"]], ",", fixed = TRUE)[[1L]])
  suggests <- trimws(strsplit(description[["Suggests"]], ",", fixed = TRUE)[[
    1L
  ]])
  dependency_name <- function(value) sub("[[:space:]]*\\(.*$", "", value)

  expect_false(any(
    c(
      "biomaRt",
      "future.apply",
      "GSVA",
      "httr",
      "pbapply",
      "qvalue",
      "scRepertoire"
    ) %in%
      dependency_name(imports)
  ))
  expect_true(all(
    c(
      "biomaRt",
      "future.apply",
      "GSVA",
      "httr",
      "pbapply",
      "qvalue",
      "scRepertoire"
    ) %in%
      dependency_name(suggests)
  ))
  expect_true(all(c("base64enc", "jsonlite") %in% dependency_name(imports)))
})

test_that("CRAN checks are not forced into local test mode", {
  setup <- readLines(testthat::test_path("setup.R"), warn = FALSE)
  expect_false(any(grepl("Sys.setenv(NOT_CRAN", setup, fixed = TRUE)))

  browser_files <- list.files(
    testthat::test_path(),
    pattern = "^test-app-.*[.]R$",
    full.names = TRUE
  )
  browser_files <- browser_files[vapply(
    browser_files,
    function(path) {
      any(grepl(
        "library(shinytest2)",
        readLines(path, warn = FALSE),
        fixed = TRUE
      ))
    },
    logical(1)
  )]
  has_skip <- vapply(
    browser_files,
    function(path) {
      any(grepl("skip_on_cran()", readLines(path, warn = FALSE), fixed = TRUE))
    },
    logical(1)
  )

  expect_true(
    all(has_skip),
    info = paste(basename(browser_files[!has_skip]), collapse = ", ")
  )
})

test_that("citation and slow example use current CRAN forms", {
  skip_if_not_source_tree()
  citation <- paste(readLines(source_file("inst", "CITATION")), collapse = "\n")
  markers <- paste(
    readLines(source_file("R", "getMarkerGenes.R")),
    collapse = "\n"
  )

  expect_match(citation, "bibentry(", fixed = TRUE)
  expect_false(grepl("citEntry(", citation, fixed = TRUE))
  expect_false(grepl("personList(", citation, fixed = TRUE))
  expect_match(markers, "#' \\donttest{", fixed = TRUE)
})

test_that("jsonlite remains a declared runtime import", {
  skip_if_not_source_tree()
  namespace <- readLines(source_file("NAMESPACE"), warn = FALSE)

  expect_true("importFrom(jsonlite,fromJSON)" %in% namespace)
  expect_true("importFrom(jsonlite,toJSON)" %in% namespace)
})

test_that("retired share databases stay out of Git and package builds", {
  skip_if_not_source_tree()
  gitignore <- readLines(source_file(".gitignore"), warn = FALSE)
  buildignore <- readLines(source_file(".Rbuildignore"), warn = FALSE)

  expect_true("inst/private-data/linked-view-shares.sqlite" %in% gitignore)
  expect_true(
    "^inst/private-data/linked-view-shares\\.sqlite$" %in% buildignore
  )
})

test_that("pkgdown output directory has no tracked source files", {
  skip_if_not_source_tree()
  tracked_docs <- system2(
    "git",
    c("-C", shQuote(source_file()), "ls-files", "--", "docs"),
    stdout = TRUE
  )

  expect_length(tracked_docs, 0L)
})

test_that("package and exported-app branding use the current identity", {
  description_path <- source_file("DESCRIPTION")
  if (!file.exists(description_path)) {
    description_path <- system.file("DESCRIPTION", package = "CerebroNexus")
  }
  description <- read.dcf(description_path)

  expect_identical(unname(description[1, "Package"]), "CerebroNexus")
  expect_match(description[1, "URL"], "mihem/CerebroNexus", fixed = TRUE)
  expect_identical(
    formals(createShinyApp)$welcome_message,
    "Welcome to CerebroNexus!"
  )
})

test_that("self-contained app vignette never purls interactive runApp calls", {
  skip_if_not_source_tree()
  lines <- readLines(
    source_file("vignettes", "create_a_self_contained_shiny_app.Rmd"),
    warn = FALSE
  )
  run_lines <- which(grepl("shiny::runApp(out_dir)", lines, fixed = TRUE))
  expect_length(run_lines, 2L)

  chunk_headers <- vapply(
    run_lines,
    function(line_number) {
      prior <- lines[seq_len(line_number)]
      prior[max(which(grepl("^```\\{r", prior)))]
    },
    character(1)
  )
  expect_true(all(grepl("purl=FALSE", chunk_headers, fixed = TRUE)))
})

test_that("summarisation has no unused unqualified ave call", {
  skip_if_not_source_tree()
  seurat_source <- paste(
    readLines(source_file("R", "seurat_utils.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_false(grepl("idx_first <- ave(", seurat_source, fixed = TRUE))
})

test_that("later remains declared because bundled runtime code uses it", {
  skip_if_not_source_tree()
  description <- read.dcf(source_file("DESCRIPTION"), fields = "Imports")[[1]]
  namespace <- readLines(source_file("NAMESPACE"), warn = FALSE)
  runtime_source <- paste(
    readLines(source_file("inst", "viewer", "utility_functions.R")),
    collapse = "\n"
  )

  expect_match(description, "later")
  expect_true("importFrom(later,later)" %in% namespace)
  expect_match(runtime_source, "later::later(", fixed = TRUE)
})
