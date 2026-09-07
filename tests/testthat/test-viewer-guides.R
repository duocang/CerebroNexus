repo_file <- function(...) {
  candidates <- c(
    file.path(getwd(), ...),
    file.path(getwd(), "..", "..", ...),
    testthat::test_path("..", "..", ...)
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing)) existing[[1L]] else candidates[[1L]]
}

helpers_file <- repo_file("inst", "viewer", "guides", "helpers.R")

test_that("Viewer guide helpers are bundled", {
  expect_true(file.exists(helpers_file))
})

if (file.exists(helpers_file)) {
  guide_env <- new.env(parent = globalenv())
  source(helpers_file, local = guide_env)

  test_that("guide catalogue is ordered and backed by vignettes", {
    catalogue <- guide_env$viewerGuideCatalogue()

    expect_identical(names(catalogue), c("section", "title", "slug"))
    expect_identical(anyDuplicated(catalogue$slug), 0L)
    expect_identical(
      unique(catalogue$section),
      c("Getting started", "Analysis", "Reference")
    )
    expect_true(all(file.exists(repo_file(
      "vignettes",
      paste0(catalogue$slug, ".Rmd")
    ))))
  })

  test_that("guide links prefer bundled HTML and fall back online", {
    root <- withr::local_tempdir()
    dir.create(
      file.path(root, "viewer", "www", "guides"),
      recursive = TRUE
    )
    writeLines(
      "<html></html>",
      file.path(root, "viewer", "www", "guides", "workflow.html")
    )

    expect_identical(
      guide_env$viewerGuideHref("workflow", root, "cerebro_www_2"),
      "cerebro_www_2/guides/workflow.html"
    )
    expect_identical(
      guide_env$viewerGuideHref("missing", root, "cerebro_www_2"),
      paste0(
        "https://mihem.github.io/CerebroNexus/articles/",
        "missing.html"
      )
    )
    expect_identical(
      guide_env$viewerGuideHref("workflow", root, NULL),
      paste0(
        "https://mihem.github.io/CerebroNexus/articles/",
        "workflow.html"
      )
    )
  })
}

test_that("Viewer navigation registers the Guides tab", {
  ui <- paste(readLines(repo_file(
    "inst",
    "viewer",
    "shiny_UI.R"
  ), warn = FALSE), collapse = "\n")
  about <- paste(readLines(repo_file(
    "inst",
    "viewer",
    "about",
    "server.R"
  ), warn = FALSE), collapse = "\n")
  runtime <- paste(readLines(repo_file(
    "inst",
    "viewer",
    "shiny_server.R"
  ), warn = FALSE), collapse = "\n")
  builder <- paste(readLines(repo_file(
    "R",
    "createShinyApp.R"
  ), warn = FALSE), collapse = "\n")

  expect_match(ui, "/viewer/guides/UI.R", fixed = TRUE)
  expect_match(ui, 'menuItem("Guides", tabName = "guides"', fixed = TRUE)
  expect_match(ui, "tab_guides", fixed = TRUE)
  expect_match(about, "#shiny-tab-guides", fixed = TRUE)
  expect_match(runtime, 'guides = "guides"', fixed = TRUE)
  expect_match(builder, 'guides = "guides"', fixed = TRUE)
})
