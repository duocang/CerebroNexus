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

  test_that("catalogued guide images are packaged", {
    catalogue <- guide_env$viewerGuideCatalogue()
    sources <- vapply(
      catalogue$slug,
      function(slug) repo_file("vignettes", paste0(slug, ".Rmd")),
      character(1)
    )
    image_refs <- unique(unlist(lapply(sources, function(source_file) {
      lines <- readLines(source_file, warn = FALSE)
      matches <- regmatches(lines, gregexpr("img/[A-Za-z0-9_.-]+", lines))
      unlist(matches, use.names = FALSE)
    })))

    expect_true(length(image_refs) > 0L)
    expect_true(all(file.exists(repo_file("vignettes", image_refs))))
  })

  test_that("catalogued guides are bundled with the Viewer", {
    catalogue <- guide_env$viewerGuideCatalogue()
    guide_files <- repo_file(
      "inst",
      "viewer",
      "www",
      "guides",
      paste0(catalogue$slug, ".html")
    )

    expect_true(all(file.exists(guide_files)))
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
  ui <- paste(
    readLines(
      repo_file(
        "inst",
        "viewer",
        "shiny_UI.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  about <- paste(
    readLines(
      repo_file(
        "inst",
        "viewer",
        "about",
        "server.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  runtime <- paste(
    readLines(
      repo_file(
        "inst",
        "viewer",
        "shiny_server.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  builder <- paste(
    readLines(
      repo_file(
        "R",
        "createShinyApp.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(ui, "/viewer/guides/UI.R", fixed = TRUE)
  expect_match(ui, 'menuItem("Guides", tabName = "guides"', fixed = TRUE)
  expect_match(ui, "tab_guides", fixed = TRUE)
  expect_match(about, "#shiny-tab-guides", fixed = TRUE)
  expect_match(runtime, 'guides = "guides"', fixed = TRUE)
  expect_match(builder, 'guides = "guides"', fixed = TRUE)
})

renderer_file <- repo_file("scripts", "render-viewer-guides.R")

test_that("Viewer guide renderer is available", {
  expect_true(file.exists(renderer_file))
})

if (file.exists(renderer_file)) {
  renderer_env <- new.env(parent = globalenv())
  source(renderer_file, local = renderer_env)

  test_that("guide rendering is offline and never evaluates vignette code", {
    skip_if_not_installed("rmarkdown")
    skip_if_not(rmarkdown::pandoc_available(), "pandoc is unavailable")
    root <- withr::local_tempdir()
    vignettes <- file.path(root, "vignettes")
    output <- file.path(root, "guides")
    dir.create(vignettes)
    dir.create(file.path(vignettes, "img"))
    dir.create(output)
    writeLines("stale", file.path(output, "stale.html"))
    writeLines(
      '<svg xmlns="http://www.w3.org/2000/svg"></svg>',
      file.path(vignettes, "img", "example.svg")
    )
    writeLines(
      c(
        "---",
        'title: "Safe rendering"',
        "---",
        "",
        "```{r}",
        'stop("must not run")',
        "```",
        "",
        "![Local image](img/example.svg)"
      ),
      file.path(vignettes, "safe.Rmd")
    )

    rendered <- renderer_env$render_viewer_guides(
      vignette_dir = vignettes,
      output_dir = output,
      slugs = "safe",
      quiet = TRUE
    )

    expect_identical(rendered, "safe")
    expect_false(file.exists(file.path(output, "stale.html")))
    expect_true(file.exists(file.path(output, "safe.html")))
    expect_true(file.exists(file.path(output, "img", "example.svg")))

    html <- paste(
      readLines(file.path(output, "safe.html"), warn = FALSE),
      collapse = "\n"
    )
    expect_false(grepl("mathjax.rstudio.com", html, fixed = TRUE))
    expect_false(grepl("bootstrap", html, fixed = TRUE))
    expect_false(grepl("highlightjs", html, fixed = TRUE))
    expect_false(grepl(normalizePath(vignettes), html, fixed = TRUE))
    expect_match(html, 'src="img/example.svg"', fixed = TRUE)
  })
}
