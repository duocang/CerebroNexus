builder_asset_path <- function(...) {
  relative <- file.path(...)
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    relative
  )
  if (!file.exists(path)) {
    path <- system.file(
      file.path("builder", relative),
      package = "CerebroNexus"
    )
  }
  path
}

builder_asset_text <- function(...) {
  paste(readLines(builder_asset_path(...), warn = FALSE), collapse = "\n")
}

test_that("builder UI copy is English-only", {
  files <- c(
    "app.R",
    "adapters.R",
    "io.R",
    "inspect.R",
    "preview.R",
    "extras.R",
    "analysis.R",
    "session.R"
  )
  text <- paste(
    vapply(files, function(file) builder_asset_text(file), character(1)),
    collapse = "\n"
  )

  expect_false(
    grepl("[\u3400-\u9fff]", text),
    info = "User-visible builder assets must not contain Han-script UI copy"
  )
})

test_that("builder uses a responsive card grid", {
  css <- builder_asset_text("www", "builder.css")

  expect_match(css, "#detail \\{")
  expect_match(css, "grid-template-columns: repeat\\(12")
  expect_match(css, "#detail > \\.span-4")
  expect_match(css, "#detail > \\.span-8")
  expect_match(css, "@media \\(max-width: 46rem\\)")
})

test_that("builder client avoids layout-measurement animation loops", {
  js <- builder_asset_text("www", "builder.js")

  expect_false(grepl("getBoundingClientRect", js, fixed = TRUE))
  expect_false(grepl("ResizeObserver", js, fixed = TRUE))
  expect_false(grepl("requestAnimationFrame", js, fixed = TRUE))
  expect_false(grepl("offsetWidth", js, fixed = TRUE))
  expect_match(js, "swatch_change", fixed = TRUE)
})

test_that("spatial translation does not invalidate image encoding", {
  lines <- readLines(builder_asset_path("app.R"), warn = FALSE)
  start <- grep("encoded_image <- reactive", lines, fixed = TRUE)
  finish <- grep("image_base_bounds <- reactive", lines, fixed = TRUE)
  encoded <- paste(lines[start:(finish - 1L)], collapse = "\n")

  expect_false(grepl("img_dx|img_dy|img_scale", encoded))
  expect_match(encoded, "img_rotate", fixed = TRUE)
})
