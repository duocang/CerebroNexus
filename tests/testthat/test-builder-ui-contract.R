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
    "spatial.R",
    "inspect.R",
    "preview.R",
    "extras.R",
    "analysis.R",
    "worker.R",
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

  expect_match(css, "#workbench \\{")
  expect_match(css, "\\.builder-stage,")
  expect_match(css, "grid-template-columns: 13.5rem")
  expect_match(css, "@media \\(max-width: 42.5rem\\)")
})

test_that("builder client avoids layout-measurement animation loops", {
  js <- builder_asset_text("www", "builder.js")

  expect_false(grepl("getBoundingClientRect", js, fixed = TRUE))
  expect_false(grepl("ResizeObserver", js, fixed = TRUE))
  expect_false(grepl("requestAnimationFrame", js, fixed = TRUE))
  expect_false(grepl("offsetWidth", js, fixed = TRUE))
  expect_false(grepl("#detail", js, fixed = TRUE))
  expect_false(grepl("swatch_change", js, fixed = TRUE))
})

test_that("builder keeps primary actions in flow and exposes a narrow manager", {
  css <- builder_asset_text("www", "builder.css")

  expect_match(css, "\\.actionbar \\{\\s*position: static", perl = TRUE)
  expect_false(grepl(
    "\\.actionbar \\{[^}]*position: fixed",
    css,
    perl = TRUE
  ))
  expect_match(css, "@media (max-width: 56.25rem)", fixed = TRUE)
  expect_match(css, "@media (max-width: 42.5rem)", fixed = TRUE)
  expect_match(css, ".rail-summary", fixed = TRUE)
  expect_match(css, ".rail.is-manager-open", fixed = TRUE)
  expect_match(css, ".builder-stage[aria-current=\"stage\"]", fixed = TRUE)
})

test_that("builder client owns accessible dialog and live-state semantics", {
  js <- builder_asset_text("www", "builder.js")

  expect_match(js, 'setAttribute("role", "dialog")', fixed = TRUE)
  expect_match(js, 'setAttribute("aria-modal", "true")', fixed = TRUE)
  expect_match(js, 'event.key === "Escape"', fixed = TRUE)
  expect_match(js, "focusableElements", fixed = TRUE)
  expect_match(js, "restoreFocus", fixed = TRUE)
  expect_false(grepl("window.confirm", js, fixed = TRUE))

  expect_match(js, "builder-live-status", fixed = TRUE)
  expect_match(js, 'setAttribute("aria-live", "polite")', fixed = TRUE)
  expect_match(js, "scheduleStatusAnnouncement", fixed = TRUE)
  expect_match(js, 'setAttribute("aria-current", "stage")', fixed = TRUE)
  expect_match(js, "window.__builderPrimaryActionVisible", fixed = TRUE)
  expect_match(js, "window.__builderMotionDuration", fixed = TRUE)
  expect_match(
    js,
    'matchMedia("(prefers-reduced-motion: reduce)")',
    fixed = TRUE
  )
})

test_that("builder previews and colour controls have text equivalents", {
  js <- builder_asset_text("www", "builder.js")

  expect_match(js, "plotly_afterplot", fixed = TRUE)
  expect_match(js, "builder-preview-summary", fixed = TRUE)
  expect_match(js, 'input[type="color"]', fixed = TRUE)
  expect_match(js, "colourLabel", fixed = TRUE)
  expect_match(js, "value.toUpperCase()", fixed = TRUE)
  expect_false(grepl("app_dir", js, fixed = TRUE))
  expect_false(grepl("publish", js, ignore.case = TRUE))
})

test_that("result actions remain native keyboard controls", {
  status <- builder_asset_text("ui", "build_status.R")

  for (id in c("open_app", "reveal_folder", "copy_path", "copy_report")) {
    expect_match(
      status,
      paste0('actionButton(\n        "', id, '"'),
      fixed = TRUE
    )
  }
  expect_false(grepl('tabindex = "-1"', status, fixed = TRUE))
})

test_that("spatial translation does not invalidate image encoding", {
  lines <- readLines(builder_asset_path("app.R"), warn = FALSE)
  start <- grep("encoded_image <- reactive", lines, fixed = TRUE)
  finish <- grep("image_base_bounds <- reactive", lines, fixed = TRUE)
  encoded <- paste(lines[start:(finish - 1L)], collapse = "\n")

  expect_false(grepl("img_dx|img_dy|img_scale", encoded))
  expect_match(encoded, "img_rotate", fixed = TRUE)
})
