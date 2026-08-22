loading_path <- builder_profile_inst_path("builder", "loading.R")
rail_path <- builder_profile_inst_path("builder", "ui", "dataset_rail.R")
if (file.exists(loading_path)) {
  sys.source(loading_path, envir = globalenv())
}
if (file.exists(rail_path)) {
  sys.source(rail_path, envir = globalenv())
}

test_that("import rail patches preserve unchanged sibling rows", {
  first <- builder_import_entry(
    "ds1",
    "Patient one",
    list(kind = "file", staged_path = "/private/ds1.rds"),
    filename = "ds1.rds",
    file_type = "RDS",
    size = 2048
  )
  second <- builder_import_entry(
    "ds2",
    "Patient two",
    list(kind = "file", staged_path = "/private/ds2.rds"),
    filename = "ds2.rds",
    file_type = "RDS",
    size = 4096
  )
  entries <- list(ds1 = first, ds2 = second)
  before <- builder_import_rail_patch(entries, current = "ds1")

  entries$ds1$load_state <- "reading"
  entries$ds1$progress_label <- "Reading Seurat object…"
  after <- builder_import_rail_patch(entries, current = "ds1")

  expect_null(names(before$rows))
  expect_identical(
    unname(vapply(before$rows, `[[`, character(1), "id")),
    c("ds1", "ds2")
  )
  expect_false(identical(
    before$rows[[1L]]$fingerprint,
    after$rows[[1L]]$fingerprint
  ))
  expect_identical(before$rows[[2L]]$fingerprint, after$rows[[2L]]$fingerprint)
  expect_match(after$rows[[1L]]$html, 'data-import-id="ds1"', fixed = TRUE)
  expect_match(after$rows[[1L]]$html, 'data-import-fingerprint="', fixed = TRUE)
})

test_that("loading workbench remains visible and accessible", {
  fun <- get0("builder_loading_workbench_ui", mode = "function")
  expect_true(is.function(fun))
  if (!is.function(fun)) {
    return()
  }
  entry <- builder_import_entry(
    "ds1",
    "All content",
    list(kind = "example", example = "all_content")
  )
  entry$load_state <- "inspecting"
  entry$progress_label <- "Checking cells, genes and metadata…"
  html <- htmltools::renderTags(fun(entry))$html

  expect_match(html, "Loading dataset", fixed = TRUE)
  expect_match(html, "All content", fixed = TRUE)
  expect_match(html, "Checking cells, genes and metadata…", fixed = TRUE)
  expect_match(html, 'aria-live="polite"', fixed = TRUE)
  expect_match(html, "builder-loading-stage", fixed = TRUE)
  expect_false(grepl("builder-remove-import", html, fixed = TRUE))
  expect_false(grepl(">Remove<", html, fixed = TRUE))
  expect_false(grepl("spinner", html, fixed = TRUE))
})

test_that("loading rail rows expose safe status and real actions", {
  entry <- builder_import_entry(
    "ds1",
    "patient-one",
    list(
      kind = "file",
      staged_path = "/private/session/upload-123/object.rds"
    ),
    filename = "patient-one.rds",
    file_type = "RDS",
    size = 2048
  )
  html <- builder_import_rail_patch(
    list(ds1 = entry),
    current = "ds1"
  )$rows[[1L]]$html

  expect_match(html, "patient-one", fixed = TRUE)
  expect_match(html, "Waiting to load", fixed = TRUE)
  expect_match(html, "builder-pick-import", fixed = TRUE)
  expect_match(html, "builder-remove-import", fixed = TRUE)
  expect_match(html, "ds ds--import is-active is-importing", fixed = TRUE)
  expect_match(html, 'data-load-state="queued"', fixed = TRUE)
  expect_match(html, 'aria-current="true"', fixed = TRUE)
  expect_match(
    html,
    'aria-label="Remove queued import patient-one"',
    fixed = TRUE
  )
  expect_match(html, "Remove from queue", fixed = TRUE)
  expect_match(html, "ds-state-dot", fixed = TRUE)
  expect_match(html, 'data-started-at-ms="', fixed = TRUE)
  expect_false(grepl("data-elapsed-ms", html, fixed = TRUE))
  expect_false(grepl("/private/session", html, fixed = TRUE))
})

test_that("active imports offer the established server cancellation action", {
  entry <- builder_import_entry(
    "ds1",
    "patient-one",
    list(kind = "file", staged_path = "/private/session/object.rds")
  )
  queue <- builder_import_add(builder_import_queue(), entry)
  queue <- builder_import_transition(queue, "ds1", "reading", 1L)

  rail_html <- builder_import_rail_patch(
    queue$entries,
    current = "ds1"
  )$rows[[1L]]$html
  workbench_html <- htmltools::renderTags(
    builder_loading_workbench_ui(queue$entries[["ds1"]])
  )$html

  expect_match(rail_html, "builder-remove-import", fixed = TRUE)
  expect_match(rail_html, "Cancel active import patient-one", fixed = TRUE)
  expect_match(rail_html, ">Cancel<", fixed = TRUE)
  expect_match(
    rail_html,
    paste(
      "ds-del btn btn-remove-soft builder-cancel-import",
      "builder-remove-import"
    ),
    fixed = TRUE
  )
  expect_false(grepl("builder-remove-import", workbench_html, fixed = TRUE))
  expect_false(grepl("Remove from queue", rail_html, fixed = TRUE))
  expect_false(grepl(">Remove<", workbench_html, fixed = TRUE))
})

test_that("error rows offer Retry and Remove without internal details", {
  entry <- builder_import_entry(
    "ds1",
    "broken",
    list(kind = "file", staged_path = "/private/session/broken.rds")
  )
  queue <- builder_import_add(builder_import_queue(), entry)
  queue <- builder_import_transition(queue, "ds1", "reading", 1L)
  queue <- builder_import_transition(
    queue,
    "ds1",
    "error",
    1L,
    error = "/private/session/broken.rds: invalid object"
  )
  html <- builder_import_rail_patch(
    queue$entries,
    current = "ds1"
  )$rows[[1L]]$html

  expect_match(html, "Could not load dataset", fixed = TRUE)
  expect_match(html, "builder-retry-import", fixed = TRUE)
  expect_match(html, "builder-remove-import", fixed = TRUE)
  expect_match(html, 'data-load-state="error"', fixed = TRUE)
  expect_match(html, "is-error", fixed = TRUE)
  expect_match(html, 'aria-label="Remove failed import broken"', fixed = TRUE)
  expect_match(html, 'data-elapsed-ms="', fixed = TRUE)
  expect_false(grepl("data-started-at-ms", html, fixed = TRUE))
  expect_false(grepl("/private/session", html, fixed = TRUE))
  expect_false(grepl("stack", html, ignore.case = TRUE))
})
