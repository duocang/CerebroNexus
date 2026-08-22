test_that("global multiselect enhancement cannot observe its own placeholder text", {
  js_file <- system.file(
    "viewer/www/multiselect.js",
    package = "CerebroNexus"
  )
  js <- paste(readLines(js_file, warn = FALSE), collapse = "\n")

  expect_match(js, "root.nodeType !== Node.ELEMENT_NODE", fixed = TRUE)
  expect_false(grepl(
    "root && root.querySelectorAll ? root : document",
    js,
    fixed = TRUE
  ))
  expect_match(
    js,
    "if (select.dataset.cerebroMultiSelectReady) return;",
    fixed = TRUE
  )
  expect_match(js, "pendingRoots", fixed = TRUE)
})
