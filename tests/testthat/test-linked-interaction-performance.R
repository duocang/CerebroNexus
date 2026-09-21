test_that("Linked views defaults large data to static but keeps an override", {
  ui <- paste(
    readLines(
      viewer_test_path("coordinated_views", "UI.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  javascript <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(ui, 'id = "cv-linked-interaction-toggle"', fixed = TRUE)
  expect_match(javascript, "var LINKED_STATIC_ABOVE = 200000;", fixed = TRUE)
  expect_match(
    javascript,
    "linkedInteractionEnabled = Number(D.n) <= LINKED_STATIC_ABOVE;",
    fixed = TRUE
  )
  expect_match(javascript, "function toggleLinkedInteraction()", fixed = TRUE)
  expect_match(javascript, "if (interactionIsStatic()) return;", fixed = TRUE)
  expect_match(
    javascript,
    "cells are rendered without hover or linked selection.",
    fixed = TRUE
  )
})
