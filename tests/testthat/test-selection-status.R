test_that("selection status renders scoped actions without duplicate sharing", {
  status <- viewer_ui_test_env()$cerebroSelectionStatus
  for (portable in c(TRUE, FALSE)) {
    ui <- htmltools::tagQuery(shiny::div(
      status("projection", "count", portable = portable)
    ))
    share <- ui$find("button.cerebro-toolbar-share")$selectedTags()
    expect_length(share, as.integer(portable))
    if (portable) {
      expect_identical(share[[1L]]$attribs[["data-view-id"]], "projection")
      expect_identical(share[[1L]]$attribs[["aria-disabled"]], "true")
    }
    expect_equal(ui$find("#count")$length(), 1L)
    for (action in c("focus", "clear")) {
      button <- ui$find(paste0(
        "#projection_",
        action,
        "_selection"
      ))$selectedTags()
      expect_length(button, 1L)
      expect_identical(
        button[[1L]]$attribs[["data-cell-view-id"]],
        "projection"
      )
      expect_identical(button[[1L]]$attribs[["data-cell-view-action"]], action)
    }
    # Decorative icons must not replace the buttons' visible accessible labels.
    for (icon in ui$find("button i")$selectedTags()) {
      expect_identical(icon$attribs[["aria-hidden"]], "true")
      expect_null(icon$attribs[["aria-label"]])
    }
  }
})

test_that("server-owned selection clearing renders a Shiny action button", {
  status <- viewer_ui_test_env()$cerebroSelectionStatus
  ui <- htmltools::tagQuery(shiny::div(
    status("projection", "count", client_actions = FALSE)
  ))
  expect_equal(ui$find("#projection_focus_selection")$length(), 0L)
  clear <- ui$find("#projection_clear_selection")
  expect_equal(clear$length(), 1L)
  expect_true(clear$hasClass("action-button"))
  expect_null(clear$selectedTags()[[1L]]$attribs[["data-cell-view-action"]])
})
