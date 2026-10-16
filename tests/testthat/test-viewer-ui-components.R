test_that("visualization toolbars render their selection status and actions", {
  pages <- viewer_ui_test_pages()
  for (page in names(pages)) {
    ui <- htmltools::tagQuery(pages[[page]])
    toolbar <- ui$find(
      if (page == "coordinated_views") ".cv-topbar" else ".cerebro-viz-toolbar"
    )
    expect_equal(toolbar$length(), 1L, info = page)
    status <- switch(
      page,
      immune_repertoire = "#ir_selection_status_UI",
      coordinated_views = "#cv-selbar",
      ".cerebro-selection-status-slot"
    )
    expect_equal(toolbar$find(status)$length(), 1L, info = page)

    actions <- toolbar$find(".cerebro-toolbar-actions")
    expect_equal(actions$length(), 1L, info = page)
    settings <- actions$find("button.cerebro-more-btn")$selectedTags()
    expect_equal(length(settings), 1L, info = page)
    target <- settings[[1L]]$attribs[["aria-controls"]]
    # Trekker deliberately opens the drawer owned by Linked views.
    drawer_page <- if (page == "trekker") {
      pages$coordinated_views
    } else {
      pages[[page]]
    }
    expect_equal(
      htmltools::tagQuery(drawer_page)$find(paste0("#", target))$length(),
      1L,
      info = page
    )
    expect_identical(
      settings[[1L]]$attribs[["data-cerebro-drawer-target"]],
      target
    )
    drawer <- htmltools::tagQuery(drawer_page)$find(paste0("#", target))
    expect_identical(drawer$selectedTags()[[1L]]$attribs$role, "dialog")
    expect_identical(
      drawer$selectedTags()[[1L]]$attribs[["aria-hidden"]],
      "true"
    )

    share <- actions$find("button.cerebro-toolbar-share")$selectedTags()
    expect_equal(length(share), if (page == "trekker") 0L else 1L, info = page)
    if (length(share)) {
      # The initial markup must not offer an export before a plot is ready.
      expect_identical(share[[1L]]$attribs[["aria-disabled"]], "true")
      expect_identical(share[[1L]]$attribs$disabled, "disabled")
      expect_identical(
        share[[1L]]$attribs[["aria-controls"]],
        "cv-config-dialog"
      )
    }
  }
})

test_that("Linked views renders its heading and status outside the canvases", {
  ui <- htmltools::tagQuery(viewer_ui_test_pages()$coordinated_views)
  expect_equal(ui$find("h1")$length(), 1L)
  expect_equal(ui$find("#cv-meta")$length(), 1L)
  expect_equal(ui$find(".cv-topbar #cv-selbar")$length(), 1L)
  expect_equal(ui$find(".cv-panes #cv-selbar")$length(), 0L)
  expect_gt(ui$find(".cv-pane")$length(), 0L)
  expect_equal(
    ui$find(".cv-pane.cv-hidden")$length(),
    ui$find(".cv-pane")$length()
  )
  expect_equal(ui$find(".cv-resize-handle")$length(), 0L)
})

test_that("Linked settings expose the shared display and data controls", {
  ui <- htmltools::tagQuery(viewer_ui_test_pages()$coordinated_views)
  drawer <- ui$find("#cv-more")
  expect_equal(drawer$length(), 1L)
  for (id in c(
    "cv-labels",
    "cv-borders",
    "cv-square-plots",
    "cv-ps",
    "cv-opacity",
    "cv-pct",
    "cv-dissolve",
    "cv-niche",
    "cv-clip"
  )) {
    expect_equal(drawer$find(paste0("#", id))$length(), 1L, info = id)
  }
  html <- as.character(drawer$selectedTags()[[1]])
  for (label in c(
    "Background image",
    "Appearance",
    "Data",
    "Spatial mapping",
    "Group filters",
    "Colour range"
  )) {
    expect_match(html, label, fixed = TRUE)
  }
  share <- ui$find("#cv-config-open")$selectedTags()[[1]]
  expect_identical(share$attribs[["data-view-id"]], "linked_views")
})
