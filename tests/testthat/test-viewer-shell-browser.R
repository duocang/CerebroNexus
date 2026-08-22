library(shinytest2)

viewer_inst_dir <- system.file(package = "CerebroNexus")

test_that("mobile navigation is modal, dismissible, and exclusive with More", {
  local_app_support(viewer_inst_dir)
  app <- AppDriver$new(
    viewer_inst_dir,
    name = "viewer_mobile_navigation",
    height = 844,
    width = 390
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)
  app$wait_for_js(
    paste0(
      "document.getElementById('cerebro-nav-close') && ",
      "document.getElementById('cerebro-nav-scrim')"
    ),
    timeout = 5000
  )

  toggle <- "document.querySelector('.main-header .sidebar-toggle')"
  nav_open <- paste0(
    "document.body.classList.contains('sidebar-open') && ",
    "Math.abs(document.querySelector('.main-sidebar').getBoundingClientRect().left)<=1 && ",
    "getComputedStyle(document.getElementById('cerebro-nav-scrim')).opacity==='1'"
  )
  expect_equal(
    app$get_js(paste0(toggle, ".getAttribute('aria-label')")),
    "Open navigation"
  )
  sidebar_id <- app$get_js("document.querySelector('.main-sidebar').id")
  expect_true(nzchar(sidebar_id))
  expect_equal(
    app$get_js(paste0(toggle, ".getAttribute('aria-controls')")),
    sidebar_id
  )
  expect_equal(
    app$get_js(
      "document.querySelector('.main-sidebar').getAttribute('aria-label')"
    ),
    "Primary navigation"
  )
  expect_true(app$get_js("document.querySelector('.main-sidebar').inert"))

  app$run_js(paste0(toggle, ".click();"))
  app$wait_for_js(nav_open)
  expect_false(app$get_js("document.querySelector('.main-sidebar').inert"))
  expect_equal(
    app$get_js("document.activeElement && document.activeElement.id"),
    "cerebro-nav-close"
  )
  expect_true(app$get_js(paste0(
    "(function(){var n=document.querySelector('.main-sidebar').getBoundingClientRect();",
    "var s=getComputedStyle(document.getElementById('cerebro-nav-scrim'));",
    "return Math.abs(n.left)<=1 && n.width<=320 && s.opacity==='1' && ",
    "s.pointerEvents==='auto';})()"
  )))

  app$run_js(
    "document.querySelector('a[href=\"#shiny-tab-coordinated_views\"]').click();"
  )
  app$wait_for_js(paste0(
    "!document.body.classList.contains('sidebar-open') && ",
    "document.querySelector('.main-sidebar').inert && ",
    "document.querySelector('.tab-pane.active[id^=\"shiny-tab-\"]')",
    ".contains(document.activeElement)"
  ))

  app$run_js(paste0(toggle, ".click();"))
  app$wait_for_js(nav_open)
  app$run_js(paste0(
    "window.__escapeUnderlay=0;",
    "document.addEventListener('keydown',function(){window.__escapeUnderlay++;},",
    "{once:true});"
  ))
  app$run_js(
    "document.dispatchEvent(new KeyboardEvent('keydown',{key:'Escape',bubbles:true}));"
  )
  app$wait_for_js("!document.body.classList.contains('sidebar-open')")
  expect_equal(app$get_js("window.__escapeUnderlay"), 0)
  expect_equal(
    app$get_js("document.activeElement && document.activeElement.className"),
    "sidebar-toggle"
  )

  app$run_js(paste0(toggle, ".click();"))
  app$wait_for_js(nav_open)
  app$run_js("document.getElementById('cv-more-btn').click();")
  app$wait_for_js(paste0(
    "!document.body.classList.contains('sidebar-open') && ",
    "document.getElementById('cv-more').classList.contains('is-open')"
  ))
  app$run_js(paste0(toggle, ".click();"))
  app$wait_for_js(paste0(
    "document.body.classList.contains('sidebar-open') && ",
    "!document.getElementById('cv-more').classList.contains('is-open')"
  ))
  app$run_js("document.getElementById('cerebro-nav-scrim').click();")
  app$wait_for_js("!document.body.classList.contains('sidebar-open')")
})

test_that("mobile More traps focus inside its modal settings page", {
  local_app_support(viewer_inst_dir)
  app <- AppDriver$new(
    viewer_inst_dir,
    name = "viewer_mobile_more_focus",
    height = 844,
    width = 390
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)
  app$run_js(
    "document.querySelector('a[href=\"#shiny-tab-coordinated_views\"]').click();"
  )
  app$run_js("document.getElementById('cv-more-btn').click();")
  app$wait_for_js("document.activeElement.id==='cv-more-close'", timeout = 5000)

  app$run_js(paste0(
    "document.dispatchEvent(new KeyboardEvent('keydown',",
    "{key:'Tab',shiftKey:true,bubbles:true}));"
  ))
  expect_true(app$get_js(paste0(
    "document.getElementById('cv-more').contains(document.activeElement) && ",
    "document.activeElement.id!=='cv-more-close'"
  )))
  app$run_js(
    "document.dispatchEvent(new KeyboardEvent('keydown',{key:'Tab',bubbles:true}));"
  )
  expect_equal(
    app$get_js("document.activeElement && document.activeElement.id"),
    "cv-more-close"
  )
  app$run_js(paste0(
    "window.__escapeUnderlay=0;",
    "document.addEventListener('keydown',function(){window.__escapeUnderlay++;},",
    "{once:true});",
    "document.dispatchEvent(new KeyboardEvent('keydown',{key:'Escape',bubbles:true}));"
  ))
  app$wait_for_js(
    "document.getElementById('cv-more').getAttribute('aria-hidden')==='true'"
  )
  expect_equal(app$get_js("window.__escapeUnderlay"), 0)
})

test_that("More closes when navigation leaves Linked views", {
  local_app_support(viewer_inst_dir)
  app <- AppDriver$new(
    viewer_inst_dir,
    name = "viewer_more_navigation_close",
    height = 900,
    width = 1440
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)
  app$run_js(
    "document.querySelector('a[href=\"#shiny-tab-coordinated_views\"]').click();"
  )
  app$wait_for_js(
    "document.getElementById('cv-title-a').textContent.trim() !== '—'",
    timeout = 15000
  )
  app$run_js("document.getElementById('cv-more-btn').click();")
  app$wait_for_js("document.activeElement.id==='cv-more-close'", timeout = 5000)
  app$run_js(
    "document.querySelector('a[href=\"#shiny-tab-groups\"]').click();"
  )
  app$wait_for_js(
    "document.getElementById('cv-more').getAttribute('aria-hidden')==='true'",
    timeout = 5000
  )
  expect_false(app$get_js(
    "document.getElementById('cv-more').classList.contains('is-open')"
  ))
})
