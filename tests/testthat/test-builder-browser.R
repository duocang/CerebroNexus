library(shinytest2)

builder_browser_dir <- builder_profile_inst_path("builder")

builder_browser_escape <- function(app) {
  app$run_js(paste0(
    "document.activeElement.dispatchEvent(new KeyboardEvent('keydown',",
    "{key:'Escape',bubbles:true}));"
  ))
}

builder_browser_geometry <- function(app) {
  app$get_js(paste0(
    "(() => {",
    "const action = document.getElementById('build');",
    "const box = action.getBoundingClientRect();",
    "return {",
    "width: window.innerWidth,",
    "documentWidth: document.documentElement.scrollWidth,",
    "actionTop: box.top, actionBottom: box.bottom,",
    "viewportHeight: window.innerHeight,",
    "position: getComputedStyle(action.closest('.actionbar')).position,",
    "primaryVisible: window.__builderPrimaryActionVisible === true",
    "};",
    "})()"
  ))
}

test_that("builder interaction reflows and preserves accessible state", {
  local_app_support(builder_browser_dir)
  app <- AppDriver$new(
    builder_browser_dir,
    name = "builder_accessible_interaction",
    width = 1280,
    height = 900,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)

  app$click("open_browser")
  app$wait_for_js(
    "document.querySelector('.sheet[role=dialog][aria-modal=true]') !== null",
    timeout = 10000
  )
  expect_true(app$get_js(
    "document.querySelector('.sheet').contains(document.activeElement)"
  ))
  builder_browser_escape(app)
  app$wait_for_js("document.querySelector('.sheet') === null", timeout = 10000)
  expect_identical(app$get_js("document.activeElement.id"), "open_browser")

  app$click("open_browser")
  app$wait_for_js(
    "document.querySelector('.example-btn[data-ex=basic_pbmc]') !== null",
    timeout = 10000
  )
  app$click(selector = ".example-btn[data-ex=basic_pbmc]")
  app$wait_for_js("document.querySelector('.sheet') === null", timeout = 10000)
  app$wait_for_js(
    paste0(
      "document.querySelector('.ds-pick[aria-current=true]') !== null && ",
      "document.querySelector('[aria-current=stage]') !== null && ",
      "document.getElementById('build') !== null"
    ),
    timeout = 60000
  )
  app$wait_for_idle(timeout = 30000)

  app$set_inputs(make_app = FALSE)
  app$wait_for_js(
    "document.getElementById('review-stage').textContent.includes('crbs_only')",
    timeout = 10000
  )
  app$set_inputs(make_app = TRUE)
  app$wait_for_js(
    paste0(
      "document.getElementById('review-stage').textContent.includes(",
      "'crbs_and_private_app')"
    ),
    timeout = 10000
  )

  output_dir <- file.path(
    tempdir(),
    paste0("builder-browser-result-", Sys.getpid())
  )
  app$set_inputs(out_dir = output_dir)
  app$wait_for_js(
    "!document.getElementById('build').disabled",
    timeout = 30000
  )
  app$run_js(paste0(
    "window.__builderCopiedText = null;",
    "Shiny.addCustomMessageHandler('builder_copy_text', function(message) {",
    "window.__builderCopiedText = message.text;",
    "});"
  ))
  app$click("build")
  app$wait_for_js(
    "document.querySelector('.result-card.success') !== null",
    timeout = 180000
  )
  expect_true(app$get_js("document.getElementById('open_app') !== null"))
  expect_true(app$get_js("document.getElementById('reveal_folder') !== null"))
  expected_release <- app$get_js(
    "document.getElementById('copy_path').dataset.path"
  )
  app$click("copy_path")
  app$wait_for_js("window.__builderCopiedText !== null", timeout = 10000)
  expect_identical(app$get_js("window.__builderCopiedText"), expected_release)

  for (viewport in list(
    c(1280L, 900L),
    c(768L, 800L),
    c(390L, 844L),
    c(320L, 720L)
  )) {
    app$get_chromote_session()$set_viewport_size(
      width = viewport[[1]],
      height = viewport[[2]]
    )
    app$wait_for_js(
      sprintf(
        "window.innerWidth === %d && window.innerHeight === %d",
        viewport[[1]],
        viewport[[2]]
      ),
      timeout = 10000
    )
    app$run_js(
      "document.getElementById('build').scrollIntoView({block:'center'});"
    )
    app$wait_for_js(
      "window.__builderPrimaryActionVisible === true",
      timeout = 10000
    )
    geometry <- builder_browser_geometry(app)
    expect_lte(geometry$documentWidth, geometry$width + 1)
    expect_identical(geometry$position, "static")
    expect_gte(geometry$actionTop, 0)
    expect_lte(geometry$actionBottom, geometry$viewportHeight)

    manager_is_compact <- app$get_js(paste0(
      "getComputedStyle(document.querySelector('.rail-summary')).display !== ",
      "'none'"
    ))
    expect_identical(manager_is_compact, viewport[[1]] <= 680L)
  }

  app$click(selector = ".rail-summary")
  app$wait_for_js(
    "document.querySelector('.rail.is-manager-open[role=dialog]') !== null",
    timeout = 10000
  )
  expect_identical(
    app$get_js("document.querySelector('.rail').getAttribute('aria-modal')"),
    "true"
  )
  app$click(selector = ".builder-drop")
  app$wait_for_js(
    "document.querySelector('.builder-confirm-dialog[role=dialog]') !== null",
    timeout = 10000
  )
  expect_true(app$get_js(paste0(
    "document.querySelector('.builder-confirm-dialog').contains(",
    "document.activeElement)"
  )))
  builder_browser_escape(app)
  app$wait_for_js(
    "document.querySelector('.builder-confirm-dialog') === null",
    timeout = 10000
  )
  expect_true(app$get_js(
    "document.activeElement.classList.contains('builder-drop')"
  ))
  builder_browser_escape(app)
  app$wait_for_js(
    "!document.querySelector('.rail').classList.contains('is-manager-open')",
    timeout = 10000
  )
  expect_true(app$get_js(
    "document.activeElement.classList.contains('rail-summary')"
  ))
  expect_identical(
    app$get_js("document.querySelector('.rail').getAttribute('aria-hidden')"),
    "true"
  )

  app$get_chromote_session()$Emulation$setEmulatedMedia(
    features = list(list(name = "prefers-reduced-motion", value = "reduce"))
  )
  app$wait_for_js("window.__builderMotionDuration === 0", timeout = 10000)
  expect_identical(
    app$get_js(
      "getComputedStyle(document.querySelector('.ds')).transitionDuration"
    ),
    "0s"
  )

  app$get_chromote_session()$Emulation$setDeviceMetricsOverride(
    width = 320,
    height = 720,
    deviceScaleFactor = 4,
    mobile = FALSE
  )
  app$wait_for_js(
    "window.innerWidth === 320 && window.devicePixelRatio === 4",
    timeout = 10000
  )
  app$run_js(
    "document.getElementById('build').scrollIntoView({block:'center'});"
  )
  app$wait_for_js(
    "window.__builderPrimaryActionVisible === true",
    timeout = 10000
  )

  expect_true(app$get_js(
    "document.getElementById('builder-live-status') !== null"
  ))
  expect_identical(
    app$get_js(
      "document.getElementById('builder-live-status').getAttribute('aria-live')"
    ),
    "polite"
  )
  app$wait_for_js(
    "document.getElementById('builder-live-status').textContent.trim().length > 0",
    timeout = 10000
  )
})

test_that("builder explains a mocked old privacy contract exactly", {
  old_contract_app <- local({
    app_file <- file.path(builder_browser_dir, "app.R")
    function() {
      namespace <- asNamespace("CerebroNexus")
      marker <- ".cerebro_bundle_privacy_contract_version"
      if (exists(marker, namespace, inherits = FALSE)) {
        if (bindingIsLocked(marker, namespace)) {
          unlockBinding(marker, namespace)
        }
        assign(marker, 0L, envir = namespace)
        lockBinding(marker, namespace)
      }
      shiny::shinyAppFile(app_file)
    }
  })
  app <- AppDriver$new(
    old_contract_app,
    name = "builder_old_privacy_contract",
    width = 768,
    height = 800,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)
  app$click("open_browser")
  app$wait_for_js(
    "document.querySelector('.example-btn[data-ex=basic_pbmc]') !== null",
    timeout = 10000
  )
  app$click(selector = ".example-btn[data-ex=basic_pbmc]")
  app$wait_for_js("document.querySelector('.sheet') === null", timeout = 10000)
  app$wait_for_js(
    "document.querySelector('.app-capability-reason') !== null",
    timeout = 60000
  )
  expect_true(app$get_js(
    "document.getElementById('make_app').matches(':disabled')"
  ))
  expect_identical(
    app$get_js(
      "document.querySelector('.app-capability-reason').textContent.trim()"
    ),
    paste(
      "Private app publication requires privacy contract v1.",
      "Build CRB-only output for now."
    )
  )
})
