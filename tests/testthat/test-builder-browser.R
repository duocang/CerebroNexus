library(shinytest2)

builder_browser_dir <- builder_profile_inst_path("builder")

builder_browser_mock_folder_picker <- function(
  output_dir,
  .local_envir = parent.frame()
) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  fake_bin <- withr::local_tempdir(.local_envir = .local_envir)
  picker <- file.path(fake_bin, "osascript")
  writeLines(
    c(
      "#!/bin/sh",
      sprintf(
        "printf '%%s\\n' %s",
        shQuote(normalizePath(output_dir, winslash = "/", mustWork = TRUE))
      )
    ),
    picker
  )
  Sys.chmod(picker, mode = "0755")
  withr::local_envvar(
    PATH = paste(fake_bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    .local_envir = .local_envir
  )
  invisible(output_dir)
}

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
    "const rail = document.querySelector('.rail');",
    "const summary = document.querySelector('.rail-summary');",
    "const main = document.querySelector('.builder-content');",
    "return {",
    "width: window.innerWidth,",
    "documentWidth: document.documentElement.scrollWidth,",
    "pageHeight: document.documentElement.scrollHeight,",
    "railVisible: getComputedStyle(rail).display !== 'none',",
    "summaryVisible: getComputedStyle(summary).display !== 'none',",
    "mainWidth: main.getBoundingClientRect().width,",
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
  output_dir <- file.path(
    tempdir(),
    paste0("builder-browser-result-", Sys.getpid())
  )
  builder_browser_mock_folder_picker(output_dir)
  withr::defer(unlink(output_dir, recursive = TRUE, force = TRUE))
  app <- AppDriver$new(
    builder_browser_dir,
    name = "builder_accessible_interaction",
    width = 1280,
    height = 900,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)

  app$wait_for_js(
    "document.querySelector('.example-btn[data-ex=basic_pbmc]') !== null",
    timeout = 10000
  )
  app$click(selector = ".example-btn[data-ex=basic_pbmc]")
  app$wait_for_js(
    paste0(
      "document.querySelector('.ds-pick[aria-current=true]') !== null && ",
      "document.querySelector('[aria-current=stage]') !== null && ",
      "document.getElementById('build') !== null"
    ),
    timeout = 60000
  )
  app$wait_for_idle(timeout = 30000)
  expect_false(app$get_js(
    "getComputedStyle(document.body).overflowY === 'hidden'"
  ))

  expect_false(app$get_js(
    "document.querySelector('.builder-select-initial') !== null"
  ))
  expect_false(app$get_js(
    "document.querySelector('.builder-duplicate') !== null"
  ))
  expect_true(app$get_js(paste0(
    "Array.from(document.querySelectorAll('.enhance-module details'))",
    ".every(node => node.open === false)"
  )))
  expect_true(app$get_js(paste0(
    "Array.from(document.querySelectorAll('.builder-stage-review details'))",
    ".every(node => node.open === false)"
  )))

  app$set_inputs(make_app = FALSE)
  app$wait_for_js(
    paste0(
      "document.getElementById('review-stage').textContent.includes(",
      "'Creates CRB files') && ",
      "!document.getElementById('review-stage').textContent.includes(",
      "'Creates CRB files + private App')"
    ),
    timeout = 10000
  )
  app$set_inputs(make_app = TRUE)
  app$wait_for_js(
    paste0(
      "document.getElementById('review-stage').textContent.includes(",
      "'Creates CRB files + private App')"
    ),
    timeout = 10000
  )

  app$click("review_current_dataset")
  app$wait_for_js(
    "document.querySelector('.rail-review-status.reviewed') !== null",
    timeout = 10000
  )
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

  geometry_by_width <- list()
  for (viewport in list(
    c(1920L, 1080L),
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
    geometry_by_width[[as.character(viewport[[1]])]] <- geometry
    expect_lte(geometry$documentWidth, geometry$width + 1)
    expect_gte(geometry$pageHeight, geometry$viewportHeight)
    expect_identical(geometry$position, "static")
    expect_gte(geometry$actionTop, 0)
    expect_lte(geometry$actionBottom, geometry$viewportHeight)

    expect_identical(geometry$summaryVisible, viewport[[1]] <= 928L)
    expect_identical(geometry$railVisible, viewport[[1]] > 928L)
  }
  expect_gte(geometry_by_width[["768"]]$mainWidth, 768L * 0.9)
  expect_lte(geometry_by_width[["768"]]$mainWidth, 768L)
  expect_true(geometry_by_width[["1280"]]$railVisible)
  expect_true(geometry_by_width[["1920"]]$railVisible)

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
  old_contract_app <- function() {
    builder_browser_old_contract_app(
      file.path(builder_browser_dir, "app.R"),
      .local_envir = parent.frame()
    )
  }
  app <- AppDriver$new(
    old_contract_app(),
    name = "builder_old_privacy_contract",
    width = 768,
    height = 800,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)
  app$wait_for_js(
    "document.querySelector('.example-btn[data-ex=basic_pbmc]') !== null",
    timeout = 10000
  )
  app$click(selector = ".example-btn[data-ex=basic_pbmc]")
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
