library(shinytest2)

builder_browser_dir <- builder_profile_inst_path("builder")

test_that("metadata retention and Group actions remain independent", {
  js <- paste(
    readLines(
      file.path(builder_browser_dir, "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(js, 'action: "set-retention"', fixed = TRUE)
  expect_match(js, "retained: retained", fixed = TRUE)
  expect_match(js, 'action: "set-groups"', fixed = TRUE)
  expect_match(js, "included: included", fixed = TRUE)
  expect_match(js, "default: defaultGroup", fixed = TRUE)
})

test_that("Spatial editor exposes named images and dynamic action boundaries", {
  environment <- new.env(parent = globalenv())
  sys.source(
    file.path(builder_browser_dir, "extras.R"),
    envir = environment
  )
  sys.source(
    file.path(builder_browser_dir, "ui", "enhance_stage.R"),
    envir = environment
  )
  html <- htmltools::renderTags(environment$builder_spatial_alignment_ui(
    "enhance",
    list(
      label = "Spatial alignment",
      sections = c("section_a", "section_b"),
      images = list(
        section_a = list(`H&E` = list(), DAPI = list()),
        section_b = list(`H&E` = list())
      ),
      spatial_image_storage = "external"
    )
  ))$html

  for (text in c(
    "Image storage",
    "External files in App (spatial-assets/)",
    "Embedded in CRB",
    "H&amp;E",
    "DAPI",
    "Apply transform to matching image label"
  )) {
    expect_match(html, text, fixed = TRUE)
  }
  expect_match(html, 'id="enhance-add_image_label"', fixed = TRUE)
  expect_match(html, 'id="enhance-alignment_status"', fixed = TRUE)
  expect_false(grepl('id="enhance-rename_image"', html, fixed = TRUE))
  expect_false(grepl('id="enhance-drop_image"', html, fixed = TRUE))
})

builder_browser_mock_folder_picker <- function(
  output_dir,
  .local_envir = parent.frame()
) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  fake_bin <- withr::local_tempdir(.local_envir = .local_envir)
  picker_script <- c(
    "#!/bin/sh",
    sprintf(
      "printf '%%s\\n' %s",
      shQuote(normalizePath(output_dir, winslash = "/", mustWork = TRUE))
    )
  )
  pickers <- file.path(fake_bin, c("osascript", "zenity", "kdialog"))
  for (picker in pickers) {
    writeLines(picker_script, picker)
  }
  Sys.chmod(pickers, mode = "0755")
  withr::local_envvar(
    PATH = paste(fake_bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    .local_envir = .local_envir
  )
  invisible(output_dir)
}

test_that("browser folder picker mock covers macOS and Linux commands", {
  output_dir <- file.path(withr::local_tempdir(), "build-output")
  builder_browser_mock_folder_picker(output_dir)

  pickers <- c("osascript", "zenity", "kdialog")
  expect_true(all(nzchar(Sys.which(pickers))))
  expect_identical(
    trimws(system2("zenity", stdout = TRUE)),
    normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  )
})

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
    "position: getComputedStyle(action).position,",
    "primaryVisible: window.__builderPrimaryActionVisible === true",
    "};",
    "})()"
  ))
}

test_that("builder interaction reflows and preserves accessible state", {
  app_dir <- builder_browser_current_contract_app(
    builder_browser_dir,
    .local_envir = environment()
  )
  local_app_support(app_dir)
  output_dir <- file.path(
    tempdir(),
    paste0("builder-browser-result-", Sys.getpid())
  )
  builder_browser_mock_folder_picker(output_dir)
  withr::defer(unlink(output_dir, recursive = TRUE, force = TRUE))
  app <- AppDriver$new(
    app_dir,
    name = "builder_accessible_interaction",
    width = 1280,
    height = 900,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)

  builder_with_browser_diagnostics(app, "browser-accessible-load-example", {
    builder_browser_wait_for_example_ready(app)
    app$click(selector = ".example-btn[data-ex=all_content]")
    app$wait_for_js(
      paste0(
        "document.querySelector('.ds-pick[aria-current=true]') !== null && ",
        "document.querySelector('[aria-current=stage]') !== null && ",
        "document.getElementById('complete_dataset_check') !== null"
      ),
      timeout = 60000
    )
    app$wait_for_idle(timeout = 30000)
  })
  builder_browser_dismiss_project_offer(app)
  app$run_js(paste0(
    "const dismiss=document.querySelector('.builder-first-run-dismiss');",
    "if(dismiss) dismiss.click();"
  ))
  app$wait_for_js(
    "document.querySelector('.builder-first-run').hidden === true",
    timeout = 10000
  )
  expect_false(app$get_js(
    "getComputedStyle(document.body).overflowY === 'hidden'"
  ))

  app$wait_for_js(
    paste0(
      "document.querySelector(",
      "'.viewer-metadata-retain[data-group=\"orig.ident\"]') !== null"
    ),
    timeout = 10000
  )
  expect_true(app$get_js(paste0(
    "document.querySelector(",
    "'.viewer-metadata-retain[data-group=\"orig.ident\"]')",
    ".checked"
  )))
  app$click(
    selector = paste0(
      ".viewer-metadata-retain[data-group=\"orig.ident\"]"
    )
  )
  app$wait_for_js(
    paste0(
      "!document.querySelector(",
      "'.viewer-metadata-retain[data-group=\"orig.ident\"]').checked"
    ),
    timeout = 10000
  )
  app$click(
    selector = paste0(
      ".viewer-metadata-retain[data-group=\"orig.ident\"]"
    )
  )
  expect_true(app$get_js(paste0(
    "!document.querySelector(",
    "'.viewer-metadata-retain[data-group=\"patient_id\"]')",
    ".disabled"
  )))
  app$click(selector = ".viewer-metadata-select[data-action=all-supported]")
  app$wait_for_js(
    paste0(
      "document.querySelector(",
      "'.viewer-metadata-retain[data-group=\"patient_id\"]').checked"
    ),
    timeout = 10000
  )
  app$click(selector = ".viewer-group-select[data-action=all]")
  app$wait_for_js(
    paste0(
      "document.querySelector(",
      "'.viewer-metadata-retain[data-group=\"orig.ident\"]').checked"
    ),
    timeout = 10000
  )

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

  builder_browser_check_all_datasets(app)
  app$wait_for_js(
    "document.getElementById('continue_to_review') !== null",
    timeout = 10000
  )
  readiness_text <- app$get_js(
    "document.querySelector('.builder-configure-readiness').textContent"
  )
  expect_false(
    app$get_js(
      "document.getElementById('continue_to_review').matches(':disabled')"
    ),
    info = readiness_text
  )
  app$click("continue_to_review")
  app$wait_for_js(
    "document.getElementById('review-stage') !== null",
    timeout = 10000
  )
  review_text <- app$get_js(
    "document.getElementById('review-stage').textContent"
  )
  expect_match(review_text, "Creates CRB files", fixed = TRUE)
  expect_match(review_text, "1 dataset", fixed = TRUE)
  expect_false(grepl("Creates Shiny App", review_text, fixed = TRUE))
  expect_false(grepl(
    "Creates CRB files + private App",
    review_text,
    fixed = TRUE
  ))
  expect_true(app$get_js(
    "document.querySelector('#review-stage .review-summary-strip') !== null"
  ))
  expect_false(app$get_js(paste0(
    "document.querySelector('#review-stage input:not([type=hidden]), ",
    "#review-stage select, #review-stage textarea') !== null"
  )))
  app$click("confirm_review")
  app$wait_for_js(
    paste0(
      "document.querySelector('[data-workflow-stage=build]') !== null && ",
      "document.getElementById('build') !== null && ",
      "document.getElementById('build').disabled"
    ),
    timeout = 30000
  )
  app$click("choose_output_folder")
  app$wait_for_js(
    paste0(
      "document.getElementById('build') !== null && ",
      "!document.getElementById('build').disabled"
    ),
    timeout = 30000
  )
  expect_false(app$get_js("document.querySelector('.result-card') !== null"))

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

test_that("duplicate spatial image names open a usable naming modal", {
  skip_if_not(identical(Sys.getenv("CEREBRO_RUN_BROWSER_TESTS"), "true"))
  app_dir <- builder_browser_current_contract_app(
    builder_browser_dir,
    .local_envir = environment()
  )
  local_app_support(app_dir)

  image_path <- tempfile("builder-modal-image-", fileext = ".png")
  png::writePNG(
    array(
      rep(c(0.12, 0.42, 0.82), each = 16L * 16L),
      dim = c(16L, 16L, 3L)
    ),
    image_path
  )
  withr::defer(unlink(image_path, force = TRUE))

  app <- AppDriver$new(
    app_dir,
    name = "builder_duplicate_spatial_image_modal",
    width = 1280,
    height = 900,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)
  builder_with_browser_diagnostics(app, "browser-spatial-modal-load-example", {
    builder_browser_wait_for_example_ready(app)
    app$click(selector = ".example-btn[data-ex=all_content]")
    app$wait_for_js(
      "document.getElementById('enhance-tissue_image_file') !== null",
      timeout = 60000
    )
  })
  builder_browser_dismiss_project_offer(app)

  builder_with_browser_diagnostics(app, "browser-spatial-modal-first-upload", {
    app$upload_file(`enhance-tissue_image_file` = image_path)
    app$wait_for_js(
      paste0(
        "Object.keys(document.getElementById(",
        "'enhance-active_image').selectize.options).length === 1 && ",
        "document.querySelector('.enhance-tissue-file-item') !== null && ",
        "document.querySelector('#enhance-alignment_status ",
        ".builder-status--ready') !== null"
      ),
      timeout = 30000
    )
  })
  section <- app$get_js(
    "document.getElementById('enhance-active_section').value"
  )
  app$upload_file(`enhance-tissue_image_file` = image_path)
  tryCatch(
    app$wait_for_js(
      paste0(
        "(() => { const modal = document.getElementById('shiny-modal'); ",
        "const label = document.getElementById('enhance-new_image_label'); ",
        "const confirm = document.getElementById('enhance-add_image_confirm'); ",
        "return modal && (modal.classList.contains('show') || ",
        "modal.classList.contains('in')) && label && confirm && ",
        "label.classList.contains('shiny-bound-input') && ",
        "confirm.classList.contains('shiny-bound-input') && ",
        "confirm.offsetParent !== null && ",
        "modal.textContent.includes('Name this image'); })()"
      ),
      timeout = 10000
    ),
    error = function(error) {
      logs <- app$get_logs()
      browser_failures <- logs[
        as.character(logs$location) == "chromote" &
          tolower(as.character(logs$level)) %in%
            c("error", "warning", "assert", "throw"),
        ,
        drop = FALSE
      ]
      stop(
        paste(
          conditionMessage(error),
          paste(browser_failures$message, collapse = "\n"),
          sep = "\n"
        ),
        call. = FALSE
      )
    }
  )
  app$set_inputs(`enhance-new_image_label` = "DAPI")
  app$wait_for_idle(timeout = 10000)
  expect_identical(
    app$get_js("document.getElementById('enhance-new_image_label').value"),
    "DAPI"
  )
  app$click("enhance-add_image_confirm")
  app$wait_for_js(
    "document.getElementById('shiny-modal') === null",
    timeout = 10000
  )
  app$wait_for_js(
    paste0(
      "Object.keys(document.getElementById(",
      "'enhance-active_image').selectize.options).length === 2"
    ),
    timeout = 30000
  )
  app$wait_for_js(
    "document.querySelector('#enhance-alignment_status .builder-status--ready') !== null",
    timeout = 10000
  )

  expect_identical(
    app$get_js("document.getElementById('enhance-active_section').value"),
    section
  )
  expect_setequal(
    unlist(
      app$get_js(
        paste0(
          "Object.keys(document.getElementById(",
          "'enhance-active_image').selectize.options).sort()"
        )
      ),
      use.names = FALSE
    ),
    c(basename(image_path), "DAPI")
  )
  builder_expect_clean_browser_logs(app)
})

test_that("builder explains a mocked old privacy contract exactly", {
  old_contract_app <- builder_browser_old_contract_app(
    builder_browser_dir,
    .local_envir = environment()
  )
  local_app_support(old_contract_app)
  app <- AppDriver$new(
    old_contract_app,
    name = "builder_old_privacy_contract",
    width = 768,
    height = 800,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)
  builder_browser_wait_for_example_ready(app)
  app$click(selector = ".example-btn[data-ex=all_content]")
  builder_browser_dismiss_project_offer(app)
  builder_browser_check_all_datasets(app)
  app$wait_for_js(
    "document.getElementById('continue_to_review') !== null",
    timeout = 10000
  )
  app$click("continue_to_review")
  app$wait_for_js(
    "document.getElementById('confirm_review') !== null",
    timeout = 10000
  )
  app$click("confirm_review")
  app$wait_for_js(
    "document.querySelector('.builder-app-capability-reason') !== null",
    timeout = 10000
  )
  expect_true(app$get_js(
    "document.querySelector('input[name=build_output_mode][value=app]').matches(':disabled')"
  ))
  expect_identical(
    app$get_js(
      "document.querySelector('.builder-app-capability-title').textContent.trim()"
    ),
    "Viewer App unavailable"
  )
  expect_match(
    app$get_js(
      "document.querySelector('.builder-app-capability-reason').textContent.trim()"
    ),
    "secure Viewer App export runtime"
  )
  expect_identical(
    app$get_js(
      "document.querySelector('.builder-app-capability-command') === null"
    ),
    TRUE
  )
})
