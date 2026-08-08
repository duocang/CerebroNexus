library(shinytest2)

builder_narrow_document_height_budget <- 5000

test_that("Builder narrow height budget excludes the recorded baseline", {
  recorded_baseline <- 5500

  expect_gte(recorded_baseline, builder_narrow_document_height_budget)
})

builder_responsive_geometry <- function(app) {
  app$get_js(paste0(
    "(() => {",
    "const rail = document.querySelector('.rail');",
    "const summary = document.querySelector('.rail-summary');",
    "const main = document.querySelector('.builder-content');",
    "const intro = document.querySelector('.stage-intro');",
    "const form = document.querySelector('.builder-form-grid');",
    "const stages = Array.from(document.querySelectorAll('.builder-stage'));",
    "const stageTops = stages.map(node => node.getBoundingClientRect().top + window.scrollY);",
    "return {",
    "viewportWidth: window.innerWidth,",
    "viewportHeight: window.innerHeight,",
    "documentWidth: document.documentElement.scrollWidth,",
    "documentHeight: document.documentElement.scrollHeight,",
    "railDisplay: getComputedStyle(rail).display,",
    "managerSummaryDisplay: getComputedStyle(summary).display,",
    "mainWidth: main.getBoundingClientRect().width,",
    "stageIntroMaxWidth: parseFloat(getComputedStyle(intro).maxWidth),",
    "formGridMaxWidth: parseFloat(getComputedStyle(form).maxWidth),",
    "workflowInDocumentOrder: stageTops.every((top, index) => index === 0 || top >= stageTops[index - 1])",
    "};",
    "})()"
  ))
}

test_that("Builder preserves responsive geometry before Build", {
  app_dir <- builder_profile_inst_path("builder")
  local_app_support(app_dir)
  app <- AppDriver$new(
    app_dir,
    name = "builder_responsive_geometry",
    width = 1920,
    height = 1080,
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
      "document.querySelector('.stage-intro') !== null && ",
      "document.querySelector('.builder-form-grid') !== null"
    ),
    timeout = 60000
  )
  app$wait_for_idle(timeout = 30000)

  geometries <- list()
  for (viewport in list(
    c(1920L, 1080L),
    c(1280L, 900L),
    c(768L, 1024L),
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
    geometry <- builder_responsive_geometry(app)
    geometries[[as.character(viewport[[1]])]] <- geometry

    expect_lte(geometry$documentWidth, geometry$viewportWidth + 1)
    expect_gte(geometry$documentHeight, geometry$viewportHeight)
    expect_equal(geometry$stageIntroMaxWidth, 768, tolerance = 1)
    expect_equal(geometry$formGridMaxWidth, 896, tolerance = 1)
    expect_true(geometry$workflowInDocumentOrder)
    expect_identical(
      geometry$managerSummaryDisplay != "none",
      viewport[[1]] <= 928L
    )
    expect_identical(
      geometry$railDisplay != "none",
      viewport[[1]] > 928L
    )
  }

  expect_gte(geometries[["768"]]$mainWidth, 768 * 0.9)
  expect_lte(geometries[["768"]]$mainWidth, 768)
  expect_lte(geometries[["390"]]$documentWidth, 391)
  # The recorded baseline was about 5500px; 5000 preserves cross-font headroom.
  expect_lt(
    geometries[["390"]]$documentHeight,
    builder_narrow_document_height_budget
  )

  app$run_js(paste0(
    "document.querySelector('.builder-field--organism').scrollIntoView({block: 'center'});",
    "document.querySelector('.builder-field--organism .selectize-input').click();"
  ))
  app$wait_for_js(
    "document.querySelector('.builder-field--organism .builder-creatable-select-row') !== null",
    timeout = 10000
  )
  organism_geometry <- app$get_js(paste0(
    "(() => { const root = document.querySelector('.builder-field--organism');",
    "const candidates = [root.querySelector('.selectize-control'),",
    "root.querySelector('.selectize-input'), root.querySelector('select')];",
    "const visibleBordered = candidates.filter(node => node && ",
    "node.getClientRects().length && parseFloat(getComputedStyle(node).borderTopWidth) > 0).length;",
    "return {visibleBordered, documentWidth: document.documentElement.scrollWidth,",
    "viewportWidth: window.innerWidth}; })()"
  ))
  expect_identical(organism_geometry$visibleBordered, 1L)
  expect_lte(
    organism_geometry$documentWidth,
    organism_geometry$viewportWidth + 1
  )
  app$click(selector = ".builder-creatable-select-input")
  organism_click_state <- app$get_js(paste0(
    "(() => { const selectize = document.getElementById('core-organism').selectize;",
    "return {active: document.activeElement === document.querySelector('.builder-creatable-select-input'),",
    "open: selectize.isOpen, focused: selectize.isFocused,",
    "ignoreFocus: selectize.ignoreFocus, activeClass: document.activeElement.className,",
    "activeId: document.activeElement.id}; })()"
  ))
  organism_click_info <- jsonlite::toJSON(
    organism_click_state,
    auto_unbox = TRUE
  )
  expect_true(organism_click_state$open, info = organism_click_info)
  app$run_js(paste0(
    "const input = document.querySelector('.builder-creatable-select-input');",
    "input.value = '  Danio rerio  '; input.dispatchEvent(new Event('input', {bubbles: true}));",
    "input.dispatchEvent(new KeyboardEvent('keydown', {key: 'Enter', bubbles: true}));"
  ))
  app$wait_for_js(
    "document.getElementById('core-organism').value === 'Danio rerio'",
    timeout = 10000
  )
  organism_rendered_value <- app$get_js(paste0(
    "(() => { const root = document.querySelector('.builder-field--organism');",
    "const item = root.querySelector('.selectize-input .item');",
    "const options = Array.from(root.querySelectorAll('.selectize-dropdown .option'));",
    "return {item: item && item.textContent.trim(),",
    "hasUndefined: options.some(option => option.textContent.trim() === 'undefined')}; })()"
  ))
  expect_identical(organism_rendered_value$item, "Danio rerio")
  expect_false(organism_rendered_value$hasUndefined)
  expect_true(app$get_js(paste0(
    "document.activeElement === document.querySelector(",
    "'.builder-field--organism .selectize-input input')"
  )))

  app$run_js(paste0(
    "window.scrollTo(0, 0);",
    "const context = document.querySelector('.dataset-context');",
    "context.classList.add('is-multiple');",
    "context.insertAdjacentHTML('afterend', `",
    "<nav class=\"dataset-compact-review\" aria-hidden=\"true\" ",
    "aria-label=\"Compact dataset review navigation\">",
    "<button class=\"dataset-compact-step\" type=\"button\">Previous</button>",
    "<div class=\"dataset-compact-track\">",
    "<button class=\"dataset-compact-segment is-current\" data-dataset-id=\"a\" type=\"button\">A</button>",
    "<button class=\"dataset-compact-segment\" data-dataset-id=\"b\" type=\"button\">B</button>",
    "</div><button class=\"dataset-compact-step\" type=\"button\">Next</button>",
    "</nav>`);"
  ))
  app$wait_for_js(
    "document.querySelector('.dataset-compact-review') !== null",
    timeout = 10000
  )
  expect_identical(
    app$get_js(
      "document.querySelector('.dataset-compact-review').getAttribute('aria-hidden')"
    ),
    "true"
  )
  app$run_js(paste0(
    "const context = document.querySelector('.dataset-context');",
    "const topbar = document.querySelector('.topbar');",
    "window.scrollBy(0, context.getBoundingClientRect().bottom - ",
    "topbar.getBoundingClientRect().bottom + 24);"
  ))
  app$wait_for_js(
    "document.querySelector('.dataset-compact-review').classList.contains('is-visible')",
    timeout = 10000
  )
  compact_geometry <- app$get_js(paste0(
    "(() => { const rect = document.querySelector('.dataset-compact-review').getBoundingClientRect();",
    "return {documentWidth: document.documentElement.scrollWidth,",
    "viewportWidth: window.innerWidth, left: rect.left, right: rect.right}; })()"
  ))
  expect_lte(compact_geometry$documentWidth, compact_geometry$viewportWidth + 1)
  expect_gte(compact_geometry$left, -1)
  expect_lte(compact_geometry$right, compact_geometry$viewportWidth + 1)

  app$run_js(paste0(
    "window.__compactSelections = []; window.__compactSetInput = Shiny.setInputValue;",
    "Shiny.setInputValue = function(name, value, options) {",
    "if (name === 'review_compact_dataset') { window.__compactSelections.push(value); return; }",
    "return window.__compactSetInput.apply(this, arguments); };",
    "document.querySelector('.dataset-compact-segment:not(.is-current)').click();"
  ))
  compact_selections <- app$get_js("window.__compactSelections")
  expect_length(compact_selections, 1L)
  expect_true(nzchar(compact_selections[[1L]]$id))
  app$run_js(paste0(
    "Shiny.setInputValue = window.__compactSetInput;",
    "delete window.__compactSetInput; delete window.__compactSelections;"
  ))

  app$run_js(paste0(
    "window.__builderFocusDatasetContext(",
    "document.querySelector('.dataset-context'));"
  ))
  app$wait_for_js(
    paste0(
      "Math.abs(document.querySelector('.dataset-context').getBoundingClientRect().top - ",
      "(document.querySelector('.topbar').getBoundingClientRect().bottom + 12)) <= 2"
    ),
    timeout = 10000
  )
  aligned_geometry <- app$get_js(paste0(
    "(() => ({contextTop: document.querySelector('.dataset-context').getBoundingClientRect().top,",
    "topbarBottom: document.querySelector('.topbar').getBoundingClientRect().bottom}))()"
  ))
  expect_gte(aligned_geometry$contextTop, aligned_geometry$topbarBottom + 10)
  expect_lte(aligned_geometry$contextTop, aligned_geometry$topbarBottom + 14)

  cat(
    "\nBuilder responsive geometry: ",
    jsonlite::toJSON(
      geometries[c("1920", "768", "390")],
      auto_unbox = TRUE
    ),
    "\n",
    sep = ""
  )
})
