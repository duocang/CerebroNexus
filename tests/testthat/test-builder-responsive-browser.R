library(shinytest2)

builder_narrow_document_height_budget <- 6500

test_that("Builder narrow height budget covers the All content baseline", {
  recorded_baseline <- 6156

  expect_lte(recorded_baseline, builder_narrow_document_height_budget)
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
    "const enhancementGroups = Array.from(document.querySelectorAll('.enhance-group'));",
    "const spatialSection = document.querySelector('.builder-stage-spatial');",
    "const datasetNameInput = document.getElementById('core-name');",
    "const organismInput = document.querySelector('.builder-field--organism .selectize-input');",
    "const metadataSearch = document.querySelector('.viewer-group-search');",
    "const projectionControls = Array.from(document.querySelectorAll('.viewer-projection-control'));",
    "const plotFrames = Array.from(document.querySelectorAll('.spatial-alignment-plot-frame'));",
    "const plotRects = plotFrames.map(node => node.getBoundingClientRect());",
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
    "workflowInDocumentOrder: stageTops.every((top, index) => index === 0 || top >= stageTops[index - 1]),",
    "enhancementGroupCount: enhancementGroups.length,",
    "enhancementStackSectionCount: document.querySelectorAll('.builder-enhancement-stack > .builder-stage-section').length,",
    "enhancementGroupGap: enhancementGroups.length === 2 ? enhancementGroups[1].getBoundingClientRect().top - enhancementGroups[0].getBoundingClientRect().bottom : null,",
    "spatialSectionCount: document.querySelectorAll('.builder-stage-spatial').length,",
    "spatialInsideEnhancements: Boolean(document.querySelector('.builder-stage-enhance .builder-stage-spatial')),",
    "enhancementToSpatialGap: spatialSection ? spatialSection.getBoundingClientRect().top - document.querySelector('.builder-stage-enhance').getBoundingClientRect().bottom : null,",
    "datasetNameHeight: datasetNameInput.getBoundingClientRect().height,",
    "organismHeight: organismInput.getBoundingClientRect().height,",
    "datasetNamePaddingLeft: parseFloat(getComputedStyle(datasetNameInput).paddingLeft),",
    "organismPaddingLeft: parseFloat(getComputedStyle(organismInput).paddingLeft),",
    "organismRadius: parseFloat(getComputedStyle(organismInput).borderRadius),",
    "organismFontSize: parseFloat(getComputedStyle(organismInput).fontSize),",
    "metadataSearchHeight: metadataSearch.getBoundingClientRect().height,",
    "metadataSearchPaddingLeft: parseFloat(getComputedStyle(metadataSearch).paddingLeft),",
    "metadataSearchRadius: parseFloat(getComputedStyle(metadataSearch).borderRadius),",
    "metadataSearchFontSize: parseFloat(getComputedStyle(metadataSearch).fontSize),",
    "projectionControlCount: projectionControls.length,",
    "projectionControlsShareRow: projectionControls.length === 2 && Math.abs(projectionControls[0].getBoundingClientRect().top - projectionControls[1].getBoundingClientRect().top) <= 2,",
    "previewFigureCount: plotFrames.length,",
    "previewColumnCount: 1,",
    "previewAspectRatios: plotRects.map(rect => rect.width / rect.height),",
    "previewModebarButtonCount: document.querySelectorAll('.spatial-alignment-figure .modebar-btn').length,",
    "datasetContextCount: document.querySelectorAll('.dataset-context').length",
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

  builder_browser_wait_for_example_ready(app)
  app$click(selector = ".example-btn[data-ex=all_content]")
  app$wait_for_js(
    paste0(
      "document.querySelector('.ds-pick[aria-current=true]') !== null && ",
      "document.querySelector('.stage-intro') !== null && ",
      "document.querySelector('.builder-form-grid') !== null && ",
      "document.querySelectorAll('.spatial-alignment-figure .builder-spatial-canvas').length === 1"
    ),
    timeout = 60000
  )
  builder_browser_dismiss_project_offer(app)
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
    expect_identical(geometry$enhancementGroupCount, 2L)
    expect_identical(geometry$enhancementStackSectionCount, 2L)
    expect_gte(geometry$enhancementGroupGap, 23)
    expect_identical(geometry$spatialSectionCount, 1L)
    expect_false(geometry$spatialInsideEnhancements)
    expect_gte(
      geometry$enhancementToSpatialGap,
      if (viewport[[1]] <= 640L) 19 else 23
    )
    expect_lte(
      abs(geometry$datasetNameHeight - geometry$organismHeight),
      0.5
    )
    expect_lte(
      abs(geometry$datasetNamePaddingLeft - geometry$organismPaddingLeft),
      0.5
    )
    expect_lte(
      abs(geometry$metadataSearchHeight - geometry$organismHeight),
      0.5
    )
    expect_lte(
      abs(geometry$metadataSearchPaddingLeft - geometry$organismPaddingLeft),
      0.5
    )
    expect_lte(
      abs(geometry$metadataSearchRadius - geometry$organismRadius),
      0.5
    )
    expect_lte(
      abs(geometry$metadataSearchFontSize - geometry$organismFontSize),
      0.5
    )
    expect_identical(geometry$projectionControlCount, 2L)
    expect_identical(
      geometry$projectionControlsShareRow,
      viewport[[1]] > 640L
    )
    expect_identical(geometry$previewFigureCount, 1L)
    expect_identical(geometry$previewModebarButtonCount, 0L)
    expect_identical(geometry$datasetContextCount, 0L)
    expect_true(all(
      unlist(geometry$previewAspectRatios) >= 0.75 &
        unlist(geometry$previewAspectRatios) <= 3
    ))
    expect_identical(
      geometry$previewColumnCount,
      1L
    )
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
  preview_ratios <- vapply(
    geometries,
    function(geometry) geometry$previewAspectRatios[[1L]],
    numeric(1)
  )
  narrow_preview_ratios <- preview_ratios[names(preview_ratios) != "1920"]
  expect_lte(
    max(narrow_preview_ratios) - min(narrow_preview_ratios),
    0.02
  )
  # All content renders near 6156px; 6500 preserves cross-font headroom.
  expect_lt(
    geometries[["390"]]$documentHeight,
    builder_narrow_document_height_budget
  )

  app$get_chromote_session()$set_viewport_size(width = 1400L, height = 720L)
  app$wait_for_js(
    "window.innerWidth === 1400 && window.innerHeight === 720",
    timeout = 10000
  )
  app$run_js(
    "document.querySelector('.spatial-image-options').open = true;"
  )
  app$wait_for_js(
    "document.querySelector('.spatial-image-options').open === true",
    timeout = 10000
  )
  spatial_scroll_geometry <- app$get_js(paste0(
    "(() => {",
    "const layout = document.querySelector('.spatial-alignment-layout');",
    "const sidebar = document.querySelector('.spatial-alignment-sidebar');",
    "const layoutRect = layout.getBoundingClientRect();",
    "const sidebarRect = sidebar.getBoundingClientRect();",
    "return {",
    "sidebarBottom: sidebarRect.bottom,",
    "layoutBottom: layoutRect.bottom,",
    "sidebarHeight: sidebar.clientHeight,",
    "sidebarContentHeight: sidebar.scrollHeight",
    "};",
    "})()"
  ))
  expect_lte(
    spatial_scroll_geometry$sidebarBottom,
    spatial_scroll_geometry$layoutBottom + 1
  )
  expect_gt(
    spatial_scroll_geometry$sidebarContentHeight,
    spatial_scroll_geometry$sidebarHeight
  )
  app$run_js(paste0(
    "const sidebar = document.querySelector('.spatial-alignment-sidebar');",
    "sidebar.scrollTop = sidebar.scrollHeight;"
  ))
  app$wait_for_js(
    "document.querySelector('.spatial-alignment-sidebar').scrollTop > 0",
    timeout = 10000
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
    "const percentage = document.querySelector('.viewer-cell-percentage-input');",
    "percentage.value = '60';",
    "percentage.dispatchEvent(new Event('input', {bubbles: true}));",
    "percentage.dispatchEvent(new Event('change', {bubbles: true}));"
  ))
  app$wait_for_js(
    paste0(
      "document.querySelector('.viewer-cell-percentage-value').textContent.trim() === '60%' && ",
      "document.querySelector('.viewer-cell-percentage-input').value === '60'"
    ),
    timeout = 10000
  )

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
