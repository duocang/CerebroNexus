generated_app_e2e_reset_runtime()

test_that("Linked views renders both retained spatial sections and images", {
  fixture <- generated_app_e2e_select_dataset("spatial")
  expect_identical(
    generated_app_e2e_visible_pages(),
    fixture$expected$visible_pages
  )
  generated_app_e2e_open_linked_views(fixture)
  driver <- generated_app_e2e_driver()

  driver$wait_for_js(
    "document.getElementById('cv-pick-spatial').selectize != null",
    timeout = 60000
  )
  expect_identical(
    unname(unlist(
      driver$get_js(paste0(
        "Object.keys(document.getElementById('cv-pick-spatial')",
        ".selectize.options)"
      )),
      use.names = FALSE
    )),
    fixture$expected$spatial_sections
  )

  sections_js <- .generated_app_e2e_js_array(
    fixture$expected$spatial_sections
  )
  driver$run_js(paste0(
    "document.getElementById('cv-pick-spatial').selectize.setValue(",
    sections_js,
    ");"
  ))
  driver$wait_for_js(
    paste0(
      "document.querySelectorAll(",
      "'.cv-pane:not(.cv-hidden) .cv-ptitle').length>=3"
    ),
    timeout = 60000
  )
  titles <- generated_app_e2e_linked_titles()
  for (section in fixture$expected$spatial_sections) {
    expect_true(any(grepl(section, titles, fixed = TRUE)), info = section)
  }

  ## Each FOV owns its own embedded image. Activate its background scope and
  ## assert that the compatibility picker exposes one embedded image;
  ## this exercises the generated CRB -> bundle -> Linked views image contract.
  for (section in fixture$expected$spatial_sections) {
    section_js <- .generated_app_e2e_js_value(section)
    driver$run_js(paste0(
      "(function(){var tab=Array.from(document.querySelectorAll(",
      "'.cv-bg-space-tab')).find(function(node){return ",
      "(node.textContent||'').trim()===",
      section_js,
      ";});if(tab)tab.click();})()"
    ))
    expected_label <- "Embedded tissue image"
    expected_label_js <- .generated_app_e2e_js_value(expected_label)
    driver$wait_for_js(
      paste0(
        "(function(){var picker=document.getElementById('cv-img-pick');",
        "return !!(picker && Array.from(picker.options).some(function(option){",
        "return option.textContent===",
        expected_label_js,
        ";}));})()"
      ),
      timeout = 60000
    )
    labels <- unname(unlist(
      driver$get_js(
        "Array.from(document.getElementById('cv-img-pick').options).map(function(option){return option.textContent;})"
      ),
      use.names = FALSE
    ))
    expect_true(expected_label %in% labels, info = section)
    expect_identical(length(labels), 2L, info = section)
  }

  expect_identical(
    driver$get_js(
      "document.querySelector(\"a[href='#shiny-tab-spatial']\") !== null"
    ),
    FALSE
  )
  generated_app_e2e_expect_clean_browser()
})
