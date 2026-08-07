generated_app_e2e_reset_runtime()

test_that("spatial page switches between both retained sections", {
  fixture <- generated_app_e2e_select_dataset("spatial")
  generated_app_e2e_activate_tab("spatial")
  generated_app_e2e_wait_input("spatial_projection_to_display")

  expect_identical(
    generated_app_e2e_value("input", "spatial_projection_to_display"),
    "section_a"
  )

  for (section in fixture$expected$spatial_sections) {
    generated_app_e2e_set_input("spatial_projection_to_display", section)
    generated_app_e2e_wait_input("spatial_projection_background_image")
    expect_identical(
      generated_app_e2e_value("input", "spatial_projection_background_image"),
      "__embedded__",
      info = section
    )

    generated_app_e2e_wait_plotly("spatial_projection")
    expect_gt(
      generated_app_e2e_plotly_point_count("spatial_projection"),
      0L
    )

    bounds <- fixture$expected$image_alignment[[section]]$bounds
    expected <- unlist(bounds, use.names = FALSE)
    expected_js <- .generated_app_e2e_js_value(as.character(expected))
    generated_app_e2e_driver()$wait_for_js(
      paste0(
        "(function(){var bg=document.getElementById(",
        .generated_app_e2e_js_value("spatial_projection_background"),
        ");if(!bg)return false;var expected=",
        expected_js,
        ";return [bg.dataset.boundsXmin,bg.dataset.boundsXmax,",
        "bg.dataset.boundsYmin,bg.dataset.boundsYmax].every(",
        "function(value,index){return value===expected[index];});})()"
      ),
      timeout = 60000
    )
    observed <- generated_app_e2e_driver()$get_js(
      paste0(
        "(function(){var bg=document.getElementById(",
        .generated_app_e2e_js_value("spatial_projection_background"),
        ");if(!bg)return null;return [bg.dataset.boundsXmin,",
        "bg.dataset.boundsXmax,bg.dataset.boundsYmin,",
        "bg.dataset.boundsYmax];})()"
      )
    )
    expect_equal(
      as.numeric(unlist(observed, use.names = FALSE)),
      as.numeric(expected),
      info = section
    )

    image_state <- generated_app_e2e_driver()$get_js(
      paste0(
        "(function(){var bg=document.getElementById(",
        .generated_app_e2e_js_value("spatial_projection_background"),
        ");if(!bg)return null;return {source:bg.dataset.backgroundImage||'',",
        "display:getComputedStyle(bg).display};})()"
      )
    )
    expect_match(as.character(image_state$source), "^data:image/png;base64,")
    expect_false(identical(as.character(image_state$display), "none"))
  }
  generated_app_e2e_expect_clean_browser()
})
