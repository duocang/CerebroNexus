library(shinytest2)

test_that("staged workflow remains focused and overflow-free", {
  skip_if_not(identical(Sys.getenv("CEREBRO_RUN_BROWSER_TESTS"), "true"))
  app_dir <- builder_profile_inst_path("builder")
  local_app_support(app_dir)

  for (viewport in list(c(1920L, 1080L), c(768L, 1024L), c(390L, 844L))) {
    app <- AppDriver$new(
      app_dir,
      name = paste0("builder_staged_", viewport[[1]]),
      width = viewport[[1]],
      height = viewport[[2]],
      load_timeout = 60000
    )
    on.exit(app$stop(), add = TRUE)
    app$wait_for_idle(timeout = 30000)
    expect_identical(
      app$get_js(
        "document.querySelectorAll('[data-workflow-stage=upload]').length"
      ),
      1L
    )
    expect_false(app$get_js(
      "!!document.querySelector('#continue_to_review, .actionbar, [data-workflow-stage=build]')"
    ))
    app$click(selector = ".example-btn[data-ex=all_content]")
    app$wait_for_js(
      "document.getElementById('continue_to_review') !== null",
      timeout = 60000
    )
    expect_identical(
      app$get_js("document.querySelectorAll('#continue_to_review').length"),
      1L
    )
    expect_lte(
      app$get_js("document.documentElement.scrollWidth"),
      viewport[[1]] + 1L
    )

    app$click("continue_to_review")
    app$wait_for_js(
      "document.activeElement === document.querySelector('[data-workflow-stage=review] h2')",
      timeout = 10000
    )
    expect_identical(
      app$get_js("document.querySelectorAll('#confirm_review').length"),
      1L
    )
    expect_identical(
      app$get_js("document.querySelectorAll('#back_to_settings').length"),
      1L
    )
    expect_false(app$get_js(
      "!!document.querySelector('[data-workflow-stage=review] input:not([type=hidden]), [data-workflow-stage=review] select, [data-workflow-stage=review] textarea')"
    ))

    app$click("confirm_review")
    app$wait_for_js(
      "document.activeElement === document.querySelector('[data-workflow-stage=build] h2')",
      timeout = 10000
    )
    expect_identical(
      app$get_js("document.querySelectorAll('#build-stage-status').length"),
      1L
    )
    expect_lte(
      app$get_js("document.documentElement.scrollWidth"),
      viewport[[1]] + 1L
    )

    app$click("back_to_review")
    app$wait_for_js(
      "document.activeElement === document.querySelector('[data-workflow-stage=review] h2')",
      timeout = 10000
    )
    app$click("back_to_settings")
    app$wait_for_js(
      "document.activeElement === document.querySelector('[data-workflow-stage=configure] h2')",
      timeout = 10000
    )
    app$set_inputs(`core-name` = paste0("Accepted ", viewport[[1]]))
    app$wait_for_idle(timeout = 10000)
    expect_false(app$get_js(
      "!!document.querySelector('[data-workflow-stage=build]')"
    ))
    expect_identical(
      app$get_js("document.getElementById('core-name').value"),
      paste0("Accepted ", viewport[[1]])
    )
    app$click("continue_to_review")
    app$wait_for_js(
      "document.getElementById('confirm_review') !== null",
      timeout = 10000
    )
    app$click("confirm_review")
    app$wait_for_js(
      "document.activeElement === document.querySelector('[data-workflow-stage=build] h2')",
      timeout = 10000
    )
    expect_identical(
      app$get_js("document.querySelectorAll('#build-stage-status').length"),
      1L
    )
    app$stop()
  }
})
