library(shinytest2)

test_that("Builder spatial sliders update the visible preview on the next frame", {
  app_dir <- builder_profile_inst_path("builder")
  local_app_support(app_dir)
  app <- AppDriver$new(
    app_dir,
    name = "builder_spatial_live_preview",
    width = 1280,
    height = 900,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)
  app$wait_for_js(
    "document.querySelector('.example-btn[data-ex=all_content]') !== null",
    timeout = 10000
  )
  app$click(selector = ".example-btn[data-ex=all_content]")
  app$wait_for_js(
    "document.getElementById('enhance-tissue_image_file') !== null",
    timeout = 60000
  )
  image_path <- tempfile("builder-live-preview-", fileext = ".png")
  png::writePNG(
    array(rep(c(0.1, 0.45, 0.85), each = 24L * 40L), c(24L, 40L, 3L)),
    image_path
  )
  withr::defer(unlink(image_path, force = TRUE))
  app$upload_file(`enhance-tissue_image_file` = image_path)
  app$wait_for_js(
    paste0(
      "document.querySelector('#enhance-alignment_spatial_plot image') !== null && ",
      "document.getElementById('enhance-img_dx') !== null"
    ),
    timeout = 30000
  )
  app$wait_for_idle(timeout = 30000)

  baseline <- app$get_js(paste0(
    "(() => {",
    "const image = document.querySelector('#enhance-alignment_spatial_plot image');",
    "const plot = document.getElementById('enhance-alignment_spatial_plot');",
    "return {x: Number(image.getAttribute('x')), transform: image.getAttribute('transform') || '',",
    "pointOpacity: Number(plot.data[0].marker.opacity), pointSize: Number(plot.data[0].marker.size)};",
    "})()"
  ))
  app$run_js(paste0(
    "(() => { const input = document.getElementById('enhance-img_dx');",
    "input.value = String(Number(input.value) + 10);",
    "input.dispatchEvent(new Event('input', {bubbles: true})); })();"
  ))
  app$wait_for_js(
    sprintf(
      "Number(document.querySelector('#enhance-alignment_spatial_plot image').getAttribute('x')) !== %.12f",
      baseline$x
    ),
    timeout = 1000
  )

  app$run_js(paste0(
    "(() => { const input = document.getElementById('enhance-img_rotate');",
    "input.value = '23'; input.dispatchEvent(new Event('input', {bubbles: true})); })();"
  ))
  app$wait_for_js(
    "(document.querySelector('#enhance-alignment_spatial_plot image').getAttribute('transform') || '').includes('rotate(-23')",
    timeout = 1000
  )

  app$run_js(paste0(
    "(() => { const opacity = document.getElementById('enhance-point_opacity');",
    "opacity.value = '55'; opacity.dispatchEvent(new Event('input', {bubbles: true}));",
    "const size = document.getElementById('enhance-point_size');",
    "size.value = '8'; size.dispatchEvent(new Event('input', {bubbles: true})); })();"
  ))
  app$wait_for_js(
    paste0(
      "document.getElementById('enhance-alignment_spatial_plot').data[0].marker.opacity === 0.55 && ",
      "document.getElementById('enhance-alignment_spatial_plot').data[0].marker.size === 8"
    ),
    timeout = 1000
  )

  expect_false(identical(baseline$pointOpacity, 0.55))
  expect_false(identical(baseline$pointSize, 8))
  builder_expect_clean_browser_logs(app)
})
