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
  image <- array(0, c(24L, 40L, 3L))
  image[1:12, 1:20, 1] <- 1
  image[1:12, 21:40, 2] <- 1
  image[13:24, 1:20, 3] <- 1
  image[13:24, 21:40, ] <- 0.8
  png::writePNG(image, image_path)
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
  app$run_js(paste0(
    "window.__builderSpatialPlotValues = 0;",
    "window.jQuery(document).on('shiny:value.builderSpatialTest', function(event) {",
    "if (event.target && event.target.id === 'enhance-alignment_spatial_plot') ",
    "window.__builderSpatialPlotValues += 1;",
    "});"
  ))

  baseline <- app$get_js(paste0(
    "(() => {",
    "const image = document.querySelector('#enhance-alignment_spatial_plot image');",
    "const plot = document.getElementById('enhance-alignment_spatial_plot');",
    "const pointIndex = plot.data.findIndex(trace => ",
    "trace.meta && trace.meta.builder_alignment_role === 'points');",
    "const points = plot.data[pointIndex];",
    "return {x: Number(image.getAttribute('x')), transform: image.getAttribute('transform') || '',",
    "pointOpacity: Number(points.marker.opacity), pointSize: Number(points.marker.size),",
    "pointX: Number(points.x[0]), pointY: Number(points.y[0]),",
    "pivotX: Number(plot.layout.meta.builder_alignment_pivot.x),",
    "pivotY: Number(plot.layout.meta.builder_alignment_pivot.y),",
    "coordinateRotation: Number(plot.layout.meta.builder_alignment_rotation)};",
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
    "(() => { const input = document.getElementById('enhance-img_scale');",
    "input.value = '1.6'; input.dispatchEvent(new Event('input', {bubbles: true})); })();"
  ))
  app$wait_for_js(
    paste0(
      "(() => { const plot = document.getElementById('enhance-alignment_spatial_plot');",
      "const image = plot.layout.images[0];",
      "const bounds = plot.layout.meta.builder_image_preview.base_bounds;",
      "const dx = Number(document.getElementById('enhance-img_dx').value);",
      "const dy = Number(document.getElementById('enhance-img_dy').value);",
      "const centreX = (Number(bounds.xmin) + Number(bounds.xmax)) / 2 + dx;",
      "const centreY = (Number(bounds.ymin) + Number(bounds.ymax)) / 2 + dy;",
      "return Math.abs((image.x + image.sizex / 2) - centreX) < 1e-8 && ",
      "Math.abs((image.y - image.sizey / 2) - centreY) < 1e-8; })()"
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
    "(() => { const input = document.getElementById('enhance-image_flip_x');",
    "input.checked = true; input.dispatchEvent(new Event('change', {bubbles: true})); })();"
  ))
  app$wait_for_js(
    "(document.querySelector('#enhance-alignment_spatial_plot image').getAttribute('transform') || '').includes('scale(-1 1)')",
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
      "(() => { const plot = document.getElementById('enhance-alignment_spatial_plot');",
      "const trace = plot.data.find(item => item.meta && ",
      "item.meta.builder_alignment_role === 'points');",
      "return trace.marker.opacity === 0.55 && trace.marker.size === 8; })()"
    ),
    timeout = 1000
  )

  target_rotation <- 42
  angle <- (target_rotation - baseline$coordinateRotation) * pi / 180
  dx <- baseline$pointX - baseline$pivotX
  dy <- baseline$pointY - baseline$pivotY
  expected_x <- baseline$pivotX + dx * cos(angle) - dy * sin(angle)
  expected_y <- baseline$pivotY + dx * sin(angle) + dy * cos(angle)
  app$run_js(paste0(
    "(() => {",
    "const rotation = document.getElementById('enhance-coordinate_rotation');",
    "[5, 15, -20, 42].forEach(value => { rotation.value = String(value);",
    "rotation.dispatchEvent(new Event('input', {bubbles: true})); });",
    "})();"
  ))
  app$wait_for_js(
    sprintf(
      paste0(
        "(() => { const plot = document.getElementById('enhance-alignment_spatial_plot');",
        "const trace = plot.data.find(item => item.meta && ",
        "item.meta.builder_alignment_role === 'points');",
        "return Math.abs(Number(trace.x[0]) - %.15f) < 1e-8 && ",
        "Math.abs(Number(trace.y[0]) - %.15f) < 1e-8; })()"
      ),
      expected_x,
      expected_y
    ),
    timeout = 1000
  )

  draft_inputs <- app$get_js(paste0(
    "(() => ({",
    "dx: Number(document.getElementById('enhance-img_dx').value),",
    "dy: Number(document.getElementById('enhance-img_dy').value),",
    "scale: Number(document.getElementById('enhance-img_scale').value)",
    "}))()"
  ))
  app$set_inputs(
    `enhance-coordinate_rotation` = target_rotation,
    `enhance-img_dx` = draft_inputs$dx,
    `enhance-img_dy` = draft_inputs$dy,
    `enhance-img_scale` = draft_inputs$scale,
    `enhance-img_rotate` = 23,
    `enhance-image_flip_x` = TRUE,
    `enhance-point_opacity` = 55,
    `enhance-point_size` = 8,
    timeout_ = 3000,
    wait_ = FALSE
  )
  app$wait_for_idle(timeout = 30000)
  expect_equal(app$get_js("window.__builderSpatialPlotValues"), 0)
  expect_true(app$get_js(
    "document.getElementById('enhance-coordinate_scale') === null"
  ))

  live_coordinate <- app$get_js(paste0(
    "(() => { const plot = document.getElementById('enhance-alignment_spatial_plot');",
    "const trace = plot.data.find(item => item.meta && ",
    "item.meta.builder_alignment_role === 'points');",
    "return {x: Array.from(trace.x), y: Array.from(trace.y)}; })()"
  ))
  app$click("enhance-save_coordinate_transform")
  app$wait_for_idle(timeout = 30000)
  app$wait_for_js(
    paste0(
      "(() => { const plot = document.getElementById('enhance-alignment_spatial_plot');",
      "const meta = plot && plot.layout && plot.layout.meta;",
      "return meta && Number(meta.builder_alignment_rotation) === 42 && ",
      "Number(meta.builder_alignment_scale) === 1; })()"
    ),
    timeout = 30000
  )
  saved_coordinate <- app$get_js(paste0(
    "(() => { const plot = document.getElementById('enhance-alignment_spatial_plot');",
    "const trace = plot.data.find(item => item.meta && ",
    "item.meta.builder_alignment_role === 'points');",
    "return {x: Array.from(trace.x), y: Array.from(trace.y)}; })()"
  ))
  expect_equal(saved_coordinate$x, live_coordinate$x, tolerance = 1e-8)
  expect_equal(saved_coordinate$y, live_coordinate$y, tolerance = 1e-8)
  restored_image <- app$get_js(paste0(
    "(() => { const image = document.querySelector(",
    "'#enhance-alignment_spatial_plot image');",
    "return {transform: image ? image.getAttribute('transform') || '' : '',",
    "rotation: Number(document.getElementById('enhance-img_rotate').value),",
    "flipX: document.getElementById('enhance-image_flip_x').checked}; })()"
  ))
  expect_match(restored_image$transform, "rotate(-23", fixed = TRUE)
  expect_match(restored_image$transform, "scale(-1 1)", fixed = TRUE)
  expect_equal(restored_image$rotation, 23)
  expect_true(restored_image$flipX)

  live_alignment <- app$get_js(paste0(
    "(() => { const plot = document.getElementById('enhance-alignment_spatial_plot');",
    "const image = plot.layout.images[0];",
    "const node = plot.querySelector('image');",
    "const trace = plot.data.find(item => item.meta && ",
    "item.meta.builder_alignment_role === 'points');",
    "return {x: image.x, y: image.y, sizex: image.sizex, sizey: image.sizey,",
    "transform: node.getAttribute('transform') || '', opacity: image.opacity,",
    "pointOpacity: trace.marker.opacity, pointSize: trace.marker.size}; })()"
  ))
  plot_values_before_save <- app$get_js("window.__builderSpatialPlotValues")
  app$click("enhance-apply_align")
  app$wait_for_js(
    sprintf(
      "window.__builderSpatialPlotValues > %d",
      as.integer(plot_values_before_save)
    ),
    timeout = 30000
  )
  app$wait_for_idle(timeout = 30000)
  saved_alignment <- app$get_js(paste0(
    "(() => { const plot = document.getElementById('enhance-alignment_spatial_plot');",
    "const image = plot.layout.images[0];",
    "const node = plot.querySelector('image');",
    "const trace = plot.data.find(item => item.meta && ",
    "item.meta.builder_alignment_role === 'points');",
    "return {x: image.x, y: image.y, sizex: image.sizex, sizey: image.sizey,",
    "transform: node.getAttribute('transform') || '', opacity: image.opacity,",
    "pointOpacity: trace.marker.opacity, pointSize: trace.marker.size}; })()"
  ))
  expect_equal(saved_alignment, live_alignment, tolerance = 1e-8)

  expect_false(identical(baseline$pointOpacity, 0.55))
  expect_false(identical(baseline$pointSize, 8))
  builder_expect_clean_browser_logs(app)
})
