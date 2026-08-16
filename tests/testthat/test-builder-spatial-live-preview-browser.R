test_that("real Ion drag is rendered locally on the next animation frame", {
  skip_if_not_installed("shinytest2")
  root <- testthat::test_path("..", "..", "inst", "builder")
  app_dir <- withr::local_tempdir()
  dir.create(file.path(app_dir, "www"))
  file.copy(
    file.path(root, "www", "builder-spatial-canvas.js"),
    file.path(app_dir, "www", "builder-spatial-canvas.js")
  )
  writeLines(
    c(
      "library(shiny)",
      "point_n <- 4000L",
      "point_angle <- seq(0, 16*pi, length.out=point_n)",
      "point_radius <- seq(.05, 1, length.out=point_n)",
      "ui <- fluidPage(tags$head(tags$script(src='builder-spatial-canvas.js')),",
      "  sliderInput('enhance-coordinate_rotation', 'Rotation', -180, 180, -90, step=.1),",
      "  tags$canvas(id='enhance-alignment_spatial_plot', class='builder-spatial-canvas',",
      "    style='display:block;width:600px;height:400px'),",
      "  tags$div(id='enhance-alignment_spatial_plot-tooltip', hidden='hidden'),",
      "  tags$p(id='enhance-alignment_spatial_plot-summary'), textOutput('server_changes'))",
      "server <- function(input, output, session) {",
      "  changes <- reactiveVal(0L)",
      "  observeEvent(input[['enhance-coordinate_rotation']], changes(changes()+1L), ignoreInit=TRUE)",
      "  output$server_changes <- renderText(changes())",
      "  session$onFlushed(function() { session$sendCustomMessage('builder_spatial_canvas_scene', list(",
      "    available=TRUE, viewKey='test', generation=1L, resetToken=1L, capped=FALSE,",
      "    points=list(x=point_radius*cos(point_angle), y=point_radius*sin(point_angle),",
      "      barcode=paste0('cell-',seq_len(point_n)), group=rep(c('A','B'),length.out=point_n),",
      "      color=rep(c('#f00','#00f'),length.out=point_n), count=rep(1L,point_n)),",
      "    bounds=list(xmin=-1,xmax=1,ymin=-1,ymax=1), image=NULL,",
      "    controls=list(coordinateRotation=-90, dx=0,dy=0,scale=1,rotation=0,",
      "      flip_x=FALSE,flip_y=FALSE,image_opacity=.8,point_opacity=.85,point_size=5)",
      "  )) }, once=TRUE)",
      "}",
      "shinyApp(ui, server)"
    ),
    file.path(app_dir, "app.R")
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    name = "builder_spatial_canvas_drag",
    width = 900,
    height = 700,
    load_timeout = 30000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_js("window.__builderSpatialCanvasMetrics.sceneMessages === 1")
  viewport <- app$get_js("window.__builderSpatialCanvasMetrics.latestViewport")
  expect_equal(viewport$centerX, 300, tolerance = 1)
  expect_equal(viewport$centerY, 200, tolerance = 1)
  track <- app$get_js(paste0(
    "(() => { const t=document.querySelector('.irs-line');",
    "const r=t.getBoundingClientRect(); return {left:r.left,right:r.right,y:r.top+r.height/2}; })()"
  ))
  handle <- app$get_js(paste0(
    "(() => { const h=document.querySelector('.irs-handle');",
    "const r=h.getBoundingClientRect(); return {x:r.left+r.width/2,y:r.top+r.height/2}; })()"
  ))
  app$run_js(paste0(
    "window.__builderSpatialCanvasMetrics.eventToRenderMs=[];",
    "window.__builderSpatialCanvasMetrics.renderTimes=[];",
    "window.__builderSpatialCanvasMetrics.longTasks=0;"
  ))
  chrome <- app$get_chromote_session()
  start_x <- handle$x
  end_x <- track$left + (track$right - track$left) * 0.75
  chrome$Input$dispatchMouseEvent(
    type = "mousePressed",
    x = start_x,
    y = handle$y,
    button = "left",
    buttons = 1,
    clickCount = 1
  )
  for (step in seq_len(120L)) {
    chrome$Input$dispatchMouseEvent(
      type = "mouseMoved",
      x = start_x + (end_x - start_x) * step / 120,
      y = handle$y,
      button = "left",
      buttons = 1
    )
    Sys.sleep(1 / 60)
  }
  app$wait_for_js(
    "window.__builderSpatialCanvasMetrics.latestCoordinateRotation > 80"
  )
  during <- app$get_js("window.__builderSpatialCanvasMetrics")
  expect_identical(app$get_value(output = "server_changes"), "0")
  chrome$Input$dispatchMouseEvent(
    type = "mouseReleased",
    x = end_x,
    y = handle$y,
    button = "left",
    clickCount = 1
  )
  app$wait_for_js(
    "document.getElementById('server_changes').textContent.trim() === '1'"
  )
  Sys.sleep(.1)

  expect_gt(during$renders, 1)
  expect_gt(during$latestCoordinateRotation, 80)
  expect_equal(
    during$latestViewport$centerX,
    viewport$centerX,
    tolerance = 1e-9
  )
  expect_equal(
    during$latestViewport$centerY,
    viewport$centerY,
    tolerance = 1e-9
  )
  expect_equal(during$latestViewport$scale, viewport$scale, tolerance = 1e-9)
  expect_identical(during$sceneMessages, 1L)
  expect_identical(app$get_value(output = "server_changes"), "1")
  metrics <- app$get_js("window.__builderSpatialCanvasMetrics")
  expect_gte(length(metrics$eventToRenderMs), 60L)
  expect_lte(unname(stats::quantile(unlist(metrics$eventToRenderMs), .95)), 34)
  expect_identical(metrics$longTasks, 0L)
})
