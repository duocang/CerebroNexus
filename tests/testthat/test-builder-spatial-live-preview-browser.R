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
      "ui <- fluidPage(tags$head(tags$script(src='builder-spatial-canvas.js')),",
      "  sliderInput('enhance-coordinate_rotation', 'Rotation', -180, 180, 0, step=.1),",
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
      "    points=list(x=c(-1,1), y=c(-1,1), barcode=c('a','b'), group=c('A','B'),",
      "      color=c('#f00','#00f'), count=c(1L,1L)),",
      "    bounds=list(xmin=-1,xmax=1,ymin=-1,ymax=1), image=NULL,",
      "    controls=list(coordinateRotation=0, dx=0,dy=0,scale=1,rotation=0,",
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
  handle <- app$get_js(paste0(
    "(() => { const h=document.querySelector('.irs-handle');",
    "const r=h.getBoundingClientRect(); return {x:r.left+r.width/2,y:r.top+r.height/2}; })()"
  ))
  track <- app$get_js(paste0(
    "(() => { const t=document.querySelector('.irs-line');",
    "const r=t.getBoundingClientRect(); return {x:r.left+r.width*.75,y:r.top+r.height/2}; })()"
  ))
  chrome <- app$get_chromote_session()
  chrome$Input$dispatchMouseEvent(
    type = "mousePressed",
    x = track$x,
    y = track$y,
    button = "left",
    buttons = 1,
    clickCount = 1
  )
  app$wait_for_js(
    "window.__builderSpatialCanvasMetrics.latestCoordinateRotation > 80"
  )
  during <- app$get_js("window.__builderSpatialCanvasMetrics")
  chrome$Input$dispatchMouseEvent(
    type = "mouseReleased",
    x = track$x,
    y = track$y,
    button = "left",
    clickCount = 1
  )

  expect_gt(during$renders, 1)
  expect_gt(during$latestCoordinateRotation, 80)
  expect_identical(during$sceneMessages, 1L)
  expect_identical(app$get_value(output = "server_changes"), "0")
})
