library(shinytest2)

sys.source(
  builder_profile_inst_path("builder", "spatial.R"),
  envir = environment()
)
sys.source(
  builder_profile_inst_path("builder", "extras.R"),
  envir = environment()
)

test_that("saved quarter-turn image pixels match the live Canvas transform", {
  skip_if_not_installed("png")
  skip_if_not_installed("base64enc")
  root <- builder_profile_inst_path("builder")
  app_dir <- withr::local_tempdir()
  dir.create(file.path(app_dir, "www"))
  file.copy(
    file.path(root, "www", "builder-spatial-canvas.js"),
    file.path(app_dir, "www", "builder-spatial-canvas.js")
  )

  image <- array(0, dim = c(20L, 40L, 4L))
  image[,, 4L] <- 1
  image[seq_len(10L), seq_len(20L), 1L] <- 1
  image[seq_len(10L), 21:40, 2L] <- 1
  image[11:20, seq_len(20L), 3L] <- 1
  image[11:20, 21:40, 1:2] <- 1
  raw <- builder_encode_image(image, max_px = 40L)
  saved <- builder_encode_image(
    image,
    max_px = 40L,
    flip_x = TRUE,
    rotate = 90
  )
  base_bounds <- list(xmin = 0, xmax = 40, ymin = 0, ymax = 20)
  saved_bounds <- builder_adjust_bounds(
    builder_alignment_oriented_bounds(base_bounds, saved),
    dx = 5,
    dy = -3,
    scale = 1.2
  )
  bounds_r <- function(bounds) {
    paste(sprintf("%s=%s", names(bounds), unlist(bounds)), collapse = ",")
  }

  writeLines(
    c(
      "library(shiny)",
      sprintf("raw_uri <- %s", deparse(raw$uri)),
      sprintf("saved_uri <- %s", deparse(saved$uri)),
      sprintf("saved_bounds <- list(%s)", bounds_r(saved_bounds)),
      "ui <- fluidPage(tags$head(tags$script(src='builder-spatial-canvas.js')),",
      "  tags$canvas(id='enhance-alignment_spatial_plot', class='builder-spatial-canvas',",
      "    style='display:block;width:600px;height:400px'),",
      "  actionButton('show_saved','Saved'),",
      "  tags$div(id='enhance-alignment_spatial_plot-tooltip', hidden='hidden'),",
      "  tags$p(id='enhance-alignment_spatial_plot-summary'))",
      "controls <- function(saved=FALSE) list(coordinateRotation=0,dx=if(saved)0 else 5,",
      "  dy=if(saved)0 else -3,scale=if(saved)1 else 1.2,rotation=if(saved)0 else 90,",
      "  flip_x=if(saved)FALSE else TRUE,flip_y=FALSE,image_opacity=1,",
      "  point_opacity=0,point_size=1)",
      "scene <- function(saved=FALSE) list(available=TRUE,viewKey='pixel-test',",
      "  generation=if(saved)2L else 1L,resetToken=if(saved)2L else 1L,capped=FALSE,",
      "  points=list(x=numeric(),y=numeric(),barcode=character(),group=character(),",
      "    color=character(),count=integer()),",
      "  bounds=list(xmin=-20,xmax=70,ymin=-30,ymax=50),",
      "  image=list(uri=if(saved)saved_uri else raw_uri,",
      "    baseBounds=if(saved)saved_bounds else list(xmin=0,xmax=40,ymin=0,ymax=20)),",
      "  controls=controls(saved))",
      "server <- function(input,output,session){",
      "  session$onFlushed(function() session$sendCustomMessage(",
      "    'builder_spatial_canvas_scene',scene(FALSE)),once=TRUE)",
      "  observeEvent(input$show_saved,session$sendCustomMessage(",
      "    'builder_spatial_canvas_scene',scene(TRUE)),ignoreInit=TRUE)",
      "}",
      "shinyApp(ui,server)"
    ),
    file.path(app_dir, "app.R")
  )

  app <- AppDriver$new(
    app_dir,
    name = "builder_spatial_image_consistency",
    width = 800,
    height = 600,
    load_timeout = 30000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_js(
    paste0(
      "window.__builderSpatialCanvasMetrics.sceneMessages===1 && ",
      "(() => {const c=document.getElementById('enhance-alignment_spatial_plot');",
      "const d=c.getContext('2d').getImageData(0,0,c.width,c.height).data;",
      "for(let i=0;i<d.length;i+=4){if(d[i]>220&&d[i+1]<80&&d[i+2]<80)return true;}",
      "return false;})()"
    ),
    timeout = 10000
  )
  app$run_js(paste0(
    "window.__liveSpatialPixels=new Uint8ClampedArray(",
    "document.getElementById('enhance-alignment_spatial_plot')",
    ".getContext('2d').getImageData(0,0,",
    "document.getElementById('enhance-alignment_spatial_plot').width,",
    "document.getElementById('enhance-alignment_spatial_plot').height).data);"
  ))
  before <- app$get_js("window.__builderSpatialCanvasMetrics.renders")
  app$click("show_saved")
  app$wait_for_js(
    sprintf(
      paste0(
        "window.__builderSpatialCanvasMetrics.sceneMessages===2 && ",
        "window.__builderSpatialCanvasMetrics.renders>=%d && ",
        "(() => {const c=document.getElementById('enhance-alignment_spatial_plot');",
        "const d=c.getContext('2d').getImageData(0,0,c.width,c.height).data;",
        "for(let i=0;i<d.length;i+=4){if(d[i]>220&&d[i+1]<80&&d[i+2]<80)return true;}",
        "return false;})()"
      ),
      before + 1L
    ),
    timeout = 10000
  )
  difference <- app$get_js(paste0(
    "(() => {const c=document.getElementById('enhance-alignment_spatial_plot');",
    "const b=c.getContext('2d').getImageData(0,0,c.width,c.height).data;",
    "let total=0,large=0;for(let i=0;i<b.length;i+=1){",
    "const d=Math.abs(b[i]-window.__liveSpatialPixels[i]);total+=d;",
    "if(d>16)large+=1;}return {mean:total/b.length,large:large/b.length};})()"
  ))

  expect_lte(difference$mean, 1)
  expect_lte(difference$large, .01)
})
