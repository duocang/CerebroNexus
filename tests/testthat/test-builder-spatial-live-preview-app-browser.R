library(shinytest2)

test_that("the real Builder renders a 4000 point stress scene without blocking", {
  root <- builder_profile_inst_path("builder")
  local_app_support(root)
  app <- AppDriver$new(
    root,
    name = "builder_spatial_canvas_real_app_drag",
    width = 1440,
    height = 1000,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)
  app$wait_for_js(
    "document.querySelector('.example-btn[data-ex=all_content]') !== null",
    timeout = 30000
  )
  app$click(selector = ".example-btn[data-ex=all_content]")
  app$wait_for_js(
    paste0(
      "document.querySelector('.builder-spatial-canvas') !== null && ",
      "document.getElementById('enhance-coordinate_rotation') !== null && ",
      "document.getElementById('enhance-coordinate_rotation')",
      ".parentElement.querySelector('.irs') !== null"
    ),
    timeout = 60000
  )
  app$run_js(paste0(
    "const dismiss=document.querySelector('.builder-first-run-dismiss');",
    "if(dismiss) dismiss.click();"
  ))
  app$run_js(paste0(
    "window.__builderSpatialCanvasMetrics.eventToRenderMs=[];",
    "window.__builderSpatialCanvasMetrics.renderTimes=[];",
    "window.__builderSpatialCanvasMetrics.longTasks=0;"
  ))
  app$run_js(paste0(
    "(() => { const n=4000, x=[], y=[], barcode=[], group=[], color=[], count=[];",
    "for(let i=0;i<n;i+=1){const a=i*16*Math.PI/n,r=.05+.95*i/n;",
    "x.push(r*Math.cos(a));y.push(r*Math.sin(a));barcode.push('cell-'+i);",
    "group.push(i%2?'B':'A');color.push(i%2?'#00f':'#f00');count.push(1);}",
    "const scene={available:true,viewKey:'real-builder-stress',generation:999,",
    "resetToken:999,capped:false,points:{x,y,barcode,group,color,count},",
    "bounds:{xmin:-1,xmax:1,ymin:-1,ymax:1},image:null,controls:{",
    "coordinateRotation:0,dx:0,dy:0,scale:1,rotation:0,flip_x:false,",
    "flip_y:false,image_opacity:.8,point_opacity:.85,point_size:5}};",
    "Shiny.shinyapp.dispatchMessage(JSON.stringify({custom:",
    "{builder_spatial_canvas_scene:scene}})); })();"
  ))
  app$wait_for_js(
    paste0(
      "window.__builderSpatialCanvasMetrics.sceneMessages >= 1 && ",
      "document.querySelector('.builder-spatial-canvas-summary').textContent",
      ".includes('4000 sampled points')"
    ),
    timeout = 10000
  )
  metrics <- app$get_js("window.__builderSpatialCanvasMetrics")
  dimensions <- app$get_js(paste0(
    "(() => { const c=document.querySelector('.builder-spatial-canvas');",
    "return {width:c.width,height:c.height}; })()"
  ))

  expect_gte(metrics$renders, 1L)
  expect_identical(metrics$longTasks, 0L)
  expect_gt(dimensions$width, 0L)
  expect_gt(dimensions$height, 0L)
})
