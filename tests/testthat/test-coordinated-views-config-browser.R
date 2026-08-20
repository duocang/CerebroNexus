# test-coordinated-views-config-browser.R — real-browser configuration state.

library(shinytest2)

config_browser_inst <- system.file(package = "CerebroNexus")
if (
  !nzchar(config_browser_inst) ||
    !file.exists(file.path(config_browser_inst, "app.R"))
) {
  config_browser_inst <- testthat::test_path("../../inst")
}

config_browser_app <- function() {
  app <- AppDriver$new(
    config_browser_inst,
    name = "cv_config_browser",
    height = 900,
    width = 1440
  )
  app$wait_for_idle(timeout = 30000)
  app$wait_for_js(
    "document.querySelector('a[href=\"#shiny-tab-coordinated_views\"]') !== null",
    timeout = 30000
  )
  app$run_js(paste0(
    "document.querySelector('a[href=\"#shiny-tab-coordinated_views\"]')",
    ".click();"
  ))
  app$wait_for_idle(timeout = 20000)
  app
}

config_browser_bundle_js <- function() {
  paste0(
    "(function(){",
    "var cells=['c0','c1','c2','c3','c4','c5','c6','c7','c8'];",
    "var x=[-2,-1,0,1,2,-2,0,2,0],y=[-2,-1,0,1,2,2,1,-2,-1];",
    "var z=[-1,-.5,0,.5,1,-.7,.2,.8,-.2];",
    "var values=[0,1,2,0,1,2,0,1,2];",
    "var b={dataset_id:'adapter-v1',",
    "dataset_fingerprint:'md5-cell-set-v1:0123456789abcdef0123456789abcdef',",
    "cells:cells,n:cells.length,groups:{cluster:{values:values,",
    "levels:['a','b','c'],colors:['#636EFA','#EF553B','#00CC96']}},",
    "cat_extra:{},cat_skipped:{},fields:{},default_group:'cluster',",
    "projections:{umap:{x:x,y:y,ndim:2},pca:{x:x,y:y,z:z,ndim:3}},",
    "default_projection:'umap',spaces:[",
    "{id:'umap',label:'umap (expression)',x:x,y:y},",
    "{id:'spatial',label:'section-a (spatial)',x:x,y:y,samples:[",
    "{name:'section-a',label:'section-a (spatial)',x:x,y:y,images:[",
    "{id:'he',label:'H&E',uri:'data:image/png;base64,iVBORw0KGgo=',",
    "bounds:{xmin:-2,xmax:2,ymin:-2,ymax:2},preset:{}}]}]},",
    "{id:'trekker',label:'Physical (Trekker)',x:x,y:y}],clone:null,",
    "trekker:{conf:[.1,.2,.3,.4,.5,.6,.7,.8,.9],",
    "evidence:[0,1,0,1,0,1,0,1,0]}};",
    "Shiny.shinyapp.dispatchMessage(JSON.stringify({custom:{coordviews_data:b}}));",
    "})();"
  )
}

test_that("the browser adapter round-trips a brushed cohort transactionally", {
  local_app_support(config_browser_inst)
  app <- config_browser_app()
  on.exit(app$stop(), add = TRUE)

  app$run_js(config_browser_bundle_js())
  app$wait_for_js(
    "document.getElementById('cv-meta').textContent.indexOf('9 cells') >= 0",
    timeout = 15000
  )
  app$wait_for_js(
    "window.cerebroLinkedViewsState && window.cerebroLinkedViewsState.ready()",
    timeout = 10000
  )

  app$run_js(paste0(
    "(function(){",
    "document.querySelector('.cv-tbtn[data-act=\"box\"]').click();",
    "var cv=document.getElementById('cv-cv-a'),r=cv.getBoundingClientRect();",
    "cv.dispatchEvent(new MouseEvent('mousedown',{clientX:r.left+8,",
    "clientY:r.top+8,bubbles:true}));",
    "cv.dispatchEvent(new MouseEvent('mousemove',{clientX:r.right-8,",
    "clientY:r.bottom-8,bubbles:true}));",
    "window.dispatchEvent(new MouseEvent('mouseup',{clientX:r.right-8,",
    "clientY:r.bottom-8,bubbles:true}));",
    "})();"
  ))
  app$wait_for_js(
    "window.cerebroLinkedViewsState.summary().selectedCells > 0",
    timeout = 10000
  )

  app$run_js("window.__cvSaved = window.cerebroLinkedViewsState.capture();")
  selected <- unlist(app$get_js("window.__cvSaved.selection.cells"))
  expect_gt(length(selected), 0L)
  expect_identical(
    app$get_js("window.__cvSaved.dataset.cell_fingerprint"),
    "md5-cell-set-v1:0123456789abcdef0123456789abcdef"
  )

  app$run_js("document.getElementById('cv-clear').click();")
  app$wait_for_js(
    "window.cerebroLinkedViewsState.summary().selectedCells === 0",
    timeout = 10000
  )
  app$run_js("window.cerebroLinkedViewsState.apply(window.__cvSaved);")
  app$wait_for_js(
    paste0(
      "window.cerebroLinkedViewsState.summary().selectedCells === ",
      length(selected)
    ),
    timeout = 10000
  )
  expect_equal(
    unlist(app$get_value(input = "coordviews_selection")),
    selected
  )

  app$run_js(paste0(
    "window.__cvBefore=JSON.stringify(window.cerebroLinkedViewsState.summary());",
    "window.__cvBad=JSON.parse(JSON.stringify(window.__cvSaved));",
    "window.__cvBad.dataset.cell_fingerprint='md5-cell-set-v1:",
    "00000000000000000000000000000000';",
    "try{window.cerebroLinkedViewsState.apply(window.__cvBad);}",
    "catch(error){window.__cvApplyError=error.message;}"
  ))
  expect_match(app$get_js("window.__cvApplyError"), "cell population")
  expect_identical(
    app$get_js("JSON.stringify(window.cerebroLinkedViewsState.summary())"),
    app$get_js("window.__cvBefore")
  )

  app$run_js(paste0(
    "window.__cvContext=JSON.parse(JSON.stringify(window.__cvSaved));",
    "window.__cvContext.selection.cells=['c0','c1'];",
    "window.__cvContext.view.projections=['umap','pca'];",
    "window.__cvContext.view.filters={cluster:['a','b']};",
    "window.__cvContext.view.hidden_levels=[{group:'cluster',levels:['c']}];",
    "window.__cvContext.view.display={percentage_cells:100,point_size:2.2,",
    "point_opacity:.35,group_labels:false,selection_mode:'box',",
    "clone_layout:'stack'};",
    "window.__cvContext.view.focus_space='projection::pca';",
    "window.__cvContext.view.lenses=window.__cvContext.view.lenses.filter(",
    "function(x){return x.space!=='projection::pca';});",
    "window.__cvContext.view.lenses[0].viewport={cx:.45,cy:.55,span:.7};",
    "window.__cvContext.view.lenses.push({space:'projection::pca',",
    "viewport:{cx:.5,cy:.5,span:1},rotation:{rx:.2,ry:.3}});",
    "window.__cvContext.view.spatial_backgrounds[0].mode='image';",
    "window.__cvContext.view.spatial_backgrounds[0].image_id='he';",
    "window.__cvContext.view.spatial_backgrounds[0].opacity=.4;",
    "window.__cvContext.view.spatial_backgrounds[0].alignment.scale_x=1.2;",
    "window.__cvContext.view.spatial_backgrounds[0].alignment.scale_y=.8;",
    "window.__cvContext.view.spatial_backgrounds[0].alignment.lock_aspect=false;",
    "window.__cvContext.view.trekker={dissolve_percentage:25,evidence:false,",
    "niche_radius:300};",
    "window.cerebroLinkedViewsState.apply(window.__cvContext);",
    "window.__cvContextAfter=window.cerebroLinkedViewsState.capture();"
  ))
  app$wait_for_js(
    "window.cerebroLinkedViewsState.summary().selectedCells === 2",
    timeout = 10000
  )
  expect_true(app$get_js(paste0(
    "window.__cvContextAfter.view.projections.join(',')==='umap,pca'&&",
    "window.__cvContextAfter.view.focus_space==='projection::pca'&&",
    "window.__cvContextAfter.view.display.point_size===2.2&&",
    "window.__cvContextAfter.view.display.point_opacity===.35&&",
    "window.__cvContextAfter.view.display.group_labels===false&&",
    "window.__cvContextAfter.view.filters.cluster.join(',')==='a,b'&&",
    "window.__cvContextAfter.view.hidden_levels[0].levels[0]==='c'&&",
    "window.__cvContextAfter.view.trekker.dissolve_percentage===25&&",
    "window.__cvContextAfter.view.trekker.evidence===false&&",
    "window.__cvContextAfter.view.trekker.niche_radius===300"
  )))
  expect_true(app$get_js(paste0(
    "(function(){var p=window.__cvContextAfter.view.lenses.filter(",
    "function(x){return x.space==='projection::pca';})[0];",
    "var b=window.__cvContextAfter.view.spatial_backgrounds[0];",
    "return p&&p.rotation&&p.rotation.rx===.2&&p.rotation.ry===.3&&",
    "b.mode==='image'&&b.image_id==='he'&&b.opacity===.4&&",
    "b.alignment.scale_x===1.2&&b.alignment.scale_y===.8&&",
    "b.alignment.lock_aspect===false;})()"
  )))

  app$run_js(paste0(
    "window.__cvCapabilityBefore=window.cerebroLinkedViewsState.capture();",
    "window.__cvCapabilityBad=JSON.parse(JSON.stringify(",
    "window.__cvCapabilityBefore));",
    "window.__cvCapabilityBad.view.projections.push('missing-projection');",
    "try{window.cerebroLinkedViewsState.apply(window.__cvCapabilityBad);}",
    "catch(error){window.__cvCapabilityError=error.message;}",
    "window.__cvCapabilityAfter=window.cerebroLinkedViewsState.capture();",
    "window.__cvCapabilityAfter.created_at=window.__cvCapabilityBefore.created_at;"
  ))
  expect_match(app$get_js("window.__cvCapabilityError"), "projection")
  expect_identical(
    app$get_js("JSON.stringify(window.__cvCapabilityAfter)"),
    app$get_js("JSON.stringify(window.__cvCapabilityBefore)")
  )
})
