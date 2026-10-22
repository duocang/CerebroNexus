## The Share dialog's download and open requests are answered by the Linked
## views server (coordinated_views/server.R). Every check here runs in a session
## that has never opened Linked views, which is where Share used to time out.

share_overview <- "#shiny-tab-overview .cerebro-toolbar-share"

share_single_view_app <- function(name) {
  inst_dir <- viewer_app_test_path()
  suppressWarnings(shinytest2::local_app_support(inst_dir))
  app <- shinytest2::AppDriver$new(
    inst_dir,
    name = name,
    height = 900,
    width = 1440,
    load_timeout = 60000
  )
  app$click(selector = 'a[href="#shiny-tab-overview"]')
  app$wait_for_js(
    paste0(
      "CerebroDatasetContext.phase()==='ready' && ",
      "window.cerebroSpecialistViews.get('overview_projection')?.ready() && ",
      "!document.querySelector('",
      share_overview,
      "').disabled"
    ),
    timeout = 60000
  )
  expect_false(app$get_js(
    "document.getElementById('shiny-tab-coordinated_views').classList.contains('active')"
  ))
  app
}

test_that("Share on a single-plot page downloads and opens before Linked views", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  ## Download the Projection view's JSON.
  app <- share_single_view_app("share_single_view_download")
  app$run_js(paste0(
    "(() => {const original=URL.createObjectURL;window.__testViewJSON=null;",
    "URL.createObjectURL=function(blob){",
    "if(blob.type.startsWith('application/json'))",
    "blob.text().then(text=>{window.__testViewJSON=text;});",
    "return original.call(URL,blob);};})()"
  ))
  app$click(selector = share_overview)
  app$wait_for_js(
    paste0(
      "document.getElementById('cv-config-dialog').open && ",
      "!document.getElementById('cv-config-download').disabled"
    ),
    timeout = 10000
  )
  app$click(selector = "#cv-config-download")
  app$wait_for_js("window.__testViewJSON !== null", timeout = 15000)
  json <- app$get_js("window.__testViewJSON")
  expect_match(
    app$get_js("document.getElementById('cv-config-status').textContent"),
    "^Downloaded the configuration"
  )
  config <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_identical(config$page$id, "overview_projection")
  app$stop()

  ## Open it in another fresh session.
  app <- share_single_view_app("share_single_view_open")
  withr::defer(app$stop())
  app$click(selector = share_overview)
  app$wait_for_js(
    "document.getElementById('cv-config-dialog').open",
    timeout = 10000
  )
  app$run_js(sprintf(
    paste0(
      "(() => {const input=document.getElementById('coordviews_config_upload');",
      "const transfer=new DataTransfer();",
      "transfer.items.add(new File([%s],'projection.json',",
      "{type:'application/json'}));",
      "input.files=transfer.files;",
      "input.dispatchEvent(new Event('change',{bubbles:true}));})()"
    ),
    jsonlite::toJSON(json, auto_unbox = TRUE)
  ))
  app$wait_for_js(
    "document.getElementById('cv-config-status').classList.contains('is-success')",
    timeout = 15000
  )
})
