generated_app_e2e_reset_runtime()

.generated_app_e2e_dom_text <- function(id, contains, timeout = 60000) {
  id_js <- .generated_app_e2e_js_value(id)
  contains_js <- .generated_app_e2e_js_value(as.list(as.character(contains)))
  driver <- generated_app_e2e_driver()
  driver$wait_for_js(
    paste0(
      "(function(){var node=document.getElementById(",
      id_js,
      ");if(!node)return false;var text=node.textContent||'';var expected=",
      contains_js,
      ";return expected.every(function(value){return text.indexOf(value)!==-1;});})()"
    ),
    timeout = timeout
  )
  as.character(driver$get_js(sprintf(
    "document.getElementById(%s).textContent",
    id_js
  )))
}

test_that("Trekker renders linked canvases and source-backed QC content", {
  fixture <- generated_app_e2e_select_dataset("trekker")
  expect_identical(
    generated_app_e2e_visible_pages(),
    fixture$expected$visible_pages
  )
  generated_app_e2e_activate_tab("trekker")

  driver <- generated_app_e2e_driver()
  driver$wait_for_js(
    paste0(
      "(function(){var spatial=document.getElementById('tk-cv-sp');",
      "var umap=document.getElementById('tk-cv-um');",
      "return !!(spatial && umap && spatial.width>0 && spatial.height>0 && ",
      "umap.width>0 && umap.height>0);})()"
    ),
    timeout = 60000
  )

  subline <- .generated_app_e2e_dom_text(
    "tk-subline",
    c("Mouse_Brain_TrekkerU_C", "24")
  )
  expect_match(subline, "TrekkerU_C", fixed = TRUE)
  stats <- .generated_app_e2e_dom_text("tk-stats", c("24", "100"))
  expect_match(stats, "nuclei", ignore.case = TRUE)
  position_classes <- .generated_app_e2e_dom_text(
    "tk-postbl",
    c("24", "100.00%", "Imported")
  )
  expect_match(position_classes, "salvaged", ignore.case = TRUE)
  moran <- .generated_app_e2e_dom_text("tk-morantbl", c("Gene1", "0.2603"))
  expect_match(moran, "Gene8", fixed = TRUE)

  expect_identical(
    generated_app_e2e_value("input", "trekker_view"),
    "pair"
  )
  expect_identical(
    length(generated_app_e2e_value("input", "trekker_group_filter_cluster")),
    length(unique(fixture$object@misc$trekker$clusters))
  )
  generated_app_e2e_expect_clean_browser()
})
