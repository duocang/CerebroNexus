# Functional check of the actual Data & QC tab; no timing claim.
main <- function() {
  args <- commandArgs(TRUE)
  stopifnot(length(args) == 3L)
  Sys.setenv(NOT_CRAN = "true")
  root <- normalizePath(args[[1L]])
  crb <- normalizePath(args[[2L]])
  q <- function(x) encodeString(x, quote = '"')
  app_dir <- tempfile("hla-qc-")
  dir.create(app_dir)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  writeLines(c(sprintf("devtools::load_all(%s,quiet=TRUE)", q(root)),
    sprintf("launchCerebro(mode='closed',crb_file_to_load=c(Demo=%s))", q(crb))),
    file.path(app_dir, "app.R"))
  shinytest2::local_app_support(app_dir)
  app <- shinytest2::AppDriver$new(app_dir, width=1619, height=950,
    timeout=120000, load_timeout=120000)
  on.exit(try(app$stop(), silent=TRUE), add=TRUE)
  browser <- app$get_chromote_session()$parent
  on.exit(try(browser$close(), silent=TRUE), add=TRUE)
  app$run_js("document.querySelector(\"a[href='#shiny-tab-hla_tcr_motifs']\").click();")
  app$wait_for_js("!!document.querySelector('#hla_tabs a[data-value=\"Data & QC\"]')", timeout=30000)
  app$run_js("document.querySelector('#hla_tabs a[data-value=\"Data & QC\"]').click();")
  ready <- paste0("['hla_coverage_table','hla_normalized_preview','hla_donor_mapping_preview'].every(id=>{",
    "const e=document.getElementById(id);return e?.offsetParent!==null&&e?.querySelectorAll('tbody tr').length>0;})")
  app$wait_for_js(ready, timeout=120000)
  app$run_js(paste0("window.__qcMount=document.getElementById('hla_data_qc_ui');",
    "window.__qcTables=['hla_coverage_table','hla_normalized_preview','hla_donor_mapping_preview'].map(id=>document.getElementById(id));",
    "document.querySelector('#hla_tabs a[data-value=\"Motif Network\"]').click();"))
  app$run_js("document.querySelector('#hla_tabs a[data-value=\"Data & QC\"]').click();")
  app$wait_for_js(ready, timeout=30000)
  state <- app$get_js(paste0("({sameMount:__qcMount===document.getElementById('hla_data_qc_ui'),",
    "sameTables:__qcTables.every(e=>document.getElementById(e.id)===e),",
    "sampleCount:document.getElementById('hla_ir_sample_count').innerText,",
    "source:document.getElementById('hla_active_source_ui').innerText,",
    "tables:['hla_coverage_table','hla_normalized_preview','hla_donor_mapping_preview'].map(id=>({id,rows:document.getElementById(id).querySelectorAll('tbody tr').length})),",
    "errors:Array.from(document.querySelectorAll('.shiny-output-error')).filter(e=>e.offsetParent!==null).map(e=>e.innerText)})"))
  stopifnot(isTRUE(state$sameMount), isTRUE(state$sameTables),
    grepl("Immune-repertoire samples: [1-9]", state$sampleCount),
    grepl("stored .crb", state$source, fixed=TRUE), length(state$errors)==0L)
  jsonlite::write_json(state, args[[3L]], pretty=TRUE, auto_unbox=TRUE)
  cat("HLA Data & QC three tables and stable return mount: PASS\n")
}
main()
