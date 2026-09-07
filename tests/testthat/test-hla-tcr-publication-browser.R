# test-hla-tcr-publication-browser.R — real Viewer publication journey.

library(shinytest2)

publication_browser_root_candidates <- c(
  normalizePath(testthat::test_path("../.."), mustWork = FALSE),
  normalizePath(".", mustWork = FALSE)
)
publication_browser_root <- publication_browser_root_candidates[file.exists(
  file.path(publication_browser_root_candidates, "data-raw")
)][1]
publication_browser_inst_candidates <- c(
  if (!is.na(publication_browser_root)) {
    file.path(publication_browser_root, "inst")
  },
  system.file(package = "CerebroNexus")
)
publication_browser_inst <- publication_browser_inst_candidates[file.exists(
  file.path(publication_browser_inst_candidates, "app.R")
)][1]
publication_browser_artifacts <- file.path(
  publication_browser_inst,
  "extdata/examples"
)
publication_browser_strict_config <- file.path(
  publication_browser_artifacts,
  "demo_hla_tcr_dextramer.linked-views.json"
)
publication_browser_viewer_config <- file.path(
  publication_browser_artifacts,
  "demo_hla_tcr_main_case.linked-view.json"
)

publication_browser_json_cells <- function(filename) {
  value <- jsonlite::fromJSON(filename, simplifyVector = FALSE)
  unname(unlist(value$selection$cells, use.names = FALSE))
}

test_that("the real Viewer restores both publication selections", {
  local_app_support(publication_browser_inst)
  app <- AppDriver$new(
    publication_browser_inst,
    name = "hla_tcr_publication_browser",
    height = 900,
    width = 1440,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  capture_root <- Sys.getenv("CEREBRONEXUS_CAPTURE_HLA_TCR_ROOT", "")
  capture_records <- list()
  capture <- function(filename, state, selected_cells = character()) {
    if (!nzchar(capture_root)) {
      return(invisible(NULL))
    }
    temporary <- tempfile(fileext = ".png")
    on.exit(unlink(temporary), add = TRUE)
    app$get_screenshot(temporary, selector = "viewport")
    destination <- file.path(capture_root, "vignettes/img", filename)
    if (!file.copy(temporary, destination, overwrite = TRUE)) {
      stop("could not publish screenshot: ", filename, call. = FALSE)
    }
    capture_records[[length(capture_records) + 1L]] <<- list(
      file = file.path("vignettes/img", filename),
      state = state,
      width = 1440L,
      height = 900L,
      selection_fingerprint = if (length(selected_cells)) {
        config_environment$cv_config_cell_fingerprint(selected_cells)
      } else {
        NULL
      }
    )
  }

  config_environment <- new.env(parent = baseenv())
  sys.source(
    file.path(
      publication_browser_inst,
      "viewer/coordinated_views/config.R"
    ),
    envir = config_environment
  )

  app$wait_for_idle(timeout = 30000)
  app$wait_for_js(
    "document.getElementById('crb_file_selector') !== null",
    timeout = 30000
  )
  app$set_inputs(
    crb_file_selector = "extdata/examples/demo_hla_tcr_dextramer.crb",
    wait_ = FALSE
  )
  app$wait_for_js(
    "document.body.textContent.indexOf('12,000') >= 0",
    timeout = 120000
  )
  capture("hla_tcr_real_data_info.png", "dataset-information")

  app$run_js(paste0(
    "document.querySelector(",
    "'a[href=\"#shiny-tab-coordinated_views\"]'",
    ").click();"
  ))
  app$wait_for_js(
    "window.cerebroLinkedViewsState && window.cerebroLinkedViewsState.ready()",
    timeout = 120000
  )
  app$wait_for_js(
    "document.getElementById('cv-meta').textContent.indexOf('12,000 cells') >= 0",
    timeout = 30000
  )
  capture("hla_tcr_main_case_linked_views_all.png", "linked-views-all-cells")

  expected_strict <- publication_browser_json_cells(
    publication_browser_strict_config
  )
  app$upload_file(coordviews_config_upload = publication_browser_strict_config)
  app$wait_for_js(
    paste0(
      "document.getElementById('cv-config-status').textContent.indexOf(",
      "'Restored 10 selected cells and view settings.') >= 0"
    ),
    timeout = 120000
  )
  restored_strict <- app$get_js(
    "window.cerebroLinkedViewsState.capture().selection.cells"
  )
  expect_length(restored_strict, length(expected_strict))
  expect_setequal(unname(unlist(restored_strict)), expected_strict)
  capture(
    "hla_tcr_strict_linked_views_selected.png",
    "linked-views-strict-selection",
    expected_strict
  )

  # A browser file input intentionally does not emit the same Shiny event
  # twice in every installed-package environment. Use a fresh real session for
  # the second checked-in configuration instead of weakening either assertion.
  app$stop()
  app <- AppDriver$new(
    publication_browser_inst,
    name = "hla_tcr_publication_viewer_case",
    height = 900,
    width = 1440,
    load_timeout = 60000
  )
  app$wait_for_js(
    "document.getElementById('crb_file_selector') !== null",
    timeout = 30000
  )
  app$set_inputs(
    crb_file_selector = "extdata/examples/demo_hla_tcr_dextramer.crb",
    wait_ = FALSE
  )
  app$wait_for_js(
    "document.body.textContent.indexOf('12,000') >= 0",
    timeout = 120000
  )
  app$run_js(paste0(
    "document.querySelector(",
    "'a[href=\"#shiny-tab-coordinated_views\"]'",
    ").click();"
  ))
  app$wait_for_js(
    "window.cerebroLinkedViewsState && window.cerebroLinkedViewsState.ready()",
    timeout = 120000
  )

  expected_viewer <- publication_browser_json_cells(
    publication_browser_viewer_config
  )
  app$upload_file(coordviews_config_upload = publication_browser_viewer_config)
  app$wait_for_js(
    paste0(
      "document.getElementById('cv-config-status').textContent.indexOf(",
      "'Restored 293 selected cells and view settings.') >= 0"
    ),
    timeout = 120000
  )
  restored_viewer <- app$get_js(
    "window.cerebroLinkedViewsState.capture().selection.cells"
  )
  expect_length(restored_viewer, length(expected_viewer))
  expect_setequal(unname(unlist(restored_viewer)), expected_viewer)
  capture(
    "hla_tcr_main_case_linked_views_selected.png",
    "linked-views-viewer-selection",
    expected_viewer
  )

  app$run_js("document.getElementById('cv-config-open').click();")
  app$wait_for_js(
    "document.getElementById('cv-config-dialog').open === true",
    timeout = 30000
  )
  capture(
    "hla_tcr_main_case_share_dialog.png",
    "share-selection-dialog",
    expected_viewer
  )
  app$run_js("document.getElementById('cv-config-close').click();")

  # Keep the specialist-page readiness check independent of a large linked
  # selection. This also mirrors how the publication screenshots are read.
  app$stop()
  app <- AppDriver$new(
    publication_browser_inst,
    name = "hla_tcr_publication_motif_pages",
    height = 900,
    width = 1440,
    load_timeout = 60000
  )
  app$wait_for_js(
    "document.getElementById('crb_file_selector') !== null",
    timeout = 30000
  )
  app$set_inputs(
    crb_file_selector = "extdata/examples/demo_hla_tcr_dextramer.crb",
    wait_ = FALSE
  )
  app$wait_for_js(
    "document.body.textContent.indexOf('12,000') >= 0",
    timeout = 120000
  )

  app$run_js(paste0(
    "document.querySelector(",
    "'a[href=\"#shiny-tab-hla_tcr_motifs\"]'",
    ").click();"
  ))
  app$wait_for_js(
    "document.querySelector('#hla_tabs.shiny-bound-input') !== null",
    timeout = 120000
  )
  app$set_inputs(hla_chain = "TRB", hla_tabs = "Motif Network", wait_ = FALSE)
  app$wait_for_js(
    paste0(
      "(function(){var x=document.getElementById('hla_plot_motifNetwork');",
      "return !!x && !x.classList.contains('recalculating') && ",
      "x.style.visibility !== 'hidden' && ",
      "document.querySelector('#hla_plot_motifNetwork .vis-network canvas') ",
      "!== null;})()"
    ),
    timeout = 120000
  )
  capture("hla_tcr_real_motif_network.png", "trb-motif-network")

  app$set_inputs(hla_tabs = "HLA Associations", wait_ = FALSE)
  app$wait_for_js(
    paste0(
      "(function(){var active=document.querySelector(",
      "'#hla_tabs li.active a[data-value=\"HLA Associations\"]');",
      "var body=document.getElementById('hla_associations_ui');",
      "return !!active && !!body && body.textContent.trim().length > 100;})()"
    ),
    timeout = 120000
  )
  capture("hla_tcr_real_hla_associations.png", "hla-association-context")

  app$set_inputs(hla_tabs = "Data & QC", wait_ = FALSE)
  app$wait_for_js(
    paste0(
      "(function(){var active=document.querySelector(",
      "'#hla_tabs li.active a[data-value=\"Data & QC\"]');",
      "var body=document.getElementById('hla_data_qc_ui');",
      "return !!active && !!body && body.textContent.trim().length > 100;})()"
    ),
    timeout = 120000
  )
  capture("hla_tcr_real_data_qc.png", "real-data-qc")

  if (nzchar(capture_root)) {
    manifest <- jsonlite::fromJSON(
      file.path(
        publication_browser_artifacts,
        "demo_hla_tcr_publication.manifest.json"
      ),
      simplifyVector = FALSE
    )
    source_commit <- system2(
      "git",
      c("-C", publication_browser_root, "rev-parse", "HEAD"),
      stdout = TRUE
    )[[1L]]
    metadata <- list(
      schema = "cerebronexus-hla-tcr-screenshots",
      version = 1L,
      source_commit = source_commit,
      dataset_fingerprint = manifest$dataset$cell_fingerprint,
      screenshots = capture_records
    )
    destination <- file.path(
      publication_browser_artifacts,
      "demo_hla_tcr_publication.screenshots.json"
    )
    staged <- paste0(destination, ".staged")
    on.exit(unlink(staged), add = TRUE)
    jsonlite::write_json(
      metadata,
      staged,
      auto_unbox = TRUE,
      pretty = TRUE,
      null = "null"
    )
    if (file.exists(destination)) {
      unlink(destination)
    }
    expect_true(file.rename(staged, destination))
  }
})
