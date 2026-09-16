state_file <- viewer_test_path("www", "cell_views_state.js")

run_state_node <- function(body) {
  skip_if(Sys.which("node") == "", "node not on PATH")
  expect_true(file.exists(state_file), info = "cell_views_state.js not found")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      "global.window = {};",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(state_file, quote = "\"")
      ),
      body
    ),
    runner
  )
  system2("node", runner, stdout = TRUE, stderr = TRUE)
}

test_that("specialist state transitions use semantic identities", {
  output <- run_state_node(paste0(
    "const S = window.CBViewState;",
    "const spaces = {",
    "  a:{id:'single::trekker_projection::trekker',_role:'trekker'},",
    "  b:{id:'single::trekker_projection::umap',_role:'umap'}",
    "};",
    "const saved = [",
    "  {spaceId:'spatial',view:{cx:1}},",
    "  {spaceId:'umap',view:{cx:2}}",
    "];",
    "console.log(JSON.stringify({",
    "  role:S.spaceByRole(spaces,'trekker').id,",
    "  lens:S.lensForSpace(saved,'umap',0).view.cx,",
    "  missing:S.lensForSpace(saved,'missing',0),",
    "  controls:S.trekkerGeneControls('trekker_projection','CD3D'),",
    "  inactive:S.trekkerGeneControls('spatial_projection','CD3D'),",
    "  panel:S.genePanelSpaceId(1,'projection::umap')",
    "}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      role = "single::trekker_projection::trekker",
      lens = 2L,
      missing = NULL,
      controls = list(trekker_mode = "gene", trekker_gene_pick = "CD3D"),
      inactive = NULL,
      panel = "__linked_gene_1::projection::umap"
    )
  )
})

test_that("expression clear transitions remove every dependent payload", {
  output <- run_state_node(paste0(
    "const S = window.CBViewState;",
    "const state = {gene:{v:[1]},genePanels:[1],rgb:{r:[1]}};",
    "S.clearExpression(state,'gene');",
    "S.clearExpression(state,'panels');",
    "S.clearExpression(state,'rgb');",
    "console.log(JSON.stringify(state));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(gene = NULL, genePanels = NULL, rgb = NULL)
  )
})

test_that("specialist restore does not overwrite a saved state on its active page", {
  output <- run_state_node(paste0(
    "const S = window.CBViewState;",
    "console.log(JSON.stringify({",
    "  current:S.shouldStashSingleState('overview_projection',",
    "    'overview_projection',true),",
    "  navigating:S.shouldStashSingleState('spatial_projection',",
    "    'overview_projection',true),",
    "  normal:S.shouldStashSingleState('overview_projection',",
    "    'overview_projection',false)",
    "}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(current = FALSE, navigating = TRUE, normal = TRUE)
  )

  renderer <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(
    renderer,
    "activateSingle(id, false, true)",
    fixed = TRUE
  )
})

test_that("specialist selected-cell results wait for stable identities", {
  output <- run_state_node(paste0(
    "const S = window.CBViewState;",
    "const pendingA = S.specialistSelectionReport(new Set([0,2]), [], 3);",
    "const readyA = S.specialistSelectionReport(new Set([0,2]),",
    "  ['cell-a','cell-b','cell-c'],3);",
    "const cleared = S.specialistSelectionReport(null,",
    "  ['cell-a','cell-b','cell-c'],3);",
    "const latest = S.specialistSelectionReport(new Set([1]),",
    "  ['cell-a','cell-b','cell-c'],3);",
    "const readyNow = S.specialistSelectionReport(new Set([2]),",
    "  ['cell-a','cell-b','cell-c'],3);",
    "console.log(JSON.stringify({pendingA,readyA,cleared,latest,readyNow}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  state <- jsonlite::fromJSON(output, simplifyVector = FALSE)

  ## A: local selection/count exist immediately, while results remain gated.
  expect_true(state$pendingA$hasSelection)
  expect_identical(state$pendingA$selectedCells, 2L)
  expect_true(state$pendingA$pending)
  expect_null(state$pendingA$ids)

  ## B: aux readiness publishes the same current selection with stable IDs.
  expect_false(state$readyA$pending)
  expect_identical(unlist(state$readyA$ids), c("cell-a", "cell-c"))

  ## C: clearing before aux stays cleared when identities become available.
  expect_false(state$cleared$hasSelection)
  expect_identical(state$cleared$selectedCells, 0L)
  expect_null(state$cleared$ids)

  ## D: recomputing at aux time uses only the latest live selection.
  expect_identical(unlist(state$latest$ids), "cell-b")

  ## E: an already-ready specialist selection publishes immediately.
  expect_true(state$readyNow$stableKeysReady)
  expect_false(state$readyNow$pending)
  expect_identical(unlist(state$readyNow$ids), "cell-c")
})
