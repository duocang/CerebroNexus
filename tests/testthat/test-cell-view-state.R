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

test_that("single prepared cache reuses only an exact progressive payload", {
  output <- run_state_node(paste0(
    "const S = window.CBViewState;",
    "const cache = S.createSinglePreparedCache(1);",
    "const coordinates = new Float32Array([1,2]);",
    "let builds = 0;",
    "const build = () => ({coordinates:coordinates, build:++builds});",
    "const first = cache.resolve('gene',7,build);",
    "const second = cache.resolve('gene',7,build);",
    "const changedToken = cache.resolve('gene',8,build);",
    "const changedId = cache.resolve('trajectory',8,build);",
    "console.log(JSON.stringify({",
    "  builds:builds, same:first===second, typed:first.coordinates===coordinates,",
    "  tokenChanged:first!==changedToken, idChanged:changedToken!==changedId,",
    "  size:cache.size()",
    "}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      builds = 3L,
      same = TRUE,
      typed = TRUE,
      tokenChanged = TRUE,
      idChanged = TRUE,
      size = 1L
    )
  )
})

test_that("single prepared cache bypasses missing tokens and clears", {
  output <- run_state_node(paste0(
    "const cache = window.CBViewState.createSinglePreparedCache(1);",
    "let builds = 0;",
    "const build = () => ({build:++builds});",
    "const nullA = cache.resolve('gene',null,build);",
    "const nullB = cache.resolve('gene',null,build);",
    "const missingA = cache.resolve('gene',undefined,build);",
    "const missingB = cache.resolve('gene',undefined,build);",
    "cache.resolve('gene',9,build);",
    "cache.clear();",
    "cache.resolve('gene',9,build);",
    "console.log(JSON.stringify({",
    "  builds:builds, nullMiss:nullA!==nullB, missingMiss:missingA!==missingB,",
    "  size:cache.size()",
    "}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(builds = 6L, nullMiss = TRUE, missingMiss = TRUE, size = 1L)
  )
})

test_that("single prepared aux patches a hidden cached view", {
  output <- run_state_node(paste0(
    "const S = window.CBViewState;",
    "const cache = S.createSinglePreparedCache(1);",
    "const space = {};",
    "cache.resolve('gene',12,() => ({",
    "  data:{n:2,cells:[]}, spaceIds:['space'], spaceById:{space:space}",
    "}));",
    "const patched = cache.update('gene',12,(prepared) => ",
    "  S.patchSinglePreparedAux(prepared,['c1','c2'],{",
    "    text:['h1','h2'],columns:[],hoverinfo:'text'",
    "  },[1,1]));",
    "const stale = cache.update('gene',11,() => true);",
    "const hit = cache.resolve('gene',12,() => null);",
    "console.log(JSON.stringify({",
    "  patched:patched, stale:stale, cells:hit.data.cells,",
    "  hover:space._hover, offsets:Array.from(space._hoverOffsets),",
    "  enabled:space._hoverEnabled",
    "}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      patched = TRUE,
      stale = FALSE,
      cells = list("c1", "c2"),
      hover = list("h1", "h2"),
      offsets = list(0L, 1L, 2L),
      enabled = TRUE
    )
  )
})

test_that("single prepared cache drops an incomplete aux patch", {
  output <- run_state_node(paste0(
    "const S = window.CBViewState;",
    "const cache = S.createSinglePreparedCache(1);",
    "cache.resolve('gene',13,() => ({",
    "  data:{n:2,cells:[]}, spaceIds:[], spaceById:{}",
    "}));",
    "const patched = cache.update('gene',13,(prepared) => ",
    "  S.patchSinglePreparedAux(prepared,['c1'],{},null));",
    "console.log(JSON.stringify({patched:patched,size:cache.size()}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(patched = FALSE, size = 0L)
  )
})
