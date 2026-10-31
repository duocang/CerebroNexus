run_specialist_state_node <- function(body) {
  run_node_script(c(
    "const fs = require('fs');",
    "const timers = []; const controls = {}; const applied = []; const handlers = {};",
    "const windowHandlers = {};",
    "let datasetContext = {epoch:'session',dataset_key:'dataset-a',generation:1};",
    "let datasetPhase = 'ready';",
    "global.window = {",
    "  setTimeout: (fn, delay) => { fn.delay = delay; timers.push(fn); return fn; },",
    "  clearTimeout: fn => { const at = timers.indexOf(fn); if (at >= 0) timers.splice(at, 1); },",
    "  addEventListener: (name, fn) => { windowHandlers[name] = fn; }",
    "};",
    "window.CerebroDatasetContext = {",
    "  current: () => datasetContext, phase: () => datasetPhase,",
    "  readyContext: () => datasetPhase === 'ready' ? datasetContext : null,",
    "  readyKey: () => window.CerebroDatasetContext.key(window.CerebroDatasetContext.readyContext()),",
    "  key: value => value ? JSON.stringify([value.epoch,value.dataset_key,value.generation]) : '',",
    "  same: (left, right) => !!left && !!right && left.epoch === right.epoch &&",
    "    left.dataset_key === right.dataset_key && left.generation === right.generation",
    "};",
    "global.document = {",
    "  getElementById: id => id === 'shiny-tab-overview' ?",
    "    {classList:{contains:()=>true},querySelectorAll:()=>[]} : (controls[id] || null),",
    "  querySelector: () => null",
    "};",
    "window.jQuery = global.jQuery = element => ({",
    "  data: () => element.binding,",
    "  on: (event, handler) => { handlers[event] = handler; },",
    "  off: event => { delete handlers[event]; }",
    "});",
    js_eval_file(viewer_test_path("www", "specialist-view-state.js")),
    body
  ))
}

test_that("HLA node selection maps cleanly between motif keys and cells", {
  segments <- data.frame(
    barcode = c("cell-1", "cell-2", "cell-3"),
    v_gene = c("TRBV1", "TRBV1", "TRBV2"),
    cdr3 = c("CASSA", "CASSA", "CASSB"),
    stringsAsFactors = FALSE
  )

  expect_identical(
    hla_segment_node_keys(segments, by_v = FALSE),
    c("CASSA", "CASSA", "CASSB")
  )
  expect_identical(
    hla_segment_node_keys(segments, by_v = TRUE),
    c("TRBV1::CASSA", "TRBV1::CASSA", "TRBV2::CASSB")
  )
  expect_identical(
    hla_cells_for_node_keys(segments, "CASSA", by_v = FALSE),
    c("cell-1", "cell-2")
  )
  ## Viewer Pack segments carry the cells' canonical rows.
  expect_null(hla_cells_for_node_keys(segments, "CASSA", column = "cell_index"))
  segments$cell_index <- c(7L, 7L, 2L)
  expect_identical(
    hla_cells_for_node_keys(
      segments,
      c("CASSA", "CASSB"),
      column = "cell_index"
    ),
    c(7L, 2L)
  )
})

test_that("Projection adapter captures shared JSON and downloads its PNG", {
  output <- run_specialist_state_node(c(
    "const calls = [];",
    "window.cerebroSavedViewDataset = {cell_count:2,cell_fingerprint:'cells'};",
    "window.cerebroCellViews = {",
    "  captureState: id => ({cells:['cell-1'],geometry:null,view:{mode:'lasso'}}),",
    "  downloadPNG: id => { calls.push(id); return true; }",
    "};",
    "windowHandlers['cerebro:specialist-state']({detail:{",
    "  viewId:'overview_projection',dataset_context:datasetContext}});",
    "const adapter = window.cerebroSpecialistViews.get('overview_projection');",
    "const captured = adapter.capture();",
    "console.log(JSON.stringify({schema:captured.schema,page:captured.page.id,",
    "  cells:captured.selection.cells,downloaded:adapter.downloadPNG(),calls:calls}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      schema = "cerebronexus-specialist-view",
      page = "overview_projection",
      cells = list("cell-1"),
      downloaded = TRUE,
      calls = list("overview_projection")
    )
  )
})

test_that("successful specialist restoration does not replay stale state", {
  output <- run_specialist_state_node(c(
    "window.cerebroCellViews = {applyState: (_id, config) => {",
    "  applied.push(config.selection.cells[0]); return {selectedCells:1};",
    "}};",
    "const adapter = window.cerebroSpecialistViews.get('overview_projection');",
    "adapter.apply({selection:{cells:['old']},controls:[]});",
    "adapter.apply({selection:{cells:['new']},controls:[]});",
    "timers.sort((a, b) => a.delay - b.delay);",
    "while (timers.length) timers.shift()();",
    "console.log(JSON.stringify(applied));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list("new")
  )
})

test_that("specialist sharing requires the current dataset generation", {
  output <- run_specialist_state_node(c(
    "window.cerebroCellViews = {captureState: () => ({cells:['cell-1']})};",
    "const adapter = window.cerebroSpecialistViews.get('overview_projection');",
    "const a1 = datasetContext;",
    "const b2 = {epoch:'session',dataset_key:'dataset-b',generation:2};",
    "const a3 = {epoch:'session',dataset_key:'dataset-a',generation:3};",
    "window.cerebroSavedViewDataset = {cell_fingerprint:'same-cells'};",
    "windowHandlers['cerebro:specialist-state']({detail:{",
    "  viewId:'overview_projection',dataset_context:a1}});",
    "const before = adapter.ready();",
    "datasetContext = b2;",
    "windowHandlers['cerebro:dataset-context']({detail:{",
    "  phase:'ready',changed:true,dataset_context:b2}});",
    "const afterB = adapter.ready();",
    "datasetContext = a3;",
    "windowHandlers['cerebro:dataset-context']({detail:{",
    "  phase:'ready',changed:true,dataset_context:a3}});",
    "windowHandlers['cerebro:specialist-state']({detail:{",
    "  viewId:'overview_projection',dataset_context:a1}});",
    "const afterLateA1 = adapter.ready();",
    "windowHandlers['cerebro:specialist-state']({detail:{",
    "  viewId:'overview_projection',dataset_context:a3}});",
    "console.log(JSON.stringify({before:before,afterB:afterB,",
    "  afterLateA1:afterLateA1,currentA3:adapter.ready()}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      before = TRUE,
      afterB = FALSE,
      afterLateA1 = FALSE,
      currentA3 = TRUE
    )
  )
})

test_that("specialist restoration follows dynamically bound controls", {
  output <- run_specialist_state_node(c(
    "window.cerebroCellViews = {applyState: () => {",
    "  if (controls.overview_dynamic) return;",
    "  controls.overview_dynamic = {id:'overview_dynamic',binding:{",
    "    receiveMessage: (_el, msg) => applied.push(msg.value)",
    "  }};",
    "  Object.keys(handlers).filter(name => name.indexOf('shiny:bound') === 0)",
    "    .forEach(name => handlers[name]({target:controls.overview_dynamic}));",
    "}};",
    "window.cerebroSpecialistViews.get('overview_projection').apply({",
    "  selection:{cells:['cell-1']},",
    "  controls:[{id:'overview_dynamic',multiple:false,values:['restored']}]",
    "});",
    "timers.sort((a, b) => a.delay - b.delay);",
    "while (timers.length) timers.shift()();",
    "console.log(JSON.stringify(applied));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list("restored")
  )
})

test_that("specialist restoration waits for late Shiny-bound controls", {
  output <- run_specialist_state_node(c(
    "window.cerebroCellViews = {applyState: () => {}};",
    "window.cerebroSpecialistViews.get('overview_projection').apply({",
    "  selection:{cells:['cell-1']},",
    "  controls:[{id:'overview_dynamic',multiple:false,values:['restored']}]",
    "});",
    "timers.filter(fn => fn.delay <= 750).forEach(fn => fn());",
    "controls.overview_dynamic = {id:'overview_dynamic',binding:{",
    "  receiveMessage: (_el, msg) => applied.push(msg.value)",
    "}};",
    "Object.keys(handlers).filter(name => name.indexOf('shiny:bound') === 0)",
    "  .forEach(name => handlers[name]({target:controls.overview_dynamic}));",
    "console.log(JSON.stringify(applied));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list("restored")
  )
})

test_that("specialist restoration keeps waiting for unresolved controls", {
  output <- run_specialist_state_node(c(
    "window.cerebroCellViews = {applyState: () => {}};",
    "window.cerebroSpecialistViews.get('overview_projection').apply({",
    "  selection:{cells:['cell-1']},",
    "  controls:[{id:'overview_dynamic',multiple:false,values:['restored']}]",
    "});",
    "timers.sort((a, b) => a.delay - b.delay);",
    "while (timers.length) timers.shift()();",
    "controls.overview_dynamic = {id:'overview_dynamic',binding:{",
    "  receiveMessage: (_el, msg) => applied.push(msg.value)",
    "}};",
    "Object.keys(handlers).filter(name => name.indexOf('shiny:bound') === 0)",
    "  .forEach(name => handlers[name]({target:controls.overview_dynamic}));",
    "console.log(JSON.stringify({applied:applied,handlers:Object.keys(handlers)}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(applied = list("restored"), handlers = list())
  )
})

test_that("specialist restoration retains only unresolved controls after the grace period", {
  output <- run_specialist_state_node(c(
    "controls.overview_static = {id:'overview_static',binding:{",
    "  receiveMessage: (_el, msg) => applied.push(msg.value)",
    "}};",
    "window.cerebroCellViews = {applyState: () => {}};",
    "window.cerebroSpecialistViews.get('overview_projection').apply({",
    "  selection:{cells:['cell-1']},",
    "  controls:[",
    "    {id:'overview_static',multiple:false,values:['static restored']},",
    "    {id:'overview_dynamic',multiple:false,values:['dynamic restored']}",
    "  ]",
    "});",
    "timers.sort((a, b) => a.delay - b.delay);",
    "while (timers.length) timers.shift()();",
    "Object.keys(handlers).filter(name => name.indexOf('shiny:bound') === 0)",
    "  .forEach(name => handlers[name]({target:controls.overview_static}));",
    "controls.overview_dynamic = {id:'overview_dynamic',binding:{",
    "  receiveMessage: (_el, msg) => applied.push(msg.value)",
    "}};",
    "Object.keys(handlers).filter(name => name.indexOf('shiny:bound') === 0)",
    "  .forEach(name => handlers[name]({target:controls.overview_dynamic}));",
    "console.log(JSON.stringify({applied:applied,handlers:Object.keys(handlers)}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      applied = list("static restored", "dynamic restored"),
      handlers = list()
    )
  )
})

test_that("specialist restoration retries a replaced binding that initially throws", {
  output <- run_specialist_state_node(c(
    "controls.overview_dynamic = {id:'overview_dynamic',binding:{",
    "  receiveMessage: () => { throw new Error('replaced'); }",
    "}};",
    "window.cerebroCellViews = {applyState: () => {}};",
    "window.cerebroSpecialistViews.get('overview_projection').apply({",
    "  selection:{cells:['cell-1']},",
    "  controls:[{id:'overview_dynamic',multiple:false,values:['restored']}]",
    "});",
    "timers.sort((a, b) => a.delay - b.delay);",
    "while (timers.length) timers.shift()();",
    "controls.overview_dynamic.binding.receiveMessage =",
    "  (_el, msg) => applied.push(msg.value);",
    "Object.keys(handlers).filter(name => name.indexOf('shiny:bound') === 0)",
    "  .forEach(name => handlers[name]({target:controls.overview_dynamic}));",
    "console.log(JSON.stringify({applied:applied,handlers:Object.keys(handlers)}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(applied = list("restored"), handlers = list())
  )
})

test_that("a new specialist restore cancels unresolved controls from the previous one", {
  output <- run_specialist_state_node(c(
    "window.cerebroCellViews = {applyState: () => {}};",
    "const adapter = window.cerebroSpecialistViews.get('overview_projection');",
    "adapter.apply({selection:{cells:['cell-1']},controls:[",
    "  {id:'overview_old',multiple:false,values:['old']}",
    "]});",
    "timers.sort((a, b) => a.delay - b.delay);",
    "while (timers.length) timers.shift()();",
    "controls.overview_new = {id:'overview_new',binding:{",
    "  receiveMessage: (_el, msg) => applied.push(msg.value)",
    "}};",
    "adapter.apply({selection:{cells:['cell-1']},controls:[",
    "  {id:'overview_new',multiple:false,values:['new']}",
    "]});",
    "controls.overview_old = {id:'overview_old',binding:{",
    "  receiveMessage: (_el, msg) => applied.push(msg.value)",
    "}};",
    "Object.keys(handlers).filter(name => name.indexOf('shiny:bound') === 0)",
    "  .forEach(name => handlers[name]({target:controls.overview_old}));",
    "console.log(JSON.stringify(applied));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list("new")
  )
})
