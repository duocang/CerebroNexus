test_that("the million-cell renderer is loaded before cell views", {
  renderer <- viewer_test_path("www", "cell_points_gpu.js")
  expect_true(file.exists(renderer))
  renderer_source <- paste(readLines(renderer, warn = FALSE), collapse = "\n")
  expect_match(renderer_source, "backend: 'webgpu'", fixed = TRUE)

  ui <- paste(
    readLines(viewer_test_path("shiny_UI.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(ui, 'cerebro_js("cell_points_gpu.js")', fixed = TRUE)
  expect_lt(
    regexpr("cell_points_gpu.js", ui, fixed = TRUE)[1],
    regexpr("cell_views.js", ui, fixed = TRUE)[1]
  )

  engine <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(engine, "CerebroPointRenderer.create", fixed = TRUE)
  expect_match(engine, "setData", fixed = TRUE)
})

test_that("WebGPU failures make the renderer fall back", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  renderer <- viewer_test_path("www", "cell_points_gpu.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      "const assert = require('assert');",
      sprintf(
        "const source = fs.readFileSync(%s, 'utf8');",
        encodeString(renderer, quote = '"')
      ),
      "let uncaptured;",
      "const device = {",
      "  lost: new Promise(() => {}),",
      "  addEventListener: (name, handler) => { if (name === 'uncapturederror') uncaptured = handler; },",
      "  createShaderModule: () => ({}),",
      "  createRenderPipelineAsync: async () => ({ getBindGroupLayout: () => ({}) }),",
      "  createBuffer: () => ({ destroy() {} }),",
      "  createBindGroup: () => ({})",
      "};",
      "Object.defineProperty(global, 'navigator', { configurable: true, value: { gpu: {",
      "  requestAdapter: async () => ({ info: {}, requestDevice: async () => device }),",
      "  getPreferredCanvasFormat: () => 'rgba8unorm'",
      "} } });",
      "global.window = global; global.devicePixelRatio = 1;",
      "global.GPUBufferUsage = { UNIFORM: 1, COPY_DST: 2, VERTEX: 4 };",
      "global.GPUTextureUsage = { RENDER_ATTACHMENT: 1, COPY_SRC: 2 };",
      "eval(source);",
      "(async () => {",
      "  const canvas = { clientWidth: 10, clientHeight: 10, style: {},",
      "    getContext: () => ({ configure() {} }) };",
      "  const renderer = CerebroPointRenderer.create(canvas);",
      "  await renderer.ready;",
      "  uncaptured({ error: new Error('driver rejected the frame') });",
      "  await renderer.failed;",
      "  assert.strictEqual(renderer.isReady(), false);",
      "  assert.strictEqual(renderer.stats().ready, false);",
      "  assert.match(renderer.stats().error, /driver rejected the frame/);",
      "})().catch(error => { console.error(error); process.exit(1); });"
    ),
    runner
  )
  status <- system2("node", runner)

  expect_identical(status, 0L)

  engine <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(engine, "renderer.failed.then", fixed = TRUE)
})

test_that("WebGPU fallback keeps validity from the materialized CPU unit", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  engine <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )
  occupancy_source <- regmatches(
    engine,
    regexpr(
      "(?s)var OCC = 64;.*?(?=\\n  function occCount)",
      engine,
      perl = TRUE
    )
  )
  unit_source <- regmatches(
    engine,
    regexpr(
      "(?s)function deferredUnitOf\\(space\\).*?(?=\\n  function transitionUnit)",
      engine,
      perl = TRUE
    )
  )
  project_source <- regmatches(
    engine,
    regexpr(
      "(?s)function project\\(p, forceCpu\\).*?(?=\\n  function pointScreenX)",
      engine,
      perl = TRUE
    )
  )
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const assert = require('assert');",
      occupancy_source,
      unit_source,
      "const D = {n:2};",
      "const space = {x:[0,1],y:[0,1],xRange:[0,1],yRange:[0,1]};",
      "const spaceById = {projection:space};",
      "const gpuCandidate = () => true;",
      project_source,
      "const panel = {spaceId:'projection',W:100,H:100,view:null};",
      "project(panel, false);",
      "assert.strictEqual(panel.ok, space._unit.ok);",
      "assert.deepStrictEqual(Array.from(panel.ok), [0,0]);",
      "project(panel, true);",
      "assert.strictEqual(panel.ok, space._unit.ok);",
      "assert.deepStrictEqual(Array.from(panel.ok), [1,1]);",
      "assert.strictEqual(panel.gpuTransformOnly, false);"
    ),
    runner
  )

  expect_identical(system2("node", runner), 0L)
})
