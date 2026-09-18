test_that("the 1M demo is opt-in and validates its sidecar", {
  helper <- viewer_test_path("million_cell_demo.R")
  expect_true(file.exists(helper))
  env <- new.env(parent = baseenv())
  sys.source(helper, envir = env)

  original <- list(
    crb_file_to_load = c(Small = "small.crb"),
    point_size = c(Small = 5),
    point_opacity = c(Small = 1),
    percentage_cells_to_show = 100
  )
  expect_identical(env$viewerAddMillionCellDemo(original, ""), original)

  root <- tempfile("cerebro-1m-demo-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  crb <- file.path(root, "mouse.crb")
  file.create(crb)
  dir.create(file.path(root, "mouse.bpcells"))
  file.create(file.path(root, "mouse.bpcells", "shape"))

  configured <- env$viewerAddMillionCellDemo(original, crb)
  label <- "10x E18 mouse brain (1M)"
  expect_identical(
    unname(configured$crb_file_to_load[label]),
    normalizePath(crb)
  )
  expect_equal(unname(configured$point_size[label]), 1)
  expect_equal(unname(configured$point_opacity[label]), 0.5)
  expect_equal(unname(configured$percentage_cells_to_show[label]), 100)
  expect_equal(unname(configured$expression_point_size[label]), 2)
  expect_equal(unname(configured$expression_point_opacity[label]), 1)

  unlink(file.path(root, "mouse.bpcells", "shape"))
  expect_error(
    env$viewerAddMillionCellDemo(original, crb),
    "adjacent non-empty .bpcells"
  )
})

test_that("the 1M preparation keeps unique gene symbols", {
  script <- testthat::test_path("..", "bench", "prepare_viewer_1m_data.R")
  skip_if_not(
    file.exists(script),
    "benchmark tree not present (expected when checking a built package)"
  )
  env <- new.env(parent = globalenv())
  sys.source(script, envir = env)

  expect_identical(
    env$.viewer1mUniqueGeneSymbols(c("Cd3e", "Cd3e", "Ms4a1"), 3L),
    c("Cd3e", "Cd3e.1", "Ms4a1")
  )
  expect_error(
    env$.viewer1mUniqueGeneSymbols(c("Cd3e", ""), 2L),
    "non-empty"
  )

  metadata <- data.frame(
    cell_barcode = c("c1", "c2", "c3"),
    seurat_clusters = c("0", "1", "2"),
    stringsAsFactors = FALSE
  )
  projection <- rbind(
    c3 = c(1, 3),
    c1 = c(0, 1),
    c2 = c(2, 2)
  )
  object <- list(
    getGeneNames = function() c("Sox2", "Dcx", "Rbfox3"),
    getMeanExpressionForCells = function(cells, genes) {
      scores <- list(
        Sox2 = c(c1 = 10, c2 = 2, c3 = 0.5),
        Dcx = c(c1 = 1, c2 = 10, c3 = 2),
        Rbfox3 = c(c1 = 0.5, c2 = 2, c3 = 10)
      )
      values <- vapply(
        genes,
        function(gene) scores[[gene]][cells],
        numeric(length(cells))
      )
      if (is.null(dim(values))) values else rowMeans(values)
    }
  )
  trajectory <- env$.viewer1mMarkerGuidedTrajectory(
    object,
    metadata,
    projection
  )

  expect_identical(rownames(trajectory$meta), metadata$cell_barcode)
  expect_true(all(diff(trajectory$meta$pseudotime) > 0))
  expect_identical(
    levels(trajectory$meta$state),
    c("Neural progenitor", "Neuroblast", "Maturing neuron")
  )
  expect_equal(nrow(trajectory$edges), 2L)
  expect_equal(
    unname(unlist(trajectory$edges[1, ])),
    c(0, 1, 2, 2)
  )
  expect_identical(trajectory$provenance$type, "marker_guided")
  expect_identical(trajectory$provenance$version, 2L)
  expect_identical(trajectory$provenance$markers$early, "Sox2")
  expect_identical(trajectory$provenance$markers$late, "Rbfox3")

  branching <- env$.viewer1mMarkerGuidedEdges(data.frame(
    state = c(
      "Neural progenitor",
      "Neuroblast",
      "Excitatory neuron",
      "Inhibitory neuron"
    ),
    DR_1 = c(0, 1, 2, 2),
    DR_2 = c(0, 0, 1, -1),
    pseudotime = c(0, 0.5, 1, 1)
  ))
  terminal_edges <- branching[branching$target_dim_1 == 2, , drop = FALSE]
  expect_equal(nrow(terminal_edges), 2L)
  expect_equal(terminal_edges$source_dim_1, c(1, 1))
  expect_equal(terminal_edges$source_dim_2, c(0, 0))
  expect_equal(sort(terminal_edges$target_dim_2), c(-1, 1))
})

test_that("the 1M demo replaces the illustrative path with marker guidance", {
  script <- testthat::test_path("..", "bench", "prepare_viewer_1m_data.R")
  skip_if_not(
    file.exists(script),
    "benchmark tree not present (expected when checking a built package)"
  )
  env <- new.env(parent = globalenv())
  sys.source(script, envir = env)

  metadata <- data.frame(
    cell_barcode = c("c1", "c2", "c3"),
    seurat_clusters = c("0", "1", "2"),
    row.names = c("c1", "c2", "c3"),
    stringsAsFactors = FALSE
  )
  projection <- cbind(x = c(0, 2, 1), y = c(1, 2, 3))
  rownames(projection) <- metadata$cell_barcode
  parameters <- list()
  object <- new.env(parent = emptyenv())
  object$trajectories <- list(
    illustrative = list(UMAP_cluster_path = list())
  )
  object$getParameters <- function() parameters
  object$addParameters <- function(field, content) {
    parameters[[field]] <<- content
  }
  object$getMethodsForTrajectories <- function() names(object$trajectories)
  object$getNamesOfTrajectories <- function(method) {
    names(object$trajectories[[method]])
  }
  object$addTrajectory <- function(method, name, trajectory) {
    object$trajectories[[method]][[name]] <- trajectory
  }
  object$availableProjections <- function() "umap"
  object$getMetaData <- function() metadata
  object$getProjection <- function(name) projection
  object$getGeneNames <- function() c("Sox2", "Dcx", "Rbfox3")
  object$getMeanExpressionForCells <- function(cells, genes) {
    scores <- list(
      Sox2 = c(c1 = 10, c2 = 2, c3 = 0.5),
      Dcx = c(c1 = 1, c2 = 10, c3 = 2),
      Rbfox3 = c(c1 = 0.5, c2 = 2, c3 = 10)
    )
    values <- vapply(
      genes,
      function(gene) scores[[gene]][cells],
      numeric(length(cells))
    )
    if (is.null(dim(values))) values else rowMeans(values)
  }

  expect_true(env$.viewer1mEnrichDemoObject(object))
  expect_identical(parameters$main_group, "seurat_clusters")
  expect_false("illustrative" %in% names(object$trajectories))
  expect_named(object$trajectories$marker_guided, "E18_neurogenesis")
  expect_false(env$.viewer1mEnrichDemoObject(object))
})

test_that("the Viewer uses one supported-method filter in every trajectory UI", {
  files <- c(
    viewer_test_path("shiny_server.R"),
    viewer_test_path("trajectory", "projection.R"),
    viewer_test_path("trajectory", "select_method_and_name.R")
  )
  for (file in files) {
    expect_match(
      paste(readLines(file, warn = FALSE), collapse = "\n"),
      "viewerSupportedTrajectoryMethods(",
      fixed = TRUE,
      info = file
    )
  }
  utility <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = utility)
  expect_identical(
    utility$viewerSupportedTrajectoryMethods(
      c("unsupported", "marker_guided", "monocle2", "illustrative")
    ),
    c("marker_guided", "monocle2", "illustrative")
  )
})

test_that("optional Viewer tabs use compact catalog capabilities", {
  server <- paste(
    readLines(viewer_test_path("shiny_server.R"), warn = FALSE),
    collapse = "\n"
  )
  fields <- c(
    markerGenes = "marker_genes",
    mostExpressedGenes = "most_expressed_genes",
    enrichedPathways = "enriched_pathways",
    extra_material = "extra_material",
    trajectory = "trajectory",
    spatial = "spatial",
    trekker = "trekker"
  )

  for (tab in names(fields)) {
    expect_match(
      server,
      sprintf(
        'toggleConditionalTab\\([\\s\\S]{0,300}"%s"[\\s\\S]{0,300}catalog_field = "%s"',
        tab,
        fields[[tab]]
      ),
      perl = TRUE,
      info = tab
    )
  }
})

test_that("Viewer spatial readers leave molecule sidecars dormant", {
  utility <- paste(
    readLines(viewer_test_path("utility_functions.R"), warn = FALSE),
    collapse = "\n"
  )
  linked <- paste(
    readLines(
      viewer_test_path("coordinated_views", "bundle.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    utility,
    'ds$getSpatialData(name, hydrate_molecules = FALSE)',
    fixed = TRUE
  )
  expect_match(
    linked,
    'crb$getSpatialData(nm, hydrate_molecules = FALSE)',
    fixed = TRUE
  )
})

test_that("million-cell hover stays columnar until the browser needs it", {
  utility <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = utility)
  columns <- utility$cerebroProjectionHoverColumns(
    data.frame(
      cell_barcode = c("cell-1", "cell-2"),
      nUMI = c(1234, 9),
      nGene = c(321, 4),
      cluster = c("B", "A")
    ),
    groups = "cluster"
  )

  expect_identical(
    vapply(columns, `[[`, character(1), "label"),
    c("Transcripts", "Expressed genes", "cluster")
  )
  expect_identical(columns[[3L]]$levels, c("B", "A"))
  expect_identical(columns[[3L]]$values, c(0L, 1L))
})

test_that("specialist pages do not request the full linked bundle", {
  engine <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(engine, "function singlePayloadBundle", fixed = TRUE)
  expect_match(engine, "var vis = linkedVis;", fixed = TRUE)
  expect_no_match(engine, "linkedVis || !!singleId", fixed = TRUE)
})

test_that("specialist pages resend whenever they become visible again", {
  pages <- list(
    list(
      c("overview", "event_projection_update_plot.R"),
      "overview_projection_render_request"
    ),
    list(
      c("gene_expression", "event_projection_update_plot.R"),
      "expression_projection_render_request"
    ),
    list(
      c("spatial", "event_projection_update_plot.R"),
      "spatial_projection_render_request"
    ),
    list(
      c("trajectory", "projection_plot.R"),
      "trajectory_projection_render_request"
    ),
    list(
      c("hla_tcr_motifs", "visualizations.R"),
      "hla_motif_network_render_request"
    )
  )

  for (page in pages) {
    source <- paste(
      readLines(do.call(viewer_test_path, as.list(page[[1]])), warn = FALSE),
      collapse = "\n"
    )
    expect_match(
      source,
      sprintf('req(input[["%s"]]', page[[2]]),
      fixed = TRUE,
      info = page[[2]]
    )
  }
})

test_that("specialist pages use binary transport when the browser supports it", {
  utility <- paste(
    readLines(viewer_test_path("utility_functions.R"), warn = FALSE),
    collapse = "\n"
  )
  engine <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(utility, "cv_wire_pack_message", fixed = TRUE)
  expect_match(utility, '"cell_view_binary"', fixed = TRUE)
  expect_match(engine, "'cell_view_binary'", fixed = TRUE)
  expect_match(engine, "!ArrayBuffer.isView(data.color)", fixed = TRUE)
})

test_that("specialist bundles carry the saved dataset fingerprint", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  source <- viewer_test_path("www", "cell_views.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      sprintf(
        "const source = fs.readFileSync(%s, 'utf8');",
        encodeString(source, quote = '"')
      ),
      "const start = source.indexOf('  function singlePayloadCells');",
      "const end = source.indexOf('  function alignSingleCoordinates', start);",
      "global.window = {cerebroSavedViewDataset:{cell_fingerprint:'stale'}};",
      "eval(source.slice(start, end));",
      "const payload = {datasetIdentity:{cell_fingerprint:'md5-cell-set-v1:0123456789abcdef0123456789abcdef'},data:{n:2,selection_key:['c1','c2']}};",
      "const present = singlePayloadBundle('overview', payload).dataset_fingerprint;",
      "delete window.cerebroSavedViewDataset;",
      "const missing = singlePayloadBundle('overview', {data:payload.data}).dataset_fingerprint;",
      "console.log(JSON.stringify({present:present,missing:missing}));"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      present = "md5-cell-set-v1:0123456789abcdef0123456789abcdef",
      missing = ""
    )
  )
})

test_that("sparse linked attributes expand into typed canonical vectors", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  source <- viewer_test_path("www", "cell_views_state.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      "global.window = {};",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(source, quote = '"')
      ),
      "const values = window.CBViewState.expandSparse(",
      "  new Int32Array([1, 4]), new Int16Array([7, 9]), 6, -1);",
      "console.log(JSON.stringify(Array.from(values)));"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output),
    c(-1L, 7L, -1L, -1L, 9L, -1L)
  )
})

test_that("shared browser base reuses only matching dataset projections", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  source <- viewer_test_path("www", "cell_views_state.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs'); global.window = {};",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(source, quote = '"')
      ),
      "const S = window.CBViewState;",
      "const first = S.sharedBase(null, 'dataset-a', 3, 'order-a');",
      "first.projections.umap = {x:new Float32Array(3),y:new Float32Array(3)};",
      "const hit = S.sharedBase(first, 'dataset-a', 3, 'order-a');",
      "const changedDataset = S.sharedBase(first, 'dataset-b', 3, 'order-a');",
      "const changedCount = S.sharedBase(first, 'dataset-a', 4, 'order-a');",
      "const changedOrder = S.sharedBase(first, 'dataset-a', 3, 'order-b');",
      "console.log(JSON.stringify({hit:hit===first,dataset:changedDataset!==first,",
      "count:changedCount!==first,order:changedOrder!==first,",
      "projection:hit.projections.umap===first.projections.umap}));"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      hit = TRUE,
      dataset = TRUE,
      count = TRUE,
      order = TRUE,
      projection = TRUE
    )
  )
})
test_that("trajectory cell views remain eligible for WebGPU", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  source <- viewer_test_path("www", "cell_views.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      sprintf(
        "const source = fs.readFileSync(%s, 'utf8');",
        encodeString(source, quote = '"')
      ),
      "const fn = source.match(/function gpuCandidate\\(p\\) \\{[\\s\\S]*?\\n  \\}/)[0];",
      "const GPU_MIN_CELLS = 4096, D = {n: 1000000};",
      "const spaceById = {trajectory: {_unit: {nz: false}, trajectory: true}};",
      "eval(fn);",
      "if (!gpuCandidate({gpu: {}, spaceId: 'trajectory'})) process.exit(1);"
    ),
    runner
  )

  expect_identical(system2("node", runner), 0L)
})

test_that("single Canvas views accept per-point sizes", {
  utility <- paste(
    readLines(viewer_test_path("utility_functions.R"), warn = FALSE),
    collapse = "\n"
  )
  javascript <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(utility, '"point_sizes"', fixed = TRUE)
  expect_match(javascript, "space.pointSizes", fixed = TRUE)
  expect_match(javascript, "pointSizes[i]", fixed = TRUE)
})

test_that("gene controls load transcriptome choices server-side", {
  source <- paste(
    readLines(
      viewer_test_path("gene_expression", "UI_projection_input_type.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  utility <- paste(
    readLines(viewer_test_path("utility_functions.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_no_match(source, "list_of_genes()", fixed = TRUE)
  expect_match(source, "serverSideGeneSelector(", fixed = TRUE)
  expect_no_match(source, "retry = FALSE", fixed = TRUE)
  expect_match(utility, "selected <- isolate(input[[input_id]])", fixed = TRUE)
})

test_that("the real Viewer benchmark accepts the Canvas baseline", {
  benchmark_file <- testthat::test_path(
    "..",
    "bench",
    "benchmark_million_cell_viewer.R"
  )
  skip_if_not(
    file.exists(benchmark_file),
    "benchmark tree not present (expected when checking a built package)"
  )
  benchmark <- paste(
    readLines(benchmark_file, warn = FALSE),
    collapse = "\n"
  )

  expect_match(benchmark, "has_gpu_renderer <- file.exists", fixed = TRUE)
  expect_match(
    benchmark,
    "CEREBRO_VIEWER_BENCH_OVERVIEW_ONLY",
    fixed = TRUE
  )
  expect_match(benchmark, "canvas[id^=\\\"cv-cv-\\\"]", fixed = TRUE)
  expect_match(benchmark, "backend:'canvas2d'", fixed = TRUE)
  expect_match(benchmark, "cerebroLinkedViewsState", fixed = TRUE)
  expect_match(benchmark, "linked_ready_ms", fixed = TRUE)
  expect_match(benchmark, "gene_ready_ms", fixed = TRUE)
  expect_match(benchmark, "rgb_ready_ms", fixed = TRUE)
})

test_that("the Ren Linked benchmark records visual and interactive readiness", {
  benchmark_file <- testthat::test_path(
    "..",
    "bench",
    "benchmark_ren_linked_transport.R"
  )
  skip_if_not(file.exists(benchmark_file), "benchmark tree not present")
  benchmark <- paste(readLines(benchmark_file, warn = FALSE), collapse = "\n")

  expect_match(benchmark, "primary_logical_ms", fixed = TRUE)
  expect_match(benchmark, "complete_logical_ms", fixed = TRUE)
  expect_match(benchmark, "visual_ready_ms", fixed = TRUE)
  expect_match(benchmark, "interactive_ready_ms", fixed = TRUE)
  expect_match(benchmark, "cerebroLinkedViewsState.visualReady()", fixed = TRUE)
  expect_match(benchmark, "wait_for_idle", fixed = TRUE)
})

test_that("the page benchmark has a publication-grade contract", {
  benchmark_file <- testthat::test_path(
    "..",
    "bench",
    "benchmark_viewer_1m_pages.R"
  )
  skip_if_not(
    file.exists(benchmark_file),
    "benchmark tree not present (expected when checking a built package)"
  )
  benchmark <- paste(readLines(benchmark_file, warn = FALSE), collapse = "\n")

  expect_no_match(benchmark, "requestAnimationFrame", fixed = TRUE)
  expect_no_match(benchmark, "!isTRUE(first) ||", fixed = TRUE)
  expect_match(benchmark, "cerebro:specialist-state", fixed = TRUE)
  expect_match(benchmark, "cerebro:linkedviews-ready", fixed = TRUE)
  expect_match(benchmark, "cerebroLinkedViewsState.ready()", fixed = TRUE)
  expect_match(benchmark, "run_observation", fixed = TRUE)
  expect_match(benchmark, "arm_and_click_page <- function", fixed = TRUE)
  expect_no_match(benchmark, "arm_page <- function", fixed = TRUE)
  expect_match(benchmark, "const generation", fixed = TRUE)
  expect_match(benchmark, "performance.now()", fixed = TRUE)
  expect_match(benchmark, "e.timeStamp >= clickStart", fixed = TRUE)
  expect_match(
    benchmark,
    "open_page(app, page, require_event = TRUE)",
    fixed = TRUE
  )
  expect_match(benchmark, "requires_ready_event(", fixed = TRUE)
  expect_match(benchmark, "ready_event_required", fixed = TRUE)
  expect_no_match(benchmark, "Network$enable", fixed = TRUE)
  expect_no_match(benchmark, "Network$webSocketFrame", fixed = TRUE)
  expect_no_match(benchmark, "frame_payload_bytes", fixed = TRUE)
  expect_match(benchmark, "Shiny.shinyapp.$socket", fixed = TRUE)
  expect_match(benchmark, "TextEncoder", fixed = TRUE)
  expect_match(benchmark, "ArrayBuffer.isView", fixed = TRUE)
  expect_match(benchmark, "data instanceof Blob", fixed = TRUE)
  expect_match(benchmark, "removeEventListener('message'", fixed = TRUE)
  expect_match(benchmark, "socket.send=meter.originalSend", fixed = TRUE)
  canvas_spec <- sub(
    "(?s).*?(canvas_page <- function.*?)(?=\\n\\npages <- list).*",
    "\\1",
    benchmark,
    perl = TRUE
  )
  expect_match(canvas_spec, "wait_idle = FALSE", fixed = TRUE)
  expect_match(
    canvas_spec,
    "/^md5-cell-set-v1:[0-9a-f]{32}$/.test(",
    fixed = TRUE
  )
  expect_no_match(canvas_spec, "datasetFingerprint.length>0", fixed = TRUE)
  fingerprint_pattern <- "^md5-cell-set-v1:[0-9a-f]{32}$"
  expect_true(grepl(
    fingerprint_pattern,
    "md5-cell-set-v1:0123456789abcdef0123456789abcdef"
  ))
  expect_false(grepl(fingerprint_pattern, "0:811c9dc5:85ebca77"))
  for (name in c(
    "overview",
    "gene_expression",
    "immune_repertoire",
    "trajectory",
    "hla",
    "spatial"
  )) {
    expect_match(
      benchmark,
      paste0(name, " = canvas_page("),
      fixed = TRUE,
      info = name
    )
  }
  coordinated_spec <- substring(
    benchmark,
    regexpr("coordinated_views = page(", benchmark, fixed = TRUE)
  )
  expect_match(coordinated_spec, "wait_idle = FALSE", fixed = TRUE)
  atomic_click <- sub(
    "(?s).*?(arm_and_click_page <- function.*?)(?=\\n\\npage_available).*",
    "\\1",
    benchmark,
    perl = TRUE
  )
  expect_equal(
    lengths(regmatches(atomic_click, gregexpr("app\\$run_js", atomic_click))),
    1L
  )
  expect_match(
    benchmark,
    "(?s)coordinated_views = page\\(.*?required = TRUE.*?ready_event = ",
    perl = TRUE
  )
  expect_match(
    benchmark,
    'results$status == "ok" & results$performance_applicable & !results$pass',
    fixed = TRUE
  )
  expect_equal(
    lengths(gregexpr(
      "expected_points = expected_cells",
      benchmark,
      fixed = TRUE
    )),
    3L
  )
  immune_spec <- regmatches(
    benchmark,
    regexpr(
      "immune_repertoire = canvas_page\\([\\s\\S]{0,300}?\\n  \\),",
      benchmark,
      perl = TRUE
    )
  )
  expect_length(immune_spec, 1L)
  expect_no_match(immune_spec, "expected_points", fixed = TRUE)
  expect_match(benchmark, "VIEWER_EXPECTED_CELLS", fixed = TRUE)
  expect_match(benchmark, "plot.data.length>0", fixed = TRUE)
  expect_match(benchmark, "state?.summary?.()", fixed = TRUE)
  expect_match(benchmark, "link.offsetParent !== null", fixed = TRUE)
  expect_match(benchmark, "app$get_screenshot", fixed = TRUE)
  expect_match(benchmark, "png::readPNG", fixed = TRUE)
  protocol <- paste(
    readLines(
      testthat::test_path("..", "bench", "viewer_1m_page_protocol.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  benchmark_contract <- paste(benchmark, protocol, sep = "\n")

  for (field in c(
    "candidate_git_sha",
    "artifact_sha256",
    "host",
    "r_version",
    "package_version",
    "chrome_version",
    "r_peak_rss_kib",
    "chrome_peak_rss_kib",
    "js_heap_used_bytes",
    "websocket_sent_payload_bytes",
    "websocket_received_payload_bytes",
    "correctness_pass",
    "rendered_point_count",
    "expected_point_count",
    "correctness_detail",
    "navigator_gpu",
    "renderer_backend",
    "renderer_adapter",
    "renderer_context_lost",
    "renderer_error",
    "visible_pixel_count",
    "visible_pixels_pass",
    "requires_webgpu",
    "performance_applicable"
  )) {
    expect_match(benchmark_contract, field, fixed = TRUE, info = field)
  }
})

test_that("benchmark TSV output escapes controls and validates columns", {
  protocol_file <- testthat::test_path(
    "..",
    "bench",
    "viewer_1m_page_protocol.R"
  )
  protocol <- new.env(parent = baseenv())
  sys.source(protocol_file, envir = protocol)
  output <- tempfile(fileext = ".tsv")
  rows <- data.frame(
    status = "error",
    error = "first line\nsecond\tfield\rreturn",
    correctness_detail = "{\n\t\"reason\": \"bad\"\n}",
    stringsAsFactors = FALSE
  )

  expect_silent(protocol$write_validated_tsv(rows, output))

  physical_lines <- readLines(output, warn = FALSE)
  expect_length(physical_lines, 2L)
  expect_true(all(
    vapply(
      physical_lines,
      protocol$tsv_field_count,
      integer(1)
    ) ==
      ncol(rows)
  ))
  expect_match(
    physical_lines[[2L]],
    "first line\\nsecond\\tfield\\rreturn",
    fixed = TRUE
  )
  expect_match(
    physical_lines[[2L]],
    "{\\n\\t\"reason\": \"bad\"\\n}",
    fixed = TRUE
  )
})

test_that("benchmark provenance identifies sidecars and Viewer Packs", {
  protocol_file <- testthat::test_path(
    "..",
    "bench",
    "viewer_1m_page_protocol.R"
  )
  protocol <- new.env(parent = baseenv())
  sys.source(protocol_file, envir = protocol)
  root <- tempfile("benchmark-artifacts-")
  dir.create(root)
  sidecar <- file.path(root, "fixture.bpcells")
  pack <- file.path(root, "fixture.viewer")
  dir.create(sidecar)
  dir.create(pack)
  writeLines(c("cell-1", "cell-2"), file.path(sidecar, "col_names"))
  writeBin(as.raw(c(1L, 2L, 3L)), file.path(sidecar, "val_data"))
  writeLines('{"schema_version":1}', file.path(pack, "manifest.json"))
  object <- new.env(parent = emptyenv())
  object$expression_backend <- list(
    type = "bpcells",
    location = basename(sidecar)
  )
  crb <- file.path(root, "fixture.crb")
  saveRDS(object, crb)

  manifest <- protocol$benchmark_artifact_provenance(crb)

  expect_identical(manifest$bpcells_sidecar, normalizePath(sidecar))
  expect_match(manifest$bpcells_sidecar_sha256, "^[0-9a-f]{64}$")
  expect_identical(
    manifest$viewer_pack_manifest,
    normalizePath(file.path(pack, "manifest.json"))
  )
  expect_match(manifest$viewer_pack_manifest_sha256, "^[0-9a-f]{64}$")
})

test_that("the page benchmark schedule and budgets are balanced", {
  protocol_file <- testthat::test_path(
    "..",
    "bench",
    "viewer_1m_page_protocol.R"
  )
  skip_if_not(
    file.exists(protocol_file),
    "benchmark tree not present (expected when checking a built package)"
  )
  protocol <- new.env(parent = baseenv())
  sys.source(protocol_file, envir = protocol)

  schedule <- protocol$build_balanced_schedule(
    c("baseline", "candidate"),
    c("overview", "trajectory"),
    5L
  )
  units <- split(
    schedule,
    interaction(schedule$round, schedule$page, schedule$visit, drop = TRUE)
  )
  expect_true(all(vapply(
    units,
    function(unit) setequal(unit$candidate, c("baseline", "candidate")),
    logical(1)
  )))
  page_visits <- split(
    schedule,
    interaction(schedule$page, schedule$visit, drop = TRUE)
  )
  expect_true(all(vapply(
    page_visits,
    function(rows) {
      positions <- table(rows$candidate, rows$candidate_position)
      max(positions) - min(positions) <= 1L
    },
    logical(1)
  )))

  expect_error(
    protocol$validate_page_profile("publication", 4L),
    "at least 5 rounds"
  )
  expect_silent(protocol$validate_page_profile("publication", 5L))
  expect_true(protocol$page_budget_pass(1999, 2000))
  expect_false(protocol$page_budget_pass(2000, 2000))
  expect_true(protocol$requires_ready_event(
    "coordinated_views",
    "first",
    warmed = FALSE
  ))
  expect_true(protocol$requires_ready_event(
    "coordinated_views",
    "repeat",
    warmed = FALSE
  ))
  expect_false(protocol$requires_ready_event(
    "coordinated_views",
    "repeat",
    warmed = TRUE
  ))
  expect_true(protocol$requires_ready_event(
    "trajectory",
    "repeat",
    warmed = TRUE
  ))
})

test_that("the cold-start benchmark measures an installed Viewer", {
  benchmark_file <- testthat::test_path(
    "..",
    "bench",
    "benchmark_million_cell_startup.R"
  )
  skip_if_not(
    file.exists(benchmark_file),
    "benchmark tree not present (expected when checking a built package)"
  )
  benchmark <- paste(
    readLines(benchmark_file, warn = FALSE),
    collapse = "\n"
  )

  expect_match(benchmark, "library(CerebroNexus)", fixed = TRUE)
  expect_no_match(benchmark, "load_all", fixed = TRUE)
  expect_match(benchmark, "library_ms", fixed = TRUE)
  expect_match(benchmark, "browser_load_ms", fixed = TRUE)
  expect_match(benchmark, "load_to_data_ms", fixed = TRUE)
  expect_match(benchmark, "browser_to_data_ms", fixed = TRUE)
  expect_match(benchmark, "process_to_data_ms", fixed = TRUE)
  expect_match(benchmark, "CEREBRO_STARTUP_GATE_LABEL", fixed = TRUE)
  expect_match(
    benchmark,
    "CEREBRO_STARTUP_MAX_PROCESS_TO_DATA_MS",
    fixed = TRUE
  )
  expect_match(benchmark, "observed_ms >= gate_ms", fixed = TRUE)
})

test_that("optional page servers load only when their page becomes visible", {
  server <- paste(
    readLines(viewer_test_path("shiny_server.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(server, "deferred_viewer_server_files <- c(", fixed = TRUE)
  expect_match(server, '"marker_genes/server.R"', fixed = TRUE)
  expect_match(server, '"color_management/server.R"', fixed = TRUE)
  expect_match(
    server,
    "load_deferred_viewer_server <- function(server_file)",
    fixed = TRUE
  )
  expect_match(
    server,
    'load_deferred_viewer_server("coordinated_views/server.R")',
    fixed = TRUE
  )
  expect_match(
    server,
    'server_file <- unname(deferred_viewer_server_files[input[["sidebar"]]])',
    fixed = TRUE
  )
  expect_match(
    server,
    "if (length(server_file) && !is.na(server_file))",
    fixed = TRUE
  )
  expect_no_match(
    server,
    'deferred_viewer_server_files[[input[["sidebar"]]]]',
    fixed = TRUE
  )
  expect_match(server, "ignoreInit\\s*=\\s*FALSE\\s*\\)")
  expect_no_match(server, "preload_deferred_viewer_server", fixed = TRUE)
  expect_no_match(server, "later::later(", fixed = TRUE)
  expect_no_match(
    server,
    "for (server_file in deferred_viewer_server_files)",
    fixed = TRUE
  )
  expect_match(server, "envir = server_scope", fixed = TRUE)
})

test_that("continuous colours keep stable paint order without comparison sort", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  source <- viewer_test_path("www", "cell_views.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      sprintf(
        "const source = fs.readFileSync(%s, 'utf8');",
        encodeString(source, quote = '"')
      ),
      "const fn = source.match(/function quantisedOrder\\(vals, span\\) \\{[\\s\\S]*?\\n  \\}/)[0];",
      "eval(fn);",
      "const values = [2, null, 1, 2, NaN, 0, 1];",
      "process.stdout.write(JSON.stringify(Array.from(quantisedOrder(values, 2))));"
    ),
    runner
  )
  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)

  expect_equal(attr(output, "status"), NULL)
  expect_identical(jsonlite::fromJSON(output), c(1L, 4L, 5L, 2L, 6L, 0L, 3L))
})

test_that("WebGPU RGB colours match the existing blend without CSS allocation", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  source <- viewer_test_path("www", "cell_views.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      sprintf(
        "const source = fs.readFileSync(%s, 'utf8');",
        encodeString(source, quote = '"')
      ),
      "const fn = source.match(/function rgbGpuColor\\(r, g, b\\) \\{[\\s\\S]*?\\n  \\}/)[0];",
      "const RGB_MIN=28, RGB_GREY_RGB=[217,219,222]; eval(fn);",
      "function old(r,g,b){const m=Math.max(r,g,b);let out;",
      "if(m<=RGB_MIN)out=RGB_GREY_RGB;else if(r>RGB_MIN&&g>RGB_MIN&&b>RGB_MIN)out=[0,0,0];",
      "else{const t=m/255;out=[r,g,b].map((v,i)=>Math.round(RGB_GREY_RGB[i]+(v/m*255-RGB_GREY_RGB[i])*t));}",
      "return out[0]|out[1]<<8|out[2]<<16|(m>RGB_MIN?0x1000000:0);}",
      "for(let r=0;r<256;r+=17)for(let g=0;g<256;g+=17)for(let b=0;b<256;b+=17)",
      "if(rgbGpuColor(r,g,b)!==old(r,g,b))process.exit(1);"
    ),
    runner
  )
  status <- system2("node", runner)

  expect_identical(status, 0L)
})

test_that("Gene projection delegates paint order without copying cell vectors", {
  runtime <- new.env(parent = globalenv())
  captured <- new.env(parent = emptyenv())
  runtime$expressionColorScale <- function(...) "scale"
  runtime$expressionReverseColorScale <- function(...) FALSE
  runtime$cerebroCellViewRender <- function(
    id,
    meta,
    data,
    hover,
    extra,
    deferred_aux
  ) {
    captured$data <- data
    captured$deferred_aux <- deferred_aux
  }
  sys.source(
    viewer_test_path("gene_expression", "func_projection_update_plot.R"),
    envir = runtime
  )
  input <- list(
    coordinates = data.frame(x = c(3, 1, 2), y = c(6, 4, 5)),
    reset_axes = FALSE,
    expression_levels = c(30, 10, 20),
    plot_parameters = list(
      draw_border = FALSE,
      keep_square = TRUE,
      plot_order = "Highest expression on top",
      point_size = 1,
      point_opacity = 0.5,
      x_range = c(1, 3),
      y_range = c(4, 6),
      is_trajectory = FALSE,
      hover_info = FALSE,
      projection = "UMAP",
      n_dimensions = 2L
    ),
    color_settings = list(
      color_scale = "Viridis",
      color_mode = "same",
      color_range = NULL,
      genes = "GeneA"
    ),
    metadata = data.frame(cell_barcode = c("c3", "c1", "c2")),
    trajectory = list(),
    display_mode = "single",
    separate_panels = FALSE
  )

  runtime$expression_projection_update_plot(input)

  expect_identical(captured$data$x, c(3, 1, 2))
  expect_identical(captured$data$color, c(30, 10, 20))
  expect_identical(captured$data$selection_key, 1:3)
  expect_identical(
    as.character(captured$deferred_aux()$selection_key),
    c("c3", "c1", "c2")
  )
  expect_identical(captured$data$paint_order, "highest")
  input$plot_parameters$plot_order <- "Random"
  runtime$expression_projection_update_plot(input)
  expect_identical(captured$data$paint_order, "natural")
})

test_that("hidden group filters activate after startup rendering", {
  source <- paste(
    readLines(
      viewer_test_path(
        "module",
        "group_filters",
        "group_filters_widget.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(source, "domain$onFlushed(", fixed = TRUE)
  expect_match(source, "delay = 0.5", fixed = TRUE)
  expect_match(source, "suspendWhenHidden = FALSE", fixed = TRUE)
})
