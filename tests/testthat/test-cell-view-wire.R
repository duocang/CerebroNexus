wire_file <- viewer_test_path("www", "cell_views_wire.js")
bundle_file <- viewer_test_path("coordinated_views", "bundle.R")
utility_file <- viewer_test_path("utility_functions.R")
message_file <- viewer_test_path("core", "cell_view_message.R")
scatter_file <- viewer_test_path("core", "cell_view_scatter.R")
codec_file <- viewer_test_path("core", "cell_view_wire.R")

wire_header <- function(payload) {
  header_length <- sum(as.integer(payload[seq_len(4L)]) * 256^(0:3))
  jsonlite::fromJSON(
    rawToChar(payload[4L + seq_len(header_length)]),
    simplifyVector = FALSE
  )
}

test_that("the isolated cell-view codec preserves the existing wire bytes", {
  skip_if_not_installed("jsonlite")
  legacy <- new.env(parent = globalenv())
  isolated <- new.env(parent = globalenv())
  sys.source(utility_file, envir = legacy)
  sys.source(utility_file, envir = isolated)
  sys.source(codec_file, envir = isolated)

  message <- list(
    id = "wire-equivalence",
    meta = list(traces = list("A", "B")),
    data = list(
      x = I(c(1.25, NA_real_, -2.5, 3.75)),
      y = I(c(4.5, 5.5, 6.5, 7.5)),
      group = I(c(0L, 1L, NA_integer_, 1L)),
      selection_key = I(c("a", "b", "c", "d"))
    )
  )
  expect_identical(
    isolated$cv_wire_pack_message(message, min_length = 1L),
    legacy$cv_wire_pack_message(message, min_length = 1L)
  )
  expect_identical(
    isolated$cv_wire_pack_cells("dataset-a", c("a", "b", NA_character_)),
    legacy$cv_wire_pack_cells("dataset-a", c("a", "b", NA_character_))
  )
})

test_that("the isolated message normalizer preserves the existing contract", {
  legacy <- new.env(parent = globalenv())
  isolated <- new.env(parent = globalenv())
  sys.source(utility_file, envir = legacy)
  sys.source(utility_file, envir = isolated)
  sys.source(message_file, envir = isolated)

  arguments <- list(
    id = "message-equivalence",
    meta = list(color_type = "categorical", traces = c("A", "B")),
    data = list(
      x = list(c(1, 2), c(3, 4)),
      y = list(c(5, 6), c(7, 8)),
      selection_key = list(1:2, 3:4),
      color = list("#111111", "#222222")
    ),
    hover = list(
      hoverinfo = "text",
      columns = list(list(label = "State", values = list(1:2, 3:4)))
    ),
    extra = list(edges = list(x0 = c(1, 2), y0 = c(3, 4)))
  )
  expect_identical(
    do.call(isolated$cerebroCellViewMessage, arguments),
    do.call(legacy$cerebroCellViewMessage, arguments)
  )
})

test_that("the isolated scatter builder preserves canonical payloads", {
  legacy <- new.env(parent = globalenv())
  isolated <- new.env(parent = globalenv())
  sys.source(utility_file, envir = legacy)
  sys.source(utility_file, envir = isolated)
  sys.source(scatter_file, envir = isolated)

  n <- 5000L
  arguments <- list(
    coordinates = list(seq_len(n) + 0.25, seq_len(n) + 0.75),
    color = factor(
      rep(c("B", "A", NA_character_), length.out = n),
      levels = c("A", "B", "unused")
    ),
    color_variable = "state",
    selection_keys = seq_len(n),
    point_size = 2,
    point_opacity = 0.8,
    color_assignments = c(A = "#111111", B = "#222222", unused = "#333333"),
    hover = FALSE
  )
  expect_identical(
    do.call(isolated$cerebroCellViewScatterPayload, arguments),
    do.call(legacy$cerebroCellViewScatterPayload, arguments)
  )
})

test_that("the browser restores compact linked-view vectors", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  skip_if_not_installed("base64enc")
  skip_if_not_installed("jsonlite")
  expect_true(file.exists(wire_file), info = "cell_views_wire.js not found")
  if (!file.exists(wire_file)) {
    return(invisible(NULL))
  }

  helpers <- new.env(parent = globalenv())
  sys.source(utility_file, envir = helpers)
  sys.source(bundle_file, envir = helpers)
  packed <- helpers$cv_wire_pack_bundle(
    list(
      cells = I(c("cell-1", "cell-2")),
      groups = list(
        cluster = helpers$cv_group(c(0L, NA_integer_), "A", "#fff")
      ),
      cat_extra = list(
        batch = helpers$cv_group(
          c(0L, 1L),
          c("A", "B"),
          c("#fff", "#000")
        )
      ),
      fields = list(score = helpers$cv_field("Score", c(0L, 1000L), 0, 1)),
      projections = list(
        umap = list(x = I(c(1.25, NA_real_)), y = I(c(-2.5, 3.75)), ndim = 2L)
      ),
      spaces = list(helpers$cv_space("spatial", "Spatial", c(4, 5), c(6, 7)))
    ),
    min_length = 1L
  )
  header <- wire_header(packed)
  expect_identical(header$groups$cluster$values$`__cv_wire__`, "i32")
  expect_identical(header$cat_extra$batch$values$`__cv_wire__`, "i8")
  expect_identical(header$fields$score$v$`__cv_wire__`, "i16")
  payload <- tempfile(fileext = ".json")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(c(payload, runner)), add = TRUE)
  writeBin(packed, payload)
  writeLines(
    c(
      "const fs = require('fs');",
      "global.window = global;",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(wire_file, quote = "\"")
      ),
      sprintf(
        "const input = fs.readFileSync(%s);",
        encodeString(payload, quote = "\"")
      ),
      "const buffer = input.buffer.slice(input.byteOffset, input.byteOffset + input.byteLength);",
      "console.log(JSON.stringify(window.CBViewWire.unpack(buffer), (_key, value) => ArrayBuffer.isView(value) ? Array.from(value) : value));"
    ),
    runner
  )
  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_equal(attr(output, "status"), NULL)
  restored <- jsonlite::fromJSON(output, simplifyVector = FALSE)

  expect_identical(restored$wire_format, "binary-v1")
  expect_identical(unlist(restored$cells), c("cell-1", "cell-2"))
  expect_identical(restored$groups$cluster$values[[1L]], 0L)
  expect_null(restored$groups$cluster$values[[2L]])
  expect_identical(unlist(restored$cat_extra$batch$values), c(0L, 1L))
  expect_identical(unlist(restored$fields$score$v), c(0L, 1000L))
  expect_identical(restored$projections$umap$x[[1L]], 1.25)
  expect_null(restored$projections$umap$x[[2L]])
  expect_equal(unlist(restored$projections$umap$y), c(-2.5, 3.75))
})

test_that("cell identities travel separately from the first frame", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  skip_if_not_installed("jsonlite")

  helpers <- new.env(parent = globalenv())
  sys.source(utility_file, envir = helpers)
  sys.source(bundle_file, envir = helpers)
  payload <- tempfile(fileext = ".bin")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(c(payload, runner)), add = TRUE)
  writeBin(
    helpers$cv_wire_pack_cells("dataset-1", c("cell-1", "cell-2")),
    payload
  )
  writeLines(
    c(
      "const fs = require('fs');",
      "global.window = global;",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(wire_file, quote = "\"")
      ),
      sprintf(
        "const input = fs.readFileSync(%s);",
        encodeString(payload, quote = "\"")
      ),
      "const buffer = input.buffer.slice(input.byteOffset, input.byteOffset + input.byteLength);",
      "console.log(JSON.stringify(window.CBViewWire.unpackCells(buffer)));"
    ),
    runner
  )
  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_equal(attr(output, "status"), NULL)
  restored <- jsonlite::fromJSON(output, simplifyVector = FALSE)
  expect_identical(restored$dataset_id, "dataset-1")
  expect_identical(unlist(restored$cells), c("cell-1", "cell-2"))
})

test_that("specialist cell views use the same binary envelope", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  skip_if_not_installed("jsonlite")

  helpers <- new.env(parent = globalenv())
  sys.source(utility_file, envir = helpers)
  sys.source(bundle_file, envir = helpers)
  packed <- helpers$cv_wire_pack_message(
    list(
      id = "overview_projection",
      data = list(
        x = I(c(1.25, NA_real_)),
        group = I(c(1L, NA_integer_)),
        selection_key = I(c("cell-1", "cell-2"))
      )
    ),
    min_length = 1L
  )
  payload <- tempfile(fileext = ".bin")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(c(payload, runner)), add = TRUE)
  writeBin(packed, payload)
  writeLines(
    c(
      "const fs = require('fs');",
      "global.window = global;",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(wire_file, quote = "\"")
      ),
      sprintf(
        "const input = fs.readFileSync(%s);",
        encodeString(payload, quote = "\"")
      ),
      "const buffer = input.buffer.slice(input.byteOffset, input.byteOffset + input.byteLength);",
      "console.log(JSON.stringify(window.CBViewWire.unpack(buffer), (_key, value) => ArrayBuffer.isView(value) ? Array.from(value) : value));"
    ),
    runner
  )
  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_equal(attr(output, "status"), NULL)
  restored <- jsonlite::fromJSON(output, simplifyVector = FALSE)
  expect_identical(restored$id, "overview_projection")
  expect_equal(restored$data$x[[1L]], 1.25)
  expect_null(restored$data$x[[2L]])
  expect_identical(restored$data$group[[1L]], 1L)
  expect_null(restored$data$group[[2L]])
  expect_identical(unlist(restored$data$selection_key), c("cell-1", "cell-2"))
})

test_that("large specialist views send their first frame before hover data", {
  skip_if_not_installed("jsonlite")
  runtime <- new.env(parent = globalenv())
  sys.source(utility_file, envir = runtime)
  sent <- list()
  runtime$input <- list(coordviews_wire_supported = TRUE)
  runtime$viewerDatasetIdentity <- function() {
    list(
      cell_count = 4096L,
      fingerprint = "md5-cell-set-v1:0123456789abcdef0123456789abcdef",
      order_fingerprint = "md5-cell-order-v1:fedcba9876543210fedcba9876543210"
    )
  }
  runtime$session <- list(
    sendBinaryMessage = function(type, payload) {
      sent[[length(sent) + 1L]] <<- list(type = type, payload = payload)
    },
    sendCustomMessage = function(type, payload) {
      sent[[length(sent) + 1L]] <<- list(type = type, payload = payload)
    }
  )
  keys <- sprintf("cell-%04d", seq_len(4096L))

  runtime$cerebroCellViewRender(
    "overview_projection",
    meta = list(color_type = "categorical", traces = "A"),
    data = list(
      x = list(I(as.numeric(seq_along(keys)))),
      y = list(I(as.numeric(seq_along(keys)))),
      selection_key = list(I(keys)),
      color = list("#123456")
    ),
    hover = list(text = list(I(keys)), hoverinfo = "text")
  )

  expect_identical(vapply(sent, `[[`, character(1), "type"), "cell_view_binary")
  first <- wire_header(sent[[1L]]$payload)
  auxiliary <- runtime$.cerebro_cell_view_aux_pending[[
    paste("overview_projection", first$data$wire_token, sep = ":")
  ]]
  expect_identical(first$data$n, 4096L)
  expect_identical(
    first$dataset_identity,
    list(
      cell_count = 4096L,
      cell_fingerprint = "md5-cell-set-v1:0123456789abcdef0123456789abcdef",
      cell_order_fingerprint = "md5-cell-order-v1:fedcba9876543210fedcba9876543210"
    )
  )
  expect_null(first$data$selection_key)
  expect_identical(first$data$x[[1L]]$`__cv_wire__`, "f32")
  expect_identical(first$hover$hoverinfo, "skip")
  expect_identical(auxiliary$id, "overview_projection")
  expect_identical(unlist(auxiliary$selection_key, use.names = FALSE), keys)
})

test_that("deferred specialist IDs can declare trace lengths without placeholders", {
  skip_if_not_installed("jsonlite")
  runtime <- new.env(parent = globalenv())
  sys.source(utility_file, envir = runtime)
  sent <- list()
  runtime$input <- list(coordviews_wire_supported = TRUE)
  runtime$viewerDatasetIdentity <- function() {
    list(
      cell_count = 4096L,
      fingerprint = "dataset-a",
      order_fingerprint = "order-a"
    )
  }
  runtime$session <- list(sendBinaryMessage = function(type, payload) {
    sent[[length(sent) + 1L]] <<- list(type = type, payload = payload)
  })
  trace_lengths <- c(2500L, 1596L)
  keys <- sprintf("cell-%04d", seq_len(sum(trace_lengths)))

  runtime$cerebroCellViewRender(
    "ir_clonalUMAP_projection",
    meta = list(color_type = "categorical", traces = c("Other", "Single")),
    data = list(
      x = list(seq_len(trace_lengths[[1L]]), seq_len(trace_lengths[[2L]])),
      y = list(seq_len(trace_lengths[[1L]]), seq_len(trace_lengths[[2L]])),
      deferred_selection_lengths = trace_lengths,
      color = list("#d9d9d9", "#123456")
    ),
    deferred_aux = function() {
      list(
        selection_key = list(
          keys[seq_len(trace_lengths[[1L]])],
          keys[trace_lengths[[1L]] + seq_len(trace_lengths[[2L]])]
        ),
        hover = list(hoverinfo = "skip")
      )
    }
  )

  expect_identical(length(sent), 1L)
  first <- wire_header(sent[[1L]]$payload)
  expect_identical(first$data$n, 4096L)
  expect_null(first$data$selection_key)
  expect_null(first$data$deferred_selection_lengths)
  auxiliary <- runtime$.cerebro_cell_view_aux_pending[[
    paste(
      "ir_clonalUMAP_projection",
      first$data$wire_token,
      sep = ":"
    )
  ]]
  expect_true(is.function(auxiliary$build))
  expect_identical(
    unlist(auxiliary$build()$selection_key, use.names = FALSE),
    keys
  )
})

test_that("specialist views can reference validated shared coordinates", {
  skip_if_not_installed("jsonlite")
  runtime <- new.env(parent = globalenv())
  sys.source(utility_file, envir = runtime)

  has_contract <- all(
    c(
      "viewerSharedProjectionName",
      "cerebroCellViewRender"
    ) %in%
      ls(runtime)
  )
  expect_true(has_contract)
  if (!has_contract) {
    return(invisible(NULL))
  }

  identity <- list(
    cell_count = 4096L,
    fingerprint = "dataset-a",
    order_fingerprint = "order-a"
  )
  shared <- list(
    dataset_fingerprint = "dataset-a",
    cell_count = 4096L,
    canonical_order_id = "order-a",
    projection = "umap"
  )
  expect_identical(
    runtime$viewerSharedProjectionName("umap", 4096L, shared, identity),
    "umap"
  )
  expect_null(runtime$viewerSharedProjectionName(
    "umap",
    4096L,
    within(shared, dataset_fingerprint <- "dataset-b"),
    identity
  ))

  sent <- list()
  runtime$input <- list(
    coordviews_wire_supported = TRUE,
    coordviews_shared_base = shared
  )
  runtime$viewerDatasetIdentity <- function() identity
  runtime$session <- list(sendBinaryMessage = function(type, payload) {
    sent[[length(sent) + 1L]] <<- list(type = type, payload = payload)
  })
  keys <- sprintf("cell-%04d", seq_len(4096L))
  runtime$cerebroCellViewRender(
    "expression_projection",
    meta = list(color_type = "continuous", space_label = "umap"),
    data = list(
      x = seq_len(4096L),
      y = rev(seq_len(4096L)),
      color = rep(0, 4096L),
      selection_key = keys,
      shared_zero_color = TRUE
    )
  )

  first <- wire_header(sent[[1L]]$payload)
  expect_identical(first$shared_projection, "umap")
  expect_null(first$data$x)
  expect_null(first$data$y)
  expect_null(first$data$color)
  expect_true(first$data$zero_color)
  expect_identical(first$data$n, 4096L)

  sent <- list()
  runtime$input$coordviews_shared_base <- NULL
  runtime$cerebroCellViewRender(
    "expression_projection",
    meta = list(color_type = "continuous", space_label = "umap"),
    data = list(
      x = seq_len(4096L),
      y = rev(seq_len(4096L)),
      color = numeric(),
      selection_key = keys,
      shared_zero_color = TRUE
    )
  )
  unshared <- wire_header(sent[[1L]]$payload)
  expect_null(unshared$shared_projection)
  expect_false(is.null(unshared$data$x))
  expect_false(is.null(unshared$data$y))
  expect_null(unshared$data$color)
  expect_true(unshared$data$zero_color)
  expect_identical(unshared$data$n, 4096L)
})

test_that("specialist selections wait for stable IDs and replay after aux", {
  javascript <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )
  config <- paste(
    readLines(viewer_test_path("www", "coordviews-config.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(
    javascript,
    "CBViewState.specialistSelectionReport",
    fixed = TRUE
  )
  expect_match(
    javascript,
    "pendingStableSelection ? null",
    fixed = TRUE
  )
  expect_match(
    javascript,
    "onSingleAuxBinary[\\s\\S]+D.cells = cells;[\\s\\S]+reportSelection\\(\\);",
    perl = TRUE
  )
  expect_match(
    javascript,
    "selectedCells: specialistReport",
    fixed = TRUE
  )
  expect_match(
    javascript,
    "!/^md5-cell-set-v1:[0-9a-f]{32}$/.test(datasetFingerprint)",
    fixed = TRUE
  )
  expect_match(
    javascript,
    "D.dataset_fingerprint || D.cell_fingerprint",
    fixed = TRUE
  )
  expect_match(javascript, "attachDatasetIdentity:", fixed = TRUE)
  expect_match(
    config,
    "cellViews.attachDatasetIdentity(identity)",
    fixed = TRUE
  )
  config_boot <- strsplit(config, "function boot() {", fixed = TRUE)[[1L]][[2L]]
  expect_lt(
    regexpr("connectShiny();", config_boot, fixed = TRUE)[[1L]],
    regexpr("if (!dialog || !open) return;", config_boot, fixed = TRUE)[[1L]]
  )
})

test_that("dataset changes invalidate cached specialist plots", {
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
      "const resetStart = source.indexOf('  function resetSingleViews');",
      "const resetEnd = source.indexOf('  function mountSingleSurface', resetStart);",
      "const attachStart = source.indexOf('  function attachSingleDatasetIdentity');",
      "const attachEnd = source.indexOf('  function reportSingleHiddenGroups', attachStart);",
      "let restored = 0;",
      "function restoreLinkedSurface() { restored += 1; }",
      "function reportSelection() {}",
      "let singleViews = {overview_projection:{datasetIdentity:{cell_fingerprint:'dataset-a'}}};",
      "let singleActive = null;",
      "let singleRequests = new Set(['overview_projection']);",
      "let singleSpaceIds = ['umap'];",
      "let singleSpaceModes = {umap:'cluster'};",
      "let singleIndexCells = ['a'];",
      "let singleIndexMap = new Map([['a', 0]]);",
      "let linkedState = {dataset:'a'};",
      "let D = {dataset_fingerprint:'dataset-a'};",
      "eval(source.slice(resetStart, resetEnd));",
      "eval(source.slice(attachStart, attachEnd));",
      "attachSingleDatasetIdentity({cell_fingerprint:'dataset-b'});",
      "console.log(JSON.stringify({views:Object.keys(singleViews),active:singleActive,requests:singleRequests.size,restored:restored}));"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(views = list(), active = NULL, requests = 0L, restored = 1L)
  )
})

test_that("JSON specialist payloads retain dataset identity", {
  javascript <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )
  handler <- strsplit(
    strsplit(
      javascript,
      "Shiny.addCustomMessageHandler('cell_view_render'",
      fixed = TRUE
    )[[1L]][[2L]],
    "Shiny.addCustomMessageHandler('cell_view_background'",
    fixed = TRUE
  )[[1L]][[1L]]

  expect_match(handler, "message.dataset_identity", fixed = TRUE)
})

test_that("zero-color specialist frames reuse shared browser geometry", {
  javascript <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(
    javascript,
    "zero_color[\\s\\S]+paintOrder: false[\\s\\S]+constantColor:",
    perl = TRUE
  )
  expect_match(javascript, "previous.unit", fixed = TRUE)
  expect_match(javascript, "gpuPositionCount", fixed = TRUE)
})
