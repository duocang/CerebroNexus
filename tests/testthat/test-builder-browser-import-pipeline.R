builder_browser_pipeline_text <- function(...) {
  paste(
    readLines(builder_profile_inst_path("builder", ...), warn = FALSE),
    collapse = "\n"
  )
}

builder_browser_pipeline_runtime <- function() {
  runtime <- new.env(parent = globalenv())
  sys.source(
    testthat::test_path(
      "..",
      "..",
      "inst",
      "viewer",
      "core",
      "spatial_coordinate_transform.R"
    ),
    envir = runtime
  )
  for (file in c("io.R", "worker.R", "extras.R", "project.R", "build.R")) {
    sys.source(
      testthat::test_path("..", "..", "inst", "builder", file),
      envir = runtime
    )
  }
  runtime
}

builder_browser_pipeline_js_function <- function(js, name) {
  marker <- paste0("function ", name, "(")
  start <- regexpr(marker, js, fixed = TRUE)[[1L]]
  if (start < 1L) {
    stop("JavaScript function not found: ", name, call. = FALSE)
  }
  candidate <- substring(js, start)
  open <- regexpr("{", candidate, fixed = TRUE)[[1L]]
  chars <- strsplit(candidate, "", fixed = TRUE)[[1L]]
  depth <- 0L
  for (index in seq.int(open, length(chars))) {
    if (identical(chars[[index]], "{")) {
      depth <- depth + 1L
    }
    if (identical(chars[[index]], "}")) {
      depth <- depth - 1L
    }
    if (depth == 0L) {
      return(paste0(chars[seq_len(index)], collapse = ""))
    }
  }
  stop("JavaScript function is incomplete: ", name, call. = FALSE)
}

builder_browser_pipeline_run_node <- function(functions, body) {
  testthat::skip_if(Sys.which("node") == "", "node not on PATH")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(c("'use strict';", functions, body), runner, useBytes = TRUE)
  system2("node", runner, stdout = TRUE, stderr = TRUE)
}

test_that("retained loader publishes its marker before registration", {
  runtime <- builder_browser_pipeline_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "shiny-upload.qs2")
  retained <- file.path(root, "session-sources", "ds1", "sample.qs2")
  marker <- file.path(root, ".transport-retained-ds1.rds")
  bytes <- charToRaw("browser-upload-bytes")
  writeBin(bytes, source)
  events <- character()

  loaded <- runtime$builder_project_load_retained_source(
    "ds1",
    list(
      source = source,
      retained_path = retained,
      transport_retained_path = marker
    ),
    progress = NULL,
    .copy = function(from, to) {
      events <<- c(events, "copy")
      expect_identical(dirname(to), dirname(retained))
      expect_match(basename(to), "[.]part$")
      file.copy(from, to, overwrite = TRUE, copy.mode = TRUE)
    },
    .transport_writer = function(path, id, retained_path) {
      events <<- c(events, "marker")
      expect_true(file.exists(retained_path))
      expect_identical(readBin(retained_path, "raw", n = 100L), bytes)
      runtime$builder_project_transport_retained_write(
        path,
        id,
        retained_path
      )
    },
    .adapter = function(path) list(path = path),
    .register = function(adapter, id, progress) {
      events <<- c(events, "register")
      expect_true(file.exists(marker))
      expect_identical(
        runtime$builder_project_transport_retained_read(marker, id),
        list(id = id, retained_path = normalizePath(retained))
      )
      list(adapter = adapter, id = id)
    }
  )

  expect_identical(events, c("copy", "marker", "register"))
  expect_identical(loaded$retained_path, normalizePath(retained))
  expect_identical(readBin(retained, "raw", n = 100L), bytes)
  expect_true(file.exists(source))
  expect_identical(readBin(source, "raw", n = 100L), bytes)
  expect_true(runtime$builder_project_transport_retained_remove(marker))
  expect_null(runtime$builder_project_transport_retained_read(marker, "ds1"))
  expect_true(runtime$builder_project_transport_retained_remove(marker))
})

test_that("sources without a browser transport path publish no marker", {
  runtime <- builder_browser_pipeline_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "local-source.qs2")
  retained <- file.path(root, "session-sources", "ds1", "local-source.qs2")
  writeBin(charToRaw("local-source"), source)
  marker_writes <- 0L

  runtime$builder_project_load_retained_source(
    "ds1",
    list(
      source = source,
      retained_path = retained,
      source_origin = "local"
    ),
    progress = NULL,
    .copy = function(from, to) file.copy(from, to, overwrite = TRUE),
    .transport_writer = function(...) {
      marker_writes <<- marker_writes + 1L
      FALSE
    },
    .adapter = identity,
    .register = function(adapter, id, progress) list(path = adapter, id = id)
  )

  expect_identical(marker_writes, 0L)
  expect_true(file.exists(source))
  expect_true(file.exists(retained))
})

test_that("browser uploads expose a distinct retained transport signal", {
  foundation <- builder_browser_pipeline_text("server", "foundation.R")
  imports <- builder_browser_pipeline_text("server", "imports.R")

  expect_match(
    foundation,
    "builder_import_queue(max_active = 1L)",
    fixed = TRUE
  )
  expect_match(
    foundation,
    '"builder_client_import_transport_release"',
    fixed = TRUE
  )
  expect_match(imports, "consume_transport_retained", fixed = TRUE)
  expect_match(imports, "transport_retained_path", fixed = TRUE)
  expect_match(imports, "builder_project_transport_retained_read", fixed = TRUE)
  expect_match(
    imports,
    "builder_project_transport_retained_remove",
    fixed = TRUE
  )
})

test_that("browser transport handoff is identity checked and advances the queue", {
  js <- builder_browser_pipeline_text("www", "builder.js")

  expect_match(js, "function handleClientImportTransportRelease", fixed = TRUE)
  expect_match(
    js,
    "message.client_id !== activeClientImport.clientId",
    fixed = TRUE
  )
  expect_match(
    js,
    "message.server_id !== activeClientImport.serverId",
    fixed = TRUE
  )
  expect_match(js, "clientImportQueue.shift();", fixed = TRUE)
  expect_match(js, "dispatchNextClientImport();", fixed = TRUE)
  expect_match(
    js,
    '"builder_client_import_transport_release",',
    fixed = TRUE
  )
  expect_match(js, "handleClientImportTransportRelease", fixed = TRUE)
})

test_that("transport release gates three browser files by exact identity", {
  js <- builder_browser_pipeline_text("www", "builder.js")
  handler <- builder_browser_pipeline_js_function(
    js,
    "handleClientImportTransportRelease"
  )
  out <- builder_browser_pipeline_run_node(
    handler,
    c(
      "const assert = require('assert');",
      "function entry(client, server) {",
      "  return {clientId: client, serverId: server, kind: 'file', outcome: null};",
      "}",
      "const a = entry('A', 'server-A');",
      "const b = entry('B', 'server-B');",
      "const c = entry('C', 'server-C');",
      "let clientImportQueue = [a, b, c];",
      "let activeClientImport = a;",
      "let renderCount = 0;",
      "const dispatched = [];",
      "function renderClientImportQueue() { renderCount += 1; }",
      "function dispatchNextClientImport() {",
      "  if (activeClientImport || !clientImportQueue.length) return;",
      "  activeClientImport = clientImportQueue[0];",
      "  dispatched.push(activeClientImport.clientId);",
      "}",
      "handleClientImportTransportRelease({client_id: 'wrong', server_id: 'server-A'});",
      "handleClientImportTransportRelease({client_id: 'A', server_id: 'wrong'});",
      "assert.deepStrictEqual(clientImportQueue, [a, b, c]);",
      "assert.strictEqual(activeClientImport, a);",
      "const realActive = activeClientImport;",
      "activeClientImport = entry('stale', 'server-stale');",
      "handleClientImportTransportRelease({client_id: 'stale', server_id: 'server-stale'});",
      "assert.deepStrictEqual(clientImportQueue, [a, b, c]);",
      "activeClientImport = realActive;",
      "handleClientImportTransportRelease({client_id: 'A', server_id: 'server-A'});",
      "assert.deepStrictEqual(clientImportQueue, [b, c]);",
      "assert.strictEqual(activeClientImport, b);",
      "assert.deepStrictEqual(dispatched, ['B']);",
      "handleClientImportTransportRelease({client_id: 'A', server_id: 'server-A'});",
      "handleClientImportTransportRelease({client_id: 'C', server_id: 'server-C'});",
      "assert.deepStrictEqual(clientImportQueue, [b, c]);",
      "assert.strictEqual(activeClientImport, b);",
      "assert.deepStrictEqual(dispatched, ['B']);",
      "handleClientImportTransportRelease({client_id: 'B', server_id: 'server-B'});",
      "assert.deepStrictEqual(clientImportQueue, [c]);",
      "assert.strictEqual(activeClientImport, c);",
      "assert.deepStrictEqual(dispatched, ['B', 'C']);",
      "assert.strictEqual(renderCount, 2);",
      "console.log('ok');"
    )
  )

  expect_equal(attr(out, "status"), NULL)
  expect_identical(out, "ok")
})

test_that("terminal and transport handlers apply different state transitions", {
  js <- builder_browser_pipeline_text("www", "builder.js")
  handlers <- c(
    builder_browser_pipeline_js_function(
      js,
      "handleClientImportTransportRelease"
    ),
    builder_browser_pipeline_js_function(js, "handleClientImportRelease")
  )
  out <- builder_browser_pipeline_run_node(
    handlers,
    c(
      "const assert = require('assert');",
      "function entry(client, server) {",
      "  return {clientId: client, serverId: server, kind: 'file', outcome: null, error: null, name: client};",
      "}",
      "let clientImportFailures = [];",
      "let clientImportQueue = [entry('A', 'server-A'), entry('B', 'server-B')];",
      "let activeClientImport = clientImportQueue[0];",
      "const transported = activeClientImport;",
      "function renderClientImportQueue() {}",
      "function scheduleStatusAnnouncement() {}",
      "function dispatchNextClientImport() {",
      "  if (!activeClientImport && clientImportQueue.length) activeClientImport = clientImportQueue[0];",
      "}",
      "handleClientImportTransportRelease({client_id: 'A', server_id: 'server-A'});",
      "assert.strictEqual(transported.outcome, null);",
      "assert.strictEqual(clientImportFailures.length, 0);",
      "assert.strictEqual(activeClientImport.clientId, 'B');",
      "const terminal = entry('X', 'server-X');",
      "clientImportQueue = [terminal, entry('Y', 'server-Y')];",
      "activeClientImport = terminal;",
      "handleClientImportRelease({client_id: 'X', server_id: 'server-X', outcome: 'error', message: 'failed'});",
      "assert.strictEqual(terminal.outcome, 'error');",
      "assert.strictEqual(terminal.error, 'failed');",
      "assert.strictEqual(activeClientImport.clientId, 'Y');",
      "console.log('ok');"
    )
  )

  expect_equal(attr(out, "status"), NULL)
  expect_identical(out, "ok")
})

test_that("sync replay of retained transport advances the browser queue", {
  js <- builder_browser_pipeline_text("www", "builder.js")
  handlers <- c(
    builder_browser_pipeline_js_function(
      js,
      "handleClientImportTransportRelease"
    ),
    builder_browser_pipeline_js_function(js, "handleClientImportSync")
  )
  out <- builder_browser_pipeline_run_node(
    handlers,
    c(
      "const assert = require('assert');",
      "const a = {clientId: 'A', serverId: null, kind: 'file', state: 'paused'};",
      "const b = {clientId: 'B', serverId: null, kind: 'file', state: 'queued'};",
      "let clientImportQueue = [a, b];",
      "let activeClientImport = a;",
      "let importSyncPending = true;",
      "let serverImportGate = false;",
      "const dispatched = [];",
      "function renderClientImportQueue() {}",
      "function dispatchNextClientImport() {",
      "  if (activeClientImport || !clientImportQueue.length) return;",
      "  activeClientImport = clientImportQueue[0];",
      "  dispatched.push(activeClientImport.clientId);",
      "}",
      "function failClientImportReconciliation() { throw new Error('unexpected reconciliation failure'); }",
      "function handleClientImportRelease() { throw new Error('unexpected terminal release'); }",
      "function startFileTransport() { throw new Error('unexpected upload restart'); }",
      "handleClientImportSync({imports: [{",
      "  client_id: 'A', server_id: 'server-A', state: 'reading', transport_retained: true",
      "}], server_busy: false});",
      "assert.strictEqual(importSyncPending, false);",
      "assert.deepStrictEqual(clientImportQueue, [b]);",
      "assert.strictEqual(activeClientImport, b);",
      "assert.deepStrictEqual(dispatched, ['B']);",
      "console.log('ok');"
    )
  )

  expect_equal(attr(out, "status"), NULL)
  expect_identical(out, "ok")
})

test_that("terminal and transport releases remain separate browser contracts", {
  js <- builder_browser_pipeline_text("www", "builder.js")
  transport <- regmatches(
    js,
    regexpr(
      "function handleClientImportTransportRelease[\\s\\S]*?\\n  }",
      js,
      perl = TRUE
    )
  )
  terminal <- regmatches(
    js,
    regexpr(
      "function handleClientImportRelease[\\s\\S]*?\\n  }",
      js,
      perl = TRUE
    )
  )

  expect_match(transport, "dispatchNextClientImport", fixed = TRUE)
  expect_false(grepl("message.outcome", transport, fixed = TRUE))
  expect_match(terminal, "message.outcome", fixed = TRUE)
})

test_that("retained loader uses clone-aware copy with an atomic part file", {
  project <- builder_browser_pipeline_text("project.R")
  loader <- regmatches(
    project,
    regexpr(
      "builder_project_load_retained_source <- function[\\s\\S]*?\\n}",
      project,
      perl = TRUE
    )
  )

  expect_match(loader, ".copy = builder_project_copy_file", fixed = TRUE)
  expect_match(loader, 'fileext = ".part"', fixed = TRUE)
  expect_match(loader, "file.rename(part, target)", fixed = TRUE)
  expect_false(grepl("file.rename(source$source", loader, fixed = TRUE))
  expect_false(grepl("unlink(source$source", loader, fixed = TRUE))
})

test_that("local sources do not opt into browser transport retention", {
  imports <- builder_browser_pipeline_text("server", "imports.R")

  expect_match(
    imports,
    '!identical(source_origin, "local")',
    fixed = TRUE
  )
  expect_match(
    imports,
    "if (isTRUE(retain_in_worker) && builder_has_text(progress_path))",
    fixed = TRUE
  )
})
