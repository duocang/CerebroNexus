builder_project_test_runtime <- function() {
  runtime <- new.env(parent = globalenv())
  for (file in c("io.R", "worker.R", "extras.R", "project.R", "build.R")) {
    path <- testthat::test_path("..", "..", "inst", "builder", file)
    sys.source(path, envir = runtime)
  }
  runtime
}

builder_project_test_manifest <- function(ids = character()) {
  list(
    schema_version = 1L,
    project = list(id = "project-safe", revision = 0L),
    datasets = lapply(ids, function(id) list(id = id))
  )
}

test_that("project dataset ids are allocated without reusing restored ids", {
  runtime <- builder_project_test_runtime()

  allocated <- runtime$builder_project_allocate_dataset_id(
    sequence = 0L,
    existing_ids = c("ds1", "ds2")
  )
  expect_identical(allocated, list(id = "ds3", sequence = 3L))

  restored <- runtime$builder_project_allocate_dataset_id(
    sequence = 1L,
    existing_ids = "ds1",
    restored_id = "ds7"
  )
  expect_identical(restored, list(id = "ds7", sequence = 7L))
  expect_error(
    runtime$builder_project_allocate_dataset_id(1L, "ds1", "ds1"),
    "already in use"
  )
})

test_that("uploaded sources are retained separately with their original names", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  first <- file.path(root, "0.qs2")
  second <- file.path(root, "1.qs2")
  writeBin(charToRaw("first"), first)
  writeBin(charToRaw("second"), second)

  retained_first <- runtime$builder_project_retain_session_source(
    first,
    "sample-alpha.qs2",
    root,
    "ds1"
  )
  retained_second <- runtime$builder_project_retain_session_source(
    second,
    "sample-beta.qs2",
    root,
    "ds2"
  )

  expect_identical(basename(retained_first), "sample-alpha.qs2")
  expect_identical(basename(retained_second), "sample-beta.qs2")
  expect_true(file.exists(retained_first))
  expect_true(file.exists(retained_second))
})

test_that("example sources use the same retained file contract as uploads", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  bundled <- file.path(root, "all_content.rds")
  writeBin(charToRaw("example-bytes"), bundled)
  catalog <- list(
    all_content = list(
      id = "all_content",
      serialized_path = bundled
    )
  )

  source <- runtime$builder_project_example_source("all_content", catalog)
  retained <- runtime$builder_project_retain_session_source(
    source$path,
    source$filename,
    root,
    "ds1"
  )

  expect_identical(source$origin, "example")
  expect_identical(source$filename, "all_content.rds")
  expect_identical(
    readBin(retained, "raw", n = 100L),
    charToRaw("example-bytes")
  )
})

test_that("examples with retained files are managed project sources", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  session_source <- file.path(root, "session-sources", "ds1", "all_content.rds")
  dir.create(dirname(session_source), recursive = TRUE)
  writeBin(charToRaw("example-bytes"), session_source)
  project <- file.path(root, "project")
  dir.create(project)
  entry <- list(
    id = "ds1",
    path = session_source,
    filename = "all_content.rds",
    example = "all_content",
    source_origin = "example"
  )

  staged <- runtime$builder_project_stage_source(entry, project)

  expect_identical(staged$source$kind, "managed")
  expect_identical(staged$source$origin, "example")
  expect_identical(staged$source$filename, "all_content.rds")
  expect_true(file.exists(staged$entry$path))
})

test_that("background source jobs commit exact bytes through a part file", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "session-sources", "ds1", "sample.qs2")
  dir.create(dirname(source), recursive = TRUE)
  writeBin(charToRaw("immutable-source"), source)
  project <- file.path(root, "project")
  dir.create(project)

  job <- runtime$builder_project_source_job(
    list(
      id = "ds1",
      path = source,
      filename = "sample.qs2",
      source_origin = "upload"
    ),
    project
  )
  result <- runtime$builder_project_copy_source_job(job)

  expect_identical(result$status, "ready")
  expect_identical(
    readBin(job$target, "raw", n = 100L),
    charToRaw("immutable-source")
  )
  expect_false(file.exists(job$part))
  expect_identical(result$fingerprint$md5, unname(tools::md5sum(job$target)))
})

test_that("background source jobs report a missing source without a partial target", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  project <- file.path(root, "project")
  dir.create(project)
  job <- runtime$builder_project_source_job(
    list(
      id = "ds1",
      path = file.path(root, "missing.qs2"),
      filename = "missing.qs2",
      source_origin = "upload"
    ),
    project
  )

  result <- runtime$builder_project_copy_source_job(job)

  expect_identical(result$status, "failed")
  expect_match(result$error, "no longer available", fixed = TRUE)
  expect_false(file.exists(job$part))
  expect_false(file.exists(job$target))
})

test_that("project source preparation records pending work without copying", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "session-sources", "ds1", "all_content.rds")
  dir.create(dirname(source), recursive = TRUE)
  writeBin(charToRaw("example"), source)
  project <- file.path(root, "project")
  dir.create(project)
  entry <- list(
    id = "ds1",
    path = source,
    filename = "all_content.rds",
    example = "all_content",
    source_origin = "example"
  )

  prepared <- runtime$builder_project_prepare_source(entry, project)

  expect_identical(prepared$source$status, "pending")
  expect_identical(prepared$source$origin, "example")
  expect_identical(prepared$source$path, "sources/ds1/all_content.rds")
  expect_false(file.exists(prepared$job$target))
})

test_that("completed background sources are merged into the latest manifest", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  project <- file.path(root, "project")
  target <- file.path(project, "sources", "ds1", "sample.qs2")
  dir.create(dirname(target), recursive = TRUE)
  writeBin(charToRaw("ready"), target)
  manifest <- builder_project_test_manifest("ds1")
  manifest$datasets[[1L]]$source <- list(
    kind = "managed",
    origin = "upload",
    filename = "sample.qs2",
    path = "sources/ds1/sample.qs2",
    status = "pending"
  )
  result <- list(
    id = "ds1",
    status = "ready",
    path = target,
    fingerprint = runtime$builder_project_file_fingerprint(
      target,
      content = TRUE
    )
  )

  updated <- runtime$builder_project_apply_source_results(
    manifest,
    list(result),
    project
  )

  expect_identical(updated$datasets[[1L]]$source$status, "ready")
  expect_identical(
    updated$datasets[[1L]]$source$fingerprint$md5,
    result$fingerprint$md5
  )
})

test_that("managed example sources retain their example identity when restored", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "sources", "ds1", "all_content.rds")
  dir.create(dirname(source), recursive = TRUE)
  writeBin(charToRaw("example"), source)
  entry <- list(id = "ds1", settings = list(name = "All content"))
  record <- list(
    id = "ds1",
    source = list(
      kind = "managed",
      origin = "example",
      example = "all_content",
      path = "sources/ds1/all_content.rds"
    ),
    configuration = list(payload = jsonlite::serializeJSON(entry))
  )

  restored <- runtime$builder_project_restore_entry(record, root)

  expect_identical(restored$source_origin, "example")
  expect_identical(restored$example, "all_content")
  expect_identical(restored$path, normalizePath(source, winslash = "/"))
})

test_that("project payload preserves saved Spatial FOV controls", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  entry <- list(
    id = "ds1",
    settings = list(
      spatial_coordinate_transforms = list(
        section_a_1_fov_1 = list(rotation_degrees = 66.9, scale = 1)
      ),
      spatial_point_appearance = list(
        section_a_1_fov_1 = list(point_opacity = 0.7, point_size = 6)
      )
    )
  )
  record <- runtime$builder_project_dataset_record(
    entry,
    source = list(kind = "missing", path = NULL)
  )

  restored <- runtime$builder_project_restore_entry(record, root)

  expect_identical(
    restored$settings$spatial_coordinate_transforms[[
      "section_a_1_fov_1"
    ]]$rotation_degrees,
    66.9
  )
  expect_identical(
    restored$settings$spatial_point_appearance[["section_a_1_fov_1"]],
    list(point_opacity = 0.7, point_size = 6)
  )
})

test_that("project spatial assets are externalized per dataset and FOV", {
  skip_if_not_installed("base64enc")
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  image_uri <- paste0(
    "data:image/png;base64,",
    base64enc::base64encode(charToRaw("project-image-bytes"))
  )
  image_record <- function(section) {
    list(
      source = list(name = "tissue.png", type = "image/png"),
      source_uri = image_uri,
      uri = image_uri,
      bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      section_id = section
    )
  }
  entry <- list(
    id = "ds1",
    settings = list(
      images = list(
        `section/a` = list(`H&E` = image_record("section/a")),
        `section-a` = list(`H&E` = image_record("section-a"))
      )
    )
  )

  payload_entry <- runtime$builder_project_stage_spatial_assets(entry, root)
  record <- runtime$builder_project_dataset_record(
    entry,
    source = list(kind = "missing", path = NULL),
    payload_entry = payload_entry
  )
  payload <- jsonlite::unserializeJSON(record$configuration$payload)
  first <- payload$settings$images[["section/a"]][["H&E"]]
  second <- payload$settings$images[["section-a"]][["H&E"]]

  expect_null(first$source_uri)
  expect_null(first$uri)
  expect_match(first$project_asset$path, "^spatial-assets/ds1/")
  expect_false(identical(first$project_asset$path, second$project_asset$path))
  expect_true(file.exists(file.path(root, first$project_asset$path)))
  expect_false(grepl("data:image", record$configuration$payload, fixed = TRUE))

  restored <- runtime$builder_project_restore_entry(record, root)
  expect_identical(
    restored$settings$images[["section/a"]][["H&E"]]$source_uri,
    image_uri
  )
  expect_identical(
    restored$settings$images[["section-a"]][["H&E"]]$uri,
    image_uri
  )
  expect_identical(
    runtime$builder_project_configuration_digest(restored),
    record$configuration$digest
  )
})

test_that("missing project spatial assets identify the affected FOV and image", {
  skip_if_not_installed("base64enc")
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  image_uri <- paste0(
    "data:image/png;base64,",
    base64enc::base64encode(charToRaw("missing-image"))
  )
  entry <- list(
    id = "ds1",
    settings = list(
      images = list(
        fov_1 = list(
          DAPI = list(
            source_uri = image_uri,
            uri = image_uri,
            bounds = list(xmin = 0, xmax = 1, ymin = 0, ymax = 1)
          )
        )
      )
    )
  )
  payload_entry <- runtime$builder_project_stage_spatial_assets(entry, root)
  record <- runtime$builder_project_dataset_record(
    entry,
    list(kind = "missing", path = NULL),
    payload_entry = payload_entry
  )
  asset <- payload_entry$settings$images$fov_1$DAPI$project_asset$path
  unlink(file.path(root, asset))

  expect_error(
    runtime$builder_project_restore_entry(record, root),
    "ds1.*fov_1.*DAPI"
  )
  status <- runtime$builder_project_dataset_status(record, root)
  expect_false(status$spatial_assets_ready)
  expect_false(status$restorable)
  expect_identical(status$label, "Needs check · spatial image missing")
  lightweight <- runtime$builder_project_restore_entry(
    record,
    root,
    hydrate_spatial_assets = FALSE
  )
  expect_null(lightweight$settings$images$fov_1$DAPI$source_uri)
})

test_that("checkpoint entries embed spatial images without mutating live settings", {
  runtime <- builder_project_test_runtime()
  entry <- list(
    id = "ds1",
    settings = list(
      spatial_image_storage = "external",
      images = list(
        fov_1 = list(image = list(uri = "data:image/png;base64,AA=="))
      )
    )
  )

  checkpoint <- runtime$builder_project_checkpoint_entries(list(entry))

  expect_identical(
    checkpoint[[1L]]$settings$spatial_image_storage,
    "embedded"
  )
  expect_identical(entry$settings$spatial_image_storage, "external")
  expect_identical(checkpoint[[1L]]$settings$images, entry$settings$images)
})

test_that("restored project settings refresh the active spatial dataset", {
  spatial_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "spatial_alignment_server.R"
  )
  project_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "project.R"
  )
  spatial <- paste(readLines(spatial_path, warn = FALSE), collapse = "\n")
  project <- paste(readLines(project_path, warn = FALSE), collapse = "\n")

  expect_match(
    spatial,
    "restore_project_settings = restore_project_settings",
    fixed = TRUE
  )
  expect_match(
    project,
    "alignment_server$restore_project_settings(restored_ids)",
    fixed = TRUE
  )
})

test_that("managed examples keep their origin across the async import boundary", {
  imports_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "imports.R"
  )
  project_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "project.R"
  )
  imports <- paste(readLines(imports_path, warn = FALSE), collapse = "\n")
  project <- paste(readLines(project_path, warn = FALSE), collapse = "\n")

  expect_match(imports, "source_origin = NULL", fixed = TRUE)
  expect_match(imports, "example_id = NULL", fixed = TRUE)
  expect_match(project, "source_origin = source$origin", fixed = TRUE)
  expect_match(project, "example_id = source$example", fixed = TRUE)
})

test_that("the project status only declares safe close after source sync", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "ui",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(source, "Project fully saved · Safe to close", fixed = TRUE)
  expect_match(source, "Saving source files · ", fixed = TRUE)
})

test_that("source synchronization warns on close without locking the workspace", {
  runtime <- builder_project_test_runtime()
  activity <- runtime$builder_activity_state(
    project_phase = "clean",
    source_syncing = TRUE,
    has_project = TRUE,
    has_datasets = TRUE
  )
  capabilities <- runtime$builder_activity_capabilities(activity)

  expect_true(capabilities$warn_before_unload)
  expect_true(capabilities$select_dataset)
  expect_true(capabilities$add_dataset)
  expect_true(capabilities$edit_dataset)
  expect_true(capabilities$save_project)
  expect_false(capabilities$page_inert)
})

test_that("the first project offer waits for every import ownership layer", {
  runtime <- builder_project_test_runtime()
  idle <- runtime$builder_activity_state(has_datasets = TRUE)
  client_busy <- runtime$builder_activity_state(
    client_imports = 1L,
    has_datasets = TRUE
  )
  server_busy <- runtime$builder_activity_state(
    server_imports = TRUE,
    has_datasets = TRUE
  )
  protocol <- list(queue = list(), pending = NULL, awaiting_ack = list())
  load <- list(kind = "load")

  expect_true(runtime$builder_imports_idle(idle, protocol))
  expect_false(runtime$builder_imports_idle(client_busy, protocol))
  expect_false(runtime$builder_imports_idle(server_busy, protocol))

  protocol$queue <- list(load)
  expect_false(runtime$builder_imports_idle(idle, protocol))
  protocol$queue <- list()
  protocol$pending <- load
  expect_false(runtime$builder_imports_idle(idle, protocol))
  protocol$pending <- NULL
  protocol$awaiting_ack <- list(token = load)
  expect_false(runtime$builder_imports_idle(idle, protocol))

  protocol$awaiting_ack <- list(token = list(kind = "preview"))
  expect_true(runtime$builder_imports_idle(idle, protocol))
})

test_that("the first project offer is retryable when imports restart before flush", {
  runtime <- builder_project_test_runtime()
  idle <- runtime$builder_activity_state(has_datasets = TRUE)
  busy <- runtime$builder_activity_state(
    client_imports = 1L,
    has_datasets = TRUE
  )
  protocol <- list(queue = list(), pending = NULL, awaiting_ack = list())
  entries <- list(list(id = "ds1"))

  expect_true(runtime$builder_project_first_save_offer_ready(
    entries,
    project = NULL,
    offered = FALSE,
    activity = idle,
    protocol = protocol
  ))
  expect_false(runtime$builder_project_first_save_offer_ready(
    entries,
    project = NULL,
    offered = FALSE,
    activity = busy,
    protocol = protocol
  ))
  expect_true(runtime$builder_project_first_save_offer_ready(
    entries,
    project = NULL,
    offered = FALSE,
    activity = idle,
    protocol = protocol
  ))
  expect_false(runtime$builder_project_first_save_offer_ready(
    entries,
    project = list(name = "restored"),
    offered = FALSE,
    activity = idle,
    protocol = protocol
  ))
  expect_false(runtime$builder_project_first_save_offer_ready(
    entries,
    project = NULL,
    offered = TRUE,
    activity = idle,
    protocol = protocol
  ))
})

test_that("project server uses a dedicated callr source copy process", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(source, "request_builder_project_source_sync", fixed = TRUE)
  expect_match(source, "callr::r_bg", fixed = TRUE)
  expect_match(source, "later::later", fixed = TRUE)
  expect_match(source, "builder_project_apply_source_results", fixed = TRUE)
})

test_that("a finished source process remains owned until its result is collected", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(
    source,
    paste(
      "  process <- isolate(builder_project_source_process())",
      "  if (!is.null(process)) {",
      sep = "\n"
    ),
    fixed = TRUE
  )
  expect_false(grepl(
    "!is.null(process) && isTRUE(process$is_alive())",
    source,
    fixed = TRUE
  ))
})

test_that("project manifests reject unsafe and duplicate dataset ids", {
  runtime <- builder_project_test_runtime()
  path <- withr::local_tempfile(fileext = ".json")

  jsonlite::write_json(
    builder_project_test_manifest(c("../outside")),
    path,
    auto_unbox = TRUE
  )
  expect_error(runtime$builder_project_read(path), "unsafe or duplicate")

  jsonlite::write_json(
    builder_project_test_manifest(c("ds1", "ds1")),
    path,
    auto_unbox = TRUE
  )
  expect_error(runtime$builder_project_read(path), "unsafe or duplicate")
})

test_that("macOS project pickers emit valid AppleScript prompts", {
  runtime <- builder_project_test_runtime()

  expect_identical(
    runtime$builder_project_osascript("directory"),
    paste0(
      "POSIX path of (choose folder with prompt ",
      "\"Choose a folder for the Builder project.\")"
    )
  )
  expect_identical(
    runtime$builder_project_osascript("manifest"),
    paste0(
      "POSIX path of (choose file with prompt ",
      "\"Open a Builder project.\" of type {\"public.json\"})"
    )
  )
})

test_that("reusable artifacts must retain their saved content fingerprint", {
  runtime <- builder_project_test_runtime()
  artifact <- withr::local_tempfile(fileext = ".crb")
  writeBin(charToRaw("original"), artifact)
  fingerprint <- list(md5 = unname(tools::md5sum(artifact)))

  expect_true(runtime$.builder_build_fingerprint_matches(artifact, fingerprint))
  writeBin(charToRaw("replacement"), artifact)
  expect_false(runtime$.builder_build_fingerprint_matches(
    artifact,
    fingerprint
  ))
})

test_that("persisted artifact fingerprints contain only plain JSON values", {
  runtime <- builder_project_test_runtime()
  artifact <- withr::local_tempfile(fileext = ".crb")
  writeBin(charToRaw("artifact"), artifact)

  fingerprint <- runtime$builder_project_file_fingerprint(
    artifact,
    content = TRUE
  )

  expect_identical(class(fingerprint$md5), "character")
  expect_silent(jsonlite::toJSON(fingerprint, auto_unbox = TRUE))
})

test_that("configuration digests are plain JSON strings", {
  runtime <- builder_project_test_runtime()

  digest <- runtime$builder_project_configuration_digest(list())

  expect_identical(class(digest), "character")
  expect_silent(jsonlite::toJSON(list(digest = digest), auto_unbox = TRUE))
})

test_that("build execution rejects a reusable CRB changed after checkpoint", {
  runtime <- builder_project_test_runtime()
  artifact <- withr::local_tempfile(fileext = ".crb")
  writeBin(charToRaw("original"), artifact)
  fingerprint <- list(md5 = unname(tools::md5sum(artifact)))
  writeBin(charToRaw("replacement"), artifact)
  stage <- withr::local_tempdir()
  plan <- structure(
    list(
      items = list(list(
        id = "ds1",
        name = "Dataset 1",
        filename = "dataset-1.crb",
        reused_artifact = list(
          path = artifact,
          fingerprint = fingerprint,
          members = list()
        )
      )),
      make_app = FALSE,
      app_auth = list(enabled = FALSE)
    ),
    class = c("builder_build_plan", "list")
  )
  unused <- function(...) stop("changed artifacts must fail before hooks run")
  hooks <- list(
    open_snapshot = unused,
    prepare = unused,
    run_analyses = unused,
    export = unused,
    attach_extras = unused,
    verify = unused
  )

  result <- runtime$builder_execute_plan(plan, stage, list(), hooks = hooks)

  expect_identical(result$state, "failure")
  expect_match(result$error, "unavailable or has changed", fixed = TRUE)
  expect_false(file.exists(file.path(stage, "dataset-1.crb")))
})

test_that("saved plan items are reused only when artifact references agree", {
  runtime <- builder_project_test_runtime()
  saved <- list(
    id = "ds1",
    name = "Dataset 1",
    filename = "dataset-1.crb",
    sidecars = "dataset-1.h5",
    viewer_bundle_assets = character(),
    private_assets = c("dataset-1.crb", "dataset-1.h5"),
    viewer_bundle_asset_claims = list(),
    private_asset_claims = list()
  )

  expect_true(runtime$builder_project_reused_plan_matches(saved, saved))
  changed <- saved
  changed$filename <- "renamed.crb"
  expect_false(runtime$builder_project_reused_plan_matches(saved, changed))
})

test_that("failed checkpoint enqueue cannot remain marked running", {
  runtime <- builder_project_test_runtime()
  manifest <- builder_project_test_manifest("ds1")
  manifest$pending_build <- list(id = "checkpoint-1", status = "running")

  updated <- runtime$builder_project_finish_pending_build(
    manifest,
    status = "failed",
    error = "The build could not be queued.",
    finished_at = "2026-08-17T12:00:00Z"
  )

  expect_identical(updated$pending_build$status, "failed")
  expect_identical(
    updated$pending_build$error,
    "The build could not be queued."
  )
  expect_identical(updated$pending_build$finished_at, "2026-08-17T12:00:00Z")
})

test_that("restore dialog explains the session-only skip action", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "ui",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(source, "Skip this dataset for this session", fixed = TRUE)
  expect_match(source, "Use ready CRB — fast, view/build only", fixed = TRUE)
  expect_match(source, "Load source — continue editing", fixed = TRUE)
  expect_match(source, "if (status$restorable) {", fixed = TRUE)
  expect_match(
    source,
    "Skipped datasets remain saved in the project",
    fixed = TRUE
  )
  expect_false(grepl(
    "Keep in the project, but skip for now",
    source,
    fixed = TRUE
  ))
})

test_that("restore choices render descriptive labels and prefer checked CRB reuse", {
  skip_if_not_installed("shiny")
  runtime <- builder_project_test_runtime()
  runtime$tags <- shiny::tags
  runtime$radioButtons <- shiny::radioButtons
  ui_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "ui",
    "project.R"
  )
  sys.source(ui_path, envir = runtime)
  root <- withr::local_tempdir()
  source_path <- file.path(root, "source.rds")
  artifact_path <- file.path(root, "artifact.crb")
  writeBin(charToRaw("source"), source_path)
  writeBin(charToRaw("artifact"), artifact_path)
  entry <- list(id = "ds1", settings = list(name = "Dataset A"))
  record <- runtime$builder_project_dataset_record(
    entry,
    source = list(
      kind = "managed",
      path = runtime$builder_project_relative_path(source_path, root)
    ),
    checked = TRUE,
    artifact = list(
      status = "ready",
      reusable = TRUE,
      path = runtime$builder_project_relative_path(artifact_path, root)
    )
  )

  html <- htmltools::renderTags(
    runtime$builder_project_restore_row_ui(record, root)
  )$html

  expect_match(html, "Use ready CRB — fast, view/build only", fixed = TRUE)
  expect_match(html, "Load source — continue editing", fixed = TRUE)
  expect_match(html, 'value="reuse" checked="checked"', fixed = TRUE)
  expect_match(html, "Checked · CRB ready", fixed = TRUE)
  expect_identical(
    lengths(regmatches(
      html,
      gregexpr("Checked · CRB ready", html, fixed = TRUE)
    )),
    1L
  )

  record$configuration$checked <- FALSE
  record$artifact$status <- "stale"
  html <- htmltools::renderTags(
    runtime$builder_project_restore_row_ui(record, root)
  )$html
  expect_match(html, "Needs check · load source", fixed = TRUE)
})

test_that("project lifecycle capabilities lock only conflicting operations", {
  runtime <- builder_project_test_runtime()

  importing <- runtime$builder_activity_state(
    client_imports = 1L,
    project_phase = "dirty",
    has_project = TRUE,
    has_datasets = TRUE
  )
  import_capabilities <- runtime$builder_activity_capabilities(importing)
  expect_true(import_capabilities$add_dataset)
  expect_true(import_capabilities$edit_dataset)
  expect_false(import_capabilities$check_dataset)
  expect_false(import_capabilities$save_project)
  expect_false(import_capabilities$build)

  saving <- runtime$builder_activity_state(
    project_phase = "saving",
    has_project = TRUE,
    has_datasets = TRUE
  )
  save_capabilities <- runtime$builder_activity_capabilities(saving)
  expect_true(save_capabilities$page_inert)
  expect_true(save_capabilities$warn_before_unload)
  expect_false(save_capabilities$mutate_datasets)
  expect_false(save_capabilities$save_project)
})

test_that("a project folder can be chosen while another dataset is importing", {
  runtime <- builder_project_test_runtime()
  importing <- runtime$builder_activity_state(
    client_imports = 1L,
    server_imports = TRUE,
    project_phase = "none",
    has_project = FALSE,
    has_datasets = TRUE
  )
  capabilities <- runtime$builder_activity_capabilities(importing)

  expect_true(capabilities$create_project)
  expect_true(capabilities$add_dataset)
  expect_false(capabilities$check_dataset)
  expect_false(capabilities$save_project)
  expect_false(capabilities$build)
})

test_that("the top bar omits the format capability summary", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "app.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_false(grepl(
    'span(class = "formats", textOutput("format_line", inline = TRUE))',
    source,
    fixed = TRUE
  ))
})

test_that("spatial drafts are committed by workflow actions instead of blocking them", {
  runtime <- builder_project_test_runtime()
  activity <- runtime$builder_activity_state(
    project_phase = "dirty",
    spatial_dirty = TRUE,
    has_project = TRUE,
    has_datasets = TRUE
  )
  capabilities <- runtime$builder_activity_capabilities(activity)

  expect_true(capabilities$check_dataset)
  expect_true(capabilities$navigate_workflow)
  expect_true(capabilities$prepare_crbs)
  expect_true(capabilities$build)
  expect_true(capabilities$warn_before_unload)
})

test_that("project saving can commit spatial drafts through the internal mutation path", {
  foundation_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "foundation.R"
  )
  imports_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "imports.R"
  )
  enhancements_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "enhancements.R"
  )
  project_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "project.R"
  )
  foundation <- paste(readLines(foundation_path, warn = FALSE), collapse = "\n")
  imports <- paste(readLines(imports_path, warn = FALSE), collapse = "\n")
  enhancements <- paste(
    readLines(enhancements_path, warn = FALSE),
    collapse = "\n"
  )
  project <- paste(readLines(project_path, warn = FALSE), collapse = "\n")

  expect_match(
    foundation,
    "replace_entry <- function(updated, internal = FALSE)",
    fixed = TRUE
  )
  expect_match(
    imports,
    "commit_enhance_images <- function(entry, images, internal = FALSE)",
    fixed = TRUE
  )
  expect_match(enhancements, "internal = TRUE", fixed = TRUE)
  expect_match(
    project,
    "materialize_coordinate_drafts(\n      notify = FALSE",
    fixed = TRUE
  )
})

test_that("opening and restoring projects make the page inert with close protection", {
  runtime <- builder_project_test_runtime()

  for (phase in c("opening", "restoring")) {
    activity <- runtime$builder_activity_state(project_phase = phase)
    capabilities <- runtime$builder_activity_capabilities(activity)
    expect_true(capabilities$page_inert)
    expect_true(capabilities$warn_before_unload)
    expect_false(capabilities$open_project)
  }
})

test_that("pending project settings survive until source imports are dispatched", {
  runtime <- builder_project_test_runtime()
  pending <- list(ds1 = list(id = "ds1"))

  expect_identical(
    runtime$builder_project_abandoned_entries(
      pending,
      operation = "opening"
    ),
    character()
  )
  expect_identical(
    runtime$builder_project_abandoned_entries(
      pending,
      import_ids = "ds1",
      operation = "restoring"
    ),
    character()
  )
  expect_identical(
    runtime$builder_project_abandoned_entries(
      pending,
      operation = "restoring"
    ),
    "ds1"
  )
})

test_that("project open work starts only after loading feedback is flushed", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(
    source,
    'builder_project_operation_phase("opening")',
    fixed = TRUE
  )
  expect_match(source, "session$onFlushed(", fixed = TRUE)
  expect_match(source, "builder_project_restore_progress", fixed = TRUE)
  expect_match(source, 'mode = "restoring"', fixed = TRUE)
})

test_that("a revision conflict permits reopening but rejects mutation", {
  runtime <- builder_project_test_runtime()
  activity <- runtime$builder_activity_state(
    project_phase = "conflict",
    has_project = TRUE,
    has_datasets = TRUE
  )
  capabilities <- runtime$builder_activity_capabilities(activity)

  expect_true(capabilities$open_project)
  expect_false(capabilities$edit_dataset)
  expect_false(capabilities$save_project)
  expect_match(
    runtime$builder_activity_reason(activity, "save_project"),
    "Reopen the project",
    fixed = TRUE
  )
})

test_that("project dirty state follows configuration checked state and order", {
  runtime <- builder_project_test_runtime()
  first <- list(
    id = "ds1",
    settings = list(name = "First"),
    acknowledgements = character(),
    spatial_drafts = list()
  )
  second <- list(
    id = "ds2",
    settings = list(name = "Second"),
    acknowledgements = character(),
    spatial_drafts = list()
  )
  manifest <- list(
    datasets = list(
      list(
        id = "ds1",
        order = 1L,
        configuration = list(
          digest = runtime$builder_project_configuration_digest(first),
          checked = FALSE
        )
      ),
      list(
        id = "ds2",
        order = 2L,
        configuration = list(
          digest = runtime$builder_project_configuration_digest(second),
          checked = TRUE
        )
      )
    )
  )

  expect_false(runtime$builder_project_live_dirty(
    list(first, second),
    "ds2",
    manifest
  ))
  expect_true(runtime$builder_project_live_dirty(
    list(first, second),
    character(),
    manifest
  ))
  changed <- first
  changed$settings$name <- "Renamed"
  expect_true(runtime$builder_project_live_dirty(
    list(changed, second),
    "ds2",
    manifest
  ))
  expect_true(runtime$builder_project_live_dirty(
    list(second, first),
    "ds2",
    manifest
  ))
})

test_that("checked project records bind confirmation to the restored entry", {
  runtime <- builder_project_test_runtime()
  restored <- list(
    id = "ds1",
    snapshot = list(
      path = "/session/new-snapshot",
      owner_token = "new-owner",
      object_md5 = "new-md5"
    ),
    settings = list(
      name = "Spatial dataset",
      spatial_coordinate_transforms = list(
        section_a_1_fov_1 = list(rotation_degrees = 66.9, scale = 1)
      )
    ),
    acknowledgements = character(),
    spatial_drafts = list()
  )
  record <- list(configuration = list(checked = TRUE))
  status <- list(source_matches = TRUE, checked = TRUE)

  mark <- runtime$builder_project_restored_check_identity(
    record,
    restored,
    status
  )
  expect_identical(mark, runtime$builder_project_check_identity(restored))

  changed <- restored
  changed$settings$spatial_coordinate_transforms[[
    "section_a_1_fov_1"
  ]]$rotation_degrees <- 70
  expect_false(identical(
    mark,
    runtime$builder_project_check_identity(changed)
  ))

  expect_null(runtime$builder_project_restored_check_identity(
    list(configuration = list(checked = FALSE)),
    restored,
    list(source_matches = TRUE, checked = FALSE)
  ))
  expect_null(runtime$builder_project_restored_check_identity(
    record,
    restored,
    list(source_matches = FALSE)
  ))
})

test_that("runtime snapshot replacement does not invalidate checked configuration", {
  runtime <- builder_project_test_runtime()
  first <- list(
    id = "ds1",
    snapshot = list(
      path = "/session/first",
      owner_token = "first-owner",
      object_md5 = "first-md5"
    ),
    settings = list(name = "Dataset", default_group = "cell_type")
  )
  replacement <- first
  replacement$snapshot <- list(
    path = "/session/replacement",
    owner_token = "replacement-owner",
    object_md5 = "replacement-md5"
  )

  expect_identical(
    runtime$builder_project_check_identity(first),
    runtime$builder_project_check_identity(replacement)
  )

  replacement$settings$default_group <- "cluster"
  expect_false(identical(
    runtime$builder_project_check_identity(first),
    runtime$builder_project_check_identity(replacement)
  ))
})

test_that("pending coordinate drafts participate in checked identity", {
  runtime <- builder_project_test_runtime()
  entry <- list(
    id = "ds1",
    snapshot = list(
      path = "/session/ds1",
      owner_token = "owner-ds1",
      object_md5 = strrep("a", 32L)
    ),
    settings = list(
      name = "Spatial dataset",
      images = list(),
      spatial_coordinate_transforms = list()
    ),
    acknowledgements = character(),
    spatial_drafts = list()
  )
  confirmed <- runtime$builder_project_effective_check_identity(entry, list())
  snapshot_identity <- runtime$.builder_worker_identity(entry$snapshot)
  draft <- function(rotation) {
    list(
      ds1 = list(
        `fov-a` = list(
          dataset = "ds1",
          section = "fov-a",
          snapshot_identity = snapshot_identity,
          spec = list(
            schema_version = 1L,
            rotation_degrees = rotation,
            scale = 1
          )
        )
      )
    )
  }

  expect_false(identical(
    confirmed,
    runtime$builder_project_effective_check_identity(entry, draft(37.5))
  ))
  expect_identical(
    runtime$builder_project_effective_check_identity(entry, draft(0)),
    confirmed
  )
  marks <- stats::setNames(confirmed, entry$id)
  expect_identical(
    runtime$builder_project_checked_ids(list(entry), marks, draft(37.5)),
    character()
  )
  expect_identical(
    runtime$builder_project_checked_ids(list(entry), marks, draft(0)),
    entry$id
  )
})

test_that("a matching ready project CRB repairs a missing checked flag", {
  runtime <- builder_project_test_runtime()
  record <- list(
    configuration = list(
      checked = FALSE,
      digest = "same-configuration"
    ),
    artifact = list(
      status = "ready",
      built_from_configuration = "same-configuration"
    )
  )

  expect_true(runtime$builder_project_record_configuration_confirmed(record))

  record$artifact$built_from_configuration <- "older-configuration"
  expect_false(runtime$builder_project_record_configuration_confirmed(record))
})

test_that("browser lifecycle code includes queue sync and close protection", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "www",
    "builder.js"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(source, "builder_client_import_state", fixed = TRUE)
  expect_match(source, "builder_activity_state", fixed = TRUE)
  expect_match(source, "beforeunload", fixed = TRUE)
  expect_match(source, "builder-operation-overlay", fixed = TRUE)
})

test_that("project save dialogs share concise structured copy", {
  ui_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "ui",
    "project.R"
  )
  css_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "www",
    "builder.components.css"
  )
  ui <- paste(readLines(ui_path, warn = FALSE), collapse = "\n")
  css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

  expect_match(ui, 'title = "Save this project"', fixed = TRUE)
  expect_match(
    ui,
    "Choose a folder to save your datasets and current settings.",
    fixed = TRUE
  )
  expect_match(
    ui,
    "Uploaded files will be copied into the project so you can continue later.",
    fixed = TRUE
  )
  expect_false(grepl("Keep this work resumable", ui, fixed = TRUE))
  expect_match(ui, 'class = "builder-project-dialog"', fixed = TRUE)
  expect_match(ui, 'class = "builder-project-dialog-summary"', fixed = TRUE)
  expect_match(css, ".builder-project-dialog-summary {", fixed = TRUE)
})

test_that("connection flush sends a message captured in reactive context", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(
    source,
    "activity_message <- builder_activity_message(isolate(builder_activity()))",
    fixed = TRUE
  )
  expect_match(
    source,
    paste0(
      'function\\(\\)\\s*\\{\\s*session\\$sendCustomMessage\\(\\s*',
      '"builder_activity_state",\\s*activity_message'
    ),
    perl = TRUE
  )
})

test_that("project save flush runs inside an isolated reactive scope", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(
    source,
    "session$onFlushed(\n    function() {\n      shiny::isolate({",
    fixed = TRUE
  )
})

test_that("activity locks do not rewrite unchanged text during DOM enhancement", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "www",
    "builder.js"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(
    source,
    "if (save.textContent !== saveLabel) save.textContent = saveLabel;",
    fixed = TRUE
  )
  expect_match(
    source,
    "message.textContent !== builderActivityState.busy_message",
    fixed = TRUE
  )
})

test_that("activity locks keep the loading workspace visible and cancellable", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "www",
    "builder.js"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_false(grepl(
    "workspace.inert = workspaceLocked",
    source,
    fixed = TRUE
  ))
  expect_match(
    source,
    "#builder-workspace input, #builder-workspace select, ",
    fixed = TRUE
  )
  expect_match(
    source,
    ".builder-retry-import, .builder-remove-import",
    fixed = TRUE
  )
})

test_that("dynamic content enhancement is coalesced per animation frame", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "www",
    "builder.js"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(
    source,
    "function scheduleDynamicContentEnhancement()",
    fixed = TRUE
  )
  expect_match(
    source,
    "new MutationObserver(scheduleDynamicContentEnhancement)",
    fixed = TRUE
  )
  expect_false(grepl("characterData: true", source, fixed = TRUE))
})

test_that("spatial canvas is cleared before switching datasets", {
  server_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "spatial_alignment_server.R"
  )
  client_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "builder",
    "www",
    "builder-spatial-canvas.js"
  )
  server_source <- paste(readLines(server_path, warn = FALSE), collapse = "\n")
  client_source <- paste(readLines(client_path, warn = FALSE), collapse = "\n")

  expect_match(
    server_source,
    'session$sendCustomMessage("builder_spatial_canvas_clear", list())',
    fixed = TRUE
  )
  expect_match(
    client_source,
    paste0(
      'Shiny.addCustomMessageHandler("builder_spatial_canvas_clear", ',
      'function (message) {\n',
      '      clear();\n',
      '    });'
    ),
    fixed = TRUE
  )
})
