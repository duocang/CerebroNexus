builder_e2e_source_runtime()

test_that("Builder release documentation matches the guided workflow", {
  builder_dir <- normalizePath(builder_profile_inst_path("builder"))
  repo <- dirname(dirname(builder_dir))
  readme_path <- file.path(repo, "README.md")
  vignette_path <- file.path(
    repo,
    "vignettes",
    "build_a_data_set_by_pointing.Rmd"
  )
  skip_if_not(
    file.exists(readme_path) && file.exists(vignette_path),
    "Builder source documentation not present (installed-package layout)"
  )

  readme <- paste(readLines(readme_path, warn = FALSE), collapse = "\n")
  readme_text <- gsub("\\s+", " ", readme)
  vignette_lines <- readLines(vignette_path, warn = FALSE)
  vignette <- paste(vignette_lines, collapse = "\n")
  vignette_text <- gsub("\\s+", " ", vignette)
  news_path <- file.path(repo, "NEWS.md")
  news <- readLines(news_path, warn = FALSE)
  news_text <- gsub("\\s+", " ", paste(news, collapse = "\n"))
  for (stage in c(
    "Import and Inspect",
    "Core setup",
    "Enhance content",
    "Review and Build"
  )) {
    expect_match(readme_text, stage, fixed = TRUE)
  }
  expect_match(readme, "build_a_data_set_by_pointing", fixed = TRUE)

  for (contract in c(
    "CRB-only",
    "generated private App",
    "private-data",
    "viewer_bundle_assets",
    "HTTP exposure",
    "snapshot disk cost",
    "Needs decision",
    "rollback",
    "recovery",
    "ownership migration",
    "Open App",
    "Reveal Folder",
    "Copy Path",
    "Copy Report"
  )) {
    expect_match(vignette, contract, fixed = TRUE)
  }
  expect_false(grepl("logged and skipped", vignette, fixed = TRUE))
  expect_match(readme_text, "planned payload targets", fixed = TRUE)
  expect_match(news_text, "planned payload targets", fixed = TRUE)
  for (contract in c(
    "distinct source adapters",
    "shared import pipeline",
    "planned payload targets",
    "snapshot payload once",
    "build-report.json",
    ".cerebro-builder-release-v1",
    "Open App is available only for a generated App"
  )) {
    expect_match(vignette_text, contract, fixed = TRUE)
  }
  combined_docs <- tolower(paste(readme_text, news_text, vignette_text))
  expect_false(grepl("exact release members", combined_docs, fixed = TRUE))
  expect_false(grepl("same adapter", combined_docs, fixed = TRUE))
  expect_false(grepl(
    "each card says whether its source is real or synthetic",
    combined_docs,
    fixed = TRUE
  ))
  expect_false(grepl(
    "disk estimate includes both sets",
    combined_docs,
    fixed = TRUE
  ))

  resource_start <- match("resource_files:", trimws(vignette_lines))
  expect_false(is.na(resource_start))
  resource_end <- which(
    trimws(vignette_lines) == "---" & seq_along(vignette_lines) > resource_start
  )[[1L]]
  resources <- trimws(vignette_lines[seq.int(
    resource_start + 1L,
    resource_end - 1L
  )])
  resources <- sub("^-\\s*", "", resources)
  resources <- resources[nzchar(resources)]
  expect_length(resources, 5L)
  expect_true(all(file.exists(file.path(dirname(vignette_path), resources))))

  version <- unname(read.dcf(file.path(repo, "DESCRIPTION"))[[1L, "Version"]])
  app <- paste(readLines(file.path(repo, "inst", "app.R")), collapse = "\n")
  news_heading <- news[grepl("^# CerebroNexus ", news)][[1L]]
  expect_identical(version, "5.0")
  expect_match(
    app,
    paste0('"cerebro_version" = "', version, '"'),
    fixed = TRUE
  )
  expect_identical(news_heading, paste("# CerebroNexus", version))
  task13 <- grep(
    "guided Builder workbench now keeps its primary action",
    news,
    fixed = TRUE
  )
  version_31 <- match("# CerebroNexus 3.1.0", news)
  expect_length(task13, 1L)
  expect_lt(task13, version_31)
})

test_that("the Builder example catalog is a stable product contract", {
  expect_true(exists("builder_example_catalog", mode = "function"))
  catalog <- builder_example_catalog()
  expected_ids <- c(
    "basic_pbmc",
    "spatial_multi_section",
    "immune_tcr_hla",
    "immune_tcr_only",
    "immune_hla_only",
    "immune_bcr_only",
    "immune_metadata_tcr",
    "immune_legacy_tcr",
    "all_content"
  )
  expect_identical(names(catalog), expected_ids)
  expect_identical(
    unname(vapply(catalog, `[[`, character(1), "id")),
    expected_ids
  )
  expect_identical(
    anyDuplicated(vapply(
      catalog,
      `[[`,
      character(1),
      "label"
    )),
    0L
  )
  required <- c(
    "id",
    "label",
    "detail",
    "provenance",
    "make",
    "serialized_path",
    "expected_manifest",
    "expected_dispositions",
    "expected_pages",
    "expected_supporting_content",
    "gallery_visible"
  )
  expect_true(all(vapply(
    catalog,
    function(record) {
      identical(names(record), required)
    },
    logical(1)
  )))
  expect_true(all(vapply(
    catalog,
    function(record) {
      record$provenance %in%
        c("real", "synthetic") &&
        file.exists(record$serialized_path) &&
        !grepl("^[a-z]+://", record$serialized_path)
    },
    logical(1)
  )))
  expect_identical(
    unname(vapply(builder_examples(), `[[`, character(1), "id")),
    expected_ids
  )
})

test_that("sourced Builder resources stay in the io.R inst tree", {
  root <- withr::local_tempdir(pattern = "builder-resource-root-")
  source_inst <- file.path(root, "checkout", "inst")
  installed_inst <- file.path(root, "current-library", "CerebroNexus")
  stale_inst <- file.path(root, "stale-library", "CerebroNexus")
  unrelated <- file.path(root, "unrelated-working-directory")
  dir.create(file.path(source_inst, "builder"), recursive = TRUE)
  dir.create(file.path(installed_inst, "builder"), recursive = TRUE)
  dir.create(stale_inst, recursive = TRUE)
  dir.create(unrelated)

  io_source <- builder_profile_inst_path("builder", "io.R")
  io_copy <- file.path(source_inst, "builder", "io.R")
  installed_io <- file.path(installed_inst, "builder", "io.R")
  expect_true(file.copy(io_source, io_copy))
  expect_true(file.copy(io_source, installed_io))
  fixture_names <- paste0(
    c(
      "spatial_multi_section",
      "immune_tcr_hla",
      "immune_tcr_only",
      "immune_hla_only",
      "immune_bcr_only",
      "immune_metadata_tcr",
      "immune_legacy_tcr",
      "all_content"
    ),
    ".rds"
  )
  resources <- c(
    "extdata/examples/pbmc_seurat.rds",
    "extdata/examples/demo_trekker.crb",
    file.path("builder", "fixtures", fixture_names)
  )
  for (relative in resources) {
    current <- file.path(source_inst, relative)
    installed <- file.path(installed_inst, relative)
    stale <- file.path(stale_inst, relative)
    dir.create(dirname(current), recursive = TRUE, showWarnings = FALSE)
    dir.create(dirname(installed), recursive = TRUE, showWarnings = FALSE)
    dir.create(dirname(stale), recursive = TRUE, showWarnings = FALSE)
    expect_true(file.copy(builder_profile_inst_path(relative), current))
    expect_true(file.copy(builder_profile_inst_path(relative), installed))
    writeBin(charToRaw("stale installed resource"), stale)
  }

  load_io <- function(loader, path) {
    runtime <- new.env(parent = globalenv())
    runtime$system.file <- local({
      root <- stale_inst
      function(..., package, lib.loc = NULL, mustWork = FALSE) {
        path <- file.path(root, ...)
        if (isTRUE(mustWork) && !file.exists(path)) {
          stop("No file found", call. = FALSE)
        }
        path
      }
    })
    if (identical(loader, "source")) {
      source(path, local = runtime, chdir = FALSE)
    } else {
      sys.source(path, envir = runtime)
    }
    runtime
  }

  withr::local_dir(unrelated)
  cases <- list(
    `direct runApp source` = list("source", io_copy, source_inst),
    `tests and data-raw sys.source` = list("sys.source", io_copy, source_inst),
    `installed app source` = list("source", installed_io, installed_inst)
  )
  for (case in names(cases)) {
    loader <- cases[[case]][[1L]]
    io_path <- cases[[case]][[2L]]
    expected_root <- cases[[case]][[3L]]
    runtime <- load_io(loader, io_path)
    catalog <- runtime$builder_example_catalog()
    expected_paths <- normalizePath(file.path(
      expected_root,
      c(resources[[1L]], resources[-c(1L, 2L)], resources[[2L]])
    ))
    observed_paths <- c(
      catalog$basic_pbmc$serialized_path,
      vapply(catalog[-1L], `[[`, character(1), "serialized_path"),
      runtime$.builder_example_path("extdata/examples/demo_trekker.crb")
    )
    expect_identical(unname(observed_paths), expected_paths, info = case)
    expect_true(all(file.exists(observed_paths)), info = case)
    expect_true(
      all(startsWith(
        observed_paths,
        paste0(normalizePath(expected_root), "/")
      )),
      info = case
    )
    expect_s4_class(
      runtime$builder_make_permanent_fixture("all_content"),
      "Seurat"
    )
  }
})

test_that("example and file adapters inspect the same immutable objects", {
  catalog <- builder_example_catalog()
  set.seed(1401)
  caller_seed <- .Random.seed

  for (record in catalog) {
    first <- record$make()
    second <- record$make()
    expect_null(first$error, info = record$id)
    expect_identical(
      serialize(first$object, NULL),
      serialize(second$object, NULL),
      info = record$id
    )
    example <- builder_adapter_inspect(
      builder_example_adapter(record$id, first$object)
    )
    file <- builder_adapter_inspect(
      builder_seurat_file_adapter(record$serialized_path)
    )
    expect_identical(
      example$legacy_profile,
      file$legacy_profile,
      info = record$id
    )
    expect_identical(example$levels, file$levels, info = record$id)
    expect_identical(
      builder_e2e_without_source(example$profile),
      builder_e2e_without_source(file$profile),
      info = record$id
    )
  }

  expect_identical(.Random.seed, caller_seed)
})

test_that("synthetic constructors and permanent fixture writes are deterministic", {
  skip_if_not_installed("callr")
  synthetic <- Filter(
    function(record) identical(record$provenance, "synthetic"),
    builder_example_catalog()
  )
  expect_length(synthetic, 8L)

  set.seed(1402)
  caller_seed <- .Random.seed
  first_objects <- lapply(synthetic, function(record) record$make()$object)
  second_objects <- lapply(synthetic, function(record) record$make()$object)
  expect_identical(.Random.seed, caller_seed)
  expect_identical(
    lapply(first_objects, serialize, connection = NULL, version = 3L),
    lapply(second_objects, serialize, connection = NULL, version = 3L)
  )

  first_dir <- withr::local_tempdir(pattern = "builder-fixtures-first-")
  second_dir <- withr::local_tempdir(pattern = "builder-fixtures-second-")
  io_path <- builder_profile_inst_path("builder", "io.R")
  write_in_fresh_process <- function(output_dir, source_path, ids) {
    callr::r(
      function(output_dir, source_path, ids) {
        sys.source(source_path, envir = .GlobalEnv)
        set.seed(1403)
        caller_seed <- .Random.seed
        builder_write_permanent_fixtures(output_dir)
        list(
          rng_restored = identical(.Random.seed, caller_seed),
          hashes = unname(tools::md5sum(file.path(
            output_dir,
            paste0(ids, ".rds")
          )))
        )
      },
      args = list(output_dir, source_path, ids),
      spinner = FALSE
    )
  }
  first_write <- write_in_fresh_process(first_dir, io_path, names(synthetic))
  second_write <- write_in_fresh_process(second_dir, io_path, names(synthetic))
  expect_identical(.Random.seed, caller_seed)
  expect_true(first_write$rng_restored)
  expect_true(second_write$rng_restored)

  names <- paste0(names(synthetic), ".rds")
  first_paths <- file.path(first_dir, names)
  second_paths <- file.path(second_dir, names)
  expect_true(all(file.exists(first_paths)))
  expect_true(all(file.exists(second_paths)))
  expect_identical(first_write$hashes, second_write$hashes)
  expect_identical(
    lapply(first_paths, function(path) {
      serialize(readRDS(path), NULL, version = 3L)
    }),
    lapply(second_paths, function(path) {
      serialize(readRDS(path), NULL, version = 3L)
    })
  )

  for (index in seq_along(synthetic)) {
    record <- synthetic[[index]]
    constructed <- first_objects[[index]]
    serialized <- readRDS(first_paths[[index]])
    expect_identical(
      serialize(constructed, NULL, version = 3L),
      serialize(serialized, NULL, version = 3L),
      info = record$id
    )
  }
  generated_names <- c(
    names,
    "spatial_section_a.png",
    "spatial_section_b.png"
  )
  committed_dir <- dirname(synthetic[[1L]]$serialized_path)
  committed_paths <- file.path(committed_dir, generated_names)
  generated_paths <- file.path(first_dir, generated_names)
  expect_true(all(file.exists(committed_paths)))
  expect_identical(
    unname(tools::md5sum(generated_paths)),
    unname(tools::md5sum(committed_paths))
  )
  read_bytes <- function(path) {
    readBin(path, what = "raw", n = as.integer(file.info(path)$size))
  }
  expect_identical(
    lapply(generated_paths, read_bytes),
    lapply(committed_paths, read_bytes)
  )
  expect_identical(.Random.seed, caller_seed)
})

test_that("immune and motif pages follow receptor semantics, not HLA presence", {
  repertoire <- function(chain) {
    list(
      sample_a = data.frame(
        barcode = "cell1",
        CTgene = paste0(chain, "V1.", chain, "J1"),
        CTnt = "ACGT",
        CTaa = "CASSLGQF",
        CTstrict = paste0(chain, "_clone"),
        stringsAsFactors = FALSE
      )
    )
  }
  crb <- function(ir = NULL, hla = NULL) {
    value <- new.env(parent = emptyenv())
    value$immune_repertoire <- ir
    value$hla_typing <- hla
    value
  }
  hla <- data.frame(sample = "sample_a", allele = "HLA-A*02:01")
  cases <- list(
    tcr_hla = list(
      crb(repertoire("TRB"), hla),
      c(
        "immune_repertoire",
        "hla_tcr_motifs"
      )
    ),
    tcr_only = list(
      crb(repertoire("TRB")),
      c(
        "immune_repertoire",
        "hla_tcr_motifs"
      )
    ),
    hla_only = list(crb(hla = hla), character()),
    bcr_only = list(crb(repertoire("IGH")), "immune_repertoire"),
    metadata_tcr = list(
      crb(repertoire("TRA")),
      c(
        "immune_repertoire",
        "hla_tcr_motifs"
      )
    ),
    legacy_tcr = list(
      crb(repertoire("TRB")),
      c(
        "immune_repertoire",
        "hla_tcr_motifs"
      )
    )
  )
  for (name in names(cases)) {
    expect_setequal(
      .builder_crb_visible_pages(cases[[name]][[1L]]),
      cases[[name]][[2L]]
    )
  }
})

test_that("Viewer defaults lead included group and projection order", {
  expect_identical(
    builder_default_first(c("sample", "cell_type", "condition"), "condition"),
    c("condition", "sample", "cell_type")
  )
  expect_identical(
    builder_default_first(c("pca", "umap", "tsne"), "tsne"),
    c("tsne", "pca", "umap")
  )
})

test_that("catalog expectations are derived from real inspection and Review", {
  for (record in builder_example_catalog()) {
    entry <- builder_e2e_entry(record)
    recommendations <- entry$settings$recommendations
    expect_type(recommendations, "list")
    expect_named(
      recommendations,
      c(
        "metadata",
        "groups",
        "projections",
        "organism",
        "nomenclature",
        "backend"
      )
    )
    expect_true(length(entry$settings$groups) > 0L, info = record$id)
    expect_true(
      all(entry$settings$groups %in% entry$settings$metadata_policy$included),
      info = record$id
    )
    expect_identical(
      entry$settings$reductions,
      recommendations$projections$included,
      info = record$id
    )
    expect_true(
      entry$settings$default_group %in% entry$settings$groups,
      info = record$id
    )
    expect_identical(
      entry$settings$default_projection,
      recommendations$projections$value,
      info = record$id
    )
    expect_identical(
      entry$settings$expression_backend,
      recommendations$backend$value,
      info = record$id
    )
    expect_false(
      identical(entry$settings$metadata_policy, recommendations$metadata),
      info = record$id
    )
    expect_length(entry$settings$metadata_policy$attention, 0L)
    expect_identical(
      recommendations$metadata$attention,
      names(Filter(
        function(column) identical(column$disposition, "attention"),
        recommendations$metadata$columns
      )),
      info = record$id
    )
    state <- builder_dataset_state(entry)
    observed <- vapply(
      state$manifest,
      function(entry) {
        entry$disposition %||% NA_character_
      },
      character(1)
    )
    observed <- observed[!is.na(observed)]
    expect_identical(
      names(observed),
      record$expected_manifest,
      info = record$id
    )
    expect_identical(observed, record$expected_dispositions, info = record$id)
    expect_identical(
      state$page_expectations$visible_conditional,
      record$expected_pages,
      info = record$id
    )
    supporting <- setdiff(record$expected_supporting_content, "extra_material")
    expect_true(
      all(file.exists(file.path(
        dirname(record$serialized_path),
        supporting
      ))),
      info = record$id
    )
  }
})

test_that("all valid examples build and reopen", {
  root <- withr::local_tempdir()
  for (record in builder_example_catalog()) {
    entry <- builder_e2e_entry(record)
    object <- record$make()$object
    snapshot <- builder_snapshot_seurat(
      object,
      file.path(root, paste0(record$id, "-snapshot")),
      available_bytes = 2^40
    )
    entry$snapshot <- snapshot
    if (record$id %in% c("spatial_multi_section", "all_content")) {
      sections <- entry$dataset_profile$spatial$sections
      images <- c("spatial_section_a.png", "spatial_section_b.png")
      image_bounds <- list(
        section_a = list(xmin = 10, xmax = 106, ymin = 20, ymax = 92),
        section_b = list(xmin = 250, xmax = 330, ymin = 40, ymax = 104)
      )
      entry$settings$images <- stats::setNames(
        lapply(seq_along(sections), function(i) {
          list(
            uri = paste0(
              "data:image/png;base64,",
              base64enc::base64encode(file.path(
                dirname(record$serialized_path),
                images[[i]]
              ))
            ),
            bounds = image_bounds[[sections[[i]]]]
          )
        }),
        sections
      )
    }
    release <- file.path(root, paste0(record$id, "-release"))
    plan <- builder_freeze_plan(list(entry), release, make_app = FALSE)
    expect_null(plan$error, info = record$id)
    expect_identical(
      plan$items[[1L]]$viewer_page_expectations$visible_conditional,
      record$expected_pages,
      info = record$id
    )
    stage <- file.path(root, paste0(record$id, "-stage"))
    dir.create(stage)
    result <- builder_execute_plan(
      plan,
      stage,
      stats::setNames(list(snapshot), record$id)
    )
    expect_identical(result$state, "success", info = record$id)
    expect_true(result$publishable, info = record$id)
    expect_length(result$built, 1L)
    reopened <- readRDS(result$built[[1L]])
    expect_setequal(
      .builder_crb_visible_pages(reopened),
      record$expected_pages
    )
    if (identical(record$id, "all_content")) {
      expect_silent(builder_e2e_validate_all_content(
        record,
        object,
        entry$settings,
        reopened
      ))
    }
  }
})

test_that("valid Seurat content blockers stop before staging or publication", {
  root <- withr::local_tempdir()
  release <- file.path(root, "invalid-release")
  dir.create(release)
  sentinel <- file.path(release, "keep.txt")
  writeLines("unchanged", sentinel)
  prior <- builder_release_identity(release)
  control <- builder_release_control_path(release)
  expect_false(file.exists(control))

  invalid <- builder_e2e_invalid_content_entry()
  expect_s4_class(invalid$object, "Seurat")
  expect_s3_class(invalid$inspected$profile, "builder_dataset_profile")
  expect_type(invalid$entry$settings$recommendations, "list")
  review <- builder_dataset_state(invalid$entry)
  expect_identical(review$readiness, "blocked")
  expect_true("immune_repertoire" %in% review$blocking_ids)

  blocked <- builder_freeze_plan(
    list(invalid$entry),
    release,
    make_app = FALSE
  )
  expect_s3_class(blocked, "builder_plan_failure")
  expect_identical(blocked$error_code, "blocking_capability")
  stage <- file.path(root, "invalid-stage")
  expect_error(
    builder_coordinator_prepare(blocked, "invalid-content-build"),
    "frozen plan|output release"
  )
  expect_error(
    builder_execute_plan(blocked, stage, list()),
    "frozen BuildPlan"
  )
  guarded_stage <- file.path(root, "guarded-stage")
  dir.create(guarded_stage)
  stage_sentinel <- file.path(guarded_stage, "keep.txt")
  writeLines("stage unchanged", stage_sentinel)
  stage_before <- tools::md5sum(stage_sentinel)
  expect_error(
    builder_execute_plan(blocked, guarded_stage, list()),
    "frozen BuildPlan"
  )

  expect_false(file.exists(stage))
  expect_identical(tools::md5sum(stage_sentinel), stage_before)
  expect_identical(list.files(guarded_stage), "keep.txt")
  expect_identical(builder_release_identity(release), prior)
  expect_identical(readLines(sentinel), "unchanged")
  expect_false(file.exists(control))
})

test_that("the exact 18 artifact combinations build, publish, and relocate", {
  root <- withr::local_tempdir()
  hermetic_library <- privacy_hermetic_library(root)
  expect_true(
    !is.null(hermetic_library) && dir.exists(hermetic_library),
    info = "generated apps require a package-free test library"
  )
  runtime_results <- list()
  matrix <- expand.grid(
    backend = c("embedded", "h5", "bpcells"),
    content = c("plain", "histology", "trekker"),
    output = c("crb_only", "generated_app"),
    stringsAsFactors = FALSE
  )
  expect_identical(nrow(matrix), 18L)
  expect_identical(anyDuplicated(matrix), 0L)

  records <- builder_example_catalog()
  source_ids <- c(
    plain = "basic_pbmc",
    histology = "spatial_multi_section",
    trekker = "all_content"
  )
  fixtures <- lapply(names(source_ids), function(content) {
    record <- records[[source_ids[[content]]]]
    entry <- builder_e2e_entry(record)
    snapshot <- builder_snapshot_seurat(
      record$make()$object,
      file.path(root, paste0(content, "-snapshot")),
      available_bytes = 2^40
    )
    entry$snapshot <- snapshot
    if (identical(content, "histology")) {
      sections <- entry$dataset_profile$spatial$sections
      image_names <- c("spatial_section_a.png", "spatial_section_b.png")
      entry$settings$images <- stats::setNames(
        lapply(seq_along(sections), function(i) {
          image <- file.path(dirname(record$serialized_path), image_names[[i]])
          list(
            uri = paste0(
              "data:image/png;base64,",
              base64enc::base64encode(image)
            ),
            bounds = list(
              xmin = as.double((i - 1L) * 200L),
              xmax = as.double((i - 1L) * 200L + 100L),
              ymin = 0,
              ymax = 100
            )
          )
        }),
        sections
      )
    }
    list(entry = entry, snapshot = snapshot)
  })
  names(fixtures) <- names(source_ids)

  for (index in seq_len(nrow(matrix))) {
    coordinate <- matrix[index, , drop = FALSE]
    label <- paste(coordinate, collapse = "/")
    fixture <- fixtures[[coordinate$content]]
    entry <- unserialize(serialize(fixture$entry, NULL))
    entry$id <- paste0("matrix-", index)
    entry$settings$name <- paste("Matrix", index)
    entry$settings$expression_backend <- coordinate$backend
    make_app <- identical(coordinate$output, "generated_app")
    release <- file.path(root, paste0("release-", index))
    plan <- builder_freeze_plan(
      list(entry),
      release,
      make_app = make_app,
      app_options = if (make_app) list(show_upload_ui = FALSE) else list()
    )
    expect_null(plan$error, info = label)
    handle <- builder_coordinator_prepare(plan, paste0("matrix-build-", index))
    result <- builder_execute_plan(
      plan,
      handle$stage,
      stats::setNames(list(fixture$snapshot), entry$id)
    )
    result$build_id <- handle$build_id
    expect_identical(result$state, "success", info = label)
    expect_true(result$publishable, info = label)
    published <- builder_coordinator_publish(handle, result)
    expect_true(published$published, info = label)
    expect_true(
      all(startsWith(
        normalizePath(published$built, winslash = "/"),
        paste0(normalizePath(release, winslash = "/"), "/")
      )),
      info = label
    )

    crb <- published$built[[1L]]
    reopened <- readRDS(crb)
    descriptor <- reopened$getExpressionBackend()
    expect_identical(descriptor$type, coordinate$backend, info = label)
    if (identical(coordinate$backend, "embedded")) {
      expect_true(length(reopened$getExpressionMatrix()) > 0L, info = label)
    } else {
      sidecar <- file.path(dirname(crb), descriptor$location)
      expect_true(
        if (coordinate$backend == "h5") {
          file.exists(sidecar) && !dir.exists(sidecar)
        } else {
          dir.exists(sidecar)
        },
        info = label
      )
      lazy <- if (coordinate$backend == "h5") {
        DelayedArray::t(HDF5Array::TENxMatrix(sidecar, group = "expression"))
      } else {
        BPCells::open_matrix_dir(sidecar)
      }
      expect_true(length(lazy) > 0L, info = label)
    }
    if (identical(coordinate$content, "histology")) {
      spatial <- reopened$spatial
      expect_identical(
        names(spatial),
        names(entry$settings$images),
        info = label
      )
      for (section in names(spatial)) {
        expect_identical(
          spatial[[section]]$histology_image,
          entry$settings$images[[section]]$uri,
          info = label
        )
        expect_identical(
          spatial[[section]]$histology_image_bounds,
          entry$settings$images[[section]]$bounds,
          info = label
        )
      }
    }
    if (identical(coordinate$content, "trekker")) {
      clusters <- reopened$getTrekker()$clusters
      expect_type(clusters, "integer")
      expect_gte(min(clusters), 0L)
    }
    if (make_app) {
      expect_true(dir.exists(published$app_dir), info = label)
      expect_true(
        file.exists(file.path(published$app_dir, "app.R")),
        info = label
      )
      expect_true(
        file.exists(file.path(published$app_dir, "cerebro_config.rds")),
        info = label
      )
      runtime_results[[label]] <- builder_e2e_run_generated_app(
        published$app_dir,
        hermetic_library,
        root,
        backend = coordinate$backend,
        content = coordinate$content,
        expected_images = entry$settings$images,
        label = label
      )
    } else {
      expect_null(published$app_dir, info = label)
      expect_false(dir.exists(file.path(release, "cerebro_app")), info = label)
    }
  }
  expect_length(runtime_results, 9L)
  expect_true(all(vapply(
    runtime_results,
    function(result) isTRUE(result$started),
    logical(1)
  )))
})
