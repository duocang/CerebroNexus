builder_e2e_source_runtime <- function(local = parent.frame()) {
  builder_profile_source_runtime(local)
  builder_dir <- builder_profile_inst_path("builder")
  for (file in c(
    "io.R",
    "recommend.R",
    "inspect.R",
    "adapters.R",
    "preview.R",
    "extras.R",
    "analysis.R",
    "marker_import.R",
    "app_bundle.R",
    "build.R",
    "prerequisite.R",
    "state.R",
    "plan.R",
    "report.R",
    "publish.R",
    "coordinator.R"
  )) {
    sys.source(file.path(builder_dir, file), envir = local)
  }
  invisible(local)
}

builder_e2e_without_source <- function(value) {
  if (!is.list(value)) {
    return(value)
  }
  value[c("source", "fingerprint")] <- NULL
  lapply(value, builder_e2e_without_source)
}

builder_e2e_review_metadata_policy <- function(recommendation, groups) {
  policy <- unserialize(serialize(recommendation, NULL, version = 3L))
  for (id in names(policy$columns)) {
    record <- policy$columns[[id]]
    if (identical(record$disposition, "blocking")) {
      next
    }
    include <- isTRUE(record$required) || id %in% groups
    disposition <- if (include) "included" else "excluded"
    record$value <- disposition
    record$disposition <- disposition
    record$effective_included <- include
    record$requires_confirmation <- FALSE
    record$group_eligible <- id %in% groups
    record$preview_allowed <- id %in% groups
    policy$columns[[id]] <- record
  }
  dispositions <- vapply(policy$columns, `[[`, character(1), "disposition")
  included <- vapply(
    policy$columns,
    `[[`,
    logical(1),
    "effective_included"
  )
  ids <- names(policy$columns)
  policy$included <- ids[included]
  policy$attention <- ids[dispositions == "attention"]
  policy$excluded <- ids[dispositions == "excluded"]
  policy$blocking <- ids[dispositions == "blocking"]
  policy$value <- policy$included
  policy$requires_confirmation <- length(policy$attention) > 0L ||
    length(policy$blocking) > 0L
  policy
}

builder_e2e_entry <- function(record, caller = parent.frame()) {
  inspected <- get("builder_adapter_inspect", caller, inherits = TRUE)(
    get("builder_example_adapter", caller, inherits = TRUE)(
      record$id,
      record$make()$object
    )
  )
  matrix <- SeuratObject::LayerData(
    inspected$object,
    assay = inspected$legacy_profile$default_assay,
    layer = inspected$legacy_profile$default_layer
  )
  recommendations <- get(
    "builder_recommend_dataset",
    caller,
    inherits = TRUE
  )(
    inspected$profile,
    matrix_summary = list(
      estimated_bytes = as.numeric(utils::object.size(matrix)),
      sparse = inherits(matrix, "sparseMatrix")
    ),
    available = list(
      build = list(bpcells = FALSE, h5 = FALSE),
      viewer = list(bpcells = FALSE, h5 = FALSE)
    ),
    required = c(
      inspected$legacy_profile$nUMI,
      inspected$legacy_profile$nGene
    ),
    dependency_ids = stats::setNames(
      list("core.qc.nUMI", "core.qc.nGene"),
      c(
        inspected$legacy_profile$nUMI,
        inspected$legacy_profile$nGene
      )
    )
  )
  settings <- get("builder_default_settings", caller, inherits = TRUE)(
    inspected$legacy_profile,
    record$label,
    recommendations = recommendations
  )
  review_groups <- as.character(inspected$legacy_profile$group_candidates)
  settings$metadata_policy <- builder_e2e_review_metadata_policy(
    recommendations$metadata,
    review_groups
  )
  settings$groups <- review_groups
  settings$included_groups <- review_groups
  settings$default_group <- inspected$legacy_profile$group_preselect[[1L]]
  list(
    id = record$id,
    profile = inspected$legacy_profile,
    dataset_profile = inspected$profile,
    levels = inspected$levels,
    revision = 0L,
    settings = settings
  )
}

builder_e2e_invalid_content_entry <- function() {
  caller <- parent.frame()
  object <- get(
    ".builder_fixture_immune",
    caller,
    inherits = TRUE
  )("tcr_hla")
  legacy <- object@misc$immune_repertoire
  legacy[[1L]]$CTaa[[1L]] <- "CASSDIVERGENTF"
  legacy[[1L]]$CTstrict[[1L]] <- "TRB_divergent_clone"
  object@misc$tcr_data <- legacy
  record <- list(
    id = "invalid-content",
    label = "Invalid content",
    make = function() list(object = object, format = "Built-in example")
  )
  inspected <- get("builder_adapter_inspect", caller, inherits = TRUE)(
    get("builder_example_adapter", caller, inherits = TRUE)(
      record$id,
      object
    )
  )
  list(
    object = object,
    inspected = inspected,
    entry = builder_e2e_entry(record, caller = caller)
  )
}

builder_e2e_validate_all_content <- function(
  record,
  source,
  settings,
  crb,
  caller = parent.frame()
) {
  check <- function(value, label) {
    if (!isTRUE(value)) {
      stop("all_content readback mismatch: ", label, call. = FALSE)
    }
  }
  same <- function(left, right) {
    isTRUE(all.equal(left, right, check.attributes = TRUE))
  }
  field_reader <- get(".builder_build_field", caller, inherits = TRUE)
  field <- function(name) field_reader(crb, name)

  promised <- names(record$expected_dispositions)[
    record$expected_dispositions == "preserved"
  ]
  check(
    identical(
      intersect(promised, c("spatial", "trekker")),
      c("spatial", "trekker")
    ),
    "catalog preserved families"
  )
  forbidden <- c(
    "marker_genes",
    "most_expressed_genes",
    "mean_expression",
    "enriched_pathways",
    "trajectories",
    "extra_material",
    "immune_repertoire",
    "hla_typing"
  )
  check(
    all(vapply(
      forbidden,
      function(name) length(field(name)) == 0L,
      logical(1)
    )),
    "Enhance content is not precomputed"
  )

  source_trekker <- source@misc$trekker
  output_trekker <- field("trekker")
  builder_fields <- c(
    "builder_group",
    "builder_colors",
    "builder_group_values"
  )
  check(
    identical(names(output_trekker), c(names(source_trekker), builder_fields)),
    "trekker source and Builder field order"
  )
  check(
    same(source_trekker, output_trekker[names(source_trekker)]),
    "trekker source fields round trip"
  )
  check(
    identical(output_trekker$builder_group, settings$default_group),
    "trekker Builder group"
  )
  expected_group_values <- as.character(
    source@meta.data[
      source_trekker$barcodes,
      settings$default_group,
      drop = TRUE
    ]
  )
  check(
    identical(output_trekker$builder_group_values, expected_group_values),
    "trekker Builder group values"
  )

  spatial <- field("spatial")
  sections <- SeuratObject::Images(source)
  check(identical(names(spatial), sections), "spatial section order")
  image_fovs <- vapply(
    record$histology_images,
    function(image) image$fov_ids[[1L]],
    character(1)
  )
  expected_images <- unique(image_fovs)
  check(
    identical(names(settings$images), expected_images),
    "default image FOV assignments"
  )
  for (section in sections) {
    source_coordinates <- SeuratObject::GetTissueCoordinates(source[[section]])
    source_coordinates <- source_coordinates[, c("x", "y"), drop = FALSE]
    output <- spatial[[section]]
    check(
      same(
        unname(as.matrix(output$coordinates)),
        unname(as.matrix(source_coordinates))
      ),
      paste(section, "coordinates")
    )
    check(
      identical(
        rownames(output$coordinates),
        SeuratObject::Cells(source[[section]])
      ),
      paste(section, "coordinate barcodes")
    )
    check(
      identical(output$coordinate_source, "object.GetTissueCoordinates"),
      paste(section, "coordinate source")
    )
    if (section %in% expected_images) {
      configured <- settings$images[[section]]
      check(
        identical(output$histology_image, configured$uri),
        paste(section, "histology image")
      )
      check(
        identical(output$histology_image_bounds, configured$bounds),
        paste(section, "image bounds")
      )
    } else {
      check(is.null(output$histology_image), "patient C has no image")
      check(is.null(output$histology_image_bounds), "patient C has no bounds")
    }
  }
  invisible(TRUE)
}
builder_e2e_run_generated_app <- function(
  app_dir,
  hermetic_library,
  root,
  backend,
  content,
  expected_images,
  label
) {
  started_at <- proc.time()[["elapsed"]]
  runtime_root <- file.path(
    root,
    paste0("runtime-", gsub("[^a-z0-9]+", "-", tolower(label)))
  )
  dir.create(runtime_root, recursive = TRUE, showWarnings = FALSE)
  check <- function(value, detail) {
    if (!isTRUE(value)) {
      stop("generated app runtime [", label, "]: ", detail, call. = FALSE)
    }
  }

  tryCatch(
    {
      port <- httpuv::randomPort(host = "127.0.0.1")
      app <- privacy_start_app(
        app_dir,
        port,
        runtime_root,
        libpath = hermetic_library,
        exclude_package = TRUE,
        test_mode = TRUE
      )
      on.exit(privacy_stop_app(app), add = TRUE)
      privacy_wait_for_app(app)

      driver <- shinytest2::AppDriver$new(
        app$base_url,
        name = paste0("builder_matrix_", gsub("[^a-z0-9]+", "_", label)),
        load_timeout = 60000
      )
      on.exit(try(driver$stop(), silent = TRUE), add = TRUE)
      driver$wait_for_idle(timeout = 60000)
      driver$wait_for_js(
        paste0(
          "document.querySelector('a[href=\"#shiny-tab-geneExpression\"]') ",
          "!== null && window.Shiny && Shiny.shinyapp.$socket.readyState === 1"
        ),
        timeout = 60000
      )

      driver$click(
        selector = "a[href='#shiny-tab-geneExpression']"
      )
      driver$wait_for_js(
        paste0(
          "document.getElementById('expression_genes_input') !== null && ",
          "document.getElementById('expression_projection_to_display') !== null"
        ),
        timeout = 60000
      )
      driver$set_inputs(expression_genes_input = "Gene1", wait_ = FALSE)
      driver$wait_for_js(
        paste0(
          "(function() {",
          "var plot = document.getElementById('expression_projection');",
          "return !!(plot && plot.data && plot.data.some(function(trace) {",
          "return trace.x && trace.x.length > 0;",
          "}));",
          "})()"
        ),
        timeout = 60000
      )

      logs <- privacy_app_logs(app)
      backend_evidence <- switch(
        backend,
        embedded = TRUE,
        h5 = grepl("Attaching h5 backend", logs, fixed = TRUE),
        bpcells = grepl("Attaching bpcells backend", logs, fixed = TRUE),
        FALSE
      )
      check(backend_evidence, paste(backend, "backend was not attached"))

      has_tab <- function(id) {
        isTRUE(driver$get_js(sprintf(
          "document.querySelector('a[href=\"#shiny-tab-%s\"]') !== null;",
          id
        )))
      }
      if (identical(content, "plain")) {
        check(!has_tab("spatial"), "plain data exposed the Spatial page")
        check(!has_tab("trekker"), "plain data exposed the Trekker page")
      } else if (identical(content, "histology")) {
        check(has_tab("spatial"), "histology data did not expose Spatial")
        check(!has_tab("trekker"), "histology data exposed Trekker")
        driver$click(
          selector = "a[href='#shiny-tab-spatial']"
        )
        driver$wait_for_js(
          paste0(
            "document.querySelector(",
            "'a[href=\"#shiny-tab-spatial\"]'",
            ").parentElement.classList.contains('active')"
          ),
          timeout = 30000
        )
        driver$wait_for_js(
          paste0(
            "(function() {",
            "var input = document.getElementById(",
            "'spatial_projection_background_image');",
            "return !!(input && input.selectize && ",
            "input.selectize.options['__embedded__']);",
            "})()"
          ),
          timeout = 60000
        )
        driver$wait_for_js(
          paste0(
            "(function() {",
            "var plot = document.getElementById('spatial_projection');",
            "return !!(plot && plot.data && plot.data.length);",
            "})()"
          ),
          timeout = 60000
        )
        driver$run_js(
          paste0(
            "document.getElementById(",
            "'spatial_projection_background_image'",
            ").selectize.setValue('__embedded__');"
          )
        )
        tryCatch(
          driver$wait_for_js(
            paste0(
              "(function() {",
              "var bg = document.getElementById(",
              "'spatial_projection_background');",
              "return !!(bg && bg.dataset.backgroundImage && ",
              "bg.dataset.backgroundImage.indexOf(",
              "'data:image/png;base64,') === 0 && ",
              "bg.dataset.boundsXmin !== undefined);",
              "})()"
            ),
            timeout = 60000
          ),
          error = function(error) {
            diagnostic <- driver$get_js(
              paste0(
                "(function() {",
                "var input = document.getElementById(",
                "'spatial_projection_background_image');",
                "var plot = document.getElementById('spatial_projection');",
                "var bg = document.getElementById(",
                "'spatial_projection_background');",
                "return {value: input && input.selectize ? ",
                "input.selectize.getValue() : null, ",
                "plot: !!(plot && plot.data && plot.data.length), ",
                "background: bg ? bg.dataset.backgroundImage || '' : null, ",
                "bounds: bg ? bg.dataset.boundsXmin || null : null};",
                "})()"
              )
            )
            stop(
              conditionMessage(error),
              "\nDOM: ",
              paste(capture.output(str(diagnostic)), collapse = " "),
              "\n",
              privacy_app_logs(app),
              call. = FALSE
            )
          }
        )
        rendered <- driver$get_js(
          paste0(
            "(function() {",
            "var bg = document.getElementById('spatial_projection_background');",
            "return {image: bg.dataset.backgroundImage, bounds: {",
            "xmin: Number(bg.dataset.boundsXmin),",
            "xmax: Number(bg.dataset.boundsXmax),",
            "ymin: Number(bg.dataset.boundsYmin),",
            "ymax: Number(bg.dataset.boundsYmax)}};",
            "})()"
          )
        )
        expected <- expected_images[[1L]]
        check(
          identical(rendered$image, expected$uri),
          "Spatial did not render the embedded data URI"
        )
        check(
          isTRUE(all.equal(rendered$bounds, expected$bounds)),
          "Spatial did not consume the embedded image bounds"
        )
      } else if (identical(content, "trekker")) {
        check(has_tab("trekker"), "Trekker data did not expose Trekker")
        driver$click(
          selector = "a[href='#shiny-tab-trekker']"
        )
        driver$wait_for_js(
          paste0(
            "document.querySelector(",
            "'a[href=\"#shiny-tab-trekker\"]'",
            ").parentElement.classList.contains('active')"
          ),
          timeout = 30000
        )
        driver$wait_for_js(
          paste0(
            "(function() {",
            "var subline = document.querySelector('#tk-subline code');",
            "var spatial = document.getElementById('tk-cv-sp');",
            "var umap = document.getElementById('tk-cv-um');",
            "return !!(subline && subline.textContent.trim().length > 0 && ",
            "spatial && spatial.width > 0 && spatial.height > 0 && ",
            "umap && umap.width > 0 && umap.height > 0);",
            "})()"
          ),
          timeout = 60000
        )
      }

      list(
        started = TRUE,
        backend = backend,
        content = content,
        elapsed = unname(proc.time()[["elapsed"]] - started_at)
      )
    },
    error = function(error) {
      stop(
        "generated app runtime [",
        label,
        "]: ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )
}
