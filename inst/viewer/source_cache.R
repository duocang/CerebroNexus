## Parsed Viewer code is immutable in installed/generated apps. Cache only the
## parsed expressions; every session still evaluates them in its own scope.
.viewer_source_cache <- new.env(parent = emptyenv())

## These helpers have no session-reactive state. Evaluate them once in a
## process-owned environment, then bind the same immutable function objects
## into each session. The remaining expressions are still evaluated locally.
.viewer_process_function_names <- c(
  "viewerUploadsEnabled",
  "viewerUploadPath",
  "viewerInitialPageDecision",
  "viewerDatasetName",
  "viewerDatasetPath",
  "viewerScatterDefaults",
  "viewerPublishChanged",
  "spatialImagePreset",
  "spatialPlotRotation",
  "rotateSpatialCoordinates",
  "viewerOutputTab",
  "viewerExpressionCells",
  "viewerExpressionCellCount",
  "viewerH5ExpressionRows",
  "viewerExpressionRow",
  "viewerExpressionValues",
  "findColumnsInteger",
  "findColumnsMatching",
  "replaceInfiniteValues",
  "neutralizeSpreadsheetFormulas",
  "prepareEmptyTable",
  "cerebroGroupFilterMask",
  "randomlySubsetCells",
  "getXYranges",
  "match_dataset_id_by_url",
  "is_cerebro_dataset",
  "read_cerebro_file",
  ".cloneCachedCrb",
  ".runtimeBackendPlanError",
  ".validateRuntimeBackendEntry",
  ".configuredRuntimeBackendPlan",
  ".runtimeBackendCacheIdentity",
  ".attachSpatialMoleculeBackend",
  ".validateThinCrbShape",
  ".runtimeH5Names",
  ".runtimeH5CellCount",
  ".checkThinCrbCells",
  ".thinCrbCellNames",
  ".hydrateThinCrbFields",
  ".deferThinCrbHydration",
  ".fallbackRuntimeBackendPlan",
  ".runtimeBackendRecoveryAdvice",
  ".attachExternalExpression",
  ".h5ExpressionRows",
  "extra_material_table_filter",
  "viewerHasTcrRepertoire",
  "filterSelectionByHiddenGroups",
  "selectedCellMask",
  "trajectorySelectionValid"
)

.viewerSourceAssignment <- function(expression) {
  if (
    is.call(expression) &&
      identical(expression[[1L]], as.name("<-")) &&
      is.symbol(expression[[2L]]) &&
      is.call(expression[[3L]]) &&
      (identical(expression[[3L]][[1L]], as.name("function")) ||
        identical(expression[[3L]][[1L]], as.name("c")))
  ) {
    return(as.character(expression[[2L]]))
  }
  NA_character_
}

## The installed package's byte code for a Viewer file's helper functions
## (written by R CMD INSTALL, see R/viewer_bytecode.R), or NULL when there is
## none for this exact file and R version. Bundles have none.
.viewerBytecode <- function(path) {
  root <- tryCatch(Cerebro.options[["cerebro_root"]], error = function(e) NULL)
  if (!is.character(root) || length(root) != 1L || !nzchar(root)) {
    return(NULL)
  }
  viewer <- normalizePath(
    file.path(root, "viewer"),
    winslash = "/",
    mustWork = FALSE
  )
  source_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!startsWith(source_path, paste0(viewer, "/"))) {
    return(NULL)
  }
  file <- file.path(
    root,
    "viewer-bytecode",
    paste0(substring(source_path, nchar(viewer) + 2L), ".rds")
  )
  if (!file.exists(file)) {
    return(NULL)
  }
  bytecode <- tryCatch(readRDS(file), error = function(e) NULL)
  if (
    !is.list(bytecode) ||
      !identical(bytecode$r_version, R.version.string) ||
      !identical(bytecode$md5, unname(tools::md5sum(path)))
  ) {
    return(NULL)
  }
  bytecode
}

viewerSource <- function(path, envir = parent.frame()) {
  path <- normalizePath(path, mustWork = TRUE)
  info <- file.info(path)
  fingerprint <- c(size = info$size, mtime = as.numeric(info$mtime))
  cached <- .viewer_source_cache[[path]]
  if (is.null(cached) || !identical(cached$fingerprint, fingerprint)) {
    expressions <- parse(file = path, keep.source = FALSE)
    shared_names <- if (identical(basename(path), "utility_functions.R")) {
      vapply(expressions, .viewerSourceAssignment, character(1))
    } else {
      rep(NA_character_, length(expressions))
    }
    ## Helpers then run compiled from their first call in this process.
    bytecode <- .viewerBytecode(path)
    if (!is.null(bytecode) && max(bytecode$index) <= length(expressions)) {
      expressions[bytecode$index] <- bytecode$code
    }
    shared <- !is.na(shared_names) &
      shared_names %in% .viewer_process_function_names
    shared_environment <- new.env(parent = environment(viewerSource))
    if (any(shared)) {
      invisible(eval(expressions[shared], envir = shared_environment))
    }
    cached <- list(
      fingerprint = fingerprint,
      expressions = expressions[!shared],
      shared_names = shared_names[shared],
      shared_environment = shared_environment
    )
    .viewer_source_cache[[path]] <- cached
  }
  for (name in cached$shared_names) {
    assign(name, cached$shared_environment[[name]], envir = envir)
  }
  invisible(eval(cached$expressions, envir = envir))
}
