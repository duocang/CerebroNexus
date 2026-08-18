##----------------------------------------------------------------------------##
## Source adapters and immutable Seurat snapshots.
##
## File and example inputs differ only while loading. Once loaded, both pass
## through the same inspection and snapshot path. A snapshot owns every disk
## backing used by the object, so later builds never depend on mutable input.
##----------------------------------------------------------------------------##

.builder_adapter_abort <- function(message) {
  stop(message, call. = FALSE)
}

.builder_adapter_scalar_text <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
}

.builder_path_is_link <- function(path) {
  link <- tryCatch(Sys.readlink(path), error = function(error) "")
  length(link) == 1L && !is.na(link) && nzchar(link)
}

.builder_path_chain_has_link <- function(path, include_leaf = TRUE) {
  expanded <- path.expand(path)
  expanded <- gsub("\\\\", "/", expanded)
  if (.Platform$OS.type == "windows") {
    if (grepl("^[A-Za-z]:/", expanded)) {
      current <- substr(expanded, 1L, 3L)
      remainder <- substring(expanded, 4L)
    } else if (startsWith(expanded, "//")) {
      unc <- strsplit(sub("^//", "", expanded), "/", fixed = TRUE)[[1L]]
      unc <- unc[nzchar(unc)]
      if (length(unc) < 2L) {
        return(TRUE)
      }
      current <- paste0("//", unc[[1L]], "/", unc[[2L]])
      remainder <- paste(unc[-c(1L, 2L)], collapse = "/")
    } else {
      expanded <- gsub("\\\\", "/", file.path(getwd(), expanded))
      current <- substr(expanded, 1L, 3L)
      remainder <- substring(expanded, 4L)
    }
    components <- strsplit(remainder, "/", fixed = TRUE)[[1L]]
    components <- components[nzchar(components)]
  } else {
    if (!startsWith(expanded, "/")) {
      expanded <- file.path(getwd(), expanded)
    }
    components <- strsplit(expanded, "/", fixed = TRUE)[[1L]]
    components <- components[nzchar(components)]
    current <- "/"
  }
  if (!include_leaf && length(components)) {
    components <- head(components, -1L)
  }
  for (component in components) {
    current <- if (identical(current, "/")) {
      paste0("/", component)
    } else if (endsWith(current, "/")) {
      paste0(current, component)
    } else {
      file.path(current, component)
    }
    if (
      (file.exists(current) || dir.exists(current)) &&
        .builder_path_is_link(current)
    ) {
      target <- Sys.readlink(current)
      if (
        identical(Sys.info()[["sysname"]], "Darwin") &&
          identical(
            unname(c(
              "/etc" = "private/etc",
              "/tmp" = "private/tmp",
              "/var" = "private/var"
            )[current]),
            target
          )
      ) {
        next
      }
      return(TRUE)
    }
  }
  FALSE
}

.builder_path_tree_has_link <- function(path) {
  if (.builder_path_chain_has_link(path)) {
    return(TRUE)
  }
  if (!dir.exists(path)) {
    return(FALSE)
  }
  walk <- function(directory) {
    members <- list.files(
      directory,
      all.files = TRUE,
      no.. = TRUE,
      recursive = FALSE,
      full.names = TRUE
    )
    for (member in members) {
      if (.builder_path_is_link(member)) {
        return(TRUE)
      }
      if (dir.exists(member) && walk(member)) {
        return(TRUE)
      }
    }
    FALSE
  }
  walk(path)
}

.builder_canonical_path <- function(path, must_work = TRUE) {
  normalizePath(path, winslash = "/", mustWork = must_work)
}

.builder_path_within <- function(path, root) {
  path <- .builder_canonical_path(path, must_work = TRUE)
  root <- .builder_canonical_path(root, must_work = TRUE)
  identical(path, root) || startsWith(path, paste0(root, "/"))
}

.builder_saved_cache <- function(object) {
  if (!inherits(object, "Seurat") || !"tools" %in% methods::slotNames(object)) {
    return(NULL)
  }
  keys <- grep(
    "(^|::)SaveSeuratRds$",
    names(object@tools),
    value = TRUE
  )
  if (!length(keys)) {
    return(NULL)
  }
  object@tools[[keys[[1L]]]]
}

.builder_clear_saved_cache <- function(object) {
  if (inherits(object, "Seurat") && "tools" %in% methods::slotNames(object)) {
    keys <- grep(
      "(^|::)SaveSeuratRds$",
      names(object@tools),
      value = TRUE
    )
    object@tools[keys] <- NULL
  }
  object
}

.builder_file_source_state <- function(path) {
  if (
    !.builder_adapter_scalar_text(path) ||
      !file.exists(path) ||
      dir.exists(path) ||
      .builder_path_chain_has_link(path)
  ) {
    .builder_adapter_abort("The Seurat source changed since it was selected.")
  }
  info <- file.info(path)
  if (
    nrow(info) != 1L ||
      is.na(info$size) ||
      !is.finite(info$size) ||
      is.na(info$mtime)
  ) {
    .builder_adapter_abort("The Seurat source file metadata is unavailable.")
  }
  list(
    location = .builder_canonical_path(path),
    size = unname(info$size),
    mtime = as.numeric(info$mtime)
  )
}

.builder_file_source_fingerprint <- function(state) {
  content_md5 <- unname(as.character(tools::md5sum(state$location)))
  paste(
    "builder-snapshot-v2",
    basename(state$location),
    state$size,
    format(
      as.POSIXct(state$mtime, origin = "1970-01-01", tz = "UTC"),
      "%Y-%m-%dT%H:%M:%OS6%z"
    ),
    content_md5,
    sep = ":"
  )
}

.builder_adapter_after_read <- function(adapter) invisible(adapter)

.builder_cached_layers_materialized <- function(object, cache) {
  if (is.null(cache)) {
    return(TRUE)
  }
  if (
    !is.data.frame(cache) ||
      !all(c("assay", "layer") %in% names(cache)) ||
      !is.character(cache$assay) ||
      !is.character(cache$layer) ||
      anyNA(cache$assay) ||
      anyNA(cache$layer) ||
      any(!nzchar(cache$assay)) ||
      any(!nzchar(cache$layer))
  ) {
    return(FALSE)
  }
  all(vapply(
    seq_len(nrow(cache)),
    function(index) {
      assay <- cache$assay[[index]]
      layer <- cache$layer[[index]]
      if (!assay %in% names(object@assays)) {
        return(FALSE)
      }
      layer %in%
        tryCatch(
          SeuratObject::Layers(object[[assay]]),
          error = function(error) character()
        )
    },
    logical(1)
  ))
}

#' Describe a Seurat file source without loading it.
builder_seurat_file_adapter <- function(path) {
  if (!.builder_adapter_scalar_text(path)) {
    .builder_adapter_abort("A Seurat file path must be one non-empty string.")
  }
  if (.builder_path_chain_has_link(path)) {
    .builder_adapter_abort("A Seurat source cannot be a symbolic link.")
  }
  if (!file.exists(path)) {
    .builder_adapter_abort("The Seurat source file does not exist.")
  }
  if (dir.exists(path)) {
    .builder_adapter_abort("The Seurat source must be a regular file.")
  }
  format <- builder_match_format(path)
  if (is.null(format)) {
    .builder_adapter_abort("The Seurat source has an unsupported file format.")
  }
  source_state <- .builder_file_source_state(path)
  structure(
    list(
      type = "file",
      location = source_state$location,
      format = format$label,
      fingerprint = .builder_file_source_fingerprint(source_state),
      source_state = source_state,
      reader = format
    ),
    class = c("builder_source_adapter", "list")
  )
}

#' Describe an in-memory example source.
builder_example_adapter <- function(id, object) {
  if (!.builder_adapter_scalar_text(id)) {
    .builder_adapter_abort("An example adapter requires a non-empty id.")
  }
  if (!inherits(object, "Seurat")) {
    .builder_adapter_abort("An example adapter requires a Seurat object.")
  }
  structure(
    list(
      type = "example",
      location = id,
      format = "Built-in example",
      fingerprint = paste0(
        "example:",
        id,
        ":",
        tryCatch(
          as.character(methods::slot(object, "version")),
          error = function(error) "unknown"
        )
      ),
      object = object
    ),
    class = c("builder_source_adapter", "list")
  )
}

.builder_adapter_load <- function(adapter) {
  if (identical(adapter$type, "example")) {
    object <- adapter$object
  } else {
    before <- .builder_file_source_state(adapter$location)
    if (!identical(before, adapter$source_state)) {
      .builder_adapter_abort("The Seurat source changed since it was selected.")
    }
    read <- builder_read_object(adapter$location)
    .builder_adapter_after_read(adapter)
    after <- .builder_file_source_state(adapter$location)
    if (!identical(before, after)) {
      .builder_adapter_abort(
        "The Seurat source changed while it was being read."
      )
    }
    if (!is.null(read$error)) {
      .builder_adapter_abort(read$error)
    }
    object <- read$object
  }
  cache <- .builder_saved_cache(object)
  if (!is.null(cache) && !.builder_cached_layers_materialized(object, cache)) {
    .builder_adapter_abort(paste0(
      "This file is an incomplete SaveSeuratRds cache stub. Open it with ",
      "LoadSeuratRds in a trusted R session, verify every layer loaded, then ",
      "save the materialized Seurat object before adding it to Builder."
    ))
  }
  .builder_clear_saved_cache(object)
}

.builder_adapter_inspect <- function(adapter, progress = NULL) {
  if (!inherits(adapter, "builder_source_adapter")) {
    .builder_adapter_abort("Expected a Builder source adapter.")
  }
  if (is.function(progress)) {
    progress("reading")
  }
  object <- .builder_adapter_load(adapter)
  source <- list(
    type = adapter$type,
    location = adapter$location,
    fingerprint = adapter$fingerprint,
    format = adapter$format
  )
  if (is.function(progress)) {
    progress("inspecting")
  }
  profile <- builder_dataset_profile(object, source)
  if (is.function(progress)) {
    progress("validating")
  }
  legacy <- describe_seurat(object)
  list(
    object = object,
    profile = profile,
    legacy_profile = legacy,
    levels = builder_group_levels_for(object, legacy$group_candidates),
    format = adapter$format,
    source = source
  )
}

#' Load and inspect either a file or example adapter through one path.
builder_adapter_inspect <- function(adapter) {
  .builder_adapter_inspect(adapter)
}

.builder_snapshot_now <- function() Sys.time()

.builder_snapshot_after_copy <- function() invisible(NULL)

.builder_snapshot_after_publish <- function(snapshot) invisible(snapshot)

.builder_snapshot_token <- function() {
  paste0(
    Sys.getpid(),
    "-",
    format(.builder_snapshot_now(), "%Y%m%d%H%M%OS6", tz = "UTC"),
    "-",
    basename(tempfile("owner-"))
  )
}

## -- Snapshot cache closure and safe loader parsing -------------------------

.builder_snapshot_cache_members <- function(path) {
  if (!.builder_adapter_scalar_text(path)) {
    .builder_adapter_abort("A layer cache contains a malformed path.")
  }
  if (
    grepl(",", path, fixed = TRUE) && (file.exists(path) || dir.exists(path))
  ) {
    .builder_adapter_abort(paste0(
      "An on-disk layer path contains a comma and is ambiguous. ",
      "Move that backing store to a path without commas and retry."
    ))
  }
  members <- strsplit(path, ",", fixed = TRUE)[[1L]]
  if (!length(members) || any(!nzchar(members))) {
    .builder_adapter_abort("A layer cache contains a malformed comma path.")
  }
  if (any(!file.exists(members) & !dir.exists(members))) {
    .builder_adapter_abort(paste0(
      "An on-disk layer dependency is missing. Restore every backing path ",
      "and retry."
    ))
  }
  members
}

.builder_snapshot_validate_cache <- function(cache) {
  required <- c("layer", "path", "class", "pkg", "fxn", "assay")
  if (is.null(cache)) {
    return(list(cache = NULL, members = list()))
  }
  if (!is.data.frame(cache) || !all(required %in% names(cache))) {
    .builder_adapter_abort("SaveSeuratRds produced a malformed layer cache.")
  }
  for (column in required) {
    if (!is.character(cache[[column]]) || anyNA(cache[[column]])) {
      .builder_adapter_abort("SaveSeuratRds produced a malformed layer cache.")
    }
  }
  if (any(!cache$pkg %in% c("BPCells", "HDF5Array"))) {
    .builder_adapter_abort(
      "The snapshot layer loader is not allowed by Builder."
    )
  }
  missing_packages <- unique(cache$pkg[
    !vapply(
      cache$pkg,
      requireNamespace,
      logical(1),
      quietly = TRUE
    )
  ])
  if (length(missing_packages)) {
    .builder_adapter_abort(paste0(
      "Snapshot loading requires missing package(s): ",
      paste(missing_packages, collapse = ", "),
      ". Install them and retry."
    ))
  }
  members <- lapply(cache$path, .builder_snapshot_cache_members)
  for (path in unique(unlist(members, use.names = FALSE))) {
    if (.builder_path_tree_has_link(path)) {
      .builder_adapter_abort(paste0(
        "An on-disk layer backing tree contains a symbolic link: ",
        path,
        ". Replace links with real files before retrying."
      ))
    }
  }
  list(cache = cache, members = members)
}

.builder_expected_bpcells_loader <- function(loaders) {
  if (length(loaders) == 1L) {
    return(loaders[[1L]])
  }
  paste(
    "function(x) {",
    "paths <- unlist(x = strsplit(x = x, split = ','));",
    "fxns <- list(",
    paste(paste0("'", loaders, "'"), collapse = ", "),
    ");",
    "mats <- vector(mode = 'list', length = length(x = paths));",
    "for (i in seq_along(paths)) {",
    "fn <- eval(str2lang(fxns[[i]]));",
    "mats[[i]] <- fn(paths[i]);",
    "};",
    "return(Reduce(cbind, mats));",
    "}"
  )
}

.builder_loader_function_body <- function(loader) {
  parsed <- tryCatch(parse(text = loader), error = function(error) NULL)
  if (is.null(parsed) || length(parsed) != 1L) {
    return(NULL)
  }
  function_call <- parsed[[1L]]
  if (
    !is.call(function_call) ||
      !identical(function_call[[1L]], as.name("function")) ||
      !length(function_call) %in% c(3L, 4L) ||
      (length(function_call) == 4L && !is.null(function_call[[4L]]))
  ) {
    return(NULL)
  }
  arguments <- function_call[[2L]]
  if (
    !is.pairlist(arguments) ||
      !identical(arguments, formals(function(x) NULL))
  ) {
    return(NULL)
  }
  function_call[[3L]]
}

.builder_loader_namespace_call <- function(body, package, function_name) {
  if (!is.call(body)) {
    return(NULL)
  }
  call <- as.list(body)
  namespace_call <- call[[1L]]
  if (!is.call(namespace_call)) {
    return(NULL)
  }
  namespace <- as.list(namespace_call)
  if (
    length(namespace) != 3L ||
      !identical(namespace[[1L]], as.name("::")) ||
      !identical(namespace[[2L]], as.name(package)) ||
      !identical(namespace[[3L]], as.name(function_name))
  ) {
    return(NULL)
  }
  call
}

.builder_bpcells_single_contract <- function(loader, path) {
  body <- .builder_loader_function_body(loader)
  directory <- .builder_loader_namespace_call(
    body,
    "BPCells",
    "open_matrix_dir"
  )
  if (
    !is.null(directory) &&
      identical(names(directory), c("", "dir")) &&
      identical(directory[[2L]], as.name("x")) &&
      dir.exists(path)
  ) {
    return(list(type = "directory", loader = trimws(loader)))
  }
  hdf5 <- .builder_loader_namespace_call(
    body,
    "BPCells",
    "open_matrix_hdf5"
  )
  if (
    !is.null(hdf5) &&
      identical(names(hdf5), c("", "path", "group")) &&
      identical(hdf5[[2L]], as.name("x")) &&
      is.character(hdf5[[3L]]) &&
      length(hdf5[[3L]]) == 1L &&
      !is.na(hdf5[[3L]]) &&
      nzchar(hdf5[[3L]]) &&
      file.exists(path)
  ) {
    return(list(
      type = "hdf5",
      group = hdf5[[3L]],
      loader = trimws(loader)
    ))
  }
  NULL
}

.builder_bpcells_loader_contract <- function(loader, paths) {
  loader <- trimws(loader)
  if (length(paths) == 1L) {
    contract <- .builder_bpcells_single_contract(loader, paths[[1L]])
    if (is.null(contract)) {
      return(NULL)
    }
    return(list(contract))
  }
  token_pattern <- paste0(
    "function\\(x\\) BPCells::(",
    "open_matrix_dir\\(dir = x\\)|",
    "open_matrix_hdf5\\(path = x, group = ",
    "'[^']+' \\))"
  )
  positions <- gregexpr(token_pattern, loader, perl = TRUE)[[1L]]
  if (identical(positions[[1L]], -1L)) {
    return(NULL)
  }
  tokens <- regmatches(loader, list(positions))[[1L]]
  if (length(tokens) != length(paths)) {
    return(NULL)
  }
  contracts <- Map(.builder_bpcells_single_contract, tokens, paths)
  if (any(vapply(contracts, is.null, logical(1)))) {
    return(NULL)
  }
  expected <- .builder_expected_bpcells_loader(tokens)
  if (!identical(loader, expected)) {
    return(NULL)
  }
  contracts
}

.builder_hdf5_loader_contract <- function(loader) {
  body <- .builder_loader_function_body(loader)
  call <- .builder_loader_namespace_call(body, "HDF5Array", "HDF5Array")
  if (
    is.null(call) ||
      !identical(names(call), c("", "filepath", "name", "as.sparse")) ||
      !identical(call[[2L]], as.name("x")) ||
      !is.character(call[[3L]]) ||
      length(call[[3L]]) != 1L ||
      is.na(call[[3L]]) ||
      !nzchar(call[[3L]]) ||
      !is.logical(call[[4L]]) ||
      length(call[[4L]]) != 1L ||
      is.na(call[[4L]])
  ) {
    return(NULL)
  }
  list(
    name = call[[3L]],
    sparse = call[[4L]]
  )
}

.builder_snapshot_load_layer <- function(cache, members, index) {
  package <- cache$pkg[[index]]
  loader <- cache$fxn[[index]]
  paths <- members[[index]]
  if (identical(package, "BPCells")) {
    contracts <- .builder_bpcells_loader_contract(loader, paths)
    if (is.null(contracts)) {
      .builder_adapter_abort(
        paste0(
          "The snapshot BPCells loader is not allowed by Builder. Only ",
          "MatrixDir and MatrixH5 inputs are supported."
        )
      )
    }
    matrices <- Map(
      function(path, contract) {
        if (identical(contract$type, "directory")) {
          return(BPCells::open_matrix_dir(path))
        }
        BPCells::open_matrix_hdf5(path, group = contract$group)
      },
      paths,
      contracts
    )
    if (length(matrices) == 1L) {
      return(matrices[[1L]])
    }
    return(Reduce(cbind, matrices))
  }
  if (identical(package, "HDF5Array")) {
    contract <- .builder_hdf5_loader_contract(loader)
    if (
      is.null(contract) ||
        length(paths) != 1L ||
        !file.exists(paths[[1L]]) ||
        !identical(cache$class[[index]], "HDF5Matrix")
    ) {
      .builder_adapter_abort(
        "The snapshot HDF5Array loader is not allowed by Builder."
      )
    }
    return(HDF5Array::HDF5Array(
      filepath = paths[[1L]],
      name = contract$name,
      as.sparse = contract$sparse
    ))
  }
  .builder_adapter_abort(
    "The snapshot layer loader is not allowed by Builder."
  )
}

.builder_snapshot_reopen_stub <- function(stub, discovered) {
  if (!inherits(stub, "Seurat")) {
    .builder_adapter_abort(
      "The Builder snapshot did not contain a Seurat object."
    )
  }
  cache <- discovered$cache
  if (is.null(cache)) {
    return(.builder_clear_saved_cache(stub))
  }
  object <- stub
  for (index in seq_len(nrow(cache))) {
    assay <- cache$assay[[index]]
    if (!assay %in% names(object@assays)) {
      .builder_adapter_abort("A snapshot cache names an assay that is absent.")
    }
    layer <- .builder_snapshot_load_layer(cache, discovered$members, index)
    tryCatch(
      suppressWarnings(
        SeuratObject::LayerData(
          object,
          assay = assay,
          layer = cache$layer[[index]]
        ) <- layer
      ),
      error = function(error) {
        .builder_adapter_abort(paste0(
          "The snapshot layer could not be restored: ",
          conditionMessage(error)
        ))
      }
    )
  }
  .builder_clear_saved_cache(object)
}

## -- Snapshot filesystem and disk budget -----------------------------------

.builder_snapshot_size <- function(path) {
  paths <- if (dir.exists(path)) {
    c(
      path,
      list.files(
        path,
        all.files = TRUE,
        no.. = TRUE,
        recursive = TRUE,
        full.names = TRUE,
        include.dirs = TRUE
      )
    )
  } else {
    path
  }
  info <- file.info(paths)
  sum(info$size[!info$isdir & !is.na(info$size)])
}

.builder_snapshot_tree_state <- function(path) {
  if (.builder_path_tree_has_link(path)) {
    .builder_adapter_abort("A snapshot source contains a symbolic link.")
  }
  root <- .builder_canonical_path(path)
  records <- list()
  visit <- function(current, relative) {
    info <- file.info(current)
    if (
      nrow(info) != 1L ||
        is.na(info$isdir) ||
        is.na(info$size) ||
        is.na(info$mtime)
    ) {
      .builder_adapter_abort(
        "A snapshot source changed while it was inspected."
      )
    }
    records[[length(records) + 1L]] <<- data.frame(
      path = relative,
      directory = isTRUE(info$isdir),
      size = unname(info$size),
      mtime = as.numeric(info$mtime),
      stringsAsFactors = FALSE
    )
    if (isTRUE(info$isdir)) {
      children <- sort(list.files(
        current,
        all.files = TRUE,
        no.. = TRUE,
        recursive = FALSE,
        full.names = TRUE
      ))
      for (child in children) {
        if (.builder_path_is_link(child)) {
          .builder_adapter_abort("A snapshot source contains a symbolic link.")
        }
        child_relative <- if (nzchar(relative)) {
          file.path(relative, basename(child))
        } else {
          basename(child)
        }
        visit(child, child_relative)
      }
    }
  }
  visit(root, "")
  do.call(rbind, records)
}

.builder_snapshot_space_query <- function(
  path,
  os_type = .Platform$OS.type
) {
  if (identical(os_type, "windows")) {
    if (grepl("^(//|\\\\\\\\)", path)) {
      .builder_adapter_abort(paste0(
        "Free-space checks for Windows UNC snapshot paths are not supported. ",
        "Choose a local drive path."
      ))
    }
    canonical <- if (grepl("^[A-Za-z]:[\\\\/]", path)) {
      path
    } else {
      normalizePath(path, winslash = "\\", mustWork = TRUE)
    }
    drive <- sub(":.*$", "", canonical)
    if (!grepl("^[A-Za-z]$", drive)) {
      .builder_adapter_abort("Builder could not identify the snapshot drive.")
    }
    expression <- paste0("(Get-PSDrive -Name '", drive, "').Free")
    return(list(
      command = "powershell",
      args = c(
        "-NoProfile",
        "-NonInteractive",
        "-Command",
        shQuote(expression)
      ),
      multiplier = 1
    ))
  }
  list(
    command = "df",
    args = c("-Pk", shQuote(path)),
    multiplier = 1024
  )
}

.builder_snapshot_available_bytes <- function(
  path,
  os_type = .Platform$OS.type,
  runner = system2
) {
  query <- .builder_snapshot_space_query(path, os_type = os_type)
  output <- tryCatch(
    runner(
      query$command,
      query$args,
      stdout = TRUE,
      stderr = TRUE
    ),
    error = function(error) character()
  )
  status <- attr(output, "status")
  if (!length(output) || (!is.null(status) && status != 0L)) {
    .builder_adapter_abort(paste0(
      "Builder could not determine free disk space for the snapshot. ",
      "Check the destination with df and retry."
    ))
  }
  if (identical(os_type, "windows")) {
    value <- suppressWarnings(as.numeric(trimws(output[[length(output)]])))
  } else {
    fields <- strsplit(trimws(output[[length(output)]]), "[[:space:]]+")[[1L]]
    value <- if (length(fields) >= 4L) {
      suppressWarnings(as.numeric(fields[[4L]]))
    } else {
      NA_real_
    }
  }
  if (length(value) != 1L || is.na(value) || !is.finite(value) || value < 0) {
    .builder_adapter_abort(paste0(
      "Builder could not interpret free disk space for the snapshot. ",
      "Check the destination with df and retry."
    ))
  }
  value * query$multiplier
}

.builder_snapshot_check_budget <- function(closure_bytes, available_bytes) {
  if (
    !is.numeric(available_bytes) ||
      length(available_bytes) != 1L ||
      is.na(available_bytes) ||
      !is.finite(available_bytes) ||
      available_bytes < 0
  ) {
    .builder_adapter_abort(
      "Available free space must be one finite byte count."
    )
  }
  .builder_snapshot_check_headroom(available_bytes)
  if (
    closure_bytes + 1024^3 > available_bytes ||
      closure_bytes > available_bytes * 0.8
  ) {
    .builder_adapter_abort(paste0(
      "There is not enough free space for an immutable snapshot plus 1 GiB ",
      "headroom. Free disk space or choose another temporary volume."
    ))
  }
  invisible(TRUE)
}

.builder_snapshot_check_headroom <- function(available_bytes) {
  if (
    !is.numeric(available_bytes) ||
      length(available_bytes) != 1L ||
      is.na(available_bytes) ||
      !is.finite(available_bytes) ||
      available_bytes < 0
  ) {
    .builder_adapter_abort(
      "Available free space must be one finite byte count."
    )
  }
  headroom <- 1024^3
  if (available_bytes < headroom) {
    .builder_adapter_abort(paste0(
      "There is not enough free space to preserve the required 1 GiB ",
      "headroom. Free disk space or choose another temporary volume."
    ))
  }
  invisible(TRUE)
}

.builder_snapshot_copy <- function(source, destination) {
  if (dir.exists(source)) {
    if (!dir.create(destination, recursive = TRUE, showWarnings = FALSE)) {
      .builder_adapter_abort("Could not create a snapshot backing directory.")
    }
    members <- list.files(
      source,
      all.files = TRUE,
      no.. = TRUE,
      full.names = TRUE
    )
    if (
      length(members) &&
        !all(file.copy(
          members,
          destination,
          recursive = TRUE,
          copy.mode = TRUE,
          copy.date = TRUE
        ))
    ) {
      .builder_adapter_abort("Could not copy an on-disk layer directory.")
    }
  } else if (
    !file.copy(
      source,
      destination,
      copy.mode = TRUE,
      copy.date = TRUE
    )
  ) {
    .builder_adapter_abort("Could not copy an on-disk layer file.")
  }
  invisible(destination)
}

.builder_snapshot_permissions <- function(root) {
  if (.Platform$OS.type == "windows") {
    ## This contract guarantees POSIX mode bits only. Windows ACL hardening is
    ## deployment-specific and intentionally remains outside this task.
    return(invisible(TRUE))
  }
  directories <- c(
    root,
    list.dirs(root, recursive = TRUE, full.names = TRUE)
  )
  files <- list.files(
    root,
    all.files = TRUE,
    no.. = TRUE,
    recursive = TRUE,
    full.names = TRUE
  )
  files <- files[!dir.exists(files)]
  if (length(directories)) {
    Sys.chmod(unique(directories), mode = "0700", use_umask = FALSE)
  }
  if (length(files)) {
    Sys.chmod(files, mode = "0600", use_umask = FALSE)
  }
  directory_modes <- as.integer(file.info(unique(directories))$mode)
  file_modes <- as.integer(file.info(files)$mode)
  if (
    anyNA(directory_modes) ||
      any(directory_modes != strtoi("700", base = 8L)) ||
      anyNA(file_modes) ||
      any(file_modes != strtoi("600", base = 8L))
  ) {
    .builder_adapter_abort(
      "Builder could not enforce private snapshot permissions."
    )
  }
  invisible(TRUE)
}

.builder_snapshot_save_stub <- function(object, path) {
  SaveSeuratRds <- SeuratObject::SaveSeuratRds
  tryCatch(
    withCallingHandlers(
      SaveSeuratRds(
        object,
        file = path,
        move = FALSE,
        compress = FALSE
      ),
      warning = function(warning) {
        message <- conditionMessage(warning)
        if (
          grepl(
            "No layers found matching search pattern provided",
            message,
            fixed = TRUE
          ) ||
            grepl(
              "The matrix provided does not exist on-disk",
              message,
              fixed = TRUE
            )
        ) {
          invokeRestart("muffleWarning")
        }
      }
    ),
    error = function(error) {
      .builder_adapter_abort(paste0(
        "Could not inspect on-disk Seurat layers: ",
        conditionMessage(error)
      ))
    }
  )
}

## -- Snapshot ownership and destructive cleanup ----------------------------

.builder_snapshot_marker_path <- function(path) {
  file.path(path, ".builder-owner.rds")
}

.builder_stage_marker_path <- function(path) {
  file.path(path, ".builder-stage-owner.rds")
}

.builder_stage_owner <- function(path) {
  owner <- list(
    path = .builder_canonical_path(path),
    owner_token = .builder_snapshot_token(),
    created_at = .builder_snapshot_now()
  )
  saveRDS(owner, .builder_stage_marker_path(path))
  if (.Platform$OS.type != "windows") {
    Sys.chmod(path, mode = "0700", use_umask = FALSE)
    Sys.chmod(
      .builder_stage_marker_path(path),
      mode = "0600",
      use_umask = FALSE
    )
  }
  owner
}

.builder_stage_owned_at <- function(path, owner) {
  if (
    !dir.exists(path) ||
      .builder_path_is_link(path) ||
      !is.list(owner)
  ) {
    return(FALSE)
  }
  marker_path <- .builder_stage_marker_path(path)
  if (!file.exists(marker_path) || .builder_path_is_link(marker_path)) {
    return(FALSE)
  }
  marker <- tryCatch(readRDS(marker_path), error = function(error) NULL)
  is.list(marker) &&
    identical(marker$path, owner$path) &&
    identical(marker$owner_token, owner$owner_token) &&
    identical(marker$created_at, owner$created_at)
}

.builder_snapshot_marker <- function(snapshot) {
  path <- .builder_snapshot_marker_path(snapshot$path)
  if (!file.exists(path) || .builder_path_is_link(path)) {
    return(NULL)
  }
  tryCatch(readRDS(path), error = function(error) NULL)
}

.builder_snapshot_owned_at <- function(path, snapshot) {
  if (
    !is.list(snapshot) ||
      !.builder_adapter_scalar_text(snapshot$path) ||
      !.builder_adapter_scalar_text(snapshot$owner_token) ||
      !dir.exists(path) ||
      .builder_path_is_link(path)
  ) {
    return(FALSE)
  }
  canonical <- .builder_canonical_path(path)
  snapshot_parent <- dirname(snapshot$path)
  if (!dir.exists(snapshot_parent)) {
    return(FALSE)
  }
  expected_snapshot_path <- file.path(
    .builder_canonical_path(snapshot_parent),
    basename(snapshot$path)
  )
  if (!identical(expected_snapshot_path, snapshot$path)) {
    return(FALSE)
  }
  object_file <- file.path(canonical, "object.rds")
  if (!file.exists(object_file) || .builder_path_is_link(object_file)) {
    return(FALSE)
  }
  marker_path <- .builder_snapshot_marker_path(canonical)
  if (!file.exists(marker_path) || .builder_path_is_link(marker_path)) {
    return(FALSE)
  }
  marker <- tryCatch(readRDS(marker_path), error = function(error) NULL)
  observed_md5 <- unname(tools::md5sum(object_file))
  is.list(marker) &&
    identical(marker$path, snapshot$path) &&
    identical(marker$owner_token, snapshot$owner_token) &&
    identical(marker$created_at, snapshot$created_at) &&
    identical(marker$object_md5, snapshot$object_md5) &&
    identical(snapshot$object_md5, observed_md5)
}

.builder_snapshot_owned <- function(snapshot) {
  if (!is.list(snapshot)) {
    return(FALSE)
  }
  .builder_snapshot_owned_at(snapshot$path %||% "", snapshot)
}

.builder_cleanup_after_check <- function(path) invisible(path)

.builder_isolate_owned_path <- function(path, validator) {
  if (!isTRUE(validator(path))) {
    return(FALSE)
  }
  .builder_cleanup_after_check(path)
  quarantine <- tempfile(
    paste0(".", basename(path), "-quarantine-"),
    tmpdir = dirname(path)
  )
  if (file.exists(quarantine) || dir.exists(quarantine)) {
    return(FALSE)
  }
  if (!file.rename(path, quarantine)) {
    return(FALSE)
  }
  if (!isTRUE(validator(quarantine))) {
    if (!file.exists(path) && !dir.exists(path)) {
      file.rename(quarantine, path)
    }
    return(FALSE)
  }
  unlink(quarantine, recursive = TRUE, force = TRUE)
  !file.exists(quarantine) && !dir.exists(quarantine)
}

.builder_stage_release <- function(path, owner) {
  .builder_isolate_owned_path(
    path,
    function(candidate) .builder_stage_owned_at(candidate, owner)
  )
}

## -- On-disk layer equivalence contracts -----------------------------------

.builder_bpcells_raw_node <- function(node) {
  class_name <- class(node)[[1L]]
  class_name %in%
    c("MatrixDir", "MatrixH5") &&
    "transpose" %in% methods::slotNames(node) &&
    identical(methods::slot(node, "transpose"), FALSE)
}

.builder_bpcells_layer_signature <- function(layer) {
  if (
    !identical(class(layer)[[1L]], "RenameDims") ||
      !"matrix" %in% methods::slotNames(layer) ||
      !"transpose" %in% methods::slotNames(layer) ||
      !identical(methods::slot(layer, "transpose"), FALSE)
  ) {
    return(NULL)
  }
  node <- methods::slot(layer, "matrix")
  if (.builder_bpcells_raw_node(node)) {
    return(c("RenameDims", class(node)[[1L]]))
  }
  if (
    !identical(class(node)[[1L]], "ColBindMatrices") ||
      !"matrix_list" %in% methods::slotNames(node) ||
      !"transpose" %in% methods::slotNames(node) ||
      !identical(methods::slot(node, "transpose"), FALSE)
  ) {
    return(NULL)
  }
  children <- methods::slot(node, "matrix_list")
  if (
    !length(children) ||
      !all(vapply(
        children,
        .builder_bpcells_raw_node,
        logical(1)
      ))
  ) {
    return(NULL)
  }
  c(
    "RenameDims",
    "ColBindMatrices",
    vapply(children, function(child) class(child)[[1L]], character(1))
  )
}

.builder_hdf5_layer_safe <- function(layer) {
  identical(class(layer)[[1L]], "HDF5Matrix") &&
    isTRUE(DelayedArray::isPristine(layer)) &&
    identical(DelayedArray::nseed(layer), 1L) &&
    identical(class(DelayedArray::seed(layer))[[1L]], "HDF5ArraySeed")
}

.builder_layer_sample <- function(layer) {
  dimensions <- dim(layer)
  rows <- unique(c(1L, max(1L, dimensions[[1L]] %/% 2L), dimensions[[1L]]))
  columns <- unique(c(1L, max(1L, dimensions[[2L]] %/% 2L), dimensions[[2L]]))
  as.matrix(layer[rows, columns, drop = FALSE])
}

.builder_snapshot_layer_contracts <- function(object) {
  contracts <- list()
  for (assay in names(object@assays)) {
    for (layer_name in SeuratObject::Layers(object[[assay]])) {
      layer <- suppressWarnings(SeuratObject::LayerData(
        object[[assay]],
        layer = layer_name
      ))
      signature <- NULL
      if (inherits(layer, "IterableMatrix")) {
        inputs <- tryCatch(
          BPCells::all_matrix_inputs(layer),
          error = function(error) NULL
        )
        input_classes <- if (is.null(inputs)) {
          character()
        } else {
          vapply(inputs, function(input) class(input)[[1L]], character(1))
        }
        disk_inputs <- input_classes %in% c("MatrixDir", "MatrixH5")
        memory_inputs <- grepl("MatrixMem", input_classes, fixed = TRUE)
        if (
          !length(input_classes) ||
            any(!disk_inputs & !memory_inputs) ||
            (any(disk_inputs) && !all(disk_inputs))
        ) {
          .builder_adapter_abort(paste0(
            "Layer `",
            assay,
            "/",
            layer_name,
            "` contains an unsupported ",
            "BPCells queued operation. Materialize it before snapshotting."
          ))
        }
        if (all(disk_inputs)) {
          signature <- .builder_bpcells_layer_signature(layer)
          if (is.null(signature)) {
            .builder_adapter_abort(paste0(
              "Layer `",
              assay,
              "/",
              layer_name,
              "` contains an unsupported ",
              "BPCells queued operation. Materialize it before snapshotting."
            ))
          }
        }
      } else if (inherits(layer, "DelayedMatrix")) {
        backing_paths <- tryCatch(
          DelayedArray::path(layer),
          error = function(error) NULL
        )
        backing_paths <- as.character(backing_paths %||% character())
        backing_paths <- backing_paths[nzchar(backing_paths)]
        if (length(backing_paths) && !.builder_hdf5_layer_safe(layer)) {
          .builder_adapter_abort(paste0(
            "Layer `",
            assay,
            "/",
            layer_name,
            "` contains an unsupported ",
            "HDF5Array delayed operation. Materialize it before snapshotting."
          ))
        }
        if (length(backing_paths)) {
          signature <- "HDF5Matrix"
        }
      }
      if (!is.null(signature)) {
        key <- paste(assay, layer_name, sep = "\r")
        contracts[[key]] <- list(
          assay = assay,
          layer = layer_name,
          signature = signature,
          dim = dim(layer),
          dimnames = dimnames(layer),
          sample = .builder_layer_sample(layer)
        )
      }
    }
  }
  contracts
}

.builder_snapshot_verify_layer_contracts <- function(object, contracts) {
  for (contract in contracts) {
    if (
      !contract$assay %in% names(object@assays) ||
        !contract$layer %in% SeuratObject::Layers(object[[contract$assay]])
    ) {
      .builder_adapter_abort("A reopened snapshot is missing an on-disk layer.")
    }
    layer <- suppressWarnings(SeuratObject::LayerData(
      object[[contract$assay]],
      layer = contract$layer
    ))
    signature <- if (inherits(layer, "IterableMatrix")) {
      .builder_bpcells_layer_signature(layer)
    } else if (.builder_hdf5_layer_safe(layer)) {
      "HDF5Matrix"
    } else {
      NULL
    }
    if (
      !identical(signature, contract$signature) ||
        !identical(dim(layer), contract$dim) ||
        !identical(dimnames(layer), contract$dimnames) ||
        !isTRUE(all.equal(
          .builder_layer_sample(layer),
          contract$sample,
          tolerance = 0,
          check.attributes = TRUE
        ))
    ) {
      .builder_adapter_abort(paste0(
        "A reopened snapshot changed the structure or values of layer `",
        contract$assay,
        "/",
        contract$layer,
        "`."
      ))
    }
  }
  invisible(TRUE)
}

.builder_snapshot_initial_estimate <- function(object) {
  estimate <- as.numeric(object.size(object))
  if (length(estimate) != 1L || is.na(estimate) || !is.finite(estimate)) {
    .builder_adapter_abort("Builder could not estimate the snapshot size.")
  }
  estimate
}

## -- Snapshot publication and public lifecycle -----------------------------

.builder_snapshot_seurat_impl <- function(
  object,
  snapshot_dir,
  available_bytes = NULL
) {
  if (!inherits(object, "Seurat")) {
    .builder_adapter_abort("A snapshot requires a Seurat object.")
  }
  if (!.builder_adapter_scalar_text(snapshot_dir)) {
    .builder_adapter_abort("A snapshot path must be one non-empty string.")
  }
  if (file.exists(snapshot_dir) || dir.exists(snapshot_dir)) {
    .builder_adapter_abort("The snapshot target already exists.")
  }
  parent <- dirname(snapshot_dir)
  if (!dir.exists(parent) || .builder_path_chain_has_link(parent)) {
    .builder_adapter_abort(paste0(
      "The snapshot parent must exist and contain no symbolic links."
    ))
  }
  target <- file.path(.builder_canonical_path(parent), basename(snapshot_dir))
  object <- .builder_clear_saved_cache(object)
  layer_contracts <- .builder_snapshot_layer_contracts(object)
  has_on_disk_layers <- length(layer_contracts) > 0L
  if (is.null(available_bytes)) {
    available_bytes <- .builder_snapshot_available_bytes(parent)
  }
  .builder_snapshot_check_budget(
    .builder_snapshot_initial_estimate(object),
    available_bytes
  )
  stage <- tempfile(
    paste0(".", basename(snapshot_dir), "-stage-"),
    tmpdir = parent
  )
  if (!dir.create(stage, mode = "0700")) {
    .builder_adapter_abort("Could not create the private snapshot stage.")
  }
  stage_owner <- NULL
  published <- FALSE
  success <- FALSE
  descriptor <- NULL
  on.exit(
    {
      if (dir.exists(stage) && !is.null(stage_owner)) {
        .builder_stage_release(stage, stage_owner)
      }
      if (published && !success && !is.null(descriptor)) {
        .builder_snapshot_release(descriptor)
      }
    },
    add = TRUE
  )
  stage_owner <- .builder_stage_owner(stage)

  stub_path <- file.path(stage, "object.rds")
  .builder_snapshot_save_stub(object, stub_path)
  if (has_on_disk_layers) {
    stub <- readRDS(stub_path)
    discovered <- .builder_snapshot_validate_cache(.builder_saved_cache(stub))
  } else {
    stub <- NULL
    discovered <- list(cache = NULL, members = list())
  }
  sources <- unique(unlist(discovered$members, use.names = FALSE))
  source_states <- lapply(sources, .builder_snapshot_tree_state)
  names(source_states) <- sources
  closure_bytes <- .builder_snapshot_size(stub_path)
  if (length(sources)) {
    closure_bytes <- closure_bytes +
      sum(vapply(
        sources,
        .builder_snapshot_size,
        numeric(1)
      ))
  }
  .builder_snapshot_check_budget(closure_bytes, available_bytes)

  rewritten <- setNames(character(length(sources)), sources)
  if (length(sources)) {
    backing <- file.path(stage, "backing")
    dir.create(backing, mode = "0700")
    for (index in seq_along(sources)) {
      source <- sources[[index]]
      destination_name <- paste0(
        sprintf("%04d", index),
        "-",
        basename(source)
      )
      stage_destination <- file.path(backing, destination_name)
      .builder_snapshot_copy(source, stage_destination)
      if (.builder_path_tree_has_link(stage_destination)) {
        .builder_adapter_abort(paste0(
          "An on-disk layer changed to a symbolic link while it was copied. ",
          "Keep inputs unchanged during snapshot creation and retry."
        ))
      }
      rewritten[[source]] <- file.path(target, "backing", destination_name)
    }
    .builder_snapshot_after_copy()
    for (source in sources) {
      if (
        !identical(
          source_states[[source]],
          .builder_snapshot_tree_state(source)
        )
      ) {
        .builder_adapter_abort(paste0(
          "An on-disk layer changed while it was copied. Keep inputs ",
          "unchanged during snapshot creation and retry."
        ))
      }
    }
    cache <- discovered$cache
    for (index in seq_len(nrow(cache))) {
      cache$path[[index]] <- paste(
        rewritten[discovered$members[[index]]],
        collapse = ","
      )
    }
    stub@tools[["SaveSeuratRds"]] <- cache
    saveRDS(stub, stub_path, compress = FALSE)
  } else {
    .builder_snapshot_after_copy()
  }

  created_at <- .builder_snapshot_now()
  owner_token <- .builder_snapshot_token()
  object_md5 <- unname(tools::md5sum(stub_path))
  descriptor <- list(
    path = target,
    object_file = file.path(target, "object.rds"),
    owner_token = owner_token,
    created_at = created_at,
    object_md5 = object_md5,
    closure_bytes = closure_bytes
  )
  marker <- list(
    path = target,
    owner_token = owner_token,
    created_at = created_at,
    object_md5 = object_md5
  )
  saveRDS(marker, .builder_snapshot_marker_path(stage))
  .builder_snapshot_permissions(stage)
  if (file.exists(target) || dir.exists(target)) {
    .builder_adapter_abort("The snapshot target appeared during creation.")
  }
  if (!file.rename(stage, target)) {
    .builder_adapter_abort("Could not atomically publish the Seurat snapshot.")
  }
  published <- TRUE
  .builder_snapshot_after_publish(descriptor)

  runtime_object <- if (length(sources)) {
    rewritten_cache <- .builder_snapshot_validate_cache(cache)
    .builder_snapshot_reopen_stub(stub, rewritten_cache)
  } else {
    object
  }
  .builder_snapshot_verify_layer_contracts(runtime_object, layer_contracts)
  success <- TRUE
  list(snapshot = descriptor, object = runtime_object)
}

#' Freeze a Seurat object and every live on-disk layer into one snapshot.
builder_snapshot_seurat <- function(
  object,
  snapshot_dir,
  available_bytes = NULL
) {
  .builder_snapshot_seurat_impl(
    object,
    snapshot_dir,
    available_bytes = available_bytes
  )$snapshot
}

#' Reopen one verified Builder snapshot.
builder_open_snapshot <- function(snapshot) {
  if (!.builder_snapshot_owned(snapshot)) {
    .builder_adapter_abort(
      "The Builder snapshot owner record is missing or changed."
    )
  }
  object_file <- file.path(snapshot$path, "object.rds")
  if (!file.exists(object_file) || .builder_path_is_link(object_file)) {
    .builder_adapter_abort(
      "The Builder snapshot object file is missing or unsafe."
    )
  }
  marker <- .builder_snapshot_marker(snapshot)
  observed_md5 <- unname(tools::md5sum(object_file))
  if (
    !.builder_adapter_scalar_text(marker$object_md5) ||
      !identical(marker$object_md5, snapshot$object_md5) ||
      !identical(snapshot$object_md5, observed_md5)
  ) {
    .builder_adapter_abort("The Builder snapshot integrity check failed.")
  }
  stub <- tryCatch(
    readRDS(object_file),
    error = function(error) {
      .builder_adapter_abort("The Builder snapshot object file is unreadable.")
    }
  )
  discovered <- .builder_snapshot_validate_cache(.builder_saved_cache(stub))
  members <- unlist(discovered$members, use.names = FALSE)
  if (
    length(members) &&
      !all(vapply(
        members,
        .builder_path_within,
        logical(1),
        root = snapshot$path
      ))
  ) {
    .builder_adapter_abort("A snapshot cache path escapes its private root.")
  }
  .builder_snapshot_reopen_stub(stub, discovered)
}

.builder_snapshot_release <- function(snapshot) {
  if (!is.list(snapshot)) {
    return(FALSE)
  }
  .builder_isolate_owned_path(
    snapshot$path %||% "",
    function(candidate) .builder_snapshot_owned_at(candidate, snapshot)
  )
}

#' Remove snapshots older than 24 hours when their owner record still matches.
builder_snapshot_cleanup <- function(registry, now = Sys.time()) {
  if (!is.list(registry)) {
    .builder_adapter_abort("The snapshot registry must be a list.")
  }
  keys <- names(registry)
  if (is.null(keys)) {
    keys <- as.character(seq_along(registry))
  }
  removed <- preserved <- errors <- character()
  for (index in seq_along(registry)) {
    key <- keys[[index]]
    snapshot <- registry[[index]]
    if (!is.list(snapshot) || !dir.exists(snapshot$path %||% "")) {
      next
    }
    if (!.builder_snapshot_owned(snapshot)) {
      preserved <- c(preserved, key)
      errors <- c(errors, key)
      next
    }
    marker <- .builder_snapshot_marker(snapshot)
    age <- tryCatch(
      as.numeric(difftime(now, marker$created_at, units = "secs")),
      error = function(error) NA_real_
    )
    if (is.na(age) || age < 24 * 60 * 60) {
      preserved <- c(preserved, key)
      next
    }
    if (.builder_snapshot_release(snapshot)) {
      removed <- c(removed, key)
    } else {
      preserved <- c(preserved, key)
      errors <- c(errors, key)
    }
  }
  list(removed = removed, preserved = preserved, errors = errors)
}

## -- Worker registration ----------------------------------------------------

.builder_register_adapter <- function(adapter, id, progress = NULL) {
  if (!.builder_adapter_scalar_text(id)) {
    .builder_adapter_abort("A dataset registration requires a non-empty id.")
  }
  objects <- get(".builder_objects", envir = globalenv())
  snapshots <- get(".builder_snapshots", envir = globalenv())
  snapshot_cache <- get0(
    ".builder_snapshot_cache",
    envir = globalenv(),
    inherits = FALSE
  )
  snapshot_root <- get(".builder_snapshot_root", envir = globalenv())
  cached <- if (
    is.environment(snapshot_cache) &&
      exists(adapter$fingerprint, envir = snapshot_cache, inherits = FALSE)
  ) {
    get(adapter$fingerprint, envir = snapshot_cache, inherits = FALSE)
  } else {
    NULL
  }
  if (
    is.list(cached) &&
      isTRUE(.builder_snapshot_owned(cached$snapshot)) &&
      identical(cached$snapshot$source_fingerprint, adapter$fingerprint)
  ) {
    reopened <- builder_open_snapshot(cached$snapshot)
    assign(id, reopened, envir = objects)
    assign(id, cached$snapshot, envir = snapshots)
    return(c(
      cached$result,
      list(
        snapshot = cached$snapshot,
        previous_snapshot = NULL,
        cache_hit = TRUE
      )
    ))
  }
  inspected <- .builder_adapter_inspect(adapter, progress = progress)
  if (is.function(progress)) {
    progress("preparing")
  }
  target <- tempfile(
    paste0("dataset-", gsub("[^A-Za-z0-9_-]", "-", id), "-"),
    tmpdir = snapshot_root
  )
  frozen <- .builder_snapshot_seurat_impl(inspected$object, target)
  snapshot <- frozen$snapshot
  snapshot$source_fingerprint <- adapter$fingerprint
  reopened <- frozen$object
  registered <- FALSE
  on.exit(
    {
      if (!registered) {
        .builder_snapshot_release(snapshot)
      }
    },
    add = TRUE
  )
  previous_snapshot <- if (exists(id, envir = snapshots, inherits = FALSE)) {
    get(id, envir = snapshots, inherits = FALSE)
  } else {
    NULL
  }
  previous_object <- if (exists(id, envir = objects, inherits = FALSE)) {
    get(id, envir = objects, inherits = FALSE)
  } else {
    NULL
  }
  tryCatch(
    {
      assign(id, reopened, envir = objects)
      assign(id, snapshot, envir = snapshots)
    },
    error = function(error) {
      if (is.null(previous_object)) {
        if (exists(id, envir = objects, inherits = FALSE)) {
          rm(list = id, envir = objects)
        }
      } else {
        assign(id, previous_object, envir = objects)
      }
      if (is.null(previous_snapshot)) {
        if (exists(id, envir = snapshots, inherits = FALSE)) {
          rm(list = id, envir = snapshots)
        }
      } else {
        assign(id, previous_snapshot, envir = snapshots)
      }
      stop(error)
    }
  )
  registered <- TRUE
  result <- list(
    profile = inspected$legacy_profile,
    dataset_profile = inspected$profile,
    format = inspected$format,
    levels = inspected$levels,
    source = inspected$source,
    snapshot = snapshot,
    previous_snapshot = previous_snapshot
  )
  if (is.environment(snapshot_cache)) {
    assign(
      adapter$fingerprint,
      list(
        snapshot = snapshot,
        result = result[c(
          "profile",
          "dataset_profile",
          "format",
          "levels",
          "source"
        )]
      ),
      envir = snapshot_cache
    )
  }
  result$cache_hit <- FALSE
  result
}
