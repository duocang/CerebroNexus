## -------------------------------------------------------------------------
## Private generated-App assembly contracts.
##
## This file is loaded by the Builder parent and disposable worker. Generated
## Apps never source it and never need the CerebroNexus package at runtime.
## -------------------------------------------------------------------------

.builder_app_request_fields <- c(
  "contract_version",
  "stage",
  "cerebro_data",
  "crb_identities",
  "selector_order",
  "initial_dataset",
  "initial_dataset_mode",
  "initial_page",
  "show_upload_ui",
  "welcome_message",
  "point_size",
  "viewer_content",
  "variable_to_compare",
  "host",
  "port",
  "max_request_size",
  "display_mode",
  "launch_browser",
  "colors",
  "crb_pick_smallest_file",
  "backend_plan",
  "backend_identities",
  "content_identities"
)

.builder_app_identity_fields <- c(
  "label",
  "path",
  "size",
  "modification_time",
  "change_time",
  "device_id",
  "inode",
  "hard_links",
  "permissions",
  "md5"
)

.builder_app_backend_identity_fields <- c(
  "type",
  "root",
  "root_fingerprint",
  "entries"
)
.builder_app_backend_file_entry_fields <- c("path", "type", "identity")
.builder_app_backend_directory_entry_fields <- c(
  "path",
  "type",
  "fingerprint"
)
.builder_app_fingerprint_fields <- c(
  "type",
  "size",
  "permissions",
  "device_id",
  "inode",
  "hard_links",
  "modification_time",
  "change_time"
)
.builder_app_config_max_bytes <- 16 * 1024^2
.builder_app_inert_max_objects <- 100000
.builder_app_inert_max_elements <- 100000
.builder_app_inert_max_bytes <- 16 * 1024^2

.builder_app_options_valid <- function(options, dataset_order) {
  expected <- c(
    "enabled",
    "show_upload_ui",
    "initial_dataset",
    "initial_dataset_mode",
    "initial_page",
    "welcome_message",
    "point_size",
    "variable_to_compare",
    "host",
    "port",
    "max_request_size",
    "display_mode",
    "launch_browser"
  )
  point_size <- if (is.list(options)) options$point_size else NULL
  point_value <- if (is.list(point_size)) {
    point_size$overview_projection_point_size
  } else {
    NULL
  }
  is.list(options) &&
    !is.object(options) &&
    identical(sort(names(options)), sort(expected)) &&
    isTRUE(options$enabled) &&
    is.logical(options$show_upload_ui) &&
    length(options$show_upload_ui) == 1L &&
    !is.na(options$show_upload_ui) &&
    is.character(options$initial_dataset) &&
    length(options$initial_dataset) == 1L &&
    !is.na(options$initial_dataset) &&
    options$initial_dataset %in% dataset_order &&
    is.character(options$initial_dataset_mode) &&
    length(options$initial_dataset_mode) == 1L &&
    !is.na(options$initial_dataset_mode) &&
    options$initial_dataset_mode %in% c("automatic", "explicit") &&
    (!identical(options$initial_dataset_mode, "automatic") ||
      identical(options$initial_dataset, dataset_order[[1L]])) &&
    is.character(options$initial_page) &&
    length(options$initial_page) == 1L &&
    !is.na(options$initial_page) &&
    options$initial_page %in% builder_viewer_known_page_ids() &&
    is.character(options$welcome_message) &&
    length(options$welcome_message) == 1L &&
    !is.na(options$welcome_message) &&
    nzchar(trimws(options$welcome_message)) &&
    is.list(point_size) &&
    !is.object(point_size) &&
    identical(names(point_size), "overview_projection_point_size") &&
    is.numeric(point_value) &&
    length(point_value) == 1L &&
    !is.na(point_value) &&
    is.finite(point_value) &&
    point_value >= 0 &&
    point_value <= 20 &&
    is.logical(options$variable_to_compare) &&
    length(options$variable_to_compare) == 1L &&
    !is.na(options$variable_to_compare) &&
    is.character(options$host) &&
    length(options$host) == 1L &&
    !is.na(options$host) &&
    nzchar(options$host) &&
    is.numeric(options$port) &&
    length(options$port) == 1L &&
    !is.na(options$port) &&
    is.finite(options$port) &&
    options$port == floor(options$port) &&
    options$port >= 1 &&
    options$port <= 65535 &&
    is.numeric(options$max_request_size) &&
    length(options$max_request_size) == 1L &&
    !is.na(options$max_request_size) &&
    is.finite(options$max_request_size) &&
    options$max_request_size > 0 &&
    is.character(options$display_mode) &&
    length(options$display_mode) == 1L &&
    !is.na(options$display_mode) &&
    options$display_mode %in% c("auto", "normal", "showcase") &&
    is.logical(options$launch_browser) &&
    length(options$launch_browser) == 1L &&
    !is.na(options$launch_browser)
}

.builder_app_viewer_content_valid <- function(value, selector_order) {
  if (
    !is.list(value) ||
      is.object(value) ||
      !identical(names(value), selector_order)
  ) {
    return(FALSE)
  }
  all(vapply(
    value,
    function(item) {
      if (
        !is.list(item) ||
          is.object(item) ||
          !identical(
            names(item),
            c(
              "default_projection",
              "default_trajectory",
              "overview_point_size"
            )
          )
      ) {
        return(FALSE)
      }
      projection <- item$default_projection
      trajectory <- item$default_trajectory
      point_size <- item$overview_point_size
      projection_valid <- is.null(projection) ||
        (is.character(projection) &&
          length(projection) == 1L &&
          !is.na(projection) &&
          nzchar(projection))
      trajectory_valid <- is.null(trajectory) ||
        (is.list(trajectory) &&
          !is.object(trajectory) &&
          identical(names(trajectory), c("method", "name")) &&
          all(vapply(
            trajectory,
            function(field) {
              is.character(field) &&
                length(field) == 1L &&
                !is.na(field) &&
                nzchar(field)
            },
            logical(1)
          )))
      projection_valid &&
        trajectory_valid &&
        is.numeric(point_size) &&
        length(point_size) == 1L &&
        !is.na(point_size) &&
        is.finite(point_size) &&
        point_size >= 0 &&
        point_size <= 20
    },
    logical(1)
  ))
}

.builder_app_viewer_content <- function(items, labels, fallback_point_size) {
  fallback <- fallback_point_size$overview_projection_point_size
  values <- lapply(items, function(item) {
    point_size <- item$overview_point_size
    if (
      !is.numeric(point_size) ||
        length(point_size) != 1L ||
        is.na(point_size) ||
        !is.finite(point_size) ||
        point_size < 0 ||
        point_size > 20
    ) {
      point_size <- fallback
    }
    projection <- item$default_projection
    if (
      !is.character(projection) ||
        length(projection) != 1L ||
        is.na(projection) ||
        !nzchar(projection)
    ) {
      projection <- NULL
    }
    trajectory <- item$default_trajectory
    if (
      !is.list(trajectory) ||
        is.object(trajectory) ||
        !identical(names(trajectory), c("method", "name")) ||
        any(vapply(
          trajectory,
          function(field) {
            !is.character(field) ||
              length(field) != 1L ||
              is.na(field) ||
              !nzchar(field)
          },
          logical(1)
        ))
    ) {
      trajectory <- NULL
    }
    list(
      default_projection = projection,
      default_trajectory = trajectory,
      overview_point_size = as.double(point_size)
    )
  })
  names(values) <- labels
  if (!.builder_app_viewer_content_valid(values, labels)) {
    stop("Frozen Viewer-content defaults are invalid.", call. = FALSE)
  }
  values
}

.builder_app_demo_data <- c(
  "extdata/examples/demo_full_tcr_bcr.crb" = "1cba0b06e2fa6d3753fa106d07a54c06",
  "extdata/examples/demo_hla_tcr_dextramer.crb" = "33f485ee36c28556e79c273a8b39f03e",
  "extdata/examples/demo_spatial_merfish.crb" = "f79806e5df9e746e96609b3ede3e7d38",
  "extdata/examples/demo_spatial_slideseq.crb" = "ec2c6e97cfec73857f3b36e022954ad6",
  "extdata/examples/demo_spatial_visium.crb" = "37ea7dd49363c341efa755346b6c0e80",
  "extdata/examples/demo_spatial_xenium.crb" = "8ed897dbf1b8be9aac099181ac6b5171",
  "extdata/examples/demo_spatial.crb" = "543f1b3323d199f31de854b95807c21a",
  "extdata/examples/demo_trekker.crb" = "081f8b377425cceee154684fc1bf5dea",
  "extdata/examples/example.crb" = "abe7e1c9102569cf9374460010686a22",
  "extdata/examples/example.h5" = "42ea78375ebdf742db55baa6ba12aabf",
  "extdata/examples/pbmc_SCE.rds" = "7b388677c44186cc8a6c13036065e1cb",
  "extdata/examples/pbmc_seurat.rds" = "7c0515903aa08f9aead17f190e4d328e"
)

.builder_app_value_bytes <- function(value, kind = typeof(value)) {
  length_value <- length(value)
  switch(
    kind,
    logical = 4 * length_value,
    integer = 4 * length_value,
    double = 8 * length_value,
    complex = 16 * length_value,
    raw = length_value,
    character = {
      sizes <- nchar(value, type = "bytes", keepNA = FALSE)
      8 * length_value + sum(sizes, na.rm = TRUE)
    },
    list = 8 * length_value,
    0
  )
}

.builder_app_has_reference <- function(value, depth = 0L, budget = NULL) {
  kind <- typeof(value)
  if (
    base::isS4(value) ||
      inherits(value, "connection") ||
      kind %in%
        c(
          "environment",
          "externalptr",
          "weakref",
          "closure",
          "builtin",
          "special",
          "language",
          "symbol",
          "expression",
          "pairlist"
        )
  ) {
    return(TRUE)
  }
  class_value <- attr(value, "class", exact = TRUE)
  allowed_classes <- list(
    c("builder_build_plan", "list"),
    c("builder_app_bundle_request", "list"),
    c("builder_app_verification", "list"),
    c("builder_asset_claim", "list"),
    c("builder_manifest_entry", "list"),
    c("builder_content_manifest", "list"),
    c("POSIXct", "POSIXt")
  )
  if (
    !is.null(class_value) &&
      !any(vapply(
        allowed_classes,
        identical,
        logical(1),
        class_value
      ))
  ) {
    return(TRUE)
  }
  if (is.null(budget)) {
    budget <- new.env(parent = emptyenv())
    budget$objects <- 0
    budget$elements <- 0
    budget$bytes <- 0
  }
  plain <- if (is.object(value)) {
    tryCatch(unclass(value), error = function(error) NULL)
  } else {
    value
  }
  if (is.null(plain) && !is.null(value)) {
    return(TRUE)
  }
  value_length <- length(plain)
  budget$objects <- budget$objects + 1
  budget$elements <- budget$elements + max(1, value_length)
  if (
    depth > 64L ||
      budget$objects > .builder_app_inert_max_objects ||
      budget$elements > .builder_app_inert_max_elements
  ) {
    return(TRUE)
  }
  budget$bytes <- budget$bytes + .builder_app_value_bytes(plain, kind)
  if (budget$bytes > .builder_app_inert_max_bytes) {
    return(TRUE)
  }
  attributes_value <- attributes(value)
  if (
    !is.null(attributes_value) &&
      any(vapply(
        unname(attributes_value),
        .builder_app_has_reference,
        logical(1),
        depth = depth + 1L,
        budget = budget
      ))
  ) {
    return(TRUE)
  }
  if (is.list(plain)) {
    return(any(vapply(
      plain,
      .builder_app_has_reference,
      logical(1),
      depth = depth + 1L,
      budget = budget
    )))
  }
  FALSE
}

.builder_app_plain_value <- function(value, depth = 0L) {
  if (depth > 64L) {
    stop("An inert App value is too deep to normalize.", call. = FALSE)
  }
  plain <- value
  attr(plain, "class") <- NULL
  if (is.list(plain)) {
    for (index in seq_along(plain)) {
      plain[index] <- list(
        .builder_app_plain_value(
          plain[[index]],
          depth = depth + 1L
        )
      )
    }
  }
  plain
}

.builder_app_assert_readable_file <- function(
  path,
  .open_file = base::file
) {
  mode <- file.info(path)$mode
  if (
    !identical(.Platform$OS.type, "windows") &&
      (length(mode) != 1L ||
        is.na(mode) ||
        bitwAnd(as.integer(mode), 292L) == 0L)
  ) {
    stop("A staged App regular file is not readable.", call. = FALSE)
  }
  connection <- tryCatch(
    .open_file(path, open = "rb"),
    error = function(error) NULL
  )
  if (is.null(connection) || !inherits(connection, "connection")) {
    stop("A staged App regular file is not readable.", call. = FALSE)
  }
  on.exit(try(close(connection), silent = TRUE), add = TRUE)
  readable <- tryCatch(
    {
      readBin(connection, what = "raw", n = 1L)
      TRUE
    },
    error = function(error) FALSE
  )
  if (!readable) {
    stop("A staged App regular file is not readable.", call. = FALSE)
  }
  invisible(TRUE)
}

.builder_app_regular_file_identity <- function(
  path,
  label = NULL,
  .digest_file = tools::md5sum
) {
  before <- tryCatch(
    fs::file_info(path, fail = TRUE, follow = FALSE),
    error = function(error) NULL
  )
  if (
    is.null(before) ||
      !identical(as.character(before$type), "file") ||
      .builder_app_is_link(path)
  ) {
    stop("A staged App regular file is missing or invalid.", call. = FALSE)
  }
  before_fingerprint <- .builder_app_file_fingerprint(before)
  if (!identical(before_fingerprint$hard_links, 1)) {
    stop("A staged App regular file cannot be a hard link.", call. = FALSE)
  }
  .builder_app_assert_readable_file(path)
  digest <- tryCatch(
    unname(as.character(.digest_file(path))),
    error = function(error) NA_character_
  )
  after <- tryCatch(
    fs::file_info(path, fail = TRUE, follow = FALSE),
    error = function(error) NULL
  )
  if (
    length(digest) != 1L ||
      is.na(digest) ||
      !grepl("^[[:xdigit:]]{32}$", digest) ||
      is.null(after) ||
      !identical(
        before_fingerprint,
        .builder_app_file_fingerprint(after)
      )
  ) {
    stop("A staged App regular file changed while it was read.", call. = FALSE)
  }
  canonical <- tryCatch(
    normalizePath(path, winslash = "/", mustWork = TRUE),
    error = function(error) NULL
  )
  if (is.null(canonical)) {
    stop("A staged App regular file has no stable path.", call. = FALSE)
  }
  list(
    label = label,
    path = canonical,
    size = before_fingerprint$size,
    modification_time = before_fingerprint$modification_time,
    change_time = before_fingerprint$change_time,
    device_id = before_fingerprint$device_id,
    inode = before_fingerprint$inode,
    hard_links = before_fingerprint$hard_links,
    permissions = before_fingerprint$permissions,
    md5 = tolower(digest)
  )
}

.builder_app_capture_file_identity <- function(
  path,
  label = NULL,
  .digest_file = tools::md5sum
) {
  .builder_app_regular_file_identity(path, label, .digest_file)
}

.builder_app_request_error <- function() {
  stop("The generated-App request contract is invalid.", call. = FALSE)
}

.builder_app_permissions_valid <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    grepl(
      "^[r-][w-][xsS-][r-][w-][xsS-][r-][w-][xtT-]$",
      value
    )
}

.builder_app_identity_valid <- function(identity, label, path) {
  is.list(identity) &&
    !is.object(identity) &&
    identical(names(identity), .builder_app_identity_fields) &&
    identical(identity$label, label) &&
    identical(identity$path, path) &&
    is.double(identity$size) &&
    length(identity$size) == 1L &&
    is.finite(identity$size) &&
    identity$size >= 0 &&
    identity$size == floor(identity$size) &&
    all(vapply(
      identity[c("modification_time", "change_time")],
      function(value) {
        is.double(value) &&
          length(value) == 1L &&
          is.finite(value)
      },
      logical(1)
    )) &&
    all(vapply(
      identity[c("device_id", "inode")],
      function(value) {
        is.double(value) &&
          length(value) == 1L &&
          is.finite(value) &&
          value >= 0 &&
          value == floor(value)
      },
      logical(1)
    )) &&
    is.double(identity$hard_links) &&
    length(identity$hard_links) == 1L &&
    is.finite(identity$hard_links) &&
    identical(identity$hard_links, 1) &&
    .builder_app_permissions_valid(identity$permissions) &&
    is.character(identity$md5) &&
    length(identity$md5) == 1L &&
    !is.na(identity$md5) &&
    grepl("^[0-9a-f]{32}$", identity$md5)
}

.builder_app_portable_file <- function(path, size, md5) {
  list(path = path, type = "file", size = size, md5 = md5)
}

.builder_app_content_identities <- function(
  crb_identities,
  backend_identities,
  backend_plan
) {
  relative_crbs <- names(backend_plan$entries)
  identities <- lapply(seq_along(relative_crbs), function(index) {
    relative_crb <- relative_crbs[[index]]
    closure <- backend_identities[[relative_crb]]
    backend <- if (identical(closure$type, "embedded")) {
      list(type = "embedded", root = NULL, entries = list())
    } else {
      entries <- lapply(closure$entries, function(entry) {
        path <- paste0("private-data/", entry$path)
        if (identical(entry$type, "directory")) {
          return(list(path = path, type = "directory"))
        }
        .builder_app_portable_file(
          path,
          entry$identity$size,
          entry$identity$md5
        )
      })
      names(entries) <- if (length(entries)) {
        paste0("private-data/", names(closure$entries))
      } else {
        character()
      }
      list(
        type = closure$type,
        root = paste0(
          "private-data/",
          backend_plan$entries[[relative_crb]]$location
        ),
        entries = entries
      )
    }
    list(
      crb = .builder_app_portable_file(
        relative_crb,
        crb_identities[[index]]$size,
        crb_identities[[index]]$md5
      ),
      backend = backend
    )
  })
  names(identities) <- relative_crbs
  identities
}

.builder_app_colors_valid <- function(colors, selector_order) {
  if (
    !is.list(colors) ||
      is.object(colors) ||
      !identical(names(colors), selector_order)
  ) {
    return(FALSE)
  }
  all(vapply(
    colors,
    function(palettes) {
      if (!is.list(palettes) || is.object(palettes)) {
        return(FALSE)
      }
      palette_names <- names(palettes)
      if (
        !length(palettes) ||
          is.null(palette_names) ||
          anyNA(palette_names) ||
          any(!nzchar(palette_names)) ||
          anyDuplicated(palette_names)
      ) {
        return(FALSE)
      }
      all(vapply(
        palettes,
        function(value) {
          level_names <- names(value)
          is.character(value) &&
            !anyNA(value) &&
            !is.null(level_names) &&
            length(level_names) == length(value) &&
            !anyNA(level_names) &&
            all(nzchar(level_names)) &&
            !anyDuplicated(level_names) &&
            isTRUE(tryCatch(
              {
                grDevices::col2rgb(value)
                TRUE
              },
              error = function(error) FALSE
            ))
        },
        logical(1)
      ))
    },
    logical(1)
  ))
}

.builder_app_backend_plan_valid <- function(plan, cerebro_data) {
  if (
    !is.list(plan) ||
      is.object(plan) ||
      !identical(names(plan), c("schema_version", "entries")) ||
      !identical(plan$schema_version, 1L) ||
      !is.list(plan$entries) ||
      is.object(plan$entries) ||
      !identical(
        names(plan$entries),
        file.path("private-data", basename(cerebro_data))
      )
  ) {
    return(FALSE)
  }
  all(vapply(
    seq_along(plan$entries),
    function(index) {
      entry <- plan$entries[[index]]
      if (
        !is.list(entry) ||
          is.object(entry) ||
          !identical(names(entry), c("type", "mode", "location")) ||
          !is.character(entry$type) ||
          length(entry$type) != 1L ||
          is.na(entry$type) ||
          !entry$type %in% c("embedded", "h5", "bpcells") ||
          !is.character(entry$mode) ||
          length(entry$mode) != 1L ||
          is.na(entry$mode)
      ) {
        return(FALSE)
      }
      if (identical(entry$type, "embedded")) {
        identical(entry$mode, "embedded") && is.null(entry$location)
      } else {
        expected_location <- paste0(
          tools::file_path_sans_ext(basename(cerebro_data[[index]])),
          if (identical(entry$type, "h5")) ".h5" else ".bpcells"
        )
        identical(entry$mode, "bundled") &&
          identical(entry$location, expected_location)
      }
    },
    logical(1)
  ))
}

.builder_app_capture_backend_identity <- function(
  entry,
  crb_path,
  .tree_identity = .builder_app_tree_identity
) {
  if (identical(entry$type, "embedded")) {
    return(list(
      type = "embedded",
      root = NULL,
      root_fingerprint = NULL,
      entries = list()
    ))
  }
  sidecar <- file.path(dirname(crb_path), entry$location)
  expected_type <- if (identical(entry$type, "h5")) "file" else "directory"
  sidecar_info <- tryCatch(
    fs::file_info(sidecar, fail = TRUE, follow = FALSE),
    error = function(error) NULL
  )
  if (
    is.null(sidecar_info) ||
      !identical(as.character(sidecar_info$type), expected_type) ||
      .builder_app_is_link(sidecar)
  ) {
    stop(
      "A verified input backend closure is missing or invalid.",
      call. = FALSE
    )
  }
  root <- tryCatch(
    normalizePath(sidecar, winslash = "/", mustWork = TRUE),
    error = function(error) NULL
  )
  if (
    is.null(root) ||
      !identical(dirname(root), dirname(crb_path)) ||
      !identical(basename(root), entry$location)
  ) {
    stop("A verified input backend closure has an invalid path.", call. = FALSE)
  }
  if (identical(entry$type, "h5")) {
    identity <- .builder_app_capture_file_identity(
      root,
      label = entry$location
    )
    entries <- stats::setNames(
      list(list(path = entry$location, type = "file", identity = identity)),
      entry$location
    )
    return(list(
      type = "h5",
      root = root,
      root_fingerprint = NULL,
      entries = entries
    ))
  }

  tree <- .tree_identity(root)
  entries <- lapply(names(tree$entries), function(relative) {
    value <- tree$entries[[relative]]
    mapping <- file.path(entry$location, relative)
    if (identical(value$type, "directory")) {
      return(list(
        path = mapping,
        type = "directory",
        fingerprint = value$fingerprint
      ))
    }
    identity <- list(
      label = mapping,
      path = file.path(root, relative),
      size = value$size,
      modification_time = value$modification_time,
      change_time = value$change_time,
      device_id = value$device_id,
      inode = value$inode,
      hard_links = value$hard_links,
      permissions = value$permissions,
      md5 = value$md5
    )
    list(path = mapping, type = "file", identity = identity)
  })
  names(entries) <- file.path(entry$location, names(tree$entries))
  list(
    type = "bpcells",
    root = root,
    root_fingerprint = tree$root_fingerprint,
    entries = entries
  )
}

.builder_app_fingerprint_valid <- function(fingerprint, expected_type) {
  if (
    !is.list(fingerprint) ||
      is.object(fingerprint) ||
      !identical(names(fingerprint), .builder_app_fingerprint_fields) ||
      !identical(fingerprint$type, expected_type) ||
      !.builder_app_permissions_valid(fingerprint$permissions)
  ) {
    return(FALSE)
  }
  nonnegative_integer <- function(value) {
    is.double(value) &&
      length(value) == 1L &&
      is.finite(value) &&
      value >= 0 &&
      value == floor(value)
  }
  finite_time <- function(value) {
    is.double(value) && length(value) == 1L && is.finite(value)
  }
  all(vapply(
    fingerprint[c("size", "device_id", "inode")],
    nonnegative_integer,
    logical(1)
  )) &&
    nonnegative_integer(fingerprint$hard_links) &&
    fingerprint$hard_links >= 1 &&
    all(vapply(
      fingerprint[c("modification_time", "change_time")],
      finite_time,
      logical(1)
    ))
}

.builder_app_backend_identities_valid <- function(
  identities,
  plan,
  cerebro_data
) {
  if (
    !is.list(identities) ||
      is.object(identities) ||
      !identical(names(identities), names(plan$entries))
  ) {
    return(FALSE)
  }
  all(vapply(
    seq_along(identities),
    function(index) {
      closure <- identities[[index]]
      entry <- plan$entries[[index]]
      if (
        !is.list(closure) ||
          is.object(closure) ||
          !identical(names(closure), .builder_app_backend_identity_fields) ||
          !identical(closure$type, entry$type) ||
          !is.list(closure$entries) ||
          is.object(closure$entries)
      ) {
        return(FALSE)
      }
      if (identical(entry$type, "embedded")) {
        return(
          is.null(closure$root) &&
            is.null(closure$root_fingerprint) &&
            !length(closure$entries)
        )
      }
      expected_root <- file.path(dirname(cerebro_data[[index]]), entry$location)
      mappings <- names(closure$entries)
      mappings_invalid <- length(closure$entries) &&
        (is.null(mappings) ||
          anyNA(mappings) ||
          any(!nzchar(mappings)) ||
          anyDuplicated(mappings) ||
          !identical(mappings, sort(mappings, method = "radix")))
      h5_invalid <- identical(entry$type, "h5") &&
        (!is.null(closure$root_fingerprint) ||
          !identical(mappings, entry$location))
      bpcells_invalid <- identical(entry$type, "bpcells") &&
        (!.builder_app_fingerprint_valid(
          closure$root_fingerprint,
          "directory"
        ) ||
          (length(mappings) &&
            !all(startsWith(mappings, paste0(entry$location, "/")))))
      if (
        !identical(closure$root, expected_root) ||
          mappings_invalid ||
          h5_invalid ||
          bpcells_invalid
      ) {
        return(FALSE)
      }
      all(vapply(
        seq_along(closure$entries),
        function(file_index) {
          mapping <- mappings[[file_index]]
          relative <- if (identical(entry$type, "h5")) {
            entry$location
          } else {
            substring(mapping, nchar(entry$location) + 2L)
          }
          tree_entry <- closure$entries[[file_index]]
          if (
            !.builder_app_safe_relative(relative) ||
              !is.list(tree_entry) ||
              is.object(tree_entry) ||
              !identical(tree_entry$path, mapping)
          ) {
            return(FALSE)
          }
          if (identical(tree_entry$type, "file")) {
            identical(
              names(tree_entry),
              .builder_app_backend_file_entry_fields
            ) &&
              .builder_app_identity_valid(
                tree_entry$identity,
                mapping,
                file.path(dirname(cerebro_data[[index]]), mapping)
              )
          } else if (identical(tree_entry$type, "directory")) {
            identical(entry$type, "bpcells") &&
              identical(
                names(tree_entry),
                .builder_app_backend_directory_entry_fields
              ) &&
              .builder_app_fingerprint_valid(
                tree_entry$fingerprint,
                "directory"
              )
          } else {
            FALSE
          }
        },
        logical(1)
      ))
    },
    logical(1)
  ))
}

.builder_app_validate_request <- function(request) {
  if (
    !identical(typeof(request), "list") ||
      !identical(
        attr(request, "class", exact = TRUE),
        c("builder_app_bundle_request", "list")
      ) ||
      .builder_app_has_reference(request)
  ) {
    .builder_app_request_error()
  }
  plain <- .builder_app_plain_value(request)
  canonical_stage <- tryCatch(
    normalizePath(plain$stage, winslash = "/", mustWork = TRUE),
    error = function(error) NULL
  )
  canonical_data <- tryCatch(
    vapply(
      plain$cerebro_data,
      normalizePath,
      character(1),
      winslash = "/",
      mustWork = TRUE
    ),
    error = function(error) NULL
  )
  if (
    !identical(names(plain), .builder_app_request_fields) ||
      !identical(plain$contract_version, 1L) ||
      !is.character(plain$stage) ||
      length(plain$stage) != 1L ||
      is.na(plain$stage) ||
      !nzchar(plain$stage) ||
      is.null(canonical_stage) ||
      !identical(canonical_stage, plain$stage) ||
      !dir.exists(canonical_stage) ||
      .builder_app_is_link(canonical_stage) ||
      !is.character(plain$cerebro_data) ||
      !length(plain$cerebro_data) ||
      anyNA(plain$cerebro_data) ||
      any(!nzchar(plain$cerebro_data)) ||
      anyDuplicated(plain$cerebro_data) ||
      is.null(canonical_data) ||
      !identical(unname(canonical_data), unname(plain$cerebro_data)) ||
      !all(dirname(canonical_data) == canonical_stage) ||
      anyDuplicated(basename(canonical_data)) ||
      !is.character(plain$selector_order) ||
      !length(plain$selector_order) ||
      anyNA(plain$selector_order) ||
      any(!nzchar(plain$selector_order)) ||
      anyDuplicated(plain$selector_order) ||
      !identical(names(plain$cerebro_data), plain$selector_order) ||
      !is.list(plain$crb_identities) ||
      is.object(plain$crb_identities) ||
      !identical(names(plain$crb_identities), plain$selector_order) ||
      length(plain$crb_identities) != length(plain$cerebro_data) ||
      !is.character(plain$initial_dataset) ||
      length(plain$initial_dataset) != 1L ||
      is.na(plain$initial_dataset) ||
      !plain$initial_dataset %in% plain$selector_order ||
      !is.character(plain$initial_dataset_mode) ||
      length(plain$initial_dataset_mode) != 1L ||
      is.na(plain$initial_dataset_mode) ||
      !plain$initial_dataset_mode %in% c("automatic", "explicit") ||
      (identical(plain$initial_dataset_mode, "automatic") &&
        !identical(
          plain$initial_dataset,
          plain$selector_order[[1L]]
        )) ||
      !is.character(plain$initial_page) ||
      length(plain$initial_page) != 1L ||
      is.na(plain$initial_page) ||
      !plain$initial_page %in% builder_viewer_known_page_ids() ||
      !is.logical(plain$show_upload_ui) ||
      length(plain$show_upload_ui) != 1L ||
      is.na(plain$show_upload_ui) ||
      !is.character(plain$welcome_message) ||
      length(plain$welcome_message) != 1L ||
      is.na(plain$welcome_message) ||
      !nzchar(trimws(plain$welcome_message)) ||
      !is.list(plain$point_size) ||
      !identical(names(plain$point_size), "overview_projection_point_size") ||
      !is.numeric(plain$point_size$overview_projection_point_size) ||
      length(plain$point_size$overview_projection_point_size) != 1L ||
      is.na(plain$point_size$overview_projection_point_size) ||
      !is.finite(plain$point_size$overview_projection_point_size) ||
      plain$point_size$overview_projection_point_size < 0 ||
      plain$point_size$overview_projection_point_size > 20 ||
      !.builder_app_viewer_content_valid(
        plain$viewer_content,
        plain$selector_order
      ) ||
      !is.logical(plain$variable_to_compare) ||
      length(plain$variable_to_compare) != 1L ||
      is.na(plain$variable_to_compare) ||
      !is.character(plain$host) ||
      length(plain$host) != 1L ||
      is.na(plain$host) ||
      !nzchar(plain$host) ||
      !is.numeric(plain$port) ||
      length(plain$port) != 1L ||
      is.na(plain$port) ||
      !is.finite(plain$port) ||
      plain$port != floor(plain$port) ||
      plain$port < 1 ||
      plain$port > 65535 ||
      !is.numeric(plain$max_request_size) ||
      length(plain$max_request_size) != 1L ||
      is.na(plain$max_request_size) ||
      !is.finite(plain$max_request_size) ||
      plain$max_request_size <= 0 ||
      !is.character(plain$display_mode) ||
      length(plain$display_mode) != 1L ||
      is.na(plain$display_mode) ||
      !plain$display_mode %in% c("auto", "normal", "showcase") ||
      !is.logical(plain$launch_browser) ||
      length(plain$launch_browser) != 1L ||
      is.na(plain$launch_browser) ||
      !.builder_app_colors_valid(plain$colors, plain$selector_order) ||
      !identical(plain$crb_pick_smallest_file, FALSE) ||
      !.builder_app_backend_plan_valid(
        plain$backend_plan,
        plain$cerebro_data
      ) ||
      !.builder_app_backend_identities_valid(
        plain$backend_identities,
        plain$backend_plan,
        plain$cerebro_data
      )
  ) {
    .builder_app_request_error()
  }
  identities_valid <- vapply(
    seq_along(plain$crb_identities),
    function(index) {
      .builder_app_identity_valid(
        plain$crb_identities[[index]],
        plain$selector_order[[index]],
        plain$cerebro_data[[index]]
      )
    },
    logical(1)
  )
  if (!all(identities_valid)) {
    .builder_app_request_error()
  }
  expected_content <- tryCatch(
    .builder_app_content_identities(
      plain$crb_identities,
      plain$backend_identities,
      plain$backend_plan
    ),
    error = function(error) NULL
  )
  if (
    is.null(expected_content) ||
      !identical(plain$content_identities, expected_content)
  ) {
    .builder_app_request_error()
  }
  plain
}

.builder_app_assert_crb_identities <- function(request) {
  current <- lapply(
    seq_along(request$cerebro_data),
    function(index) {
      .builder_app_capture_file_identity(
        request$cerebro_data[[index]],
        request$selector_order[[index]]
      )
    }
  )
  names(current) <- request$selector_order
  if (!identical(current, request$crb_identities)) {
    stop("A verified input CRB changed after request creation.", call. = FALSE)
  }
  invisible(TRUE)
}

.builder_app_capture_backend_identities <- function(request) {
  current <- lapply(seq_along(request$cerebro_data), function(index) {
    relative_crb <- names(request$backend_plan$entries)[[index]]
    .builder_app_capture_backend_identity(
      request$backend_plan$entries[[relative_crb]],
      request$cerebro_data[[index]]
    )
  })
  names(current) <- names(request$backend_plan$entries)
  current
}

.builder_app_assert_backend_identities <- function(request) {
  current <- tryCatch(
    .builder_app_capture_backend_identities(request),
    error = function(error) NULL
  )
  if (is.null(current) || !identical(current, request$backend_identities)) {
    stop(
      "A verified input backend closure changed after request creation.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.builder_app_assert_input_identities <- function(request) {
  .builder_app_assert_crb_identities(request)
  .builder_app_assert_backend_identities(request)
  invisible(TRUE)
}

.builder_app_backend_entry <- function(item) {
  mode <- item$expression_backend
  if (
    !is.character(mode) ||
      length(mode) != 1L ||
      is.na(mode) ||
      !mode %in% c("embedded", "h5", "bpcells")
  ) {
    stop("BuildPlan contains an invalid expression backend.", call. = FALSE)
  }
  if (identical(mode, "embedded")) {
    return(list(type = "embedded", mode = "embedded", location = NULL))
  }
  sidecars <- item$sidecars
  if (
    !is.character(sidecars) ||
      length(sidecars) != 1L ||
      is.na(sidecars) ||
      !nzchar(sidecars)
  ) {
    stop("BuildPlan contains an invalid backend sidecar.", call. = FALSE)
  }
  list(type = mode, mode = "bundled", location = sidecars)
}

.builder_app_plan_contract <- function(plan, context = "App assembly") {
  invalid <- function() {
    stop(
      context,
      paste0(
        " requires an inert, reference-free frozen contract-v1 ",
        "BuildPlan."
      ),
      call. = FALSE
    )
  }
  if (
    !identical(typeof(plan), "list") ||
      !identical(
        attr(plan, "class", exact = TRUE),
        c("builder_build_plan", "list")
      )
  ) {
    invalid()
  }
  raw_items <- .subset2(plan, "items")
  if (
    !is.list(raw_items) ||
      any(vapply(
        raw_items,
        function(item) {
          !is.list(item) || !is.null(attr(item, "class", exact = TRUE))
        },
        logical(1)
      ))
  ) {
    invalid()
  }
  app_items <- lapply(raw_items, function(item) {
    list(
      id = .subset2(item, "id"),
      name = .subset2(item, "name"),
      filename = .subset2(item, "filename"),
      colors = .subset2(item, "colors"),
      default_projection = .subset2(item, "default_projection"),
      default_trajectory = .subset2(item, "default_trajectory"),
      overview_point_size = .subset2(item, "overview_point_size"),
      expression_backend = .subset2(item, "expression_backend"),
      sidecars = .subset2(item, "sidecars")
    )
  })
  plan <- list(
    app_contract_version = .subset2(plan, "app_contract_version"),
    make_app = .subset2(plan, "make_app"),
    dataset_order = .subset2(plan, "dataset_order"),
    items = app_items,
    app_options = .subset2(plan, "app_options")
  )
  if (.builder_app_has_reference(plan)) {
    invalid()
  }
  .builder_app_plain_value(plan)
}

builder_app_bundle_request <- function(plan, built, labels) {
  plan <- .builder_app_plan_contract(plan)
  if (
    !identical(plan[["app_contract_version"]], 1L) ||
      !isTRUE(plan[["make_app"]])
  ) {
    stop(
      "App assembly requires a contract-v1 BuildPlan with App output enabled.",
      call. = FALSE
    )
  }
  order <- plan$dataset_order
  items <- plan$items
  if (
    !is.character(order) ||
      !length(order) ||
      anyNA(order) ||
      any(!nzchar(order)) ||
      anyDuplicated(order) ||
      !is.list(items) ||
      length(items) != length(order)
  ) {
    stop("BuildPlan dataset order is invalid.", call. = FALSE)
  }
  if (
    .builder_app_has_reference(built) ||
      .builder_app_has_reference(labels)
  ) {
    stop("Verified CRB labels must be inert values.", call. = FALSE)
  }
  built <- .builder_app_plain_value(built)
  labels <- .builder_app_plain_value(labels)
  item_ids <- vapply(items, `[[`, character(1), "id")
  item_labels <- vapply(items, `[[`, character(1), "name")
  filenames <- vapply(items, `[[`, character(1), "filename")
  if (
    !identical(item_ids, order) ||
      anyDuplicated(item_labels) ||
      !is.character(labels) ||
      !identical(unname(labels), item_labels) ||
      !is.character(built) ||
      length(built) != length(items) ||
      !identical(names(built), item_labels) ||
      anyDuplicated(built)
  ) {
    stop("Verified CRB labels do not match BuildPlan.", call. = FALSE)
  }
  valid_files <- vapply(
    built,
    function(path) {
      length(path) == 1L &&
        !is.na(path) &&
        nzchar(path) &&
        file.exists(path) &&
        !dir.exists(path) &&
        !.builder_app_is_link(path)
    },
    logical(1)
  )
  if (!all(valid_files)) {
    stop("A verified staged CRB is missing or invalid.", call. = FALSE)
  }
  resolved <- vapply(
    built,
    normalizePath,
    character(1),
    winslash = "/",
    mustWork = TRUE
  )
  stage <- dirname(resolved[[1L]])
  if (
    !all(dirname(resolved) == stage) ||
      !identical(unname(basename(resolved)), filenames)
  ) {
    stop(
      "Verified CRBs must be exact files inside one assigned stage.",
      call. = FALSE
    )
  }
  options <- plan$app_options
  if (
    is.list(options) &&
      identical(options$initial_dataset_mode, "automatic") &&
      !identical(options$initial_dataset, order[[1L]])
  ) {
    stop(
      "An automatic initial dataset must be the first ordered dataset.",
      call. = FALSE
    )
  }
  if (
    .builder_app_has_reference(options) ||
      !.builder_app_options_valid(options, order)
  ) {
    stop("Frozen generated-App options are invalid.", call. = FALSE)
  }
  if (
    identical(options$initial_dataset_mode, "automatic") &&
      !identical(options$initial_dataset, order[[1L]])
  ) {
    stop(
      "An automatic initial dataset must be the first ordered dataset.",
      call. = FALSE
    )
  }
  initial_index <- match(options$initial_dataset, order)
  colors <- lapply(items, `[[`, "colors")
  names(colors) <- item_labels
  viewer_content <- .builder_app_viewer_content(
    items,
    item_labels,
    options$point_size
  )
  backend_entries <- lapply(items, .builder_app_backend_entry)
  names(backend_entries) <- paste0("private-data/", filenames)
  crb_identities <- lapply(
    seq_along(resolved),
    function(index) {
      .builder_app_capture_file_identity(
        resolved[[index]],
        item_labels[[index]]
      )
    }
  )
  names(crb_identities) <- item_labels
  backend_plan <- list(schema_version = 1L, entries = backend_entries)
  backend_identities <- lapply(seq_along(resolved), function(index) {
    relative_crb <- names(backend_entries)[[index]]
    .builder_app_capture_backend_identity(
      backend_entries[[relative_crb]],
      resolved[[index]]
    )
  })
  names(backend_identities) <- names(backend_entries)
  content_identities <- .builder_app_content_identities(
    crb_identities,
    backend_identities,
    backend_plan
  )

  request <- structure(
    list(
      contract_version = 1L,
      stage = stage,
      cerebro_data = stats::setNames(unname(resolved), item_labels),
      crb_identities = crb_identities,
      selector_order = item_labels,
      initial_dataset = item_labels[[initial_index]],
      initial_dataset_mode = options$initial_dataset_mode,
      initial_page = options$initial_page,
      show_upload_ui = options$show_upload_ui,
      welcome_message = options$welcome_message,
      point_size = options$point_size,
      viewer_content = viewer_content,
      variable_to_compare = options$variable_to_compare,
      host = options$host,
      port = as.integer(options$port),
      max_request_size = options$max_request_size,
      display_mode = options$display_mode,
      launch_browser = options$launch_browser,
      colors = colors,
      crb_pick_smallest_file = FALSE,
      backend_plan = backend_plan,
      backend_identities = backend_identities,
      content_identities = content_identities
    ),
    class = c("builder_app_bundle_request", "list")
  )
  .builder_app_validate_request(request)
  request
}

.builder_app_path_exists <- function(path) {
  file.exists(path) || dir.exists(path) || .builder_app_is_link(path)
}

.builder_app_is_link <- function(path) {
  linked <- tryCatch(fs::is_link(path), error = function(error) NA)
  length(linked) != 1L || is.na(linked) || isTRUE(unname(linked))
}

.builder_app_path_within <- function(path, root, must_exist = TRUE) {
  resolved <- tryCatch(
    normalizePath(path, winslash = "/", mustWork = must_exist),
    error = function(error) NULL
  )
  if (is.null(resolved)) {
    return(FALSE)
  }
  identical(resolved, root) || startsWith(resolved, paste0(root, "/"))
}

.builder_app_file_fingerprint <- function(info) {
  if (is.null(info) || nrow(info) != 1L) {
    stop("A staged App entry could not be fingerprinted safely.", call. = FALSE)
  }
  fingerprint <- list(
    type = as.character(info$type),
    size = as.double(info$size),
    permissions = as.character(info$permissions),
    device_id = as.double(info$device_id),
    inode = as.double(info$inode),
    hard_links = as.double(info$hard_links),
    modification_time = as.double(info$modification_time),
    change_time = as.double(info$change_time)
  )
  scalar <- vapply(fingerprint, length, integer(1)) == 1L
  missing <- vapply(fingerprint, function(value) anyNA(value), logical(1))
  numeric_fields <- c(
    "size",
    "device_id",
    "inode",
    "hard_links",
    "modification_time",
    "change_time"
  )
  finite <- vapply(
    fingerprint[numeric_fields],
    function(value) is.finite(value),
    logical(1)
  )
  if (
    !all(scalar) ||
      any(missing) ||
      !all(finite) ||
      !nzchar(fingerprint$type) ||
      !nzchar(fingerprint$permissions) ||
      fingerprint$size < 0 ||
      fingerprint$device_id < 0 ||
      fingerprint$inode < 0 ||
      fingerprint$hard_links < 1
  ) {
    stop("A staged App entry could not be fingerprinted safely.", call. = FALSE)
  }
  fingerprint
}

.builder_app_list_directory <- function(path) {
  as.character(fs::dir_ls(
    path,
    all = TRUE,
    recurse = FALSE,
    type = "any",
    fail = TRUE
  ))
}

.builder_app_assert_readable_directory <- function(path, info = NULL) {
  if (identical(.Platform$OS.type, "windows")) {
    return(invisible(TRUE))
  }
  mode <- if (is.null(info)) {
    file.info(path)$mode
  } else {
    info$permissions
  }
  mode <- suppressWarnings(as.integer(mode))
  readable_and_searchable <- length(mode) == 1L &&
    !is.na(mode) &&
    any(vapply(
      c(320L, 40L, 5L),
      function(mask) {
        bitwAnd(mode, mask) == mask
      },
      logical(1)
    ))
  if (!readable_and_searchable) {
    stop(
      "A staged App directory could not be enumerated safely.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.builder_app_enumerate_tree <- function(
  root,
  .list_directory = .builder_app_list_directory
) {
  queue <- list(list(path = root, depth = 0L))
  entries <- character()
  fingerprints <- list()
  snapshots <- list()
  while (length(queue)) {
    current <- queue[[1L]]
    queue <- queue[-1L]
    directory <- current$path
    if (current$depth > 64L) {
      stop(
        "The staged App directory tree is too deep to verify.",
        call. = FALSE
      )
    }
    before <- tryCatch(
      fs::file_info(directory, fail = TRUE, follow = FALSE),
      error = function(error) NULL
    )
    if (
      is.null(before) ||
        !identical(as.character(before$type), "directory") ||
        .builder_app_is_link(directory)
    ) {
      stop("A staged App directory changed before enumeration.", call. = FALSE)
    }
    .builder_app_assert_readable_directory(directory, before)
    if (
      !identical(.Platform$OS.type, "windows") &&
        file.access(directory, mode = 5L) != 0L
    ) {
      stop(
        "A staged App directory could not be enumerated safely.",
        call. = FALSE
      )
    }
    listed <- tryCatch(
      sort(.list_directory(directory), method = "radix"),
      error = function(error) NULL
    )
    if (is.null(listed)) {
      stop(
        "A staged App directory could not be enumerated safely.",
        call. = FALSE
      )
    }
    after <- tryCatch(
      fs::file_info(directory, fail = TRUE, follow = FALSE),
      error = function(error) NULL
    )
    if (
      is.null(after) ||
        !identical(
          .builder_app_file_fingerprint(before),
          .builder_app_file_fingerprint(after)
        )
    ) {
      stop("A staged App directory changed during enumeration.", call. = FALSE)
    }
    if (length(listed)) {
      if (
        anyDuplicated(listed) ||
          !all(dirname(listed) == directory) ||
          length(entries) + length(listed) > 100000L
      ) {
        stop("The staged App directory returned unsafe entries.", call. = FALSE)
      }
      info <- tryCatch(
        fs::file_info(listed, fail = TRUE, follow = FALSE),
        error = function(error) NULL
      )
      linked <- tryCatch(fs::is_link(listed), error = function(error) NULL)
      if (
        is.null(info) ||
          nrow(info) != length(listed) ||
          is.null(linked) ||
          length(linked) != length(listed) ||
          anyNA(linked) ||
          any(linked)
      ) {
        stop(
          "The staged App contains a symbolic or unreadable entry.",
          call. = FALSE
        )
      }
      types <- as.character(info$type)
      if (anyNA(types) || any(!types %in% c("file", "directory"))) {
        stop(
          "The staged App contains an unsupported filesystem entry.",
          call. = FALSE
        )
      }
      listed_fingerprints <- lapply(
        seq_len(nrow(info)),
        function(index) {
          .builder_app_file_fingerprint(info[index, , drop = FALSE])
        }
      )
      directories <- listed[types == "directory"]
      queue <- c(
        queue,
        lapply(directories, function(path) {
          list(path = path, depth = current$depth + 1L)
        })
      )
      entries <- c(entries, listed)
      fingerprints <- c(fingerprints, listed_fingerprints)
    }
    snapshots[[directory]] <- list(
      entries = listed,
      fingerprint = .builder_app_file_fingerprint(after)
    )
  }
  for (directory in names(snapshots)) {
    confirmed_directory <- tryCatch(
      fs::file_info(directory, fail = TRUE, follow = FALSE),
      error = function(error) NULL
    )
    confirmed <- tryCatch(
      sort(.list_directory(directory), method = "radix"),
      error = function(error) NULL
    )
    if (
      is.null(confirmed_directory) ||
        .builder_app_is_link(directory) ||
        !identical(
          .builder_app_file_fingerprint(confirmed_directory),
          snapshots[[directory]]$fingerprint
        ) ||
        is.null(confirmed) ||
        !identical(confirmed, snapshots[[directory]]$entries)
    ) {
      stop("A staged App directory changed after enumeration.", call. = FALSE)
    }
  }
  if (length(entries)) {
    confirmed <- tryCatch(
      fs::file_info(entries, fail = TRUE, follow = FALSE),
      error = function(error) NULL
    )
    if (is.null(confirmed) || nrow(confirmed) != length(entries)) {
      stop("Staged App entries changed after enumeration.", call. = FALSE)
    }
    confirmed_fingerprints <- lapply(
      seq_len(nrow(confirmed)),
      function(index) {
        .builder_app_file_fingerprint(confirmed[index, , drop = FALSE])
      }
    )
    if (
      !all(mapply(
        identical,
        fingerprints,
        confirmed_fingerprints,
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
      ))
    ) {
      stop("Staged App entries changed after enumeration.", call. = FALSE)
    }
  }
  list(paths = entries, fingerprints = fingerprints)
}

.builder_app_tree_identity_once <- function(
  root,
  .digest_file = tools::md5sum
) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  enumerated <- .builder_app_enumerate_tree(root)
  relative <- if (length(enumerated$paths)) {
    substring(enumerated$paths, nchar(root) + 2L)
  } else {
    character()
  }
  order <- order(relative, method = "radix")
  paths <- enumerated$paths[order]
  relative <- relative[order]
  fingerprints <- enumerated$fingerprints[order]
  entries <- lapply(seq_along(paths), function(index) {
    fingerprint <- fingerprints[[index]]
    if (identical(fingerprint$type, "file")) {
      identity <- .builder_app_capture_file_identity(
        paths[[index]],
        .digest_file = .digest_file
      )
      return(list(
        path = relative[[index]],
        type = "file",
        size = identity$size,
        modification_time = identity$modification_time,
        change_time = identity$change_time,
        device_id = identity$device_id,
        inode = identity$inode,
        hard_links = identity$hard_links,
        permissions = identity$permissions,
        md5 = identity$md5
      ))
    }
    list(
      path = relative[[index]],
      type = "directory",
      fingerprint = fingerprint
    )
  })
  names(entries) <- relative
  root_info <- tryCatch(
    fs::file_info(root, fail = TRUE, follow = FALSE),
    error = function(error) NULL
  )
  list(
    schema_version = 1L,
    root_fingerprint = .builder_app_file_fingerprint(root_info),
    entries = entries
  )
}

.builder_app_tree_identity <- function(
  root,
  .digest_file = tools::md5sum
) {
  .builder_app_tree_identity_once(root, .digest_file)
}

.builder_app_portable_tree_entries <- function(identity, prefix = NULL) {
  entries <- identity$entries
  if (!is.null(prefix)) {
    child_prefix <- paste0(prefix, "/")
    selected <- startsWith(names(entries), child_prefix)
    entries <- entries[selected]
    names(entries) <- substring(names(entries), nchar(child_prefix) + 1L)
  }
  portable <- lapply(entries, function(entry) {
    if (identical(entry$type, "directory")) {
      return(list(path = entry$path, type = "directory"))
    }
    .builder_app_portable_file(
      entry$path,
      entry$size,
      entry$md5
    )
  })
  if (!is.null(prefix)) {
    portable <- lapply(seq_along(portable), function(index) {
      entry <- portable[[index]]
      entry$path <- names(portable)[[index]]
      entry
    })
    names(portable) <- names(entries)
  }
  portable
}

.builder_app_package_path <- function(...) {
  path <- system.file(..., package = "CerebroNexus")
  if (!nzchar(path) || !.builder_app_path_exists(path)) {
    stop("A package-owned trusted template is missing.", call. = FALSE)
  }
  path
}

.builder_app_assert_root_topology <- function(identity) {
  paths <- names(identity$entries)
  root_paths <- paths[!grepl("/", paths, fixed = TRUE)]
  expected <- c(
    "app.R" = "file",
    "cerebro_config.rds" = "file",
    "extdata" = "directory",
    "private-data" = "directory",
    "viewer" = "directory"
  )
  actual <- vapply(
    identity$entries[root_paths],
    `[[`,
    character(1),
    "type"
  )
  if (!identical(actual, expected)) {
    stop(
      "The staged App root differs from its trusted topology.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.builder_app_assert_trusted_templates <- function(identity) {
  app_template <- .builder_app_package_path(
    "viewer",
    "_bundle_app.R"
  )
  expected_app <- .builder_app_capture_file_identity(app_template)
  actual_app <- identity$entries[["app.R"]]
  if (
    is.null(actual_app) ||
      !identical(actual_app$type, "file") ||
      !identical(
        .builder_app_portable_file(
          "app.R",
          actual_app$size,
          actual_app$md5
        ),
        .builder_app_portable_file(
          "app.R",
          expected_app$size,
          expected_app$md5
        )
      )
  ) {
    stop("The staged App differs from its trusted template.", call. = FALSE)
  }

  trusted_roots <- list(
    "viewer" = .builder_app_package_path("viewer"),
    extdata = .builder_app_package_path("extdata")
  )
  for (relative_root in names(trusted_roots)) {
    root_entry <- identity$entries[[relative_root]]
    if (is.null(root_entry) || !identical(root_entry$type, "directory")) {
      stop("The staged App differs from its trusted template.", call. = FALSE)
    }
    expected <- .builder_app_portable_tree_entries(
      .builder_app_tree_identity(trusted_roots[[relative_root]])
    )
    actual <- .builder_app_portable_tree_entries(identity, relative_root)
    if (!identical(actual, expected)) {
      stop("The staged App differs from its trusted template.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

.builder_app_output_content_identities <- function(identity, request) {
  relative_crbs <- names(request$backend_plan$entries)
  content <- lapply(relative_crbs, function(relative_crb) {
    crb_entry <- identity$entries[[relative_crb]]
    crb <- if (is.list(crb_entry) && identical(crb_entry$type, "file")) {
      .builder_app_portable_file(
        relative_crb,
        crb_entry$size,
        crb_entry$md5
      )
    } else {
      NULL
    }
    plan_entry <- request$backend_plan$entries[[relative_crb]]
    backend <- if (identical(plan_entry$type, "embedded")) {
      list(type = "embedded", root = NULL, entries = list())
    } else {
      root <- paste0("private-data/", plan_entry$location)
      if (identical(plan_entry$type, "h5")) {
        entry <- identity$entries[[root]]
        entries <- if (is.list(entry) && identical(entry$type, "file")) {
          stats::setNames(
            list(.builder_app_portable_file(
              root,
              entry$size,
              entry$md5
            )),
            root
          )
        } else {
          list()
        }
      } else {
        prefix <- paste0(root, "/")
        paths <- names(identity$entries)
        paths <- paths[startsWith(paths, prefix)]
        entries <- lapply(paths, function(path) {
          entry <- identity$entries[[path]]
          if (identical(entry$type, "directory")) {
            return(list(path = path, type = "directory"))
          }
          .builder_app_portable_file(path, entry$size, entry$md5)
        })
        names(entries) <- paths
      }
      list(type = plan_entry$type, root = root, entries = entries)
    }
    list(crb = crb, backend = backend)
  })
  names(content) <- relative_crbs
  content
}

.builder_app_assert_private_topology <- function(identity, request) {
  expected <- character()
  for (content in request$content_identities) {
    expected[[content$crb$path]] <- "file"
    backend <- content$backend
    if (identical(backend$type, "bpcells")) {
      expected[[backend$root]] <- "directory"
    }
    for (entry in backend$entries) {
      expected[[entry$path]] <- entry$type
    }
  }
  expected <- expected[order(names(expected), method = "radix")]
  paths <- names(identity$entries)
  private_paths <- paths[startsWith(paths, "private-data/")]
  actual <- vapply(
    identity$entries[private_paths],
    `[[`,
    character(1),
    "type"
  )
  if (!identical(actual, expected)) {
    stop(
      "The staged App copied content has an unexpected private topology.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.builder_app_tree_summary <- function(identity) {
  portable <- .builder_app_portable_tree_entries(identity)
  digest_file <- tempfile("builder-app-tree-", fileext = ".rds")
  on.exit(unlink(digest_file), add = TRUE)
  saveRDS(portable, digest_file, version = 3)
  types <- vapply(portable, `[[`, character(1), "type")
  list(
    schema_version = 1L,
    entry_count = length(portable),
    file_count = sum(types == "file"),
    directory_count = sum(types == "directory"),
    aggregate_md5 = unname(tolower(as.character(tools::md5sum(digest_file))))
  )
}

.builder_app_real_path <- function(path) {
  resolved <- tryCatch(
    as.character(fs::path_real(path)),
    error = function(error) NULL
  )
  if (
    is.null(resolved) ||
      length(resolved) != 1L ||
      is.na(resolved) ||
      !nzchar(resolved)
  ) {
    stop("A staged App entry has no canonical path.", call. = FALSE)
  }
  resolved
}

.builder_app_validate_private_locations <- function(
  identity,
  root,
  .canonical_path = .builder_app_real_path
) {
  paths <- names(identity$entries)
  if (!length(paths)) {
    return(invisible(TRUE))
  }
  lower <- tolower(paths)
  private_root <- .canonical_path(file.path(root, "private-data"))
  canonical_entries <- vapply(
    paths,
    function(path) .canonical_path(file.path(root, path)),
    character(1)
  )
  private <- canonical_entries == private_root |
    startsWith(canonical_entries, paste0(private_root, "/"))
  data_entry <-
    grepl("[.](crb|h5|hdf5|rds)$", lower) |
    grepl("(^|/)[^/]+[.]bpcells($|/)", lower)
  data_entry[paths == "cerebro_config.rds"] <- FALSE
  expected_demo_hash <- unname(.builder_app_demo_data[paths])
  allowed_demo <-
    !is.na(expected_demo_hash) &
    vapply(
      identity$entries,
      function(entry) identical(entry$type, "file"),
      logical(1)
    ) &
    vapply(
      seq_along(identity$entries),
      function(index) {
        identical(
          identity$entries[[index]]$md5,
          expected_demo_hash[[index]]
        )
      },
      logical(1)
    )
  if (any(data_entry & !private & !allowed_demo)) {
    stop(
      "The staged App contains private data outside private-data.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.builder_app_safe_relative <- function(path) {
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(path) ||
      grepl("\\", path, fixed = TRUE) ||
      startsWith(path, "/") ||
      grepl("^[A-Za-z]:", path)
  ) {
    return(FALSE)
  }
  parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
  all(nzchar(parts)) && !any(parts %in% c(".", ".."))
}

builder_build_app <- function(
  request,
  stage,
  create_app = CerebroNexus::createShinyApp
) {
  request <- .builder_app_validate_request(request)
  stage <- normalizePath(stage, winslash = "/", mustWork = TRUE)
  if (!identical(stage, request$stage) || .builder_app_is_link(stage)) {
    stop("App assembly requires the request's assigned stage.", call. = FALSE)
  }
  app_dir <- file.path(stage, "cerebro_app")
  if (.builder_app_path_exists(app_dir)) {
    stop("The staged App directory already exists.", call. = FALSE)
  }
  .builder_app_assert_input_identities(request)
  create_app(
    cerebro_data = request$cerebro_data,
    result_dir = app_dir,
    colors = request$colors,
    cerebro_options = list(
      exclude_trivial_metadata = TRUE,
      viewer_content = request$viewer_content
    ),
    overwrite = FALSE,
    quiet = TRUE,
    verbose = FALSE,
    crb_pick_smallest_file = FALSE,
    show_upload_ui = request$show_upload_ui,
    initial_dataset = request$initial_dataset,
    initial_page = request$initial_page,
    welcome_message = request$welcome_message,
    point_size = request$point_size,
    variable_to_compare = request$variable_to_compare,
    host = request$host,
    port = request$port,
    max_request_size = request$max_request_size,
    display_mode = request$display_mode,
    launch_browser = request$launch_browser
  )
  .builder_app_assert_input_identities(request)
  if (
    !dir.exists(app_dir) ||
      .builder_app_is_link(app_dir) ||
      !.builder_app_path_within(app_dir, stage)
  ) {
    stop(
      "App assembly did not create the assigned private directory.",
      call. = FALSE
    )
  }
  app_dir
}

builder_verify_app <- function(
  app_dir,
  request,
  .tree_identity = .builder_app_tree_identity,
  .retain_tree_identity = FALSE
) {
  request <- .builder_app_validate_request(request)
  if (!dir.exists(app_dir) || .builder_app_is_link(app_dir)) {
    stop("The staged App directory is missing or symbolic.", call. = FALSE)
  }
  app_dir <- normalizePath(app_dir, winslash = "/", mustWork = TRUE)
  expected_app_dir <- file.path(request$stage, "cerebro_app")
  if (!identical(app_dir, expected_app_dir)) {
    stop("The staged App is outside its assigned stage.", call. = FALSE)
  }
  tree_before <- .tree_identity(app_dir)
  legacy <- file.path(app_dir, "data")
  if (.builder_app_path_exists(legacy)) {
    stop(
      "The staged App contains the forbidden legacy data directory.",
      call. = FALSE
    )
  }
  app_file <- file.path(app_dir, "app.R")
  config_file <- file.path(app_dir, "cerebro_config.rds")
  if (
    !file.exists(app_file) ||
      dir.exists(app_file) ||
      .builder_app_is_link(app_file)
  ) {
    stop("The staged app.R is missing or symbolic.", call. = FALSE)
  }
  tryCatch(
    parse(file = app_file, keep.source = FALSE),
    error = function(error) {
      stop("The staged app.R cannot be parsed.", call. = FALSE)
    }
  )
  if (
    !file.exists(config_file) ||
      dir.exists(config_file) ||
      .builder_app_is_link(config_file)
  ) {
    stop("The staged App config is missing or symbolic.", call. = FALSE)
  }
  config_info <- tryCatch(
    fs::file_info(config_file, fail = TRUE, follow = FALSE),
    error = function(error) NULL
  )
  if (
    is.null(config_info) ||
      nrow(config_info) != 1L ||
      !identical(as.character(config_info$type), "file") ||
      !is.finite(as.double(config_info$size)) ||
      as.double(config_info$size) > .builder_app_config_max_bytes
  ) {
    stop("The staged App config is too large to read safely.", call. = FALSE)
  }
  config <- tryCatch(readRDS(config_file), error = function(error) error)
  if (
    inherits(config, "condition") ||
      !is.list(config) ||
      .builder_app_has_reference(config)
  ) {
    stop("The staged App config is not an inert readable list.", call. = FALSE)
  }
  config <- .builder_app_plain_value(config)
  crbs <- config[["crb_file_to_load"]]
  expected_crbs <- stats::setNames(
    file.path("private-data", basename(request$cerebro_data)),
    request$selector_order
  )
  if (!identical(crbs, expected_crbs)) {
    stop(
      "The staged App selector labels or order differ from request.",
      call. = FALSE
    )
  }
  if (!identical(config[["initial_dataset"]], request$initial_dataset)) {
    stop("The staged App initial dataset differs from request.", call. = FALSE)
  }
  if (!identical(config[["initial_page"]], request$initial_page)) {
    stop("The staged App starting page differs from request.", call. = FALSE)
  }
  if (!identical(config[["show_upload_ui"]], request$show_upload_ui)) {
    stop("The staged App upload policy differs from request.", call. = FALSE)
  }
  if (!identical(config[["welcome_message"]], request$welcome_message)) {
    stop("The staged App welcome message differs from request.", call. = FALSE)
  }
  if (!identical(config[["point_size"]], request$point_size)) {
    stop("The staged App point sizes differ from request.", call. = FALSE)
  }
  if (!identical(config[["viewer_content"]], request$viewer_content)) {
    stop("The staged App Viewer defaults differ from request.", call. = FALSE)
  }
  if (
    !identical(config[["variable_to_compare"]], request$variable_to_compare)
  ) {
    stop(
      "The staged App comparison option differs from request.",
      call. = FALSE
    )
  }
  expected_run_options <- list(
    schema_version = 1L,
    max_request_size_bytes = as.double(request$max_request_size * 1024^2),
    shiny_app_options = list(
      port = as.integer(request$port),
      host = request$host,
      launch.browser = request$launch_browser,
      quiet = TRUE,
      display.mode = request$display_mode
    )
  )
  if (!identical(config[[".bundle_run_options"]], expected_run_options)) {
    stop("The staged App launch options differ from request.", call. = FALSE)
  }
  if (!identical(config[["colors"]], request$colors)) {
    stop("The staged App palettes differ from request.", call. = FALSE)
  }
  if (
    !identical(
      config[["crb_pick_smallest_file"]],
      request$crb_pick_smallest_file
    )
  ) {
    stop(
      "The staged App smallest-file policy differs from request.",
      call. = FALSE
    )
  }
  backend_plan <- config[[".bundle_backend_plan"]]
  if (!identical(backend_plan, request$backend_plan)) {
    stop("The staged App backend plan differs from request.", call. = FALSE)
  }

  private_root <- file.path(app_dir, "private-data")
  if (!dir.exists(private_root) || .builder_app_is_link(private_root)) {
    stop(
      "The staged App private-data directory is missing or symbolic.",
      call. = FALSE
    )
  }
  private_root <- normalizePath(private_root, winslash = "/", mustWork = TRUE)
  .builder_app_validate_private_locations(tree_before, app_dir)
  .builder_app_assert_trusted_templates(tree_before)
  .builder_app_assert_root_topology(tree_before)
  configured_files <- character()
  for (index in seq_along(crbs)) {
    relative_crb <- unname(crbs[[index]])
    if (
      !.builder_app_safe_relative(relative_crb) ||
        !startsWith(relative_crb, "private-data/")
    ) {
      stop("A configured CRB path escapes private-data.", call. = FALSE)
    }
    crb <- file.path(app_dir, relative_crb)
    if (
      !file.exists(crb) ||
        dir.exists(crb) ||
        .builder_app_is_link(crb) ||
        !.builder_app_path_within(crb, private_root)
    ) {
      stop(
        "A configured CRB is missing, symbolic, or outside private-data.",
        call. = FALSE
      )
    }
    configured_files <- c(configured_files, normalizePath(crb, winslash = "/"))
    entry <- backend_plan$entries[[relative_crb]]
    if (identical(entry$mode, "bundled")) {
      if (!.builder_app_safe_relative(entry$location)) {
        stop(
          "A configured backend sidecar path escapes private-data.",
          call. = FALSE
        )
      }
      sidecar <- file.path(dirname(crb), entry$location)
      expected_type <- switch(
        entry$type,
        h5 = "file",
        bpcells = "directory",
        NULL
      )
      sidecar_info <- tryCatch(
        fs::file_info(sidecar, fail = TRUE, follow = FALSE),
        error = function(error) NULL
      )
      if (
        is.null(expected_type) ||
          !.builder_app_path_exists(sidecar) ||
          .builder_app_is_link(sidecar) ||
          is.null(sidecar_info) ||
          !identical(as.character(sidecar_info$type), expected_type) ||
          !.builder_app_path_within(sidecar, private_root)
      ) {
        stop(
          paste0(
            "A configured backend sidecar must be the expected ",
            if (identical(expected_type, "file")) {
              "regular file"
            } else {
              "directory"
            },
            " inside private-data."
          ),
          call. = FALSE
        )
      }
      configured_files <- c(
        configured_files,
        normalizePath(sidecar, winslash = "/", mustWork = TRUE)
      )
    }
  }

  .builder_app_assert_private_topology(tree_before, request)
  copied_content <- .builder_app_output_content_identities(
    tree_before,
    request
  )
  if (!identical(copied_content, request$content_identities)) {
    stop(
      "The staged App copied content differs from frozen input.",
      call. = FALSE
    )
  }

  tree_after <- .tree_identity(app_dir)
  if (!identical(tree_before, tree_after)) {
    stop(
      "The staged App tree changed during verification.",
      call. = FALSE
    )
  }

  verification <- structure(
    list(
      valid = TRUE,
      contract_version = 1L,
      app_dir = app_dir,
      selector_order = request$selector_order,
      initial_dataset = request$initial_dataset,
      show_upload_ui = request$show_upload_ui,
      colors = request$colors,
      backend_plan = request$backend_plan,
      private_files = configured_files,
      legacy_data_absent = TRUE,
      diagnostic_tree_identity = .builder_app_tree_summary(tree_after)
    ),
    class = c("builder_app_verification", "list")
  )
  if (isTRUE(.retain_tree_identity)) {
    attr(verification, "parent_tree_identity") <- tree_after
  }
  verification
}
