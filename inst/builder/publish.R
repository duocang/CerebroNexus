##----------------------------------------------------------------------------##
## Parent-owned, recoverable publication of one Builder release.
##
## A coordinator holds one owner-identified directory lock from stage
## registration through publication. Every destructive transition is preceded
## or followed by an atomic journal record so a later parent can recover after
## process death without guessing ownership from elapsed time.
##----------------------------------------------------------------------------##

.builder_release_text <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
}

.builder_release_exists <- function(path) {
  file.exists(path) || dir.exists(path) || .builder_release_link(path)
}

.builder_release_link <- function(path) {
  linked <- tryCatch(fs::is_link(path), error = function(error) NA)
  length(linked) == 1L && (is.na(linked) || isTRUE(unname(linked)))
}

.builder_release_token <- function(prefix = "owner") {
  paste(
    prefix,
    Sys.getpid(),
    format(Sys.time(), "%Y%m%d%H%M%OS6", tz = "UTC"),
    sprintf("%08x", sample.int(.Machine$integer.max, 1L)),
    sep = "-"
  )
}

.builder_release_host <- function() {
  host <- unname(Sys.info()[["nodename"]])
  if (!.builder_release_text(host)) "unknown-host" else host
}

.builder_release_or <- function(value, fallback) {
  if (is.null(value)) fallback else value
}

.builder_release_mode_owner_only <- function(path) {
  if (identical(.Platform$OS.type, "windows")) {
    return(TRUE)
  }
  mode <- file.info(path)$mode
  length(mode) == 1L && !is.na(mode) && bitwAnd(as.integer(mode), 63L) == 0L
}

.builder_release_path <- function(target) {
  if (!.builder_release_text(target)) {
    stop("A release target is required.", call. = FALSE)
  }
  if (.builder_release_link(target)) {
    stop("The release target cannot be a symbolic link.", call. = FALSE)
  }
  canonical <- .canonicalTargetPath(target)
  leaf <- basename(canonical)
  if (
    !nzchar(leaf) ||
      leaf %in% c(".", "..", "/") ||
      .windowsPathSegmentInvalid(leaf)
  ) {
    stop("The release target has an unsafe name.", call. = FALSE)
  }
  parent <- dirname(canonical)
  if (!dir.exists(parent) || .builder_release_link(parent)) {
    stop(
      "The release parent must be an existing real directory.",
      call. = FALSE
    )
  }
  canonical
}

builder_release_control_path <- function(target) {
  target <- .builder_release_path(target)
  .canonicalTargetPath(file.path(
    dirname(target),
    paste0(".", basename(target), ".cerebro-control")
  ))
}

.builder_release_relative <- function(path, root) {
  path <- .canonicalTargetPath(path)
  root <- .canonicalTargetPath(root)
  if (!.pathWithin(path, root)) {
    stop("A release entry escaped its owned root.", call. = FALSE)
  }
  if (identical(path, root)) "" else substring(path, nchar(root) + 2L)
}

.builder_release_file_fingerprint <- function(info) {
  if (is.null(info) || nrow(info) != 1L) {
    stop("A release entry could not be fingerprinted safely.", call. = FALSE)
  }
  fingerprint <- list(
    type = as.character(info$type),
    size = as.double(info$size),
    permissions = as.character(info$permissions),
    device_id = as.double(info$device_id),
    inode = as.double(info$inode),
    modification_time = as.double(info$modification_time),
    change_time = as.double(info$change_time)
  )
  scalar <- vapply(fingerprint, length, integer(1)) == 1L
  missing <- vapply(fingerprint, function(value) anyNA(value), logical(1))
  numeric_fields <- c(
    "size",
    "device_id",
    "inode",
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
      fingerprint$size != floor(fingerprint$size) ||
      fingerprint$device_id < 0 ||
      fingerprint$inode < 0
  ) {
    stop("A release entry could not be fingerprinted safely.", call. = FALSE)
  }
  fingerprint
}

.builder_release_normalized_paths <- function(paths) {
  normalized <- tryCatch(
    as.character(fs::path_norm(fs::path_abs(paths))),
    error = function(error) NULL
  )
  if (
    is.null(normalized) ||
      length(normalized) != length(paths) ||
      anyNA(normalized) ||
      any(!nzchar(normalized))
  ) {
    stop("Release entries returned unsafe paths.", call. = FALSE)
  }
  normalized
}

.builder_release_list_directory <- function(path) {
  as.character(fs::dir_ls(
    path,
    all = TRUE,
    recurse = FALSE,
    type = "any",
    fail = TRUE
  ))
}

.builder_release_enumerate <- function(
  target,
  .list_directory = .builder_release_list_directory
) {
  queue <- target
  entries <- character()
  entry_fingerprints <- list()
  snapshots <- list()
  while (length(queue)) {
    directory <- queue[[1L]]
    queue <- queue[-1L]
    before <- tryCatch(
      fs::file_info(directory, fail = TRUE, follow = FALSE),
      error = function(error) NULL
    )
    if (is.null(before) || !identical(as.character(before$type), "directory")) {
      stop("A release directory changed before enumeration.", call. = FALSE)
    }
    listed <- tryCatch(
      sort(.list_directory(directory), method = "radix"),
      error = function(error) NULL
    )
    if (is.null(listed)) {
      stop("A release directory could not be enumerated safely.", call. = FALSE)
    }
    after <- tryCatch(
      fs::file_info(directory, fail = TRUE, follow = FALSE),
      error = function(error) NULL
    )
    if (
      is.null(after) ||
        !identical(
          .builder_release_file_fingerprint(before),
          .builder_release_file_fingerprint(after)
        )
    ) {
      stop("A release directory changed during enumeration.", call. = FALSE)
    }
    if (length(listed)) {
      parents <- vapply(listed, dirname, "")
      if (
        anyDuplicated(listed) ||
          !all(vapply(
            parents,
            function(parent) {
              identical(
                .canonicalTargetPath(parent),
                .canonicalTargetPath(directory)
              )
            },
            logical(1)
          ))
      ) {
        stop("A release directory returned unsafe entries.", call. = FALSE)
      }
      listed_info <- tryCatch(
        fs::file_info(listed, fail = TRUE, follow = FALSE),
        error = function(error) NULL
      )
      if (is.null(listed_info) || nrow(listed_info) != length(listed)) {
        stop("Release entries changed during enumeration.", call. = FALSE)
      }
      listed_paths <- .builder_release_normalized_paths(listed)
      info_paths <- .builder_release_normalized_paths(listed_info$path)
      if (
        anyDuplicated(listed_paths) ||
          anyDuplicated(info_paths) ||
          !setequal(listed_paths, info_paths)
      ) {
        stop("Release entries changed during enumeration.", call. = FALSE)
      }
      info_index <- match(listed_paths, info_paths)
      listed_info <- listed_info[info_index, , drop = FALSE]
      listed_fingerprints <- lapply(
        seq_len(nrow(listed_info)),
        function(index) {
          .builder_release_file_fingerprint(
            listed_info[index, , drop = FALSE]
          )
        }
      )
      directories <- as.character(listed_info$type) %in% "directory"
      queue <- c(queue, listed[directories])
      entries <- c(entries, listed)
      entry_fingerprints <- c(entry_fingerprints, listed_fingerprints)
    }
    snapshots[[directory]] <- listed
  }
  for (directory in names(snapshots)) {
    confirmed <- tryCatch(
      sort(.list_directory(directory), method = "radix"),
      error = function(error) NULL
    )
    if (is.null(confirmed) || !identical(confirmed, snapshots[[directory]])) {
      stop("A release directory changed after enumeration.", call. = FALSE)
    }
  }
  if (length(entries)) {
    confirmed_info <- tryCatch(
      fs::file_info(entries, fail = TRUE, follow = FALSE),
      error = function(error) NULL
    )
    if (is.null(confirmed_info) || nrow(confirmed_info) != length(entries)) {
      stop("Release entries changed after enumeration.", call. = FALSE)
    }
    entry_paths <- .builder_release_normalized_paths(entries)
    confirmed_paths <- .builder_release_normalized_paths(confirmed_info$path)
    if (
      anyDuplicated(entry_paths) ||
        anyDuplicated(confirmed_paths) ||
        !setequal(entry_paths, confirmed_paths)
    ) {
      stop("Release entries changed after enumeration.", call. = FALSE)
    }
    confirmed_index <- match(entry_paths, confirmed_paths)
    confirmed_info <- confirmed_info[confirmed_index, , drop = FALSE]
    confirmed_fingerprints <- lapply(
      seq_len(nrow(confirmed_info)),
      function(index) {
        .builder_release_file_fingerprint(
          confirmed_info[index, , drop = FALSE]
        )
      }
    )
    stable <- mapply(
      identical,
      entry_fingerprints,
      confirmed_fingerprints,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )
    if (!all(stable)) {
      stop("Release entries changed after enumeration.", call. = FALSE)
    }
  }
  list(paths = entries, fingerprints = entry_fingerprints)
}

.builder_release_payload_snapshot <- function(path) {
  info <- tryCatch(
    fs::file_info(path, fail = TRUE, follow = FALSE),
    error = function(error) NULL
  )
  if (is.null(info) || nrow(info) != 1L) {
    stop("A release payload could not be read safely.", call. = FALSE)
  }
  snapshot <- list(
    type = as.character(info$type),
    size = as.double(info$size),
    permissions = as.character(info$permissions),
    device_id = as.double(info$device_id),
    inode = as.double(info$inode),
    hard_links = as.double(info$hard_links),
    modification_time = as.double(info$modification_time),
    change_time = as.double(info$change_time)
  )
  scalar <- vapply(snapshot, length, integer(1)) == 1L
  missing <- vapply(snapshot, function(value) anyNA(value), logical(1))
  numeric_fields <- c(
    "size",
    "device_id",
    "inode",
    "hard_links",
    "modification_time",
    "change_time"
  )
  finite <- vapply(
    snapshot[numeric_fields],
    function(value) is.finite(value),
    logical(1)
  )
  if (
    !all(scalar) ||
      any(missing) ||
      !all(finite) ||
      !identical(snapshot$type, "file") ||
      !nzchar(snapshot$permissions) ||
      snapshot$size < 0 ||
      snapshot$size != floor(snapshot$size) ||
      snapshot$device_id < 0 ||
      snapshot$inode < 0 ||
      snapshot$hard_links < 1
  ) {
    stop("A release payload could not be read safely.", call. = FALSE)
  }
  snapshot
}

.builder_release_payload_md5_valid <- function(md5) {
  is.character(md5) &&
    length(md5) == 1L &&
    !is.na(md5) &&
    grepl("^[[:xdigit:]]{32}$", md5)
}

.builder_release_payload_md5 <- function(path) {
  md5 <- tryCatch(
    suppressWarnings(unname(tools::md5sum(path))),
    error = function(error) NA_character_
  )
  if (!.builder_release_payload_md5_valid(md5)) {
    stop("A release payload could not be read safely.", call. = FALSE)
  }
  md5
}

.builder_release_ignorable_metadata <- function(paths) {
  paths <- gsub("\\", "/", as.character(paths), fixed = TRUE)
  basename(paths) == ".DS_Store"
}

builder_release_identity <- function(
  target,
  .list_directory = .builder_release_list_directory,
  .hash_file = .builder_release_payload_md5
) {
  target <- .builder_release_path(target)
  if (!.builder_release_exists(target)) {
    return(list(schema_version = 1L, exists = FALSE, entries = list()))
  }
  if (!dir.exists(target) || .builder_release_link(target)) {
    stop("A Builder release must be a real directory.", call. = FALSE)
  }
  enumerated <- .builder_release_enumerate(target, .list_directory)
  entries <- enumerated$paths
  if (!length(entries)) {
    return(list(schema_version = 1L, exists = TRUE, entries = list()))
  }
  relative <- vapply(entries, .builder_release_relative, "", root = target)
  linked <- vapply(entries, .builder_release_link, logical(1))
  entry_types <- vapply(
    enumerated$fingerprints,
    `[[`,
    character(1),
    "type"
  )
  ignorable <- .builder_release_ignorable_metadata(relative) &
    !linked &
    !is.na(entry_types) &
    entry_types == "file"
  if (any(ignorable)) {
    entries <- entries[!ignorable]
    relative <- relative[!ignorable]
    linked <- linked[!ignorable]
    entry_types <- entry_types[!ignorable]
  }
  if (!length(entries)) {
    return(list(schema_version = 1L, exists = TRUE, entries = list()))
  }
  if (any(linked)) {
    stop(
      "A release identity cannot include a symbolic link: ",
      basename(entries[[which(linked)[[1L]]]]),
      call. = FALSE
    )
  }
  unsupported <- is.na(entry_types) |
    !entry_types %in% c("file", "directory")
  if (any(unsupported)) {
    stop(
      "A release identity cannot include an unsupported filesystem entry: ",
      basename(entries[[which(unsupported)[[1L]]]]),
      call. = FALSE
    )
  }
  order_index <- order(relative, method = "radix")
  entries <- entries[order_index]
  relative <- relative[order_index]
  records <- lapply(seq_along(entries), function(index) {
    path <- entries[[index]]
    directory <- identical(entry_types[[order_index[[index]]]], "directory")
    if (directory) {
      return(list(
        path = gsub("\\", "/", relative[[index]], fixed = TRUE),
        type = "directory",
        size = 0,
        md5 = NA_character_
      ))
    }
    before <- .builder_release_payload_snapshot(path)
    md5 <- tryCatch(.hash_file(path), error = function(error) NA_character_)
    if (!.builder_release_payload_md5_valid(md5)) {
      stop("A release payload could not be read safely.", call. = FALSE)
    }
    after <- .builder_release_payload_snapshot(path)
    if (!identical(before, after)) {
      stop("The release changed while its identity was read.", call. = FALSE)
    }
    list(
      path = gsub("\\", "/", relative[[index]], fixed = TRUE),
      type = "file",
      size = before$size,
      md5 = md5
    )
  })
  list(schema_version = 1L, exists = TRUE, entries = records)
}

.builder_release_record_name <- ".cerebro-builder-release-v1"
.builder_release_record_header <- "CEREBRO_BUILDER_RELEASE_V1"

.builder_release_identity_members <- function(
  identity,
  include_record = FALSE
) {
  entries <- identity$entries
  if (!include_record && length(entries)) {
    paths <- vapply(entries, `[[`, character(1), "path")
    entries <- entries[!paths %in% .builder_release_record_name]
  }
  lapply(entries, function(entry) {
    list(
      type = if (identical(entry$type, "directory")) "D" else "F",
      path = entry$path
    )
  })
}

.builder_release_record_error <- function(message) {
  stop("The release ownership record ", message, call. = FALSE)
}

.builder_release_utf8 <- function(value) {
  converted <- iconv(value, from = "", to = "UTF-8", sub = NA_character_)
  if (anyNA(converted) || !all(validUTF8(converted))) {
    .builder_release_record_error("contains text that is not valid UTF-8.")
  }
  non_ascii <- grepl("[^ -~]", converted)
  Encoding(converted[non_ascii]) <- "UTF-8"
  converted
}

.builder_release_raw_md5 <- function(bytes) {
  path <- tempfile("cerebro-builder-record-")
  connection <- .builder_release_open_owner_only(path)
  on.exit(
    {
      try(close(connection), silent = TRUE)
      if (file.exists(path)) {
        unlink(path, force = TRUE)
      }
    },
    add = TRUE
  )
  writeBin(bytes, connection)
  close(connection)
  unname(tools::md5sum(path))
}

.builder_release_open_owner_only <- function(path) {
  previous_umask <- Sys.umask("0177")
  on.exit(Sys.umask(previous_umask), add = TRUE)
  suppressWarnings(file(path, open = "w+xb"))
}

.builder_release_split_preserve <- function(text, delimiter) {
  positions <- gregexpr(delimiter, text, fixed = TRUE)[[1L]]
  if (identical(positions, -1L)) {
    return(text)
  }
  starts <- c(1L, positions + nchar(delimiter))
  ends <- c(positions - 1L, nchar(text))
  mapply(
    function(start, end) substr(text, start, end),
    starts,
    ends,
    USE.NAMES = FALSE
  )
}

.builder_release_read_record <- function(
  target,
  identity = builder_release_identity(target),
  exact = TRUE,
  allow_abandoned = FALSE
) {
  target <- .builder_release_path(target)
  record_path <- file.path(target, .builder_release_record_name)
  if (!file.exists(record_path)) {
    return(NULL)
  }
  if (dir.exists(record_path) || .builder_release_link(record_path)) {
    .builder_release_record_error("is not a regular file.")
  }
  if (!identical(.Platform$OS.type, "windows")) {
    mode <- file.info(record_path)$mode
    if (
      length(mode) != 1L || is.na(mode) || bitwAnd(as.integer(mode), 73L) != 0L
    ) {
      .builder_release_record_error("must not be executable.")
    }
  }
  size <- file.info(record_path)$size
  if (length(size) != 1L || is.na(size) || size < 1) {
    .builder_release_record_error("is empty or unreadable.")
  }
  bytes <- readBin(record_path, what = "raw", n = as.integer(size))
  identity_paths <- if (length(identity$entries)) {
    vapply(identity$entries, `[[`, character(1), "path")
  } else {
    character()
  }
  record_index <- which(identity_paths %in% .builder_release_record_name)
  if (
    length(record_index) != 1L ||
      !identical(identity$entries[[record_index]]$type, "file") ||
      !identical(
        as.double(length(bytes)),
        identity$entries[[record_index]]$size
      ) ||
      !identical(
        .builder_release_raw_md5(bytes),
        identity$entries[[record_index]]$md5
      )
  ) {
    .builder_release_record_error("changed while its snapshot was read.")
  }
  text <- tryCatch(rawToChar(bytes), error = function(error) NA_character_)
  if (
    length(text) != 1L ||
      is.na(text) ||
      !isTRUE(validUTF8(text)) ||
      grepl("[\001-\010\013\014\016-\037\177]", text)
  ) {
    .builder_release_record_error("is not valid UTF-8 text.")
  }
  Encoding(text) <- "UTF-8"
  lines <- .builder_release_split_preserve(text, "\n")
  if (length(lines) && !nzchar(lines[[length(lines)]])) {
    lines <- lines[-length(lines)]
  }
  if (
    !length(lines) || !identical(lines[[1L]], .builder_release_record_header)
  ) {
    .builder_release_record_error("has an invalid header.")
  }
  member_lines <- lines[-1L]
  if (any(!nzchar(member_lines))) {
    .builder_release_record_error("contains a blank member.")
  }
  if (!identical(member_lines, sort(member_lines, method = "radix"))) {
    .builder_release_record_error("members are not sorted.")
  }
  members <- lapply(member_lines, function(line) {
    fields <- .builder_release_split_preserve(line, "\t")
    if (length(fields) != 2L || !fields[[1L]] %in% c("F", "D")) {
      .builder_release_record_error("contains a malformed member.")
    }
    path <- tryCatch(
      .portableBundlePath(fields[[2L]], "An ownership record member"),
      error = function(error) NULL
    )
    if (
      is.null(path) ||
        identical(path, .builder_release_record_name) ||
        grepl("[[:cntrl:]]", path)
    ) {
      .builder_release_record_error("contains an unsafe member path.")
    }
    list(type = fields[[1L]], path = path)
  })
  member_paths <- if (length(members)) {
    vapply(members, `[[`, character(1), "path")
  } else {
    character()
  }
  if (anyDuplicated(member_paths)) {
    .builder_release_record_error("contains a duplicate member.")
  }
  actual <- .builder_release_identity_members(identity)
  actual_paths <- if (length(actual)) {
    vapply(actual, `[[`, character(1), "path")
  } else {
    character()
  }
  actual_by_path <- stats::setNames(actual, actual_paths)
  missing <- setdiff(member_paths, actual_paths)
  wrong_type <- vapply(
    members,
    function(member) {
      !member$path %in% actual_paths ||
        !identical(actual_by_path[[member$path]]$type, member$type)
    },
    logical(1)
  )
  recorded_present <- intersect(member_paths, actual_paths)
  shell_foreign <- actual_paths[
    !.builder_release_ignorable_metadata(actual_paths)
  ]
  abandoned <- isTRUE(allow_abandoned) &&
    !isTRUE(exact) &&
    length(member_paths) > 0L &&
    !length(recorded_present) &&
    !length(shell_foreign)
  if ((length(missing) || any(wrong_type)) && !abandoned) {
    .builder_release_record_error("does not match its recorded members.")
  }
  foreign <- if (abandoned) {
    character()
  } else {
    setdiff(actual_paths, member_paths)
  }
  foreign <- foreign[!.builder_release_ignorable_metadata(foreign)]
  if (isTRUE(exact) && length(foreign)) {
    .builder_release_record_error("does not match the complete release.")
  }
  list(
    bytes = bytes,
    members = members,
    foreign = foreign,
    abandoned = abandoned,
    identity = identity
  )
}

.builder_release_write_record <- function(
  target,
  identity,
  token,
  .move = file.rename,
  .after_create = function(temporary) invisible(NULL)
) {
  target <- .builder_release_path(target)
  if (
    !.builder_release_identity_valid(identity) || !.builder_release_text(token)
  ) {
    .builder_release_record_error("cannot be written from invalid state.")
  }
  members <- .builder_release_identity_members(identity)
  if (length(members)) {
    utf8_paths <- .builder_release_utf8(vapply(
      members,
      `[[`,
      character(1),
      "path"
    ))
    members <- Map(
      function(member, path) {
        member$path <- path
        member
      },
      members,
      utf8_paths
    )
  }
  lines <- if (length(members)) {
    sort(
      vapply(
        members,
        function(member) {
          paste(member$type, member$path, sep = "\t")
        },
        ""
      ),
      method = "radix"
    )
  } else {
    character()
  }
  content <- paste0(
    paste(c(.builder_release_record_header, lines), collapse = "\n"),
    "\n"
  )
  record_path <- file.path(target, .builder_release_record_name)
  temporary <- file.path(
    target,
    paste0(".", .builder_release_record_name, ".", token, ".tmp")
  )
  connection <- tryCatch(
    .builder_release_open_owner_only(temporary),
    error = function(error) NULL
  )
  if (is.null(connection)) {
    .builder_release_record_error(
      "temporary path could not be created exclusively."
    )
  }
  created <- TRUE
  connection_open <- TRUE
  moved <- FALSE
  on.exit(
    {
      if (connection_open) {
        try(close(connection), silent = TRUE)
      }
      if (!moved && created) {
        unlink(temporary, force = TRUE)
      }
    },
    add = TRUE
  )
  content_bytes <- charToRaw(enc2utf8(content))
  writeBin(content_bytes, connection)
  flush(connection)
  created_info <- tryCatch(
    fs::file_info(temporary, fail = TRUE, follow = FALSE),
    error = function(error) NULL
  )
  if (
    is.null(created_info) ||
      !identical(as.character(created_info$type), "file") ||
      !identical(as.double(created_info$size), as.double(length(content_bytes)))
  ) {
    .builder_release_record_error("temporary file is unsafe after creation.")
  }
  .after_create(temporary)
  final_info <- tryCatch(
    fs::file_info(temporary, fail = TRUE, follow = FALSE),
    error = function(error) NULL
  )
  stable_fields <- c(
    "type",
    "size",
    "permissions",
    "device_id",
    "inode",
    "hard_links",
    "modification_time",
    "change_time"
  )
  seek(connection, where = 0L, origin = "start", rw = "read")
  confirmed_bytes <- readBin(
    connection,
    what = "raw",
    n = length(content_bytes) + 1L
  )
  verified_info <- tryCatch(
    fs::file_info(temporary, fail = TRUE, follow = FALSE),
    error = function(error) NULL
  )
  if (
    is.null(final_info) ||
      is.null(verified_info) ||
      !identical(
        as.list(created_info[stable_fields]),
        as.list(final_info[stable_fields])
      ) ||
      !identical(
        as.list(final_info[stable_fields]),
        as.list(verified_info[stable_fields])
      ) ||
      !identical(confirmed_bytes, content_bytes)
  ) {
    .builder_release_record_error(
      "temporary file is unsafe before publication."
    )
  }
  close(connection)
  connection_open <- FALSE
  if (
    !isTRUE(tryCatch(.move(temporary, record_path), error = function(error) {
      FALSE
    }))
  ) {
    .builder_release_record_error("could not be replaced atomically.")
  }
  moved <- TRUE
  rename_fields <- setdiff(stable_fields, "change_time")
  tryCatch(
    {
      published_info <- fs::file_info(
        record_path,
        fail = TRUE,
        follow = FALSE
      )
      if (
        !identical(
          as.list(verified_info[rename_fields]),
          as.list(published_info[rename_fields])
        )
      ) {
        .builder_release_record_error("changed during publication.")
      }
      final_identity <- builder_release_identity(target)
      published <- .builder_release_read_record(
        target,
        final_identity,
        exact = TRUE
      )
      confirmed_info <- fs::file_info(
        record_path,
        fail = TRUE,
        follow = FALSE
      )
      if (
        !identical(
          as.list(published_info[rename_fields]),
          as.list(confirmed_info[rename_fields])
        ) ||
          !identical(published$bytes, content_bytes)
      ) {
        .builder_release_record_error("changed during publication.")
      }
      published
    },
    error = function(error) {
      unlink(record_path, force = TRUE)
      stop(error)
    }
  )
}

builder_release_state <- function(
  target,
  exact_record = TRUE,
  allow_abandoned = FALSE
) {
  identity <- builder_release_identity(target)
  record <- if (isTRUE(identity$exists)) {
    .builder_release_read_record(
      target,
      identity,
      exact = exact_record,
      allow_abandoned = allow_abandoned
    )
  } else {
    NULL
  }
  confirmed_identity <- builder_release_identity(target)
  if (!identical(confirmed_identity, identity)) {
    stop("The release changed while its snapshot was read.", call. = FALSE)
  }
  list(
    schema_version = 1L,
    identity = identity,
    record = if (is.null(record)) {
      NULL
    } else {
      list(
        bytes = record$bytes,
        members = record$members,
        foreign = record$foreign,
        abandoned = isTRUE(record$abandoned)
      )
    }
  )
}

.builder_release_identity_valid <- function(identity) {
  is.list(identity) &&
    identical(identity$schema_version, 1L) &&
    is.logical(identity$exists) &&
    length(identity$exists) == 1L &&
    !is.na(identity$exists) &&
    is.list(identity$entries)
}

.builder_release_atomic_rds <- function(value, path, token) {
  temporary <- file.path(
    dirname(path),
    paste0(".", basename(path), ".", token, ".tmp")
  )
  if (.builder_release_exists(temporary)) {
    stop("A journal temporary path is already occupied.", call. = FALSE)
  }
  saved <- FALSE
  on.exit(
    {
      if (!saved && file.exists(temporary)) unlink(temporary, force = TRUE)
    },
    add = TRUE
  )
  saveRDS(value, temporary, version = 3)
  Sys.chmod(temporary, mode = "0600")
  if (!file.rename(temporary, path)) {
    stop(
      "The publication journal could not be replaced atomically.",
      call. = FALSE
    )
  }
  saved <- TRUE
  invisible(value)
}

.builder_release_read_rds <- function(path, subject) {
  if (!file.exists(path) || dir.exists(path) || .builder_release_link(path)) {
    stop(subject, " is missing or unsafe.", call. = FALSE)
  }
  tryCatch(readRDS(path), error = function(error) {
    stop(subject, " cannot be read safely.", call. = FALSE)
  })
}

.builder_release_allowed_control <- c(
  "journal.rds",
  "lock",
  "stages",
  "backup",
  "diagnostics"
)

.builder_release_assert_control <- function(control) {
  if (!dir.exists(control) || .builder_release_link(control)) {
    stop("The release control path is not a real directory.", call. = FALSE)
  }
  if (!.builder_release_mode_owner_only(control)) {
    stop("The release control directory must be owner-only.", call. = FALSE)
  }
  entries <- list.files(control, all.files = TRUE, no.. = TRUE)
  unknown <- setdiff(entries, .builder_release_allowed_control)
  if (length(unknown)) {
    stop(
      "The release has unknown control occupants: ",
      paste(unknown, collapse = ", "),
      ". Nothing was removed.",
      call. = FALSE
    )
  }
  known_paths <- file.path(control, entries)
  linked <- vapply(known_paths, .builder_release_link, logical(1))
  if (any(linked)) {
    stop(
      "The release control directory contains a symbolic link: ",
      entries[[which(linked)[[1L]]]],
      ". Nothing was removed.",
      call. = FALSE
    )
  }
  invisible(control)
}

.builder_release_ensure_control <- function(target) {
  control <- builder_release_control_path(target)
  if (!dir.exists(control)) {
    if (
      .builder_release_exists(control) || !dir.create(control, mode = "0700")
    ) {
      stop("The release control directory could not be created.", call. = FALSE)
    }
  }
  .builder_release_assert_control(control)
  control
}

.builder_release_owner <- function(token) {
  list(
    schema_version = 1L,
    token = token,
    host = .builder_release_host(),
    pid = as.integer(Sys.getpid()),
    acquired_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
}

.builder_release_acquire_lock <- function(control, token) {
  lock <- file.path(control, "lock")
  if (!dir.create(lock, mode = "0700", showWarnings = FALSE)) {
    stop(
      "Another coordinator owns this release, or recovery is required.",
      call. = FALSE
    )
  }
  owner_path <- file.path(lock, "owner.rds")
  owner <- .builder_release_owner(token)
  tryCatch(
    .builder_release_atomic_rds(owner, owner_path, token),
    error = function(error) {
      entries <- list.files(lock, all.files = TRUE, no.. = TRUE)
      if (!length(entries)) {
        unlink(lock, recursive = TRUE, force = TRUE)
      }
      stop(error)
    }
  )
  lock
}

.builder_release_lock_owner <- function(lock) {
  owner <- .builder_release_read_rds(
    file.path(lock, "owner.rds"),
    "The release lock owner"
  )
  valid <- is.list(owner) &&
    identical(owner$schema_version, 1L) &&
    .builder_release_text(owner$token) &&
    .builder_release_text(owner$host) &&
    is.integer(owner$pid) &&
    length(owner$pid) == 1L &&
    !is.na(owner$pid)
  if (!valid) {
    stop("The release lock owner is invalid.", call. = FALSE)
  }
  owner
}

.builder_release_assert_lock <- function(lock, token) {
  if (!dir.exists(lock) || .builder_release_link(lock)) {
    stop("The coordinator no longer owns the release lock.", call. = FALSE)
  }
  owner <- .builder_release_lock_owner(lock)
  if (!identical(owner$token, token)) {
    stop("The coordinator release lock identity changed.", call. = FALSE)
  }
  invisible(owner)
}

.builder_release_lock_known <- function(lock) {
  entries <- list.files(lock, all.files = TRUE, no.. = TRUE)
  identical(entries, "owner.rds")
}

.builder_release_release_lock <- function(control, lock, token) {
  .builder_release_assert_lock(lock, token)
  if (!.builder_release_lock_known(lock)) {
    stop(
      "The release lock contains unknown files and was preserved.",
      call. = FALSE
    )
  }
  isolated <- file.path(control, paste0(".released-lock-", token))
  if (.builder_release_exists(isolated) || !file.rename(lock, isolated)) {
    stop("The release lock could not be isolated safely.", call. = FALSE)
  }
  unlink(isolated, recursive = TRUE, force = TRUE)
  !dir.exists(lock)
}

.builder_release_journal_path <- function(control) {
  file.path(control, "journal.rds")
}

.builder_release_write_phase <- function(handle, phase, detail = NULL) {
  journal <- handle$record
  journal$phase <- phase
  journal$updated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  journal$detail <- detail
  .builder_release_atomic_rds(journal, handle$journal, handle$token)
  handle$record <- journal
  handle
}

.builder_release_journal <- function(target, required = FALSE) {
  control <- builder_release_control_path(target)
  path <- .builder_release_journal_path(control)
  if (!file.exists(path)) {
    if (required) {
      stop("The release journal is missing.", call. = FALSE)
    }
    return(NULL)
  }
  journal <- .builder_release_read_rds(path, "The release journal")
  valid <- is.list(journal) &&
    identical(journal$schema_version, 1L) &&
    .builder_release_text(journal$target) &&
    .builder_release_text(journal$control) &&
    .builder_release_text(journal$build_id) &&
    .builder_release_text(journal$token) &&
    .builder_release_text(journal$phase) &&
    identical(
      .canonicalTargetPath(journal$target),
      .builder_release_path(target)
    ) &&
    identical(.canonicalTargetPath(journal$control), control)
  if (!valid) {
    stop("The release journal is invalid.", call. = FALSE)
  }
  journal
}

builder_discover_recovery <- function(target) {
  target <- .builder_release_path(target)
  control <- builder_release_control_path(target)
  if (!dir.exists(control)) {
    return(list(state = "ready", target = target, backup = NULL))
  }
  .builder_release_assert_control(control)
  journal <- .builder_release_journal(target)
  lock <- file.path(control, "lock")
  if (
    !is.null(journal) &&
      identical(journal$phase, "complete") &&
      dir.exists(lock)
  ) {
    return(list(
      state = "stale_lock",
      target = target,
      control = control,
      backup = NULL,
      journal = journal
    ))
  }
  if (
    is.null(journal) || journal$phase %in% c("complete", "aborted", "recovered")
  ) {
    return(list(
      state = "ready",
      target = target,
      backup = NULL,
      journal = journal
    ))
  }
  backup <- .builder_release_or(journal$backup, file.path(control, "backup"))
  list(
    state = "recovery_required",
    target = target,
    control = control,
    stage = journal$stage,
    backup = backup,
    phase = journal$phase,
    journal = journal,
    message = paste0(
      "Release recovery is required. Preserved backup: ",
      backup
    )
  )
}

builder_release_cleanup_control <- function(target) {
  target <- .builder_release_path(target)
  if (.builder_release_exists(target)) {
    stop(
      "Release control data cannot be removed while its target exists.",
      call. = FALSE
    )
  }
  control <- builder_release_control_path(target)
  if (!dir.exists(control)) {
    return(TRUE)
  }
  .builder_release_assert_control(control)
  recovery <- builder_discover_recovery(target)
  journal <- recovery$journal
  if (
    !identical(recovery$state, "ready") ||
      !is.list(journal) ||
      !identical(journal$phase, "complete")
  ) {
    stop(
      "Incomplete release control data was preserved for recovery.",
      call. = FALSE
    )
  }
  entries <- list.files(control, all.files = TRUE, no.. = TRUE)
  if (!setequal(entries, c("journal.rds", "stages"))) {
    stop(
      "Release control data contains entries that must be preserved.",
      call. = FALSE
    )
  }
  stages <- file.path(control, "stages")
  if (
    !dir.exists(stages) ||
      .builder_release_link(stages) ||
      length(list.files(stages, all.files = TRUE, no.. = TRUE))
  ) {
    stop(
      "Release stage data was preserved because it is not empty and safe.",
      call. = FALSE
    )
  }
  unlink(stages, recursive = TRUE, force = TRUE)
  unlink(file.path(control, "journal.rds"), force = TRUE)
  if (length(list.files(control, all.files = TRUE, no.. = TRUE))) {
    stop("Release control data changed during cleanup.", call. = FALSE)
  }
  unlink(control, recursive = TRUE, force = TRUE)
  !dir.exists(control)
}

builder_prepare_release <- function(
  target,
  build_id,
  expected_prior = NULL,
  expected_prior_state = NULL
) {
  target <- .builder_release_path(target)
  if (!.builder_release_text(build_id)) {
    stop("A release build id is required.", call. = FALSE)
  }
  stage_id <- gsub("[^A-Za-z0-9._-]", "-", build_id)
  stage_id <- .portableBundlePath(stage_id, "The release build id")
  control <- .builder_release_ensure_control(target)
  recovery <- builder_discover_recovery(target)
  if (identical(recovery$state, "stale_lock")) {
    .builder_release_isolate_stale_lock(control, recovery$journal)
    recovery <- builder_discover_recovery(target)
  }
  completed_journal <- recovery$journal
  completed_backup <- if (is.null(completed_journal)) {
    NULL
  } else {
    completed_journal$backup
  }
  if (
    identical(recovery$state, "ready") &&
      .builder_release_text(completed_backup) &&
      dir.exists(completed_backup)
  ) {
    backup_identity <- builder_release_identity(completed_backup)
    if (!identical(backup_identity, completed_journal$expected_prior)) {
      stop(
        "A completed release has an unrecognized preserved backup.",
        call. = FALSE
      )
    }
    unlink(completed_backup, recursive = TRUE, force = TRUE)
    if (dir.exists(completed_backup)) {
      stop("The completed release backup could not be removed.", call. = FALSE)
    }
  }
  if (identical(recovery$state, "recovery_required")) {
    stop(recovery$message, call. = FALSE)
  }
  token <- .builder_release_token("release")
  lock <- .builder_release_acquire_lock(control, token)
  prepared <- FALSE
  on.exit(
    {
      if (!prepared && dir.exists(lock)) {
        try(.builder_release_release_lock(control, lock, token), silent = TRUE)
      }
    },
    add = TRUE
  )
  stages <- file.path(control, "stages")
  if (!dir.exists(stages) && !dir.create(stages, mode = "0700")) {
    stop("The release stage registry could not be created.", call. = FALSE)
  }
  if (
    .builder_release_link(stages) || !.builder_release_mode_owner_only(stages)
  ) {
    stop("The release stage registry is unsafe.", call. = FALSE)
  }
  stage <- file.path(stages, paste0(stage_id, "-", token))
  if (.builder_release_exists(stage) || !dir.create(stage, mode = "0700")) {
    stop("The assigned release stage could not be created.", call. = FALSE)
  }
  if (is.null(expected_prior)) {
    expected_prior <- builder_release_identity(target)
  }
  if (!.builder_release_identity_valid(expected_prior)) {
    unlink(stage, recursive = TRUE, force = TRUE)
    stop("The expected prior release identity is invalid.", call. = FALSE)
  }
  if (is.null(expected_prior_state)) {
    expected_prior_state <- builder_release_state(target, exact_record = FALSE)
  }
  if (
    !is.list(expected_prior_state) ||
      !identical(expected_prior_state$schema_version, 1L) ||
      !identical(expected_prior_state$identity, expected_prior)
  ) {
    unlink(stage, recursive = TRUE, force = TRUE)
    stop("The expected prior release state is invalid.", call. = FALSE)
  }
  journal <- .builder_release_journal_path(control)
  backup <- file.path(control, "backup")
  if (.builder_release_exists(backup)) {
    unlink(stage, recursive = TRUE, force = TRUE)
    stop("A preserved release backup requires recovery.", call. = FALSE)
  }
  record <- list(
    schema_version = 1L,
    target = target,
    control = control,
    stage = stage,
    backup = backup,
    journal = journal,
    lock = lock,
    build_id = build_id,
    token = token,
    host = .builder_release_host(),
    pid = as.integer(Sys.getpid()),
    expected_prior = expected_prior,
    expected_prior_state = expected_prior_state,
    phase = "prepared",
    updated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    detail = NULL
  )
  .builder_release_atomic_rds(record, journal, token)
  prepared <- TRUE
  structure(
    c(record, list(record = record)),
    class = c("builder_release_handle", "list")
  )
}

.builder_release_handle <- function(handle) {
  if (!inherits(handle, "builder_release_handle")) {
    stop("A release handle is required.", call. = FALSE)
  }
  .builder_release_assert_lock(handle$lock, handle$token)
  journal <- .builder_release_journal(handle$target, required = TRUE)
  if (!identical(journal$token, handle$token)) {
    stop("The release journal identity changed.", call. = FALSE)
  }
  handle$record <- journal
  handle
}

.builder_release_restore <- function(handle, detail, .move = file.rename) {
  target_exists <- .builder_release_exists(handle$target)
  backup_exists <- dir.exists(handle$backup)
  if (target_exists) {
    if (.builder_release_exists(handle$stage)) {
      handle <- .builder_release_write_phase(
        handle,
        "recovery_required",
        detail
      )
      return(list(handle = handle, restored = FALSE))
    }
    if (!isTRUE(.move(handle$target, handle$stage))) {
      handle <- .builder_release_write_phase(
        handle,
        "recovery_required",
        detail
      )
      return(list(handle = handle, restored = FALSE))
    }
  }
  if (backup_exists && !isTRUE(.move(handle$backup, handle$target))) {
    handle <- .builder_release_write_phase(handle, "recovery_required", detail)
    return(list(handle = handle, restored = FALSE))
  }
  handle <- .builder_release_write_phase(handle, "prepared", detail)
  list(handle = handle, restored = TRUE)
}

builder_publish_release <- function(
  handle,
  .move = file.rename,
  .after_phase = function(phase) invisible(NULL),
  .after_move = function(move) invisible(NULL),
  .verify_payload = function(root, phase) TRUE
) {
  handle <- .builder_release_handle(handle)
  if (!identical(handle$record$phase, "prepared")) {
    stop("The release is not in its prepared phase.", call. = FALSE)
  }
  if (!dir.exists(handle$stage) || .builder_release_link(handle$stage)) {
    stop("The assigned release stage is missing or unsafe.", call. = FALSE)
  }
  handle$record$prepared_identity <- builder_release_identity(handle$stage)
  if (
    !is.null(handle$expected_stage_identity) &&
      !identical(
        handle$record$prepared_identity,
        handle$expected_stage_identity
      )
  ) {
    stop("The verified stage changed before publication.", call. = FALSE)
  }
  handle <- .builder_release_write_phase(handle, "locked")
  current_state <- builder_release_state(
    handle$target,
    exact_record = FALSE,
    allow_abandoned = isTRUE(
      handle$expected_prior_state$record$abandoned
    )
  )
  current <- current_state$identity
  if (
    !identical(current, handle$expected_prior) ||
      !identical(current_state, handle$expected_prior_state)
  ) {
    stop(
      "The release changed after Review; nothing was published.",
      call. = FALSE
    )
  }
  if (isTRUE(current$exists)) {
    moved <- tryCatch(
      .move(handle$target, handle$backup),
      error = function(error) FALSE
    )
    if (!isTRUE(moved)) {
      handle <- .builder_release_write_phase(
        handle,
        "prepared",
        "The prior release could not be protected."
      )
      stop("The prior release could not be protected.", call. = FALSE)
    }
    .after_move("old_to_backup")
  }
  handle <- .builder_release_write_phase(handle, "old_moved")
  .after_phase("old_moved")
  verified <- tryCatch(
    identical(.verify_payload(handle$stage, "before_rename"), TRUE),
    error = function(error) FALSE
  )
  if (!isTRUE(verified)) {
    restored <- .builder_release_restore(
      handle,
      "Publication verification failed; the prior release was restored.",
      .move = .move
    )
    if (!isTRUE(restored$restored)) {
      stop("Publication verification and restoration failed.", call. = FALSE)
    }
    stop(
      "Publication verification failed; the prior release was restored.",
      call. = FALSE
    )
  }
  moved <- tryCatch(
    .move(handle$stage, handle$target),
    error = function(error) FALSE
  )
  if (!isTRUE(moved)) {
    restored <- .builder_release_restore(
      handle,
      paste0("Publication failed. Preserved backup: ", handle$backup),
      .move = .move
    )
    if (!isTRUE(restored$restored)) {
      stop(
        "Publication and restoration failed. Preserved backup: ",
        handle$backup,
        call. = FALSE
      )
    }
    stop("Publication failed; the prior release was restored.", call. = FALSE)
  }
  .after_move("new_to_target")
  verified <- tryCatch(
    identical(.verify_payload(handle$target, "after_rename"), TRUE),
    error = function(error) FALSE
  )
  if (!isTRUE(verified)) {
    restored <- .builder_release_restore(
      handle,
      "Publication verification failed; the prior release was restored.",
      .move = .move
    )
    if (!isTRUE(restored$restored)) {
      stop("Publication verification and restoration failed.", call. = FALSE)
    }
    stop(
      "Publication verification failed; the prior release was restored.",
      call. = FALSE
    )
  }
  handle <- .builder_release_write_phase(handle, "new_published")
  .after_phase("new_published")
  handle <- .builder_release_write_phase(handle, "complete")
  .after_phase("complete")
  .builder_release_release_lock(handle$control, handle$lock, handle$token)
  warning <- NULL
  if (dir.exists(handle$backup)) {
    unlink(handle$backup, recursive = TRUE, force = TRUE)
    if (dir.exists(handle$backup)) {
      warning <- paste0(
        "The release was published, but its prior backup remains: ",
        handle$backup
      )
    }
  }
  list(
    error = NULL,
    published = TRUE,
    target = handle$target,
    identity = builder_release_identity(handle$target),
    journal = handle$journal,
    warning = warning
  )
}

builder_abort_release <- function(handle) {
  handle <- .builder_release_handle(handle)
  if (
    handle$record$phase %in%
      c("old_moved", "new_published", "recovery_required")
  ) {
    stop("This release requires recovery and cannot be aborted.", call. = FALSE)
  }
  if (dir.exists(handle$stage)) {
    unlink(handle$stage, recursive = TRUE, force = TRUE)
  }
  handle <- .builder_release_write_phase(handle, "aborted")
  .builder_release_release_lock(handle$control, handle$lock, handle$token)
  list(aborted = TRUE, target = handle$target)
}

.builder_release_pid_alive <- function(pid) {
  isTRUE(tryCatch(tools::pskill(pid, signal = 0L), error = function(error) {
    FALSE
  }))
}

.builder_release_isolate_stale_lock <- function(control, journal) {
  lock <- file.path(control, "lock")
  owner <- .builder_release_lock_owner(lock)
  recoverable <- identical(owner$token, journal$token) &&
    identical(owner$host, .builder_release_host()) &&
    !.builder_release_pid_alive(owner$pid) &&
    .builder_release_lock_known(lock)
  if (!recoverable) {
    stop(
      "The release lock is not safely recoverable; manual inspection is required.",
      call. = FALSE
    )
  }
  isolated <- file.path(control, paste0(".stale-lock-", owner$token))
  if (.builder_release_exists(isolated) || !file.rename(lock, isolated)) {
    stop("The stale release lock could not be isolated.", call. = FALSE)
  }
  unlink(isolated, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}

builder_recover_release <- function(target, action = c("restore", "abort")) {
  action <- match.arg(action)
  recovery <- builder_discover_recovery(target)
  if (!identical(recovery$state, "recovery_required")) {
    return(list(recovered = FALSE, state = recovery$state))
  }
  journal <- recovery$journal
  control <- recovery$control
  lock <- file.path(control, "lock")
  if (dir.exists(lock)) {
    .builder_release_isolate_stale_lock(control, journal)
  }
  token <- .builder_release_token("recovery")
  recovery_lock <- .builder_release_acquire_lock(control, token)
  on.exit(
    {
      if (dir.exists(recovery_lock)) {
        try(
          .builder_release_release_lock(control, recovery_lock, token),
          silent = TRUE
        )
      }
    },
    add = TRUE
  )
  journal$recovery_from <- if (
    startsWith(journal$phase, "recovering_") &&
      .builder_release_text(journal$recovery_from)
  ) {
    journal$recovery_from
  } else {
    journal$phase
  }
  journal$phase <- paste0("recovering_", action)
  journal$detail <- paste0("Recovery action started: ", action)
  journal$updated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  journal$token <- token
  journal$host <- .builder_release_host()
  journal$pid <- as.integer(Sys.getpid())
  journal$lock <- recovery_lock
  .builder_release_atomic_rds(
    journal,
    .builder_release_journal_path(control),
    token
  )
  target <- recovery$target
  backup <- recovery$backup
  if (identical(action, "restore")) {
    if (!dir.exists(backup)) {
      already_restored <-
        .builder_release_exists(target) &&
        identical(
          builder_release_identity(target),
          journal$expected_prior
        )
      if (!already_restored) {
        stop("The preserved release backup is missing.", call. = FALSE)
      }
    }
    if (dir.exists(backup) && .builder_release_exists(target)) {
      published_identity <- journal$prepared_identity
      owned_published <-
        journal$recovery_from %in%
        c("old_moved", "new_published") &&
        .builder_release_identity_valid(published_identity) &&
        identical(builder_release_identity(target), published_identity) &&
        !.builder_release_exists(journal$stage)
      if (!owned_published || !file.rename(target, journal$stage)) {
        stop(
          "The release target is occupied; the backup was preserved.",
          call. = FALSE
        )
      }
    }
    if (dir.exists(backup) && !file.rename(backup, target)) {
      stop("The preserved release backup could not be restored.", call. = FALSE)
    }
  } else if (dir.exists(backup)) {
    stop(
      "A preserved prior release cannot be discarded by abort.",
      call. = FALSE
    )
  }
  stage <- journal$stage
  if (dir.exists(stage) && .pathWithin(stage, file.path(control, "stages"))) {
    unlink(stage, recursive = TRUE, force = TRUE)
  }
  journal$phase <- "recovered"
  journal$detail <- paste0("Recovery action: ", action)
  journal$recovery_from <- NULL
  journal$updated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  journal$token <- token
  journal$lock <- recovery_lock
  .builder_release_atomic_rds(
    journal,
    .builder_release_journal_path(control),
    token
  )
  .builder_release_release_lock(control, recovery_lock, token)
  list(recovered = TRUE, action = action, target = target)
}
