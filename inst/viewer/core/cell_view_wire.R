## Shared binary codec for all Canvas cell views.
##
## Keep this module free of Shiny/session state so packing can be profiled and
## optimized independently from page-specific bundle construction.

cv_wire_integer_type <- function(values) {
  if (anyNA(values)) {
    return("i32")
  }
  bounds <- range(values)
  if (bounds[[1L]] >= -128L && bounds[[2L]] <= 127L) {
    "i8"
  } else if (bounds[[1L]] >= -32768L && bounds[[2L]] <= 32767L) {
    "i16"
  } else {
    "i32"
  }
}

cv_wire_pack_bundle <- function(
  bundle,
  min_length = 4096L,
  include_cells = TRUE
) {
  if (!isTRUE(include_cells)) {
    bundle$cells <- NULL
  }
  cv_wire_pack_message(bundle, min_length = min_length)
}

cv_wire_pack_cells <- function(dataset_id, cells) {
  dataset <- charToRaw(enc2utf8(as.character(dataset_id)))
  values <- charToRaw(enc2utf8(as.character(jsonlite::toJSON(
    as.character(cells),
    auto_unbox = FALSE,
    na = "null"
  ))))
  c(
    writeBin(as.integer(length(dataset)), raw(), size = 4L, endian = "little"),
    dataset,
    values
  )
}

cv_wire_pack_message <- function(message, min_length = 4096L) {
  chunks <- list()
  data_size <- 0L
  pack <- function(values, type) {
    bytes <- if (identical(type, "json")) {
      charToRaw(enc2utf8(as.character(jsonlite::toJSON(
        as.character(values),
        auto_unbox = FALSE,
        na = "null"
      ))))
    } else {
      size <- switch(type, i8 = 1L, i16 = 2L, i32 = 4L, f32 = 4L, f64 = 8L)
      writeBin(
        if (startsWith(type, "i")) as.integer(values) else as.numeric(values),
        raw(),
        size = size,
        endian = "little"
      )
    }
    alignment <- switch(
      type,
      i8 = 1L,
      i16 = 2L,
      i32 = 4L,
      f32 = 4L,
      f64 = 8L,
      1L
    )
    offset <- data_size + ((alignment - data_size %% alignment) %% alignment)
    chunks[[length(chunks) + 1L]] <<- list(offset = offset, bytes = bytes)
    data_size <<- offset + length(bytes)
    list(
      `__cv_wire__` = type,
      length = length(values),
      offset = offset,
      bytes = length(bytes)
    )
  }
  walk <- function(value, field = NULL) {
    if (is.list(value)) {
      value_names <- names(value)
      packed <- lapply(seq_along(value), function(index) {
        child_field <- if (
          !is.null(value_names) && nzchar(value_names[[index]])
        ) {
          value_names[[index]]
        } else {
          field
        }
        walk(value[[index]], child_field)
      })
      names(packed) <- value_names
      return(packed)
    }
    if (
      !is.atomic(value) || length(value) <= 1L || length(value) < min_length
    ) {
      return(value)
    }
    if (is.factor(value) || is.character(value)) {
      return(pack(as.character(value), "json"))
    }
    if (is.integer(value)) {
      return(pack(value, cv_wire_integer_type(value)))
    }
    if (is.numeric(value)) {
      return(pack(
        value,
        if (
          field %in%
            c(
              "x",
              "y",
              "z",
              "from_x",
              "from_y",
              "to_x",
              "to_y",
              "point_sizes",
              "color",
              "r",
              "g",
              "b"
            )
        ) {
          "f32"
        } else {
          "f64"
        }
      ))
    }
    value
  }

  message <- walk(message)
  message$wire_format <- "binary-v1"
  header <- charToRaw(enc2utf8(as.character(jsonlite::toJSON(
    message,
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  ))))
  header_padding <- (4L - length(header) %% 4L) %% 4L
  data_start <- 4L + length(header) + header_padding
  payload <- raw(data_start + data_size)
  payload[seq_len(4L)] <- writeBin(
    as.integer(length(header)),
    raw(),
    size = 4L,
    endian = "little"
  )
  payload[4L + seq_along(header)] <- header
  for (chunk in chunks) {
    first <- data_start + chunk$offset + 1L
    payload[seq.int(first, length.out = length(chunk$bytes))] <- chunk$bytes
  }
  payload
}
