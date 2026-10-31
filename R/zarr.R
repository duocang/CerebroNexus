## A small Zarr reader for AnnData stores ------------------------------------------
##
## AnnData can be stored as Zarr (`.zarr`), and SpatialData keeps its cell table
## that way. The reader in h5ad.R is written against four primitives (list the
## nodes, read an array, read attributes, group or array?), so a Zarr store only
## has to provide those: `.h5adContext()` hands back a context of kind "zarr",
## and `.h5adRead()` / `.h5adAttrs()` dispatch to the functions below.
##
## Zarr v3 (what zarr-python 3 and SpatialData 0.7 write) and v2 (zarr-python 2,
## anndata before 0.12) are read, with the codecs bytes, zstd, gzip, crc32c,
## sharding, the variable-length string codec, and Blosc chunks compressed with
## lz4, zstd or zlib (plain or byte-shuffled; blosclz, snappy and the bit
## shuffle say so). Any other codec stops with its name, and so does an array
## with a transpose codec (Rarr writes one for arrays of two dimensions).
##
## Blosc chunks are decoded in R: zstd and zlib at the speed of those
## libraries, lz4 (the numcodecs default) at about 4 MB/s of output. Re-save a
## large store with zstd, or use an .h5ad file.
##
## Why not anndataR (the scverse reader)? Tried on the fixtures of this package
## and on a 300,000 cell h5ad: it cannot read AIRR (awkward) arrays or `.h5mu`,
## returns nothing for sharded Zarr v3 stores (and so for the SpatialData
## tables), and read the large h5ad about 2.2 times slower than the reader in
## h5ad.R at the same memory. Revisit once anndataR reads sharded Zarr.
##
## Arrays are returned the way rhdf5 returns HDF5 datasets: a C-order array of
## shape (n_obs, n_dims) comes back as an R array of dim (n_dims, n_obs), which is
## what every reader in h5ad.R expects.

#' Is this path a Zarr store (a directory with Zarr metadata)?
#' @keywords internal
#' @noRd
.zarrIsStore <- function(path) {
  is.character(path) &&
    length(path) == 1L &&
    dir.exists(path) &&
    any(file.exists(file.path(path, c("zarr.json", ".zgroup", ".zarray"))))
}

#' Context of a Zarr store, rooted at `root` (e.g. "tables/table").
#' @keywords internal
#' @noRd
.zarrContext <- function(file, root = NULL) {
  base <- if (is.null(root)) file else file.path(file, root)
  if (!dir.exists(base)) {
    stop("Zarr node not found: ", base, call. = FALSE)
  }
  base <- normalizePath(base, winslash = "/", mustWork = TRUE)
  v3 <- list.files(
    base,
    pattern = "^zarr\\.json$",
    recursive = TRUE,
    all.files = TRUE
  )
  v2 <- list.files(
    base,
    pattern = "^\\.(zarray|zgroup)$",
    recursive = TRUE,
    all.files = TRUE
  )
  files <- c(v3, v2)
  if (!length(files)) {
    stop("`", base, "` holds no Zarr metadata.", call. = FALSE)
  }
  dirs <- dirname(files)
  keep <- dirs != "."
  files <- files[keep]
  dirs <- dirs[keep]
  otype <- vapply(
    seq_along(files),
    function(i) {
      name <- basename(files[[i]])
      if (identical(name, ".zarray")) {
        return("H5I_DATASET")
      }
      if (identical(name, ".zgroup")) {
        return("H5I_GROUP")
      }
      text <- readChar(file.path(base, files[[i]]), 4096L, useBytes = TRUE)
      if (grepl('"node_type"\\s*:\\s*"array"', text)) {
        "H5I_DATASET"
      } else {
        "H5I_GROUP"
      }
    },
    character(1)
  )
  unique_dirs <- !duplicated(dirs)
  list(
    kind = "zarr",
    file = file,
    base = base,
    path = paste0("/", dirs[unique_dirs]),
    otype = otype[unique_dirs],
    prefix = ""
  )
}

#' Metadata of one node: list(version, node_type, meta, attributes).
#' @keywords internal
#' @noRd
.zarrNode <- function(directory) {
  v3 <- file.path(directory, "zarr.json")
  if (file.exists(v3)) {
    meta <- jsonlite::fromJSON(v3, simplifyVector = FALSE)
    return(list(
      version = 3L,
      meta = meta,
      attributes = meta$attributes %||% list()
    ))
  }
  zarray <- file.path(directory, ".zarray")
  zattrs <- file.path(directory, ".zattrs")
  attributes <- if (file.exists(zattrs)) {
    jsonlite::fromJSON(zattrs, simplifyVector = FALSE)
  } else {
    list()
  }
  if (file.exists(zarray)) {
    return(list(
      version = 2L,
      meta = jsonlite::fromJSON(zarray, simplifyVector = FALSE),
      attributes = attributes
    ))
  }
  if (file.exists(file.path(directory, ".zgroup"))) {
    return(list(version = 2L, meta = list(), attributes = attributes))
  }
  stop("No Zarr metadata in ", directory, call. = FALSE)
}

#' Attributes as the plain structures jsonlite gives (vectors where possible).
#' @keywords internal
#' @noRd
.zarrAttrs <- function(ctx, path) {
  directory <- file.path(ctx$base, sub("^/", "", path))
  node <- tryCatch(.zarrNode(directory), error = function(e) NULL)
  if (is.null(node) || !length(node$attributes)) {
    return(list())
  }
  jsonlite::fromJSON(
    jsonlite::toJSON(node$attributes, auto_unbox = TRUE, null = "null"),
    simplifyVector = TRUE
  )
}

#' Read a whole array. See the header for the shape convention.
#' @keywords internal
#' @noRd
.zarrRead <- function(ctx, path, ...) {
  directory <- file.path(ctx$base, sub("^/", "", path))
  node <- .zarrNode(directory)
  if (is.null(node$meta$shape)) {
    stop("`", path, "` is a Zarr group, not an array.", call. = FALSE)
  }
  .zarrReadArray(directory, node)
}

.zarrDtype <- function(node) {
  if (node$version == 3L) {
    return(as.character(node$meta$data_type))
  }
  dtype <- as.character(node$meta$dtype)
  big <- startsWith(dtype, ">")
  code <- sub("^[<>|=]", "", dtype)
  names <- c(
    i1 = "int8",
    i2 = "int16",
    i4 = "int32",
    i8 = "int64",
    u1 = "uint8",
    u2 = "uint16",
    u4 = "uint32",
    u8 = "uint64",
    f4 = "float32",
    f8 = "float64",
    b1 = "bool",
    O = "string"
  )
  fixed <- regmatches(code, regexec("^([US])([0-9]+)$", code))[[1L]]
  if (length(fixed)) {
    ## numpy fixed-width strings: U = UTF-32, S = bytes
    result <- "string"
    attr(result, "big") <- big
    attr(result, "fixed") <- c(kind = fixed[[2L]], width = fixed[[3L]])
    return(result)
  }
  if (!code %in% names(names)) {
    stop("Unsupported Zarr data type `", dtype, "`.", call. = FALSE)
  }
  result <- unname(names[[code]])
  attr(result, "big") <- big
  result
}

.zarrFill <- function(dtype, fill) {
  if (is.null(fill) || identical(as.character(dtype), "string")) {
    return(switch(dtype, string = "", bool = FALSE, 0))
  }
  if (is.character(fill)) {
    return(switch(
      fill,
      "NaN" = NaN,
      "Infinity" = Inf,
      "-Infinity" = -Inf,
      as.numeric(fill)
    ))
  }
  if (identical(as.character(dtype), "bool")) {
    as.logical(fill)
  } else {
    as.numeric(fill)
  }
}

#' Decode the bytes of one chunk into a flat vector (C order, full chunk shape).
#' @keywords internal
#' @noRd
.zarrDecodeChunk <- function(bytes, node, dtype, chunk_shape) {
  n <- prod(chunk_shape)
  endian <- "little"
  string <- identical(as.character(dtype), "string")
  if (node$version == 3L) {
    codecs <- node$meta$codecs
    names <- vapply(
      codecs,
      function(codec) as.character(codec$name),
      character(1)
    )
    for (i in rev(seq_along(codecs))) {
      name <- names[[i]]
      if (name %in% c("bytes", "vlen-utf8", "vlen-bytes", "transpose")) {
        if (identical(name, "bytes")) {
          endian <- codecs[[i]]$configuration$endian %||% "little"
        }
        next
      }
      bytes <- switch(
        name,
        zstd = qs2::zstd_decompress_raw(bytes),
        gzip = memDecompress(bytes, type = "gzip"),
        crc32c = bytes[seq_len(length(bytes) - 4L)],
        blosc = .zarrBlosc(bytes),
        stop("Unsupported Zarr codec `", name, "`.", call. = FALSE)
      )
    }
  } else {
    if (isTRUE(attr(dtype, "big"))) {
      endian <- "big"
    }
    compressor <- node$meta$compressor
    if (!is.null(compressor)) {
      bytes <- switch(
        as.character(compressor$id),
        zstd = qs2::zstd_decompress_raw(bytes),
        gzip = ,
        zlib = memDecompress(bytes, type = "gzip"),
        blosc = .zarrBlosc(bytes),
        stop(
          "Unsupported Zarr compressor `",
          compressor$id,
          "`.",
          call. = FALSE
        )
      )
    }
  }
  if (string) {
    fixed <- attr(dtype, "fixed")
    if (!is.null(fixed)) {
      return(.zarrFixedStrings(
        bytes,
        n,
        kind = fixed[["kind"]],
        width = as.integer(fixed[["width"]]),
        endian = endian
      ))
    }
    return(.zarrVlenStrings(bytes, n))
  }
  .zarrBytesToVector(bytes, dtype, endian, n)
}

#' numpy fixed-width strings (`<U7`, `|S5`): n items of `width` UTF-32 code
#' points or bytes, padded with zeros.
#' @keywords internal
#' @noRd
.zarrFixedStrings <- function(bytes, n, kind, width, endian = "little") {
  if (n == 0L || width == 0L) {
    return(rep("", n))
  }
  if (identical(kind, "U")) {
    codes <- readBin(
      bytes,
      "integer",
      n = n * width,
      size = 4L,
      endian = endian
    )
    codes <- matrix(codes, nrow = width)
    return(vapply(
      seq_len(n),
      function(i) {
        code <- codes[, i]
        stop_at <- match(0L, code, nomatch = width + 1L) - 1L
        intToUtf8(code[seq_len(stop_at)])
      },
      character(1)
    ))
  }
  letters <- matrix(bytes[seq_len(n * width)], nrow = width)
  result <- vapply(
    seq_len(n),
    function(i) {
      item <- letters[, i]
      keep <- which(item != as.raw(0L))
      if (length(keep)) rawToChar(item[seq_len(max(keep))]) else ""
    },
    character(1)
  )
  ## bytes of unknown encoding: UTF-8 when they are, else read as latin1
  invalid <- !validUTF8(result)
  Encoding(result) <- "UTF-8"
  if (any(invalid)) {
    result[invalid] <- iconv(result[invalid], "latin1", "UTF-8")
  }
  result
}

#' LZ4 block format (what Blosc stores): sequences of a literal run and a match.
#' @keywords internal
#' @noRd
.zarrLz4Block <- function(source, size) {
  out <- raw(size)
  last <- length(source)
  ip <- 1L
  op <- 1L
  extend <- function(value) {
    repeat {
      extra <- as.integer(source[[ip]])
      ip <<- ip + 1L
      value <- value + extra
      if (extra != 255L) {
        return(value)
      }
    }
  }
  while (ip <= last) {
    token <- as.integer(source[[ip]])
    ip <- ip + 1L
    literals <- bitwShiftR(token, 4L)
    if (literals == 15L) {
      literals <- extend(literals)
    }
    if (literals > 0L) {
      out[op:(op + literals - 1L)] <- source[ip:(ip + literals - 1L)]
      ip <- ip + literals
      op <- op + literals
    }
    if (ip > last) {
      break
    }
    offset <- as.integer(source[[ip]]) + 256L * as.integer(source[[ip + 1L]])
    ip <- ip + 2L
    length <- bitwAnd(token, 15L)
    if (length == 15L) {
      length <- extend(length)
    }
    length <- length + 4L
    from <- op - offset
    out[op:(op + length - 1L)] <- if (offset >= length) {
      out[from:(from + length - 1L)]
    } else {
      rep_len(out[from:(op - 1L)], length)
    }
    op <- op + length
  }
  out
}

#' Decode one Blosc chunk (a c-blosc 1 frame, what numcodecs and the Zarr v3
#' blosc codec write): a 16-byte header, the start of every block, and per block
#' one or more compressed streams. The frame describes itself, so the codec's
#' own configuration is not needed. Streams compressed with lz4, zstd or zlib,
#' with no shuffle or the byte shuffle, are read; anything else stops with its
#' name.
#' @keywords internal
#' @noRd
.zarrBlosc <- function(bytes) {
  if (length(bytes) < 16L) {
    stop("A Blosc chunk is shorter than its header.", call. = FALSE)
  }
  if (as.integer(bytes[[1L]]) != 2L) {
    stop(
      "Unsupported Blosc format version ",
      as.integer(bytes[[1L]]),
      " (only the c-blosc 1 format is read).",
      call. = FALSE
    )
  }
  flags <- as.integer(bytes[[3L]])
  typesize <- as.integer(bytes[[4L]])
  sizes <- readBin(bytes[5:16], "integer", n = 3L, size = 4L, endian = "little")
  nbytes <- sizes[[1L]]
  blocksize <- sizes[[2L]]
  if (length(bytes) < sizes[[3L]]) {
    ## R would read past the end as zeros, which a matrix reads as "not
    ## expressed"
    stop(
      "A Blosc chunk is truncated: its header says ",
      sizes[[3L]],
      " bytes and the file has ",
      length(bytes),
      ".",
      call. = FALSE
    )
  }
  if (nbytes == 0L) {
    return(raw())
  }
  if (bitwAnd(flags, 2L)) {
    return(bytes[17:(16L + nbytes)])
  }
  compressor <- c("blosclz", "lz4", "snappy", "zlib", "zstd")[
    bitwAnd(bitwShiftR(flags, 5L), 7L) + 1L
  ]
  if (!compressor %in% c("lz4", "zlib", "zstd")) {
    stop(
      "This Zarr store uses Blosc with the `",
      compressor,
      "` compressor, which is not supported (lz4, zstd and zlib are).",
      call. = FALSE
    )
  }
  if (bitwAnd(flags, 4L)) {
    stop(
      "This Zarr store uses Blosc with the bit shuffle, which is not ",
      "supported (no shuffle and the byte shuffle are).",
      call. = FALSE
    )
  }
  if (bitwAnd(flags, 8L)) {
    stop(
      "This Zarr store uses Blosc with the delta filter, which is not ",
      "supported.",
      call. = FALSE
    )
  }
  shuffled <- bitwAnd(flags, 1L) > 0L && typesize > 1L
  dont_split <- bitwAnd(flags, 16L) > 0L
  nblocks <- as.integer(ceiling(nbytes / blocksize))
  starts <- readBin(
    bytes[17:(16L + 4L * nblocks)],
    "integer",
    n = nblocks,
    size = 4L,
    endian = "little"
  )
  out <- raw(nbytes)
  written <- 0L
  for (block in seq_len(nblocks)) {
    size <- if (block == nblocks) {
      nbytes - blocksize * (nblocks - 1L)
    } else {
      blocksize
    }
    leftover <- size != blocksize
    splits <- if (
      !dont_split &&
        typesize <= 16L &&
        blocksize %/% typesize >= 128L &&
        !leftover
    ) {
      typesize
    } else {
      1L
    }
    stream_size <- size %/% splits
    position <- starts[[block]] + 1L
    pieces <- vector("list", splits)
    for (split in seq_len(splits)) {
      compressed <- readBin(
        bytes[position:(position + 3L)],
        "integer",
        n = 1L,
        size = 4L,
        endian = "little"
      )
      position <- position + 4L
      stream <- bytes[position:(position + compressed - 1L)]
      position <- position + compressed
      pieces[[split]] <- if (compressed == stream_size) {
        stream
      } else {
        switch(
          compressor,
          lz4 = .zarrLz4Block(stream, stream_size),
          zstd = qs2::zstd_decompress_raw(stream),
          zlib = memDecompress(stream, type = "gzip")
        )
      }
    }
    decoded <- unlist(pieces, use.names = FALSE)
    if (shuffled && size >= typesize) {
      count <- size %/% typesize
      main <- count * typesize
      planes <- matrix(decoded[seq_len(main)], nrow = count)
      decoded <- c(as.vector(t(planes)), decoded[-seq_len(main)])
    }
    out[(written + 1L):(written + size)] <- decoded
    written <- written + size
  }
  out
}

#' Numcodecs' variable-length string layout: uint32 count, then (uint32 length,
#' bytes) per item.
#' @keywords internal
#' @noRd
.zarrVlenStrings <- function(bytes, n) {
  if (!length(bytes)) {
    return(rep("", n))
  }
  count <- readBin(bytes[1:4], "integer", size = 4L, endian = "little")
  result <- character(count)
  position <- 5L
  for (i in seq_len(count)) {
    size <- readBin(
      bytes[position:(position + 3L)],
      "integer",
      size = 4L,
      endian = "little"
    )
    position <- position + 4L
    if (size > 0L) {
      result[[i]] <- rawToChar(bytes[position:(position + size - 1L)])
      position <- position + size
    }
  }
  Encoding(result) <- "UTF-8"
  result
}

.zarrBytesToVector <- function(bytes, dtype, endian, n) {
  read <- function(what, size, signed = TRUE) {
    readBin(bytes, what, n = n, size = size, signed = signed, endian = endian)
  }
  switch(
    dtype,
    int8 = read("integer", 1L),
    uint8 = as.integer(bytes),
    int16 = read("integer", 2L),
    uint16 = read("integer", 2L, signed = FALSE),
    int32 = read("integer", 4L),
    uint32 = {
      value <- read("integer", 4L)
      value <- as.numeric(value)
      value[value < 0] <- value[value < 0] + 2^32
      value
    },
    int64 = .zarrInt64(bytes, n, endian, signed = TRUE),
    uint64 = .zarrInt64(bytes, n, endian, signed = FALSE),
    float32 = read("numeric", 4L),
    float64 = read("numeric", 8L),
    bool = as.logical(as.integer(bytes[seq_len(n)])),
    stop("Unsupported Zarr data type `", dtype, "`.", call. = FALSE)
  )
}

#' 64-bit integers as doubles (exact below 2^53), from two 32-bit halves.
#' @keywords internal
#' @noRd
.zarrInt64 <- function(bytes, n, endian, signed) {
  halves <- readBin(bytes, "integer", n = 2L * n, size = 4L, endian = endian)
  first <- halves[seq(1L, 2L * n, by = 2L)]
  second <- halves[seq(2L, 2L * n, by = 2L)]
  low <- if (identical(endian, "little")) first else second
  high <- if (identical(endian, "little")) second else first
  low <- as.numeric(low)
  low[low < 0] <- low[low < 0] + 2^32
  high <- as.numeric(high)
  if (!signed) {
    high[high < 0] <- high[high < 0] + 2^32
  }
  high * 2^32 + low
}

#' Assemble an array from its chunks.
#' @keywords internal
#' @noRd
.zarrReadArray <- function(directory, node) {
  meta <- node$meta
  shape <- as.integer(unlist(meta$shape))
  dtype <- .zarrDtype(node)
  if (node$version == 3L) {
    grid <- meta$chunk_grid
    chunk_shape <- as.integer(unlist(grid$configuration$chunk_shape))
    key <- meta$chunk_key_encoding
    separator <- key$configuration$separator %||%
      if (identical(key$name, "v2")) "." else "/"
    prefix <- if (identical(key$name, "v2")) "" else "c"
    fill <- .zarrFill(dtype, meta$fill_value)
    transpose <- Filter(
      function(codec) identical(codec$name, "transpose"),
      meta$codecs
    )
    if (length(transpose)) {
      stop(
        "Zarr arrays with a transpose codec are not supported.",
        call. = FALSE
      )
    }
  } else {
    chunk_shape <- as.integer(unlist(meta$chunks))
    separator <- meta$dimension_separator %||% "."
    prefix <- ""
    fill <- .zarrFill(dtype, meta$fill_value)
    if (identical(meta$order, "F")) {
      stop("Fortran-order Zarr arrays are not supported.", call. = FALSE)
    }
    if (length(meta$filters)) {
      ids <- vapply(meta$filters, function(f) as.character(f$id), character(1))
      if (!all(ids %in% c("vlen-utf8"))) {
        stop("Unsupported Zarr filter `", ids[[1L]], "`.", call. = FALSE)
      }
    }
  }
  total <- prod(shape)
  mode <- switch(dtype, string = "character", bool = "logical", "numeric")
  flat <- rep(fill, total)
  if (identical(mode, "numeric")) {
    flat <- as.numeric(flat)
  }
  if (!length(shape)) {
    chunk_shape <- integer()
  }
  counts <- if (length(shape)) {
    as.integer(ceiling(shape / chunk_shape))
  } else {
    integer()
  }
  sharding <- if (node$version == 3L) {
    Filter(
      function(codec) identical(codec$name, "sharding_indexed"),
      meta$codecs
    )
  } else {
    list()
  }

  grid_index <- if (length(counts)) {
    as.matrix(expand.grid(lapply(counts, function(n) seq_len(n) - 1L)))
  } else {
    matrix(integer(), nrow = 1L, ncol = 0L)
  }
  for (row in seq_len(nrow(grid_index))) {
    index <- grid_index[row, ]
    name <- if (length(index)) {
      paste(c(if (nzchar(prefix)) prefix, index), collapse = separator)
    } else if (node$version == 3L) {
      prefix
    } else {
      "0"
    }
    file <- file.path(directory, name)
    if (!file.exists(file)) {
      next
    }
    bytes <- readBin(file, "raw", n = file.size(file))
    values <- if (length(sharding)) {
      .zarrDecodeShard(
        bytes,
        sharding[[1L]]$configuration,
        dtype,
        chunk_shape,
        fill
      )
    } else {
      .zarrDecodeChunk(bytes, node, dtype, chunk_shape)
    }
    if (!length(shape)) {
      flat[[1L]] <- values[[1L]]
      next
    }
    ## Assigned in place: passing `flat` to a function copied the whole array
    ## once per chunk (27 s for one 28 M-value array of a Visium store).
    if (length(shape) == 1L) {
      ## a chunk of a 1-d array is one contiguous run: no index vectors
      first <- index * chunk_shape
      n <- min(chunk_shape, shape - first)
      flat[(first + 1):(first + n)] <- if (n < length(values)) {
        values[seq_len(n)]
      } else {
        values
      }
      next
    }
    place <- .zarrPlacement(shape, index * chunk_shape, chunk_shape)
    flat[place$target] <- values[place$source]
  }
  if (length(shape) <= 1L) {
    return(flat)
  }
  array(flat, dim = rev(shape))
}

#' C-order strides of an array shape.
#' @keywords internal
#' @noRd
.zarrStrides <- function(dims) {
  if (!length(dims)) integer() else rev(cumprod(c(1, rev(dims)[-length(dims)])))
}

#' Where the values of one chunk (flat, C order, full chunk shape) go in the flat
#' C-order array of shape `shape` when the chunk starts at `start`: 1-based
#' positions in the array (`target`) and in the chunk (`source`); edge chunks
#' are clipped.
#' @keywords internal
#' @noRd
.zarrPlacement <- function(shape, start, chunk_shape) {
  k <- length(shape)
  array_strides <- .zarrStrides(shape)
  chunk_strides <- .zarrStrides(chunk_shape)
  extent <- pmin(chunk_shape, shape - start)
  inside <- lapply(seq_len(k), function(d) seq_len(extent[[d]]) - 1L)
  if (k == 1L) {
    return(list(
      target = start[[1L]] + inside[[1L]] + 1L,
      source = inside[[1L]] + 1L
    ))
  }
  walk <- function(offsets) {
    Reduce(
      function(acc, d) as.vector(outer(acc, offsets[[d]], "+")),
      rev(seq_len(k - 1L)),
      init = offsets[[k]]
    )
  }
  list(
    target = walk(lapply(seq_len(k), function(d) {
      (start[[d]] + inside[[d]]) * array_strides[[d]]
    })) +
      1L,
    source = if (all(extent == chunk_shape)) {
      seq_len(prod(chunk_shape))
    } else {
      walk(lapply(seq_len(k), function(d) inside[[d]] * chunk_strides[[d]])) +
        1L
    }
  )
}

#' @keywords internal
#' @noRd
.zarrPlace <- function(flat, values, shape, start, chunk_shape) {
  place <- .zarrPlacement(shape, start, chunk_shape)
  flat[place$target] <- values[place$source]
  flat
}

#' A shard (sharding_indexed): inner chunks followed (or preceded) by an index of
#' (offset, size) pairs, 16 bytes each, plus a 4-byte checksum when the index
#' codecs include crc32c. Returns the shard as a flat C-order vector.
#' @keywords internal
#' @noRd
.zarrDecodeShard <- function(bytes, config, dtype, shard_shape, fill) {
  inner_shape <- as.integer(unlist(config$chunk_shape))
  counts <- as.integer(ceiling(shard_shape / inner_shape))
  n <- prod(counts)
  index_names <- vapply(
    config$index_codecs,
    function(codec) as.character(codec$name),
    character(1)
  )
  index_size <- 16L * n + if ("crc32c" %in% index_names) 4L else 0L
  at_start <- identical(config$index_location, "start")
  index_bytes <- if (at_start) {
    bytes[seq_len(index_size)]
  } else {
    bytes[(length(bytes) - index_size + 1L):length(bytes)]
  }
  entries <- .zarrInt64(
    index_bytes[seq_len(16L * n)],
    2L * n,
    "little",
    signed = FALSE
  )
  offsets <- entries[seq(1L, 2L * n, by = 2L)]
  sizes <- entries[seq(2L, 2L * n, by = 2L)]
  missing_marker <- 2^64 - 1

  flat <- rep(fill, prod(shard_shape))
  if (is.numeric(fill)) {
    flat <- as.numeric(flat)
  }
  inner <- list(version = 3L, meta = list(codecs = config$codecs))
  grid <- as.matrix(expand.grid(lapply(counts, function(c) seq_len(c) - 1L)))
  ## the index lists the inner chunks in C order; expand.grid varies the first
  ## dimension fastest, so reorder to C order
  grid <- grid[
    do.call(order, unname(as.list(as.data.frame(grid)))),
    ,
    drop = FALSE
  ]
  for (k in seq_len(n)) {
    if (sizes[[k]] == 0 || offsets[[k]] >= missing_marker) {
      next
    }
    chunk <- bytes[(offsets[[k]] + 1):(offsets[[k]] + sizes[[k]])]
    values <- .zarrDecodeChunk(chunk, inner, dtype, inner_shape)
    flat <- .zarrPlace(
      flat,
      values,
      shard_shape,
      grid[k, ] * inner_shape,
      inner_shape
    )
  }
  flat
}
