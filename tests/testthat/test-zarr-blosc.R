## Blosc chunks and numpy fixed-width strings in the Zarr reader ---------------

## The decoder is checked against real c-blosc output (lz4, byte shuffle, as
## written by Rarr) and against frames built here from the format description
## for the streams Rarr does not write (zstd, zlib, no shuffle, odd block sizes).

hex_raw <- function(x) {
  x <- paste(x, collapse = "")
  from <- seq(1L, nchar(x), by = 2L)
  as.raw(strtoi(substring(x, from, from + 1L), 16L))
}

blosc_lz4_ramp <- hex_raw(paste0(
  "02012104b0040000b004000057010000140000000b010000fff1010203040506",
  "0708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20212223242526",
  "2728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f40414243444546",
  "4748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f60616263646566",
  "6768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f80818283848586",
  "8788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6",
  "a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6",
  "c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6",
  "e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff00000114502829",
  "2a2b2c100000001f000100eb1f010100145001010101010c0000001f000100ff",
  "145000000000000c0000001f000100ff14500000000000"
))
blosc_lz4_counts <- hex_raw(paste0(
  "020121086009000060090000a6000000140000000c0000001f000100ff145000",
  "000000000c0000001f000100ff145000000000000c0000001f000100ff145000",
  "000000000c0000001f000100ff145000000000000c0000001f000100ff145000",
  "000000000c0000001f000100ff1450000000000015000000af00f00000000000",
  "0000080a00ff0b5000000000081500000070003f000040000005000f0a00ff0a",
  "500000000040"
))

## One c-blosc 1 frame: header, start of each block, then per block the streams
## (a size and the bytes; a stream that does not shrink is stored as it is).
blosc_frame <- function(
  payload,
  typesize,
  compressor = c("zlib", "zstd", "none"),
  shuffle = TRUE,
  blocksize = length(payload),
  dont_split = FALSE
) {
  compressor <- match.arg(compressor)
  nbytes <- length(payload)
  nblocks <- as.integer(ceiling(nbytes / blocksize))
  blocks <- vector("list", nblocks)
  for (block in seq_len(nblocks)) {
    from <- (block - 1L) * blocksize + 1L
    bytes <- payload[from:min(nbytes, from + blocksize - 1L)]
    size <- length(bytes)
    if (shuffle && typesize > 1L && size >= typesize) {
      count <- size %/% typesize
      main <- count * typesize
      planes <- matrix(bytes[seq_len(main)], nrow = typesize)
      bytes <- c(as.vector(t(planes)), bytes[-seq_len(main)])
    }
    splits <- if (
      !dont_split &&
        typesize <= 16L &&
        blocksize %/% typesize >= 128L &&
        size == blocksize
    ) {
      typesize
    } else {
      1L
    }
    stream_size <- size %/% splits
    streams <- lapply(seq_len(splits), function(i) {
      stream <- bytes[((i - 1L) * stream_size + 1L):(i * stream_size)]
      packed <- switch(
        compressor,
        zlib = memCompress(stream, "gzip"),
        zstd = qs2::zstd_compress_raw(stream, 3L),
        none = stream
      )
      if (length(packed) >= stream_size) {
        packed <- stream
      }
      c(writeBin(length(packed), raw(), size = 4L), packed)
    })
    blocks[[block]] <- unlist(streams)
  }
  starts <- 16L + 4L * nblocks + c(0L, cumsum(vapply(blocks, length, 1L)))
  body <- unlist(blocks)
  flags <- (if (shuffle) 1L else 0L) +
    (if (dont_split) 16L else 0L) +
    32L * c(zlib = 3L, zstd = 4L, none = 3L)[[compressor]]
  c(
    as.raw(c(2L, 1L, flags, typesize)),
    writeBin(
      as.integer(c(nbytes, blocksize, 16L + 4L * nblocks + length(body))),
      raw(),
      size = 4L
    ),
    writeBin(as.integer(starts[seq_len(nblocks)]), raw(), size = 4L),
    body
  )
}

blosc_double_bytes <- function(x) writeBin(as.numeric(x), raw(), size = 8L)

test_that("Blosc chunks written by c-blosc (lz4, byte shuffle) are decoded", {
  expect_identical(
    CerebroNexus:::.zarrBlosc(blosc_lz4_ramp),
    writeBin(1:300, raw(), size = 4L)
  )
  expect_identical(
    CerebroNexus:::.zarrBlosc(blosc_lz4_counts),
    blosc_double_bytes(rep(c(0, 1, 0, 0, 2, 0, 0, 0, 0, 3), 30))
  )
})

test_that("Blosc zlib and zstd streams, shuffled or not, split or not", {
  set.seed(1)
  payloads <- list(
    smooth = blosc_double_bytes(cumsum(rnorm(5000))),
    counts = blosc_double_bytes(rpois(3000, 0.3)),
    integers = writeBin(as.integer(sample(0:50, 4000, TRUE)), raw(), size = 4L),
    flags = as.raw(sample(0:1, 6000, TRUE)),
    incompressible = as.raw(sample(0:255, 3000, TRUE))
  )
  typesize <- c(
    smooth = 8L,
    counts = 8L,
    integers = 4L,
    flags = 1L,
    incompressible = 1L
  )
  for (name in names(payloads)) {
    payload <- payloads[[name]]
    for (compressor in c("zlib", "zstd", "none")) {
      for (shuffle in c(TRUE, FALSE)) {
        for (blocksize in c(length(payload), 4096L, 1000L)) {
          for (dont_split in c(FALSE, TRUE)) {
            frame <- blosc_frame(
              payload,
              typesize[[name]],
              compressor,
              shuffle,
              min(blocksize, length(payload)),
              dont_split
            )
            expect_identical(
              CerebroNexus:::.zarrBlosc(frame),
              payload,
              info = paste(name, compressor, shuffle, blocksize, dont_split)
            )
          }
        }
      }
    }
  }
})

test_that("a Blosc chunk stored without compression is copied", {
  payload <- blosc_double_bytes(1:10)
  frame <- c(
    as.raw(c(2L, 1L, 2L, 8L)),
    writeBin(
      c(length(payload), length(payload), 16L + length(payload)),
      raw(),
      size = 4L
    ),
    payload
  )
  expect_identical(CerebroNexus:::.zarrBlosc(frame), payload)
})

test_that("Blosc features that are not read stop with their name", {
  frame <- blosc_frame(blosc_double_bytes(1:300), 8L, "zlib")
  with_flags <- function(flags) {
    frame[[3L]] <- as.raw(flags)
    frame
  }
  expect_error(CerebroNexus:::.zarrBlosc(with_flags(1L)), "`blosclz`")
  expect_error(
    CerebroNexus:::.zarrBlosc(with_flags(1L + 32L * 2L)),
    "`snappy`"
  )
  expect_error(
    CerebroNexus:::.zarrBlosc(with_flags(1L + 4L + 96L)),
    "bit shuffle"
  )
  expect_error(CerebroNexus:::.zarrBlosc(with_flags(1L + 8L + 96L)), "delta")
  bad_version <- frame
  bad_version[[1L]] <- as.raw(5L)
  expect_error(CerebroNexus:::.zarrBlosc(bad_version), "version 5")
  expect_error(CerebroNexus:::.zarrBlosc(raw(4)), "shorter than its header")
})

## A store with one array, laid out the way zarr-python 2 or 3 would.
write_zarr_array_store <- function(meta_v2 = NULL, meta_v3 = NULL, chunks) {
  store <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(store, "x"))
  writeLines(
    '{"zarr_format":3,"node_type":"group","attributes":{}}',
    file.path(store, "zarr.json")
  )
  if (!is.null(meta_v3)) {
    writeLines(meta_v3, file.path(store, "x", "zarr.json"))
    dir.create(file.path(store, "x", "c"))
    for (i in seq_along(chunks)) {
      writeBin(chunks[[i]], file.path(store, "x", "c", i - 1L))
    }
  } else {
    writeLines(meta_v2, file.path(store, "x", ".zarray"))
    for (i in seq_along(chunks)) {
      writeBin(chunks[[i]], file.path(store, "x", i - 1L))
    }
  }
  CerebroNexus:::.zarrContext(store)
}

test_that("Zarr v2 and v3 arrays with Blosc chunks are read", {
  expected <- rep(c(0, 1, 0, 0, 2, 0, 0, 0, 0, 3), 30)
  v2 <- write_zarr_array_store(
    meta_v2 = paste0(
      '{"zarr_format":2,"shape":[300],"chunks":[300],"dtype":"<f8",',
      '"compressor":{"id":"blosc","cname":"lz4","clevel":5,"shuffle":1,',
      '"blocksize":0},"fill_value":0,"order":"C","filters":null}'
    ),
    chunks = list(blosc_lz4_counts)
  )
  expect_equal(CerebroNexus:::.zarrRead(v2, "x"), expected)

  v3 <- write_zarr_array_store(
    meta_v3 = paste0(
      '{"zarr_format":3,"node_type":"array","shape":[300],',
      '"data_type":"float64","chunk_grid":{"name":"regular",',
      '"configuration":{"chunk_shape":[300]}},"chunk_key_encoding":',
      '{"name":"default","configuration":{"separator":"/"}},"fill_value":0,',
      '"codecs":[{"name":"bytes","configuration":{"endian":"little"}},',
      '{"name":"blosc","configuration":{"cname":"zstd","clevel":5,',
      '"shuffle":"shuffle","typesize":8,"blocksize":0}}]}'
    ),
    chunks = list(blosc_lz4_counts)
  )
  expect_equal(CerebroNexus:::.zarrRead(v3, "x"), expected)

  ## two chunks, the second one shorter than the first
  two <- write_zarr_array_store(
    meta_v2 = paste0(
      '{"zarr_format":2,"shape":[450],"chunks":[300],"dtype":"<i4",',
      '"compressor":{"id":"blosc","cname":"lz4","clevel":5,"shuffle":1,',
      '"blocksize":0},"fill_value":0,"order":"C","filters":null}'
    ),
    chunks = list(
      blosc_lz4_ramp,
      blosc_frame(writeBin(301:600, raw(), size = 4L), 4L, "zlib")
    )
  )
  expect_equal(CerebroNexus:::.zarrRead(two, "x"), as.numeric(1:450))
})

test_that("an unsupported codec or compressor is named in the error", {
  v3 <- write_zarr_array_store(
    meta_v3 = paste0(
      '{"zarr_format":3,"node_type":"array","shape":[4],',
      '"data_type":"int32","chunk_grid":{"name":"regular",',
      '"configuration":{"chunk_shape":[4]}},"chunk_key_encoding":',
      '{"name":"default","configuration":{"separator":"/"}},"fill_value":0,',
      '"codecs":[{"name":"bytes","configuration":{"endian":"little"}},',
      '{"name":"lzma","configuration":{}}]}'
    ),
    chunks = list(as.raw(1:16))
  )
  expect_error(CerebroNexus:::.zarrRead(v3, "x"), "codec `lzma`")
  v2 <- write_zarr_array_store(
    meta_v2 = paste0(
      '{"zarr_format":2,"shape":[4],"chunks":[4],"dtype":"<i4",',
      '"compressor":{"id":"lzma"},"fill_value":0,"order":"C",',
      '"filters":null}'
    ),
    chunks = list(as.raw(1:16))
  )
  expect_error(CerebroNexus:::.zarrRead(v2, "x"), "compressor `lzma`")
})

test_that("numpy fixed-width strings are read", {
  utf32 <- function(strings, width) {
    codes <- unlist(lapply(strings, function(s) {
      points <- utf8ToInt(s)
      c(points, rep(0L, width - length(points)))
    }))
    writeBin(as.integer(codes), raw(), size = 4L)
  }
  strings <- c("B cell", "T", "", "café")
  expect_identical(
    CerebroNexus:::.zarrFixedStrings(utf32(strings, 6L), 4L, "U", 6L),
    strings
  )
  bytes <- unlist(lapply(c("ab", "abcd", ""), function(s) {
    c(charToRaw(s), raw(4L - nchar(s)))
  }))
  expect_identical(
    CerebroNexus:::.zarrFixedStrings(bytes, 3L, "S", 4L),
    c("ab", "abcd", "")
  )

  store <- write_zarr_array_store(
    meta_v2 = paste0(
      '{"zarr_format":2,"shape":[4],"chunks":[4],"dtype":"<U6",',
      '"compressor":null,"fill_value":"","order":"C","filters":null}'
    ),
    chunks = list(utf32(strings, 6L))
  )
  expect_identical(CerebroNexus:::.zarrRead(store, "x"), strings)
})
