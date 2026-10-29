# Tests for the Cerebro hla_typing slot + getter/setter round-trip.

test_that("addHLATyping / getHLATyping round-trips a named list", {
  crb <- Cerebro$new()
  crb$addHLATyping(
    list(
      sample_1 = c("HLA-A*02:01", "HLA-B*08:01"),
      sample_2 = c("HLA-A*01:01")
    ),
    source_type = "genotyped"
  )
  t <- crb$getHLATyping()
  expect_true(hla_is_typing_table(t))
  expect_equal(length(unique(t$sample)), 2L)
  expect_true(all(t$source_type == "genotyped"))
})

test_that("addHLATyping accepts a pre-normalized canonical table unchanged", {
  crb <- Cerebro$new()
  canon <- hla_normalize_typing(
    list(s1 = "HLA-A*02:01"),
    source_type = "synthetic"
  )
  crb$addHLATyping(canon)
  t <- crb$getHLATyping()
  expect_equal(nrow(t), 1L)
  expect_equal(t$source_type, "synthetic")
})

test_that("addHLATyping validates a canonical-looking table, not stores it raw", {
  crb <- Cerebro$new()
  # Has the canonical columns (so hla_is_typing_table() passes) but junk values:
  # an unrecognisable allele, a locus contradicting its allele, an out-of-range
  # copy, and an invalid provenance. None must reach downstream analysis.
  dirty <- data.frame(
    sample = c("s1", "s1", "s2"),
    donor_id = NA_character_,
    locus = c("HLA-A", "HLA-B", "HLA-A"), # row 2 locus contradicts its allele
    copy = c(1L, 9L, 1L), # 9 is not a diploid copy
    allele = c("HLA-A*02:01", "HLA-A*11:01", "NOT-AN-ALLELE"),
    resolution = NA_character_,
    source_type = c("genotyped", "wishful", "genotyped"), # 'wishful' invalid
    typing_method = NA_character_,
    source_reference = NA_character_,
    confidence = NA_real_,
    stringsAsFactors = FALSE
  )
  crb$addHLATyping(dirty)
  t <- crb$getHLATyping()

  expect_equal(nrow(t), 2L) # the unrecognisable allele row is dropped
  expect_false("NOT-AN-ALLELE" %in% t$allele)
  expect_setequal(t$locus, "HLA-A") # locus re-derived from the allele
  expect_true(any(is.na(t$copy))) # the out-of-range copy became NA
  expect_true("unknown" %in% t$source_type) # invalid provenance coerced
  expect_false("wishful" %in% t$source_type)
})

test_that("getHLATyping returns an empty canonical table when none is set", {
  crb <- Cerebro$new()
  t <- crb$getHLATyping()
  expect_true(hla_is_typing_table(t))
  expect_equal(nrow(t), 0L)
  # The R6 getter builds this empty table with base R (not hla_normalize_typing)
  # so it survives a package-free createShinyApp() bundle where the namespace is
  # absent. Pin it to the canonical empty table so the two cannot drift apart.
  expect_equal(t, hla_normalize_typing(list(), source_type = "unknown"))
})

test_that("stored typing is queryable by carrier index", {
  crb <- Cerebro$new()
  crb$addHLATyping(
    list(s1 = "HLA-A*02:01", s2 = "HLA-A*02:01", s3 = "HLA-A*01:01"),
    source_type = "genotyped"
  )
  ci <- hla_carrier_index(crb$getHLATyping())
  expect_setequal(ci[["HLA-A*02:01"]], c("s1", "s2"))
})
