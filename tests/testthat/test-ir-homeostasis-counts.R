ir_home_env <- new.env(parent = globalenv())
ir_home_env$`%||%` <- function(a, b) if (is.null(a)) b else a
sys.source(
  viewer_test_path("immune_repertoire", "encode.R"),
  envir = ir_home_env
)
ir_home_file <- testthat::test_path(
  "../../inst/viewer/immune_repertoire/data.R"
)
if (!file.exists(ir_home_file)) {
  ir_home_file <- system.file(
    "viewer/immune_repertoire/data.R",
    package = "CerebroNexus"
  )
}
for (expr in parse(ir_home_file)) {
  if (
    is.call(expr) &&
      as.character(expr[[1]])[1] %in% c("<-", "=") &&
      is.call(expr[[3]]) &&
      identical(expr[[3]][[1]], as.name("function"))
  ) {
    eval(expr, ir_home_env)
  }
}

test_that("shared clone histograms preserve distinct Abundance and Homeostasis policies", {
  values <- c("", "", NA, "NA", "01", "1", "克隆", "克隆", "ab")
  frame <- data.frame(
    CTgene = values,
    CTnt = values,
    CTaa = values,
    CTstrict = values
  )
  for (named in c(FALSE, TRUE)) {
    raw <- list(frame, frame[FALSE, ], NULL, data.frame(unrelated = "x"))
    if (named) {
      names(raw) <- c("z10", "z2", "absent", "missing_column")
    }
    encoded <- ir_home_env$ir_encode_repertoire(raw)
    sample <- if (named) "z10" else "1"
    for (column in c("CTgene", "CTnt", "CTaa", "CTstrict")) {
      ## Abundance leaves the blank clone out, Homeostasis counts it.
      expect_identical(
        as.list(ir_home_env$ir_abundance_histogram(encoded, column)),
        list(sample = c(sample, sample), abundance = 1:2, n_clones = c(4L, 1L))
      )
      expect_identical(
        as.list(ir_home_env$ir_homeostasis_counts(encoded, column)),
        list(sample = c(sample, sample), abundance = 1:2, n_clones = c(4L, 2L))
      )
    }
  }
  empty <- data.frame(
    sample = character(),
    abundance = integer(),
    n_clones = integer()
  )
  encoded <- ir_home_env$ir_encode_repertoire(list())
  expect_identical(ir_home_env$ir_abundance_histogram(encoded, "CTgene"), empty)
  expect_identical(ir_home_env$ir_homeostasis_counts(encoded, "CTgene"), empty)
})

test_that("Homeostasis exact bins match scRepertoire, including blanks and thresholds", {
  skip_if_not_installed("scRepertoire")
  frames <- list(
    z10 = c(rep("a", 4), rep("b", 2), "", NA, "NA"),
    z2 = c("", "", "c", "d"),
    empty = character(),
    missing = c(NA, NA),
    unicode = c("克隆", "克隆", "other")
  )
  repertoire <- lapply(frames, function(x) {
    data.frame(CTgene = x, CTnt = x, CTaa = x, CTstrict = x)
  })
  for (call in c("gene", "nt", "aa", "strict")) {
    for (breaks in list(
      c(
        Rare = .0001,
        Small = .001,
        Medium = .01,
        Large = .1,
        Hyperexpanded = 1
      ),
      c(A = .25, B = .5, C = 1),
      c(A = .25, B = .5)
    )) {
      column <- paste0("CT", call)
      counts <- ir_home_env$ir_homeostasis_counts(
        ir_home_env$ir_encode_repertoire(repertoire),
        column
      )
      actual <- ir_home_env$ir_homeostasis_matrix(counts, breaks)
      expected <- scRepertoire::clonalHomeostasis(
        repertoire,
        cloneCall = call,
        chain = "both",
        cloneSize = breaks,
        exportTable = TRUE
      )
      expect_equal(actual, expected, tolerance = 1e-12)
    }
  }
  homeostasis <- ir_home_env$ir_homeostasis_counts(
    ir_home_env$ir_encode_repertoire(repertoire),
    "CTgene"
  )
  expect_equal(sum(homeostasis$abundance * homeostasis$n_clones), 15)
})

test_that("Homeostasis plot preserves stacked cell proportions", {
  skip_if_not_installed("scRepertoire")
  data <- list(
    B = data.frame(CTgene = c("a", "a", "b", "c")),
    A = data.frame(CTgene = c("x", "y"))
  )
  breaks <- c(Small = .25, Large = 1)
  p <- ir_home_env$ir_homeostasis_plot(
    ir_home_env$ir_homeostasis_counts(
      ir_home_env$ir_encode_repertoire(data),
      "CTgene"
    ),
    breaks,
    "Harmonic"
  )
  reference <- scRepertoire::clonalHomeostasis(
    data,
    cloneCall = "gene",
    cloneSize = breaks,
    palette = "Harmonic"
  )
  actual <- ggplot2::ggplot_build(p)$data[[1]]
  expected <- ggplot2::ggplot_build(reference)$data[[1]]
  expect_equal(
    actual[c("x", "y", "ymin", "ymax", "fill")],
    expected[c("x", "y", "ymin", "ymax", "fill")],
    ignore_attr = TRUE
  )
})

test_that("grouped Homeostasis pools exact clone identities and preserves empty levels", {
  skip_if_not_installed("scRepertoire")
  make <- function(clones, groups) {
    data.frame(
      CTgene = clones,
      CTnt = clones,
      CTaa = clones,
      CTstrict = clones,
      severity = groups,
      stringsAsFactors = FALSE
    )
  }
  raw <- list(
    B = make(
      c("same", "same", "", "NA", NA, "克隆", "a\034b"),
      c("mild", "severe", "mild", "", "zero", NA, "g\034x")
    ),
    A = make(
      c("same", "same", "", "a\034b"),
      c("mild", "mild", "severe", "g\034x")
    ),
    empty = make(character(), character()),
    missing = make(NA_character_, "all_missing_sample")
  )
  for (factor_groups in c(FALSE, TRUE)) {
    data <- raw
    if (factor_groups) {
      data <- lapply(data, function(x) {
        x$severity <- factor(
          x$severity,
          levels = c(
            "severe",
            "mild",
            "",
            "zero",
            "g\034x",
            "unused",
            "all_missing_sample"
          )
        )
        x
      })
    }
    encoded <- ir_home_env$ir_encode_repertoire(data)
    severity <- ir_home_env$ir_row_values(encoded, "severity", NULL)
    for (call in c("gene", "nt", "aa", "strict")) {
      counts <- ir_home_env$ir_homeostasis_counts(
        encoded,
        paste0("CT", call),
        "both",
        severity
      )
      for (breaks in list(c(A = .25, B = .5, C = 1), c(A = .25, B = .5))) {
        expected <- scRepertoire::clonalHomeostasis(
          data,
          cloneCall = call,
          chain = "both",
          group.by = "severity",
          cloneSize = breaks,
          exportTable = TRUE
        )
        expect_equal(
          ir_home_env$ir_homeostasis_matrix(counts, breaks),
          expected,
          tolerance = 1e-12
        )
      }
    }
    # Shared clone "same" has three cells in mild, even across source samples.
    mild <- counts[counts$sample == "mild", ]
    expect_equal(mild$abundance, c(1L, 3L))
    expect_equal(mild$n_clones, c(1L, 1L))
  }
  data <- list(
    B = make(c("x", "x", "y"), c("severe", "mild", "mild")),
    A = make(c("x", "z"), c("mild", "severe"))
  )
  breaks <- c(Small = .5, Large = 1)
  grouped <- function(data) {
    encoded <- ir_home_env$ir_encode_repertoire(data)
    ir_home_env$ir_homeostasis_counts(
      encoded,
      "CTgene",
      "both",
      ir_home_env$ir_row_values(encoded, "severity", NULL)
    )
  }
  actual <- ir_home_env$ir_homeostasis_plot(
    grouped(data),
    breaks,
    "inferno"
  )
  expected <- scRepertoire::clonalHomeostasis(
    data,
    cloneCall = "gene",
    group.by = "severity",
    cloneSize = breaks,
    palette = "inferno"
  )
  cols <- c("x", "y", "ymin", "ymax", "fill")
  expect_equal(
    ggplot2::ggplot_build(actual)$data[[1]][cols],
    ggplot2::ggplot_build(expected)$data[[1]][cols],
    ignore_attr = TRUE
  )
  # A new dataset must produce its own labels, not reuse a grouped result.
  other <- list(C = make(c("q", "q"), c("new", "new")))
  expect_identical(grouped(other)$sample, "new")
})
