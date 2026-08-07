test_that("one slide applied to every section keeps each section's own extent", {
  local({
    source(
      system.file("builder/extras.R", package = "CerebroNexus"),
      local = TRUE
    )

    ## Three sections cut from one block: same slide scan, but each sits at its
    ## own offset in the coordinate space.
    picture <- list(
      uri = "data:image/png;base64,AAAA",
      bytes = 4L,
      width = 300L,
      height = 240L
    )
    per_section <- list(
      A = list(
        bounds = list(xmin = 0, xmax = 100, ymin = 0, ymax = 80),
        cover = list(outside = 0L, total = 100L)
      ),
      B = list(
        bounds = list(xmin = 500, xmax = 600, ymin = 0, ymax = 80),
        cover = list(outside = 0L, total = 100L)
      ),
      C = list(
        bounds = list(xmin = 2000, xmax = 2100, ymin = 0, ymax = 80),
        cover = list(outside = 7L, total = 100L)
      )
    )

    got <- builder_pair_sections(picture, per_section)

    ## The picture is shared -- that is the whole point of "apply to all".
    expect_identical(unique(vapply(got, function(x) x$uri, "")), picture$uri)

    ## The extent is NOT. Copying one section's numbers to the rest put four
    ## slides out of five thousands of units from their own cells, selectable
    ## in the viewer and invisible on screen.
    expect_identical(
      vapply(got, function(x) x$bounds$xmin, numeric(1)),
      c(A = 0, B = 500, C = 2000)
    )

    ## Nor is the coverage count, which is what made the bug silent: a copied
    ## "0 cells outside" from section A reported success for every section.
    expect_identical(
      vapply(got, function(x) x$outside, integer(1)),
      c(A = 0L, B = 0L, C = 7L)
    )
  })
})
