fake_gene_session <- function() {
  session <- new.env(parent = emptyenv())
  session$userData <- new.env(parent = emptyenv())
  session$registered <- list()
  session$registerDataObj <- function(name, data, filter) {
    session$registered[[name]] <- list(data = data, filter = filter)
    paste0("dataobj/", name, "?nonce=", length(session$registered))
  }
  session
}

test_that("gene selectors of the same genes and selection share one URL", {
  scope <- viewer_runtime_env()
  session <- fake_gene_session()
  genes <- c("Gad1", "Slc17a7", "Snap25")

  first <- scope$viewerGeneChoicesUrl(session, genes)
  ## The R/G/B channels of the same page ask for the same list.
  expect_identical(scope$viewerGeneChoicesUrl(session, genes), first)
  expect_identical(scope$viewerGeneChoicesUrl(session, genes, ""), first)
  expect_length(session$registered, 1L)

  ## Another selection or another gene list gets its own URL.
  seeded <- scope$viewerGeneChoicesUrl(session, genes, "Snap25")
  expect_false(identical(seeded, first))
  expect_false(identical(scope$viewerGeneChoicesUrl(session, "NEW"), first))
  expect_length(session$registered, 3L)
})

test_that("the shared gene search answers like updateSelectizeInput", {
  scope <- viewer_runtime_env()
  data <- list(
    genes = c("Gad1", "Gad2", "Slc17a7", "Snap25"),
    selected = "Snap25"
  )
  ask <- function(query, maxop = 2L) {
    response <- scope$viewerGeneChoicesJSON(
      data,
      list(
        QUERY_STRING = paste0(
          "?query=",
          utils::URLencode(query, reserved = TRUE),
          "&maxop=",
          maxop
        )
      )
    )
    expect_identical(
      response$headers[["Cache-Control"]],
      "private, max-age=3600"
    )
    jsonlite::fromJSON(response$content)
  }
  ## The first page keeps the selected gene even beyond `maxop`.
  expect_identical(ask("")$value, c("Gad1", "Snap25"))
  ## Every word must match, case-insensitively.
  expect_identical(ask("GAD", 10L)$value, c("Gad1", "Gad2", "Snap25"))
  expect_identical(ask("gad 2", 10L)$value, c("Gad2", "Snap25"))
  expect_identical(ask("")$label, ask("")$value)
})
