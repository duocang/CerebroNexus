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

test_that("gene selectors of the same genes share one gene list URL", {
  scope <- viewer_runtime_env()
  session <- fake_gene_session()
  genes <- c("Gad1", "Slc17a7", "Snap25")

  first <- scope$viewerGeneChoicesUrl(session, genes)
  ## The R/G/B channels of the same page ask for the same list.
  expect_identical(scope$viewerGeneChoicesUrl(session, genes), first)
  expect_length(session$registered, 1L)

  ## Another gene list gets its own URL.
  expect_false(identical(scope$viewerGeneChoicesUrl(session, "NEW"), first))
  expect_length(session$registered, 2L)
})

test_that("the gene list URL answers the whole list, whatever the query", {
  scope <- viewer_runtime_env()
  genes <- c("ABCD4", "CD4", "CD40", "Gad1")
  ask <- function(query) {
    response <- scope$viewerGeneChoicesJSON(
      list(genes = genes),
      list(QUERY_STRING = paste0("?query=", query, "&maxop=1"))
    )
    expect_identical(
      response$headers[["Cache-Control"]],
      "private, max-age=3600"
    )
    jsonlite::fromJSON(response$content)
  }
  ## The browser searches the list itself (www/gene-selector.js).
  expect_identical(ask(""), genes)
  expect_identical(ask("CD4"), genes)
})

test_that("a gene selector update carries the selection and the list URL", {
  scope <- viewer_runtime_env()
  session <- fake_gene_session()
  sent <- list()
  session$sendInputMessage <- function(id, message) {
    sent[[length(sent) + 1L]] <<- list(id = id, message = message)
  }
  genes <- c("CD4", "CD8A")

  scope$viewerUpdateGeneSelector(
    session,
    "expression_genes_input",
    genes,
    "CD4"
  )
  scope$viewerUpdateGeneSelector(session, "expression_rgb_gene_r", genes, NULL)

  expect_identical(sent[[1L]]$message$value, "CD4")
  expect_null(sent[[2L]]$message$value)
  expect_identical(
    sent[[1L]]$message$cerebro_gene_choices,
    sent[[2L]]$message$cerebro_gene_choices
  )
  expect_null(sent[[1L]]$message$url)
  expect_length(session$registered, 1L)
})
