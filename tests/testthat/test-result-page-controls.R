test_that("mean-only expression groups are reachable without expression reads", {
  shiny::testServer(function(input, output, session) {
    getGroupsWithMostExpressedGenes <- function() "percent-group"
    getGroupsWithMeanExpression <- function() c("mean-only", "percent-group")
    getMetaData <- function() stop("must not read cells for control choices")
    sys.source(viewer_test_path("most_expressed_genes", "select_group.R"), environment())
  }, {
    html <- output$most_expressed_genes_select_group_UI$html
    expect_match(html, 'value="mean-only"', fixed = TRUE)
    expect_equal(lengths(regmatches(html, gregexpr('value="percent-group"', html, fixed = TRUE))), 1L)
  })
})

test_that("enrichment method changes preserve valid choices and reject stale methods", {
  shiny::testServer(function(input, output, session) {
    scope <- environment()
    sent <- NULL
    session$sendInputMessage <- function(id, message) sent <<- message
    getMethodsForEnrichedPathways <- function() c("Enrichr", "GSVA", "empty")
    getGroupsWithEnrichedPathways <- function(method) switch(method,
      Enrichr = c("cluster", "sample"), GSVA = "sample", empty = character())
    sys.source(viewer_test_path("enriched_pathways", "select_content.R"), scope)
  }, {
    session$flushReact()
    initial <- output$enriched_pathways_select_method_and_table_UI$html
    expect_match(initial, 'id="enriched_pathways_selected_table"', fixed = TRUE)
    session$setInputs(enriched_pathways_selected_method = "Enrichr", enriched_pathways_selected_table = "sample")
    session$setInputs(enriched_pathways_selected_method = "GSVA")
    expect_identical(scope$sent$value, "sample")
    expect_identical(output$enriched_pathways_select_method_and_table_UI$html, initial)
    session$setInputs(enriched_pathways_selected_method = "empty")
    expect_length(scope$sent$value, 0L)
    before <- scope$sent
    session$setInputs(enriched_pathways_selected_method = "old-dataset-method")
    expect_identical(scope$sent, before)
  })
})

test_that("stored expression metric preference survives a grouping change", {
  expressions <- as.list(parse(viewer_test_path("most_expressed_genes", "table.R")))
  wanted <- c("most_expressed_genes_available_groups", "most_expressed_genes_selected_tables",
    "most_expressed_genes_selected_table", "most_expressed_genes_metric_description",
    'output[["most_expressed_genes_table_or_text_UI"]]')
  definitions <- Filter(function(e) is.call(e) && identical(e[[1L]], quote(`<-`)) &&
    paste(deparse(e[[2L]]), collapse = "") %in% wanted, expressions)
  shiny::testServer(function(input, output, session) {
    getGroupsWithMostExpressedGenes <- function() c("A", "B")
    getGroupsWithMeanExpression <- function() c("A", "B", "mean-only")
    viewerGetMostExpressedGenes <- function(group) data.frame(group = "x", gene = "Gad1", pct = 0.5)
    getMeanExpression <- function(group) data.frame(group = "x", gene = "Gad1", mean_expr = 1.5)
    for (definition in definitions) eval(definition, environment())
  }, {
    session$setInputs(most_expressed_genes_selected_group = "A", most_expressed_genes_metric_type = "mean_expr")
    session$setInputs(most_expressed_genes_selected_group = "B")
    expect_match(output$most_expressed_genes_table_or_text_UI$html, 'value="mean_expr" selected', fixed = TRUE)
    session$setInputs(most_expressed_genes_selected_group = "mean-only")
    expect_match(output$most_expressed_genes_table_or_text_UI$html, 'value="mean_expr" selected', fixed = TRUE)
    expect_no_match(output$most_expressed_genes_table_or_text_UI$html, 'value="pct"', fixed = TRUE)
    session$setInputs(most_expressed_genes_selected_group = "stale")
    expect_error(most_expressed_genes_selected_tables(), class = "shiny.silent.error")
  })
})
