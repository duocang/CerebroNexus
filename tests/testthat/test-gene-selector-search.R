## Shiny's own selectize with the Viewer's gene selector (www/gene-selector.js).
## With a server search the first page held the first 1000 genes (A...), so
## "CD4" first matched ABCD4 there; that option stayed highlighted after the
## search returned and Enter chose it. Text typed before the search was set up
## was never searched.
test_that("a typed gene is highlighted and chosen by Enter, also when typed early", {
  skip_if_no_chrome()
  browser <- chromote::ChromoteSession$new()
  on.exit(browser$close(), add = TRUE)
  evaluate <- function(code) {
    result <- browser$Runtime$evaluate(
      expression = code,
      awaitPromise = TRUE,
      returnByValue = TRUE
    )
    if (!is.null(result$exceptionDetails)) {
      stop(
        result$exceptionDetails$exception$description %||%
          result$exceptionDetails$text,
        call. = FALSE
      )
    }
    result$result$value
  }
  read_js <- function(path) {
    paste(readLines(path, warn = FALSE), collapse = "\n")
  }
  shared <- system.file("www/shared", package = "shiny")
  evaluate(read_js(file.path(shared, "jquery.js")))
  evaluate(read_js(file.path(shared, "selectize", "js", "selectize.min.js")))
  evaluate(paste0(
    "document.body.innerHTML = '<select id=\"expression_genes_input\" multiple></select>",
    "<select id=\"expression_rgb_gene_r\"></select>';",
    ## Shiny's selectInput binding, which gene-selector.js wraps.
    "const selectBinding = {name: 'shiny.selectInput', receiveMessage: function () {}};",
    "window.Shiny = {inputBindings: {getBindings: () => [{binding: selectBinding}]}};",
    "window.genes = Array.from({length: 999}, (_, i) => 'A' + String(i).padStart(4, '0'))",
    "  .concat(['ABCD4']).concat(['CD2', 'CD4', 'CD40', 'CD44', 'CD8A']).sort();",
    "window.fetch = () => new Promise(resolve => {",
    "  window.releaseList = () => resolve({ok: true, json: () => Promise.resolve(window.genes)});",
    "});",
    "const config = {valueField: 'value', labelField: 'label', searchField: ['label'],",
    "  maxOptions: 1000, loadThrottle: 300};",
    "$('#expression_genes_input').selectize(Object.assign({create: true}, config));",
    "$('#expression_rgb_gene_r').selectize(Object.assign({allowEmptyOption: true}, config));",
    "void 0;"
  ))
  evaluate(read_js(viewer_test_path("www", "gene-selector.js")))
  result <- evaluate(paste0(
    "(async () => {",
    "const binding = Shiny.inputBindings.getBindings()[0].binding;",
    "const settle = () => new Promise(resolve => setTimeout(resolve, 50));",
    "const type = (s, text) => { s.$control_input.val(text);",
    "  s.$control_input[0].dispatchEvent(new Event('input', {bubbles: true})); };",
    "const enter = s => { const e = $.Event('keydown'); e.keyCode = e.which = 13;",
    "  s.$control_input.trigger(e); };",
    "const active = s => s.$activeOption && s.$activeOption.attr('data-value');",
    "const genes = document.getElementById('expression_genes_input');",
    "const s = genes.selectize;",
    ## Typed before the list arrives.
    "s.focus(); type(s, 'CD4');",
    "binding.receiveMessage(genes, {cerebro_gene_choices: 'list'});",
    "window.releaseList(); await settle();",
    "const early = {options: Object.keys(s.options).length, active: active(s)};",
    "enter(s); early.chosen = s.getValue(); s.blur();",
    ## Typed into a channel once the list is in.
    "const channel = document.getElementById('expression_rgb_gene_r');",
    "const r = channel.selectize;",
    "binding.receiveMessage(channel, {cerebro_gene_choices: 'list', value: ''});",
    "await settle();",
    "r.focus(); type(r, 'C'); type(r, 'CD'); type(r, 'CD2');",
    "const ready = {active: active(r)}; enter(r); ready.chosen = r.getValue();",
    "return {early, ready};",
    "})()"
  ))
  expect_equal(result$early$options, 1005)
  expect_identical(result$early$active, "CD4")
  expect_identical(unlist(result$early$chosen), "CD4")
  expect_identical(result$ready$active, "CD2")
  expect_identical(result$ready$chosen, "CD2")
})
