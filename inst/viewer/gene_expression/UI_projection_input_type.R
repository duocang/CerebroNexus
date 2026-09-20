##----------------------------------------------------------------------------##
## UI elements to choose whether gene(s) or gene sets should be analyzed.
## Keep gene controls mounted: rebuilding three whole-transcriptome selectize
## inputs when RGB mode opens is prohibitively expensive for large gene lists.
##----------------------------------------------------------------------------##
output[["expression_projection_input_type_UI"]] <- renderUI({
  channel_input <- function(channel, label) {
    selectizeInput(
      paste0("expression_rgb_gene_", channel),
      label = label,
      choices = NULL,
      selected = "",
      multiple = FALSE,
      options = list(
        maxOptions = 1000,
        create = FALSE,
        allowEmptyOption = TRUE,
        placeholder = paste(tolower(label), "gene..."),
        loadThrottle = 300
      )
    )
  }

  tagList(
    conditionalPanel(
      paste0(
        "input.expression_analysis_mode == 'Gene(s)' && ",
        "input.expression_projection_genes_in_separate_panels != 'rgb'"
      ),
      selectizeInput(
        "expression_genes_input",
        label = "Gene(s)",
        choices = NULL,
        multiple = TRUE,
        options = list(
          maxOptions = 1000,
          create = TRUE,
          plugins = list("remove_button"),
          loadThrottle = 300
        )
      )
    ),
    conditionalPanel(
      paste0(
        "input.expression_analysis_mode == 'Gene(s)' && ",
        "input.expression_projection_genes_in_separate_panels == 'rgb'"
      ),
      div(
        class = "cerebro-gene-rgb-row cerebro-control-enter",
        div(
          class = "cerebro-gene-rgb-channel",
          channel_input("r", "Red channel")
        ),
        div(
          class = "cerebro-gene-rgb-channel",
          channel_input("g", "Green channel")
        ),
        div(
          class = "cerebro-gene-rgb-channel",
          channel_input("b", "Blue channel")
        )
      )
    ),
    conditionalPanel(
      "input.expression_analysis_mode == 'Gene set'",
      uiOutput("expression_select_gene_set_UI")
    ),
    tags$script(HTML(
      "
      (function () {
        if (!window.jQuery) return;
        var modeSelector = '#expression_projection_genes_in_separate_panels';
        var geneSelector = '#expression_genes_input';
        function copyGenesToRgb() {
          var source = document.getElementById('expression_genes_input');
          var genes = source && source.selectize
            ? source.selectize.getValue() : [];
          if (!Array.isArray(genes)) genes = genes ? [genes] : [];
          ['r', 'g', 'b'].forEach(function (channel, index) {
            var input = document.getElementById(
              'expression_rgb_gene_' + channel
            );
            var control = input && input.selectize;
            if (!control) return;
            var gene = genes[index] || '';
            if (gene) control.addOption({value: gene, text: gene});
            control.setValue(gene);
          });
        }
        window.jQuery(document)
          .off('change.cerebroExpressionRgb', modeSelector)
          .on('change.cerebroExpressionRgb', modeSelector, function () {
            if (this.value !== 'rgb') return;
            copyGenesToRgb();
          })
          .off('change.cerebroExpressionRgbPrefill', geneSelector)
          .on('change.cerebroExpressionRgbPrefill', geneSelector, function () {
            var mode = document.getElementById(
              'expression_projection_genes_in_separate_panels'
            );
            if (!mode || mode.value !== 'rgb') copyGenesToRgb();
          });
      })();
      "
    ))
  )
})

output[["expression_select_gene_set_UI"]] <- renderUI({
  req(identical(input[["expression_analysis_mode"]], "Gene set"))
  selectizeInput(
    "expression_select_gene_set",
    label = "Gene set",
    choices = data.table::as.data.table(
      data.frame("Gene sets" = c("-", getGeneSetNames()))
    ),
    multiple = FALSE
  )
})

## Wait for the dynamic input-type UI's first bound input, then initialise all
## four stable gene controls. Later display-mode switches only show/hide them.
observeEvent(
  input[["expression_analysis_mode"]],
  {
    for (input_id in c(
      "expression_genes_input",
      "expression_rgb_gene_r",
      "expression_rgb_gene_g",
      "expression_rgb_gene_b"
    )) {
      local({
        id <- input_id
        serverSideGeneSelector(
          session,
          id,
          active = function() identical(input[["sidebar"]], "geneExpression")
        )
      })
    }
  },
  once = TRUE
)
