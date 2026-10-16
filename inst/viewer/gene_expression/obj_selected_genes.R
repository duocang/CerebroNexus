##----------------------------------------------------------------------------##
## Reactive data that holds genes provided by user or in selected gene set.
##----------------------------------------------------------------------------##
expressionInputGenes <- function(value) {
  genes <- unique(trimws(unlist(strsplit(as.character(value), "[,;[:space:]]+"))))
  genes[!is.na(genes) & nzchar(genes)]
}

## Mean/separate retain their full list. Seed RGB only when that list changes;
## returning to RGB must preserve manual channel assignments, including blanks.
expression_rgb_seed <- NULL
observeEvent(
  list(input[["expression_projection_genes_in_separate_panels"]],
    input[["expression_genes_input"]], list_of_genes()),
  {
    req(identical(input[["expression_projection_genes_in_separate_panels"]], "rgb"))
    source <- input[["expression_genes_input"]]
    req(!is.null(source))
    source <- expressionInputGenes(source)
    available <- list_of_genes()
    genes <- unique(available[match(tolower(source), tolower(available))])
    genes <- genes[!is.na(genes)]
    if (identical(expression_rgb_seed, genes)) return()
    expression_rgb_seed <<- genes
    session$sendCustomMessage("cerebro_expression_rgb_seed", list(
      dataset_context = viewer_loaded_dataset_context(),
      source = as.list(as.character(input[["expression_genes_input"]])),
      genes = as.list(head(genes, 3L))
    ))
  }
)

expression_selected_genes_input <- reactive({
  req(
    input[["expression_analysis_mode"]],
    list_of_genes()
  )
  ## prepare empty list for data
  gene_sets <- list(
    "genes_to_display" = character(),
    "genes_to_display_present" = character(),
    "genes_to_display_missing" = character(),
    "rgb_genes" = list(r = NULL, g = NULL, b = NULL)
  )
  ## ...
  if (input[["expression_analysis_mode"]] == "Gene(s)") {
    if (
      identical(
        input[["expression_projection_genes_in_separate_panels"]],
        "rgb"
      )
    ) {
      rgb <- lapply(c("r", "g", "b"), function(channel) {
        input[[paste0("expression_rgb_gene_", channel)]]
      })
      names(rgb) <- c("r", "g", "b")
      gene_sets[["rgb_genes"]] <- rgb
      gene_sets[["genes_to_display"]] <- unique(unlist(Filter(
        function(gene) !is.null(gene) && nzchar(gene),
        rgb
      )))
    } else {
      gene_sets[["genes_to_display"]] <- expressionInputGenes(
        input[["expression_genes_input"]]
      )
    }
    ## ...
  } else if (input[["expression_analysis_mode"]] == "Gene set") {
    req(input[["expression_select_gene_set"]])
    gene_sets[["genes_to_display"]] <- getGenesForGeneSet(input[[
      "expression_select_gene_set"
    ]])
  }
  ## check which are available in the data set
  genes_to_display_here <- list_of_genes()[match(
    tolower(gene_sets[["genes_to_display"]]),
    tolower(list_of_genes())
  )]
  ## get which genes are available in the data set
  gene_sets[["genes_to_display_present"]] <- na.omit(genes_to_display_here)
  ## get names of provided genes that are not in the data set
  gene_sets[["genes_to_display_missing"]] <- gene_sets[[
    "genes_to_display"
  ]][which(is.na(genes_to_display_here))]
  ## Channel names must use the same dataset spelling as expression rows.
  ## Preserve channel positions (including repeated and absent genes).
  gene_sets[["rgb_genes"]] <- lapply(gene_sets[["rgb_genes"]], function(gene) {
    if (length(gene) != 1L || is.na(gene) || !nzchar(gene)) {
      return(NULL)
    }
    index <- match(tolower(gene), tolower(list_of_genes()))
    if (is.na(index)) NULL else unname(list_of_genes()[index])
  })
  return(gene_sets)
})

## Let a multi-select gesture settle before any million-cell expression read.
## The first value still initializes immediately.
expression_selection_snapshot <- debounceAfterFirst(
  reactive(list(available = list_of_genes(), value = expression_selected_genes_input())),
  200
)
expression_selected_genes <- reactive({
  snapshot <- expression_selection_snapshot()
  ## A previous dataset's canonical selection is never a valid intermediate
  ## frame. Only debounce edits within the same gene universe.
  if (!identical(snapshot$available, list_of_genes())) {
    return(expression_selected_genes_input())
  }
  snapshot$value
})
