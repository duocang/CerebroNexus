##----------------------------------------------------------------------------##
## Table of gene IDs and symbols.
##----------------------------------------------------------------------------##
gene_id_table_path <- function(filename) {
  candidates <- c(
    file.path(Cerebro.options[["cerebro_root"]], "extdata", filename),
    file.path(getwd(), "extdata", filename),
    system.file("extdata", filename, package = "CerebroNexus")
  )
  path <- candidates[file.exists(candidates)][1L]
  if (is.na(path)) {
    stop("Gene ID conversion table is unavailable.", call. = FALSE)
  }
  path
}

output[["gene_info"]] <- DT::renderDataTable({
  if (input[["geneIdConversion_organism"]] == "mouse") {
    conversion_table <- read.table(
      gene_id_table_path("mm10_gene_ID_name.tsv.gz"),
      sep = "\t",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  } else if (input[["geneIdConversion_organism"]] == "human") {
    conversion_table <- read.table(
      gene_id_table_path("hg38_gene_ID_name.tsv.gz"),
      sep = "\t",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  }
  DT::datatable(
    conversion_table,
    filter = "none",
    selection = "multiple",
    escape = FALSE,
    rownames = FALSE,
    options = list(
      scrollX = FALSE,
      dom = "Bfrtip",
      lengthMenu = c(15, 30, 50, 100),
      pageLength = 50
    )
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["geneIdConversion_info"]], {
  showModal(
    modalDialog(
      geneIdConversion_info[["text"]],
      title = geneIdConversion_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
geneIdConversion_info <- list(
  title = "Gene ID/symbol conversion",
  text = p(
    "Conversion table containing Gencode identifiers, Ensembl identifiers, Havana identifiers, gene symbol and gene type for mouse (version M16) and human (version 27)."
  )
)
