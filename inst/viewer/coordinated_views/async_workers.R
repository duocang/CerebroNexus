## Pure preparation of a genes x cells slice for linked-view colour updates.

cv_prepare_gene_values <- function(expression_matrix, genes, cells) {
  expression_matrix <- as.matrix(expression_matrix)
  if (is.null(rownames(expression_matrix))) {
    rownames(expression_matrix) <- genes[seq_len(nrow(expression_matrix))]
  }
  column_index <- if (is.null(colnames(expression_matrix))) {
    seq_len(ncol(expression_matrix))
  } else {
    match(cells, colnames(expression_matrix))
  }
  stats::setNames(
    lapply(genes, function(gene) {
      row_index <- match(gene, rownames(expression_matrix))
      if (is.na(row_index)) {
        return(rep(0, length(cells)))
      }
      values <- as.numeric(expression_matrix[row_index, column_index])
      values[is.na(values)] <- 0
      values
    }),
    genes
  )
}
