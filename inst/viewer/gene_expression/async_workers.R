## Pure post-processing for a small genes x cells expression slice.

expression_prepare_levels <- function(
  expression_matrix,
  mode,
  genes_present,
  rgb_genes,
  n_cells
) {
  if (!length(genes_present)) {
    if (identical(mode, "rgb")) {
      return(list(
        r = rep(0, n_cells),
        g = rep(0, n_cells),
        b = rep(0, n_cells)
      ))
    }
    return(rep(0, n_cells))
  }
  expression_matrix <- as.matrix(expression_matrix)
  if (identical(mode, "rgb")) {
    return(lapply(rgb_genes, function(gene) {
      if (is.null(gene) || !gene %in% rownames(expression_matrix)) {
        return(rep(0, n_cells))
      }
      unname(as.numeric(expression_matrix[gene, , drop = TRUE]))
    }))
  }
  if (identical(mode, "separate")) {
    return(stats::setNames(
      lapply(seq_len(nrow(expression_matrix)), function(i) {
        unname(as.numeric(expression_matrix[i, , drop = TRUE]))
      }),
      rownames(expression_matrix)
    ))
  }
  if (nrow(expression_matrix) == 1L) {
    return(unname(as.numeric(expression_matrix[1L, , drop = TRUE])))
  }
  unname(colMeans(expression_matrix))
}
