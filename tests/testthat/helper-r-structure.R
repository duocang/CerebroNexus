# Inspect R calls without depending on whitespace, comments or quote style.
r_test_calls <- function(source, name = NULL) {
  if (is.character(source)) {
    source <- parse(text = source, keep.source = FALSE)
  }
  result <- list()
  visit <- function(node) {
    if (
      is.call(node) &&
        (is.null(name) || identical(node[[1L]], as.name(name)))
    ) {
      result[[length(result) + 1L]] <<- node
    }
    if (is.call(node) || is.expression(node) || is.pairlist(node)) {
      for (i in seq_along(node)) {
        if (!identical(node[[i]], quote(expr = ))) visit(node[[i]])
      }
    }
  }
  visit(source)
  result
}
