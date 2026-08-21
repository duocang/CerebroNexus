expressionColorScale <- function(name) {
  if (!identical(name, "Cerebro orange")) {
    return(name)
  }
  list(
    list(0, "#aeb5bb"),
    list(0.08, "#f7c89d"),
    list(0.38, "#f49a4c"),
    list(0.7, "#e75f25"),
    list(1, "#9f251f")
  )
}

expressionReverseColorScale <- function(name) {
  !identical(name, "Cerebro orange")
}
