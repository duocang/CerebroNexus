args <- commandArgs(TRUE)
for (n in c("clonalHomeostasis", ".groupList", ".checkList", ".theCall", ".checkCloneSize", ".orderInput", ".dataWrangle", ".makeList", ".checkCloneCall", ".orderingFunction", ".colorizer", ".checkBlanks", ".list.input.return")) {
  cat("\nFUNCTION", n, "\n")
  print(get0(n, envir = asNamespace("scRepertoire")))
}
