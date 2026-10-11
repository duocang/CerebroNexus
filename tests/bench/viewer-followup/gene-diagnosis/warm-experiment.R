# Targeted screening only: compare complete initialization + first-row cost.
# No source replacement, no browser-performance claim.
args <- commandArgs(TRUE)
pkgload::load_all(args[[1]],quiet=TRUE)
source(file.path(args[[1]],'inst/viewer/utility_functions.R'))
times <- list()
measure <- function(label, expr) {
  start <- proc.time()
  value <- force(expr)
  times[[label]] <<- unname((proc.time()-start)[['elapsed']])
  value
}
obj <- measure('dataset', .attachExternalExpression(qs2::qs_read(args[[2]]),args[[2]]))
Rprof(paste0(args[[4]],'.Rprof'),interval=.01,gc.profiling=TRUE)
if(args[[3]]=='full') {
  value <- measure('warmup',viewerExpressionRow(obj,1L,rownames(obj$expression)[[1L]]))
} else {
  value <- measure('warmup',ncol(obj$expression))
}
row <- measure('first_gene',viewerExpressionRow(obj,seq_len(1000000L),'Gad1'))
Rprof(NULL)
jsonlite::write_json(list(arm=args[[3]],times=times,
  total=times$warmup+times$first_gene,
  rss=unname(ps::ps_memory_info()[['rss']]),length=length(row),
  values_sha256=digest::digest(row,algo='sha256'),na=sum(is.na(row)),zero=sum(row==0,na.rm=TRUE)),
  paste0(args[[4]],'.json'),pretty=TRUE,auto_unbox=TRUE)
