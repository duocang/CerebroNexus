args <- commandArgs(TRUE)
pkgload::load_all('.',quiet=TRUE)
source('inst/viewer/utility_functions.R')
source('inst/viewer/core/viewer_pack.R')
obj <- qs2::qs_read(args[[1]])
pack <- viewerPackOpen(args[[1]],obj)
method <- obj$getMethodsForTrajectories()[[1]]
name <- obj$getNamesOfTrajectories(method)[[1]]
trajectory_data_reactive <- function() obj$getTrajectory(method,name)
index <- viewerPackTrajectoryIndex(pack,method,name)
trajectory_row_index_reactive <- function() index
frame <- viewerPackReadAsset(pack,'first_frame')
metadata <- obj$getMetaData()
viewerProjectionFirstFrameMetadata <- function() metadata
viewerPackCurrent <- function() pack
getMetaData <- function() metadata
original <- system2('git',c('show','5a511f56:inst/viewer/trajectory/server.R'),stdout=TRUE)
expr <- Filter(function(x)is.call(x)&&identical(x[[1]],as.name('<-'))&&identical(x[[2]],as.name('trajectory_cells_reactive')),parse(text=original))[[1]]
eval(expr)
baseline <- trajectory_cells_reactive
candidate <- baseline
replace_join <- function(expr) {
  if (!is.call(expr)) return(expr)
  if (identical(expr[[1]],as.name('<-')) && is.call(expr[[3]]) &&
      identical(expr[[3]][[1]],as.name('cbind')))
    return(quote(for (column in columns) cells[[column]] <- metadata[[column]][cell_index]))
  for (i in seq_along(expr)[-1]) expr[[i]] <- replace_join(expr[[i]])
  expr
}
body(candidate) <- replace_join(body(baseline))
stopifnot(!identical(body(candidate),body(baseline)))
results <- lapply(list('seurat_clusters',c('nUMI','nGene','sample','seurat_clusters')),function(columns){
  a <- system.time(x<-baseline(columns));b <- system.time(y<-candidate(columns))
  stopifnot(identical(x,y))
  list(columns=columns,baseline=unname(a[['elapsed']]),candidate=unname(b[['elapsed']]),identical=TRUE,rows=nrow(x))
})
jsonlite::write_json(results,args[[2]],auto_unbox=TRUE,pretty=TRUE)
print(results)
