# Offline exact coordinate equivalence; no alternate renderer or benchmark.
args <- commandArgs(TRUE);root <- args[[1]];file <- args[[2]]
pkgload::load_all(root,quiet=TRUE)
source(file.path(root,'inst/viewer/utility_functions.R'))
source(file.path(root,'inst/viewer/core/viewer_pack.R'))
source(file.path(root,'inst/viewer/clone_contract.R'))
source(file.path(root,'inst/viewer/coordinated_views/bundle.R'))
obj <- qs2::qs_read(file);obj <- .attachExternalExpression(obj,file)
pack <- viewerPackOpen(file,obj);attr(obj,'cerebro_viewer_pack') <- pack
cells <- viewerPackCellBarcodes(pack)
spaces <- cv_build_trajectories(obj,cells)
projection <- obj$getProjection('umap')
rows <- lapply(spaces,function(s) {
  present <- is.finite(s$x)&is.finite(s$y)
  list(id=s$id,cells=length(s$x),present=sum(present),excluded=sum(!present),
    coordinateBytes=length(s$x)*8,
    canonicalEqualX=identical(as.numeric(s$x[present]),round(as.numeric(projection[[1]])[present],3)),
    canonicalEqualY=identical(as.numeric(s$y[present]),round(as.numeric(projection[[2]])[present],3)),
    maxAbsDifferenceX=max(abs(s$x[present]-round(as.numeric(projection[[1]])[present],3))),
    maxAbsDifferenceY=max(abs(s$y[present]-round(as.numeric(projection[[2]])[present],3))))
})
jsonlite::write_json(list(cells=length(cells),spaces=rows,frames=pack$manifest$trajectory_frames),args[[3]],auto_unbox=TRUE,pretty=TRUE)
