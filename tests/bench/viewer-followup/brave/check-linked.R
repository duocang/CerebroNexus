args <- commandArgs(TRUE); root <- args[[1]]; file <- args[[2]]
pkgload::load_all(root,quiet=TRUE)
source(file.path(root,'inst/viewer/utility_functions.R'))
source(file.path(root,'inst/viewer/core/viewer_pack.R'))
source(file.path(root,'inst/viewer/clone_contract.R'))
source(file.path(root,'inst/viewer/coordinated_views/bundle.R'))
obj <- qs2::qs_read(file)
obj <- .attachExternalExpression(obj,file)
pack <- viewerPackOpen(file,obj)
attr(obj,'cerebro_viewer_pack') <- pack
n <- pack$cell_count
candidate_time <- system.time(candidate <- cv_build_trajectories(obj,seq_len(n),pack))
cells <- viewerPackCellBarcodes(pack)
baseline_time <- system.time(baseline <- cv_build_trajectories(obj,cells))
stopifnot(identical(candidate,baseline),length(candidate)>0L)
if(length(args)>3L) testthat::test_local(root,filter='^coordinated-views$',reporter='summary',stop_on_failure=TRUE)
jsonlite::write_json(list(cells=n,spaces=length(candidate),exact_equal=TRUE,baseline=as.list(baseline_time),candidate=as.list(candidate_time)),args[[3]],auto_unbox=TRUE,pretty=TRUE)
