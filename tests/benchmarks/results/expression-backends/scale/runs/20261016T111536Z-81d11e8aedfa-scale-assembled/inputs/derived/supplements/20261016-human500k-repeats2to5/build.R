# User-authorized missing build replicates 2--5; same original scale prefix/export functions.
# Reuses the scale prefix and export implementation helpers, under a kernel
# memory limit instead of changing/forging the formal scale resource assessment.
suppressPackageStartupMessages(library(CerebroNexus))
source(file.path(Sys.getenv('BENCH_ROOT'), 'benchmark.R'))
ev <- Sys.getenv('SUPPLEMENT_EVIDENCE'); scratch <- Sys.getenv('SUPPLEMENT_SCRATCH')
src <- 'human_pfc_hbcc'; nc <- 500000; spec <- BENCH_SOURCES[[src]]
plan <- readRDS(file.path(ev,'query_plan.rds'))
stopifnot(plan$n_cells==nc, plan$nnz==1982146320,
          plan$nnz <= .Machine$integer.max,
          plan$query_plan_fingerprint=='47fca3dda5ed5d1c8b7ddeb57c94d078')
template <- read.csv(file.path(Sys.getenv('REPO'), 'tests/benchmarks/results/expression-backends/scale/runs/20261016T002447Z-81d11e8aedfa-scale/10_export.csv'))
row <- template[1,,drop=FALSE]; row[1,] <- NA
row$run_id <- Sys.getenv('BENCH_RUN_ID'); row$profile <- 'scale'
row$source <- src; row$label <- spec$label; row$n_cells <- nc
row$backend <- 'embedded'; row$export_repeat <- as.integer(Sys.getenv('BUILD_REPEAT')); row$order_position <- row$export_repeat
row$status <- 'OK'; row$query_plan_fingerprint <- plan$query_plan_fingerprint
dest <- file.path(ev,'10_export.csv'); stage <- 'read'
tryCatch({
  bench_msg('Supplemental bounded build: materializing the exact 500k scale prefix')
  gc(reset=TRUE)
  row$read_secs <- bench_time(m <- bench_read_export_matrix(spec,
    file.path(scratch,'sources',basename(spec$url)),nc,'scale'))
  row$n_genes <- nrow(m); row$nnz <- length(m@x)
  stopifnot(ncol(m)==plan$n_cells,nrow(m)==plan$n_genes,length(m@x)==plan$nnz)
  bench_msg('Materialized %d genes x %d cells; nnz %.0f',nrow(m),ncol(m),length(m@x))
  stage <- 'seurat'; row$seurat_secs <- bench_time(obj <- bench_make_seurat(m))
  rm(m); gc(verbose=FALSE)
  stage <- 'export'; crb <- file.path(scratch,'bench.crb')
  bench_msg('Exporting embedded')
  row$export_secs <- bench_time(CerebroNexus::exportFromSeurat(
    object=obj,assay='RNA',slot='counts',file=crb,
    experiment_name=sprintf('%s_%.0f',src,nc),organism=spec$organism,
    groups=c('sample','cluster'),nUMI='nUMI',nGene='nGene',
    expression_matrix_mode='embedded',viewer_binary='never',verbose=FALSE))
  row$crb_mb <- bench_path_mb(crb); row$sibling_mb <- 0; row$total_mb <- row$crb_mb
  row$rss_mb <- bench_rss_mb(); row$peak_rss_mb <- bench_peak_rss_mb()
  g <- gc(); row$r_peak_mb <- sum(g[,ncol(g)],na.rm=TRUE)
  bench_append_row(dest,row)
  bench_msg('BUILD_OK %.1f MiB; peak %.1f MiB',row$total_mb,row$peak_rss_mb)
},error=function(e) {
  row$status <- sprintf('FAILED(%s): %s',stage,conditionMessage(e))
  bench_append_row(dest,row); bench_msg('%s',row$status); quit(status=1L)
})
