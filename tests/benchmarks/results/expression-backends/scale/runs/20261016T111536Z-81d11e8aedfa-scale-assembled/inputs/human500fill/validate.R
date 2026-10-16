source(file.path(Sys.getenv('BENCH_ROOT'),'benchmark.R'))
ev <- Sys.getenv('SUPPLEMENT_EVIDENCE'); k <- as.integer(Sys.getenv('BUILD_REPEAT'))
ex <- read.csv(file.path(ev,'10_export.csv')); ac <- read.csv(file.path(ev,'20_access.csv')); ti <- read.csv(file.path(ev,'21_access_timings.csv'))
stopifnot(nrow(ex)==1,nrow(ac)==2,nrow(ti)==76,all(ex$status=='OK'),all(ac$status=='OK'),all(ac$correctness=='OK'),
 ex$n_genes==34176,ex$n_cells==500000,ex$nnz==1982146320,ex$export_repeat==k,all(ac$export_repeat==k),
 setequal(ac$access_repeat,1:2),all(ac$query_plan_fingerprint=='47fca3dda5ed5d1c8b7ddeb57c94d078'),all(ac$access_contract=='materialized-v2'))
bench_validate_timing_evidence(ex,ac,ti)
jsonlite::write_json(list(status='PASS',run_id=Sys.getenv('BENCH_RUN_ID'),build_repeat=k,build_repeats=1L,access_processes=2L,raw_timing_records=76L,
 memory_max_bytes=as.numeric(Sys.getenv('BENCH_MEMORY_LIMIT_BYTES')),memory_policy='99pct-available-at-batch-start; cgroup-v2; swap-disabled',
 protocol='same scale prefix/export/access; explicit supplemental acquisition outside formal conservative resource preflight'),file.path(ev,'validation.json'),pretty=TRUE,auto_unbox=TRUE)
cat('REPLICATE_VALIDATED',k,'\n')
