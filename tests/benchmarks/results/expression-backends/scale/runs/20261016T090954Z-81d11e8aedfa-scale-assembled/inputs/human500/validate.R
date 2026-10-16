source(file.path(Sys.getenv('BENCH_ROOT'),'benchmark.R'))
ev <- Sys.getenv('SUPPLEMENT_EVIDENCE')
ex <- read.csv(file.path(ev,'10_export.csv'))
ac <- read.csv(file.path(ev,'20_access.csv'))
ti <- read.csv(file.path(ev,'21_access_timings.csv'))
stopifnot(nrow(ex)==1,nrow(ac)==2,all(ex$status=='OK'),all(ac$status=='OK'),
          all(ac$correctness=='OK'),all(ac$query_plan_fingerprint=='47fca3dda5ed5d1c8b7ddeb57c94d078'),
          all(ac$access_contract=='materialized-v2'))
bench_validate_timing_evidence(ex,ac,ti)
jsonlite::write_json(list(status='PASS',run_id=Sys.getenv('BENCH_RUN_ID'),
  experiment_type='bounded supplemental feasibility; not a formal scale run',
  build_repeats=1L,access_processes=2L,raw_timing_records=nrow(ti),
  memory_max_gib=99,swap_max_bytes=0,
  query_plan_fingerprint='47fca3dda5ed5d1c8b7ddeb57c94d078'),
  file.path(ev,'validation.json'),pretty=TRUE,auto_unbox=TRUE)
cat('SUPPLEMENT_VALIDATED\n')
