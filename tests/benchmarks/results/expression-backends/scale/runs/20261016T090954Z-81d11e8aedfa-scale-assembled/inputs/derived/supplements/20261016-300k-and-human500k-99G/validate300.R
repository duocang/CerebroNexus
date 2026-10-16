source(file.path(Sys.getenv('BENCH_ROOT'),'benchmark.R'))
out <- Sys.getenv('STAGE'); ex <- read.csv(file.path(out,'10_export.csv'))
ac <- read.csv(file.path(out,'20_access.csv')); ti <- read.csv(file.path(out,'21_access_timings.csv'))
stopifnot(nrow(ex)==30L,all(ex$status=='OK'),nrow(ac)==60L,
  all(ac$status=='OK'),all(ac$correctness=='OK'),all(ex$n_cells==300000),all(ac$n_cells==300000))
bench_validate_timing_evidence(ex,ac,ti)
jsonlite::write_json(list(status='PASS',run_id=Sys.getenv('BENCH_RUN_ID'),
  build_count=30L,access_count=60L,timing_count=nrow(ti),profile='scale',tier_type='scale_supplement',
  build_repeats=5L,access_repeats_per_build=2L,cli_validation_passed=TRUE),
  file.path(out,'validation.json'),pretty=TRUE,auto_unbox=TRUE)
