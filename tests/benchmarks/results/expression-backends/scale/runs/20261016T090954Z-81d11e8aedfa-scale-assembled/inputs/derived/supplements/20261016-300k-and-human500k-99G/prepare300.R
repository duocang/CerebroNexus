source(file.path(Sys.getenv('BENCH_ROOT'),'benchmark.R'))
out <- Sys.getenv('STAGE')
specs <- BENCH_SOURCES
for (src in c('mouse_brain_e18','human_pfc_hbcc')) {
  specs[[src]]$tiers <- specs[[src]]$comparison_tiers <- 300000
}
schedule <- bench_scale_schedule(specs)
stopifnot(nrow(schedule)==30,all(schedule$n_cells==300000),all(schedule$access_repeats==2))
write.csv(schedule,file.path(out,'05_schedule.csv'),row.names=FALSE)
schedule$n_cells <- sprintf('%.0f',schedule$n_cells)
write.table(schedule,file.path(out,'05_schedule.tsv'),sep='\t',quote=FALSE,row.names=FALSE,col.names=FALSE)
old <- file.path(Sys.getenv('REPO'),'tests/benchmarks/results/expression-backends/scale/runs/20261016T002447Z-81d11e8aedfa-scale')
stopifnot(file.copy(file.path(old,'00_probe.csv'),out))
manifest <- read.csv(file.path(old,'source_manifest.csv'))
manifest$run_id <- Sys.getenv('BENCH_RUN_ID')
write.csv(manifest,file.path(out,'source_manifest.csv'),row.names=FALSE)
writeLines('run_id,profile,source,n_cells,backend,export_repeat,order_position,stage,exit_code',file.path(out,'crashes.csv'))
jsonlite::write_json(list(type='additional scale tier',n_cells=300000,
  source_metadata_origin=basename(old),metadata_reused=TRUE,
  source_files_rehashed_before_measurements=TRUE,memory_max_gib=90,swap_max_bytes=0,
  run_id=Sys.getenv('BENCH_RUN_ID'),original_runs_and_CURRENT_unchanged=TRUE),
  file.path(out,'supplement-provenance.json'),pretty=TRUE,auto_unbox=TRUE)
