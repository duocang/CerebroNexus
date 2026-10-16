#!/usr/bin/env Rscript
# Derived figures only; raw runs and CURRENT are never modified.
suppressPackageStartupMessages({library(ggplot2); library(patchwork); library(dplyr); library(jsonlite)})
args <- commandArgs(TRUE)
stage <- normalizePath(if (length(args)) args[1] else dirname(sub('^--file=', '', grep('^--file=', commandArgs(), value=TRUE)[1])))
out <- file.path(stage, 'replot-output')
dir.create(out, showWarnings=FALSE)
skill <- Sys.getenv('FIGURE_SKILL', '/mnt/c/Users/wangx/.codex/skills/nature-figure')
source(file.path(stage, 'code/benchmark.R'))
source(file.path(skill, 'scripts/panel_alignment.R'))
source(file.path(stage, 'inputs/derived/measure_rendered_panels.R'))
run_ids <- c(full='20261015T213433Z-81d11e8aedfa-full', scale='20261016T002447Z-81d11e8aedfa-scale')
full_cells <- c(mouse_brain_e18=1306127, human_pfc_hbcc=1486324)
read_run <- function(tier) {
  dir <- file.path(stage, 'inputs', tier)
  read <- function(name) read.csv(file.path(dir, name), stringsAsFactors=FALSE)
  manifest <- read('run_manifest.csv'); values <- setNames(manifest$value, manifest$key)
  stopifnot(values[['git_sha']] == '81d11e8aedfae380516f16e4f95c29728fbad02a',
            values[['timing_contract']] == 'monotonic-v1', values[['elapsed_clock']] == 'CLOCK_MONOTONIC',
            values[['git_dirty']] == 'false', values[['run_id']] == run_ids[[tier]])
  inventory <- read('evidence_manifest.csv')
  paths <- file.path(dir, inventory$path)
  stopifnot(all(file.info(paths)$size == inventory$bytes), all(unname(tools::md5sum(paths)) == inventory$md5))
  exports <- read('10_export.csv'); access <- read('20_access.csv'); timings <- read('21_access_timings.csv')
  bench_validate_timing_evidence(exports, access, timings)
  bench_validate_results(read('05_schedule.csv'), exports, access, read('crashes.csv'), bench_profile(values[['profile']]))
  stopifnot(all(access$correctness == 'OK'), all(access$access_contract == 'materialized-v2'))
  if (tier == 'full') stopifnot(all(exports$n_cells == full_cells[exports$source]), all(exports$backend %in% c('h5','bpcells')))
  if (tier == 'scale') stopifnot(setequal(unique(exports$n_cells), c(1000,10000,50000,100000,500000,1000000)))
  decorate <- function(x) {x$source_run_id <- run_ids[[tier]]; x$tier_type <- tier; x}
  list(exports=decorate(exports), access=decorate(access), query_panel=decorate(read('query_panel.csv')),
       manifest=manifest, timings=nrow(timings))
}
runs <- lapply(names(run_ids), read_run); names(runs) <- names(run_ids)
exports <- bind_rows(lapply(runs, function(x) x$exports))
access <- bind_rows(lapply(runs, function(x) x$access))
query_panel <- bind_rows(lapply(runs, function(x) x$query_panel))
stopifnot(nrow(exports)==190L, sum(exports$status=='OK')==185L, nrow(access)==370L)
# Supplementary acquisitions remain separately identifiable from both original runs.
supp <- file.path(stage,'inputs/derived/supplements/20261016-300k-and-human500k-99G')
inventory <- fromJSON(file.path(supp,'input-manifest.json'))
for (i in seq_len(nrow(inventory))) {
  path <- file.path(supp,inventory$path[i])
  stopifnot(file.info(path)$size==inventory$bytes[i],
    digest::digest(file=path,algo='sha256')==inventory$sha256[i])
}
read_supplement <- function(folder,id,tier_type,n_build,n_access,n_cells) {
  dir <- file.path(supp,folder)
  read <- function(name) read.csv(file.path(dir,name),stringsAsFactors=FALSE)
  val <- fromJSON(file.path(dir,'validation.json')); stopifnot(val$status=='PASS',val$run_id==id)
  ex <- read('10_export.csv'); ac <- read('20_access.csv'); ti <- read('21_access_timings.csv')
  stopifnot(nrow(ex)==n_build,nrow(ac)==n_access,all(ex$status=='OK'),all(ac$status=='OK'),
    all(ex$run_id==id),all(ac$run_id==id),all(ex$n_cells==n_cells),all(ac$n_cells==n_cells),
    all(ac$correctness=='OK'),all(ac$access_contract=='materialized-v2'))
  for (field in c('row','block','subset_row','subset_block'))
    stopifnot(all(ac[[paste0(field,'_fingerprint')]]==ac[[paste0('reference_',field,'_fingerprint')]]))
  bench_validate_timing_evidence(ex,ac,ti)
  if (folder=='scale300') {
    manifest <- read('run_manifest.csv'); values <- setNames(manifest$value,manifest$key)
    stopifnot(values[['git_sha']]=='81d11e8aedfae380516f16e4f95c29728fbad02a',values[['git_dirty']]=='false',
      values[['profile']]=='scale',values[['elapsed_clock']]=='CLOCK_MONOTONIC',
      all(table(ex$source,ex$backend)==5L),all(read('resource_check.csv')$embedded_safe))
    bench_validate_results(read('05_schedule.csv'),ex,ac,read('crashes.csv'),bench_profile('scale'))
    qp <- read('query_panel.csv')
  } else {
    stopifnot(all(ex$source=='human_pfc_hbcc'),all(ex$backend=='embedded'),ex$nnz==1982146320,
      all(ac$query_plan_fingerprint=='47fca3dda5ed5d1c8b7ddeb57c94d078'),
      identical(sort(as.integer(ac$access_repeat)),1:2),val$build_repeats==1L)
    reference <- access[access$source=='human_pfc_hbcc' & access$n_cells==500000,]
    stopifnot(nrow(reference)==20L,all(reference$query_plan_fingerprint==ac$query_plan_fingerprint[1]))
    for (field in c('row','block','subset_row','subset_block'))
      stopifnot(all(reference[[paste0(field,'_fingerprint')]]==ac[[paste0(field,'_fingerprint')]][1]))
    qp <- read('query_panel_from_prior_attempt.csv')
    qp$query_plan_source_run_id <- qp$run_id
    qp$run_id <- id
  }
  decorate <- function(x) {x$source_run_id<-id; x$tier_type<-tier_type; x}
  list(exports=decorate(ex),access=decorate(ac),query_panel=decorate(qp),timings=nrow(ti))
}
extra300 <- read_supplement('scale300','20261016T061232Z-81d11e8aedfa-scale-300k','scale_supplement',30L,60L,300000)
extra500 <- read_supplement('human500-99G','20261016T061232Z-81d11e8aedfa-human500k-embedded-99G','feasibility',1L,2L,500000)
exports <- bind_rows(exports,extra300$exports,extra500$exports)
access <- bind_rows(access,extra300$access,extra500$access)
query_panel <- bind_rows(query_panel,extra300$query_panel,extra500$query_panel)
stopifnot(nrow(exports)==221L,sum(exports$status=='OK')==216L,nrow(access)==432L)
stopifnot(!anyDuplicated(access[c('source_run_id','source','n_cells','backend','export_repeat','access_repeat')]))
for (n in c('exports','access','query_panel')) write.csv(get(n), file.path(out,paste0('combined_',n,'.csv')),row.names=FALSE)
write.csv(exports[exports$status!='OK',], file.path(out,'resource_skips.csv'),row.names=FALSE)
metrics <- c('hot_p50_secs','block_secs','rss_mb')
build_access <- access %>% group_by(source_run_id,profile,tier_type,source,n_cells,backend,export_repeat) %>%
  summarise(n_access=n(), across(all_of(metrics), median), .groups='drop')
stopifnot(all(build_access$n_access==2L))
write.csv(build_access,file.path(out,'access_per_build.csv'),row.names=FALSE)
summary_access <- build_access %>% group_by(source_run_id,profile,tier_type,source,n_cells,backend) %>%
  summarise(n_builds=n(), across(all_of(metrics),list(median=median,min=min,max=max)),.groups='drop')
summary_export <- exports %>% filter(status=='OK') %>% group_by(source_run_id,profile,tier_type,source,n_cells,backend) %>%
  summarise(n_builds=n(), across(total_mb,list(median=median,min=min,max=max)),.groups='drop')
for (summary in list(summary_access,summary_export))
  stopifnot(all(summary$n_builds==ifelse(summary$tier_type=='feasibility',1L,5L)))
write.csv(summary_access,file.path(out,'access_summary.csv'),row.names=FALSE)
write.csv(summary_export,file.path(out,'storage_summary.csv'),row.names=FALSE)
write.csv(summary_access[summary_access$tier_type=='full',],file.path(out,'full_endpoints.csv'),row.names=FALSE)
validation <- list(status='PASS',full_run=run_ids[['full']],scale_run=run_ids[['scale']],
  supplement_300k_run=extra300$exports$run_id[1],feasibility_500k_run=extra500$exports$run_id[1],
  successful_builds=sum(exports$status=='OK'),resource_skips=sum(exports$status!='OK'),access_processes=nrow(access),
  raw_timing_records=sum(vapply(runs,function(x) x$timings,integer(1)))+extra300$timings+extra500$timings,
  clocks='CLOCK_MONOTONIC',profiles_share_query_strategy=TRUE,query_genes_fixed_across_tiers=FALSE,
  summary='Two read processes are summarized within each build; median/min-max across five builds except Human embedded 500k: one build, no across-build uncertainty.')
write_json(validation,file.path(out,'data-validation.json'),pretty=TRUE,auto_unbox=TRUE)

cols <- c(embedded='#B4553F',bpcells='#D9A03C',h5='#3F7F93')
shapes <- c(embedded=17,bpcells=16,h5=15)
display <- c(embedded='Embedded',bpcells='BPCells',h5='HDF5')
sources <- c('mouse_brain_e18','human_pfc_hbcc')
titles <- c(mouse_brain_e18='Mouse brain E18',human_pfc_hbcc='Human PFC (HBCC)')
metric_titles <- c(hot_p50_secs='Warmed single-gene read (s)',block_secs='12-gene block read (s)',rss_mb='Loaded process RSS (GiB)',total_mb='Total on-disk storage (GiB)')
plots <- list()
for (metric in names(metric_titles)) {
  df <- if (metric=='total_mb') summary_export else summary_access
  df$mid <- df[[paste0(metric,'_median')]]; df$lo <- df[[paste0(metric,'_min')]]; df$hi <- df[[paste0(metric,'_max')]]
  if (metric %in% c('rss_mb','total_mb')) df[c('mid','lo','hi')] <- df[c('mid','lo','hi')]/1024
  stopifnot(all(is.finite(as.matrix(df[c('mid','lo','hi')]))), all(as.matrix(df[c('mid','lo','hi')])>0))
  limits <- range(df$lo,df$hi)*c(0.75,1.35)
  for (src in sources) {
    d <- df[df$source==src,]; d$backend <- factor(d$backend,levels=names(cols))
    stopifnot(all(is.finite(d$n_cells)), all(d$n_cells > 0))
    endpoint <- d[d$tier_type=='full',]
    replicated <- d[d$tier_type!='feasibility',]
    single <- d[d$tier_type=='feasibility',]
    bridge <- d[d$n_cells>=1e6 & d$backend!='embedded',]
    p <- ggplot(d,aes(n_cells,mid,colour=backend,shape=backend,group=backend)) +
      geom_vline(xintercept=full_cells[[src]],colour='grey78',linewidth=0.3,linetype='dotted') +
      geom_line(data=d[d$tier_type %in% c('scale','scale_supplement'),],linewidth=0.5) +
      geom_line(data=bridge,linewidth=0.5,linetype='dashed') +
      geom_errorbar(data=replicated,aes(ymin=lo,ymax=hi),width=0,linewidth=0.4) +
      geom_point(data=replicated,size=1.8) +
      geom_point(data=single,shape=23,fill='white',size=2.8,stroke=0.7,show.legend=FALSE) +
      geom_point(data=endpoint,shape=21,fill=NA,colour='black',size=3.1,stroke=0.45,show.legend=FALSE) +
      scale_colour_manual(values=cols,labels=display,drop=FALSE) +
      scale_shape_manual(values=shapes,labels=display,drop=FALSE) +
      scale_x_log10(limits=c(800,1.85e6),breaks=c(1e3,1e4,5e4,1e5,3e5,5e5,1e6),labels=c('1k','10k','50k','100k','300k','500k','1m'),expand=c(0,0)) +
      scale_y_log10(limits=limits,breaks=if(metric=='rss_mb') c(0.5,1,2,5,10,20,50) else scales::breaks_log(),
        labels=function(x) format(x,trim=TRUE,scientific=FALSE)) +
      labs(title=unname(titles[src]),subtitle=sprintf('Full dataset: %s cells',format(full_cells[[src]],big.mark=',',scientific=FALSE)),
           x='Cells (log scale)',y=unname(metric_titles[metric]),colour=NULL,shape=NULL) +
      theme_minimal(base_size=9,base_family='sans') +
      theme(panel.grid.minor=element_blank(),panel.grid.major=element_line(colour='grey92',linewidth=0.3),
        axis.text=element_text(size=7.5,colour='grey20'),axis.title=element_text(size=9),
        axis.text.x=element_text(angle=45,hjust=1),
        plot.title=element_text(size=10,face='bold'),plot.subtitle=element_text(size=8),
        legend.text=element_text(size=9),legend.position='bottom',plot.margin=margin(5,10,5,5))
    plots[[length(plots)+1L]] <- p
  }
}
figure <- wrap_plots(plots,ncol=2,guides='collect') + plot_annotation(
  title='Expression backend scaling through the full datasets',tag_levels='a',
  caption=paste(
    'Both axes are logarithmic. Rings/dashed extensions: reused Full dataset measurements. 300k: new measurements.',
    'Filled points: median across 5 builds; whiskers: min-max. Access: median of 2 processes within each build.',
    'Hollow diamonds: Human embedded 500k, 1 build and 2 reads under a 99 GiB cap; no across-build uncertainty.',
    'Same query strategy at every tier; selected genes vary with tier. Lines join observations, not fitted scaling laws.',
    'Original Human embedded 500k resource skips retained. Embedded 1m/full not scheduled. RSS is after load.',sep='\n'),
  theme=theme(plot.title=element_text(size=14,face='bold'),plot.caption=element_text(size=8,hjust=0,lineheight=1.2),
    plot.margin=margin(10,10,10,10))) & theme(legend.position='bottom',plot.tag=element_text(face='bold',size=11))
saveRDS(figure,file.path(out,'figure.rds'))
require_patchwork_panel_alignment(figure,
  manifest_path=file.path(out,'figure.alignment-layout.json'),report_path=file.path(out,'figure.alignment.json'),
  width_in=240/25.4,height_in=260/25.4,panel_ids=letters[1:8],
  audit_script=file.path(skill,'scripts/audit_panel_alignment.py'),python='/usr/bin/python3',
  overlay_svg=file.path(out,'figure.alignment.svg'))
pdf_temp <- tempfile(fileext='.pdf')
if (identical(Sys.getenv('FIGURE_PDF_DEVICE','pdf'),'cairo')) {
  grDevices::cairo_pdf(pdf_temp,width=240/25.4,height=260/25.4,family='sans',bg='white')
} else {
  grDevices::pdf(pdf_temp,width=240/25.4,height=260/25.4,family='Helvetica',useDingbats=FALSE,bg='white')
}
print(figure)
grDevices::dev.off()
stopifnot(file.copy(pdf_temp,file.path(out,'scale_with_full_endpoints.pdf'),overwrite=TRUE))
unlink(pdf_temp)
if (requireNamespace('svglite',quietly=TRUE)) {
  svglite::svglite(file.path(out,'scale_with_full_endpoints.svg'),width=240/25.4,height=260/25.4)
  print(figure)
  grDevices::dev.off()
} else message('Optional SVG omitted: svglite unavailable; requested PDF/PNG are exported.')
ggsave(file.path(out,'scale_with_full_endpoints.png'),figure,width=240,height=260,units='mm',dpi=350,bg='white')
cat('Derived plotting and data checks completed.\n')
