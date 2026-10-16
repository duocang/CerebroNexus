"""Offline, lossless assembly. Does not run or download benchmark data."""
from pathlib import Path
import csv, json, hashlib, shutil
from datetime import datetime, timezone

repo = Path('C:/Users/wangx/.codex/worktrees/benchmark-clock-smoke/CerebroNexus')
task = Path(__file__).resolve().parent
root = repo/'tests/benchmarks/results/expression-backends'
derived = root/'derived/20261016-scale-with-full'
supp = derived/'supplements/20261016-300k-and-human500k-99G'
stamp = datetime.now(timezone.utc).strftime('%Y%m%dT%H%M%SZ')
run_id = stamp+'-81d11e8aedfa-scale-assembled'
stage = task/run_id
stage.mkdir(exist_ok=False)
sources = [
 ('scale',root/'scale/runs/20261016T002447Z-81d11e8aedfa-scale','20261016T002447Z-81d11e8aedfa-scale','scale'),
 ('full',root/'full/runs/20261015T213433Z-81d11e8aedfa-full','20261015T213433Z-81d11e8aedfa-full','full'),
 ('scale300',supp/'scale300','20261016T061232Z-81d11e8aedfa-scale-300k','scale_supplement'),
 ('human500',supp/'human500-99G','20261016T061232Z-81d11e8aedfa-human500k-embedded-99G','feasibility')]
def read(p):
 with p.open(encoding='utf-8-sig',newline='') as f:return list(csv.DictReader(f))
def write(p,rows,fields=None):
 fields=fields or list(dict.fromkeys(k for r in rows for k in r))
 with p.open('w',encoding='utf-8',newline='') as f:
  w=csv.DictWriter(f,fieldnames=fields);w.writeheader();w.writerows(rows)
def sha(p):
 with p.open('rb') as f:return hashlib.file_digest(f,'sha256').hexdigest()
acquisitions=[];merged={n:[] for n in ['10_export.csv','20_access.csv','21_access_timings.csv']};skips=[]
for name,src,id,tier in sources:
 shutil.copytree(src,stage/'inputs'/name)
 acquisitions.append(dict(source_run_id=id,path='inputs/'+name,tier_type=tier))
 for filename in merged:
  for row in read(src/filename):
   assert row['run_id']==id
   row.update(source_run_id=id,source_profile=row.get('profile','panel_c2' if name=='full' else 'scale'),tier_type=tier,collection_id=run_id)
   if filename=='10_export.csv' and row['status']!='OK':skips.append(row)
   else:merged[filename].append(row)
shutil.copytree(derived,stage/'inputs/derived')
for filename,rows in merged.items():write(stage/filename,rows)
write(stage/'resource_skips.csv',skips)
write(stage/'acquisitions.csv',acquisitions)
schedule=[{**{k:r[k] for k in ['profile','source','n_cells','backend','export_repeat','order_position','source_run_id','source_profile','tier_type','collection_id']},'comparison':'TRUE','access_repeats':'2'} for r in merged['10_export.csv']]
write(stage/'05_schedule.csv',schedule)
write(stage/'crashes.csv',[],['run_id','profile','source','n_cells','backend','export_repeat','order_position','stage','exit_code'])
for name in ['00_probe.csv']:
 shutil.copy2(sources[0][1]/name,stage/name)
for name in ['access_per_build.csv','access_summary.csv','storage_summary.csv','full_endpoints.csv','combined_query_panel.csv','supplement_summary.csv']:
 shutil.copy2(derived/name,stage/name)
source_rows=[];resource_rows=[];query_rows=[]
for name,src,id,tier in sources:
 for file,target in [('source_manifest.csv',source_rows),('resource_check.csv',resource_rows),('query_plan_manifest.csv',query_rows)]:
  if (src/file).exists():
   target.extend([{**r,'source_run_id':id,'source_profile':r.get('profile','scale' if name!='full' else 'panel_c2'),'tier_type':tier,'collection_id':run_id} for r in read(src/file)])
write(stage/'source_manifest.csv',source_rows)
write(stage/'resource_checks_by_acquisition.csv',resource_rows)
write(stage/'query_plans_by_acquisition.csv',query_rows)
shutil.copy2(stage/'combined_query_panel.csv',stage/'query_panel.csv')
manifest=dict(key='',value='')
values={'study_id':run_id,'run_id':run_id,'profile':'scale','result_kind':'assembled','new_measurements':'0',
 'assembled_at':datetime.now(timezone.utc).isoformat(),'measurement_git_sha':'81d11e8aedfae380516f16e4f95c29728fbad02a',
 'git_sha':'81d11e8aedfae380516f16e4f95c29728fbad02a','git_branch':'paper/real-data-benchmark',
 'assembly_worktree_dirty':'true','timing_contract':'monotonic-v1','elapsed_clock':'CLOCK_MONOTONIC',
 'acquisition_environment':'see inputs/*/run_manifest.csv and inputs/human500/run.log',
 'memory_policy':'mixed historical acquisition policies; NOT measured under the new 95-percent policy',
 'repeat_policy':'43 combinations x 5 builds; Human embedded 500k x 1; 2 reads per build',
 'previous_current':(root/'scale/CURRENT').read_text().strip(),
 'full_dependency':'none: full evidence is copied under inputs/full',
 'coverage':'all configured dataset/tier/backend combinations; Human embedded 500k lacks four prospective repeats'}
write(stage/'run_manifest.csv',[dict(key=k,value=v) for k,v in values.items()])
shutil.copy2(task/'backup-record.json',stage/'backup-record.json')
(stage/'figures').mkdir()
for ext in ['png','pdf']:
 shutil.copy2(derived/f'scale_with_full_endpoints.{ext}',stage/f'figures/expression_backend_benchmark_overview.{ext}')
# These are exact existing figures, not a redraw or a new performance run.
for name in ['figure.alignment.json','figure.alignment-layout.json','figure.collision-audit.json','pdf-text-effective.json','pdf-text-audit.json','source-audit.json','QA_NOTES.md','FIGURE_CONTRACT.txt']:
 shutil.copy2(derived/name,stage/name)
(stage/'code').mkdir()
for name in ['benchmark.R','assembled_scale.R']:
 shutil.copy2(repo/'tests/benchmarks/expression-backends/scripts'/name,stage/'code'/name)
replot=(derived/'replot.R').read_text(encoding='utf-8')
start=replot.index('repo <- normalizePath');end=replot.index("skill <-",start)
replot=replot[:start]+"stage <- normalizePath(if (length(args)) args[1] else dirname(sub('^--file=', '', grep('^--file=', commandArgs(), value=TRUE)[1])))\nout <- file.path(stage, 'replot-output')\ndir.create(out, showWarnings=FALSE)\n"+replot[end:]
replot=replot.replace("source(file.path(repo, 'tests/benchmarks/expression-backends/scripts/benchmark.R'))","source(file.path(stage, 'code/benchmark.R'))")
replot=replot.replace("source(file.path(out, 'measure_rendered_panels.R'))","source(file.path(stage, 'inputs/derived/measure_rendered_panels.R'))")
replot=replot.replace("dir <- file.path(root, tier, 'runs', run_ids[[tier]])","dir <- file.path(stage, 'inputs', tier)")
replot=replot.replace("supp <- file.path(out,'supplements/20261016-300k-and-human500k-99G')","supp <- file.path(stage,'inputs/derived/supplements/20261016-300k-and-human500k-99G')")
(stage/'replot.R').write_text(replot,encoding='utf-8')
(stage/'validate.R').write_text("#!/usr/bin/env Rscript\nargs <- commandArgs(TRUE)\nstage <- normalizePath(if(length(args)) args[1] else getwd())\nsource(file.path(stage,'code/benchmark.R'))\nsource(file.path(stage,'code/assembled_scale.R'))\nbench_validate_assembled_scale(stage)\n",encoding='utf-8')
shutil.copy2(__file__,stage/'assembly_recipe.py')
summary=f'''# Scale: assembled existing measurements

Collection `{run_id}`. **No new measurements.** This is a complete grid assembled
from four real acquisitions, not one rerun of the revised Scale protocol.

- Common sizes: 1k, 10k, 50k, 100k, 300k, 500k, 1m.
- Mouse endpoint: 1,306,127 cells; Human endpoint: 1,486,324 cells.
- 44 dataset/size/backend combinations: 43 have five builds; Human embedded
  500k has **one build**. Every build has two independent access processes.
- Total: 216 successful builds, 432 access processes, 16,416 raw timings.
- Five original 500k resource skips remain in `resource_skips.csv` and the
  unchanged original input run. They are not successful repeats and are not pooled.
- Embedded above 500k was not measured. No embedded full endpoint is invented.

`run_id`, `profile`, and every original numerical measurement remain acquisition
values. `collection_id`, `source_run_id`, `source_profile`, and `tier_type` identify
the assembly and origin. `05_schedule.csv` describes actual included successful
builds; it is NOT the five-repeat prospective schedule. Original schedules,
resource checks, manifests, query plans, logs and skip rows live under `inputs/`.
`resource_checks_by_acquisition.csv` retains historical checks without inventing
a single shared resource budget. The new 95%-of-available-RAM policy applies only
to future Scale acquisitions; it is not assigned retroactively to these data.

The collection includes all evidence needed from Full and derived directories.
Those external directories are no longer needed for validation or replotting.
R packages and the nature-figure QA skill remain environment dependencies.

## Figure and statistics

`figures/expression_backend_benchmark_overview.png` and `.pdf` are byte-identical
to the verified derived figure. Rings/dashes identify reused Full endpoints;
300k points are supplemental acquisitions. The hollow Human 500k diamond is n=1,
without a five-build error bar. Access values are medians of two processes within
each build, then median/min-max over builds. Different tiers can use different
genes selected by the same rule; these are descriptive measurements, not fitted
scaling laws. Original and supplemental memory limits, priorities and host/cache
states differ. A single 500k success does not establish repeatable performance.

The old ceiling image is intentionally not activated: it lacks the new evidence.
Historical runs, derived files and Full CURRENT are unchanged. Only Scale CURRENT
and its latest figure copies are switched after validation and archival.

## Offline verification and reproduction

In the existing R environment, from this collection directory:

```sh
Rscript validate.R .
FIGURE_SKILL=/path/to/nature-figure Rscript replot.R .
```

Replot writes `replot-output/`, reads only bundled acquisition evidence and does
not measure, download, alter raw inputs, or update CURRENT. It preserves the
existing figure contract and runs the rendered panel-alignment check.
`assembly_inventory.csv` records SHA256 for all delivered inputs and artifacts.
The archived assembly recipe documents the one-time source-copy operation.
'''
(stage/'summary.md').write_text(summary,encoding='utf-8')
(stage/'README.md').write_text(summary,encoding='utf-8')
files=sorted(p for p in stage.rglob('*') if p.is_file())
write(stage/'assembly_inventory.csv',[dict(path=p.relative_to(stage).as_posix(),bytes=p.stat().st_size,sha256=sha(p)) for p in files])
record={'stage':str(stage),'run_id':run_id,'destination':str(root/'scale/runs'/run_id)}
(task/'assembly-record.json').write_text(json.dumps(record,indent=2),encoding='utf-8')
print(json.dumps(record,indent=2))
