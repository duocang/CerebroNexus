import json,pathlib,statistics
p=pathlib.Path(__file__).resolve().parent
out={'trajectory':[]}
for f in sorted(p.glob('trajectory-[BCD][1-5].json')):
 d=json.loads(f.read_text());ops=d['ops']
 page=next(o for o in ops if o['name']=='page' and o['value']=='#shiny-tab-trajectory')
 default=next(o for o in ops if o['name']=='trajectory_point_color' and o['value']=='seurat_clusters')
 ev=next(e for e in default['events'] if e['detail'].get('timing'))
 fresh=default['start']-page['start']+ev['ms']
 state=next((o for o in ops if o['name']=='trajectory_point_color' and o['value']=='state'),None)
 returns=[o for o in ops if o['name']=='page' and o['value']=='#shiny-tab-trajectory' and o is not page]
 rss=f.with_suffix('.rss').read_text().split()
 stages=[json.loads(x) for x in f.with_suffix('.stages.jsonl').read_text().splitlines()]
 start=next(s['mono'] for s in stages if s['name']=='trajectory_cells_reactive' and s['phase']=='start')
 end=next(s['mono'] for s in stages if s['name']=='trajectory_cells_reactive' and s['phase']=='end')
 out['trajectory'].append({'run':f.stem,'primary_draw_ms':fresh,'cell_prep_ms':end-start,'selector_init_ms':default['start']-page['start'],
   'warm_state_ms':state['events'][0]['ms'] if state and state['events'] else None,
   'revisit_ms':returns[0]['events'][0]['ms'] if returns and returns[0]['events'] else None,
   'points':ev['detail']['renderedPointCount'],'timing':ev['detail']['timing'],
   'initial_ws_bytes':sum(m['bytes'] for m in default['messages']),
   'r_peak_MiB':int(rss[-1])/2**20 if rss else None,'js_heap_peak_MiB':default.get('heapPeak',0)/2**20,
   'errors':d['errors']})
mf=p/'lifecycle-C.json'
if mf.exists():
 d=json.loads(mf.read_text());browser_mem={m['label']:m['browser'] for m in d['memory']}
 mem=[json.loads(line) for line in (p/'lifecycle-C.memory.jsonl').read_text().splitlines()]
 out['memory']=[{'label':m['label'],'dataset':m['context'],'rss_MiB':m['rss']/2**20,
   'gc_live_MiB':sum(row[1] for row in m['gc']),'caches':m['caches'],
   'wrong_epoch_accepted':browser_mem.get(m['label'],{}).get('wrongEpochAccepted'),'stale':browser_mem.get(m['label'],{}).get('staleChecks')} for m in mem]
 out['r_peak_MiB']=int((p/'lifecycle-C.rss').read_text().split()[-1])/2**20
 out['js_heap_peak_MiB']=max(o.get('heapPeak',0) for o in d['ops'])/2**20
groups={'original_baseline':['B1','B2','B3'],'column_candidate':['C1','C2','C3'],
 'summary_baseline':['C3','C4','C5'],'summary_candidate':['D1','D2','D3'],
 'final_candidate':['D1','D2','D3','D4','D5']}
out['comparisons']={}
for group,names in groups.items():
 rows=[r for r in out['trajectory'] if r['run'].removeprefix('trajectory-') in names]
 if rows:
  out['comparisons'][group]={'runs':[r['run'] for r in rows]}
  for metric in ['primary_draw_ms','r_peak_MiB','js_heap_peak_MiB','initial_ws_bytes']:
   v=[r[metric] for r in rows if r[metric] is not None]
   out['comparisons'][group][metric]={'median':statistics.median(v),'min':min(v),'max':max(v),'n':len(v)}
(p/'summary.json').write_text(json.dumps(out,indent=2)+'\n')
for row in out['trajectory']:print({k:v for k,v in row.items() if k not in ['timing','errors']})
for arm in ['B','C','D']:
 rows=[r for r in out['trajectory'] if r['run'].startswith('trajectory-'+arm)]
 if rows:print(arm,'n',len(rows),'median',statistics.median(r['primary_draw_ms'] for r in rows),'range',[min(r['primary_draw_ms'] for r in rows),max(r['primary_draw_ms'] for r in rows)])
