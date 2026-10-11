from pathlib import Path
import json,gzip
p=Path(__file__).resolve().parent
d=json.loads((p/'closeout.json').read_text() if (p/'closeout.json').exists() else gzip.decompress((p/'closeout.json.gz').read_bytes()))
ops=d['ops']; painted=d['painted']; rows=[]
for i,o in enumerate(ops):
 name,value=o['name'],o['value']
 if not ((name=='expression_genes_input' and value==['Gad1']) or (name=='page' and value in ['#shiny-tab-coordinated_views','#shiny-tab-trajectory']) or (name=='trajectory_point_color' and value=='state')): continue
 start=o['start']
 if name=='page' and value=='#shiny-tab-coordinated_views':
  events=[e for e in o['events'] if e['detail'].get('painted') and e['detail'].get('primaryReady')]
  first=events[0]['ms'] if events else None
  complete=max((e['ms'] for e in events if e['detail'].get('complete')),default=None)
  row=dict(name=name,value=value,start=start,first_usable_ms=first,complete_ms=complete,summary=o.get('linked'))
 else:
  id='expression_projection' if name=='expression_genes_input' else 'trajectory_projection'
  ready=next((v for v in painted if v['detail']['id']==id and v['eventAt']>=start),None)
  row=dict(name=name,value=value,start=start,first_usable_ms=round(ready['eventAt']-start,3) if ready else None,
           complete_ms=o.get('groupPaint'),ready=ready)
 row['messages']=o['messages'];row['heap_peak']=o.get('heapPeak');row['events']=o['events'];rows.append(row)
gene=next(r for r in rows if r['value']==['Gad1'])
for row in rows:
 end=next((o['start'] for o in ops if o['start']>row['start'] and (o['name']=='page' or (o['name']=='trajectory_point_color' and o['value']=='state'))),float('inf'))
 window=[o for o in ops if row['start']<=o['start']<end]
 row['window_end']=None if end==float('inf') else end
 row['window_payload_bytes']=sum(m['bytes'] for o in window for m in o['messages'])
 row['window_heap_peak']=max((o.get('heapPeak',0) for o in window),default=0)
 row['window_events']=[dict(ms=o['start']+e['ms']-row['start'],detail=e['detail']) for o in window for e in o['events']]
 row['window_note']='Includes automatic selector operations and any later interactions before the next page/State action. State window includes the real brush and its demanded auxiliary payload; it is not first-entry traffic.'
gene['diag']=[dict(name=e['name'],ms=round(e['at']-gene['start'],3),detail=e.get('detail')) for e in d['diag'] if gene['start']<=e['at']<gene['start']+gene['complete_ms'] and e['name'] not in ['socket_send','socket_receive']]
result=dict(observations=rows,context=d['context'],errors=d['errors'],viewport=d['viewport'],ua=d['ua'],
 r_rss_bytes=[int(x) for x in (p/'closeout.rss').read_text().split()],
 browser_all_process_rss_observation_bytes=3853611008,browser_rss_scope='23 Brave processes, including other user tabs; not isolated test-page RSS or a peak')
(p/'closeout-summary.json').write_text(json.dumps(result,indent=2)+'\n')
for r in rows: print(r['name'],r['value'],r['first_usable_ms'],r['complete_ms'])
print('Gene stages',[(e['name'],e['ms']) for e in gene['diag'] if e['name'] in ['decode_start','first_draw_end','gpu_idle_done','raf1_fired','raf2_fired','painted']])
