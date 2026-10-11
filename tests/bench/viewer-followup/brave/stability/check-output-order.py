import json,pathlib,sys
p=pathlib.Path(__file__).resolve().parent
id='coordviews_selected_cells_UI'
for name in sys.argv[1:]:
 d=json.loads((p/(name+'.json')).read_text());state='initial';violations=[];counts={}
 for e in d['lifecycle']:
  counts[e['kind']]=counts.get(e['kind'],0)+1
  if e['kind']!='wire':continue
  m=e['packet'];recalc=m.get('recalculating',{})
  if recalc.get('name')==id:state='running' if recalc['status']=='recalculating' else 'idle'
  if m.get('progress',{}).get('message',{}).get('id')==id:state='invalidated'
  if id in m.get('values',{}) or id in m.get('errors',{}):
   if state!='idle':violations.append({'time':e['time'],'state':state,'kind':'value' if id in m.get('values',{}) else 'error'})
   state='value' if id in m.get('values',{}) else 'error'
 server_state='initial';server_violations=[]
 for line in (p/(name+'.stages.jsonl')).read_text().splitlines():
  s=json.loads(line)
  if s['name']!='selected_output_message':continue
  m=s['detail'];r=m.get('recalculating',{})
  if r.get('name')==id:server_state='running' if r['status']=='recalculating' else 'idle'
  if m.get('progress',{}).get('message',{}).get('id')==id:server_state='invalidated'
  values=m.get('values',[]);errors=m.get('errors',[])
  if id in values or id in errors:
   if server_state!='idle':server_violations.append({'mono':s['mono'],'state':server_state,'kind':'value' if id in values else 'error'})
   server_state='value' if id in values else 'error'
 result={'run':name,'browser_saved_violations':violations,'server_full_violations':server_violations,'counts':counts,'max_nodes':max([e.get('nodes',0) for e in d['lifecycle']]),'pages':[]}
 for op in d['ops']:
  if op['name']=='page' and op['value']=='#shiny-tab-coordinated_views':
   ev=[e for e in op['events'] if 'complete' in e['detail']]
   result['pages'].append({'first_ms':ev[0]['ms'] if ev else None,'last_complete_ms':max([e['ms'] for e in ev if e['detail']['complete']],default=None),'ws_bytes':sum(m['bytes'] for m in op['messages'])})
 (p/(name+'.order.json')).write_text(json.dumps(result,indent=2)+'\n');print(json.dumps(result))
