import json,pathlib,statistics,gzip
p=pathlib.Path(__file__).resolve().parent
result=[]
for f in sorted({p/x.name.removesuffix('.gz') for x in p.glob('run[1-5].json*')}):
 d=json.loads(f.read_text() if f.exists() else gzip.decompress(pathlib.Path(str(f)+'.gz').read_bytes())); ev=d['events']
 sent=next(e for e in ev if e['name']=='socket_send' and 'Gad1' in str(e['detail'].get('data',{}).get('expression_genes_input','')))
 inp=next((e for e in reversed(ev) if e['name']=='input' and e['detail']['name']=='expression_genes_input' and e['at']<=sent['at']),None)
 start=(inp or sent)['at']
 clock=next((c for c in d['clock'] if 'end' in c),None)
 offset=clock['mono']-(clock['start']+clock['end'])/2 if clock else None
 stages=[json.loads(x) for x in f.with_suffix('.stages.jsonl').read_text().splitlines()]
 arrived=next(s['mono'] for s in stages if s['name']=='manageInputs' and s['phase']=='start' and s['detail'].get('gene')=='Gad1')
 server_zero=start+offset if offset is not None else arrived
 browser=[dict(name=e['name'],ms=round(e['at']-start,2),detail=e.get('detail')) for e in ev if start<=e['at']<start+10000 and e['name'] not in ('output','socket_send','socket_receive')]
 server=[dict(name=s['name'],phase=s['phase'],ms=round(s['mono']-server_zero,2),detail=s['detail']) for s in stages if server_zero-2500<s['mono']<server_zero+9000]
 binary=[e for e in ev if e['name']=='socket_receive' and start<=e['at']<start+10000 and e['detail']['bytes']>500000]
 row={'run':f.stem,'origin':'input' if inp else 'socket_send','clock_uncertainty_ms':(clock['end']-clock['start'])/2 if clock else None,
  'input_to_socket_ms':sent['at']-start,'input_to_R_ms':arrived-server_zero,'viewport':d['viewport'],
  'browser':browser,'server':server,'large_messages':[dict(ms=e['at']-start,**e['detail']) for e in binary]}
 row['clock_caveat']='Midpoint estimates only; cross-clock alignment violates socket causality in runs 2/3. Do not use these absolute server ms for precise latency subtraction. Use same-clock durations.'
 observed=next(s['mono'] for s in stages if s['name']=='gene_input' and s['phase']=='received' and s.get('detail')=='Gad1')
 row['server_relative_to_gene_observer']=[dict(name=s['name'],phase=s['phase'],ms=s['mono']-observed,detail=s['detail']) for s in stages if observed-2500<s['mono']<observed+9000]
 row['clock_probes']=d['clock']
 log_file=f.with_suffix('.reactlog.json')
 log=json.loads(log_file.read_text() if log_file.exists() else gzip.decompress(pathlib.Path(str(log_file)+'.gz').read_bytes()))
 change=next(e for e in log if e['action']=='valueChange' and e.get('reactId')=='r1$expression_genes_input' and 'Gad1' in str(e.get('value')))
 labels={e['reactId']:e.get('label') for e in log if e['action']=='define'};stack={};intervals=[]
 for e in log:
  if e['action']=='enter':stack[e['reactId']]=e
  elif e['action']=='exit' and e['reactId'] in stack:
   a=stack.pop(e['reactId']);beg=(a['time']-change['time'])*1000+row['input_to_R_ms'];dur=(e['time']-a['time'])*1000
   if -2500<beg<9000 and dur>20:intervals.append({'id':e['reactId'],'start_ms':round(beg,2),'duration_ms':round(dur,2),'label':labels.get(e['reactId'])})
 row['reactives']=intervals
 result.append(row)
 print(f.stem,'R arrival',row['input_to_R_ms'],'socket',row['input_to_socket_ms'])
 print([(e['name'],e['ms']) for e in browser if e['name'] in ('decode_start','decode_end','first_draw_end','gpu_idle_done','raf1_scheduled','raf1_fired','raf2_fired','painted','complete_group_painted')])
 print('rows',[(s['phase'],s['ms'],s['detail']) for s in server if s['name']=='viewerExpressionRow'])
(p/'timelines.json').write_text(json.dumps(result,indent=2)+'\n')
