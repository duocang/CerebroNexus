"""Select the single Gene debounce line, record exact identities; same checkout."""
import pathlib,subprocess,hashlib,json,sys
root=pathlib.Path(__file__).resolve().parents[4]
rel='inst/viewer/gene_expression/obj_projection_data_to_plot.R'
base=subprocess.check_output(['git','show','28444813:'+rel],cwd=root)
arm,label=sys.argv[1:]
value=base if arm=='B' else base.replace(b'<- debounceAfterFirst(',b'<- shiny::debounce(')
assert arm in ('B','C')
(root/rel).write_bytes(value)
folder=pathlib.Path(__file__).parent
identity={'baseline':'28444813','arm':arm,'candidate':'standard Shiny 50 ms prepared-value debounce',
 'sources':{rel:hashlib.sha256(value).hexdigest()},'harness':{n:hashlib.sha256((folder/n).read_bytes()).hexdigest() for n in ['remaining-server.R','remaining.js']}}
(folder/'remaining'/f'{label}.identity.json').write_text(json.dumps(identity,indent=2)+'\n')
