"""Hash evidence and final sources after the test processes have stopped."""
import hashlib
import json
from pathlib import Path

here = Path(__file__).resolve().parent
root = here.parents[3]
evidence = here / 'remaining'
paths = [p for p in evidence.iterdir() if p.is_file() and p.name != 'FINAL_SHA256.json']
paths += [here/p for p in ('remaining-server.R','remaining.js','memory-server.R','memory.js',
    'payload-audit.R','reconnect-inspect.R','summarize-memory.py')]
paths += [root/p for p in ('inst/viewer/coordinated_views/bundle.R',
    'inst/viewer/coordinated_views/server.R','inst/viewer/www/cell_views.js',
    'inst/viewer/utility_functions.R','inst/viewer/gene_expression/func_projection_update_plot.R',
    'inst/viewer/gene_expression/obj_projection_data_to_plot.R')]
manifest = {
    'dataset_sha256':'cd6f4aed684ecefc73e8136db692faf9b6a633fe3e6918e11e07771c31814e38',
    'starting_commit':'28444813',
    'gene_mechanism_commit':'f4e4fe3b',
    'linked_mechanism_commit':'97f91ac5',
    'trajectory_mechanism_commit':'5d3d18d7',
    'note':'Candidates ran as working-tree patches before their commits. Final lifecycle sources are identified by the hashes below. Gene frozen harness differs from Linked/Trajectory; memory is diagnostic and never pooled as a performance arm.',
    'sha256':{str(p.relative_to(root)).replace('\\','/'):hashlib.sha256(p.read_bytes()).hexdigest()
        for p in sorted(paths)}
}
(evidence/'FINAL_SHA256.json').write_text(json.dumps(manifest,indent=2),encoding='utf-8')
print('Hashed',len(paths),'files')
