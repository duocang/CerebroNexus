const assert = require('assert');
const source = require('fs').readFileSync(process.argv[2], 'utf8');
const js = require('./js-source.cjs')(source);
const controls = {}, boxes = [{checked:true}];
const document = {
  getElementById: id => controls[id] || null,
  querySelectorAll: () => [{querySelectorAll: () => boxes}]
};
const singleDefaultResourceNames = new Map();
const singleResourceDescriptors = new Map();
const window = {cerebroSavedViewDataset:{cell_count:4}};
const fetched = [];
function canonicalDatasetIdentityMatches(resource, identity) { return resource.cells === identity.cell_count; }
function fetchValidatedSingleGeometryResource(resource) { fetched.push(resource.url); return Promise.resolve(); }
eval(js.declarations(['singleGeometryRequestEligible','prefetchRegisteredSingleResource']));
const id = 'spatial_projection';
singleResourceDescriptors.set(id + ':slice', {resource:{cells:4,url:'slice.bin'}});
assert.equal(fetched.length, 0, 'registration must not fetch');
prefetchRegisteredSingleResource(id);
assert.equal(fetched.length, 0, 'unknown initial filters must not prefetch');
singleDefaultResourceNames.set(id, 'slice');
prefetchRegisteredSingleResource(id);
assert.deepEqual(fetched, ['slice.bin']);
controls[id + '_percentage_cells_to_show'] = {value:50};
prefetchRegisteredSingleResource(id);
assert.equal(fetched.length, 1);
controls[id + '_percentage_cells_to_show'].value = 100;
boxes[0].checked = false;
prefetchRegisteredSingleResource(id);
assert.equal(fetched.length, 1);
boxes[0].checked = true;
prefetchRegisteredSingleResource(id);
assert.equal(fetched.length, 2);
window.cerebroSavedViewDataset.cell_count = 5;
prefetchRegisteredSingleResource(id);
assert.equal(fetched.length, 2);

const spaceById = {}, drawn = [];
const context = {
  save(){}, restore(){}, beginPath(){}, closePath(){drawn.push('closed');},
  moveTo(x,y){drawn.push(['move',x,y]);}, lineTo(x,y){drawn.push(['line',x,y]);},
  fill(){drawn.push(['fill',this.fillStyle]);}, stroke(){drawn.push('stroke');}
};
function dataToScreen(p,x,y){ return [x*2,y*3]; }
function cssColor(color, fallback){ return color || fallback; }
eval(js.declarations(['singleHulls','drawHulls']));
spaceById.slice = {hulls:singleHulls({group_hulls:{x:[[0,1,0],[0,1]],y:[[0,0,1],[0,1]],color:['#123456']}})};
drawHulls({spaceId:'slice',ctx:context});
assert.deepEqual(drawn, [['move',0,0],['line',2,0],['line',0,3],'closed',['fill','#123456'],'stroke']);
