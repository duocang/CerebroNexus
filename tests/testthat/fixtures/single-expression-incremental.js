const assert = require('assert'), fs = require('fs');
const source = fs.readFileSync(process.argv[2], 'utf8');
function between(start, end) { return source.slice(source.indexOf(start), source.indexOf(end)); }
var D, panels, singleViews, singleActive, singleSpaceIds, singleSpaceModes, spaceById;
var singleIndexCells, singleIndexMap, rebuildingBase = false;
var colorBy, _ordD, _clipD, pick, hoverCell, cardMeta, _layoutKey;
var FIELD_PREFIX = 'field:', RGB_MODE = 'rgb', calls, emitted;
function singlePalette(scale) { return scale || ['#000000', '#ffffff']; }
function visibleSingleId() { return singleActive; }
function $(id) { return null; }
function updateCanvasDescription() {}
function unpinTip() {}
function closeCard() {}
function renderLegend() { calls.legend++; }
function renderSelbar() {}
function resizeAll() {}
function drawAll() { calls.draw++; }
function refreshPanelHover() { calls.hoverRefresh = (calls.hoverRefresh || 0) + 1; }
function scheduleSingleMetadata() {}
function reportSelection() {}
function hydrateSparseSingleColor() {}
function hydrateColorPacketMap() {}
function activateSingle() { calls.activate++; }
function requestAnimationFrame(cb) { cb(); }
function CustomEvent(type, args) { this.type = type; this.detail = args.detail; }
var window = {dispatchEvent: e => emitted.push(e)};
const datasetContext = {epoch:'incremental-test',dataset_key:'A',generation:1};
function singleView(id) { return singleViews[id]; }
function sameDatasetContext(a,b) { return JSON.stringify(a) === JSON.stringify(b); }
function currentDatasetContext() { return datasetContext; }
function datasetMessageFresh(message) { return sameDatasetContext(message.dataset_context,datasetContext); }
eval(between('  function singleIndex()', '  function singlePayloadCells'));
eval(between('  function quantisedField(', '  function singleEdges('));
eval(between('  function singleGpuReady(', '  function activateSingle('));
eval(between('  function updateSingleExpression(', '  function clearSingleSelection('));
const id = 'expression_projection', spaceId = 'single::' + id;
function setup() {
  calls = {activate: 0, legend: 0, draw: 0}; emitted = [];
  D = {n: 4, cells: ['d', 'b', 'a', 'c'], fields: {}, cat_extra: {}};
  singleActive = id; singleSpaceIds = [spaceId]; singleSpaceModes = {};
  spaceById = {[spaceId]: {id: spaceId, x: new Float64Array([4,2,1,3]),
    y: new Float64Array([8,4,2,6]), _unit: {ok: [1,1,1,1]}}};
  panels = [{key:'A', spaceId, view:{cx:.4,cy:.6,span:.3},
    lassoData:[[0,0],[1,0],[1,1]], sx:new Float32Array(4), gpu:null}];
  singleViews = {[id]: {dataset_context:datasetContext,render_token:1,meta:{color_type:'continuous',color_variable:'old',render_token:1},
    data:{x:spaceById[spaceId].x,y:spaceById[spaceId].y,
      selection_key:D.cells.slice(),color:[1,2,3,4]},selection:['b'],mode:'box'}};
  singleIndexCells = null; singleIndexMap = null; singleIndex();
  _ordD = D; _clipD = D;
}
function update(data, meta = {}) { return recolorSingle({id,
  dataset_context:datasetContext,render_token:Number(singleViews[id].render_token)+1,
  meta:{color_type:'continuous',color_variable:'new',render_token:2,...meta},data}); }
(async function() {
  setup(); const original = {D, cells:D.cells, index:singleIndexMap, space:spaceById[spaceId],
    unit:spaceById[spaceId]._unit, view:panels[0].view, sx:panels[0].sx, lasso:panels[0].lassoData};
  hoverCell = 2;
  assert(update({color:new Float32Array([9,NaN,-2,5]),paint_order:'highest'}));
  assert.equal(calls.activate, 0); assert.strictEqual(D, original.D);
  assert.equal(hoverCell, 2); assert.equal(calls.hoverRefresh, 1);
  assert.strictEqual(D.cells,original.cells); assert.strictEqual(singleIndexMap,original.index);
  assert.strictEqual(spaceById[spaceId],original.space); assert.strictEqual(spaceById[spaceId]._unit,original.unit);
  assert.strictEqual(panels[0].view,original.view); assert.strictEqual(panels[0].sx,original.sx);
  assert.strictEqual(panels[0].lassoData,original.lasso);
  assert.deepStrictEqual(singleViews[id].selection,['b']);
  let field=D.fields['single:'+id+':0']; assert.deepStrictEqual(field.raw,[9,null,-2,5]);
  assert.deepStrictEqual(field.v,[1000,null,0,636]); assert.equal(field.label,'new');
  assert.equal(_ordD,null); assert.equal(_clipD,null);
  assert.equal(panels[0].colorBy,FIELD_PREFIX+'single:'+id+':0');
  update({rgb:{r:[0,255,0,0],g:[20,0,30,0],b:[1,2,3,4]},rgb_scaled:true,
    rgb_genes:{r:'A',g:'B',b:'C'}},{color_type:'rgb',color_variable:'RGB'});
  assert.equal(calls.activate,0); assert.deepStrictEqual(Array.from(D.rgb.g),[20,0,30,0]);
  assert.equal(D.fields['single:'+id+':0'],undefined); assert.equal(panels[0].colorBy,RGB_MODE);
  update({color:[1,2,3,4],colorscale:['#123456','#abcdef'],paint_order:'natural'});
  assert.equal(D.rgb,undefined); assert.equal(D.fields['single:'+id+':0'].paintOrder,false);
  await new Promise(setImmediate);
  // Previous paints used the same D object; only the newest metadata may report ready.
  assert.equal(emitted.length,1); assert.equal(emitted[0].detail.renderToken,2);
  for (const mutate of [
    () => {singleActive='overview_projection';},
    () => {D.cells=[];},
    () => {singleViews[id].data.z=[1,2,3,4];},
    () => {singleViews[id].data.panels=[{}];},
    () => {singleSpaceIds.push('other');},
    () => {panels.push({spaceId:'other'});},
    () => {singleViews[id].meta.color_type='categorical';}
  ]) {setup();mutate();update({color:[1,2,3,4]});assert.equal(calls.draw,0);}
  for (const data of [{color:[1,2]}, {color:{A:[1,2,3,4],B:[4,3,2,1]}},
    {color:[1,2,3,4],x:[9,8,7,6]}, {rgb:{r:[1],g:[2],b:[3]}}]) {
    setup();update(data);assert.equal(calls.activate,1);assert.equal(calls.draw,0);
  }
  setup();update({color:[1,2,3,4]},{appearance:{draw_border:true}});assert.equal(calls.activate,1);
  setup();update({color:[1,2,3,4]},{color_type:'coexpression'});assert.equal(calls.activate,1);
  setup();D.cells=['a','a','b','c'];singleViews[id].data.selection_key=D.cells.slice();
  update({color:[1,2,3,4]});assert.equal(calls.activate,1);
  console.log('PASS incremental expression state, numeric/NA/RGB values, order invalidation, stale paint and fallbacks');
})().catch(e=>{console.error(e);process.exitCode=1;});
