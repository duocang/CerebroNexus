const assert=require('assert'), fs=require('fs'), vm=require('vm');
const root=process.argv[2], js=require('./js-source.cjs')(fs.readFileSync(root+'/cell_views.js','utf8'));
const scope={console,performance,setTimeout:()=>0,clearTimeout(){},document:{addEventListener(){}},
  dispatchEvent(){},CustomEvent:function(){},Shiny:{addCustomMessageHandler(){},setInputValue(){}},
  singleActive:null,singleTiming:{},singleRequests:new Set(),recordSpecialistPayload(){},
  canonicalIdentity:JSON.stringify,reuseSharedSingleProjection:x=>x,
  hydrateSingleProjectionResource:x=>Promise.resolve(x),singleCacheKey:()=>'',
  singleView:()=>view,registerSingle:()=>{registered++;return null;}};
scope.window=scope;
vm.createContext(scope);
vm.runInContext(fs.readFileSync(root+'/dataset-context.js','utf8'),scope);
vm.runInContext(fs.readFileSync(root+'/cell_views_state.js','utf8'),scope);
vm.runInContext(js.declarations(['datasetContextApi','datasetMessageFresh','sameDatasetContext',
  'renderSingle','onSingleBinary','onSingleAuxBinary','updateSingleBackground']),scope);
const current={epoch:'s',dataset_key:'B',generation:2};
const old={epoch:'s',dataset_key:'B',generation:1};
scope.CerebroDatasetContext.receive({phase:'ready',dataset_context:current});
const view={dataset_context:current,aux_render_token:5,data:{wire_token:1},background_identity:JSON.stringify({image:'slice'})};
let registered=0;
scope.CBViewWire={unpack:buffer=>buffer};
async function run(){
  for(const dataset_context of [null,old,{...current,epoch:'other'},{...current,dataset_key:'A'}]){
    const message={id:'spatial_projection',dataset_context,render_token:5,selection_key:['stale'],
      background_identity:{image:'slice'},values:{opacity:.1}};
    scope.renderSingle(message);
    scope.onSingleBinary(message);
    scope.onSingleAuxBinary(message);
    scope.updateSingleBackground(message);
    await Promise.resolve();
    assert.equal(registered,0);
    assert.equal(view.data.selection_key,undefined);
    assert.equal(view.backgroundState,undefined);
  }
  const fresh={id:'spatial_projection',dataset_context:current,render_token:5,wire_token:1,
    selection_key:['B-1'],hover:{text:['cell B']},background_identity:{image:'slice'},values:{opacity:.7}};
  scope.renderSingle(fresh);
  scope.onSingleBinary(fresh);
  await Promise.resolve();
  assert.equal(registered,2,'both transports reach the real renderer for a current frame');
  scope.onSingleAuxBinary({...fresh,render_token:4});
  assert.equal(view.data.selection_key,undefined);
  scope.onSingleAuxBinary(fresh);
  assert.deepEqual(view.data.selection_key,['B-1']);
  scope.updateSingleBackground({...fresh,background_identity:{image:'wrong'}});
  assert.equal(view.backgroundState,undefined);
  scope.updateSingleBackground(fresh);
  assert.equal(view.backgroundState.opacity,.7);
}
run().catch(error=>{console.error(error);process.exitCode=1;});
