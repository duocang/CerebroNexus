const assert=require('assert'),fs=require('fs'),vm=require('vm');
const js=require('./js-source.cjs')(fs.readFileSync(process.argv[2],'utf8'));
const nodes={};
const section={style:{}};
nodes['cv-img-pick-ctl']={style:{},closest:()=>section};
const selected={options:[],clearOptions(){this.options=[];},addOption(x){this.options=x;},
  setValue(x){this.value=x;},enable(){this.enabled=true;},disable(){this.enabled=false;}};
nodes['cv-bg-image-select']={selectize:selected};
nodes['coordviews_image_ui']={};nodes['cv-clone-layout-ctl']={};
let active={id:'Trekker',images:[{id:'one',label:'First'},{id:'two',label:'Second'}]};
const scope={$:id=>nodes[id],D:{spaces:[]},spaceById:{},IMG_NONE:'none',window:{},
  activeSpatial:()=>active,backgroundSpaces:()=>active?[active]:[],revealEl:(node,visible)=>node.visible=visible};
vm.createContext(scope);
vm.runInContext(js.declarations(['spatialImages','currentImage','spatialName','backgroundStateKey',
  'imgKey','renderImagePicker','updateSpaceScopedControls','presetState','imageRenderState']),scope);
scope.renderImagePicker();
assert.deepEqual(Array.from(selected.options,x=>x.text),['First','Second','None']);
assert.equal(selected.value,'one');assert.equal(selected.enabled,true);
scope.updateSpaceScopedControls();assert.equal(nodes.coordviews_image_ui.visible,true);
active._customImageId='none';scope.renderImagePicker();scope.updateSpaceScopedControls();
assert.equal(selected.value,'none');assert.equal(nodes.coordviews_image_ui.visible,false);
active._customImageId='two';scope.updateSpaceScopedControls();
assert.equal(scope.currentImage(active).id,'two');assert.equal(nodes.coordviews_image_ui.visible,true);
assert.notEqual(scope.backgroundStateKey(active),scope.backgroundStateKey({...active,_spatialSample:true}));
const samples={id:'slice',samples:[{name:'a'},{name:'b'}]},img={id:'same'};
const initial=scope.imgKey(samples,img);samples._sampleName='b';
assert.notEqual(scope.imgKey(samples,img),initial);samples._sampleName='a';
assert.equal(scope.imgKey(samples,img),initial);
assert.notEqual(scope.imgKey(samples,{id:'other'}),initial);
const preset={preset:{rotation:32,offsetX:12,scaleX:2,scaleY:3,opacity:.4}};
const state=scope.presetState(preset);
assert.equal(state.rotate,32);assert.equal(state.offsetX,12);assert.equal(state.lock,false);
assert.equal(scope.imageRenderState(preset,state),state);
active={id:'empty'};scope.renderImagePicker();scope.updateSpaceScopedControls();
assert.equal(selected.enabled,false);assert.equal(nodes.coordviews_image_ui.visible,false);
active=null;scope.renderImagePicker();assert.equal(section.style.display,'none');

const legend={style:{},children:[],appendChild(child){this.children.push(child);}};
nodes['cv-legend']=legend;
Object.assign(scope,{singleActive:null,evidenceOn:false,colorBy:'rgb',RGB_MODE:'rgb',
  GENE_MODE:'gene',GENE_PANELS_MODE:'genes',RGB_GREY:'#888',
  renderPanelScales(){},renderColorbar(){},fieldOf:()=>null,esc:x=>x,
  document:{createElement:()=>({})}});
scope.D.rgb={genes:['CD3D','MS4A1','LYZ']};
vm.runInContext(js.declarations(['renderLegend']),scope);
scope.renderLegend();
const text=legend.children.map(x=>x.innerHTML||x.textContent).join(' ');
for(const label of ['CD3D','MS4A1','LYZ','Low expression','R + G','R + B','G + B','R + G + B','scaled independently'])
  assert(text.includes(label),label);
