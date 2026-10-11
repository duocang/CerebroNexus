const fs=require('fs'),assert=require('assert'),vm=require('vm');
const source=fs.readFileSync(process.argv[2],'utf8');
for(const backend of ['webgpu','webgl2']){
 let start=source.indexOf('    function setData(next)');if(backend==='webgl2')start=source.indexOf('    function setData(next)',start+1);
 const end=source.indexOf(backend==='webgpu'?'    function uniforms(':'    function drawPass(',start);
 const uploads=[];const scope={ready:true,data:null,positionBuffer:{},colorBuffer:{},layerBuffer:{},metrics:{},lastFrame:{},program:{},performance,
  gl:{useProgram(){}},replaceBuffer:(buffer,array)=>{uploads.push(array);return buffer;},upload:(buffer,location,array)=>uploads.push(array)};
 vm.createContext(scope);vm.runInContext(source.slice(start,end),scope);
 const positions=new Float32Array([0,0,1,1]),colors=new Uint8Array([1,2,3,255,4,5,6,255]),layers=new Uint32Array([0,0]);
 scope.setData({positions,colors,layers,count:2,foreground:false});assert.equal(uploads.length,3);
 scope.setData({positions,colors,layers,count:2,foreground:false});assert.equal(uploads.length,3);
 const updated=colors.slice();updated[3]=13;
 scope.setData({positions,colors:updated,layers,count:2,foreground:false});assert.equal(uploads.length,4);assert.strictEqual(uploads[3],updated);
 const newLayers=layers.slice();newLayers[1]=1;
 scope.setData({positions,colors:updated,layers:newLayers,count:2,foreground:true});assert.equal(uploads.length,5);assert.strictEqual(uploads[4],newLayers);
 scope.setData({positions,colors:updated,layers:newLayers,count:1,foreground:true});assert.equal(uploads.length,5);assert.equal(scope.data.count,1);
 const newPositions=positions.slice();scope.setData({positions:newPositions,colors:updated,layers:newLayers,count:2,foreground:true});assert.equal(uploads.length,6);assert.strictEqual(uploads[5],newPositions);
 assert.throws(()=>scope.setData({positions:new Float32Array(1),colors,layers,count:2}),/shorter/);
 console.log('PASS',backend,'independent attribute uploads, unchanged buffers and count-only changes');
}
