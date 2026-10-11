const assert=require('assert'),fs=require('fs');
const source=fs.readFileSync(process.argv[2],'utf8');
function extract(name){
  const start=source.indexOf('  function '+name+'(');assert(start>=0,name);
  const brace=source.indexOf('{',start);let depth=1,end=brace+1;
  while(depth&&end<source.length){if(source[end]==='{')depth++;if(source[end]==='}')depth--;end++;}
  return source.slice(start,end);
}
const projectionResourceCache=new Map(),projectionSubsetResourceCache=new Map(),metadataCodesResourceCache=new Map(),categoricalResourceCache=new Map();
let fetches=0;
const window={fetch:async()=>{fetches++;return {ok:true,arrayBuffer:async()=>new ArrayBuffer(8000000)};}};
eval(extract('resourceCacheKey'));eval(extract('fetchCachedResource'));eval(extract('clearStaticResourceCaches'));
assert(extract('invalidateDatasetViewState').includes('clearStaticResourceCaches();'));
(async()=>{
 const rows=[];
 for(let dataset=0;dataset<10;dataset++){
  clearStaticResourceCaches();
  const resource={url:`/dataset-${dataset}/projection.bin`,checksum:'immutable'};
  const a=await fetchCachedResource(resource,projectionResourceCache,'test','',buffer=>new Float32Array(buffer));
  const b=await fetchCachedResource(resource,projectionResourceCache,'test','',()=>{throw Error('duplicate decode');});
  assert.strictEqual(a.value,b.value);assert(b.cacheHit);assert.equal(projectionResourceCache.size,1);
  rows.push({datasets:dataset+1,retainedBytes:a.value.byteLength,entries:projectionResourceCache.size});
 }
 assert.equal(fetches,10);
 for(const map of [projectionSubsetResourceCache,metadataCodesResourceCache,categoricalResourceCache])map.set('old',Promise.resolve({value:new Uint8Array(100)}));
 clearStaticResourceCaches();
 for(const map of [projectionResourceCache,projectionSubsetResourceCache,metadataCodesResourceCache,categoricalResourceCache])assert.equal(map.size,0);
 // In-flight failure after invalidation must not remove a new same-key request.
 let rejectOld,resolveNew;
 window.fetch=()=>new Promise((resolve,reject)=>{rejectOld=reject;});
 const resource={url:'/same',checksum:'same'};
 const old=fetchCachedResource(resource,projectionResourceCache,'test','',x=>x).catch(()=>null);
 clearStaticResourceCaches();
 window.fetch=()=>new Promise(resolve=>{resolveNew=resolve;});
 const fresh=fetchCachedResource(resource,projectionResourceCache,'test','',x=>x);
 const saved=projectionResourceCache.get(resourceCacheKey(resource,''));
 rejectOld(Error('obsolete'));await old;
 assert.strictEqual(projectionResourceCache.get(resourceCacheKey(resource,'')),saved);
 resolveNew({ok:true,arrayBuffer:async()=>new ArrayBuffer(8)});await fresh;
 clearStaticResourceCaches();
 console.log(JSON.stringify({pass:true,rows,afterInvalidationEntries:projectionResourceCache.size,lateFailureSafe:true,sameDatasetReuse:true}));
})().catch(e=>{console.error(e);process.exitCode=1;});
