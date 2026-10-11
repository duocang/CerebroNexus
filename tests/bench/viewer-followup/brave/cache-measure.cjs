// Directed retained-buffer measurement of the actual resource cache function.
// No browser driving; this is a Node unit experiment, not browser RSS evidence.
const fs=require('fs'),vm=require('vm');
const source=fs.readFileSync('inst/viewer/www/cell_views.js','utf8');
const code=source.slice(source.indexOf('  function resourceCacheKey('),source.indexOf('  var projectionResourceCache ='));
const sandbox={performance,window:{fetch:async()=>({ok:true,arrayBuffer:async()=>new ArrayBuffer(8000000)})}};
vm.createContext(sandbox);vm.runInContext(code,sandbox);
(async()=>{
 const cache=new Map(), rows=[];
 for(let dataset=0;dataset<10;dataset++){
  await sandbox.fetchCachedResource({url:`/dataset-${dataset}/projection.bin`,checksum:'immutable'},cache,'test','',buffer=>new Float32Array(buffer));
  let retained=0;for(const entry of cache.values())retained+=(await entry).value.byteLength;
  rows.push({distinctDatasets:dataset+1,entries:cache.size,retainedBytes:retained});
 }
 const repeated=await sandbox.fetchCachedResource({url:'/dataset-0/projection.bin',checksum:'immutable'},cache,'test','',buffer=>new Float32Array(buffer));
 console.log(JSON.stringify({kind:'actual-helper synthetic 8MB assets',rows,repeatedCacheHit:repeated.cacheHit,sourceSha256:require('crypto').createHash('sha256').update(source).digest('hex')}));
})();
