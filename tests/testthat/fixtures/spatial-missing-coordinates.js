const assert = require('assert'), fs = require('fs');
const source = fs.readFileSync(process.argv[2], 'utf8');
let D = {n:4};
function emptyVector(value) { return Array(D.n).fill(value); }
eval(source.slice(source.indexOf('  function alignSingleCoordinates('),
  source.indexOf('  function quantisedField(')));
for (const nested of [false,true]) {
  const payload = nested ? {
    meta:{traces:['A','B']},
    data:{x:[[0,null],[3]],y:[[2,4],[null]],z:[[null,5],[6]]},hover:{}
  } : {meta:{},data:{x:[0,null,3],y:[2,4,null],z:[null,5,6]},hover:{}};
  const result = alignSingleCoordinates(payload,nested);
  assert.strictEqual(result.x[0],0); // A genuine zero is valid.
  assert.strictEqual(result.y[0],2);
  assert(Number.isNaN(result.x[1]));
  assert(Number.isNaN(result.y[2]));
  assert(Number.isNaN(result.x[3])); // Unfilled slots are also missing.
  assert(Number.isNaN(result.y[3]));
  assert(Number.isNaN(result.z[0]));
  assert(Number.isNaN(result.z[3]));
}
const x = new Float64Array([0,NaN,3,NaN]), y = new Float64Array([2,4,NaN,NaN]);
const direct = alignSingleCoordinates({meta:{},data:{x,y},hover:{hoverinfo:'skip'}},false);
assert.strictEqual(direct.x,x);assert.strictEqual(direct.y,y);
console.log('PASS missing spatial coordinates remain missing in flat, grouped and typed payloads');
