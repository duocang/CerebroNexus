const assert = require('assert'), fs = require('fs');
const source = fs.readFileSync(process.argv[2], 'utf8');
const js = require("./js-source.cjs")(source);
function singleNestedValue(_space, values, i) { return values[i]; }
eval(js.declarations(["singleHoverColumnValue"]));
const value = (levels, code) => singleHoverColumnValue({levels, values:[code]}, 0, {});
assert.equal(value('one sample', 0), 'one sample');
assert.equal(value(['one sample'], 0), 'one sample');
assert.equal(value(['A','B'], 1), 'B');
assert.equal(value('one sample', null), 'NA');
assert.equal(value(['A'], null), 'NA');
assert.equal(value(['A'], -1), 'NA');
assert.equal(value(['A'], NaN), 'NA');
assert.equal(value(undefined, 0), '0');
assert.equal(singleHoverColumnValue({values:[1630],format:'integer'},0,{}), '1,630');
assert.equal(singleHoverColumnValue({values:[0.901],format:'fixed',digits:2},0,{}), '0.90');
console.log('single-hover-levels: 10 assertions passed');
