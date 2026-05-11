const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('OverloadResolution', () => {
    const overload = generatedModule('dev/capylang/test/OverloadResolution.js');
    assert.equal(overload.chooseInts(), 103);
    assert.equal(overload.chooseLongs(), 203);
});
