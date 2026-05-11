const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('LocalFunctions', () => {
    const localFunctions = generatedModule('dev/capylang/test/LocalFunctions.js');
    assert.equal(localFunctions.sumDown(5), 15);
    assert.equal(localFunctions.parity(15), false);
    assert.equal(localFunctions.localWithLet(10), 11);
    assert.equal(localFunctions.maxLocal(2, 5), 5);
});
