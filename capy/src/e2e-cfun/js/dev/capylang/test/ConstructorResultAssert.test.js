const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('ConstructorResultAssert', () => {
    const constructorAssert = generatedModule('dev/capylang/test/ConstructorResultAssert.js');
    assert.equal(constructorAssert.validDateAssertSucceeds(), true);
    assert.equal(constructorAssert.invalidDateAssertFails(), true);
});
