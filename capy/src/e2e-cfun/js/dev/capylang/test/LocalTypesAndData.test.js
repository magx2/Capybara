const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('LocalTypesAndData', () => {
    const localTypes = generatedModule('dev/capylang/test/LocalTypesAndData.js');
    assert.equal(localTypes.fooMe('foo'), 'xyz');
    assert.equal(localTypes.localDataWithImportedResult(7), 'ok:7');
    assert.equal(localTypes.localSingleUsedInLocalTypeAndFunction(true), 'stop');
    assert.equal(localTypes.localGenericDataFieldAccessFollowedByIndex('abc'), 'a');
    assert.equal(localTypes.localGenericOptionConstructorPatternBinding(7), 77);
});
