const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('ResultInfixGeneric', () => {
    const infixResult = generatedModule('dev/capylang/test/ResultInfixGeneric.js');
    assert.equal(infixResult.applyMapper(new infixResult.Ok({ value: 7 }), infixResult.mapToString).value, 'v=7');
    assert.equal(infixResult.applyMapper(new infixResult.Ok({ value: 3 }), infixResult.mapToFail).message, 'bad:3');
    assert.equal(infixResult.propagateFail(new infixResult.Fail({ message: 'boom' }), infixResult.mapToString).message, 'boom');
    assert.equal(infixResult.applyFlatMapper(new infixResult.Ok({ value: 7 }), infixResult.mapToNestedOk).value, 'mapped_7');
    assert.equal(infixResult.flattenNestedResult(infixResult.wrapNestedFail(5)).message, 'wrapped_bad:5');
});
