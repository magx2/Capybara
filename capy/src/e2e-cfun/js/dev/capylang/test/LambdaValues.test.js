const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('LambdaValues', () => {
    const lambda = generatedModule('dev/capylang/test/LambdaValues.js');
    assert.equal(lambda.makeSupplier()(), 'hello');
    assert.equal(lambda.invokeSupplier(), 'hello');
    assert.equal(lambda.invokeFieldSupplier(), 'hello');
    assert.equal(lambda.add10()(5), 15);
    assert.equal(lambda.invokePartialAdd(), 15);
    assert.equal(lambda.invokePartialAddWithGeneratedNameCollision(), 15);
    assert.equal(lambda.invokePartialConcat(), 'A-B');
    assert.equal(lambda.invokePartialCombine(), 159);
    assert.equal(lambda.invokePartialPair(), '1:2');
    assert.equal(lambda.isAgeValid()(42), true);
    assert.equal(lambda.isAgeValid()(-1), false);
    assert.equal(lambda.invokePartialContainsMethod(), true);
    assert.equal(lambda.invokePartialReplaceMethod(), 'bonono');
});
