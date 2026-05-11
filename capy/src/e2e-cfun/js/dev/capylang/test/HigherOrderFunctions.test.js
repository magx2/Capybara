const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('HigherOrderFunctions', () => {
    const higher = generatedModule('dev/capylang/test/HigherOrderFunctions.js');
    assert.deepEqual(higher.map([1, 2, 3]).asList(), [1, 4, 9]);
    assert.equal(higher.hof(x => `n=${x}`), 'n=5');
    assert.equal(higher.invokeHof('Hello'), 'Hello 5');
    assert.equal(higher.runLambda(), -45);
    assert.equal(higher.runLambda2(), -45);
    assert.equal(higher.invokeHofUnused('Hello'), 'Hello');
    assert.equal(higher.runLambdaUnusedMiddle(), 12);
    assert.equal(higher.runLambdaNoArgs(), 'Hello, Wrold');
});
