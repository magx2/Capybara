const test = require('node:test');
const { assert, assertCapyEqual, assertSuccess, generatedModule } = require('./_support');

test('EmptyLiteralInference', () => {
    const empty = generatedModule('dev/capylang/test/EmptyLiteralInference.js');
    assertCapyEqual(empty.reverseInts([1, 2, 3]), [3, 2, 1]);
    assertCapyEqual(empty.duplicateStrings(['a', 'b']), ['a', 'a', 'b', 'b']);
    assert.equal(empty.reduceDictToNamedValuesSize(new Map([['a', 1], ['b', 2]])), 2);
    assertCapyEqual(assertSuccess(empty.reduceListToSuccessValues([1, 2, 3])), [1, 2, 3]);
    assertCapyEqual(assertSuccess(empty.reduceSetToSuccessValues(new Set(['a', 'b']))), new Set(['a', 'b']));
    assert.equal(empty.reduceListToBoxValues([1, 2, 3]).value.length, 3);
    assert.equal(empty.buildTestFileFromThreeInferredTestCaseLists(), 3);
});
