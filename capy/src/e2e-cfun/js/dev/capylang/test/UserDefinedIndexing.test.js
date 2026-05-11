const test = require('node:test');
const { assert, assertNone, assertSome, generatedModule } = require('./_support');

test('UserDefinedIndexing', () => {
    const indexing = generatedModule('dev/capylang/test/UserDefinedIndexing.js');
    assert.equal(indexing.intIndex(), 'one');
    assert.equal(indexing.stringIndex(), 42.5);
    assert.equal(indexing.matrixIndexName(), 'second');
    assertSome(indexing.chainedIndex(), 'e');
    assertSome(indexing.listVariableIndex(['a', 'b', 'c'], 1), 'b');
    assertNone(indexing.listVariableIndex(['a'], 3));
});
