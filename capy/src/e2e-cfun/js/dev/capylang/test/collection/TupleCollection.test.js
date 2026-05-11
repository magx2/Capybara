const test = require('node:test');
const { assert, generatedModule } = require('../_support');

test('TupleCollection', () => {
    const tuple = generatedModule('dev/capylang/test/collection/TupleCollection.js');
    assert.equal(tuple.tupleWithFunctionsApplyFirst(7), 8);
    assert.equal(tuple.tupleWithFunctionsApplySecond(7), 14);
});
