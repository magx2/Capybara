const test = require('node:test');
const { assert, assertError, assertSuccess, generatedModule } = require('./_support');

test('ResultError', () => {
    const resultError = generatedModule('dev/capylang/test/ResultError.js');
    assert.equal(assertSuccess(resultError.successResult()), 7);
    assert.equal(assertError(resultError.failResult()), 'boom');
    assert.equal(assertSuccess(resultError.parseAndAddRestSize('10')), 13);
    assert.match(assertError(resultError.parseAndAddRestSize('abc')), /Cannot parse string to long/);
    assert.equal(assertSuccess(resultError.mapUntypedResult()), 'mapped_success_value');
    assert.match(assertSuccess(resultError.dictReduceThenMap(new Map([['a', 1], ['b', 2]]))), /done$/);
});
