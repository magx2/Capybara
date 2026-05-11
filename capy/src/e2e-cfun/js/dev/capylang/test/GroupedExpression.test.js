const test = require('node:test');
const { assert, assertError, assertSuccess, generatedModule } = require('./_support');

test('GroupedExpression', () => {
    const grouped = generatedModule('dev/capylang/test/GroupedExpression.js');
    assert.equal(assertSuccess(grouped.nestedPipeInMatchBranch(1)), 3);
    assert.equal(assertSuccess(grouped.groupedGuardedMatchIsExhaustive('-+abc')), 'plus:pre:tail:build');
    assert.equal(assertError(grouped.groupedGuardedMatchIsExhaustive('-abc')), 'bad:a');
    assert.equal(assertSuccess(grouped.semverStyleMixedCasePipeBodies('+meta')), '1.2.3+001');
    assert.equal(assertError(grouped.nestedResultErrorMatchCase('')), 'empty');
});
