const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('OperatorPrecedence', () => {
    const precedence = generatedModule('dev/capylang/test/OperatorPrecedence.js');
    assert.equal(precedence.leapYear(2024), true);
    assert.equal(precedence.leapYear(1900), false);
    assert.equal(precedence.andBindsAfterModuloAndComparison(4, 9), true);
    assert.equal(precedence.orBindsAfterModuloAndComparison(5, 10), false);
    assert.equal(precedence.powerBindsBeforeMultiplication(), true);
    assert.equal(precedence.andBindsBeforeOr(), true);
});
