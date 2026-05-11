const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('MinLiteralValue', () => {
    const min = generatedModule('dev/capylang/test/MinLiteralValue.js');
    assert.equal(min.minInt(), -2147483648);
    assert.equal(min.minIntDigits(), 10);
});
