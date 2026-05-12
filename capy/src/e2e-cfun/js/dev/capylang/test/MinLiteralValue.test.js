const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('MinLiteralValue', () => {
    const min = generatedModule('dev/capylang/test/MinLiteralValue.js');
    assert.equal(min.minInt(), -2147483648);
    assert.equal(min.minIntDigits(), 10);
    assert.equal(min.minLong(), -9223372036854775808n);
    assert.equal(min.maxLong(), 9223372036854775807n);
    assert.equal(min.maxSafeIntegerLong(), 9007199254740991n);
    assert.equal(min.longAddition(), 9223372036854775807n);
    assert.equal(min.longOverflow(), -9223372036854775808n);
});
