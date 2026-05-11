const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('PrimitiveMethods', () => {
    const primitive = generatedModule('dev/capylang/test/PrimitiveMethods.js');
    assert.equal(primitive.longToInt(42), 42);
    assert.equal(primitive.longToInt(2147483648), -2147483648);
    assert.equal(primitive.longToInt(4294967295), -1);
    assert.equal(primitive.floatToInt(42.9), 42);
    assert.equal(primitive.floatToInt(-42.9), -42);
    assert.equal(primitive.doubleToInt(2147483648), 2147483647);
    assert.equal(primitive.doubleToLong(42.9), 42);
});
