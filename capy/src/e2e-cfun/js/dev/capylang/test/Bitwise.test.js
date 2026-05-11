const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('Bitwise', () => {
    const bitwise = generatedModule('dev/capylang/test/Bitwise.js');
    assert.equal(bitwise.bitAnd(0b1100, 0b1010), 0b1000);
    assert.equal(bitwise.bitNand(0b1100, 0b1010), ~0b1000);
    assert.equal(bitwise.bitOr(0b1100, 0b1010), 0b1110);
    assert.equal(bitwise.bitXor(0b1100, 0b1010), 0b0110);
    assert.equal(bitwise.bitNot(0b1100), ~0b1100);
});
