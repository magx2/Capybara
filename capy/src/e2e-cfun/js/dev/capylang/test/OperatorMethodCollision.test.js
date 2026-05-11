const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('OperatorMethodCollision', () => {
    const collision = generatedModule('dev/capylang/test/OperatorMethodCollision.js');
    const counter = new collision.Counter({ value: 10 });
    assert.equal(collision.namedPlus(counter, 5), 15);
    assert.equal(collision.operatorPlus(counter, 5), 115);
});
