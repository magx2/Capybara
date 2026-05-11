const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('NumericWidening', () => {
    const widening = generatedModule('dev/capylang/test/NumericWidening.js');
    assert.equal(widening.returnLong(7), 7);
    assert.equal(widening.multiplyWithLong(3), 30);
    assert.equal(widening.invokeDouble(1.25), 1.75);
    assert.equal(widening.invokeDoubleFromInt(6), 6.5);
    assert.equal(widening.buildMeasurement(9, 2.5).amount, 9);
    assert.equal(widening.buildMeasurementPositional(11, 4.5).ratio, 4.5);
});
