const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('NumericWidening', () => {
    const widening = generatedModule('dev/capylang/test/NumericWidening.js');
    assert.equal(widening.returnLong(7), 7n);
    assert.equal(widening.multiplyWithLong(3), 30n);
    assert.equal(widening.multiplyLongChain(), 1000000000000000000n);
    assert.equal(widening.divideLongByGroupedIntChain(120000000000n), 2n);
    assert.equal(widening.multiplyLongByGroupedIntChain(2n), 120000000000n);
    assert.equal(widening.invokeDouble(1.25), 1.75);
    assert.equal(widening.invokeDoubleFromInt(6), 6.5);
    assert.equal(widening.buildMeasurement(9, 2.5).amount, 9n);
    assert.equal(widening.buildMeasurementPositional(11, 4.5).ratio, 4.5);
});
