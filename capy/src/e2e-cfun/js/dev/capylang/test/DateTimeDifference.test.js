const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('DateTimeDifference', () => {
    const dateTime = generatedModule('dev/capylang/test/DateTimeDifference.js');
    assert.equal(dateTime.forwardDifferenceIso(), 'P1DT1H1M1S');
    assert.equal(dateTime.backwardDifferenceIso(), 'P-1DT-1H-1M-1S');
    assert.equal(dateTime.differenceRoundTrips(), true);
});
