const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('PrivateData', () => {
    const privateData = generatedModule('dev/capylang/test/PrivateData.js');
    assert.equal(privateData.parseInt(7), 'ok:7');
    assert.equal(privateData.parseConflict('x'), 'internal:x');
    assert.equal(Object.prototype.hasOwnProperty.call(privateData, '_Parse'), false);
    assert.equal(Object.prototype.hasOwnProperty.call(privateData, 'Parse'), true);
});
