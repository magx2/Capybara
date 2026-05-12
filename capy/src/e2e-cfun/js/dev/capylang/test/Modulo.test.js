const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('Modulo', () => {
    const modulo = generatedModule('dev/capylang/test/Modulo.js');
    assert.equal(modulo.modInt(10, 3), 1);
    assert.equal(modulo.modInt(-10, 3), -1);
    assert.equal(modulo.modLong(10n, 4n), 2n);
    assert.equal(modulo.modFloat(7.5, 2.0), 1.5);
});
