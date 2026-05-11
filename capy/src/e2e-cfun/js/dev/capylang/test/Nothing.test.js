const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('Nothing', () => {
    const nothing = generatedModule('dev/capylang/test/Nothing.js');
    assert.throws(() => nothing.nothing1(1), /nothing1` is not yet implemented/);
    assert.throws(() => nothing.nothing2(1), /nothing2` is not yet implemented/);
});
