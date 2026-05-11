const test = require('node:test');
const { assertNone, assertSome, generatedModule } = require('./_support');

test('IndexCollection', () => {
    const index = generatedModule('dev/capylang/test/IndexCollection.js');
    assertSome(index.listIndex5([10, 20, 30, 40, 50, 60]), 60);
    assertNone(index.listIndex5([10, 20]));
    assertSome(index.stringIndexMinus2('capybara'), 'r');
    assertNone(index.stringIndexMinus2('a'));
});
