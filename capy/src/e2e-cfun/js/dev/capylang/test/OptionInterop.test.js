const test = require('node:test');
const { assertNone, assertSome, generatedModule } = require('./_support');

test('OptionInterop', () => {
    const optionInterop = generatedModule('dev/capylang/test/OptionInterop.js');
    assertSome(optionInterop.someValue(7), 7);
    assertNone(optionInterop.noneValue());
});
