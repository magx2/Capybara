const test = require('node:test');
const { assert, assertCapyEqual, assertNone, assertSome, generatedModule } = require('./_support');

test('NativeTypes', () => {
    const native = generatedModule('dev/capylang/test/NativeTypes.js');
    assert.equal(native.stringLength('capybara'), 8);
    assert.equal(native.stringTrim('  capybara  '), 'capybara');
    assertSome(native.stringGet('capybara', 0), 'c');
    assertSome(native.stringGet('capybara', -1), 'a');
    assertNone(native.stringGet('capybara', 99));
    assert.equal(native.stringGetRange('abcdef', -4, -1), 'cde');
    assert.equal(native.listSize([1, 2, 3]), 3);
    assertSome(native.listGet([1, 2, 3], -1), 3);
    assertNone(native.listGet([1, 2, 3], 9));
    assertCapyEqual(native.listGetRangeMethod([1, 2, 3, 4, 5, 6], -4, -1), [3, 4, 5]);
    assert.equal(native.setContains(new Set([1, 2, 3]), 2), true);
    assert.equal(native.dictSize(new Map([['one', 1], ['two', 2]])), 2);
    assertSome(native.dictGet(new Map([['one', 1]]), 'one'), 1);
    assertNone(native.dictIndex(new Map([['one', 1]]), 'two'));
    assert.equal(native.tupleSecond([1, 'two']), 'two');
});
