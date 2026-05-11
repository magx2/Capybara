const test = require('node:test');
const { assert, capy, generatedModule } = require('./_support');

test('MatchWhen', () => {
    const guarded = generatedModule('dev/capylang/test/MatchWhen.js');
    assert.equal(guarded.foo(new capy.Some({ value: '-' })), 'minus');
    assert.equal(guarded.foo(capy.None), 'none');
    assert.equal(guarded.boo(new capy.Some({ value: 2 })), 'two');
    assert.equal(guarded.nestedGuard(new capy.Some({ value: 11 })), 'big');
    assert.equal(guarded.sign(new guarded.Sign({ value: '-', weight: 20, enabled: true })), 'heavy minus');
    assert.equal(guarded.letter(guarded.Vowel), 'vowel');
    assert.equal(guarded.rangeLabel(new guarded.Range({ start: 1, stop: 5, step: 1, enabled: true })), 'ascending');
});
