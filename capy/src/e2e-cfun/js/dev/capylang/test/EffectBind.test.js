const test = require('node:test');
const { assert, generatedModule, unsafe } = require('./_support');

test('EffectBind', () => {
    const effects = generatedModule('dev/capylang/test/EffectBind.js');
    assert.equal(unsafe(effects.addEffects(2, 3)), 5);
    assert.equal(unsafe(effects.nestedEffect(7)), 'v=7');
    assert.equal(effects.constructDelayedDivideByZero(), true);
    assert.match(unsafe(effects.clockNowIso()), /T/);
    assert.equal(typeof unsafe(effects.currentMillisValue()), 'number');
    assert.equal(typeof unsafe(effects.nanoTimeValue()), 'number');
});
