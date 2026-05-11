const test = require('node:test');
const { assert, capy, generatedModule, modulePath, spawnSync, unsafe } = require('./_support');

test('EffectMainProgram', () => {
    const effectMain = generatedModule('dev/capylang/test/EffectMainProgram.js');
    const effectProgram = unsafe(effectMain.main(['one', 'two']));
    assert.equal(capy.isType(effectProgram, 'Program'), true);
    assert.deepEqual(effectProgram.results, ['effect-main:2']);
    const effectRun = spawnSync('node', [modulePath('dev/capylang/test/EffectMainProgram.js'), 'one', 'two'], { encoding: 'utf8' });
    assert.equal(effectRun.status, 0, effectRun.stderr);
    assert.equal(effectRun.stdout.trim(), 'effect-main:2');
});
