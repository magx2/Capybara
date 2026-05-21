const test = require('node:test');
const { assert, capy, captureOutput, generatedModule, modulePath, spawnSync, unsafe } = require('./_support');

test('EffectMainProgram', () => {
    const effectMain = generatedModule('dev/capylang/test/EffectMainProgram.js');
    const printed = captureOutput(() => unsafe(effectMain.main(['one', 'two'])));
    const effectProgram = printed.result;
    assert.equal(capy.isType(effectProgram, 'Program'), true);
    assert.equal(capy.isType(effectProgram, 'Success'), true, capy.toStringValue(effectProgram));
    assert.equal(Object.prototype.hasOwnProperty.call(effectProgram, 'results'), false);
    assert.equal(printed.stdout.trim(), 'effect-main:2');
    assert.equal(printed.stderr.trim(), '');
    const effectRun = spawnSync('node', [modulePath('dev/capylang/test/EffectMainProgram.js'), 'one', 'two'], { encoding: 'utf8' });
    assert.equal(effectRun.status, 0, effectRun.stderr);
    assert.equal(effectRun.stdout.trim(), 'effect-main:2');
    assert.equal(effectRun.stderr.trim(), '');
});
