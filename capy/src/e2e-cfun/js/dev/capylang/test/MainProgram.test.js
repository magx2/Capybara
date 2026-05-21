const test = require('node:test');
const { assert, capy, generatedModule, modulePath, spawnSync, unsafe } = require('./_support');

test('MainProgram', () => {
    const main = generatedModule('dev/capylang/test/MainProgram.js');
    const mainResult = main.main(['ok']);
    const program = capy.isEffect(mainResult) ? unsafe(mainResult) : mainResult;
    assert.equal(capy.isType(program, 'Program'), true);
    assert.equal(capy.isType(program, 'Success'), true, capy.toStringValue(program));
    assert.equal(Object.prototype.hasOwnProperty.call(program, 'results'), false);
    const mainRun = spawnSync('node', [modulePath('dev/capylang/test/MainProgram.js'), 'ok'], { encoding: 'utf8' });
    assert.equal(mainRun.status, 0, mainRun.stderr);
    assert.equal(mainRun.stdout.trim(), '');
    assert.equal(mainRun.stderr.trim(), '');
});
