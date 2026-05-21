const test = require('node:test');
const { assert, capy, captureOutput, generatedModule, modulePath, spawnSync, unsafe } = require('./_support');

test('MainProgram', () => {
    const main = generatedModule('dev/capylang/test/MainProgram.js');
    const printed = captureOutput(() => unsafe(main.main(['ok'])));
    const program = printed.result;
    assert.equal(capy.isType(program, 'Program'), true);
    assert.equal(capy.isType(program, 'Success'), true, capy.toStringValue(program));
    assert.equal(Object.prototype.hasOwnProperty.call(program, 'results'), false);
    assert.equal(printed.stdout.trim(), 'ok');
    assert.equal(printed.stderr.trim(), '');
    const mainRun = spawnSync('node', [modulePath('dev/capylang/test/MainProgram.js'), 'ok'], { encoding: 'utf8' });
    assert.equal(mainRun.status, 0, mainRun.stderr);
    assert.equal(mainRun.stdout.trim(), 'ok');
    assert.equal(mainRun.stderr.trim(), '');
});
