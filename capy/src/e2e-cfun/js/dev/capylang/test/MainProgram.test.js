const test = require('node:test');
const { assert, capy, generatedModule, modulePath, spawnSync } = require('./_support');

test('MainProgram', () => {
    const main = generatedModule('dev/capylang/test/MainProgram.js');
    const program = main.main(['ok']);
    assert.equal(capy.isType(program, 'Program'), true);
    assert.equal(program.results[0], 'ok');
    const mainRun = spawnSync('node', [modulePath('dev/capylang/test/MainProgram.js'), 'ok'], { encoding: 'utf8' });
    assert.equal(mainRun.status, 0, mainRun.stderr);
    assert.equal(mainRun.stdout.trim(), 'ok');
});
