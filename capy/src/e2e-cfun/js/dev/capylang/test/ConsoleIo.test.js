const test = require('node:test');
const {
    assert,
    assertCapyEqual,
    captureOutput,
    generatedModule,
    modulePath,
    spawnSync,
    unsafe,
} = require('./_support');

test('ConsoleIo', () => {
    const consoleIo = generatedModule('dev/capylang/test/ConsoleIo.js');
    const printed = captureOutput(() => {
        assert.equal(unsafe(consoleIo.emitAll()), ' line');
        assert.equal(unsafe(consoleIo.emitPrimitives(7)), false);
        assertCapyEqual(unsafe(consoleIo.emitByteLists([65, 90])), [65, 90]);
    });
    assert.equal(printed.stdout.replace(/\r\n/g, '\n'), 'out line\n7|8|9|1.5|2.5|true\n7\n8\n9\n1.5\n2.5\nfalse\nAZAZ\n');
    assert.equal(printed.stderr.replace(/\r\n/g, '\n'), 'err line\n7|8|9|1.5|2.5|true\n7\n8\n9\n1.5\n2.5\nfalse\nAZAZ\n');

    const readScript = `const m = require(${JSON.stringify(modulePath('dev/capylang/test/ConsoleIo.js'))}); console.log(m.readOne().unsafe_run()); console.log(m.readEofMarker().unsafe_run());`;
    const readRun = spawnSync('node', ['-e', readScript], { input: 'Capy\n', encoding: 'utf8' });
    assert.equal(readRun.status, 0, readRun.stderr);
    assert.deepEqual(readRun.stdout.trim().split(/\r?\n/), ['Capy', 'EOF']);
});
