const test = require('node:test');
const {
    assert,
    assertCapyEqual,
    assertError,
    assertSuccess,
    fs,
    generatedModule,
    os,
    path,
    unsafe,
} = require('./_support');

test('FileIo', () => {
    const fileIo = generatedModule('dev/capylang/test/FileIo.js');
    const tempDir = fs.mkdtempSync(path.join(os.tmpdir(), 'capy-js-e2e-'));
    const textFile = path.join(tempDir, 'text.txt');
    const linesFile = path.join(tempDir, 'lines.txt');
    const bytesFile = path.join(tempDir, 'bytes.bin');

    assert.equal(assertSuccess(unsafe(fileIo.writeTextFile(textFile, 'hello'))), 'hello');
    assert.equal(assertSuccess(unsafe(fileIo.appendTextFile(textFile, ' world'))), ' world');
    assert.equal(assertSuccess(unsafe(fileIo.readTextFile(textFile))), 'hello world');
    assertCapyEqual(assertSuccess(unsafe(fileIo.writeLinesFile(linesFile, ['alpha']))), ['alpha']);
    assertCapyEqual(assertSuccess(unsafe(fileIo.appendLinesFile(linesFile, ['beta']))), ['beta']);
    assertCapyEqual(assertSuccess(unsafe(fileIo.readLinesFile(linesFile))), ['alpha', 'beta']);
    assertCapyEqual(assertSuccess(unsafe(fileIo.writeBytesFile(bytesFile, [65, 66]))), [65, 66]);
    assertCapyEqual(assertSuccess(unsafe(fileIo.appendBytesFile(bytesFile, [67]))), [67]);
    assertCapyEqual(assertSuccess(unsafe(fileIo.readBytesFile(bytesFile))), [65, 66, 67]);
    assert.equal(assertSuccess(unsafe(fileIo.fileSize(bytesFile))), 3);
    assert.equal(unsafe(fileIo.existsPath(bytesFile)), true);
    assert.match(assertError(unsafe(fileIo.readTextFile(path.join(tempDir, 'missing.txt')))), /read_text failed/);

    const dir = path.join(tempDir, 'nested', 'dir');
    const child = path.join(dir, 'child.txt');
    const deleteTarget = path.join(tempDir, 'delete-me.txt');
    assertSuccess(unsafe(fileIo.createDirectoriesAt(dir)));
    assert.equal(unsafe(fileIo.isDirectoryPath(dir)), true);
    assert.equal(assertSuccess(unsafe(fileIo.writeTextFile(child, 'x'))), 'x');
    assert.equal(assertSuccess(unsafe(fileIo.listDirectory(dir))).length, 1);
    assertSuccess(unsafe(fileIo.createFileAt(deleteTarget)));
    assert.equal(unsafe(fileIo.isFilePath(deleteTarget)), true);
    assert.equal(assertSuccess(unsafe(fileIo.deletePath(deleteTarget))), true);
    assert.equal(unsafe(fileIo.existsPath(deleteTarget)), false);

    const source = path.join(tempDir, 'source.txt');
    const copied = path.join(tempDir, 'copied.txt');
    const moved = path.join(tempDir, 'moved.txt');
    assert.equal(assertSuccess(unsafe(fileIo.writeTextFile(source, 'payload'))), 'payload');
    assertSuccess(unsafe(fileIo.copyPath(source, copied)));
    assert.equal(assertSuccess(unsafe(fileIo.readTextFile(copied))), 'payload');
    assertSuccess(unsafe(fileIo.movePath(copied, moved)));
    assert.equal(unsafe(fileIo.existsPath(copied)), false);
    assert.equal(assertSuccess(unsafe(fileIo.readTextFile(moved))), 'payload');
});
