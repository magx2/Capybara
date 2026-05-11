const test = require('node:test');
const {
    assert,
    fs,
    path,
    generatedRoot,
    collectGeneratedJsFiles,
} = require('./_support');

test('loads every generated JavaScript module without unresolved module variables', () => {
    const files = collectGeneratedJsFiles(generatedRoot);
    const unresolved = [];

    for (const file of files) {
        const source = fs.readFileSync(file, 'utf8');
        const defined = new Set([...source.matchAll(/const (__module_[A-Za-z0-9_]+) = require/g)].map(match => match[1]));
        for (const match of source.matchAll(/\b(__module_[A-Za-z0-9_]+)\b/g)) {
            if (!defined.has(match[1])) {
                unresolved.push(`${path.relative(generatedRoot, file)}:${match[1]}`);
            }
        }
        assert.doesNotThrow(() => require(file), path.relative(generatedRoot, file));
    }

    assert.deepEqual([...new Set(unresolved)].sort(), []);
});
