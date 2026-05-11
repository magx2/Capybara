const test = require('node:test');
const assert = require('node:assert/strict');
const { mkdtemp, mkdir, writeFile, readFile, access } = require('node:fs/promises');
const { spawnSync } = require('node:child_process');
const { join } = require('node:path');
const { tmpdir } = require('node:os');

function runCapy(args) {
    const classpath = process.env.CAPY_CLI_CLASSPATH;
    assert.ok(classpath, 'CAPY_CLI_CLASSPATH must be provided by the Gradle jsTests task');
    return spawnSync('java', ['-cp', classpath, 'dev.capylang.Capy', ...args], {
        encoding: 'utf8',
    });
}

function runNode(args) {
    return spawnSync('node', args, {
        encoding: 'utf8',
    });
}

async function tempProject() {
    return mkdtemp(join(tmpdir(), 'capy-js-test-'));
}

async function exists(path) {
    try {
        await access(path);
        return true;
    } catch {
        return false;
    }
}

test('prints version', () => {
    const result = runCapy(['--version']);

    assert.equal(result.status, 0, result.stderr);
    assert.equal(result.stderr.trim(), '');
    assert.match(result.stdout.trim(), /^Capybara compiler version: /);
});

test('compile-generate JS writes linked JSON, CommonJS modules, and runnable output', async () => {
    const root = await tempProject();
    const sourceDir = join(root, 'src');
    const generatedDir = join(root, 'generated');
    const linkedDir = join(root, 'linked');
    await mkdir(join(sourceDir, 'foo'), { recursive: true });
    await writeFile(join(sourceDir, 'foo', 'Main.cfun'), `
from /capy/lang/Option import { * }
from /capy/collection/List import { * }

fun answer(): int = [10, 20, 12].reduce(0, (acc, value) => acc + value)

fun maybe(flag: bool): Option[String] =
    if flag then Some { "ok" } else None {}

fun read(flag: bool): String =
    match maybe(flag) with
    case Some { value } -> value
    case None -> "none"
`);

    const result = runCapy([
        'compile-generate',
        'JS',
        '-i', sourceDir,
        '-o', generatedDir,
        '--linked-output', linkedDir,
    ]);

    assert.equal(result.status, 0, result.stderr);
    assert.equal(result.stdout.trim(), '');
    assert.equal(await exists(join(linkedDir, 'program.json')), true);
    assert.equal(await exists(join(linkedDir, 'foo', 'Main.json')), true);
    assert.equal(await exists(join(generatedDir, 'foo', 'Main.js')), true);
    assert.equal(await exists(join(generatedDir, 'dev', 'capylang', 'capybara.js')), true);

    const node = runNode([
        '-e',
        `const m = require(${JSON.stringify(join(generatedDir, 'foo', 'Main.js'))}); console.log([m.answer(), m.read(true), m.read(false)].join('|'));`,
    ]);
    assert.equal(node.status, 0, node.stderr);
    assert.equal(node.stdout.trim(), '42|ok|none');
});

test('generate JS from linked input', async () => {
    const root = await tempProject();
    const sourceDir = join(root, 'src');
    const linkedDir = join(root, 'linked');
    const generatedDir = join(root, 'generated');
    await mkdir(join(sourceDir, 'foo'), { recursive: true });
    await writeFile(join(sourceDir, 'foo', 'Main.cfun'), 'fun answer(): int = 7\n');

    const compile = runCapy(['compile', '-i', sourceDir, '-o', linkedDir]);
    assert.equal(compile.status, 0, compile.stderr);

    const generate = runCapy(['generate', 'javascript', '-i', linkedDir, '-o', generatedDir]);
    assert.equal(generate.status, 0, generate.stderr);
    assert.equal(await exists(join(generatedDir, 'foo', 'Main.js')), true);

    const node = runNode([
        '-e',
        `const m = require(${JSON.stringify(join(generatedDir, 'foo', 'Main.js'))}); console.log(m.answer());`,
    ]);
    assert.equal(node.status, 0, node.stderr);
    assert.equal(node.stdout.trim(), '7');
});

test('compile-generate JS accepts object-oriented modules', async () => {
    const root = await tempProject();
    const sourceDir = join(root, 'src');
    const generatedDir = join(root, 'generated');
    await mkdir(join(sourceDir, 'foo'), { recursive: true });
    await writeFile(join(sourceDir, 'foo', 'Main.coo'), `
class Main {
    def main(args: List[String]): int = args.size()
}
`);

    const result = runCapy(['compile-generate', 'JS', '-i', sourceDir, '-o', generatedDir]);

    assert.equal(result.status, 0, result.stderr);
    assert.equal(await exists(join(generatedDir, 'foo', 'Main.js')), true);

    const node = runNode([join(generatedDir, 'foo', 'Main.js'), 'one', 'two']);
    assert.equal(node.status, 0, node.stderr);
    assert.equal(node.stdout.trim(), '2');
});

test('generated JS output is pruned by manifest on reuse', async () => {
    const root = await tempProject();
    const sourceDir = join(root, 'src');
    const generatedDir = join(root, 'generated');
    await mkdir(join(sourceDir, 'foo'), { recursive: true });
    await mkdir(join(generatedDir, 'stale'), { recursive: true });
    await writeFile(join(sourceDir, 'foo', 'Main.cfun'), 'fun answer(): int = 1\n');
    await writeFile(join(generatedDir, 'stale', 'Old.js'), 'module.exports = {};\n');

    const result = runCapy(['compile-generate', 'JS', '-i', sourceDir, '-o', generatedDir]);

    assert.equal(result.status, 0, result.stderr);
    assert.equal(await exists(join(generatedDir, 'foo', 'Main.js')), true);
    assert.equal(await exists(join(generatedDir, 'stale', 'Old.js')), false);
    const manifest = await readFile(join(generatedDir, '.capy-output-manifest'), 'utf8');
    assert.equal(manifest.includes('foo/Main.js'), true);
});
