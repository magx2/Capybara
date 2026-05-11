const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const { spawnSync } = require('node:child_process');

const generatedRoot = process.env.CAPY_E2E_CFUN_JS_GENERATED_DIR;
assert.ok(generatedRoot, 'CAPY_E2E_CFUN_JS_GENERATED_DIR must be provided by the Gradle e2e-cfun-js task');

const modulePath = relativePath => path.join(generatedRoot, relativePath);
const capy = require(modulePath('dev/capylang/capybara.js'));
const generatedModule = relativePath => require(modulePath(relativePath));

function collectGeneratedJsFiles(directory) {
    return fs.readdirSync(directory, { withFileTypes: true })
            .flatMap(entry => {
                const entryPath = path.join(directory, entry.name);
                if (entry.isDirectory()) {
                    return collectGeneratedJsFiles(entryPath);
                }
                return entry.name.endsWith('.js') ? [entryPath] : [];
            });
}

function assertCapyEqual(actual, expected) {
    assert.equal(capy.equals(actual, expected), true, `${capy.toStringValue(actual)} != ${capy.toStringValue(expected)}`);
}

function assertSuccess(result) {
    assert.equal(capy.isType(result, 'Success'), true, capy.toStringValue(result));
    return result.value;
}

function assertError(result) {
    assert.equal(capy.isType(result, 'Error'), true, capy.toStringValue(result));
    return result.message;
}

function assertSome(option, expected) {
    assert.equal(capy.isType(option, 'Some'), true, capy.toStringValue(option));
    assertCapyEqual(option.value, expected);
    return option.value;
}

function assertNone(option) {
    assert.equal(capy.isType(option, 'None'), true, capy.toStringValue(option));
}

function captureOutput(fn) {
    const originalOut = process.stdout.write;
    const originalErr = process.stderr.write;
    const out = [];
    const err = [];
    process.stdout.write = chunk => {
        out.push(String(chunk));
        return true;
    };
    process.stderr.write = chunk => {
        err.push(String(chunk));
        return true;
    };
    try {
        return { result: fn(), stdout: out.join(''), stderr: err.join('') };
    } finally {
        process.stdout.write = originalOut;
        process.stderr.write = originalErr;
    }
}

function unsafe(effect) {
    return effect.unsafe_run();
}

module.exports = {
    assert,
    fs,
    os,
    path,
    spawnSync,
    generatedRoot,
    modulePath,
    capy,
    generatedModule,
    collectGeneratedJsFiles,
    assertCapyEqual,
    assertSuccess,
    assertError,
    assertSome,
    assertNone,
    captureOutput,
    unsafe,
};
