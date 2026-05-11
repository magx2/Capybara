const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');

const generatedRoot = process.env.CAPY_E2E_COO_JS_GENERATED_DIR;
assert.ok(generatedRoot, 'CAPY_E2E_COO_JS_GENERATED_DIR must be provided by the Gradle e2e-coo-js task');

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

function captureOutput(fn) {
    const originalOut = process.stdout.write;
    const out = [];
    process.stdout.write = chunk => {
        out.push(String(chunk));
        return true;
    };
    try {
        return { result: fn(), stdout: out.join('') };
    } finally {
        process.stdout.write = originalOut;
    }
}

module.exports = {
    assert,
    fs,
    path,
    generatedRoot,
    modulePath,
    capy,
    generatedModule,
    collectGeneratedJsFiles,
    captureOutput,
};
