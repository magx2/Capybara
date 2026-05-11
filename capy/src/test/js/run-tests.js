const { readdir } = require('node:fs/promises');
const { join, resolve } = require('node:path');
const { pathToFileURL } = require('node:url');

async function collectTestFiles(directory) {
    const entries = await readdir(directory, { withFileTypes: true });
    const tests = [];

    for (const entry of entries) {
        const entryPath = join(directory, entry.name);
        if (entry.isDirectory()) {
            tests.push(...await collectTestFiles(entryPath));
        } else if (entry.name.endsWith('.test.js')) {
            tests.push(entryPath);
        }
    }

    return tests;
}

async function main() {
    const testRoot = resolve(process.argv[2] ?? __dirname);
    const tests = (await collectTestFiles(testRoot)).sort();

    if (tests.length === 0) {
        throw new Error(`No JavaScript test files found under ${testRoot}`);
    }

    for (const testFile of tests) {
        await import(pathToFileURL(testFile).href);
    }
}

main().catch((error) => {
    console.error(error);
    process.exitCode = 1;
});
