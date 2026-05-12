'use strict';

const fs = require('fs');
const path = require('path');

function parseArgs(argv) {
    const options = {
        tests: [],
        availableTests: false,
        log: 'NONE',
    };
    for (let index = 0; index < argv.length; index++) {
        const arg = argv[index];
        switch (arg) {
            case '--generated-dir':
                options.generatedDir = requireValue(argv, ++index, arg);
                break;
            case '--output-dir':
                options.outputDir = requireValue(argv, ++index, arg);
                break;
            case '--report-type':
                options.reportType = requireValue(argv, ++index, arg).toUpperCase();
                break;
            case '--log':
                options.log = requireValue(argv, ++index, arg).toUpperCase();
                break;
            case '--tests':
                options.tests.push(requireValue(argv, ++index, arg));
                break;
            case '--available-tests':
                options.availableTests = true;
                break;
            default:
                throw new Error(`Unknown argument: ${arg}`);
        }
    }
    if (!options.generatedDir) {
        throw new Error('Missing --generated-dir');
    }
    if (!options.availableTests && !options.outputDir) {
        throw new Error('Missing --output-dir');
    }
    if (!options.availableTests && options.reportType !== 'JUNIT') {
        throw new Error('Only JUNIT report type is supported');
    }
    return options;
}

function requireValue(argv, index, option) {
    if (index >= argv.length) {
        throw new Error(`Missing value for ${option}`);
    }
    return argv[index];
}

function unsafeRun(value) {
    while (value && (typeof value.unsafe_run === 'function' || typeof value.unsafeRun === 'function')) {
        value = typeof value.unsafe_run === 'function' ? value.unsafe_run() : value.unsafeRun();
    }
    return value;
}

function flattenTestValues(value) {
    value = unsafeRun(value);
    if (value == null) {
        return [];
    }
    if (Array.isArray(value)) {
        return value.flatMap(flattenTestValues);
    }
    return [value];
}

function testFileName(testFile) {
    return testFile.file_name ?? testFile.fileName ?? testFile.path;
}

function testCases(testFile) {
    return testFile.test_cases ?? testFile.testCases ?? [];
}

function normalizedTestFileName(fileName) {
    return String(fileName).endsWith('.cfun') ? String(fileName).slice(0, -'.cfun'.length) : String(fileName);
}

function escapeTestName(testName) {
    return String(testName).replace(/\\/g, '\\\\').replace(/"/g, '\\"');
}

function unescapeTestName(testName) {
    let result = '';
    let escaping = false;
    for (const ch of testName) {
        if (escaping) {
            result += ch;
            escaping = false;
        } else if (ch === '\\') {
            escaping = true;
        } else {
            result += ch;
        }
    }
    return escaping ? result + '\\' : result;
}

function testId(testFile, testCase) {
    return `${normalizedTestFileName(testFileName(testFile))}."${escapeTestName(testCase.name)}"`;
}

function parseSelector(raw) {
    const selector = raw.trim();
    const separator = selector.indexOf('."');
    if (separator < 0) {
        return { raw, fileName: normalizedTestFileName(selector), testName: null };
    }
    if (!selector.endsWith('"')) {
        throw new Error(`Invalid test selector \`${raw}\``);
    }
    return {
        raw,
        fileName: normalizedTestFileName(selector.slice(0, separator)),
        testName: unescapeTestName(selector.slice(separator + 2, -1)),
    };
}

function selectorMatchesFile(selector, fileName) {
    return selector.fileName === normalizedTestFileName(fileName);
}

function selectorMatches(selector, fileName, testName) {
    return selectorMatchesFile(selector, fileName) && (selector.testName == null || selector.testName === testName);
}

function filterTestFiles(files, rawSelectors) {
    if (rawSelectors.length === 0) {
        return files;
    }
    const selectors = rawSelectors.map(parseSelector);
    const matched = new Array(selectors.length).fill(false);
    const filtered = [];

    for (const testFile of files) {
        const fileName = testFileName(testFile);
        for (let i = 0; i < selectors.length; i++) {
            if (selectors[i].testName == null && selectorMatchesFile(selectors[i], fileName)) {
                matched[i] = true;
            }
        }
        const cases = testCases(testFile).filter(testCase => {
            let include = false;
            for (let i = 0; i < selectors.length; i++) {
                if (selectorMatches(selectors[i], fileName, testCase.name)) {
                    matched[i] = true;
                    include = true;
                }
            }
            return include;
        });
        if (cases.length > 0) {
            filtered.push({ ...testFile, test_cases: cases, testCases: cases });
        }
    }

    const missing = selectors.filter((_, index) => !matched[index]).map(selector => selector.raw);
    if (missing.length === 1) {
        throw new Error(`Test selector \`${missing[0]}\` did not match any test`);
    }
    if (missing.length > 1) {
        throw new Error(`Test selectors did not match any test: ${missing.map(selector => `\`${selector}\``).join(', ')}`);
    }
    return filtered;
}

function evaluateAssertions(value) {
    value = unsafeRun(value);
    if (value == null) {
        return [{ result: false, message: 'Test returned no assertions', type: 'TestRunner' }];
    }
    if (Array.isArray(value)) {
        return value.flatMap(evaluateAssertions);
    }
    if (Array.isArray(value.assertions)) {
        return value.assertions.map(supplier => {
            const assertion = unsafeRun(typeof supplier === 'function' ? supplier() : supplier);
            return {
                result: Boolean(assertion.result),
                message: assertion.message ?? '',
                type: assertion.type ?? 'Assertion',
            };
        });
    }
    if (typeof value.succeeded === 'function') {
        return [{ result: Boolean(value.succeeded()), message: value.message ?? '', type: value.type ?? 'Assertion' }];
    }
    if (Object.prototype.hasOwnProperty.call(value, 'result')) {
        return [{ result: Boolean(value.result), message: value.message ?? '', type: value.type ?? 'Assertion' }];
    }
    return [{ result: false, message: `Unsupported assertion result: ${String(value)}`, type: 'TestRunner' }];
}

function runCase(testCase) {
    const startedAt = process.hrtime.bigint();
    try {
        const bodyResult = typeof testCase.body === 'function' ? testCase.body() : testCase.result;
        const assertions = evaluateAssertions(bodyResult);
        const failed = assertions.find(assertion => !assertion.result);
        return {
            name: testCase.name,
            assertions,
            failed,
            error: null,
            time: Number(process.hrtime.bigint() - startedAt) / 1_000_000_000,
        };
    } catch (error) {
        return {
            name: testCase.name,
            assertions: [],
            failed: { result: false, message: error && error.stack ? error.stack : String(error), type: 'Error' },
            error,
            time: Number(process.hrtime.bigint() - startedAt) / 1_000_000_000,
        };
    }
}

function teamCityEscape(value) {
    return String(value)
        .replace(/\|/g, '||')
        .replace(/'/g, "|'")
        .replace(/\n/g, '|n')
        .replace(/\r/g, '|r')
        .replace(/\[/g, '|[')
        .replace(/]/g, '|]');
}

function logSuiteStarted(logType, name) {
    if (logType === 'TC' || logType === 'TEAM_CITY') {
        console.log(`##teamcity[testSuiteStarted name='${teamCityEscape(name)}']`);
    }
}

function logSuiteFinished(logType, name) {
    if (logType === 'TC' || logType === 'TEAM_CITY') {
        console.log(`##teamcity[testSuiteFinished name='${teamCityEscape(name)}']`);
    }
}

function logTestStarted(logType, name) {
    if (logType === 'TC' || logType === 'TEAM_CITY') {
        console.log(`##teamcity[testStarted name='${teamCityEscape(name)}']`);
    }
}

function logTestFinished(logType, name) {
    if (logType === 'TC' || logType === 'TEAM_CITY') {
        console.log(`##teamcity[testFinished name='${teamCityEscape(name)}']`);
    }
}

function logTestFailed(logType, name, message) {
    if (logType === 'TC' || logType === 'TEAM_CITY') {
        console.log(`##teamcity[testFailed name='${teamCityEscape(name)}' message='${teamCityEscape(message)}']`);
    }
}

function xmlEscape(value) {
    return String(value)
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;')
        .replace(/'/g, '&apos;');
}

function suiteReport(testFile, results) {
    const fileName = testFileName(testFile);
    const failures = results.filter(result => result.failed).length;
    const assertions = results.reduce((sum, result) => sum + result.assertions.length, 0);
    const time = results.reduce((sum, result) => sum + result.time, 0);
    const cases = results.map(result => {
        const base = `  <testcase name="${xmlEscape(result.name)}" classname="${xmlEscape(fileName)}" assertions="${result.assertions.length}" time="${result.time}" file="${xmlEscape(fileName)}" line="0">`;
        if (!result.failed) {
            return `${base}</testcase>`;
        }
        return `${base}<failure message="${xmlEscape(result.failed.message)}" type="${xmlEscape(result.failed.type)}">${xmlEscape(result.failed.message)}</failure></testcase>`;
    }).join('');
    return `<testsuite name="${xmlEscape(fileName)}" tests="${results.length}" failures="${failures}" errors="0" skipped="0" assertions="${assertions}" time="${time}" timestamp="${new Date().toISOString()}">${cases}</testsuite>`;
}

function reportPath(outputDir, fileName) {
    const normalized = String(fileName).replace(/^\/+/, '').replace(/\//g, '.');
    return path.join(outputDir, `TEST-${normalized}.xml`);
}

function run(options) {
    const runtimePath = path.resolve(options.generatedDir, 'capy', 'test', 'CapyTestRuntime.js');
    const runtime = require(runtimePath);
    let files = flattenTestValues(runtime.gatherTests());

    if (options.availableTests) {
        files.flatMap(file => testCases(file).map(testCase => testId(file, testCase))).forEach(id => console.log(id));
        return 0;
    }

    files = filterTestFiles(files, options.tests);
    fs.mkdirSync(options.outputDir, { recursive: true });

    let failed = false;
    for (const testFile of files) {
        const fileName = testFileName(testFile);
        logSuiteStarted(options.log, fileName);
        const results = [];
        for (const testCase of testCases(testFile)) {
            logTestStarted(options.log, testCase.name);
            const result = runCase(testCase);
            if (result.failed) {
                failed = true;
                logTestFailed(options.log, testCase.name, result.failed.message);
            }
            logTestFinished(options.log, testCase.name);
            results.push(result);
        }
        logSuiteFinished(options.log, fileName);
        fs.writeFileSync(reportPath(options.outputDir, fileName), suiteReport(testFile, results));
    }
    return failed ? 1 : 0;
}

try {
    process.exitCode = run(parseArgs(process.argv.slice(2)));
} catch (error) {
    console.error(error && error.stack ? error.stack : String(error));
    process.exitCode = 2;
}
