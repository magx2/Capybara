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
    if (!options.availableTests && !supportedReportTypes().includes(options.reportType)) {
        throw new Error(`Supported report types are ${supportedReportTypes().join(', ')}`);
    }
    return options;
}

function supportedReportTypes() {
    return ['JUNIT', 'CTRF', 'JEST'];
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

function defineRuntimeMethod(target, name, fn) {
    Object.defineProperty(target, name, { value: fn, enumerable: false, configurable: true, writable: true });
}

function installRuntimeCompatibility(runtime) {
    const truthy = value => runtime.__capy_truthy ? runtime.__capy_truthy(unsafeRun(value)) : Boolean(unsafeRun(value));
    const applyLambda = (fn, item) => {
        const arity = typeof fn === 'function' ? fn.length : 1;
        if (arity === 0) return fn();
        if (arity > 1 && Array.isArray(item)) return fn(...item);
        return fn(item);
    };
    const asIterable = value => {
        value = unsafeRun(value);
        if (value == null) return [];
        if (typeof value === 'string') return Array.from(value);
        if (value instanceof Map) return value.entries();
        if (value && typeof value[Symbol.iterator] === 'function') return value;
        return [];
    };
    const dataIs = (value, typeName) => runtime.__capy_data_is
        ? runtime.__capy_data_is(value, typeName)
        : value && String(value.__type || '').split(/[/.]/).pop() === typeName;
    const matchBinding = (value, name, index) => runtime.__capy_match_binding
        ? runtime.__capy_match_binding(value, name, index)
        : value?.[name] ?? value?.[`$${index}`];
    const none = () => runtime.__capy_data
        ? runtime.__capy_data('None', Object.fromEntries([]))
        : { __type: 'None' };
    const optionFilter = (value, predicate) => {
        value = unsafeRun(value);
        if (dataIs(value, 'None')) return value;
        if (dataIs(value, 'Some')) {
            const inner = matchBinding(value, 'value', 0);
            return truthy(predicate(inner)) ? none() : value;
        }
        throw new Error('Unsupported CFUN expression: option filter');
    };
    const optionReduce = (value, initial, reducer) => {
        value = unsafeRun(value);
        if (dataIs(value, 'None')) return initial;
        if (dataIs(value, 'Some')) return unsafeRun(reducer(initial, matchBinding(value, 'value', 0)));
        throw new Error('Unsupported CFUN expression: option reduce');
    };
    const patchOptionData = value => {
        if (dataIs(value, 'Some') || dataIs(value, 'None')) {
            defineRuntimeMethod(value, 'filter', predicate => optionFilter(value, predicate));
            defineRuntimeMethod(value, 'reduce', (initial, reducer) => optionReduce(value, initial, reducer));
            defineRuntimeMethod(value, 'reduce_left', value.reduce);
        }
        return value;
    };
    const seqReject = (value, predicate) => {
        value = unsafeRun(value);
        if (value instanceof Map) {
            const result = new Map();
            for (const [key, item] of value) {
                if (!truthy(applyLambda(predicate, [key, item]))) result.set(key, item);
            }
            return result;
        }
        const result = [];
        for (const item of asIterable(value)) {
            if (!truthy(applyLambda(predicate, item))) result.push(item);
        }
        return result;
    };
    const originalReduce = runtime.__capy_reduce;
    const reduce = (value, initial, keyed, reducer) => {
        value = unsafeRun(value);
        if (dataIs(value, 'Some') || dataIs(value, 'None')) {
            return optionReduce(value, initial, reducer);
        }
        if (typeof originalReduce === 'function') {
            return originalReduce(value, initial, keyed, reducer);
        }
        let result = initial;
        for (const item of asIterable(value)) {
            result = reducer(result, item);
        }
        return result;
    };

    if (typeof runtime.__capy_data === 'function') {
        const originalData = runtime.__capy_data;
        runtime.__capy_data = (typeName, fields) => patchOptionData(originalData(typeName, fields));
    }
    runtime.__capy_reduce = reduce;
    runtime.__capy_option_filter = optionFilter;
    runtime.__capy_option_reduce = optionReduce;
    runtime.__capy_seq_reject = seqReject;

    defineRuntimeMethod(Array.prototype, 'reject', function reject(predicate) { return seqReject(this, predicate); });
    defineRuntimeMethod(Set.prototype, 'reject', function reject(predicate) { return seqReject(this, predicate); });
    defineRuntimeMethod(Map.prototype, 'reject', function reject(predicate) { return seqReject(this, predicate); });
    defineRuntimeMethod(String.prototype, 'reject', function reject(predicate) { return seqReject(String(this), predicate); });
    const stringFilterPipe = function filterPipe(predicate) {
        return typeof runtime.__capy_seq_filter === 'function'
            ? runtime.__capy_seq_filter(String(this), predicate)
            : Array.from(String(this)).filter(item => truthy(predicate(item)));
    };
    defineRuntimeMethod(String.prototype, '|-', stringFilterPipe);
    defineRuntimeMethod(String.prototype, '__', stringFilterPipe);
    defineRuntimeMethod(String.prototype, '____', stringFilterPipe);
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

function appendSelectedGeneratedTestFiles(files, generatedDir, rawSelectors) {
    if (rawSelectors.length === 0) {
        return files;
    }

    const existingFileNames = new Set(files.map(file => normalizedTestFileName(testFileName(file))));
    const appendedFileNames = new Set();
    const appended = [];
    for (const selector of rawSelectors.map(parseSelector)) {
        if (existingFileNames.has(selector.fileName) || appendedFileNames.has(selector.fileName)) {
            continue;
        }

        const modulePath = path.resolve(generatedDir, selector.fileName.replace(/^\/+/, '') + 'CapyTest.js');
        if (!fs.existsSync(modulePath)) {
            continue;
        }

        const module = require(modulePath);
        if (typeof module.tests === 'function') {
            const testFiles = flattenTestValues(module.tests());
            appended.push(...testFiles);
            for (const testFile of testFiles) {
                appendedFileNames.add(normalizedTestFileName(testFileName(testFile)));
            }
        }
    }
    return files.concat(appended);
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
    const start = Date.now();
    const startedAt = process.hrtime.bigint();
    try {
        const bodyResult = typeof testCase.body === 'function' ? testCase.body() : testCase.result;
        const assertions = evaluateAssertions(bodyResult);
        const failed = assertions.find(assertion => !assertion.result);
        const durationMs = Math.max(0, Math.round(Number(process.hrtime.bigint() - startedAt) / 1_000_000));
        return {
            name: testCase.name,
            assertions,
            failed,
            error: null,
            time: durationMs / 1000,
            durationMs,
            start,
            stop: start + durationMs,
        };
    } catch (error) {
        const durationMs = Math.max(0, Math.round(Number(process.hrtime.bigint() - startedAt) / 1_000_000));
        return {
            name: testCase.name,
            assertions: [],
            failed: { result: false, message: error && error.stack ? error.stack : String(error), type: 'Error' },
            error,
            time: durationMs / 1000,
            durationMs,
            start,
            stop: start + durationMs,
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

function shouldWriteJUnit(reportType) {
    return reportType === 'JUNIT';
}

function shouldWriteCtrf(reportType) {
    return reportType === 'CTRF';
}

function shouldWriteJest(reportType) {
    return reportType === 'JEST';
}

function ctrfReport(suites, start, stop) {
    const tests = suites.flatMap(suite => suite.results.map(result => {
        const test = {
            name: result.name,
            status: result.failed ? 'failed' : 'passed',
            duration: result.durationMs,
            start: result.start,
            stop: result.stop,
            suite: [suite.fileName],
            filePath: suite.fileName,
        };
        if (result.failed) {
            test.message = String(result.failed.message ?? '');
            test.trace = String(result.failed.message ?? '');
            test.extra = { failureType: String(result.failed.type ?? 'Assertion') };
        }
        return test;
    }));
    const failed = tests.filter(test => test.status === 'failed').length;
    const report = {
        reportFormat: 'CTRF',
        specVersion: '0.0.0',
        timestamp: new Date(stop).toISOString(),
        generatedBy: 'Capybara',
        results: {
            tool: { name: 'Capybara' },
            summary: {
                tests: tests.length,
                passed: tests.length - failed,
                failed,
                skipped: 0,
                pending: 0,
                other: 0,
                suites: suites.length,
                start,
                stop,
                duration: Math.max(0, stop - start),
            },
            tests,
        },
    };
    return `${JSON.stringify(report, null, 2)}\n`;
}

function ctrfReportPath(outputDir) {
    return path.join(outputDir, 'ctrf-report.json');
}

function jestReport(suites, start) {
    const failedSuites = suites.filter(suite => suite.results.some(result => result.failed)).length;
    const results = suites.flatMap(suite => suite.results);
    const failedTests = results.filter(result => result.failed).length;
    const report = {
        success: failedTests === 0,
        numTotalTestSuites: suites.length,
        numPassedTestSuites: suites.length - failedSuites,
        numFailedTestSuites: failedSuites,
        numPendingTestSuites: 0,
        numTotalTests: results.length,
        numPassedTests: results.length - failedTests,
        numFailedTests: failedTests,
        numPendingTests: 0,
        numTodoTests: 0,
        startTime: start,
        testResults: suites.map(suite => {
            const suiteFailed = suite.results.some(result => result.failed);
            const suiteStart = suite.results[0]?.start ?? start;
            const suiteEnd = suite.results.length === 0
                ? suiteStart
                : Math.max(...suite.results.map(result => result.stop));
            return {
                name: suite.fileName,
                status: suiteFailed ? 'failed' : 'passed',
                startTime: suiteStart,
                endTime: suiteEnd,
                assertionResults: suite.results.map(result => ({
                    ancestorTitles: [suite.fileName],
                    title: result.name,
                    status: result.failed ? 'failed' : 'passed',
                    duration: result.durationMs,
                    failureMessages: result.failed ? [String(result.failed.message ?? '')] : [],
                    location: null,
                })),
            };
        }),
    };
    return `${JSON.stringify(report, null, 2)}\n`;
}

function jestReportPath(outputDir) {
    return path.join(outputDir, 'jest-report.json');
}

function run(options) {
    const runtimePath = path.resolve(options.generatedDir, 'capy', 'test', 'CapyTestRuntime.js');
    const runtime = require(runtimePath);
    installRuntimeCompatibility(runtime);
    let files = flattenTestValues(runtime.gatherTests());
    files = appendSelectedGeneratedTestFiles(files, options.generatedDir, options.tests);

    if (options.availableTests) {
        files.flatMap(file => testCases(file).map(testCase => testId(file, testCase))).forEach(id => console.log(id));
        return 0;
    }

    files = filterTestFiles(files, options.tests);
    fs.mkdirSync(options.outputDir, { recursive: true });

    let failed = false;
    const suites = [];
    const runStartedAt = Date.now();
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
        suites.push({ testFile, fileName, results });
        if (shouldWriteJUnit(options.reportType)) {
            fs.writeFileSync(reportPath(options.outputDir, fileName), suiteReport(testFile, results));
        }
    }
    if (shouldWriteCtrf(options.reportType)) {
        fs.writeFileSync(ctrfReportPath(options.outputDir), ctrfReport(suites, runStartedAt, Date.now()));
    }
    if (shouldWriteJest(options.reportType)) {
        fs.writeFileSync(jestReportPath(options.outputDir), jestReport(suites, runStartedAt));
    }
    return failed ? 1 : 0;
}

try {
    process.exitCode = run(parseArgs(process.argv.slice(2)));
} catch (error) {
    console.error(error && error.stack ? error.stack : String(error));
    process.exitCode = 2;
}
