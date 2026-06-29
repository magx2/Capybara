'use strict';

const { main } = require('dev/capylang/cli/Capy.js');
const { __capy_unsafe_run } = require('capy/test/CapyTestRuntime.js');

function dataField(value, name, fallback) {
    if (value && typeof value === 'object' && Object.prototype.hasOwnProperty.call(value, name)) {
        return value[name];
    }
    return fallback;
}

function exitCodeValue(value) {
    const raw = dataField(value, 'value', value);
    const numeric = Number(raw);
    return Number.isInteger(numeric) ? numeric : 1;
}

function programExitCode(program) {
    const result = __capy_unsafe_run(program);
    const type = result && typeof result === 'object' ? String(result.__type || '') : '';
    if (type === 'Failed' || type.endsWith('.Failed') || type.endsWith('/Failed')) {
        return exitCodeValue(dataField(result, 'exit_code', dataField(result, 'exitCode', dataField(result, 'value', 1))));
    }
    return 0;
}

process.exitCode = programExitCode(main(process.argv.slice(2)));
