'use strict';

const fs = require('fs');
const path = require('path');

const { parser } = require('dev/capylang/compiler/parser/CapybaraParserProvider.js');

function sourceKind(name) {
    return { __type: name };
}

function rawModule(name, modulePath, source, kind) {
    return {
        __type: 'RawModule',
        name,
        path: modulePath,
        input: source,
        sourceKind: sourceKind(kind),
    };
}

function unsafeRun(effect) {
    let value = effect;
    while (value && (typeof value.unsafe_run === 'function' || typeof value.unsafeRun === 'function')) {
        value = typeof value.unsafe_run === 'function' ? value.unsafe_run() : value.unsafeRun();
    }
    return value;
}

function assertEqual(actual, expected, label) {
    if (actual !== expected) {
        throw new Error(`${label}: expected ${JSON.stringify(expected)}, got ${JSON.stringify(actual)}`);
    }
}

const modules = [
    rawModule(
        'ImportedFunctional',
        'dev/capylang/smoke',
        'from /capy/lang/String import { String }\n'
            + 'data User { name: String }\n'
            + 'fun greet(user: User): String = "hello {user.name}"\n',
        'FUNCTIONAL'
    ),
    rawModule(
        'Clock',
        'dev/capylang/smoke',
        'interface Clock {\n'
            + '  def now(): int\n'
            + '}\n'
            + 'class FixedClock(value: int): Clock {\n'
            + '  field name: String = "fixed"\n'
            + '  def now(): int = value\n'
            + '}\n',
        'OBJECT_ORIENTED'
    ),
];

const parserImpl = unsafeRun(parser());
const parsed = parserImpl.parse(modules);

assertEqual(parsed.__type, 'ParsedProgram', 'parsed type');
assertEqual(parsed.modules.length, 2, 'parsed module count');
assertEqual(parsed.modules[0].imports[0].modulePath, '/capy/lang/String', 'import module');
assertEqual(parsed.modules[0].definitions[0].__type, 'DataDeclaration', 'first functional definition');
assertEqual(parsed.modules[1].objectOriented.interfaces[0].name, 'Clock', 'object interface');
assertEqual(parsed.modules[1].objectOriented.classes[0].name, 'FixedClock', 'object class');

try {
    const { compile } = require('dev/capylang/compiler/CapybaraCompiler.js');
    const { empty_native_provider_manifest } = require('dev/capylang/compiler/CompiledProgram.js');
    const emptyLibraries = { to_list: () => [] };
    const compilerResult = unsafeRun(compile(
        [
            rawModule('Simple', 'dev/capylang/smoke', 'fun one(): int = 1\n', 'FUNCTIONAL'),
            rawModule('SimpleObject', 'dev/capylang/smoke', 'interface SimpleObject {\n  def value(): int\n}\n', 'OBJECT_ORIENTED'),
        ],
        emptyLibraries,
        empty_native_provider_manifest(),
        empty_native_provider_manifest()
    ));
    assertEqual(compilerResult.__type, 'Left', 'compiler result');
} catch (error) {
    throw new Error(`generated JavaScript compiler smoke failed: ${error.message}`);
}

const output = process.env.JAVASCRIPT_NATIVE_PARSER_SMOKE_OUTPUT;
if (output) {
    fs.mkdirSync(output, { recursive: true });
    fs.writeFileSync(path.join(output, 'native-parser-smoke.txt'), 'ok\n', 'utf8');
}
