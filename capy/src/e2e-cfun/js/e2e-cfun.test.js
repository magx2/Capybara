const test = require('node:test');
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

test('translates primitive functions, booleans, and basic collections', () => {
    const simple = generatedModule('dev/capylang/test/SimpleFunction.js');
    assert.equal(simple.alwaysTrue(), true);
    assert.equal(simple.add(1, 2), 3);
    assert.equal(simple.subtract(1, 2), -1);
    assert.equal(simple.multiply(5, 3), 15);
    assert.equal(simple.divide(10, 2), 5);
    assert.equal(simple.divide(9, 2), 4);
    assert.equal(simple.classify(1), 'positive');
    assert.equal(simple.classify(0), 'non-positive');
    assert.equal(simple.classify(-1), 'non-positive');
    assert.equal(simple.isPositive(1), true);
    assert.equal(simple.isPositive(0), false);
    assert.equal(simple.greet('World'), 'Hello, World');
    assert.equal(simple.doubleThenClassify(1), 'positive');
    assert.equal(simple.orderOfExpression(3, 5), 77);
    assert.equal(simple.power(2, 3), 8);
    assert.equal(simple.power(-4, 3), -64);
    assertCapyEqual(simple.staticList(), [1, 2, 3]);
    assert.equal(simple.callPrivateAndPublic(3), 9);
    assert.equal(simple.typedLetString(), 'd');
    assert.equal(simple.typedLetTupleString(), 'd');
    assert.equal(simple.typedLetTupleInt(), 1);
    assert.equal(simple.typedLetBool(), true);
    assert.equal(simple.typedLetInt(), 7);
    assert.equal(simple.typedLetLong(), 7);
    assert.equal(simple.typedLetFloat(), 1.5);
    assert.equal(simple.typedLetDouble(), 1.5);
    assertCapyEqual(simple.typedLetList(), [1, 2, 3]);
    assertCapyEqual(simple.typedLetSet(), new Set([1, 2, 3]));
    assert.equal(simple.typedLetDict().get('a'), 1);

    const bitwise = generatedModule('dev/capylang/test/Bitwise.js');
    assert.equal(bitwise.bitAnd(0b1100, 0b1010), 0b1000);
    assert.equal(bitwise.bitNand(0b1100, 0b1010), ~0b1000);
    assert.equal(bitwise.bitOr(0b1100, 0b1010), 0b1110);
    assert.equal(bitwise.bitXor(0b1100, 0b1010), 0b0110);
    assert.equal(bitwise.bitNot(0b1100), ~0b1100);

    const boolLogic = generatedModule('dev/capylang/test/BoolLogic.js');
    assert.equal(boolLogic.andLogic(true, true), true);
    assert.equal(boolLogic.andLogic(true, false), false);
    assert.equal(boolLogic.andLogic(false, true), false);
    assert.equal(boolLogic.orLogic(false, true), true);
    assert.equal(boolLogic.orLogic(false, false), false);
    assert.equal(boolLogic.equalLogic(true, true), true);
    assert.equal(boolLogic.notEqualLogic(true, false), true);
    assert.equal(boolLogic.notLogic(false), true);
    assert.equal(boolLogic.notFieldAccess(new boolLogic.BoolBox({ failed: true })), false);
    assert.equal(boolLogic.notMethodCall(new boolLogic.BoolBox({ failed: false })), true);
    assert.equal(boolLogic.complexLogic(false, true), true);
    assert.equal(boolLogic.byteToBool(0), false);
    assert.equal(boolLogic.intToBool(2), true);
    assert.equal(boolLogic.stringToBool(''), false);
    assert.equal(boolLogic.listToBool([1]), true);
    assert.equal(boolLogic.setToBool(new Set()), false);
    assert.equal(boolLogic.dictToBool(new Map([['a', 1]])), true);
    assert.equal(boolLogic.andInt(2, 1), true);
    assert.equal(boolLogic.orString('', ''), false);
    assert.equal(boolLogic.notList([]), true);

    const list = generatedModule('dev/capylang/test/collection/ListCollection.js');
    assertCapyEqual(list.staticList(), [1, 2, 3]);
    assertCapyEqual(list.emptyStaticList(), []);
    assertCapyEqual(list.staticListWithTrailingComma(), [1, 2, 3]);
    assertCapyEqual(list.append([1, 2, 3]), [1, 2, 3, 5]);
    assertCapyEqual(list.appendList([1, 2], [3, 4]), [1, 2, 3, 4]);
    assertCapyEqual(list.remove([1, 2, 2, 3]), [1, 3]);
    assertCapyEqual(list.removeList([1, 2, 2, 3, 4], [2, 4]), [1, 3]);
    assert.equal(list.contains([1, 2, 3], 2), true);
    assert.equal(list.any([1, 2, 3]), true);
    assert.equal(list.any([0, 1, 2]), false);
    assert.equal(list.anyEmpty([]), false);
    assert.equal(list.all([1, 2, 3]), true);
    assert.equal(list.all([0, 1, 2]), false);
    assert.equal(list.allEmpty([]), true);
    assert.equal(list.isEmpty([]), true);
    assert.equal(list.notIsEmpty([1, 2, 3]), true);
    assert.equal(list.notNestedIsEmpty(new list.ListHolder({ values: [1, 2, 3] })), true);
    assert.equal(list.size([1, 2, 3]), 3);
    assert.equal(list.letNamedList(), 4);

    const set = generatedModule('dev/capylang/test/collection/SetCollection.js');
    assertCapyEqual(set.staticSet(), new Set([1, 2, 3]));
    assertCapyEqual(set.emptyStaticSet(), new Set());
    assertCapyEqual(set.staticSetWithTrailingComma(), new Set([1, 2, 3]));
    assertCapyEqual(set.append(new Set([1, 2, 3])), new Set([1, 2, 3, 5]));
    assertCapyEqual(set.appendSet(new Set([1, 2]), new Set([3, 4])), new Set([1, 2, 3, 4]));
    assertCapyEqual(set.remove(new Set([1, 2, 3])), new Set([1, 3]));
    assertCapyEqual(set.removeSet(new Set([1, 2, 3, 4]), new Set([2, 4])), new Set([1, 3]));
    assert.equal(set.contains(new Set([1, 2, 3]), 2), true);
    assert.equal(set.containsMethod(new Set([1, 2, 3]), 9), false);
    assert.equal(set.subsetNamed(new Set([1, 2]), new Set([1, 2, 3])), true);
    assert.equal(set.properSubsetSymbol(new Set([1, 2]), new Set([1, 2])), false);
    assert.equal(set.supersetNamed(new Set([1, 2, 3]), new Set([1, 2])), true);
    assert.equal(set.properSupersetSymbol(new Set([1, 2, 3]), new Set([1, 2])), true);
    assertCapyEqual(set.unionSymbol(new Set([1, 2]), new Set([2, 3])), new Set([1, 2, 3]));
    assertCapyEqual(set.intersectionNamed(new Set([1, 2, 3]), new Set([2, 3, 4])), new Set([2, 3]));
    assertCapyEqual(set.differenceNamed(new Set([1, 2, 3]), new Set([2])), new Set([1, 3]));
    assertCapyEqual(set.symmetricDifferenceNamed(new Set([1, 2, 3]), new Set([2, 3, 4])), new Set([1, 4]));
    assertCapyEqual(set.cartesianProductNamed(new Set([1, 2]), new Set(['a', 'b'])), new Set([[1, 'a'], [1, 'b'], [2, 'a'], [2, 'b']]));
    assertCapyEqual(set.powerSetNamed(new Set([1, 2])), new Set([new Set(), new Set([1]), new Set([2]), new Set([1, 2])]));
    assert.equal(set.allEmpty(new Set()), true);
    assert.equal(set.notNestedIsEmpty(new set.SetHolder({ values: new Set([1]) })), true);
    assert.equal(set.letNamedSet(), 3);

    const dict = generatedModule('dev/capylang/test/collection/DictCollection.js');
    const expectedDict = new Map([['one', 1], ['two', 2], ['three', 3]]);
    assertCapyEqual(dict.staticDict(), expectedDict);
    assertCapyEqual(dict.emptyStaticDict(), new Map());
    assertCapyEqual(dict.append(new Map(expectedDict)), new Map([['one', 1], ['two', 2], ['three', 3], ['four', 4]]));
    assertCapyEqual(dict.appendDict(new Map([['one', 1], ['two', 2]]), new Map([['three', 3], ['four', 4]])), new Map([['one', 1], ['two', 2], ['three', 3], ['four', 4]]));
    assertCapyEqual(dict.remove(new Map(expectedDict)), new Map([['one', 1], ['three', 3]]));
    assert.equal(dict.contains(expectedDict, 'two'), true);
    assert.equal(dict.anyValue(expectedDict), true);
    assert.equal(dict.anyEntry(new Map([['two', 3]])), false);
    assert.equal(dict.allValues(new Map([['zero', 0], ['one', 1]])), false);
    assert.equal(dict.allEntries(expectedDict), true);
    assert.equal(dict.notNestedIsEmpty(new dict.DictHolder({ values: expectedDict })), true);
    assert.equal(dict.letNamedDict(), 2);
    assertCapyEqual(dict.dictToList(expectedDict), [
        new dict.Entry({ k: 'one', v: 1 }),
        new dict.Entry({ k: 'two', v: 2 }),
        new dict.Entry({ k: 'three', v: 3 }),
    ]);
    assert.equal(dict.dictToString(expectedDict), 'one 1, two 2, three 3');

    const values = new Map([['a', 1], ['b', 2]]);
    assert.equal(dict.size(values), 2);
    assert.equal(dict.appendTuple(values).get('leet'), 1337);
    assert.equal(dict.appendTupleOverride(new Map(expectedDict)).get('two'), 200);

    const tuple = generatedModule('dev/capylang/test/collection/TupleCollection.js');
    assert.equal(tuple.tupleWithFunctionsApplyFirst(7), 8);
    assert.equal(tuple.tupleWithFunctionsApplySecond(7), 14);
});

test('translates data, enum, import, and visibility cases', () => {
    const data = generatedModule('dev/capylang/test/Data.js');
    assert.equal(data.area(new data.Circle({ radius: 2.0 })), 12.56);
    assert.equal(data.area(new data.Rectangle({ width: 3.0, height: 4.0 })), 12.0);
    assert.equal(data.daVinci(new data.Circle({ radius: 2.0 })).__capybaraType, 'Rectangle');
    assert.equal(data.daVinci(new data.Circle({ radius: 2.0 })).width, 4.0);
    assert.equal(data.daVinci(new data.Rectangle({ width: 6.0, height: 2.0 })).__capybaraType, 'Circle');
    assert.equal(data.daVinci(new data.Rectangle({ width: 6.0, height: 2.0 })).radius, 2.0);
    assert.equal(data.printableDataToString(), 'PrintableData { "foo": "abc", "boo": 123 }');
    assert.equal(data.stringPlusDataLeft(), 'prefix=PrintableData { "foo": "abc", "boo": 123 }');
    assert.equal(data.stringPlusDataRight(), 'PrintableData { "foo": "abc", "boo": 123 }=suffix');
    assert.equal(data.knightHolderToString(), 'KnightHolder { "knight": Tom }');
    assert.equal(data.createPositionalData('abc', 10).text, 'abc');
    assert.equal(data.createPositionalDataExpr('abc', 10).text, 'abc!');
    assert.equal(data.readDataType(data.createDataWithType('record')), 'record');
    assert.equal(data.genericFieldPipe(new data.Box({ value: '41' })).value, 43);
    assert.equal(data.genericFieldPipe(new data.Box({ value: 'x' })).__capybaraType, 'BoxError');

    const dataConstructor = generatedModule('dev/capylang/test/DataConstructor.js');
    assert.equal(dataConstructor.oddNumber(3), 3);
    assert.equal(dataConstructor.oddNumber(4), 5);
    assert.equal(dataConstructor.validatedUserAge(7), 'ok:7');
    assert.equal(dataConstructor.validatedUserAge(0), 'err:Age has to be greater than 0. Was 0.');
    assert.equal(dataConstructor.validatedUserAgeOrDefault(0), 1);
    assert.equal(dataConstructor.validatedNamedUser('Ada', 'admin'), 'ok:Ada:admin');
    assert.equal(dataConstructor.validatedNamedUser('', 'admin'), 'err:Name was empty');
    assert.equal(dataConstructor.validatedProfile('Ada', ''), 'err:Profile role was empty');

    const enumCollection = generatedModule('dev/capylang/test/EnumCollection.js');
    assert.equal(enumCollection.colorMatch(enumCollection.RED), 1);
    assert.equal(enumCollection.colorMatch(enumCollection.GREEN_BLUE), 3);
    assert.equal(enumCollection.colorMatchWildcard(enumCollection.WHITE), 0);
    assert.equal(enumCollection.colorOrder(enumCollection.GREEN_BLUE), 2);
    assert.equal(enumCollection.colorName(enumCollection.GREEN_BLUE), 'GREEN_BLUE');
    assertCapyEqual(enumCollection.colorValues(), new Set([
        enumCollection.RED,
        enumCollection.BLUE,
        enumCollection.GREEN_BLUE,
        enumCollection.YELLOW,
        enumCollection.BLACK,
        enumCollection.WHITE,
    ]));
    assert.equal(assertSuccess(enumCollection.parseColorName('BLUE')), enumCollection.BLUE);
    assert.equal(assertSuccess(enumCollection.parseColorOrder(3)), enumCollection.YELLOW);
    assert.match(assertError(enumCollection.parseColorName('UNKNOWN')), /Unable to parse Color/);
    assert.equal(enumCollection.qualifiedColorWithoutBraces(), enumCollection.BLUE);
    assert.equal(enumCollection.unqualifiedColorWithoutBraces(), enumCollection.GREEN_BLUE);
    assert.equal(enumCollection.inferredColorWithoutBraces(), enumCollection.RED);
    assertCapyEqual(enumCollection.colorListWithoutBraces(), [enumCollection.RED, enumCollection.BLUE, enumCollection.GREEN_BLUE]);
    assert.equal(enumCollection.paletteWithoutBraces().primary, enumCollection.WHITE);

    const enumData = generatedModule('dev/capylang/test/EnumDataField.js');
    const root = new enumData.Path({ root: enumData.ABSOLUTE, segments: [] });
    const relative = new enumData.Path({ root: enumData.RELATIVE, segments: [] });
    assert.equal(enumData.rootName(root), 'ABSOLUTE');
    assert.equal(enumData.isAbsolute(root), true);
    assert.equal(enumData.isAbsolute(relative), false);

    const derive = generatedModule('dev/capylang/test/DeriveFeature.js');
    assert.equal(derive.showUser(), 'DerivedUser { name, age }');
    assert.equal(derive.showEmpty(), 'Empty {  }');
    assert.equal(derive.showEmployee(), 'Employee { id, department }');
    assert.equal(derive.showNamed(), 'Named { id }');
    assert.equal(derive.userBiggerThan(40), true);
    assert.equal(derive.userMixedParameterDeriveFalse(), false);
    assert.equal(derive.userFieldValuesSummary(), 'name=Ada,age=42');

    const privateData = generatedModule('dev/capylang/test/PrivateData.js');
    assert.equal(privateData.parseInt(7), 'ok:7');
    assert.equal(privateData.parseConflict('x'), 'internal:x');
    assert.equal(Object.prototype.hasOwnProperty.call(privateData, '_Parse'), false);
    assert.equal(Object.prototype.hasOwnProperty.call(privateData, 'Parse'), true);

    const importsMain = generatedModule('dev/capylang/test/imports/Main.js');
    assert.equal(importsMain.calculator(2, 3, '+'), 5);
    assert.equal(importsMain.calculator(15, 0, 'p3'), 100);
    assert.equal(importsMain.x(2, 3, '+'), 'Day as usual');
    assert.equal(importsMain.xyz(1, 2, 3).xyz, '123');
    assert.equal(importsMain.type5(5, 2).__capybaraType, 'D51');
    assert.equal(importsMain.type5(0, 7).__capybaraType, 'D52');

    const packageLocal = generatedModule('dev/capylang/test/localVisibility/PackageLocalSupport.js');
    const packagePeer = generatedModule('dev/capylang/test/localVisibility/PackagePeer.js');
    const packageChild = generatedModule('dev/capylang/test/localVisibility/child/PackageChild.js');
    assert.equal(packageLocal.ownPackageValue(1), 6);
    assert.equal(packagePeer.peerCanUseLocalMembers(3), 8);
    assert.equal(packageChild.childCanUseLocalMembers(1), 6);
});

test('translates numeric conversions, native methods, strings, indexes, and slices', () => {
    const modulo = generatedModule('dev/capylang/test/Modulo.js');
    assert.equal(modulo.modInt(10, 3), 1);
    assert.equal(modulo.modInt(-10, 3), -1);
    assert.equal(modulo.modLong(10, 4), 2);
    assert.equal(modulo.modFloat(7.5, 2.0), 1.5);

    const min = generatedModule('dev/capylang/test/MinLiteralValue.js');
    assert.equal(min.minInt(), -2147483648);
    assert.equal(min.minIntDigits(), 10);

    const primitive = generatedModule('dev/capylang/test/PrimitiveMethods.js');
    assert.equal(primitive.longToInt(42), 42);
    assert.equal(primitive.longToInt(2147483648), -2147483648);
    assert.equal(primitive.longToInt(4294967295), -1);
    assert.equal(primitive.floatToInt(42.9), 42);
    assert.equal(primitive.floatToInt(-42.9), -42);
    assert.equal(primitive.doubleToInt(2147483648), 2147483647);
    assert.equal(primitive.doubleToLong(42.9), 42);

    const widening = generatedModule('dev/capylang/test/NumericWidening.js');
    assert.equal(widening.returnLong(7), 7);
    assert.equal(widening.multiplyWithLong(3), 30);
    assert.equal(widening.invokeDouble(1.25), 1.75);
    assert.equal(widening.invokeDoubleFromInt(6), 6.5);
    assert.equal(widening.buildMeasurement(9, 2.5).amount, 9);
    assert.equal(widening.buildMeasurementPositional(11, 4.5).ratio, 4.5);

    const native = generatedModule('dev/capylang/test/NativeTypes.js');
    assert.equal(native.stringLength('capybara'), 8);
    assert.equal(native.stringTrim('  capybara  '), 'capybara');
    assertSome(native.stringGet('capybara', 0), 'c');
    assertSome(native.stringGet('capybara', -1), 'a');
    assertNone(native.stringGet('capybara', 99));
    assert.equal(native.stringGetRange('abcdef', -4, -1), 'cde');
    assert.equal(native.listSize([1, 2, 3]), 3);
    assertSome(native.listGet([1, 2, 3], -1), 3);
    assertNone(native.listGet([1, 2, 3], 9));
    assertCapyEqual(native.listGetRangeMethod([1, 2, 3, 4, 5, 6], -4, -1), [3, 4, 5]);
    assert.equal(native.setContains(new Set([1, 2, 3]), 2), true);
    assert.equal(native.dictSize(new Map([['one', 1], ['two', 2]])), 2);
    assertSome(native.dictGet(new Map([['one', 1]]), 'one'), 1);
    assertNone(native.dictIndex(new Map([['one', 1]]), 'two'));
    assert.equal(native.tupleSecond([1, 'two']), 'two');

    const strings = generatedModule('dev/capylang/test/StringCollection.js');
    assert.equal(strings.contains('capybara', 'pyb'), true);
    assert.equal(strings.any('capybara'), true);
    assert.equal(strings.anyWithIndex('capybara'), true);
    assert.equal(strings.anyWithIndex('ycapi'), false);
    assert.equal(strings.all('capybara'), true);
    assert.equal(strings.allWithIndex('capy bara'), false);
    assert.equal(strings.startsWithMethod('capybara', 'cap'), true);
    assert.equal(strings.endWithMethod('capybara', 'bara'), true);
    assert.equal(strings.trimMethod('  capybara  '), 'capybara');
    assert.equal(strings.replaceMethod('aaaa', 'a', 'b'), 'bbbb');
    assert.equal(strings.notNestedIsEmpty(new strings.BufferHolder({ buffer: 'capybara' })), true);
    assert.equal(strings.notTrimmedIsEmpty('   '), false);
    assert.equal(assertSuccess(strings.toIntMethod('123')), 123);
    assert.match(assertError(strings.toIntMethod('abc')), /Cannot parse string to int: abc/);
    assert.equal(assertSuccess(strings.toLongMethod('12345678901')), 12345678901);
    assert.equal(assertSuccess(strings.toDoubleMethod('12.5')), 12.5);
    assert.equal(assertSuccess(strings.toFloatMethod('1.25')), 1.25);
    assert.equal(assertSuccess(strings.toBoolMethod('false')), false);
    assert.match(assertError(strings.toBoolMethod('abc')), /Cannot parse string to bool: abc/);

    const slice = generatedModule('dev/capylang/test/SliceCollection.js');
    assertCapyEqual(slice.listFrom1([0, 1, 2, 3, 4, 5]), [1, 2, 3, 4, 5]);
    assertCapyEqual(slice.listToMinus3([0, 1, 2, 3, 4, 5]), [0, 1, 2]);
    assertCapyEqual(slice.list1Minus2([0, 1, 2, 3, 4, 5]), [1, 2, 3]);
    assert.equal(slice.stringFrom1('abcdef'), 'bcdef');
    assert.equal(slice.stringToMinus3('abcdef'), 'abc');
    assert.equal(slice.string1Minus2('abcdef'), 'bcd');

    const index = generatedModule('dev/capylang/test/IndexCollection.js');
    assertSome(index.listIndex5([10, 20, 30, 40, 50, 60]), 60);
    assertNone(index.listIndex5([10, 20]));
    assertSome(index.stringIndexMinus2('capybara'), 'r');
    assertNone(index.stringIndexMinus2('a'));

    const precedence = generatedModule('dev/capylang/test/OperatorPrecedence.js');
    assert.equal(precedence.leapYear(2024), true);
    assert.equal(precedence.leapYear(1900), false);
    assert.equal(precedence.andBindsAfterModuloAndComparison(4, 9), true);
    assert.equal(precedence.orBindsAfterModuloAndComparison(5, 10), false);
    assert.equal(precedence.powerBindsBeforeMultiplication(), true);
    assert.equal(precedence.andBindsBeforeOr(), true);
});

test('translates match expressions, local declarations, overloads, and with expressions', () => {
    const match = generatedModule('dev/capylang/test/MatchByType.js');
    assert.equal(match.matchString('abc'), 's:abc');
    assert.equal(match.matchString(12), 'i:12');
    assert.equal(match.matchInt(9), 10);
    assert.equal(match.matchIntIgnoreBinding('x'), 0);
    assert.equal(match.isData(new match.Person({ name: 'Tom' })), true);
    assert.equal(match.isData(match.READY), true);
    assert.equal(match.classifyEnum(match.READY), 'READY');
    assert.equal(match.classifyEnum(new match.Person({ name: 'Tom' })), 'data');
    assert.equal(match.genericEnumName(match.DONE), 'DONE');
    assertCapyEqual(match.mergedEnumListNames(match.DONE), ['DONE', 'READY']);
    assert.equal(match.classifyJsonChar('{'), 'object');
    assert.equal(match.classifyJsonChar('"'), 'string');
    assert.equal(match.classifyJsonChar('7'), 'number');
    assert.equal(match.classifyJsonCharWithNamedWildcard('x'), 'unknown: x');
    assert.equal(match.mood(false), "I'm sad");
    assert.equal(match.bareGenericTypePattern(new Map([['a', 1]])), 3);
    assert.equal(match.bareGenericTypePattern(new capy.Some({ value: 'a' })), 4);

    const alternatives = generatedModule('dev/capylang/test/MatchCaseAlternatives.js');
    assert.equal(alternatives.digitLabel('1'), 'digit');
    assert.equal(alternatives.digitLabel('0'), 'zero');
    assert.equal(alternatives.intBucket(20), 'tens');
    assert.equal(alternatives.longBucket(200), 'hundreds');
    assert.equal(alternatives.signName(new alternatives.Plus({ sign: '+' })), 'non-zero');

    const guarded = generatedModule('dev/capylang/test/MatchWhen.js');
    assert.equal(guarded.foo(new capy.Some({ value: '-' })), 'minus');
    assert.equal(guarded.foo(capy.None), 'none');
    assert.equal(guarded.boo(new capy.Some({ value: 2 })), 'two');
    assert.equal(guarded.nestedGuard(new capy.Some({ value: 11 })), 'big');
    assert.equal(guarded.sign(new guarded.Sign({ value: '-', weight: 20, enabled: true })), 'heavy minus');
    assert.equal(guarded.letter(guarded.Vowel), 'vowel');
    assert.equal(guarded.rangeLabel(new guarded.Range({ start: 1, stop: 5, step: 1, enabled: true })), 'ascending');

    const grouped = generatedModule('dev/capylang/test/GroupedExpression.js');
    assert.equal(assertSuccess(grouped.nestedPipeInMatchBranch(1)), 3);
    assert.equal(assertSuccess(grouped.groupedGuardedMatchIsExhaustive('-+abc')), 'plus:pre:tail:build');
    assert.equal(assertError(grouped.groupedGuardedMatchIsExhaustive('-abc')), 'bad:a');
    assert.equal(assertSuccess(grouped.semverStyleMixedCasePipeBodies('+meta')), '1.2.3+001');
    assert.equal(assertError(grouped.nestedResultErrorMatchCase('')), 'empty');

    const localFunctions = generatedModule('dev/capylang/test/LocalFunctions.js');
    assert.equal(localFunctions.sumDown(5), 15);
    assert.equal(localFunctions.parity(15), false);
    assert.equal(localFunctions.localWithLet(10), 11);
    assert.equal(localFunctions.maxLocal(2, 5), 5);

    const localTypes = generatedModule('dev/capylang/test/LocalTypesAndData.js');
    assert.equal(localTypes.fooMe('foo'), 'xyz');
    assert.equal(localTypes.localDataWithImportedResult(7), 'ok:7');
    assert.equal(localTypes.localSingleUsedInLocalTypeAndFunction(true), 'stop');
    assert.equal(localTypes.localGenericDataFieldAccessFollowedByIndex('abc'), 'a');
    assert.equal(localTypes.localGenericOptionConstructorPatternBinding(7), 77);

    const overload = generatedModule('dev/capylang/test/OverloadResolution.js');
    assert.equal(overload.chooseInts(), 103);
    assert.equal(overload.chooseLongs(), 203);

    const collision = generatedModule('dev/capylang/test/OperatorMethodCollision.js');
    const counter = new collision.Counter({ value: 10 });
    assert.equal(collision.namedPlus(counter, 5), 15);
    assert.equal(collision.operatorPlus(counter, 5), 115);

    const withModule = generatedModule('dev/capylang/test/With.js');
    assert.equal(withModule.updateFoo(new withModule.Foo({ a: 1, b: 'old', c: 1.5 })).b, 'new value');
    assert.equal(withModule.chainLetter(new withModule.B({ x: 7, b: 123 })).x, 99);
    assert.equal(withModule.updateA(new withModule.A({ x: 5, a: 'abc' })).a, 'abc!');
    assert.equal(withModule.updateSanitizedCounter(withModule.makeSanitizedCounter(3)).constructor_runs, 2);
    assert.equal(withModule.validatedCounterAfterSingleWith(-1), 'err:negative:-1');

    const withExpression = generatedModule('dev/capylang/test/WithExpression.js');
    assert.equal(withExpression.dataWithMultiple(new withExpression.Foo({ a: 1, b: 'old', c: 2.5 })).b, 'new value');
    assert.equal(withExpression.parentWith(new withExpression.A({ x: 1, a: 'alpha' })).x, 2);
    assert.equal(withExpression.parentWithPreservesSubtype(new withExpression.C({ x: 3, c: 4.5 })), 'C:4:4.5');
});

test('translates pipes, empty literal inference, options, and results', () => {
    const empty = generatedModule('dev/capylang/test/EmptyLiteralInference.js');
    assertCapyEqual(empty.reverseInts([1, 2, 3]), [3, 2, 1]);
    assertCapyEqual(empty.duplicateStrings(['a', 'b']), ['a', 'a', 'b', 'b']);
    assert.equal(empty.reduceDictToNamedValuesSize(new Map([['a', 1], ['b', 2]])), 2);
    assertCapyEqual(assertSuccess(empty.reduceListToSuccessValues([1, 2, 3])), [1, 2, 3]);
    assertCapyEqual(assertSuccess(empty.reduceSetToSuccessValues(new Set(['a', 'b']))), new Set(['a', 'b']));
    assert.equal(empty.reduceListToBoxValues([1, 2, 3]).value.length, 3);
    assert.equal(empty.buildTestFileFromThreeInferredTestCaseLists(), 3);

    const pipe = generatedModule('dev/capylang/test/Pipe.js');
    assert.deepEqual(pipe.map1([1, 2, 3]).asList(), [2, 4, 6]);
    assert.deepEqual(pipe.filter1([-1, 0, 1, 2]).asList(), [1, 2]);
    assert.equal(pipe.reduce1([1, 2, 3, 4]), 10);
    assert.equal(pipe.reduce2([-1, 1, 2, 3]), -2);
    assert.equal(pipe.reduceString2(['a', 'b', 'c']), 'start: a b c');
    assert.deepEqual(pipe.stringMap('abc').asList(), ['[a]', '[b]', '[c]']);
    assert.deepEqual(pipe.flatMap1([1, 2]).asList(), [1, 2, 3, 2, 3, 4]);
    assert.equal(pipe.pipeThenChainAfterLambda(), 3);
    assert.equal(pipe.pipeThenChainMethodsAfterLambda(), 'ok');

    const named = generatedModule('dev/capylang/test/NamedPipeMethods.js');
    assert.deepEqual(named.seqMapNamed([1, 2, 3]), [3, 6, 9]);
    assert.equal(named.seqReduceNamed([1, 2, 3]), 6);
    assert.equal(named.seqReduceLeftNamed([1, 2, 3]), 6);
    assert.equal(named.resultReduceNamed(new capy.Success({ value: 3 })), 'ok:3');
    assert.equal(named.resultReduceNamed(new capy.Error({ message: 'boom' })), 'err:boom');
    assert.equal(named.resultReduceOperator(new capy.Success({ value: 3 })), 'ok:3');

    const constructorAssert = generatedModule('dev/capylang/test/ConstructorResultAssert.js');
    assert.equal(constructorAssert.validDateAssertSucceeds(), true);
    assert.equal(constructorAssert.invalidDateAssertFails(), true);
});

test('translates higher-order functions, lambdas, recursion, options, and custom results', () => {
    const higher = generatedModule('dev/capylang/test/HigherOrderFunctions.js');
    assert.deepEqual(higher.map([1, 2, 3]).asList(), [1, 4, 9]);
    assert.equal(higher.hof(x => `n=${x}`), 'n=5');
    assert.equal(higher.invokeHof('Hello'), 'Hello 5');
    assert.equal(higher.runLambda(), -45);
    assert.equal(higher.runLambda2(), -45);
    assert.equal(higher.invokeHofUnused('Hello'), 'Hello');
    assert.equal(higher.runLambdaUnusedMiddle(), 12);
    assert.equal(higher.runLambdaNoArgs(), 'Hello, Wrold');

    const lambda = generatedModule('dev/capylang/test/LambdaValues.js');
    assert.equal(lambda.makeSupplier()(), 'hello');
    assert.equal(lambda.invokeSupplier(), 'hello');
    assert.equal(lambda.invokeFieldSupplier(), 'hello');
    assert.equal(lambda.add10()(5), 15);
    assert.equal(lambda.invokePartialAdd(), 15);
    assert.equal(lambda.invokePartialAddWithGeneratedNameCollision(), 15);
    assert.equal(lambda.invokePartialConcat(), 'A-B');
    assert.equal(lambda.invokePartialCombine(), 159);
    assert.equal(lambda.invokePartialPair(), '1:2');
    assert.equal(lambda.isAgeValid()(42), true);
    assert.equal(lambda.isAgeValid()(-1), false);
    assert.equal(lambda.invokePartialContainsMethod(), true);
    assert.equal(lambda.invokePartialReplaceMethod(), 'bonono');

    const resultBind = generatedModule('dev/capylang/test/ResultBind.js');
    const users = assertSuccess(resultBind.users('Ann', 'Bob', 'Cid'));
    assert.equal(users.length, 3);
    assert.equal(users[0].name, 'Ann');
    assert.equal(users[2].name, 'Cid');
    assert.equal(resultBind.firstErrorMessage('', 'Bob'), 'You need to pass name');

    const resultError = generatedModule('dev/capylang/test/ResultError.js');
    assert.equal(assertSuccess(resultError.successResult()), 7);
    assert.equal(assertError(resultError.failResult()), 'boom');
    assert.equal(assertSuccess(resultError.parseAndAddRestSize('10')), 13);
    assert.match(assertError(resultError.parseAndAddRestSize('abc')), /Cannot parse string to long/);
    assert.equal(assertSuccess(resultError.mapUntypedResult()), 'mapped_success_value');
    assert.match(assertSuccess(resultError.dictReduceThenMap(new Map([['a', 1], ['b', 2]]))), /done$/);

    const infixResult = generatedModule('dev/capylang/test/ResultInfixGeneric.js');
    assert.equal(infixResult.applyMapper(new infixResult.Ok({ value: 7 }), infixResult.mapToString).value, 'v=7');
    assert.equal(infixResult.applyMapper(new infixResult.Ok({ value: 3 }), infixResult.mapToFail).message, 'bad:3');
    assert.equal(infixResult.propagateFail(new infixResult.Fail({ message: 'boom' }), infixResult.mapToString).message, 'boom');
    assert.equal(infixResult.applyFlatMapper(new infixResult.Ok({ value: 7 }), infixResult.mapToNestedOk).value, 'mapped_7');
    assert.equal(infixResult.flattenNestedResult(infixResult.wrapNestedFail(5)).message, 'wrapped_bad:5');

    const optionInterop = generatedModule('dev/capylang/test/OptionInterop.js');
    assertSome(optionInterop.someValue(7), 7);
    assertNone(optionInterop.noneValue());

    const option = generatedModule('dev/capylang/test/OptionToOptional.js');
    assert.equal(option.stringOption(new capy.Some({ value: 'abc' })), 'abc');
    assert.equal(option.stringOption(capy.None), '<empty>');
    assert.equal(option.boolOption(new capy.Some({ value: true })), true);
    assert.equal(option.optionToMessage(capy.None), 'missing');
    assert.equal(option.intOptionToMessage(new capy.Some({ value: 10 })), 'value=10');
    assert.equal(option.matchDeserialize('bad'), 'failed: bad');
    assert.equal(option.matchDeserialize('ok'), 'prefix:json:7');

    const rec = generatedModule('dev/capylang/test/RecFunctions.js');
    assert.equal(rec.sum(5, 0), 15);
    assert.equal(rec.sumInner(5), 15);
    assert.equal(rec.sumWithLet(5, 0), 15);
    assert.equal(rec.factorialLarge(5, 1), 120);
    assert.equal(rec.unmarkedTailSum(5, 0), 15);
    assert.equal(rec.unmarkedLocalTailFactorialLarge(5), 120);
    assert.equal(rec.unmarkedNonTailSum(5), 15);
    assert.equal(rec.sumPresentValues([1, 2, 3], 0), 6);
    assert.equal(rec.sumLengthsUntilError(['a', 'bb', 'bad', 'cccc'], 0), 'error:bad length:3');
    assert.equal(rec.sumLengthsUntilError(['a', 'bb', 'ccc'], 0), 'ok:6');

    const seq = generatedModule('dev/capylang/test/SeqCollection.js');
    const oneTwoThree = seq.oneTwoThree();
    assert.equal(oneTwoThree.value, 1);
    assert.equal(seq.headOrDefault(oneTwoThree, -1), 1);
    assert.equal(seq.headOrDefaultWildcard(seq.End, -1), -1);
    assertSome(seq.firstFqSingleton(oneTwoThree), 1);
    assertNone(seq.firstShortSingleton(seq.End));
    assert.equal(seq.headOrDefault(seq.tail(oneTwoThree), -1), 2);
    assert.equal(seq.sumFirstTwo(oneTwoThree), 3);
    assert.equal(seq.headOrDefault(seq.dropLocal(oneTwoThree, 1), -1), 2);
    assert.equal(seq.headOrDefault(seq.tail(seq.untilLocal(oneTwoThree, 2)), -1), -1);
});

test('translates JS runtime-backed regex, date time, effects, and file IO', () => {
    const regex = generatedModule('dev/capylang/test/RegexCollection.js');
    assert.equal(regex.matchesNamed('xxfooyy'), true);
    assert.equal(regex.matchesAlias('xxbaryy'), false);
    assert.equal(regex.findNamed('xxfooyy'), true);
    assert.equal(regex.findAllNamedCount('xxfooyy'), 1);
    assert.equal(regex.replaceNamed('a1b11'), 'a#b##');
    assert.deepEqual(regex.splitNamed('a,b,c'), ['a', 'b', 'c']);
    assert.equal(regex.escapedSlashMatch(), true);

    const dateTime = generatedModule('dev/capylang/test/DateTimeDifference.js');
    assert.equal(dateTime.forwardDifferenceIso(), 'P1DT1H1M1S');
    assert.equal(dateTime.backwardDifferenceIso(), 'P-1DT-1H-1M-1S');
    assert.equal(dateTime.differenceRoundTrips(), true);

    const effects = generatedModule('dev/capylang/test/EffectBind.js');
    assert.equal(unsafe(effects.addEffects(2, 3)), 5);
    assert.equal(unsafe(effects.nestedEffect(7)), 'v=7');
    assert.equal(effects.constructDelayedDivideByZero(), true);
    assert.match(unsafe(effects.clockNowIso()), /T/);
    assert.equal(typeof unsafe(effects.currentMillisValue()), 'number');
    assert.equal(typeof unsafe(effects.nanoTimeValue()), 'number');

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

test('translates reflection, extensions, indexing, interpolation, and entrypoints', () => {
    const reflection = generatedModule('dev/capylang/test/ReflectionFeature.js');
    const reflectedA = reflection.reflectionDataA(new reflection.A({ name: 'letter-a', a: 1 }));
    assert.equal(reflectedA.name, 'A');
    assert.deepEqual(reflectedA.fields.map(field => field.name), ['name', 'a']);
    assert.deepEqual(reflectedA.fields.map(field => field.value), ['letter-a', 1]);
    const reflectedB = reflection.reflectionDataB(new reflection.B({ name: 'letter-b', b: ['red', 'blue'] }));
    assert.deepEqual(reflectedB.fields.map(field => field.value), ['letter-b', ['red', 'blue']]);
    const reflectedEmpty = reflection.reflectionDataEmpty();
    assert.equal(reflectedEmpty.name, 'ReflectedEmpty');
    assert.equal(reflectedEmpty.packagePath, 'dev/capylang/test/ReflectionFeature');
    assert.deepEqual(reflectedEmpty.fields, []);
    const employee = reflection.reflectionDataEmployee(new reflection.ReflectedEmployee({ id: 'E-1', department: 'R&D', active: true }));
    assert.deepEqual(employee.fields.map(field => field.name), ['id', 'department', 'active']);
    assert.deepEqual(employee.fields.map(field => field.value), ['E-1', 'R&D', true]);
    const json = reflection.reflectionJsonShape(new reflection.ReflectedEnvelope({
        label: 'outer',
        payload: new reflection.A({ name: 'inner', a: 7 }),
    }));
    assert.equal(json.value.get('label').value, 'outer');
    assert.equal(json.value.get('payload').value.get('name').value, 'inner');
    assert.equal(json.value.get('payload').value.get('a').value, 7);

    const extension = generatedModule('dev/capylang/test/Extension.js');
    assert.equal(extension.newPoint3d__float__float__float(1.0, 2.0, 3.0).z, 3.0);
    assert.equal(extension.newPoint3dVer2(4.0, 5.0, 6.0).z, 6.0);
    assert.equal(extension.length2d(new extension.Point({ x: 3.0, y: 4.0 })), 5.0);
    assert.equal(extension.onlyPoint2d(), 3.0);
    assert.equal(extension.testSubstitution(new extension.Point3D({ x: 3.0, y: 4.0, z: 100.0 })), 5.0);
    const weighted = new extension.WeightedLabeledPoint3D({ x: 1.0, y: 2.0, z: 3.0, label: 'capy', weight: 4.0, tag: 'v1' });
    assert.equal(extension.weightedLabeledPointScore(weighted), 10.0);
    assert.equal(extension.weightedLabeledPointLabel(weighted), 'capy:v1');
    assert.equal(extension.overloadAnyPrefersParent(new extension.Point3D({ x: 3.0, y: 4.0, z: 5.0 })), 'point');
    assert.equal(extension.newLabeledPoint(1.0, 2.0, 'origin').label, 'origin');

    const indexing = generatedModule('dev/capylang/test/UserDefinedIndexing.js');
    assert.equal(indexing.intIndex(), 'one');
    assert.equal(indexing.stringIndex(), 42.5);
    assert.equal(indexing.matrixIndexName(), 'second');
    assertSome(indexing.chainedIndex(), 'e');
    assertSome(indexing.listVariableIndex(['a', 'b', 'c'], 1), 'b');
    assertNone(indexing.listVariableIndex(['a'], 3));

    const interpolation = generatedModule('dev/capylang/test/StringInterpolation.js');
    assert.equal(interpolation.simple('Capybara'), 'Hello Capybara');
    assert.equal(interpolation.expression('Capy', 7), 'Hello Capy7');
    assert.equal(interpolation.escaped('ignored'), 'Hello \\{}');
    assert.equal(interpolation.escapedWithInterpolationValue('boo'), 'xyz {boo}');
    assert.equal(interpolation.quoteEscape(new interpolation.Foo({ name: 'Martin' })), 'Hello "Martin"');
    assert.equal(interpolation.dataField(new interpolation.Foo({ name: 'Martin' })), 'Hello Martin');
    assert.equal(interpolation.singleQuoted('Capybara'), 'Hello {name}');

    const nothing = generatedModule('dev/capylang/test/Nothing.js');
    assert.throws(() => nothing.nothing1(1), /nothing1` is not yet implemented/);
    assert.throws(() => nothing.nothing2(1), /nothing2` is not yet implemented/);

    const main = generatedModule('dev/capylang/test/MainProgram.js');
    const program = main.main(['ok']);
    assert.equal(capy.isType(program, 'Program'), true);
    assert.equal(program.results[0], 'ok');
    const mainRun = spawnSync('node', [modulePath('dev/capylang/test/MainProgram.js'), 'ok'], { encoding: 'utf8' });
    assert.equal(mainRun.status, 0, mainRun.stderr);
    assert.equal(mainRun.stdout.trim(), 'ok');

    const effectMain = generatedModule('dev/capylang/test/EffectMainProgram.js');
    const effectProgram = unsafe(effectMain.main(['one', 'two']));
    assert.equal(capy.isType(effectProgram, 'Program'), true);
    assert.deepEqual(effectProgram.results, ['effect-main:2']);
    const effectRun = spawnSync('node', [modulePath('dev/capylang/test/EffectMainProgram.js'), 'one', 'two'], { encoding: 'utf8' });
    assert.equal(effectRun.status, 0, effectRun.stderr);
    assert.equal(effectRun.stdout.trim(), 'effect-main:2');

    const consoleIo = generatedModule('dev/capylang/test/ConsoleIo.js');
    const printed = captureOutput(() => {
        assert.equal(unsafe(consoleIo.emitAll()), ' line');
        assert.equal(unsafe(consoleIo.emitPrimitives(7)), false);
        assertCapyEqual(unsafe(consoleIo.emitByteLists([65, 90])), [65, 90]);
    });
    assert.equal(printed.stdout.replace(/\r\n/g, '\n'), 'out line\n7|8|9|1.5|2.5|true\n7\n8\n9\n1.5\n2.5\nfalse\nAZAZ\n');
    assert.equal(printed.stderr.replace(/\r\n/g, '\n'), 'err line\n7|8|9|1.5|2.5|true\n7\n8\n9\n1.5\n2.5\nfalse\nAZAZ\n');

    const readScript = `const m = require(${JSON.stringify(modulePath('dev/capylang/test/ConsoleIo.js'))}); console.log(m.readOne().unsafe_run()); console.log(m.readEofMarker().unsafe_run());`;
    const readRun = spawnSync('node', ['-e', readScript], { input: 'Capy\n', encoding: 'utf8' });
    assert.equal(readRun.status, 0, readRun.stderr);
    assert.deepEqual(readRun.stdout.trim().split(/\r?\n/), ['Capy', 'EOF']);
});
