const test = require('node:test');
const { assert, assertError, assertSuccess, generatedModule } = require('./_support');

test('StringCollection', () => {
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
    assert.equal(assertSuccess(strings.toLongMethod('12345678901')), 12345678901n);
    assert.equal(assertSuccess(strings.toLongMethod('9223372036854775807')), 9223372036854775807n);
    assert.match(assertError(strings.toLongMethod('9223372036854775808')), /Cannot parse string to long/);
    assert.equal(assertSuccess(strings.toDoubleMethod('12.5')), 12.5);
    assert.equal(assertSuccess(strings.toFloatMethod('1.25')), 1.25);
    assert.equal(assertSuccess(strings.toBoolMethod('false')), false);
    assert.match(assertError(strings.toBoolMethod('abc')), /Cannot parse string to bool: abc/);
});
