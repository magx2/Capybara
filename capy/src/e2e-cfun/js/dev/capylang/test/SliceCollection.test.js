const test = require('node:test');
const { assert, assertCapyEqual, generatedModule } = require('./_support');

test('SliceCollection', () => {
    const slice = generatedModule('dev/capylang/test/SliceCollection.js');
    assertCapyEqual(slice.listFrom1([0, 1, 2, 3, 4, 5]), [1, 2, 3, 4, 5]);
    assertCapyEqual(slice.listToMinus3([0, 1, 2, 3, 4, 5]), [0, 1, 2]);
    assertCapyEqual(slice.list1Minus2([0, 1, 2, 3, 4, 5]), [1, 2, 3]);
    assert.equal(slice.stringFrom1('abcdef'), 'bcdef');
    assert.equal(slice.stringToMinus3('abcdef'), 'abc');
    assert.equal(slice.string1Minus2('abcdef'), 'bcd');
});
