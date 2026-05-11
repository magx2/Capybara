const test = require('node:test');
const { assert, assertCapyEqual, generatedModule } = require('../_support');

test('DictCollection', () => {
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
});
