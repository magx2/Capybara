const test = require('node:test');
const { assert, assertCapyEqual, generatedModule } = require('../_support');

test('ListCollection', () => {
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
});
