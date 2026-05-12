const test = require('node:test');
const { assert, assertCapyEqual, generatedModule } = require('../_support');

test('SetCollection', () => {
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
    assert.equal(set.containsStructuralDataOperator(new Set([new set.User({ name: 'Ada' })])), true);
    assert.equal(set.containsStructuralDataMethod(new Set([new set.User({ name: 'Ada' })])), true);
    assert.equal(set.containsStructuralTuple(new Set([[1, 'one']])), true);
    assert.equal(set.structuralLiteralSize(), 1);
    assert.equal(set.structuralAppendSize(new Set([new set.User({ name: 'Ada' })])), 1);
    assert.equal(
        set.structuralUnionSize(
            new Set([new set.User({ name: 'Ada' })]),
            new Set([new set.User({ name: 'Ada' })]),
        ),
        1,
    );
    assert.equal(set.structuralMappedValues().size, 1);
    assert.equal(
        set.structuralProperSubsetAfterUnion(
            new Set([new set.User({ name: 'Ada' })]),
            new Set([new set.User({ name: 'Ada' })]),
        ),
        false,
    );
    assert.equal(
        set.structuralPowerSetSize(new Set([new set.User({ name: 'Ada' }), new set.User({ name: 'Ada' })])),
        2,
    );
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
});
