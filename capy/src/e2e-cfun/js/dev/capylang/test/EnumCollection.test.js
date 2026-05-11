const test = require('node:test');
const { assert, assertCapyEqual, assertError, assertSuccess, generatedModule } = require('./_support');

test('EnumCollection', () => {
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
});
