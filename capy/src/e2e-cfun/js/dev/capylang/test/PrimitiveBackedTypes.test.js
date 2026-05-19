const test = require('node:test');
const { assert, assertError, assertSuccess, fs, generatedModule, modulePath } = require('./_support');

test('PrimitiveBackedTypes', () => {
    const primitiveBackedTypes = generatedModule('dev/capylang/test/PrimitiveBackedTypes.js');
    assert.equal(primitiveBackedTypes.rawUserId(7), 7);
    assert.equal(primitiveBackedTypes.unwrapUserId(7), 7);
    assert.equal(primitiveBackedTypes.unwrapRawUserId(7), 7);
    assert.equal(primitiveBackedTypes.plusUserIds(2, 3), 5);
    assert.equal(primitiveBackedTypes.addUserIds(4, 5), 9);
    assert.equal(primitiveBackedTypes.incrementUserId(6), 7);
    assert.equal(primitiveBackedTypes.stringAt('abc', 1), 'b');
    assert.equal(primitiveBackedTypes.stringGet('abc', 2), 'c');
    assert.equal(primitiveBackedTypes.unwrapScore(primitiveBackedTypes.scoreOf(11)), 11);
    assert.equal(assertSuccess(primitiveBackedTypes.newUserId(7)), 7);
    assert.equal(assertError(primitiveBackedTypes.newUserId(0)), 'bad user id');
    assert.deepEqual(primitiveBackedTypes.__capybaraPrimitiveTypes.user_id, {
        cfunType: '/dev/capylang/test/PrimitiveBackedTypes.user_id',
        backingType: 'int',
    });
    assert.deepEqual(primitiveBackedTypes.__capybaraPrimitiveTypes.score, {
        cfunType: '/dev/capylang/test/PrimitiveBackedTypes.score',
        backingType: 'int',
    });

    const generated = fs.readFileSync(modulePath('dev/capylang/test/PrimitiveBackedTypes.js'), 'utf8');
    assert.match(generated, /function addUserIds\(left, right\) \{\s+return plus__op_plus__user_id__user_id\(left, right\);/);
});
