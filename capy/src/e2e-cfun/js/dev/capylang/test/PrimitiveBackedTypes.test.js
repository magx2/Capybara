const test = require('node:test');
const { assert, assertError, assertSuccess, fs, generatedModule, modulePath } = require('./_support');

test('PrimitiveBackedTypes', () => {
    const primitiveBackedTypes = generatedModule('dev/capylang/test/PrimitiveBackedTypes.js');
    assert.equal(primitiveBackedTypes.rawUserId(7), 7);
    assert.equal(primitiveBackedTypes.unwrapUserId(7), 7);
    assert.equal(primitiveBackedTypes.unwrapRawUserId(7), 7);
    assert.equal(primitiveBackedTypes.plusUserIds(2, 3), 5);
    assert.equal(primitiveBackedTypes.addUserIds(4, 5), 9);
    assert.equal(primitiveBackedTypes.unwrapScore(primitiveBackedTypes.scoreOf(11)), 11);
    assert.equal(assertSuccess(primitiveBackedTypes.newUserId(7)), 7);
    assert.equal(assertError(primitiveBackedTypes.newUserId(0)), 'bad user id');

    const generated = fs.readFileSync(modulePath('dev/capylang/test/PrimitiveBackedTypes.js'), 'utf8');
    assert.match(generated, /function addUserIds\(left, right\) \{\s+return plus__op_plus__user_id__user_id\(left, right\);/);
});
