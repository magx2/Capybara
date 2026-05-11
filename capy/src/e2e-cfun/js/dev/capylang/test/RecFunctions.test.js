const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('RecFunctions', () => {
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
});
