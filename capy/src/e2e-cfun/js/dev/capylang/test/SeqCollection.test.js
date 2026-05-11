const test = require('node:test');
const { assert, assertNone, assertSome, generatedModule } = require('./_support');

test('SeqCollection', () => {
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
