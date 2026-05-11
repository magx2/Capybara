const test = require('node:test');
const { assert, assertCapyEqual, capy, generatedModule } = require('./_support');

test('MatchByType', () => {
    const match = generatedModule('dev/capylang/test/MatchByType.js');
    assert.equal(match.matchString('abc'), 's:abc');
    assert.equal(match.matchString(12), 'i:12');
    assert.equal(match.matchInt(9), 10);
    assert.equal(match.matchIntIgnoreBinding('x'), 0);
    assert.equal(match.isData(new match.Person({ name: 'Tom' })), true);
    assert.equal(match.isData(match.READY), true);
    assert.equal(match.classifyEnum(match.READY), 'READY');
    assert.equal(match.classifyEnum(new match.Person({ name: 'Tom' })), 'data');
    assert.equal(match.genericEnumName(match.DONE), 'DONE');
    assertCapyEqual(match.mergedEnumListNames(match.DONE), ['DONE', 'READY']);
    assert.equal(match.classifyJsonChar('{'), 'object');
    assert.equal(match.classifyJsonChar('"'), 'string');
    assert.equal(match.classifyJsonChar('7'), 'number');
    assert.equal(match.classifyJsonCharWithNamedWildcard('x'), 'unknown: x');
    assert.equal(match.mood(false), "I'm sad");
    assert.equal(match.bareGenericTypePattern(new Map([['a', 1]])), 3);
    assert.equal(match.bareGenericTypePattern(new capy.Some({ value: 'a' })), 4);
});
