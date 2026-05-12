const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('MatchCaseAlternatives', () => {
    const alternatives = generatedModule('dev/capylang/test/MatchCaseAlternatives.js');
    assert.equal(alternatives.digitLabel('1'), 'digit');
    assert.equal(alternatives.digitLabel('0'), 'zero');
    assert.equal(alternatives.intBucket(20), 'tens');
    assert.equal(alternatives.longBucket(200n), 'hundreds');
    assert.equal(alternatives.signName(new alternatives.Plus({ sign: '+' })), 'non-zero');
});
