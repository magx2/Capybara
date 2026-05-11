const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('RegexCollection', () => {
    const regex = generatedModule('dev/capylang/test/RegexCollection.js');
    assert.equal(regex.matchesNamed('xxfooyy'), true);
    assert.equal(regex.matchesAlias('xxbaryy'), false);
    assert.equal(regex.findNamed('xxfooyy'), true);
    assert.equal(regex.findAllNamedCount('xxfooyy'), 1);
    assert.equal(regex.replaceNamed('a1b11'), 'a#b##');
    assert.deepEqual(regex.splitNamed('a,b,c'), ['a', 'b', 'c']);
    assert.equal(regex.escapedSlashMatch(), true);
});
