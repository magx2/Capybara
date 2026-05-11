const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('WithExpression', () => {
    const withExpression = generatedModule('dev/capylang/test/WithExpression.js');
    assert.equal(withExpression.dataWithMultiple(new withExpression.Foo({ a: 1, b: 'old', c: 2.5 })).b, 'new value');
    assert.equal(withExpression.parentWith(new withExpression.A({ x: 1, a: 'alpha' })).x, 2);
    assert.equal(withExpression.parentWithPreservesSubtype(new withExpression.C({ x: 3, c: 4.5 })), 'C:4:4.5');
});
