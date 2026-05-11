const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('With', () => {
    const withModule = generatedModule('dev/capylang/test/With.js');
    assert.equal(withModule.updateFoo(new withModule.Foo({ a: 1, b: 'old', c: 1.5 })).b, 'new value');
    assert.equal(withModule.chainLetter(new withModule.B({ x: 7, b: 123 })).x, 99);
    assert.equal(withModule.updateA(new withModule.A({ x: 5, a: 'abc' })).a, 'abc!');
    assert.equal(withModule.updateSanitizedCounter(withModule.makeSanitizedCounter(3)).constructor_runs, 2);
    assert.equal(withModule.validatedCounterAfterSingleWith(-1), 'err:negative:-1');
});
