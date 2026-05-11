const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('reflects object-oriented interface', () => {
    const { X } = generatedModule('dev/capylang/test/X.js');
    const x = X.type();

    assert.equal(x.name, 'X');
    assert.deepEqual(x.methods.map(method => method.name), ['print']);
    assert.deepEqual(x.parents, []);
});

test('reflects object-oriented trait', () => {
    const { Y } = generatedModule('dev/capylang/test/Y.js');
    const y = Y.type();

    assert.equal(y.name, 'Y');
    assert.deepEqual(y.methods.map(method => method.name), ['bracket']);
    assert.deepEqual(y.parents, []);
});

test('reflects object-oriented class with parents', () => {
    const { Z } = generatedModule('dev/capylang/test/Z.js');
    const z = Z.type();

    assert.equal(z.name, 'Z');
    assert.equal(z.open, false);
    assert.deepEqual(z.fields.map(field => field.name), ['name']);
    assert.deepEqual(z.methods.map(method => method.name).sort(), ['greet', 'print']);
    assert.deepEqual(z.parents.map(parent => parent.name).sort(), ['X', 'Y']);
});
