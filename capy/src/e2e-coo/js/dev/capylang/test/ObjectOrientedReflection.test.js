const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('reflects object-oriented interface', () => {
    const { X } = generatedModule('dev/capylang/test/X.js');
    const x = X.type();

    assert.equal(x.name, 'X');
    assert.deepEqual(x.annotations.map(annotation => annotation.name), ['CooTypeMarker']);
    assert.equal(annotationArgument(x.annotations[0], 'label').value.value, 'contract');
    assert.deepEqual(x.methods.map(method => method.name), ['print']);
    assert.deepEqual(x.methods[0].annotations.map(annotation => annotation.name), ['CooMethodMarker']);
    assert.deepEqual(x.parents, []);
});

test('reflects object-oriented trait', () => {
    const { Y } = generatedModule('dev/capylang/test/Y.js');
    const y = Y.type();

    assert.equal(y.name, 'Y');
    assert.deepEqual(y.annotations.map(annotation => annotation.name), ['CooTypeMarker']);
    assert.equal(annotationArgument(y.annotations[0], 'label').value.value, 'mixin');
    assert.deepEqual(y.methods.map(method => method.name), ['bracket']);
    assert.deepEqual(y.methods[0].annotations.map(annotation => annotation.name), ['CooMethodMarker']);
    assert.deepEqual(y.parents, []);
});

test('reflects object-oriented class with parents', () => {
    const { Z } = generatedModule('dev/capylang/test/Z.js');
    const z = Z.type();

    assert.equal(z.name, 'Z');
    assert.equal(z.open, false);
    assert.deepEqual(z.annotations.map(annotation => annotation.name), ['CooTypeMarker']);
    assert.equal(annotationArgument(z.annotations[0], 'label').value.value, 'entity');
    assert.deepEqual(z.fields.map(field => field.name), ['name']);
    assert.deepEqual(z.fields[0].annotations.map(annotation => annotation.name), ['CooFieldMarker']);
    assert.equal(annotationArgument(z.fields[0].annotations[0], 'value').value.value, 'display_name');
    assert.deepEqual(z.methods.map(method => method.name).sort(), ['greet', 'print']);
    assert.deepEqual(z.methods.map(method => method.annotations[0].name).sort(), ['CooMethodMarker', 'CooMethodMarker']);
    assert.deepEqual(z.parents.map(parent => parent.name).sort(), ['X', 'Y']);
});

function annotationArgument(annotation, name) {
    return annotation.arguments.find(argument => argument.name === name);
}
