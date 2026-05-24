const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('reflects object-oriented interface and trait annotations', () => {
    const { AnnotatedContract } = generatedModule('dev/capylang/test/AnnotatedContract.js');
    const { AnnotatedNaming } = generatedModule('dev/capylang/test/AnnotatedNaming.js');
    const contract = AnnotatedContract.type();
    const naming = AnnotatedNaming.type();

    assert.deepEqual(contract.annotations.map(annotation => annotation.name), ['OoAnnotationMarker', 'OoTypeLabel', 'Deprecated']);
    assert.deepEqual(contract.annotations[0].arguments, []);
    assert.equal(contract.annotations[0].pkg.name, 'ObjectOrientedAnnotationDefinitions');
    assert.equal(annotationArgument(contract.annotations[1], 'label').value.value, 'contract');
    assert.equal(annotationArgument(contract.annotations[2], 'message').value.value, 'use AnnotatedContractV2');
    assert.equal(annotationArgument(contract.annotations[2], 'since').value.value, '');
    assert.deepEqual(contract.methods.map(method => method.name), ['render']);
    assert.deepEqual(contract.methods[0].annotations.map(annotation => annotation.name), ['OoMethodName']);

    assert.deepEqual(naming.annotations.map(annotation => annotation.name), ['OoAnnotationMarker', 'OoTypeLabel', 'Deprecated']);
    assert.equal(annotationArgument(naming.annotations[1], 'label').value.value, 'mixin');
    assert.equal(annotationArgument(naming.annotations[2], 'message').value.value, 'use AnnotatedNamingV2');
    assert.equal(annotationArgument(naming.annotations[2], 'since').value.value, '3.0');
    assert.deepEqual(naming.methods.map(method => method.name), ['prefix']);
});

test('reflects object-oriented class field and method annotations', () => {
    const { AnnotatedWidget } = generatedModule('dev/capylang/test/AnnotatedWidget.js');
    const widget = AnnotatedWidget.type();

    assert.deepEqual(widget.annotations.map(annotation => annotation.name), ['OoAnnotationMarker', 'OoTypeLabel', 'Deprecated']);
    assert.equal(annotationArgument(widget.annotations[1], 'label').value.value, 'entity');
    assert.equal(widget.annotations[2].pkg.name, 'Annotations');
    assert.equal(widget.annotations[2].pkg.path, 'capy/meta_prog');
    assert.equal(annotationArgument(widget.annotations[2], 'message').value.value, 'use AnnotatedView');
    assert.equal(annotationArgument(widget.annotations[2], 'since').value.value, '3.0');
    assert.deepEqual(widget.parents.map(parent => parent.name).sort(), ['AnnotatedContract', 'AnnotatedNaming']);
    assert.deepEqual(widget.fields.map(field => field.name), ['name']);
    assert.deepEqual(widget.fields[0].annotations.map(annotation => annotation.name), ['OoAnnotationMarker', 'OoFieldName', 'Deprecated']);
    assert.deepEqual(widget.fields[0].annotations[0].arguments, []);
    assert.equal(annotationArgument(widget.fields[0].annotations[1], 'value').value.value, 'display_name');
    assert.equal(annotationArgument(widget.fields[0].annotations[2], 'message').value.value, 'use label');
    assert.equal(annotationArgument(widget.fields[0].annotations[2], 'since').value.value, '');
    assert.deepEqual(widget.methods.map(method => method.name), ['render']);
    assert.deepEqual(widget.methods[0].annotations.map(annotation => annotation.name), ['OoMethodName', 'Deprecated']);
    assert.equal(annotationArgument(widget.methods[0].annotations[0], 'value').value.value, 'render');
    assert.equal(annotationArgument(widget.methods[0].annotations[1], 'message').value.value, 'use render_label');
    assert.equal(annotationArgument(widget.methods[0].annotations[1], 'since').value.value, '3.1');
});

function annotationArgument(annotation, name) {
    return annotation.arguments.find(argument => argument.name === name);
}
