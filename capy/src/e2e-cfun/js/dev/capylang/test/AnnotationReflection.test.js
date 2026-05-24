const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('reflects functional declaration annotations', () => {
    const annotations = generatedModule('dev/capylang/test/AnnotationReflection.js');
    const info = annotations.annotationReflectionSample();

    assert.equal(info.name, 'AnnotatedCustomer');
    assert.deepEqual(info.annotations.map(annotation => annotation.name), [
        'ImportedAnnotationMarker',
        'LocalDataLabel',
        'ImportedDataLabel',
    ]);
    assert.deepEqual(info.annotations[0].arguments, []);
    assert.equal(info.annotations[0].pkg.name, 'AnnotationReflectionDefinitions');
    assert.equal(annotationArgument(info.annotations[1], 'label').value.value, 'local');
    assert.equal(annotationArgument(info.annotations[2], 'label').value.value, 'imported');
    assert.equal(annotationArgument(info.annotations[2], 'order').value.value, 5);
    assert.equal(annotationArgument(info.annotations[2], 'active').value.value, true);
});

test('reflects functional field annotations', () => {
    const annotations = generatedModule('dev/capylang/test/AnnotationReflection.js');
    const info = annotations.annotationReflectionData(new annotations.AnnotatedCustomer({
        id: 'C-2',
        display: 'Bea',
    }));

    assert.deepEqual(info.fields.map(field => field.name), ['id', 'display']);
    assert.deepEqual(info.fields[0].annotations.map(annotation => annotation.name), [
        'ImportedAnnotationMarker',
        'ImportedFieldName',
    ]);
    assert.deepEqual(info.fields[0].annotations[0].arguments, []);
    assert.equal(annotationArgument(info.fields[0].annotations[1], 'value').value.value, 'customer_id');
    assert.deepEqual(info.fields[1].annotations.map(annotation => annotation.name), ['LocalFieldMarker']);
    assert.equal(info.fields[1].value, 'Bea');
});

test('reflects annotations imported with qualified import syntax', () => {
    const annotations = generatedModule('dev/capylang/test/AnnotationQualifiedImportReflection.js');
    const info = annotations.qualifiedImportAnnotationReflectionSample();

    assert.equal(info.name, 'QualifiedImportCustomer');
    assert.deepEqual(info.annotations.map(annotation => annotation.name), ['ImportedDataLabel']);
    assert.equal(info.annotations[0].pkg.name, 'AnnotationReflectionDefinitions');
    assert.equal(annotationArgument(info.annotations[0], 'label').value.value, 'qualified-import');
    assert.equal(annotationArgument(info.annotations[0], 'order').value.value, 11);
    assert.equal(annotationArgument(info.annotations[0], 'active').value.value, true);
    assert.deepEqual(info.fields[0].annotations.map(annotation => annotation.name), ['ImportedFieldName']);
    assert.equal(annotationArgument(info.fields[0].annotations[0], 'value').value.value, 'qualified_id');
});

test('reflects standard Deprecated annotation', () => {
    const annotations = generatedModule('dev/capylang/test/DeprecatedAnnotationReflection.js');
    const info = annotations.deprecatedAnnotationReflectionSample();

    assert.equal(info.name, 'DeprecatedCustomer');
    assert.deepEqual(info.annotations.map(annotation => annotation.name), ['Deprecated']);
    assert.equal(info.annotations[0].pkg.name, 'Annotations');
    assert.equal(info.annotations[0].pkg.path, 'capy/meta_prog');
    assert.equal(annotationArgument(info.annotations[0], 'message').value.value, 'use CurrentCustomer');
    assert.equal(annotationArgument(info.annotations[0], 'since').value.value, '2.0');

    assert.deepEqual(info.fields[0].annotations.map(annotation => annotation.name), ['Deprecated']);
    assert.equal(annotationArgument(info.fields[0].annotations[0], 'message').value.value, 'use display_name');
    assert.equal(annotationArgument(info.fields[0].annotations[0], 'since').value.value, '');
});

test('reflects standard Deprecated annotation on enum values', () => {
    const annotations = generatedModule('dev/capylang/test/DeprecatedAnnotationReflection.js');
    const info = annotations.deprecatedEnumAnnotationReflectionSample();

    assert.equal(info.name, 'DEPRECATED_READY');
    assert.deepEqual(info.fields, []);
    assert.deepEqual(info.annotations.map(annotation => annotation.name), ['Deprecated']);
    assert.equal(info.annotations[0].pkg.name, 'Annotations');
    assert.equal(annotationArgument(info.annotations[0], 'message').value.value, 'use CurrentStatus');
    assert.equal(annotationArgument(info.annotations[0], 'since').value.value, '2.2');
});

function annotationArgument(annotation, name) {
    return annotation.arguments.find(argument => argument.name === name);
}
