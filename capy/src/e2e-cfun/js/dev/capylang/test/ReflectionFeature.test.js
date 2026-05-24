const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('ReflectionFeature', () => {
    const reflection = generatedModule('dev/capylang/test/ReflectionFeature.js');
    const reflectedA = reflection.reflectionDataA(new reflection.A({ name: 'letter-a', a: 1 }));
    assert.equal(reflectedA.name, 'A');
    assert.deepEqual(reflectedA.fields.map(field => field.name), ['name', 'a']);
    assert.deepEqual(reflectedA.fields.map(field => field.value), ['letter-a', 1]);
    const reflectedB = reflection.reflectionDataB(new reflection.B({ name: 'letter-b', b: ['red', 'blue'] }));
    assert.deepEqual(reflectedB.fields.map(field => field.value), ['letter-b', ['red', 'blue']]);
    const reflectedEmpty = reflection.reflectionDataEmpty();
    assert.equal(reflectedEmpty.name, 'ReflectedEmpty');
    assert.equal(reflectedEmpty.packagePath, 'dev/capylang/test/ReflectionFeature');
    assert.deepEqual(reflectedEmpty.fields, []);
    const employee = reflection.reflectionDataEmployee(new reflection.ReflectedEmployee({ id: 'E-1', department: 'R&D', active: true }));
    assert.deepEqual(employee.fields.map(field => field.name), ['id', 'department', 'active']);
    assert.deepEqual(employee.fields.map(field => field.value), ['E-1', 'R&D', true]);
    const singleton = reflection.reflectionData(reflection.ReflectedSingleton);
    assert.equal(singleton.name, 'ReflectedSingleton');
    assert.deepEqual(singleton.annotations.map(annotation => annotation.name), ['ReflectionDataMarker']);
    assert.equal(annotationArgument(singleton.annotations[0], 'label').value.value, 'singleton');
    const json = reflection.reflectionJsonShape(new reflection.ReflectedEnvelope({
        label: 'outer',
        payload: new reflection.A({ name: 'inner', a: 7 }),
    }));
    assert.equal(json.value.get('label').value, 'outer');
    assert.equal(json.value.get('payload').value.get('name').value, 'inner');
    assert.equal(json.value.get('payload').value.get('a').value, 7);
});

test('reflects functional data and field annotations', () => {
    const reflection = generatedModule('dev/capylang/test/ReflectionFeature.js');
    const reflected = reflection.reflectionDataAnnotatedUser(new reflection.ReflectedAnnotatedUser({
        id: 'U-1',
        display: 'Ada',
    }));

    assert.deepEqual(reflected.annotations.map(annotation => annotation.name), ['ReflectionDataMarker']);
    assert.equal(annotationArgument(reflected.annotations[0], 'label').value.value, 'user');
    assert.equal(annotationArgument(reflected.annotations[0], 'label').value.kind, 'string');
    assert.equal(annotationArgument(reflected.annotations[0], 'order').value.value, 7);
    assert.equal(annotationArgument(reflected.annotations[0], 'active').value.value, true);
    assert.equal(reflected.pkg.path, 'dev/capylang/test/ReflectionFeature');

    assert.deepEqual(reflected.fields.map(field => field.annotations[0].name), ['ReflectionFieldMarker', 'ReflectionFieldMarker']);
    assert.equal(annotationArgument(reflected.fields[0].annotations[0], 'value').value.value, 'identifier');
    assert.equal(annotationArgument(reflected.fields[1].annotations[0], 'value').value.value, 'display');
    assert.deepEqual(reflected.fields.map(field => field.type.name), ['String', 'String']);
});

function annotationArgument(annotation, name) {
    return annotation.arguments.find(argument => argument.name === name);
}
