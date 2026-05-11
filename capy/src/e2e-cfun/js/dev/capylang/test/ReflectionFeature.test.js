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
    const json = reflection.reflectionJsonShape(new reflection.ReflectedEnvelope({
        label: 'outer',
        payload: new reflection.A({ name: 'inner', a: 7 }),
    }));
    assert.equal(json.value.get('label').value, 'outer');
    assert.equal(json.value.get('payload').value.get('name').value, 'inner');
    assert.equal(json.value.get('payload').value.get('a').value, 7);
});
