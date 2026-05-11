const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('DeriveFeature', () => {
    const derive = generatedModule('dev/capylang/test/DeriveFeature.js');
    assert.equal(derive.showUser(), 'DerivedUser { name, age }');
    assert.equal(derive.showEmpty(), 'Empty {  }');
    assert.equal(derive.showEmployee(), 'Employee { id, department }');
    assert.equal(derive.showNamed(), 'Named { id }');
    assert.equal(derive.userBiggerThan(40), true);
    assert.equal(derive.userMixedParameterDeriveFalse(), false);
    assert.equal(derive.userFieldValuesSummary(), 'name=Ada,age=42');
});
