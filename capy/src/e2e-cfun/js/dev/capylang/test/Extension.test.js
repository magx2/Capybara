const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('Extension', () => {
    const extension = generatedModule('dev/capylang/test/Extension.js');
    assert.equal(extension.newPoint3d__float__float__float(1.0, 2.0, 3.0).z, 3.0);
    assert.equal(extension.newPoint3dVer2(4.0, 5.0, 6.0).z, 6.0);
    assert.equal(extension.length2d(new extension.Point({ x: 3.0, y: 4.0 })), 5.0);
    assert.equal(extension.onlyPoint2d(), 3.0);
    assert.equal(extension.testSubstitution(new extension.Point3D({ x: 3.0, y: 4.0, z: 100.0 })), 5.0);
    const weighted = new extension.WeightedLabeledPoint3D({ x: 1.0, y: 2.0, z: 3.0, label: 'capy', weight: 4.0, tag: 'v1' });
    assert.equal(extension.weightedLabeledPointScore(weighted), 10.0);
    assert.equal(extension.weightedLabeledPointLabel(weighted), 'capy:v1');
    assert.equal(extension.overloadAnyPrefersParent(new extension.Point3D({ x: 3.0, y: 4.0, z: 5.0 })), 'point');
    assert.equal(extension.newLabeledPoint(1.0, 2.0, 'origin').label, 'origin');
});
