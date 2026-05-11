const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('EnumDataField', () => {
    const enumData = generatedModule('dev/capylang/test/EnumDataField.js');
    const root = new enumData.Path({ root: enumData.ABSOLUTE, segments: [] });
    const relative = new enumData.Path({ root: enumData.RELATIVE, segments: [] });
    assert.equal(enumData.rootName(root), 'ABSOLUTE');
    assert.equal(enumData.isAbsolute(root), true);
    assert.equal(enumData.isAbsolute(relative), false);
});
