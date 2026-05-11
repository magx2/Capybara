const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('Data', () => {
    const data = generatedModule('dev/capylang/test/Data.js');
    assert.equal(data.area(new data.Circle({ radius: 2.0 })), 12.56);
    assert.equal(data.area(new data.Rectangle({ width: 3.0, height: 4.0 })), 12.0);
    assert.equal(data.daVinci(new data.Circle({ radius: 2.0 })).__capybaraType, 'Rectangle');
    assert.equal(data.daVinci(new data.Circle({ radius: 2.0 })).width, 4.0);
    assert.equal(data.daVinci(new data.Rectangle({ width: 6.0, height: 2.0 })).__capybaraType, 'Circle');
    assert.equal(data.daVinci(new data.Rectangle({ width: 6.0, height: 2.0 })).radius, 2.0);
    assert.equal(data.printableDataToString(), 'PrintableData { "foo": "abc", "boo": 123 }');
    assert.equal(data.stringPlusDataLeft(), 'prefix=PrintableData { "foo": "abc", "boo": 123 }');
    assert.equal(data.stringPlusDataRight(), 'PrintableData { "foo": "abc", "boo": 123 }=suffix');
    assert.equal(data.knightHolderToString(), 'KnightHolder { "knight": Tom }');
    assert.equal(data.createPositionalData('abc', 10).text, 'abc');
    assert.equal(data.createPositionalDataExpr('abc', 10).text, 'abc!');
    assert.equal(data.readDataType(data.createDataWithType('record')), 'record');
    assert.equal(data.genericFieldPipe(new data.Box({ value: '41' })).value, 43);
    assert.equal(data.genericFieldPipe(new data.Box({ value: 'x' })).__capybaraType, 'BoxError');
});
