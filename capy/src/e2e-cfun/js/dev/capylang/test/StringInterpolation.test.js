const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('StringInterpolation', () => {
    const interpolation = generatedModule('dev/capylang/test/StringInterpolation.js');
    assert.equal(interpolation.simple('Capybara'), 'Hello Capybara');
    assert.equal(interpolation.expression('Capy', 7), 'Hello Capy7');
    assert.equal(interpolation.escaped('ignored'), 'Hello \\{}');
    assert.equal(interpolation.escapedWithInterpolationValue('boo'), 'xyz {boo}');
    assert.equal(interpolation.quoteEscape(new interpolation.Foo({ name: 'Martin' })), 'Hello "Martin"');
    assert.equal(interpolation.dataField(new interpolation.Foo({ name: 'Martin' })), 'Hello Martin');
    assert.equal(interpolation.singleQuoted('Capybara'), 'Hello {name}');
});
