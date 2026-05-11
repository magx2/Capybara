const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('Imports', () => {
    const importsMain = generatedModule('dev/capylang/test/imports/Main.js');
    assert.equal(importsMain.calculator(2, 3, '+'), 5);
    assert.equal(importsMain.calculator(15, 0, 'p3'), 100);
    assert.equal(importsMain.x(2, 3, '+'), 'Day as usual');
    assert.equal(importsMain.xyz(1, 2, 3).xyz, '123');
    assert.equal(importsMain.type5(5, 2).__capybaraType, 'D51');
    assert.equal(importsMain.type5(0, 7).__capybaraType, 'D52');
});
