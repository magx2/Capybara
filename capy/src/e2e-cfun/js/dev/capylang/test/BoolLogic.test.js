const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('BoolLogic', () => {
    const boolLogic = generatedModule('dev/capylang/test/BoolLogic.js');
    assert.equal(boolLogic.andLogic(true, true), true);
    assert.equal(boolLogic.andLogic(true, false), false);
    assert.equal(boolLogic.andLogic(false, true), false);
    assert.equal(boolLogic.orLogic(false, true), true);
    assert.equal(boolLogic.orLogic(false, false), false);
    assert.equal(boolLogic.equalLogic(true, true), true);
    assert.equal(boolLogic.notEqualLogic(true, false), true);
    assert.equal(boolLogic.notLogic(false), true);
    assert.equal(boolLogic.notFieldAccess(new boolLogic.BoolBox({ failed: true })), false);
    assert.equal(boolLogic.notMethodCall(new boolLogic.BoolBox({ failed: false })), true);
    assert.equal(boolLogic.complexLogic(false, true), true);
    assert.equal(boolLogic.byteToBool(0), false);
    assert.equal(boolLogic.intToBool(2), true);
    assert.equal(boolLogic.stringToBool(''), false);
    assert.equal(boolLogic.listToBool([1]), true);
    assert.equal(boolLogic.setToBool(new Set()), false);
    assert.equal(boolLogic.dictToBool(new Map([['a', 1]])), true);
    assert.equal(boolLogic.andInt(2, 1), true);
    assert.equal(boolLogic.orString('', ''), false);
    assert.equal(boolLogic.notList([]), true);
});
