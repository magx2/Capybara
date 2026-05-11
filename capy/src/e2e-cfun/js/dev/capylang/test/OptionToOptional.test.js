const test = require('node:test');
const { assert, capy, generatedModule } = require('./_support');

test('OptionToOptional', () => {
    const option = generatedModule('dev/capylang/test/OptionToOptional.js');
    assert.equal(option.stringOption(new capy.Some({ value: 'abc' })), 'abc');
    assert.equal(option.stringOption(capy.None), '<empty>');
    assert.equal(option.boolOption(new capy.Some({ value: true })), true);
    assert.equal(option.optionToMessage(capy.None), 'missing');
    assert.equal(option.intOptionToMessage(new capy.Some({ value: 10 })), 'value=10');
    assert.equal(option.matchDeserialize('bad'), 'failed: bad');
    assert.equal(option.matchDeserialize('ok'), 'prefix:json:7');
});
