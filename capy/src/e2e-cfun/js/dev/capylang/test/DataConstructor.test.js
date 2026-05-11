const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('DataConstructor', () => {
    const dataConstructor = generatedModule('dev/capylang/test/DataConstructor.js');
    assert.equal(dataConstructor.oddNumber(3), 3);
    assert.equal(dataConstructor.oddNumber(4), 5);
    assert.equal(dataConstructor.validatedUserAge(7), 'ok:7');
    assert.equal(dataConstructor.validatedUserAge(0), 'err:Age has to be greater than 0. Was 0.');
    assert.equal(dataConstructor.validatedUserAgeOrDefault(0), 1);
    assert.equal(dataConstructor.validatedNamedUser('Ada', 'admin'), 'ok:Ada:admin');
    assert.equal(dataConstructor.validatedNamedUser('', 'admin'), 'err:Name was empty');
    assert.equal(dataConstructor.validatedProfile('Ada', ''), 'err:Profile role was empty');
});
