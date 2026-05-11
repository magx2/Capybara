const test = require('node:test');
const { assert, assertSuccess, generatedModule } = require('./_support');

test('ResultBind', () => {
    const resultBind = generatedModule('dev/capylang/test/ResultBind.js');
    const users = assertSuccess(resultBind.users('Ann', 'Bob', 'Cid'));
    assert.equal(users.length, 3);
    assert.equal(users[0].name, 'Ann');
    assert.equal(users[2].name, 'Cid');
    assert.equal(resultBind.firstErrorMessage('', 'Bob'), 'You need to pass name');
});
