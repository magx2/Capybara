const test = require('node:test');
const { assert, capy, generatedModule } = require('./_support');

test('NamedPipeMethods', () => {
    const named = generatedModule('dev/capylang/test/NamedPipeMethods.js');
    assert.deepEqual(named.seqMapNamed([1, 2, 3]), [3, 6, 9]);
    assert.equal(named.seqReduceNamed([1, 2, 3]), 6);
    assert.equal(named.seqReduceLeftNamed([1, 2, 3]), 6);
    assert.equal(named.resultReduceNamed(new capy.Success({ value: 3 })), 'ok:3');
    assert.equal(named.resultReduceNamed(new capy.Error({ message: 'boom' })), 'err:boom');
    assert.equal(named.resultReduceOperator(new capy.Success({ value: 3 })), 'ok:3');
});
