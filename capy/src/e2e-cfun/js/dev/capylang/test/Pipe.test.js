const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('Pipe', () => {
    const pipe = generatedModule('dev/capylang/test/Pipe.js');
    assert.deepEqual(pipe.map1([1, 2, 3]).asList(), [2, 4, 6]);
    assert.deepEqual(pipe.filter1([-1, 0, 1, 2]).asList(), [1, 2]);
    assert.equal(pipe.reduce1([1, 2, 3, 4]), 10);
    assert.equal(pipe.reduce2([-1, 1, 2, 3]), -2);
    assert.equal(pipe.reduceString2(['a', 'b', 'c']), 'start: a b c');
    assert.deepEqual(pipe.stringMap('abc').asList(), ['[a]', '[b]', '[c]']);
    assert.deepEqual(pipe.flatMap1([1, 2]).asList(), [1, 2, 3, 2, 3, 4]);
    assert.equal(pipe.pipeThenChainAfterLambda(), 3);
    assert.equal(pipe.pipeThenChainMethodsAfterLambda(), 'ok');
});
