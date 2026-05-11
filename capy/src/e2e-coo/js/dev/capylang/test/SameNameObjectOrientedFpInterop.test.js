const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('object-oriented code can use FP sibling module without imports when file names match', () => {
    const { SharedInteractor } = generatedModule('dev/capylang/test/SharedInteractor.js');
    const shared = generatedModule('dev/capylang/test/SharedInterop.js');
    const interactor = new SharedInteractor();

    assert.equal(interactor.invoke_fp_function('Capy'), 'dog:Capy');
    const dog = interactor.create_fp_data('Bara');
    assert.equal(dog instanceof shared.SharedDog, true);
    assert.equal(dog.name, 'Bara');
    assert.equal(interactor.match_fp_type('Mochi'), 'dog:Mochi');
});
