const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('object-oriented code can invoke FP functions, create FP data, and match FP types', () => {
    const { PetInteractor } = generatedModule('dev/capylang/test/PetInteractor.js');
    const fp = generatedModule('dev/capylang/test/ObjectOrientedFpInterop.js');
    const interactor = new PetInteractor();

    assert.equal(interactor.invoke_fp_function('Capy'), 'dog:Capy');
    const dog = interactor.create_fp_data('Bara');
    assert.equal(dog instanceof fp.InteropDog, true);
    assert.equal(dog.name, 'Bara');
    assert.equal(interactor.match_fp_type('Mochi'), 'dog:Mochi');
    assert.equal(interactor.invoke_rec_function(10), 36);
});
