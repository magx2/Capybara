const test = require('node:test');
const { assert, capy, generatedModule } = require('./_support');

test('object-oriented code can invoke FP functions, create FP data, and match FP types', () => {
    const { PetInteractor } = generatedModule('dev/capylang/test/PetInteractor.js');
    const fp = generatedModule('dev/capylang/test/ObjectOrientedFpInterop.js');
    const interactor = new PetInteractor();

    assert.equal(interactor.invoke_fp_function('Capy'), 'dog:Capy');
    const dog = interactor.create_fp_data('Bara');
    assert.equal(dog instanceof fp.InteropDog, true);
    assert.equal(dog.name, 'Bara');
    assert.equal(interactor.match_fp_type('Mochi'), 'dog:Mochi');
    assert.equal(interactor.create_fp_empty(), fp.InteropNone);
    assert.equal(interactor.match_fp_empty(), 'none');
    assert.equal(interactor.create_runtime_none(), capy.None);
    assert.equal(interactor.invoke_rec_function(10), 36);
    assert.equal(interactor.echo_user_id(7), 7);
    assert.equal(interactor.construct_user_id(11), 11);
    assert.equal(interactor.make_user_id(13), 13);
    assert.equal(interactor.unwrap_user_id(17), 17);
    assert.equal(interactor.add_user_ids(19, 23), 42);
    assert.equal(interactor.user_id_slots(10).length, 10);
    assert.equal(interactor.local_user_id(29), 29);
});
