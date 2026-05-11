const test = require('node:test');
const { assert, captureOutput, generatedModule } = require('./_support');

test('class methods use constructor state and inherited behavior', () => {
    const { Person } = generatedModule('dev/capylang/test/Person.js');
    const person = new Person('Capy');

    assert.equal(person.greet(), 'hello Capy');
    assert.equal(person.mutable(), '2');
    assert.equal(person.print(), 'hello Capy!');
});

test('trait defaults and contracts are available on generated classes', () => {
    const { Person } = generatedModule('dev/capylang/test/Person.js');
    const { Printable } = generatedModule('dev/capylang/test/Printable.js');
    const { BracketNaming } = generatedModule('dev/capylang/test/BracketNaming.js');
    const person = new Person('Capy');

    assert.equal(person.display(true), '[Capy]');
    assert.equal(person.display(false), 'Capy');
    assert.equal(person instanceof Printable, true);
    assert.equal(person instanceof BracketNaming, true);
    assert.equal(person.bracket('Capy'), '[Capy]');
});

test('nested blocks, local methods, loops, arrays, exceptions, and void methods execute', () => {
    const { Person } = generatedModule('dev/capylang/test/Person.js');
    const person = new Person('Capy');
    const people = [person, new Person('Bara')];

    assert.equal(person.nested_label(), 'hello Capy');
    assert.equal(person.local_increment(7), 8);
    assert.equal(person.parity(12), true);
    assert.equal(person.parity(15), false);
    assert.equal(person.first_positive([-2, 0, 4]), 4);
    assert.equal(person.first_positive([-2, 0]), 0);
    assert.equal(person.first_large([3, 12, 20]), 12);
    assert.equal(person.while_flag(true), 1);
    assert.equal(person.while_flag(false), 0);
    assert.equal(person.do_once(true), 1);
    assert.equal(person.do_once(false), 2);
    assert.equal(person.second_name(['zero', 'one', 'two']), 'one');
    assert.equal(person.first_id([9, 8, 7]), 9);
    assert.equal(person.copy_people(people), people);
    assert.deepEqual(person.names(), ['zero', 'one']);
    assert.equal(person.slots(4).length, 4);
    assert.equal(person.recover(false), 'ok');
    assert.equal(person.recover(true), 'boom');
    assert.equal(person.catch_index(['zero']), 'ArrayIndexOutOfBoundsException');
    assert.doesNotThrow(() => person.ping());
    assert.doesNotThrow(() => person.warmup(true));
    assert.doesNotThrow(() => person.warmup(false));
    assert.doesNotThrow(() => person.foreach_warm([1, 2, 3]));
    assert.doesNotThrow(() => person.while_warm(false));
    assert.doesNotThrow(() => person.do_warm());
});

test('stdout methods print characters and lines', () => {
    const { Person } = generatedModule('dev/capylang/test/Person.js');
    const person = new Person('Capy');

    const printed = captureOutput(() => {
        person.emit_greeting();
        person.emit_boxed_name();
        person.emit_star();
    });

    assert.equal(printed.stdout.replace(/\r\n/g, '\n'), 'hello Capy\n[Capy]\n*');
});
