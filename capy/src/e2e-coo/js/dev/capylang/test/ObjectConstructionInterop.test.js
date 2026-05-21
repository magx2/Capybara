const test = require('node:test');
const { assert, generatedModule, captureOutput } = require('./_support');

test('functional object construction is lazy, fresh, and CommonJS-wired', () => {
    const interop = generatedModule('dev/capylang/test/ObjectConstructionInterop.js');
    const { TrackedPerson } = generatedModule('dev/capylang/test/TrackedPerson.js');

    const created = captureOutput(() => interop.makePerson('Ada'));
    assert.equal(created.stdout, '');

    const first = captureOutput(() => created.result.unsafe_run());
    assert.equal(first.result instanceof TrackedPerson, true);
    assert.equal(first.result.label(), 'Ada');
    assert.equal(first.stdout, 'constructed:Ada\n');

    const second = captureOutput(() => created.result.unsafe_run());
    assert.equal(second.result instanceof TrackedPerson, true);
    assert.notEqual(second.result, first.result);
    assert.equal(second.stdout, 'constructed:Ada\n');
});

test('functional object construction supports parent types, zero args, and sequencing', () => {
    const interop = generatedModule('dev/capylang/test/ObjectConstructionInterop.js');
    const { TrackedPerson } = generatedModule('dev/capylang/test/TrackedPerson.js');
    const { EmptyTracked } = generatedModule('dev/capylang/test/EmptyTracked.js');

    const printable = captureOutput(() => interop.makePrintable('Bara').unsafe_run());
    assert.equal(printable.result instanceof TrackedPerson, true);
    assert.equal(printable.result.label(), 'Bara');

    const emptyEffect = captureOutput(() => interop.makeEmpty());
    assert.equal(emptyEffect.stdout, '');
    const empty = captureOutput(() => emptyEffect.result.unsafe_run());
    assert.equal(empty.result instanceof EmptyTracked, true);
    assert.equal(empty.result.label(), 'empty');
    assert.equal(empty.stdout, 'empty\n');

    const sequenced = captureOutput(() => interop.sequenceTwo('A', 'B').unsafe_run());
    assert.equal(sequenced.result, 'A:B');
    assert.equal(sequenced.stdout, 'constructed:A\nconstructed:B\n');
});
