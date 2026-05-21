package dev.capylang.test;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.ResourceLock;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import static org.assertj.core.api.Assertions.assertThat;

@ResourceLock("java.lang.System.out")
class ObjectConstructionInteropTest {
    @Test
    void functionalObjectConstructionIsLazyAndFresh() {
        var originalOut = System.out;
        var out = new ByteArrayOutputStream();
        try {
            System.setOut(new PrintStream(out));

            var effect = ObjectConstructionInterop.makePerson("Ada");
            assertThat(out.toString()).isEmpty();

            var first = effect.unsafeRun();
            assertThat(first).isInstanceOf(TrackedPerson.class);
            assertThat(first.label()).isEqualTo("Ada");
            assertThat(out.toString()).isEqualTo("constructed:Ada" + System.lineSeparator());

            out.reset();
            var second = effect.unsafeRun();
            assertThat(second).isInstanceOf(TrackedPerson.class);
            assertThat(second).isNotSameAs(first);
            assertThat(out.toString()).isEqualTo("constructed:Ada" + System.lineSeparator());
        } finally {
            System.setOut(originalOut);
        }
    }

    @Test
    void functionalObjectConstructionSupportsParentTypesAndSequencing() {
        var originalOut = System.out;
        var out = new ByteArrayOutputStream();
        try {
            System.setOut(new PrintStream(out));

            var printable = ObjectConstructionInterop.makePrintable("Bara").unsafeRun();
            assertThat(printable).isInstanceOf(TrackedPerson.class);
            assertThat(printable.label()).isEqualTo("Bara");

            out.reset();
            assertThat(ObjectConstructionInterop.sequenceTwo("A", "B").unsafeRun()).isEqualTo("A:B");
            assertThat(out.toString()).isEqualTo(
                    "constructed:A" + System.lineSeparator()
                    + "constructed:B" + System.lineSeparator()
            );
        } finally {
            System.setOut(originalOut);
        }
    }

    @Test
    void zeroArgumentObjectConstructionIsDelayed() {
        var originalOut = System.out;
        var out = new ByteArrayOutputStream();
        try {
            System.setOut(new PrintStream(out));

            var effect = ObjectConstructionInterop.makeEmpty();
            assertThat(out.toString()).isEmpty();

            var empty = effect.unsafeRun();
            assertThat(empty).isInstanceOf(EmptyTracked.class);
            assertThat(empty.label()).isEqualTo("empty");
            assertThat(out.toString()).isEqualTo("empty" + System.lineSeparator());
        } finally {
            System.setOut(originalOut);
        }
    }

    @Test
    void qualifiedImportObjectConstructionWorks() {
        var originalOut = System.out;
        var out = new ByteArrayOutputStream();
        try {
            System.setOut(new PrintStream(out));

            var person = ObjectConstructionInterop.makeQualifiedPerson("Qual").unsafeRun();
            assertThat(person).isInstanceOf(TrackedPerson.class);
            assertThat(person.label()).isEqualTo("Qual");
            assertThat(out.toString()).isEqualTo("constructed:Qual" + System.lineSeparator());

            out.reset();
            var printable = ObjectConstructionInterop.makeQualifiedPrintable("Iface").unsafeRun();
            assertThat(printable).isInstanceOf(TrackedPerson.class);
            assertThat(printable.label()).isEqualTo("Iface");
            assertThat(out.toString()).isEqualTo("constructed:Iface" + System.lineSeparator());
        } finally {
            System.setOut(originalOut);
        }
    }
}
