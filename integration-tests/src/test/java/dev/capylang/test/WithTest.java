package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class WithTest {
    @Test
    void updateFoo() {
        var result = With.updateFoo(new With.Foo(1, "old", 1.5));

        assertThat(result.a()).isEqualTo(2);
        assertThat(result.b()).isEqualTo("new value");
        assertThat(result.c()).isEqualTo(1.5);
    }

    @Test
    void chainFoo() {
        var result = With.chainFoo(new With.Foo(1, "old", 1.5));

        assertThat(result.a()).isEqualTo(2);
        assertThat(result.b()).isEqualTo("new value");
        assertThat(result.c()).isEqualTo(1.5);
    }

    @Test
    void updateLetterPreservesSubtype() {
        var result = With.updateLetter(new With.A(1, "hello"));

        assertThat(result).isInstanceOf(With.A.class);
        var value = (With.A) result;
        assertThat(value.x()).isEqualTo(2);
        assertThat(value.a()).isEqualTo("hello");
    }

    @Test
    void chainLetterPreservesSubtype() {
        var result = With.chainLetter(new With.B(7, 123));

        assertThat(result).isInstanceOf(With.B.class);
        var value = (With.B) result;
        assertThat(value.x()).isEqualTo(99);
        assertThat(value.b()).isEqualTo(123);
    }

    @Test
    void concreteSubtypeCanUpdateParentAndOwnFields() {
        var result = With.updateA(new With.A(5, "abc"));

        assertThat(result.x()).isEqualTo(6);
        assertThat(result.a()).isEqualTo("abc!");
    }

    @Test
    void withReappliesConstructorForSingleUpdate() {
        var result = With.updateSanitizedCounter(With.makeSanitizedCounter(3));

        assertThat(result.number()).isEqualTo(5);
        assertThat(result.constructor_runs()).isEqualTo(2);
    }

    @Test
    void withReappliesConstructorForChainedUpdates() {
        var result = With.chainSanitizedCounter(With.makeSanitizedCounter(3));

        assertThat(result.number()).isEqualTo(9);
        assertThat(result.constructor_runs()).isEqualTo(3);
    }

    @Test
    void withReappliesResultReturningConstructorForSingleUpdate() {
        assertThat(With.validatedCounterAfterSingleWith(1)).isEqualTo("ok:2:2");
        assertThat(With.validatedCounterAfterSingleWith(-1)).isEqualTo("err:negative:-1");
    }

    @Test
    void withReappliesResultReturningConstructorForChainedUpdates() {
        assertThat(With.validatedCounterAfterMultipleWiths(4)).isEqualTo("ok:0:3");
        assertThat(With.validatedCounterAfterMultipleWiths(3)).isEqualTo("err:negative:-1");
    }
}
