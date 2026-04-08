package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class WithExpressionTest {
    @Test
    void withUpdatesSingleDataField() {
        var result = WithExpression.dataWithSingle(new WithExpression.Foo(1, "old", 2.5));

        assertThat(result.a()).isEqualTo(2);
        assertThat(result.b()).isEqualTo("old");
        assertThat(result.c()).isEqualTo(2.5);
    }

    @Test
    void withUpdatesMultipleDataFields() {
        var result = WithExpression.dataWithMultiple(new WithExpression.Foo(1, "old", 2.5));

        assertThat(result.a()).isEqualTo(2);
        assertThat(result.b()).isEqualTo("new value");
        assertThat(result.c()).isEqualTo(2.5);
    }

    @Test
    void withSupportsChainingForData() {
        var result = WithExpression.dataWithChain(new WithExpression.Foo(1, "old", 2.5));

        assertThat(result.a()).isEqualTo(2);
        assertThat(result.b()).isEqualTo("new value");
        assertThat(result.c()).isEqualTo(2.5);
    }

    @Test
    void withUpdatesParentTypeAndPreservesSubtype() {
        var updatedA = WithExpression.parentWith(new WithExpression.A(1, "alpha"));
        assertThat(updatedA).isInstanceOf(WithExpression.A.class);
        assertThat(((WithExpression.A) updatedA).x()).isEqualTo(2);
        assertThat(((WithExpression.A) updatedA).a()).isEqualTo("alpha");

        var updatedB = WithExpression.parentWith(new WithExpression.B(4, 99));
        assertThat(updatedB).isInstanceOf(WithExpression.B.class);
        assertThat(((WithExpression.B) updatedB).x()).isEqualTo(5);
        assertThat(((WithExpression.B) updatedB).b()).isEqualTo(99);
    }

    @Test
    void withSupportsChainingForParentType() {
        var result = WithExpression.parentWithChain(new WithExpression.C(3, 7.5));
        assertThat(result).isInstanceOf(WithExpression.C.class);
        assertThat(((WithExpression.C) result).x()).isEqualTo(100);
        assertThat(((WithExpression.C) result).c()).isEqualTo(7.5);
    }

    @Test
    void parentWithPreservesSubtypeInMatch() {
        assertThat(WithExpression.parentWithPreservesSubtype(new WithExpression.A(1, "x"))).isEqualTo("A:2:x");
        assertThat(WithExpression.parentWithPreservesSubtype(new WithExpression.B(2, 3))).isEqualTo("B:3:3");
        assertThat(WithExpression.parentWithPreservesSubtype(new WithExpression.C(3, 4.5))).isEqualTo("C:4:4.5");
    }
}

