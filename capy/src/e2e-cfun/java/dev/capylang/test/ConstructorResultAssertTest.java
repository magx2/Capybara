package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ConstructorResultAssertTest {
    @Test
    void constructorExpressionUsesResultAssertForSuccessChecks() {
        assertThat(ConstructorResultAssert.validDateAssertSucceeds()).isTrue();
    }

    @Test
    void constructorExpressionUsesResultAssertForFailureChecks() {
        assertThat(ConstructorResultAssert.invalidDateAssertFails()).isTrue();
    }
}
