package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class LocalFunctionsTest {
    @Test
    void localRecursiveFunctionCanBeCalled() {
        assertThat(LocalFunctions.sumDown(0)).isEqualTo(0);
        assertThat(LocalFunctions.sumDown(1)).isEqualTo(1);
        assertThat(LocalFunctions.sumDown(5)).isEqualTo(15);
    }

    @Test
    void localMutualRecursionWorks() {
        assertThat(LocalFunctions.parity(0)).isTrue();
        assertThat(LocalFunctions.parity(1)).isFalse();
        assertThat(LocalFunctions.parity(12)).isTrue();
        assertThat(LocalFunctions.parity(15)).isFalse();
    }

    @Test
    void localFunctionCanUseLetExpression() {
        assertThat(LocalFunctions.localWithLet(0)).isEqualTo(1);
        assertThat(LocalFunctions.localWithLet(10)).isEqualTo(11);
    }
}
