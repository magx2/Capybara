package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class MinLiteralValueTest {
    @Test
    void minIntCompilesAndExecutes() {
        assertThat(MinLiteralValue.minInt()).isEqualTo(Integer.MIN_VALUE);
    }

    @Test
    void minIntWorksInRegularExpressions() {
        assertThat(MinLiteralValue.minIntDigits()).isEqualTo(10);
    }

    @Test
    void minLongCompilesAndExecutes() {
        assertThat(MinLiteralValue.minLong()).isEqualTo(Long.MIN_VALUE);
    }
}
