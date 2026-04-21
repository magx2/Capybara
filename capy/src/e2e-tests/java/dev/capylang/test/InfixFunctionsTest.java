package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class InfixFunctionsTest {
    @Test
    void methodStyleFunctionWithThis() {
        var sum = new InfixFunctions.Sum(10);
        assertThat(InfixFunctions.add(sum, 5).value()).isEqualTo(15);
    }

    @Test
    void infixPlusMethod() {
        var sum = new InfixFunctions.Sum(10);
        assertThat(InfixFunctions.plus(sum, 5).value()).isEqualTo(15);
    }

    @Test
    void infixCombinedLiteralMethod() {
        var sum = new InfixFunctions.Sum(10);
        assertThat(InfixFunctions.plusMinus(sum, 5).value()).isEqualTo(5);
    }

    @Test
    void infixMultiplyMethod() {
        var sum = new InfixFunctions.Sum(10);
        assertThat(InfixFunctions.mul(sum, 5).value()).isEqualTo(50);
    }

    @Test
    void backtickedSlashSlashMethod() {
        var sum = new InfixFunctions.Sum(10);
        assertThat(InfixFunctions.slashSlash(sum, 5).value()).isEqualTo(2);
    }

    @Test
    void backtickedDollarMinusMethod() {
        var sum = new InfixFunctions.Sum(10);
        assertThat(InfixFunctions.dollarMinus(sum, 5).value()).isEqualTo(4);
    }
}
