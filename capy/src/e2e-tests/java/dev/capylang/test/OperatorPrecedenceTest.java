package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class OperatorPrecedenceTest {
    @Test
    void leapYearUsesModuloAndComparisonBeforeBooleanOperators() {
        assertThat(OperatorPrecedence.leapYear(2024)).isTrue();
        assertThat(OperatorPrecedence.leapYear(2023)).isFalse();
        assertThat(OperatorPrecedence.leapYear(1900)).isFalse();
        assertThat(OperatorPrecedence.leapYear(2000)).isTrue();
    }

    @Test
    void andBindsAfterModuloAndComparison() {
        assertThat(OperatorPrecedence.andBindsAfterModuloAndComparison(4, 9)).isTrue();
        assertThat(OperatorPrecedence.andBindsAfterModuloAndComparison(4, 10)).isFalse();
        assertThat(OperatorPrecedence.andBindsAfterModuloAndComparison(5, 9)).isFalse();
    }

    @Test
    void orBindsAfterModuloAndComparison() {
        assertThat(OperatorPrecedence.orBindsAfterModuloAndComparison(4, 10)).isTrue();
        assertThat(OperatorPrecedence.orBindsAfterModuloAndComparison(5, 9)).isTrue();
        assertThat(OperatorPrecedence.orBindsAfterModuloAndComparison(5, 10)).isFalse();
    }

    @Test
    void multiplicationBindsBeforeAddition() {
        assertThat(OperatorPrecedence.multiplicationBindsBeforeAddition()).isTrue();
    }

    @Test
    void powerBindsBeforeMultiplication() {
        assertThat(OperatorPrecedence.powerBindsBeforeMultiplication()).isTrue();
    }

    @Test
    void arithmeticBindsBeforeComparisonAndEquality() {
        assertThat(OperatorPrecedence.arithmeticBindsBeforeComparisonAndEquality()).isTrue();
    }

    @Test
    void andBindsBeforeOr() {
        assertThat(OperatorPrecedence.andBindsBeforeOr()).isTrue();
    }
}
