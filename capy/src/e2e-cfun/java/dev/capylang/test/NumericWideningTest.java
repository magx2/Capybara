package dev.capylang.test;

import org.junit.jupiter.api.Test;

import java.lang.reflect.Method;

import static org.assertj.core.api.Assertions.assertThat;

class NumericWideningTest {
    @Test
    void shouldWidenIntToLongForReturnTypeAndInvocation() throws NoSuchMethodException {
        assertThat(NumericWidening.returnLong(7)).isEqualTo(7L);
        assertThat(NumericWidening.typedLetLong(7)).isEqualTo(7L);
        assertThat(NumericWidening.multiplyWithLong(3)).isEqualTo(30L);
        assertThat(NumericWidening.invokeLong(4)).isEqualTo(5L);

        Method returnLong = NumericWidening.class.getMethod("returnLong", int.class);
        Method invokeLong = NumericWidening.class.getMethod("invokeLong", int.class);
        assertThat(returnLong.getReturnType()).isEqualTo(long.class);
        assertThat(invokeLong.getReturnType()).isEqualTo(long.class);
    }

    @Test
    void shouldEvaluateIntArithmeticSubexpressionsInWidenedLongContext() {
        assertThat(NumericWidening.multiplyLongChain()).isEqualTo(1_000_000_000_000_000_000L);
        assertThat(NumericWidening.divideLongByGroupedIntChain(120_000_000_000L)).isEqualTo(2L);
        assertThat(NumericWidening.multiplyLongByGroupedIntChain(2L)).isEqualTo(120_000_000_000L);
        assertThat(NumericWidening.divideLongByGroupedIntChainBranch(120_000_000_000L, true)).isEqualTo(2L);
        assertThat(NumericWidening.divideLongByGroupedIntChainBranch(120L, false)).isEqualTo(2L);
    }

    @Test
    void shouldWidenFloatToDoubleForReturnTypeAndInvocation() throws NoSuchMethodException {
        assertThat(NumericWidening.returnDouble(1.25f)).isEqualTo(1.25d);
        assertThat(NumericWidening.typedLetDouble(1.25f)).isEqualTo(1.25d);
        assertThat(NumericWidening.invokeDouble(1.25f)).isEqualTo(1.75d);

        Method returnDouble = NumericWidening.class.getMethod("returnDouble", float.class);
        Method invokeDouble = NumericWidening.class.getMethod("invokeDouble", float.class);
        assertThat(returnDouble.getReturnType()).isEqualTo(double.class);
        assertThat(invokeDouble.getReturnType()).isEqualTo(double.class);
    }

    @Test
    void shouldWidenIntToFloatAndDouble() {
        assertThat(NumericWidening.intToFloat(6)).isEqualTo(6.0f);
        assertThat(NumericWidening.typedLetFloat(6)).isEqualTo(6.0f);
        assertThat(NumericWidening.intToDouble(6)).isEqualTo(6.0d);
        assertThat(NumericWidening.invokeDoubleFromInt(6)).isEqualTo(6.5d);
        assertThat(NumericWidening.buildFloatBox(8).value()).isEqualTo(8.0f);
    }

    @Test
    void shouldWidenLongToFloatAndDouble() {
        assertThat(NumericWidening.longToFloat(6L)).isEqualTo(6.0f);
        assertThat(NumericWidening.typedLetFloatFromLong(6L)).isEqualTo(6.0f);
        assertThat(NumericWidening.longToDouble(6L)).isEqualTo(6.0d);
        assertThat(NumericWidening.invokeDoubleFromLong(6L)).isEqualTo(6.5d);
        assertThat(NumericWidening.buildFloatBoxFromLong(8L).value()).isEqualTo(8.0f);
    }

    @Test
    void shouldWidenWhenConstructingData() {
        var measurement = NumericWidening.buildMeasurement(9, 2.5f);
        var positionalMeasurement = NumericWidening.buildMeasurementPositional(11, 4.5f);

        assertThat(measurement.amount()).isEqualTo(9L);
        assertThat(measurement.ratio()).isEqualTo(2.5d);
        assertThat(positionalMeasurement.amount()).isEqualTo(11L);
        assertThat(positionalMeasurement.ratio()).isEqualTo(4.5d);
        assertThat(NumericWidening.amountFromMeasurement(12)).isEqualTo(12L);
        assertThat(NumericWidening.ratioFromMeasurement(3.5f)).isEqualTo(3.5d);
        assertThat(NumericWidening.buildLongBox(13).value()).isEqualTo(13L);
    }
}
