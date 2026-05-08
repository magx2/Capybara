package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class PrimitiveMethodsTest {
    @Test
    void longToIntUsesJavaCastSemantics() {
        assertThat(PrimitiveMethods.longToInt(42L)).isEqualTo(42);
        assertThat(PrimitiveMethods.longToInt(2_147_483_647L)).isEqualTo(Integer.MAX_VALUE);
        assertThat(PrimitiveMethods.longToInt(2_147_483_648L)).isEqualTo(Integer.MIN_VALUE);
        assertThat(PrimitiveMethods.longToInt(4_294_967_295L)).isEqualTo(-1);
        assertThat(PrimitiveMethods.longToInt(-2_147_483_649L)).isEqualTo(Integer.MAX_VALUE);
    }

    @Test
    void floatToIntUsesJavaCastSemantics() {
        assertThat(PrimitiveMethods.floatToInt(42.9f)).isEqualTo(42);
        assertThat(PrimitiveMethods.floatToInt(-42.9f)).isEqualTo(-42);
        assertThat(PrimitiveMethods.floatToInt(2_147_483_648f)).isEqualTo(Integer.MAX_VALUE);
        assertThat(PrimitiveMethods.floatToInt(-2_147_483_904f)).isEqualTo(Integer.MIN_VALUE);
    }

    @Test
    void doubleToIntUsesJavaCastSemantics() {
        assertThat(PrimitiveMethods.doubleToInt(42.9d)).isEqualTo(42);
        assertThat(PrimitiveMethods.doubleToInt(-42.9d)).isEqualTo(-42);
        assertThat(PrimitiveMethods.doubleToInt(2_147_483_648d)).isEqualTo(Integer.MAX_VALUE);
        assertThat(PrimitiveMethods.doubleToInt(-2_147_483_649d)).isEqualTo(Integer.MIN_VALUE);
    }

    @Test
    void floatToLongUsesJavaCastSemantics() {
        assertThat(PrimitiveMethods.floatToLong(42.9f)).isEqualTo(42L);
        assertThat(PrimitiveMethods.floatToLong(-42.9f)).isEqualTo(-42L);
        assertThat(PrimitiveMethods.floatToLong(9_223_372_036_854_775_808.0f)).isEqualTo(Long.MAX_VALUE);
        assertThat(PrimitiveMethods.floatToLong(-9_223_372_036_854_775_808.0f)).isEqualTo(Long.MIN_VALUE);
    }

    @Test
    void doubleToLongUsesJavaCastSemantics() {
        assertThat(PrimitiveMethods.doubleToLong(42.9d)).isEqualTo(42L);
        assertThat(PrimitiveMethods.doubleToLong(-42.9d)).isEqualTo(-42L);
        assertThat(PrimitiveMethods.doubleToLong(9_223_372_036_854_775_808.0d)).isEqualTo(Long.MAX_VALUE);
        assertThat(PrimitiveMethods.doubleToLong(-9_223_372_036_854_775_809.0d)).isEqualTo(Long.MIN_VALUE);
    }
}
