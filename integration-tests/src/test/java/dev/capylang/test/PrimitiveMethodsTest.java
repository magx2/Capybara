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
}
