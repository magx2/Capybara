package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ModuloTest {
    @Test
    void modInt() {
        assertThat(Modulo.modInt(10, 3)).isEqualTo(1);
        assertThat(Modulo.modInt(-10, 3)).isEqualTo(-1);
    }

    @Test
    void modLong() {
        assertThat(Modulo.modLong(10L, 4L)).isEqualTo(2L);
    }

    @Test
    void modByte() {
        assertThat(Modulo.modByte((byte) 0x07, (byte) 0x03)).isEqualTo((byte) 0x01);
    }

    @Test
    void modFloat() {
        assertThat(Modulo.modFloat(7.5f, 2.0f)).isEqualTo(1.5f);
    }

    @Test
    void modDouble() {
        assertThat(Modulo.modDouble(7.5d, 2.0d)).isEqualTo(1.5d);
    }

    @Test
    void modMixedIntLong() {
        assertThat(Modulo.modMixedIntLong(10, 4L)).isEqualTo(2L);
    }
}
