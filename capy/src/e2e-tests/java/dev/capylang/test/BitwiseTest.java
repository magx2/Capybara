package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class BitwiseTest {
    @Test
    void bitAnd() {
        assertThat(Bitwise.bitAnd(0b1100, 0b1010)).isEqualTo(0b1000);
    }

    @Test
    void bitNand() {
        assertThat(Bitwise.bitNand(0b1100, 0b1010)).isEqualTo(~0b1000);
    }

    @Test
    void bitOr() {
        assertThat(Bitwise.bitOr(0b1100, 0b1010)).isEqualTo(0b1110);
    }

    @Test
    void bitXor() {
        assertThat(Bitwise.bitXor(0b1100, 0b1010)).isEqualTo(0b0110);
    }

    @Test
    void bitNot() {
        assertThat(Bitwise.bitNot(0b1100)).isEqualTo(~0b1100);
    }
}
