package dev.capylang;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

class CapybaraUtilTest {
    @ParameterizedTest
    @CsvSource({
            "1.5, 3, 3.375",
            "7.5, 0, 1.0"
    })
    void shouldCalculatePowerForDouble(double base, int exponent, double expected) {
        assertEquals(expected, CapybaraUtil.power(base, exponent));
    }

    @ParameterizedTest
    @CsvSource({
            "1.5, 3, 3.375",
            "7.5, 0, 1.0"
    })
    void shouldCalculatePowerForFloat(float base, int exponent, float expected) {
        assertEquals(expected, CapybaraUtil.power(base, exponent));
    }

    @ParameterizedTest
    @CsvSource({
            "3, 4, 81",
            "7, 0, 1"
    })
    void shouldCalculatePowerForShort(short base, int exponent, short expected) {
        assertEquals(expected, CapybaraUtil.power(base, exponent));
    }

    @ParameterizedTest
    @CsvSource({
            "2, 5, 32",
            "7, 0, 1"
    })
    void shouldCalculatePowerForByte(byte base, int exponent, byte expected) {
        assertEquals(expected, CapybaraUtil.power(base, exponent));
    }
}
