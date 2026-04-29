package dev.capylang.test;

import capy.test.Assert;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;

class FloatAssertTest {
    @Test
    void shouldSupportNumericComparisonAssertionsForFloat() {
        var assertResult = Assert.assertAll(java.util.List.of(
                Assert.assertThat(10.5f).isGreaterThan(9.5f),
                Assert.assertThat(10.5f).isLessThan(11.5f),
                Assert.assertThat(10.5f).isGreaterOrEqualsThan(10.5f),
                Assert.assertThat(10.5f).isLessOrEqualsThan(10.5f),
                Assert.assertThat(0f).isZero(),
                Assert.assertThat(1f).isOne(),
                Assert.assertThat(10.5f).isBetween(9.5f, 11.5f)
        ));

        assertFalse(assertResult.failed());
    }
}
