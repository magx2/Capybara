package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class DateTimeDifferenceTest {
    @Test
    void shouldRenderForwardDifferenceAsCanonicalIsoDuration() {
        assertThat(DateTimeDifference.forwardDifferenceIso()).isEqualTo("P1DT1H1M1S");
    }

    @Test
    void shouldRenderBackwardDifferenceAsNegatedCanonicalIsoDuration() {
        assertThat(DateTimeDifference.backwardDifferenceIso()).isEqualTo("P-1DT-1H-1M-1S");
    }

    @Test
    void shouldRoundTripDifferenceThroughPlus() {
        assertThat(DateTimeDifference.differenceRoundTrips()).isTrue();
    }
}
