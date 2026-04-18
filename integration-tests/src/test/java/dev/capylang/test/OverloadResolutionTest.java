package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class OverloadResolutionTest {
    @Test
    void shouldPreferExactListOverloadOverWidenableOne() {
        assertThat(OverloadResolution.chooseInts()).isEqualTo(103);
        assertThat(OverloadResolution.chooseLongs()).isEqualTo(203);
    }
}
