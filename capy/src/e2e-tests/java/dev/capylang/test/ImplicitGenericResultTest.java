package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ImplicitGenericResultTest {
    @Test
    void errorOrElseWithoutExplicitResultType() {
        assertThat(ImplicitGenericResult.errorOrElseWithoutExplicitResultType()).isEqualTo(0);
    }

    @Test
    void errorOrWithoutExplicitResultType() {
        assertThat(ImplicitGenericResult.errorOrWithoutExplicitResultType()).isEqualTo(101);
    }
}
