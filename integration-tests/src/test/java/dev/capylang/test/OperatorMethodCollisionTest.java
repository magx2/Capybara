package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class OperatorMethodCollisionTest {
    @Test
    void shouldKeepNamedAndOperatorMethodsDistinct() {
        var counter = new OperatorMethodCollision.Counter(10);
        assertThat(OperatorMethodCollision.namedPlus(counter, 5)).isEqualTo(15);
        assertThat(OperatorMethodCollision.operatorPlus(counter, 5)).isEqualTo(115);
    }
}
