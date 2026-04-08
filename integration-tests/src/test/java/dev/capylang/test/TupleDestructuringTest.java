package dev.capylang.test;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class TupleDestructuringTest {
    @Test
    void mapPairs() {
        assertThat(TupleDestructuring.mapPairs(List.of(List.of(10, 2), List.of(99, 2))))
                .isEqualTo(List.of("digits(10) should return 2", "digits(99) should return 2"));
    }

    @Test
    void filterPairs() {
        assertThat(TupleDestructuring.filterPairs(List.of(List.of(1, 2), List.of(3, 2), List.of(5, 5))))
                .isEqualTo(List.of(List.of(3, 2), List.of(5, 5)));
    }

    @Test
    void flatMapPairs() {
        assertThat(TupleDestructuring.flatMapPairs(List.of(List.of(1, 2), List.of(3, 4))))
                .isEqualTo(List.of(1, 2, 3, 4));
    }

    @Test
    void anyEqual() {
        assertThat(TupleDestructuring.anyEqual(List.of(List.of(1, 2), List.of(3, 3)))).isTrue();
        assertThat(TupleDestructuring.anyEqual(List.of(List.of(1, 2), List.of(4, 3)))).isFalse();
    }

    @Test
    void allLess() {
        assertThat(TupleDestructuring.allLess(List.of(List.of(1, 2), List.of(3, 4)))).isTrue();
        assertThat(TupleDestructuring.allLess(List.of(List.of(2, 1), List.of(3, 4)))).isFalse();
    }
}
