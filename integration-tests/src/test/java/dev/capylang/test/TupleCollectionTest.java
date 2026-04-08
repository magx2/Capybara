package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class TupleCollectionTest {
    @Test
    void tupleWithFunctionsApplyFirst() {
        assertThat(TupleCollection.tupleWithFunctionsApplyFirst(7)).isEqualTo(8);
    }

    @Test
    void tupleWithFunctionsApplySecond() {
        assertThat(TupleCollection.tupleWithFunctionsApplySecond(7)).isEqualTo(14);
    }
}
