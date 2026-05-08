package dev.capylang.test;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

class UserDefinedIndexingTest {
    @Test
    void intIndex() {
        assertThat(UserDefinedIndexing.intIndex()).isEqualTo("one");
    }

    @Test
    void stringIndex() {
        assertThat(UserDefinedIndexing.stringIndex()).isEqualTo(42.5);
    }

    @Test
    void matrixIndexName() {
        assertThat(UserDefinedIndexing.matrixIndexName()).isEqualTo("second");
    }

    @Test
    void chainedIndex() {
        assertThat(UserDefinedIndexing.chainedIndex()).isEqualTo(Optional.of("e"));
    }

    @Test
    void listVariableIndex() {
        assertThat(UserDefinedIndexing.listVariableIndex(List.of("a", "b", "c"), 1)).isEqualTo(Optional.of("b"));
        assertThat(UserDefinedIndexing.listVariableIndex(List.of("a"), 3)).isEmpty();
    }
}
