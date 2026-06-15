package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledDataType;
import org.junit.jupiter.api.Test;

import java.util.List;

import static dev.capylang.compiler.PrimitiveLinkedType.ANY;
import static dev.capylang.compiler.PrimitiveLinkedType.ENUM;
import static org.assertj.core.api.Assertions.assertThat;

class CapybaraTypeFinderTest {
    @Test
    void enumPrimitiveAndEnumValueMergeToEnum() {
        var ready = new CompiledDataType("READY", List.of(), List.of(), List.of(), List.of(), null, true, true);

        assertThat(CapybaraTypeFinder.findHigherType(ENUM, ready)).isEqualTo(ENUM);
        assertThat(CapybaraTypeFinder.findHigherType(ready, ENUM)).isEqualTo(ENUM);
    }

    @Test
    void enumPrimitiveAndRegularSingletonDoNotMergeToEnum() {
        var singleton = new CompiledDataType("READY", List.of(), List.of(), List.of(), true);

        assertThat(CapybaraTypeFinder.findHigherType(ENUM, singleton)).isEqualTo(ANY);
        assertThat(CapybaraTypeFinder.findHigherType(singleton, ENUM)).isEqualTo(ANY);
    }
}
