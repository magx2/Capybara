package dev.capylang.test;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class EmptyLiteralInferenceTest {
    @Test
    void reverseIntsUsesInlineEmptyLiteral() {
        var reversed = EmptyLiteralInference.reverseInts(List.of(1, 2, 3));
        assertThat(reversed).containsExactly(3, 2, 1);
    }

    @Test
    void duplicateStringsUsesInlineEmptyLiteral() {
        var duplicated = EmptyLiteralInference.duplicateStrings(List.of("a", "b"));
        assertThat(duplicated).containsExactly("a", "a", "b", "b");
    }

    @Test
    void emptyListOfTypeParamInfersConcreteTypeFromCallSite() {
        assertThat(EmptyLiteralInference.emptyListOfTypeParam(42)).isEmpty();
        assertThat(EmptyLiteralInference.emptyListOfTypeParam("x")).isEmpty();
    }

    @Test
    void toSeqUsingEndLiteralCompilesAndBuildsSequence() {
        var seq = EmptyLiteralInference.toSeqUsingEndLiteral(List.of(4, 5));
        assertThat(EmptyLiteralInference.seqEndHeadOrDefault(seq, -1)).isEqualTo(5);
    }
}
