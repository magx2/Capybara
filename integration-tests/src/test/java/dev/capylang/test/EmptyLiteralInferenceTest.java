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
    void typedEmptyDictKeepsConcreteValueTypeAfterAppend() {
        assertThat(EmptyLiteralInference.typedEmptyDictThenAppendJsonBox("capy")).isEqualTo(1);
    }

    @Test
    void reduceWithUntypedEmptyListAccumulatorInfersConcreteElementType() {
        assertThat(EmptyLiteralInference.reduceDictToNamedValuesSize(java.util.Map.of("a", 1, "b", 2))).isEqualTo(2);
    }

    @Test
    void reduceWithUntypedEmptyListAccumulatorInfersConcreteRecordTypeFromKeyValuePairs() {
        assertThat(EmptyLiteralInference.reduceDictToParseCasesSize(java.util.Map.of("a", 1, "b", 2))).isEqualTo(2);
    }

    @Test
    void reduceWithUntypedEmptyListAccumulatorSupportsSwappedKeyValueNames() {
        assertThat(EmptyLiteralInference.reduceDictToFormatCasesSize(java.util.Map.of("a", 1, "b", 2))).isEqualTo(2);
    }

    @Test
    void twoIndependentlyInferredReduceListsCanBeConcatenated() {
        assertThat(EmptyLiteralInference.reduceDictToTwoListsAndConcatSizes(java.util.Map.of("a", 1, "b", 2))).isEqualTo(4);
    }

    @Test
    void toSeqUsingEndLiteralCompilesAndBuildsSequence() {
        var seq = EmptyLiteralInference.toSeqUsingEndLiteral(List.of(4, 5));
        assertThat(EmptyLiteralInference.seqEndHeadOrDefault(seq, -1)).isEqualTo(5);
    }

    @Test
    void concatInferredSubtypeListsCanFlowIntoParentTypedSink() {
        var batch = EmptyLiteralInference.concatInferredParentSubtypeLists(List.of("ok", "err"));
        assertThat(batch.outcomes()).hasSize(4);
        assertThat(batch.outcomes()).allMatch(outcome -> outcome instanceof EmptyLiteralInference.Outcome);
    }

    @Test
    void threeInferredTestCaseListsCanBeConcatenatedIntoTestFile() {
        assertThat(EmptyLiteralInference.buildTestFileFromThreeInferredTestCaseLists()).isEqualTo(3);
    }
}
