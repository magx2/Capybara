package dev.capylang.test;

import capy.lang.Result;
import org.junit.jupiter.api.Test;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Optional;
import java.util.Set;

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
    void reduceWithInlineSuccessEmptyListInfersResultListType() {
        var result = EmptyLiteralInference.reduceListToSuccessValues(List.of(1, 2, 3));
        assertThat(result).isInstanceOf(Result.Success.class);
        assertThat(((Result.Success<List<Integer>>) result).value()).containsExactly(1, 2, 3);
    }

    @Test
    void reduceWithInlineSuccessEmptySetInfersResultSetType() {
        var result = EmptyLiteralInference.reduceSetToSuccessValues(Set.of("a", "b"));
        assertThat(result).isInstanceOf(Result.Success.class);
        assertThat(((Result.Success<Set<String>>) result).value()).containsExactlyInAnyOrder("a", "b");
    }

    @Test
    void reduceWithInlineSuccessEmptyDictInfersResultDictType() {
        var values = new LinkedHashMap<String, Integer>();
        values.put("a", 1);
        values.put("b", 2);

        var result = EmptyLiteralInference.reduceDictToSuccessValues(values);

        assertThat(result).isInstanceOf(Result.Success.class);
        assertThat(((Result.Success<java.util.Map<String, Integer>>) result).value()).containsEntry("a", 1).containsEntry("b", 2);
    }

    @Test
    void reduceWithInlineSomeEmptyListInfersOptionListType() {
        assertThat(EmptyLiteralInference.reduceListToSomeValues(List.of(1, 2, 3)))
                .isEqualTo(Optional.of(List.of(1, 2, 3)));
    }

    @Test
    void reduceWithInlineGenericDataEmptyListInfersBoxListType() {
        assertThat(EmptyLiteralInference.reduceListToBoxValues(List.of(1, 2, 3)).value())
                .containsExactly(1, 2, 3);
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
