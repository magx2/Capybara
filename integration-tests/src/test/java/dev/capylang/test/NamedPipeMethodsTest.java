package dev.capylang.test;

import capy.lang.Result;
import dev.capylang.CapybaraException;
import org.junit.jupiter.api.Test;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;

class NamedPipeMethodsTest {
    @Test
    void listAliasesMatchOperators() {
        var values = List.of(-1, 0, 1, 2);

        assertThat(NamedPipeMethods.listMapNamed(values)).isEqualTo(NamedPipeMethods.listMapOperator(values));
        assertThat(NamedPipeMethods.listFilterNamed(values)).isEqualTo(NamedPipeMethods.listFilterOperator(values));
        assertThat(NamedPipeMethods.listFlatMapNamed(List.of(1, 2))).isEqualTo(NamedPipeMethods.listFlatMapOperator(List.of(1, 2)));
        assertThat(NamedPipeMethods.listReduceNamed(values)).isEqualTo(NamedPipeMethods.listReduceOperator(values));
        assertThat(NamedPipeMethods.listReduceLeftNamed(values)).isEqualTo(NamedPipeMethods.listReduceLeftSymbolic(values));
    }

    @Test
    void setAliasesMatchOperators() {
        var values = Set.of(-1, 0, 1, 2);

        assertThat(NamedPipeMethods.setMapNamed(values)).isEqualTo(NamedPipeMethods.setMapOperator(values));
        assertThat(NamedPipeMethods.setFilterNamed(values)).isEqualTo(NamedPipeMethods.setFilterOperator(values));
        assertThat(NamedPipeMethods.setFlatMapNamed(Set.of(1, 2))).isEqualTo(NamedPipeMethods.setFlatMapOperator(Set.of(1, 2)));
        assertThat(NamedPipeMethods.setReduceNamed(values)).isEqualTo(NamedPipeMethods.setReduceOperator(values));
        assertThat(NamedPipeMethods.setReduceLeftNamed(values)).isEqualTo(NamedPipeMethods.setReduceLeftSymbolic(values));
    }

    @Test
    void dictAliasesMatchOperators() {
        var values = new LinkedHashMap<String, Integer>();
        values.put("keep", 1);
        values.put("drop", 2);
        values.put("last", 11);

        assertThat(NamedPipeMethods.dictMapNamed(values)).isEqualTo(NamedPipeMethods.dictMapOperator(values));
        assertThat(NamedPipeMethods.dictFilterNamed(values)).isEqualTo(NamedPipeMethods.dictFilterOperator(values));
        assertThat(NamedPipeMethods.dictFlatMapNamed(values)).isEqualTo(NamedPipeMethods.dictFlatMapOperator(values));
        assertThat(NamedPipeMethods.dictReduceNamed(values)).isEqualTo(NamedPipeMethods.dictReduceOperator(values));
        assertThat(NamedPipeMethods.dictReduceLeftNamed(values)).isEqualTo(NamedPipeMethods.dictReduceLeftSymbolic(values));
    }

    @Test
    void seqAliasesProduceExpectedResults() {
        var values = List.of(1, 2, 3);

        assertThat(NamedPipeMethods.seqMapNamed(values)).isEqualTo(NamedPipeMethods.seqMapOperator(values));
        assertThat(NamedPipeMethods.seqFlatMapNamed(values)).isEqualTo(NamedPipeMethods.seqFlatMapOperator(values));
        assertThat(NamedPipeMethods.seqReduceNamed(values)).isEqualTo(6);
        assertThat(NamedPipeMethods.seqReduceLeftNamed(values)).isEqualTo(6);
    }

    @Test
    void stringFilterAliasesMatchExpectedResults() {
        assertThat(NamedPipeMethods.stringFilterNamed("abc")).isEqualTo(List.of("a", "c"));
        assertThat(NamedPipeMethods.stringFilterNamed("abc")).isEqualTo(NamedPipeMethods.stringFilterSymbolic("abc"));
    }

    @Test
    void optionAliasesMatchOperatorsAndExpectedValues() {
        assertThat(NamedPipeMethods.optionMapNamed(Optional.of("capy"))).isEqualTo(NamedPipeMethods.optionMapOperator(Optional.of("capy")));
        assertThat(NamedPipeMethods.optionFilterNamed(Optional.of("drop"))).isEqualTo(NamedPipeMethods.optionFilterOperator(Optional.of("drop")));
        assertThat(NamedPipeMethods.optionFlatMapNamed(Optional.of("capy"))).isEqualTo(Optional.of("capy!"));
        assertThat(NamedPipeMethods.optionReduceNamed(Optional.of("capy"))).isEqualTo("start:capy");
        assertThat(NamedPipeMethods.optionReduceNamed(Optional.empty())).isEqualTo("start:");
        assertThat(NamedPipeMethods.optionReduceLeftNamed(Optional.of("capy"))).isEqualTo("start:capy");
    }

    @Test
    void resultAliasesMatchOperatorsAndExpectedValues() {
        var success = new Result.Success<>(3);
        var error = new Result.Error<Integer>(new CapybaraException("boom"));

        assertThat(NamedPipeMethods.resultMapNamed(success)).isEqualTo(NamedPipeMethods.resultMapOperator(success));
        assertThat(NamedPipeMethods.resultFlatMapNamed(success)).isEqualTo(NamedPipeMethods.resultFlatMapOperator(success));
        assertThat(NamedPipeMethods.resultReduceNamed(success)).isEqualTo(NamedPipeMethods.resultReduceOperator(success));
        assertThat(NamedPipeMethods.resultReduceNamed(success)).isEqualTo("ok:3");
        assertThat(NamedPipeMethods.resultReduceNamed(error)).isEqualTo("err:boom");
        assertThat(NamedPipeMethods.resultReduceLeftNamed(success)).isEqualTo("ok:3");
    }
}
