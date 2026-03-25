package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ResultInfixGenericTest {
    @Test
    void applySuccess() {
        var input = new ResultInfixGeneric.Ok<>(7);
        var result = ResultInfixGeneric.applyMapper(input, ResultInfixGeneric::mapToString);
        assertThat(result).isInstanceOf(ResultInfixGeneric.Ok.class);
        assertThat(((ResultInfixGeneric.Ok<String>) result).value()).isEqualTo("v=7");
    }

    @Test
    void applyFailMapper() {
        var input = new ResultInfixGeneric.Ok<>(3);
        var result = ResultInfixGeneric.applyMapper(input, ResultInfixGeneric::mapToFail);
        assertThat(result).isInstanceOf(ResultInfixGeneric.Fail.class);
        assertThat(((ResultInfixGeneric.Fail) result).message()).isEqualTo("bad:3");
    }

    @Test
    void propagateFail() {
        var input = new ResultInfixGeneric.Fail("boom");
        var result = ResultInfixGeneric.propagateFail(input, ResultInfixGeneric::mapToString);
        assertThat(result).isInstanceOf(ResultInfixGeneric.Fail.class);
        assertThat(((ResultInfixGeneric.Fail) result).message()).isEqualTo("boom");
    }

    @Test
    void flatMapSuccess() {
        var input = new ResultInfixGeneric.Ok<>(7);
        var result = ResultInfixGeneric.applyFlatMapper(input, ResultInfixGeneric::mapToNestedOk);
        assertThat(result).isInstanceOf(ResultInfixGeneric.Ok.class);
        assertThat(((ResultInfixGeneric.Ok<?>) result).value()).isEqualTo("mapped_7");
    }

    @Test
    void flatMapNestedFail() {
        var input = new ResultInfixGeneric.Ok<>(3);
        var result = ResultInfixGeneric.applyFlatMapper(input, ResultInfixGeneric::mapToNestedFail);
        assertThat(result).isInstanceOf(ResultInfixGeneric.Fail.class);
        assertThat(((ResultInfixGeneric.Fail) result).message()).isEqualTo("nested_bad:3");
    }

    @Test
    void flatMapPropagatesOuterFail() {
        var input = new ResultInfixGeneric.Fail("boom");
        var result = ResultInfixGeneric.applyFlatMapper(input, ResultInfixGeneric::mapToNestedOk);
        assertThat(result).isInstanceOf(ResultInfixGeneric.Fail.class);
        assertThat(((ResultInfixGeneric.Fail) result).message()).isEqualTo("boom");
    }

}
