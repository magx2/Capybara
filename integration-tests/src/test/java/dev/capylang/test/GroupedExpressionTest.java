package dev.capylang.test;

import capy.lang.Result;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class GroupedExpressionTest {
    @Test
    void nestedPipeInMatchBranch() {
        var result = GroupedExpression.nestedPipeInMatchBranch(1);
        assertThat(result).isInstanceOf(Result.Success.class);
        assertThat(((Result.Success<Integer>) result).value()).isEqualTo(3);
    }

    @Test
    void groupedGuardedMatchIsExhaustiveForGroupedFirstBranch() {
        assertThat(GroupedExpression.groupedGuardedMatchIsExhaustive("-+abc")).isEqualTo(new Result.Success<>("plus:pre:tail:build"));
        var bad = GroupedExpression.groupedGuardedMatchIsExhaustive("-abc");
        assertThat(bad).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error<String>) bad).ex().getMessage()).isEqualTo("bad:a");
        assertThat(GroupedExpression.groupedGuardedMatchIsExhaustive("+abc")).isEqualTo(new Result.Success<>("plus:build"));
        assertThat(GroupedExpression.groupedGuardedMatchIsExhaustive("xabc")).isEqualTo(new Result.Success<>("other:x"));
        assertThat(GroupedExpression.groupedGuardedMatchIsExhaustive("")).isEqualTo(new Result.Success<>("none"));
    }

    @Test
    void semverStyleMixedCasePipeBodies() {
        assertThat(GroupedExpression.semverStyleMixedCasePipeBodies("-+meta"))
                .isEqualTo(new Result.Success<>("1.2.3-rc1+001"));
        assertThat(GroupedExpression.semverStyleMixedCasePipeBodies("+meta"))
                .isEqualTo(new Result.Success<>("1.2.3+001"));
        var bad = GroupedExpression.semverStyleMixedCasePipeBodies("-meta");
        assertThat(bad).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error<String>) bad).ex().getMessage()).isEqualTo("bad:m");
        var unexpected = GroupedExpression.semverStyleMixedCasePipeBodies("xmeta");
        assertThat(unexpected).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error<String>) unexpected).ex().getMessage()).isEqualTo("unexpected:x");
        assertThat(GroupedExpression.semverStyleMixedCasePipeBodies(""))
                .isEqualTo(new Result.Success<>("1.2.3"));
    }

    @Test
    void innerUngroupedPipeInMatchInsideLambda() {
        assertThat(GroupedExpression.innerUngroupedPipeInMatchInsideLambda("-+meta"))
                .isEqualTo(new Result.Success<>("1.2.3-rc1+001"));
        assertThat(GroupedExpression.innerUngroupedPipeInMatchInsideLambda("+meta"))
                .isEqualTo(new Result.Success<>("1.2.3+001"));
        var bad = GroupedExpression.innerUngroupedPipeInMatchInsideLambda("-meta");
        assertThat(bad).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error<String>) bad).ex().getMessage()).isEqualTo("bad:m");
        var unexpected = GroupedExpression.innerUngroupedPipeInMatchInsideLambda("xmeta");
        assertThat(unexpected).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error<String>) unexpected).ex().getMessage()).isEqualTo("unexpected:x");
        assertThat(GroupedExpression.innerUngroupedPipeInMatchInsideLambda(""))
                .isEqualTo(new Result.Success<>("1.2.3"));
    }

    @Test
    void nestedResultErrorMatchCase() {
        assertThat(GroupedExpression.nestedResultErrorMatchCase("x"))
                .isEqualTo(new Result.Success<>(1));

        var ended = GroupedExpression.nestedResultErrorMatchCase("a");
        assertThat(ended).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error<Integer>) ended).ex().getMessage()).isEqualTo("bad:a:end");

        var tail = GroupedExpression.nestedResultErrorMatchCase("abc");
        assertThat(tail).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error<Integer>) tail).ex().getMessage()).isEqualTo("bad:a:bc");

        var empty = GroupedExpression.nestedResultErrorMatchCase("");
        assertThat(empty).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error<Integer>) empty).ex().getMessage()).isEqualTo("empty");
    }
}

