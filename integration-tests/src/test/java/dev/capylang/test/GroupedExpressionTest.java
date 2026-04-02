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
}
