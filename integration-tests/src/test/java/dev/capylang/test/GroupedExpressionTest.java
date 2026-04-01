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
}
