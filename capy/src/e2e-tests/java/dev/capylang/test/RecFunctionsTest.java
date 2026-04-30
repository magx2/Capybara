package dev.capylang.test;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class RecFunctionsTest {
    @Test
    void tailRecursiveTopLevelFunctionWorks() {
        assertThat(RecFunctions.sum(5, 0)).isEqualTo(15);
    }

    @Test
    void tailRecursiveLocalFunctionWorks() {
        assertThat(RecFunctions.sumInner(5)).isEqualTo(15);
    }

    @Test
    void tailRecursiveFunctionSupportsLetAndMatchTailPositions() {
        assertThat(RecFunctions.sumWithLet(5, 0)).isEqualTo(15);
        assertThat(RecFunctions.sumWithMatch(5, 0)).isEqualTo(15);
    }

    @Test
    void tailRecursiveFunctionIsLoweredWithoutJavaStackGrowth() {
        assertThat(RecFunctions.countDownLarge(50_000, 0)).isEqualTo(50_000);
    }

    @Test
    void tailRecursiveMatchPreservesOptionAndResultBindings() {
        assertThat(RecFunctions.sumPresentValues(List.of(1, 2, 3), 0)).isEqualTo(6);
        assertThat(RecFunctions.sumLengthsUntilError(List.of("a", "bb", "bad", "cccc"), 0))
                .isEqualTo("error:bad length:3");
        assertThat(RecFunctions.sumLengthsUntilError(List.of("a", "bb", "ccc"), 0)).isEqualTo("ok:6");
    }
}
