package dev.capylang.test;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatCode;

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
        assertThat(RecFunctions.factorialLarge(5, 1)).isEqualTo(120);
        assertThatCode(() -> RecFunctions.factorialLarge(50_000, 1)).doesNotThrowAnyException();
    }

    @Test
    void unmarkedRecursiveFunctionsStillCompileAndRun() {
        assertThat(RecFunctions.unmarkedTailSum(5, 0)).isEqualTo(15);
        assertThat(RecFunctions.unmarkedTailFactorialLarge(5, 1)).isEqualTo(120);
        assertThatCode(() -> RecFunctions.unmarkedTailFactorialLarge(50_000, 1)).doesNotThrowAnyException();
        assertThat(RecFunctions.unmarkedLocalTailSum(5)).isEqualTo(15);
        assertThat(RecFunctions.unmarkedLocalTailFactorialLarge(5)).isEqualTo(120);
        assertThatCode(() -> RecFunctions.unmarkedLocalTailFactorialLarge(50_000)).doesNotThrowAnyException();
        assertThat(RecFunctions.unmarkedNonTailSum(5)).isEqualTo(15);
    }

    @Test
    void tailRecursiveMatchPreservesOptionAndResultBindings() {
        assertThat(RecFunctions.sumPresentValues(List.of(1, 2, 3), 0)).isEqualTo(6);
        assertThat(RecFunctions.sumLengthsUntilError(List.of("a", "bb", "bad", "cccc"), 0))
                .isEqualTo("error:bad length:3");
        assertThat(RecFunctions.sumLengthsUntilError(List.of("a", "bb", "ccc"), 0)).isEqualTo("ok:6");
    }
}
