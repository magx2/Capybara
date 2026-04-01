package dev.capylang.test;

import java.util.Optional;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class MatchWhenTest {
    @Test
    void fooPrefersGuardedCasesBeforeGeneralCase() {
        assertThat(MatchWhen.foo(Optional.of("-"))).isEqualTo("minus");
        assertThat(MatchWhen.foo(Optional.of("+"))).isEqualTo("plus");
        assertThat(MatchWhen.foo(Optional.of("x"))).isEqualTo("other");
        assertThat(MatchWhen.foo(Optional.empty())).isEqualTo("none");
    }

    @Test
    void booPrefersGuardedConstructorPatternCasesBeforeGeneralCase() {
        assertThat(MatchWhen.boo(Optional.of(1))).isEqualTo("one");
        assertThat(MatchWhen.boo(Optional.of(2))).isEqualTo("two");
        assertThat(MatchWhen.boo(Optional.of(7))).isEqualTo("7");
        assertThat(MatchWhen.boo(Optional.empty())).isEqualTo("none");
    }

    @Test
    void nestedGuardWorksWithConstructorPatternBinding() {
        assertThat(MatchWhen.nestedGuard(Optional.of(11))).isEqualTo("big");
        assertThat(MatchWhen.nestedGuard(Optional.of(5))).isEqualTo("5");
        assertThat(MatchWhen.nestedGuard(Optional.empty())).isEqualTo("none");
    }

    @Test
    void signPrefersGuardedDataCasesBeforeGeneralCase() {
        assertThat(MatchWhen.sign(new MatchWhen.Sign("-", 20, true))).isEqualTo("heavy minus");
        assertThat(MatchWhen.sign(new MatchWhen.Sign("+", 20, true))).isEqualTo("heavy plus");
        assertThat(MatchWhen.sign(new MatchWhen.Sign("-", 5, true))).isEqualTo("minus");
        assertThat(MatchWhen.sign(new MatchWhen.Sign("+", 5, true))).isEqualTo("plus");
        assertThat(MatchWhen.sign(new MatchWhen.Sign("x", 5, true))).isEqualTo("other");
        assertThat(MatchWhen.sign(new MatchWhen.Sign("-", 20, false))).isEqualTo("minus");
    }

    @Test
    void letterSupportsGuardsForUserDefinedTypeCases() {
        assertThat(MatchWhen.letter(MatchWhen.Vowel.INSTANCE)).isEqualTo("vowel");
        assertThat(MatchWhen.letter(MatchWhen.Consonant.INSTANCE)).isEqualTo("hard");
    }

    @Test
    void rangeSupportsComplexGuardExpressionsOnCustomData() {
        assertThat(MatchWhen.rangeLabel(new MatchWhen.Range(1, 5, 1, true))).isEqualTo("ascending");
        assertThat(MatchWhen.rangeLabel(new MatchWhen.Range(5, 1, -1, true))).isEqualTo("descending");
        assertThat(MatchWhen.rangeLabel(new MatchWhen.Range(1, 5, 0, false))).isEqualTo("disabled 1");
        assertThat(MatchWhen.rangeLabel(new MatchWhen.Range(1, 5, 0, true))).isEqualTo("stuck");
        assertThat(MatchWhen.rangeLabel(new MatchWhen.Range(1, 5, 2, true))).isEqualTo("other");
    }
}
