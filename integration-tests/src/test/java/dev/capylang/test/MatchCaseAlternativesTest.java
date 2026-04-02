package dev.capylang.test;

import capy.lang.Result;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class MatchCaseAlternativesTest {
    @Test
    void digitLabelMatchesAlternativeLiteralPatterns() {
        assertThat(MatchCaseAlternatives.digitLabel("1")).isEqualTo("digit");
        assertThat(MatchCaseAlternatives.digitLabel("7")).isEqualTo("digit");
        assertThat(MatchCaseAlternatives.digitLabel("0")).isEqualTo("zero");
        assertThat(MatchCaseAlternatives.digitLabel("x")).isEqualTo("other");
    }

    @Test
    void intBucketMatchesAlternativeIntPatterns() {
        assertThat(MatchCaseAlternatives.intBucket(1)).isEqualTo("small");
        assertThat(MatchCaseAlternatives.intBucket(3)).isEqualTo("small");
        assertThat(MatchCaseAlternatives.intBucket(20)).isEqualTo("tens");
        assertThat(MatchCaseAlternatives.intBucket(99)).isEqualTo("other");
    }

    @Test
    void longBucketMatchesAlternativeLongPatterns() {
        assertThat(MatchCaseAlternatives.longBucket(1L)).isEqualTo("small");
        assertThat(MatchCaseAlternatives.longBucket(3L)).isEqualTo("small");
        assertThat(MatchCaseAlternatives.longBucket(200L)).isEqualTo("hundreds");
        assertThat(MatchCaseAlternatives.longBucket(999L)).isEqualTo("other");
    }

    @Test
    void signNameSupportsAlternativeConstructorPatterns() {
        assertThat(MatchCaseAlternatives.signName(new MatchCaseAlternatives.Plus("+"))).isEqualTo("non-zero");
        assertThat(MatchCaseAlternatives.signName(new MatchCaseAlternatives.Minus("-"))).isEqualTo("non-zero");
        assertThat(MatchCaseAlternatives.signName(new MatchCaseAlternatives.Zero("0"))).isEqualTo("zero");
    }
}
