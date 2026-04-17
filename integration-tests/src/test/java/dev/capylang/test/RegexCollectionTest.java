package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class RegexCollectionTest {
    @Test
    void tildeOperator() {
        assertThat(RegexCollection.hasFoo("xxfooyy")).isTrue();
        assertThat(RegexCollection.hasFoo("xxbaryy")).isFalse();
    }

    @Test
    void tildeOperatorWithFlagsLiteralParses() {
        assertThat(RegexCollection.hasFooWithFlag("xxFooyy")).isTrue();
        assertThat(RegexCollection.hasFooWithFlag("xxbaryy")).isFalse();
    }

    @Test
    void tildeTildeOperator() {
        assertThat(RegexCollection.allFoo("xxfooyy")).containsExactly("foo");
        assertThat(RegexCollection.allFoo("xxbaryy")).isEmpty();
    }

    @Test
    void tildeGreaterOperator() {
        assertThat(RegexCollection.redactOnes("a1b11")).isEqualTo("a#b##");
    }

    @Test
    void slashGreaterOperator() {
        assertThat(RegexCollection.splitCsv("a,b,c")).containsExactly("a,b,c");
    }

    @Test
    void escapedSlashInLiteral() {
        assertThat(RegexCollection.escapedSlashMatch()).isTrue();
    }
}
