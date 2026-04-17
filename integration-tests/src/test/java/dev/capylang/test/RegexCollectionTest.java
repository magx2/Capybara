package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class RegexCollectionTest {
    @Test
    void matchesNamed() {
        assertThat(RegexCollection.matchesNamed("xxfooyy")).isTrue();
        assertThat(RegexCollection.matchesNamed("xxbaryy")).isFalse();
    }

    @Test
    void matchesAliasQuestion() {
        assertThat(RegexCollection.matchesAlias("xxfooyy")).isTrue();
        assertThat(RegexCollection.matchesAlias("xxbaryy")).isFalse();
    }

    @Test
    void findNamedAndAlias() {
        assertThat(RegexCollection.findNamed("xxfooyy")).isTrue();
        assertThat(RegexCollection.findNamed("xxbaryy")).isFalse();
        assertThat(RegexCollection.findAlias("xxfooyy")).isTrue();
        assertThat(RegexCollection.findAlias("xxbaryy")).isFalse();
    }

    @Test
    void findAllNamedAndAlias() {
        assertThat(RegexCollection.findAllNamedCount("xxfooyy")).isEqualTo(1);
        assertThat(RegexCollection.findAllNamedCount("xxbaryy")).isZero();
        assertThat(RegexCollection.findAllAliasCount("xxfooyy")).isEqualTo(1);
        assertThat(RegexCollection.findAllAliasCount("xxbaryy")).isZero();
    }

    @Test
    void replaceNamedAndAlias() {
        assertThat(RegexCollection.replaceNamed("a1b11")).isEqualTo("a#b##");
        assertThat(RegexCollection.replaceAlias("a1b11")).isEqualTo("a#b##");
    }

    @Test
    void splitNamedAndAlias() {
        assertThat(RegexCollection.splitNamed("a,b,c")).containsExactly("a,b,c");
        assertThat(RegexCollection.splitAlias("a,b,c")).containsExactly("a,b,c");
    }

    @Test
    void escapedSlashInLiteral() {
        assertThat(RegexCollection.escapedSlashMatch()).isTrue();
    }

    @Test
    void matchGroupsApi() {
        assertThat(RegexCollection.firstGroupIsPattern("xxfooyy")).isTrue();
        assertThat(RegexCollection.firstGroupIsPattern("xxbaryy")).isFalse();
        assertThat(RegexCollection.groupsSize("xxfooyy")).isEqualTo(1);
        assertThat(RegexCollection.groupsSize("xxbaryy")).isZero();
    }
}
