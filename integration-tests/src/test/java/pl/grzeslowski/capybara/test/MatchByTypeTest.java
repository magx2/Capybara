package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;

class MatchByTypeTest {
    @Test
    void matchString() {
        assertThat(MatchByType.matchString("abc")).isEqualTo("s:abc");
        assertThat(MatchByType.matchString(12)).isEqualTo("i:12");
    }

    @Test
    void matchInt() {
        assertThat(MatchByType.matchInt(9)).isEqualTo(10);
        assertThat(MatchByType.matchInt("x")).isEqualTo(0);
    }

    @Test
    void dataTypeMatchesOnlyDataAndType() {
        assertThat(MatchByType.isData(1)).isFalse();
        assertThat(MatchByType.isData(1L)).isFalse();
        assertThat(MatchByType.isData(true)).isFalse();
        assertThat(MatchByType.isData(1.5)).isFalse();
        assertThat(MatchByType.isData(1.5f)).isFalse();
        assertThat(MatchByType.isData(List.of(1, 2))).isFalse();
        assertThat(MatchByType.isData(Set.of(1, 2))).isFalse();
        assertThat(MatchByType.isData(Map.of("a", 1))).isFalse();

        assertThat(MatchByType.isData(new MatchByType.Person("Tom"))).isTrue();
        assertThat(MatchByType.isDataFromEntity(new MatchByType.Person("Jerry"))).isTrue();
    }

    @Test
    void matchCaseCanUseMultiplePatterns() {
        assertThat(MatchByType.classifyJsonChar("{")).isEqualTo("object");
        assertThat(MatchByType.classifyJsonChar("\"")).isEqualTo("string");
        assertThat(MatchByType.classifyJsonChar("-")).isEqualTo("number");
        assertThat(MatchByType.classifyJsonChar("7")).isEqualTo("number");
        assertThat(MatchByType.classifyJsonChar("x")).isEqualTo("other");
    }

    @Test
    void wildcardCaseCanBindMatchedValue() {
        assertThat(MatchByType.classifyJsonCharWithNamedWildcard("-")).isEqualTo("minus");
        assertThat(MatchByType.classifyJsonCharWithNamedWildcard("7")).isEqualTo("number");
        assertThat(MatchByType.classifyJsonCharWithNamedWildcard("x")).isEqualTo("unknown: x");
    }
}
