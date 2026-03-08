package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

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
}
