package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class StringCollectionTest {
    @Test
    void contains() {
        assertThat(StringCollection.contains("capybara", "pyb")).isTrue();
        assertThat(StringCollection.contains("capybara", "dog")).isFalse();
    }

    @Test
    void containsMethod() {
        assertThat(StringCollection.containsMethod("capybara", "pyb")).isTrue();
        assertThat(StringCollection.containsMethod("capybara", "dog")).isFalse();
    }

    @Test
    void containsEmptySubstring() {
        assertThat(StringCollection.contains("capybara", "")).isTrue();
        assertThat(StringCollection.contains("", "")).isTrue();
    }
}
