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

    @Test
    void startsWithMethod() {
        assertThat(StringCollection.startsWithMethod("capybara", "cap")).isTrue();
        assertThat(StringCollection.startsWithMethod("capybara", "bara")).isFalse();
    }

    @Test
    void endWithMethod() {
        assertThat(StringCollection.endWithMethod("capybara", "bara")).isTrue();
        assertThat(StringCollection.endWithMethod("capybara", "cap")).isFalse();
    }

    @Test
    void trimMethod() {
        assertThat(StringCollection.trimMethod("  capybara  ")).isEqualTo("capybara");
        assertThat(StringCollection.trimMethod("capybara")).isEqualTo("capybara");
    }

    @Test
    void isEmptyMethod() {
        assertThat(StringCollection.isEmptyMethod("")).isTrue();
        assertThat(StringCollection.isEmptyMethod("capybara")).isFalse();
    }
}
