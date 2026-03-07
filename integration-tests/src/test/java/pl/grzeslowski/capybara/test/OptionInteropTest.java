package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

class OptionInteropTest {
    @Test
    void someMapsToOptionalOf() {
        Optional<Integer> value = OptionInterop.someValue(7);

        assertThat(value).isPresent();
        assertThat(value).contains(7);
    }

    @Test
    void noneMapsToOptionalEmpty() {
        Optional<Integer> value = OptionInterop.noneValue();

        assertThat(value).isEmpty();
    }
}
