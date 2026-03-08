package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

class OptionToOptionalTest {
    @Test
    void stringOptionHandlesSomeAndNone() {
        assertThat(OptionToOptional.stringOption(Optional.of("abc"))).isEqualTo("abc");
        assertThat(OptionToOptional.stringOption(Optional.empty())).isEqualTo("<empty>");
    }

    @Test
    void intOptionHandlesSomeAndNone() {
        assertThat(OptionToOptional.intOption(Optional.of(7))).isEqualTo(7);
        assertThat(OptionToOptional.intOption(Optional.empty())).isEqualTo(-1);
    }

    @Test
    void boolOptionHandlesSomeAndNone() {
        assertThat(OptionToOptional.boolOption(Optional.of(true))).isTrue();
        assertThat(OptionToOptional.boolOption(Optional.empty())).isFalse();
    }

    @Test
    void optionToMessageHandlesSomeAndNone() {
        assertThat(OptionToOptional.optionToMessage(Optional.of("abc"))).isEqualTo("value=abc");
        assertThat(OptionToOptional.optionToMessage(Optional.empty())).isEqualTo("missing");
    }

    @Test
    void intOptionToMessageHandlesSomeAndNone() {
        assertThat(OptionToOptional.intOptionToMessage(Optional.of(10))).isEqualTo("value=10");
        assertThat(OptionToOptional.intOptionToMessage(Optional.empty())).isEqualTo("missing");
    }
}
