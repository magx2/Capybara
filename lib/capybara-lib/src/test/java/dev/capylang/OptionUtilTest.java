package dev.capylang;

import capy.lang.Option;
import org.junit.jupiter.api.Test;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

class OptionUtilTest {
    @Test
    void shouldConvertJavaOptionalPresentToCapySome() {
        var capyOption = OptionUtil.toCapyOption(Optional.of("capy"));

        assertThat(capyOption).isEqualTo(new Option.Some<>("capy"));
    }

    @Test
    void shouldConvertJavaOptionalEmptyToCapyNone() {
        var capyOption = OptionUtil.toCapyOption(Optional.empty());

        assertThat(capyOption).isEqualTo(none());
    }

    @Test
    void shouldConvertCapySomeToJavaOptionalPresent() {
        var javaOptional = OptionUtil.toJavaOptional(new Option.Some<>(42));

        assertThat(javaOptional).hasValue(42);
    }

    @Test
    void shouldConvertCapyNoneToJavaOptionalEmpty() {
        var javaOptional = OptionUtil.toJavaOptional(none());

        assertThat(javaOptional).isEmpty();
    }

    @SuppressWarnings("unchecked")
    private static <T> Option<T> none() {
        return (Option<T>) Option.None.INSTANCE;
    }
}
