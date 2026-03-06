package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

class DictCollectionTest {
    private static final Map<String, Integer> EXPECTED = Map.of(
            "one", 1,
            "two", 2,
            "three", 3
    );

    @Test
    void staticDict() {
        assertThat(DictCollection.staticDict()).isEqualTo(EXPECTED);
    }

    @Test
    void staticDictWithTrailingComma() {
        assertThat(DictCollection.staticDictWithTrailingComma()).isEqualTo(EXPECTED);
    }
}
