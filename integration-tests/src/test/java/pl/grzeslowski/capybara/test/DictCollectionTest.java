package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

import java.util.HashMap;
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

    @Test
    void append() {
        var input = new HashMap<>(EXPECTED);

        var result = DictCollection.append(input);

        assertThat(result).isEqualTo(Map.of(
                "one", 1,
                "two", 2,
                "three", 3,
                "four", 4
        ));
        assertThat(input).isEqualTo(EXPECTED);
    }

    @Test
    void appendDict() {
        var left = new HashMap<>(Map.of("one", 1, "two", 2));
        var right = new HashMap<>(Map.of("three", 3, "four", 4));

        var result = DictCollection.appendDict(left, right);

        assertThat(result).isEqualTo(Map.of(
                "one", 1,
                "two", 2,
                "three", 3,
                "four", 4
        ));
        assertThat(left).isEqualTo(Map.of("one", 1, "two", 2));
        assertThat(right).isEqualTo(Map.of("three", 3, "four", 4));
    }
}
