package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

class DictCollectionTest {
    @Test
    void staticDict() {
        assertThat(DictCollection.staticDict()).isEqualTo(Map.of(
                "one", 1,
                "two", 2,
                "three", 3
        ));
    }
}
