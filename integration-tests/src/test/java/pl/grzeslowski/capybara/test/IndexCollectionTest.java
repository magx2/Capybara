package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

class IndexCollectionTest {
    @Test
    void listIndex5() {
        assertThat(IndexCollection.listIndex5(List.of(10, 20, 30, 40, 50, 60))).isEqualTo(Optional.of(60));
        assertThat(IndexCollection.listIndex5(List.of(10, 20))).isEmpty();
    }

    @Test
    void listIndexMinus2() {
        assertThat(IndexCollection.listIndexMinus2(List.of(10, 20, 30, 40))).isEqualTo(Optional.of(30));
        assertThat(IndexCollection.listIndexMinus2(List.of(10))).isEmpty();
    }

    @Test
    void stringIndex5() {
        assertThat(IndexCollection.stringIndex5("capybar")).isEqualTo(Optional.of("a"));
        assertThat(IndexCollection.stringIndex5("abc")).isEmpty();
    }

    @Test
    void stringIndexMinus2() {
        assertThat(IndexCollection.stringIndexMinus2("capybara")).isEqualTo(Optional.of("r"));
        assertThat(IndexCollection.stringIndexMinus2("a")).isEmpty();
    }
}
