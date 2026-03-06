package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class ListCollectionTest {
    @Test
    void staticList() {
        assertThat(ListCollection.staticList()).isEqualTo(List.of(1, 2, 3));
    }

    @Test
    void emptyStaticList() {
        assertThat(ListCollection.emptyStaticList()).isEmpty();
    }
}
