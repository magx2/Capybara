package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;

class SetCollectionTest {
    @Test
    void staticSet() {
        assertThat(SetCollection.staticSet()).isEqualTo(Set.of(1, 2, 3));
    }

    @Test
    void emptyStaticSet() {
        assertThat(SetCollection.emptyStaticSet()).isEmpty();
    }

    @Test
    void staticSetWithTrailingComma() {
        assertThat(SetCollection.staticSetWithTrailingComma()).isEqualTo(Set.of(1, 2, 3));
    }
}
