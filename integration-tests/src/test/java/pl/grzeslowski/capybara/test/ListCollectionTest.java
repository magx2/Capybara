package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;
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

    @Test
    void staticListWithTrailingComma() {
        assertThat(ListCollection.staticListWithTrailingComma()).isEqualTo(List.of(1, 2, 3));
    }

    @Test
    void append() {
        var input = new ArrayList<>(List.of(1, 2, 3));

        var result = ListCollection.append(input);

        assertThat(result).isEqualTo(List.of(1, 2, 3, 5));
        assertThat(input).isEqualTo(List.of(1, 2, 3));
    }

    @Test
    void appendList() {
        var left = new ArrayList<>(List.of(1, 2));
        var right = new ArrayList<>(List.of(3, 4));

        var result = ListCollection.appendList(left, right);

        assertThat(result).isEqualTo(List.of(1, 2, 3, 4));
        assertThat(left).isEqualTo(List.of(1, 2));
        assertThat(right).isEqualTo(List.of(3, 4));
    }

    @Test
    void remove() {
        var input = new ArrayList<>(List.of(1, 2, 2, 3));

        var result = ListCollection.remove(input);

        assertThat(result).isEqualTo(List.of(1, 3));
        assertThat(input).isEqualTo(List.of(1, 2, 2, 3));
    }

    @Test
    void removeList() {
        var left = new ArrayList<>(List.of(1, 2, 2, 3, 4));
        var right = new ArrayList<>(List.of(2, 4));

        var result = ListCollection.removeList(left, right);

        assertThat(result).isEqualTo(List.of(1, 3));
        assertThat(left).isEqualTo(List.of(1, 2, 2, 3, 4));
        assertThat(right).isEqualTo(List.of(2, 4));
    }

    @Test
    void contains() {
        assertThat(ListCollection.contains(List.of(1, 2, 3), 2)).isTrue();
        assertThat(ListCollection.contains(List.of(1, 2, 3), 9)).isFalse();
    }
}
