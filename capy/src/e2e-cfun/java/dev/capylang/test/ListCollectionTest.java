package dev.capylang.test;

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

    @Test
    void any() {
        assertThat(ListCollection.any(List.of(1, 2, 3))).isTrue();
        assertThat(ListCollection.any(List.of(0, 1, 2))).isFalse();
    }

    @Test
    void anyEmpty() {
        assertThat(ListCollection.anyEmpty(List.of())).isFalse();
    }

    @Test
    void all() {
        assertThat(ListCollection.all(List.of(1, 2, 3))).isTrue();
        assertThat(ListCollection.all(List.of(0, 1, 2))).isFalse();
    }

    @Test
    void allEmpty() {
        assertThat(ListCollection.allEmpty(List.of())).isTrue();
    }

    @Test
    void isEmpty() {
        assertThat(ListCollection.isEmpty(List.of())).isTrue();
        assertThat(ListCollection.isEmpty(List.of(1, 2, 3))).isFalse();
    }

    @Test
    void notIsEmpty() {
        assertThat(ListCollection.notIsEmpty(List.of())).isFalse();
        assertThat(ListCollection.notIsEmpty(List.of(1, 2, 3))).isTrue();
    }

    @Test
    void notNestedIsEmpty() {
        assertThat(ListCollection.notNestedIsEmpty(new ListCollection.ListHolder(List.of()))).isFalse();
        assertThat(ListCollection.notNestedIsEmpty(new ListCollection.ListHolder(List.of(1, 2, 3)))).isTrue();
    }

    @Test
    void size() {
        assertThat(ListCollection.size(List.of(1, 2, 3))).isEqualTo(3);
        assertThat(ListCollection.size(List.of())).isEqualTo(0);
    }

    @Test
    void letNamedList() {
        assertThat(ListCollection.letNamedList()).isEqualTo(4);
    }

    @Test
    void iteratorSum() {
        assertThat(ListCollection.iteratorSum(List.of(1, 2, 3))).isEqualTo(6);
        assertThat(ListCollection.iteratorSum(List.of())).isZero();
    }

    @Test
    void iteratorEmpty() {
        assertThat(ListCollection.iteratorEmpty(List.of())).isTrue();
        assertThat(ListCollection.iteratorEmpty(List.of(1))).isFalse();
    }

    @Test
    void iteratorMapSum() {
        assertThat(ListCollection.iteratorMapSum(List.of(1, 2, 3))).isEqualTo(12);
    }
}
