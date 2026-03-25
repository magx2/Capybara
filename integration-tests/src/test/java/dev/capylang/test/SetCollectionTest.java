package dev.capylang.test;

import org.junit.jupiter.api.Test;

import java.util.HashSet;
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
    void emptyStaticSetWithSpaces() {
        assertThat(SetCollection.emptyStaticSetWithSpaces()).isEmpty();
    }

    @Test
    void staticSetWithTrailingComma() {
        assertThat(SetCollection.staticSetWithTrailingComma()).isEqualTo(Set.of(1, 2, 3));
    }

    @Test
    void append() {
        var input = new HashSet<>(Set.of(1, 2, 3));

        var result = SetCollection.append(input);

        assertThat(result).isEqualTo(Set.of(1, 2, 3, 5));
        assertThat(input).isEqualTo(Set.of(1, 2, 3));
    }

    @Test
    void appendSet() {
        var left = new HashSet<>(Set.of(1, 2));
        var right = new HashSet<>(Set.of(3, 4));

        var result = SetCollection.appendSet(left, right);

        assertThat(result).isEqualTo(Set.of(1, 2, 3, 4));
        assertThat(left).isEqualTo(Set.of(1, 2));
        assertThat(right).isEqualTo(Set.of(3, 4));
    }

    @Test
    void remove() {
        var input = new HashSet<>(Set.of(1, 2, 3));

        var result = SetCollection.remove(input);

        assertThat(result).isEqualTo(Set.of(1, 3));
        assertThat(input).isEqualTo(Set.of(1, 2, 3));
    }

    @Test
    void removeSet() {
        var left = new HashSet<>(Set.of(1, 2, 3, 4));
        var right = new HashSet<>(Set.of(2, 4));

        var result = SetCollection.removeSet(left, right);

        assertThat(result).isEqualTo(Set.of(1, 3));
        assertThat(left).isEqualTo(Set.of(1, 2, 3, 4));
        assertThat(right).isEqualTo(Set.of(2, 4));
    }

    @Test
    void contains() {
        assertThat(SetCollection.contains(Set.of(1, 2, 3), 2)).isTrue();
        assertThat(SetCollection.contains(Set.of(1, 2, 3), 9)).isFalse();
    }

    @Test
    void size() {
        assertThat(SetCollection.size(Set.of(1, 2, 3))).isEqualTo(3);
        assertThat(SetCollection.size(Set.of())).isEqualTo(0);
    }
}
