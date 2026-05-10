package dev.capylang.test.collection;

import org.junit.jupiter.api.Test;

import java.util.HashSet;
import java.util.List;
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
    void containsMethod() {
        assertThat(SetCollection.containsMethod(Set.of(1, 2, 3), 2)).isTrue();
        assertThat(SetCollection.containsMethod(Set.of(1, 2, 3), 9)).isFalse();
    }

    @Test
    void subset() {
        assertThat(SetCollection.subsetNamed(Set.of(1, 2), Set.of(1, 2, 3))).isTrue();
        assertThat(SetCollection.subsetNamed(Set.of(1, 4), Set.of(1, 2, 3))).isFalse();
        assertThat(SetCollection.subsetSymbol(Set.of(1, 2), Set.of(1, 2, 3))).isTrue();
        assertThat(SetCollection.subsetSymbol(Set.of(1, 4), Set.of(1, 2, 3))).isFalse();
    }

    @Test
    void properSubset() {
        assertThat(SetCollection.properSubsetNamed(Set.of(1, 2), Set.of(1, 2, 3))).isTrue();
        assertThat(SetCollection.properSubsetNamed(Set.of(1, 2), Set.of(1, 2))).isFalse();
        assertThat(SetCollection.properSubsetSymbol(Set.of(1, 2), Set.of(1, 2, 3))).isTrue();
        assertThat(SetCollection.properSubsetSymbol(Set.of(1, 2), Set.of(1, 2))).isFalse();
    }

    @Test
    void superset() {
        assertThat(SetCollection.supersetNamed(Set.of(1, 2, 3), Set.of(1, 2))).isTrue();
        assertThat(SetCollection.supersetNamed(Set.of(1, 2, 3), Set.of(1, 4))).isFalse();
        assertThat(SetCollection.supersetSymbol(Set.of(1, 2, 3), Set.of(1, 2))).isTrue();
        assertThat(SetCollection.supersetSymbol(Set.of(1, 2, 3), Set.of(1, 4))).isFalse();
    }

    @Test
    void properSuperset() {
        assertThat(SetCollection.properSupersetNamed(Set.of(1, 2, 3), Set.of(1, 2))).isTrue();
        assertThat(SetCollection.properSupersetNamed(Set.of(1, 2), Set.of(1, 2))).isFalse();
        assertThat(SetCollection.properSupersetSymbol(Set.of(1, 2, 3), Set.of(1, 2))).isTrue();
        assertThat(SetCollection.properSupersetSymbol(Set.of(1, 2), Set.of(1, 2))).isFalse();
    }

    @Test
    void union() {
        assertThat(SetCollection.unionNamed(Set.of(1, 2), Set.of(2, 3))).isEqualTo(Set.of(1, 2, 3));
        assertThat(SetCollection.unionSymbol(Set.of(1, 2), Set.of(2, 3))).isEqualTo(Set.of(1, 2, 3));
    }

    @Test
    void intersection() {
        assertThat(SetCollection.intersectionNamed(Set.of(1, 2, 3), Set.of(2, 3, 4))).isEqualTo(Set.of(2, 3));
        assertThat(SetCollection.intersectionSymbol(Set.of(1, 2, 3), Set.of(2, 3, 4))).isEqualTo(Set.of(2, 3));
    }

    @Test
    void setDifferences() {
        assertThat(SetCollection.differenceNamed(Set.of(1, 2, 3), Set.of(2))).isEqualTo(Set.of(1, 3));
    }

    @Test
    void symmetricDifference() {
        assertThat(SetCollection.symmetricDifferenceNamed(Set.of(1, 2, 3), Set.of(2, 3, 4))).isEqualTo(Set.of(1, 4));
        assertThat(SetCollection.symmetricDifferenceSymbol(Set.of(1, 2, 3), Set.of(2, 3, 4))).isEqualTo(Set.of(1, 4));
    }

    @Test
    void cartesianProduct() {
        var expected = Set.of(
                List.of(1, "a"),
                List.of(1, "b"),
                List.of(2, "a"),
                List.of(2, "b")
        );

        assertThat(SetCollection.cartesianProductNamed(Set.of(1, 2), Set.of("a", "b"))).isEqualTo(expected);
        assertThat(SetCollection.cartesianProductSymbol(Set.of(1, 2), Set.of("a", "b"))).isEqualTo(expected);
    }

    @Test
    void powerSet() {
        var expected = Set.of(
                Set.of(),
                Set.of(1),
                Set.of(2),
                Set.of(1, 2)
        );

        assertThat(SetCollection.powerSetNamed(Set.of(1, 2))).isEqualTo(expected);
        assertThat(SetCollection.powerSetSymbol(Set.of(1, 2))).isEqualTo(expected);
    }

    @Test
    void any() {
        assertThat(SetCollection.any(Set.of(1, 2, 3))).isTrue();
        assertThat(SetCollection.any(Set.of(0, 1, 2))).isFalse();
    }

    @Test
    void anyEmpty() {
        assertThat(SetCollection.anyEmpty(Set.of())).isFalse();
    }

    @Test
    void all() {
        assertThat(SetCollection.all(Set.of(1, 2, 3))).isTrue();
        assertThat(SetCollection.all(Set.of(0, 1, 2))).isFalse();
    }

    @Test
    void allEmpty() {
        assertThat(SetCollection.allEmpty(Set.of())).isTrue();
    }

    @Test
    void isEmpty() {
        assertThat(SetCollection.isEmpty(Set.of())).isTrue();
        assertThat(SetCollection.isEmpty(Set.of(1, 2, 3))).isFalse();
    }

    @Test
    void notIsEmpty() {
        assertThat(SetCollection.notIsEmpty(Set.of())).isFalse();
        assertThat(SetCollection.notIsEmpty(Set.of(1, 2, 3))).isTrue();
    }

    @Test
    void notNestedIsEmpty() {
        assertThat(SetCollection.notNestedIsEmpty(new SetCollection.SetHolder(Set.of()))).isFalse();
        assertThat(SetCollection.notNestedIsEmpty(new SetCollection.SetHolder(Set.of(1, 2, 3)))).isTrue();
    }

    @Test
    void size() {
        assertThat(SetCollection.size(Set.of(1, 2, 3))).isEqualTo(3);
        assertThat(SetCollection.size(Set.of())).isEqualTo(0);
    }

    @Test
    void letNamedSet() {
        assertThat(SetCollection.letNamedSet()).isEqualTo(3);
    }
}
