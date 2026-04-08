package dev.capylang.test;

import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

class DictCollectionTest {
    private static final Map<String, Integer> EXPECTED = expected();

    private static Map<String, Integer> expected() {
        var map = new LinkedHashMap<String, Integer>();
        map.put("one", 1);
        map.put("two", 2);
        map.put("three", 3);
        return map;
    }

    @Test
    void staticDict() {
        assertThat(DictCollection.staticDict()).isEqualTo(EXPECTED);
    }

    @Test
    void emptyStaticDict() {
        assertThat(DictCollection.emptyStaticDict()).isEmpty();
    }

    @Test
    void emptyStaticDictWithSpaces() {
        assertThat(DictCollection.emptyStaticDictWithSpaces()).isEmpty();
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

    @Test
    void appendTuple() {
        var input = new HashMap<>(EXPECTED);

        var result = DictCollection.appendTuple(input);

        assertThat(result).isEqualTo(Map.of(
                "one", 1,
                "two", 2,
                "three", 3,
                "leet", 1337
        ));
        assertThat(input).isEqualTo(EXPECTED);
    }

    @Test
    void appendTupleOverride() {
        var input = new HashMap<>(EXPECTED);

        var result = DictCollection.appendTupleOverride(input);

        assertThat(result).isEqualTo(Map.of(
                "one", 1,
                "two", 200,
                "three", 3
        ));
        assertThat(input).isEqualTo(EXPECTED);
    }

    @Test
    void remove() {
        var input = new HashMap<>(EXPECTED);

        var result = DictCollection.remove(input);

        assertThat(result).isEqualTo(Map.of(
                "one", 1,
                "three", 3
        ));
        assertThat(input).isEqualTo(EXPECTED);
    }

    @Test
    void removeDict() {
        var left = new HashMap<>(Map.of("one", 1, "two", 2, "three", 3));
        var right = new HashMap<>(Map.of("two", 1000, "three", 2000));

        var result = DictCollection.removeDict(left, right);

        assertThat(result).isEqualTo(Map.of("one", 1));
        assertThat(left).isEqualTo(Map.of("one", 1, "two", 2, "three", 3));
        assertThat(right).isEqualTo(Map.of("two", 1000, "three", 2000));
    }

    @Test
    void contains() {
        assertThat(DictCollection.contains(EXPECTED, "two")).isTrue();
        assertThat(DictCollection.contains(EXPECTED, "missing")).isFalse();
    }

    @Test
    void isEmpty() {
        assertThat(DictCollection.isEmpty(Map.of())).isTrue();
        assertThat(DictCollection.isEmpty(EXPECTED)).isFalse();
    }

    @Test
    void notIsEmpty() {
        assertThat(DictCollection.notIsEmpty(Map.of())).isFalse();
        assertThat(DictCollection.notIsEmpty(EXPECTED)).isTrue();
    }

    @Test
    void notNestedIsEmpty() {
        assertThat(DictCollection.notNestedIsEmpty(new DictCollection.DictHolder(Map.of()))).isFalse();
        assertThat(DictCollection.notNestedIsEmpty(new DictCollection.DictHolder(EXPECTED))).isTrue();
    }

    @Test
    void size() {
        assertThat(DictCollection.size(EXPECTED)).isEqualTo(3);
        assertThat(DictCollection.size(Map.of())).isEqualTo(0);
    }

    @Test
    void dictToList() {
        assertThat(DictCollection.dictToList(EXPECTED))
                .containsExactlyInAnyOrder(
                        new DictCollection.Entry("one", 1),
                        new DictCollection.Entry("two", 2),
                        new DictCollection.Entry("three", 3)
                );
    }

    @Test
    void dictToString() {
        var result = DictCollection.dictToString(EXPECTED);
        assertThat(result).isEqualTo("one 1, two 2, three 3");
    }
}
