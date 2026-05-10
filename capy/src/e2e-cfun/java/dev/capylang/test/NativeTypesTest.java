package dev.capylang.test;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;

class NativeTypesTest {
    @Test
    void stringLength() {
        assertThat(NativeTypes.stringLength("capybara")).isEqualTo(8);
    }

    @Test
    void stringTrim() {
        assertThat(NativeTypes.stringTrim("  capybara  ")).isEqualTo("capybara");
    }

    @Test
    void stringGet() {
        assertThat(NativeTypes.stringGet("capybara", 0)).isEqualTo(Optional.of("c"));
        assertThat(NativeTypes.stringGet("capybara", -1)).isEqualTo(Optional.of("a"));
        assertThat(NativeTypes.stringGet("capybara", 99)).isEqualTo(Optional.empty());
        assertThat(NativeTypes.stringGetRange("capybara", 1, -1)).isEqualTo("apybar");
    }

    @Test
    void stringGetRange() {
        var value = "abcdef";

        assertThat(NativeTypes.stringGetRange(value, 0, 6)).isEqualTo("abcdef");
        assertThat(NativeTypes.stringGetRange(value, 0, 0)).isEqualTo("");
        assertThat(NativeTypes.stringGetRange(value, 3, 3)).isEqualTo("");
        assertThat(NativeTypes.stringGetRange(value, 1, 4)).isEqualTo("bcd");
        assertThat(NativeTypes.stringGetRange(value, 0, -1)).isEqualTo("abcde");
        assertThat(NativeTypes.stringGetRange(value, -4, -1)).isEqualTo("cde");
        assertThat(NativeTypes.stringGetRange(value, -3, 6)).isEqualTo("def");
    }

    @Test
    void listMethods() {
        assertThat(NativeTypes.listSize(List.of(1, 2, 3))).isEqualTo(3);
        assertThat(NativeTypes.listContains(List.of(1, 2, 3), 2)).isTrue();
        assertThat(NativeTypes.listContains(List.of(1, 2, 3), 9)).isFalse();
        assertThat(NativeTypes.listGet(List.of(1, 2, 3), 1)).isEqualTo(Optional.of(2));
        assertThat(NativeTypes.listGet(List.of(1, 2, 3), -1)).isEqualTo(Optional.of(3));
        assertThat(NativeTypes.listGet(List.of(1, 2, 3), 9)).isEqualTo(Optional.empty());
        assertThat(NativeTypes.listGetRangeMethod(List.of(1, 2, 3, 4), 1, -1)).isEqualTo(List.of(2, 3));
        assertThat(NativeTypes.listGetRange(List.of(1, 2, 3, 4), 1, -1)).isEqualTo(List.of(2, 3));
    }

    @Test
    void listGetRangeMethod() {
        var values = List.of(1, 2, 3, 4, 5, 6);

        assertThat(NativeTypes.listGetRangeMethod(values, 0, 6)).isEqualTo(List.of(1, 2, 3, 4, 5, 6));
        assertThat(NativeTypes.listGetRangeMethod(values, 0, 0)).isEqualTo(List.of());
        assertThat(NativeTypes.listGetRangeMethod(values, 3, 3)).isEqualTo(List.of());
        assertThat(NativeTypes.listGetRangeMethod(values, 1, 4)).isEqualTo(List.of(2, 3, 4));
        assertThat(NativeTypes.listGetRangeMethod(values, 0, -1)).isEqualTo(List.of(1, 2, 3, 4, 5));
        assertThat(NativeTypes.listGetRangeMethod(values, -4, -1)).isEqualTo(List.of(3, 4, 5));
        assertThat(NativeTypes.listGetRangeMethod(values, -3, 6)).isEqualTo(List.of(4, 5, 6));
    }

    @Test
    void setMethods() {
        assertThat(NativeTypes.setIsEmpty(Set.of())).isTrue();
        assertThat(NativeTypes.setContains(Set.of(1, 2, 3), 2)).isTrue();
        assertThat(NativeTypes.setContains(Set.of(1, 2, 3), 9)).isFalse();
        assertThat(NativeTypes.setGet(Set.of(1, 2, 3), 2)).isEqualTo(Optional.of(2));
        assertThat(NativeTypes.setGet(Set.of(1, 2, 3), 9)).isEqualTo(Optional.empty());
        assertThat(NativeTypes.setIndex(Set.of(1, 2, 3), 2)).isEqualTo(Optional.of(2));
        assertThat(NativeTypes.setIndex(Set.of(1, 2, 3), 9)).isEqualTo(Optional.empty());
    }

    @Test
    void dictMethods() {
        assertThat(NativeTypes.dictSize(Map.of("one", 1, "two", 2))).isEqualTo(2);
        assertThat(NativeTypes.dictContainsKey(Map.of("one", 1), "one")).isTrue();
        assertThat(NativeTypes.dictContainsKey(Map.of("one", 1), "two")).isFalse();
        assertThat(NativeTypes.dictGet(Map.of("one", 1), "one")).isEqualTo(Optional.of(1));
        assertThat(NativeTypes.dictGet(Map.of("one", 1), "two")).isEqualTo(Optional.empty());
        assertThat(NativeTypes.dictIndex(Map.of("one", 1), "one")).isEqualTo(Optional.of(1));
        assertThat(NativeTypes.dictIndex(Map.of("one", 1), "two")).isEqualTo(Optional.empty());
    }

    @Test
    void tupleAlias() {
        assertThat(NativeTypes.tupleSecond(List.of(1, "two"))).isEqualTo("two");
        assertThat(NativeTypes.tupleGetSecond(List.of(1, "two"))).isEqualTo("two");
    }
}
