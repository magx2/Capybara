package dev.capylang.test;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
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
    void listMethods() {
        assertThat(NativeTypes.listSize(List.of(1, 2, 3))).isEqualTo(3);
        assertThat(NativeTypes.listContains(List.of(1, 2, 3), 2)).isTrue();
        assertThat(NativeTypes.listContains(List.of(1, 2, 3), 9)).isFalse();
    }

    @Test
    void setMethods() {
        assertThat(NativeTypes.setIsEmpty(Set.of())).isTrue();
        assertThat(NativeTypes.setContains(Set.of(1, 2, 3), 2)).isTrue();
        assertThat(NativeTypes.setContains(Set.of(1, 2, 3), 9)).isFalse();
    }

    @Test
    void dictMethods() {
        assertThat(NativeTypes.dictSize(Map.of("one", 1, "two", 2))).isEqualTo(2);
        assertThat(NativeTypes.dictContainsKey(Map.of("one", 1), "one")).isTrue();
        assertThat(NativeTypes.dictContainsKey(Map.of("one", 1), "two")).isFalse();
    }

    @Test
    void tupleAlias() {
        assertThat(NativeTypes.tupleSecond(List.of(1, "two"))).isEqualTo("two");
    }
}
