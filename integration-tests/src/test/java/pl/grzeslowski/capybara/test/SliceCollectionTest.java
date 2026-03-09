package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class SliceCollectionTest {
    @Test
    void listFrom1() {
        assertThat(SliceCollection.listFrom1(List.of(0, 1, 2, 3, 4, 5))).isEqualTo(List.of(1, 2, 3, 4, 5));
    }

    @Test
    void listTo5() {
        assertThat(SliceCollection.listTo5(List.of(0, 1, 2, 3, 4, 5))).isEqualTo(List.of(0, 1, 2, 3, 4));
    }

    @Test
    void listToMinus3() {
        assertThat(SliceCollection.listToMinus3(List.of(0, 1, 2, 3, 4, 5))).isEqualTo(List.of(0, 1, 2));
    }

    @Test
    void list12() {
        assertThat(SliceCollection.list12(List.of(0, 1, 2, 3, 4, 5))).isEqualTo(List.of(1));
    }

    @Test
    void list1Minus2() {
        assertThat(SliceCollection.list1Minus2(List.of(0, 1, 2, 3, 4, 5))).isEqualTo(List.of(1, 2, 3));
    }

    @Test
    void stringFrom1() {
        assertThat(SliceCollection.stringFrom1("abcdef")).isEqualTo("bcdef");
    }

    @Test
    void stringTo5() {
        assertThat(SliceCollection.stringTo5("abcdef")).isEqualTo("abcde");
    }

    @Test
    void stringToMinus3() {
        assertThat(SliceCollection.stringToMinus3("abcdef")).isEqualTo("abc");
    }

    @Test
    void string12() {
        assertThat(SliceCollection.string12("abcdef")).isEqualTo("b");
    }

    @Test
    void string1Minus2() {
        assertThat(SliceCollection.string1Minus2("abcdef")).isEqualTo("bcd");
    }
}
