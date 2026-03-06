package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class PipeTest {
    @Test
    void map1() {
        assertThat(Pipe.map1(List.of(1, 2, 3))).isEqualTo(List.of(2, 4, 6));
    }

    @Test
    void map2() {
        assertThat(Pipe.map2(List.of(1, 2, 3))).isEqualTo(List.of(1, 3, 5));
    }

    @Test
    void map3() {
        assertThat(Pipe.map3(List.of(1, 2, 3))).isEqualTo(List.of("Hello 1", "Hello 2", "Hello 3"));
    }

    @Test
    void filter1() {
        assertThat(Pipe.filter1(List.of(-1, 0, 1, 2))).isEqualTo(List.of(-1, 0));
    }

    @Test
    void filter2() {
        assertThat(Pipe.filter2(List.of(1, 2, 3, 4))).isEqualTo(List.of(2));
    }

    @Test
    void reduce1() {
        assertThat(Pipe.reduce1(List.of(1, 2, 3, 4))).isEqualTo(10);
    }

    @Test
    void reduce2() {
        assertThat(Pipe.reduce2(List.of(-1, 1, 2, 3))).isEqualTo(12);
    }
}
