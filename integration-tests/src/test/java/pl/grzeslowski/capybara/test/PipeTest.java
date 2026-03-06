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
}
