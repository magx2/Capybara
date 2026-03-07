package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Set;

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

    @Test
    void flatMap1() {
        assertThat(Pipe.flatMap1(List.of(1, 2))).isEqualTo(List.of(1, 2, 3, 2, 3, 4));
    }

    @Test
    void flatMap2() {
        assertThat(Pipe.flatMap2(List.of(-1, 1, 2))).isEqualTo(List.of(-2, -1, 0));
    }

    @Test
    void setMap1() {
        assertThat(Pipe.setMap1(Set.of(1, 2, 3))).isEqualTo(Set.of(2, 4, 6));
    }

    @Test
    void setFilter1() {
        assertThat(Pipe.setFilter1(Set.of(-1, 0, 1, 2))).isEqualTo(Set.of(-1, 0));
    }

    @Test
    void setReduce1() {
        assertThat(Pipe.setReduce1(Set.of(1, 2, 3, 4))).isEqualTo(10);
    }

    @Test
    void setFlatMap1() {
        assertThat(Pipe.setFlatMap1(Set.of(1, 2))).isEqualTo(Set.of(1, 11, 2, 12));
    }

    @Test
    void dataPipe() {
        assertThat(Pipe.dataPipe(1, 2, 3).x()).isEqualTo(6);
    }
}
