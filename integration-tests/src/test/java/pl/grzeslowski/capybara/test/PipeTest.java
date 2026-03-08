package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.Optional;
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
    void reduceString1() {
        assertThat(Pipe.reduceString1(List.of("capy", "bara"))).isEqualTo("capybara");
    }

    @Test
    void reduceString2() {
        assertThat(Pipe.reduceString2(List.of("a", "b", "c"))).isEqualTo("start: a b c");
    }

    @Test
    void reduceString2Empty() {
        assertThat(Pipe.reduceString2(List.of())).isEqualTo("start: ");
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
    void setReduceString2() {
        assertThat(Pipe.setReduceString2(Set.of("a"))).isEqualTo("start: a");
    }

    @Test
    void setReduceString2Empty() {
        assertThat(Pipe.setReduceString2(Set.of())).isEqualTo("start: ");
    }

    @Test
    void setFlatMap1() {
        assertThat(Pipe.setFlatMap1(Set.of(1, 2))).isEqualTo(Set.of(1, 11, 2, 12));
    }

    @Test
    void mapDict() {
        assertThat(Pipe.mapDict(Map.of("a", 1, "b", 2))).isEqualTo(Map.of("a", "1", "b", "2"));
    }

    @Test
    void reduceDict() {
        var reduced = Pipe.reduceDict(Map.of("a", 1, "b", 2));
        assertThat(reduced).isEqualTo("start: (a:1), (b:2)");
    }

    @Test
    void reduceDictEmpty() {
        var reduced = Pipe.reduceDict(Map.of());
        assertThat(reduced).isEqualTo("start: ");
    }

    @Test
    void dictReduceString2() {
        assertThat(Pipe.dictReduceString2(Map.of("k", "v"))).isEqualTo("start: v");
    }

    @Test
    void dictReduceString2Empty() {
        assertThat(Pipe.dictReduceString2(Map.of())).isEqualTo("start: ");
    }

    @Test
    void dataPipe() {
        assertThat(Pipe.dataPipe(1, 2, 3)).isEmpty();
        assertThat(Pipe.dataPipe(-1, 2, 3)).contains(new Pipe.Value(4));
    }

    @Test
    void typePipeLambda() {
        var result = Pipe.typePipeLambda(5);
        assertThat(result).isInstanceOf(Pipe.IntBox.class);
        assertThat(((Pipe.IntBox) result).value()).isEqualTo(6);
    }

    @Test
    void typePipeRef() {
        var result = Pipe.typePipeRef(7);
        assertThat(result).isInstanceOf(Pipe.IntBox.class);
        assertThat(((Pipe.IntBox) result).value()).isEqualTo(8);
    }

    @Test
    void typePipeOptional() {
        Optional<Pipe.Boxed> positive = Pipe.typePipeOptional(5);
        Optional<Pipe.Boxed> nonPositive = Pipe.typePipeOptional(0);

        assertThat(positive).isEmpty();
        assertThat(nonPositive).isPresent();
        assertThat(nonPositive.get()).isInstanceOf(Pipe.IntBox.class);
        assertThat(((Pipe.IntBox) nonPositive.get()).value()).isEqualTo(1);
    }
}
