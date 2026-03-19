package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class SeqCollectionTest {
    @Test
    void buildsRecursiveGenericSequence() {
        var seq = SeqCollection.oneTwoThree();
        assertThat(seq).isInstanceOf(SeqCollection.Cons.class);

        var first = (SeqCollection.Cons<Integer>) seq;
        assertThat(first.value()).isEqualTo(1);
        assertThat(first.rest()).isInstanceOf(SeqCollection.Cons.class);
    }

    @Test
    void headAndTailWorkForRecursiveType() {
        var seq = SeqCollection.oneTwoThree();
        assertThat(SeqCollection.headOrDefault(seq, -1)).isEqualTo(1);

        var tail = SeqCollection.tail(seq);
        assertThat(SeqCollection.headOrDefault(tail, -1)).isEqualTo(2);
    }

    @Test
    void sumFirstTwoUsesNestedSeqOfTField() {
        assertThat(SeqCollection.sumFirstTwo(SeqCollection.oneTwoThree())).isEqualTo(3);
        assertThat(SeqCollection.sumFirstTwo(SeqCollection.End.INSTANCE)).isEqualTo(0);
    }

    @Test
    void dropLocalInfersSeqReturnTypeWithoutExplicitAnnotation() {
        var dropped = SeqCollection.dropLocal(SeqCollection.oneTwoThree(), 1);
        assertThat(SeqCollection.headOrDefault(dropped, -1)).isEqualTo(2);
        assertThat(SeqCollection.headOrDefault(SeqCollection.dropLocal(SeqCollection.oneTwoThree(), 3), -1)).isEqualTo(-1);
    }

    @Test
    void untilLocalInfersSeqReturnTypeWithoutExplicitAnnotation() {
        var untilTwo = SeqCollection.untilLocal(SeqCollection.oneTwoThree(), 2);
        assertThat(SeqCollection.headOrDefault(untilTwo, -1)).isEqualTo(1);
        assertThat(SeqCollection.headOrDefault(SeqCollection.tail(untilTwo), -1)).isEqualTo(-1);
    }
}
