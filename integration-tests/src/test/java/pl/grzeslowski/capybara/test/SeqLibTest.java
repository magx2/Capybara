package pl.grzeslowski.capybara.test;

import capy.lang.Seq;
import org.junit.jupiter.api.Test;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

class SeqLibTest {
    @Test
    void toSeqFromDictBuildsFiniteSequenceOfEntries() {
        var dict = new LinkedHashMap<String, Integer>();
        dict.put("one", 1);
        dict.put("two", 2);

        var seq = Seq.toSeq(dict);
        var entries = seq.take(2);

        assertThat(entries).hasSize(2);
        assertThat(entries).containsExactlyInAnyOrder(
                List.of("one", 1),
                List.of("two", 2)
        );
    }
}
