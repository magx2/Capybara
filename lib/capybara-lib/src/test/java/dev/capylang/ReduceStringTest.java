package dev.capylang;

import capy.collection.Dict;
import capy.collection.List;
import capy.collection.Set;
import capy.lang.Seq;
import org.junit.jupiter.api.Test;

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ReduceStringTest {
    @Test
    void shouldReduceListValuesToStringWithoutLeadingSeparator() {
        assertAll(
                () -> assertEquals(
                        "1, 2, 3",
                        List.reduce(java.util.List.of(1, 2, 3), "", acc -> value -> acc + ", " + value)
                ),
                () -> assertEquals(
                        "1 | 2 | 3",
                        List.reduce(java.util.List.of(1, 2, 3), "", acc -> value -> acc + " | " + value)
                ),
                () -> assertEquals(
                        "values: 1 :: 2 :: 3",
                        List.reduce(java.util.List.of(1, 2, 3), "values: ", acc -> value -> acc + " :: " + value)
                ),
                () -> assertEquals(
                        ",1,2",
                        List.reduce(java.util.List.of(",1", ",2"), "", acc -> value -> acc + value)
                )
        );
    }

    @Test
    void shouldReduceDictEntriesToStringWithoutLeadingSeparator() {
        var values = new LinkedHashMap<java.lang.String, java.lang.Integer>();
        values.put("a", 1);
        values.put("b", 2);

        assertAll(
                () -> assertEquals(
                        "a:1, b:2",
                        Dict.reduce(values, "", acc -> key -> value -> acc + ", " + key + ":" + value)
                ),
                () -> assertEquals(
                        "a=1 | b=2",
                        Dict.reduce(values, "", acc -> key -> value -> acc + " | " + key + "=" + value)
                ),
                () -> assertEquals(
                        "values: a:1 :: b:2",
                        Dict.reduce(values, "values: ", acc -> key -> value -> acc + " :: " + key + ":" + value)
                )
        );
    }

    @Test
    void shouldReduceSetValuesToStringWithoutLeadingSeparator() {
        var values = new LinkedHashSet<>(java.util.List.of(1, 2, 3));

        var commaSeparated = Set.reduce(values, "", acc -> value -> acc + ", " + value);
        var pipeSeparated = Set.reduce(values, "", acc -> value -> acc + " | " + value);
        var prefixed = Set.reduce(values, "values: ", acc -> value -> acc + " :: " + value);

        assertAll(
                () -> assertFalse(commaSeparated.startsWith(", ")),
                () -> assertTrue(commaSeparated.contains("1")),
                () -> assertTrue(commaSeparated.contains("2")),
                () -> assertTrue(commaSeparated.contains("3")),
                () -> assertFalse(pipeSeparated.startsWith(" | ")),
                () -> assertTrue(pipeSeparated.contains("1")),
                () -> assertTrue(pipeSeparated.contains("2")),
                () -> assertTrue(pipeSeparated.contains("3")),
                () -> assertTrue(prefixed.startsWith("values: ")),
                () -> assertFalse(prefixed.startsWith("values:  :: ")),
                () -> assertTrue(prefixed.contains("1")),
                () -> assertTrue(prefixed.contains("2")),
                () -> assertTrue(prefixed.contains("3"))
        );
    }

    @Test
    void shouldReduceSeqValuesToStringWithoutLeadingSeparator() {
        @SuppressWarnings("unchecked")
        var empty = (Seq<java.lang.Integer>) Seq.End.INSTANCE;

        assertAll(
                () -> assertEquals(
                        "1, 2, 3",
                        Seq.toSeq(java.util.List.of(1, 2, 3)).reduce("", acc -> value -> acc + ", " + value)
                ),
                () -> assertEquals(
                        "1 | 2 | 3",
                        Seq.toSeq(java.util.List.of(1, 2, 3)).reduce("", acc -> value -> acc + " | " + value)
                ),
                () -> assertEquals(
                        "values: 1 :: 2 :: 3",
                        Seq.toSeq(java.util.List.of(1, 2, 3)).reduce("values: ", acc -> value -> acc + " :: " + value)
                ),
                () -> assertEquals(
                        "seed",
                        empty.reduce("seed", acc -> value -> acc + value)
                )
        );
    }
}
