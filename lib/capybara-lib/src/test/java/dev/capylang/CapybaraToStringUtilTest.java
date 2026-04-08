package dev.capylang;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

class CapybaraToStringUtilTest {
    @Test
    void shouldEscapeStrings() {
        assertEquals("\"va\\\\\\\"lue\"", CapybaraToStringUtil.toStringValue("va\\\"lue"));
    }

    @Test
    void shouldFormatCollectionsAndMaps() {
        assertEquals("{x=[1,\"two\"]}", CapybaraToStringUtil.toStringValue(Map.of("x", List.of(1, "two"))));
    }
}
