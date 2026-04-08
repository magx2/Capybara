package dev.capylang;

import org.junit.jupiter.api.Test;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

class CapybaraToStringUtilTest {
    @Test
    void shouldRenderNull() {
        assertEquals("null", CapybaraToStringUtil.__capybaraToStringValue(null));
    }

    @Test
    void shouldEscapeStringValues() {
        assertEquals("\"hello \\\"capy\\\"\\\\lang\"", CapybaraToStringUtil.__capybaraToStringValue("hello \"capy\"\\lang"));
    }

    @Test
    void shouldRenderEnumSingletonByTypeName() {
        assertEquals("SingletonEnum", CapybaraToStringUtil.__capybaraToStringValue(SingletonEnum.INSTANCE));
    }

    @Test
    void shouldRenderEnumNamedValueByName() {
        assertEquals("SECOND", CapybaraToStringUtil.__capybaraToStringValue(NamedEnum.SECOND));
    }

    @Test
    void shouldRenderMapAndCollectionValuesRecursively() {
        var nested = new LinkedHashMap<String, Object>();
        nested.put("k", List.of("v", NamedEnum.FIRST));
        nested.put("entry", Map.entry("x", "y"));

        assertEquals("{k=[\"v\",FIRST],entry=x=\"y\"}", CapybaraToStringUtil.__capybaraToStringValue(nested));
    }

    private enum SingletonEnum {
        INSTANCE
    }

    private enum NamedEnum {
        FIRST,
        SECOND
    }
}
