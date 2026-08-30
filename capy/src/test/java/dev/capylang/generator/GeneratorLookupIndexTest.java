package dev.capylang.generator;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

class GeneratorLookupIndexTest {
    @Test
    void indexesFunctionsByExactNameAndPrefixWithoutChangingOrder() {
        var first = function("__capy_schema_value|Order|first");
        var unrelated = function("value");
        var second = function("__capy_schema_value|Order|second");
        var functions = List.<Object>of(first, unrelated, second);

        assertThat(GeneratorLookupIndex.functionsNamed(functions, "value"))
                .containsExactly(unrelated);
        assertThat(GeneratorLookupIndex.functionsNamedWithPrefix(functions, "__capy_schema_value|Order|"))
                .containsExactly(first, second);
    }

    @Test
    void indexesModulesByNormalizedPath() {
        var module = Map.<String, Object>of("path", "capy\\collection", "name", "List");

        assertThat(GeneratorLookupIndex.moduleByPath("/capy/collection/List/", List.<Object>of(module)))
                .contains(module);
    }

    private static Map<String, Object> function(String name) {
        return Map.of("name", name);
    }
}
