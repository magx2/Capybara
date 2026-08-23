package dev.capylang.compiler;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.assertj.core.api.Assertions.assertThat;

class LinkedJsonCodecTest {
    @TempDir
    Path tempDir;

    @Test
    void readsLegacyLinkedProgram() throws Exception {
        Files.writeString(tempDir.resolve("program.json"), """
                {
                  "modules" : [ "java.util.TreeSet", [ {
                    "name" : "Legacy",
                    "path" : "dev/test",
                    "types" : { "@class" : "java.util.TreeMap" },
                    "visiblePrimitiveBackedTypes" : { "@class" : "java.util.TreeMap" },
                    "functions" : [ "java.util.TreeSet", [ {
                      "name" : "legacy_fun",
                      "visibility" : null,
                      "parameters" : [ "java.util.ImmutableCollections$ListN", [ {
                        "name" : "value",
                        "type" : [ "dev.capylang.compiler.PrimitiveLinkedType", "INT" ],
                        "location" : { "line" : 1, "column" : 12 }
                      } ] ],
                      "returnType" : {
                        "@class" : "dev.capylang.compiler.CollectionLinkedType$CompiledList",
                        "elementType" : [ "dev.capylang.compiler.PrimitiveLinkedType", "STRING" ]
                      },
                      "expression" : {
                        "@class" : "dev.capylang.compiler.expression.CompiledIntValue",
                        "value" : 1,
                        "source" : "1",
                        "location" : { "line" : 1, "column" : 25 }
                      },
                      "location" : { "line" : 1, "column" : 0 }
                    } ] ],
                    "imports" : [ "java.util.ImmutableCollections$ListN", [ ] ],
                    "derivers" : { "@class" : "java.util.TreeMap" },
                    "annotations" : { "@class" : "java.util.TreeMap" },
                    "staticImports" : [ "java.util.ImmutableCollections$ListN", [ ] ]
                  } ] ],
                  "objectOrientedModules" : [ "java.util.ImmutableCollections$ListN", [ ] ],
                  "nativeProviders" : { "providers" : [ "java.util.ImmutableCollections$ListN", [ ] ] },
                  "nativeProviderCatalog" : {
                    "declarations" : [ "java.util.ImmutableCollections$ListN", [ ] ],
                    "bindings" : [ "java.util.ImmutableCollections$ListN", [ ] ]
                  }
                }
                """);

        var program = LinkedJsonCodec.readProgram(tempDir, true);

        assertThat(program.modules()).hasSize(1);
        var function = program.modules().getFirst().functions().getFirst();
        assertThat(function.visibility()).isEqualTo("public");
        assertThat(function.parameters().getFirst().typeReference().name()).isEqualTo("int");
        assertThat(function.returnType().name()).isEqualTo("List");
        assertThat(function.returnType().arguments().getFirst().name()).isEqualTo("String");
        assertThat(function.body()).isInstanceOf(CompiledExpression.CompiledUnsupportedExpression.class);
    }
}
