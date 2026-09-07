package dev.capylang.compiler;

import dev.capylang.compiler.parser.TypeReference;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class StrictSemanticAnalyzerTest {
    @Test
    void permitsNumericWideningButRejectsNarrowing() {
        assertThat(assignable("int", "long")).isTrue();
        assertThat(assignable("long", "float")).isTrue();
        assertThat(assignable("double", "int")).isFalse();
    }

    @Test
    void checksNestedGenericArguments() {
        assertThat(StrictSemanticAnalyzer.isAssignableForTesting(
                type("List", type("int")),
                type("List", type("long"))
        )).isTrue();
        assertThat(StrictSemanticAnalyzer.isAssignableForTesting(
                type("List", type("String")),
                type("List", type("int"))
        )).isFalse();
    }

    @Test
    void checksFunctionParametersContravariantlyAndReturnsCovariantly() {
        assertThat(assignable("long => int", "int => long")).isTrue();
        assertThat(assignable("int => long", "long => long")).isFalse();
    }

    private static boolean assignable(String actual, String expected) {
        return StrictSemanticAnalyzer.isAssignableForTesting(type(actual), type(expected));
    }

    private static TypeReference type(String name, TypeReference... arguments) {
        return new TypeReference(name, List.of(arguments));
    }
}
