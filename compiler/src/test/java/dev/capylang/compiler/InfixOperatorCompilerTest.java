package dev.capylang.compiler;

import dev.capylang.compiler.parser.RawModule;
import org.junit.jupiter.api.Test;

import java.util.List;

import static dev.capylang.compiler.CollectionLinkedType.CompiledList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;

class InfixOperatorCompilerTest {
    @Test
    void shouldFailPlusForDataParentAndSubtype() {
        var error = compileFailure("""
                type Json = JsonArray | JsonNull
                data JsonArray { value: list[Json] }
                single JsonNull

                fun foo(left: JsonArray, right: Json): Json =
                    left + right
                """);

        assertThat(error.message()).contains("`+` operator is not defined for `JsonArray + Json`");
    }

    @Test
    void shouldAllowPlusForCollectionAndSubtypeElement() {
        var compiled = compileProgram("""
                type Json = JsonArray | JsonNull
                data JsonArray { value: list[Json] }
                single JsonNull

                fun append(values: list[Json], value: JsonArray): list[Json] =
                    values + value
                """);

        var function = compiled.modules().first().functions().stream()
                .filter(it -> it.name().equals("append"))
                .findFirst()
                .orElseThrow();

        assertThat(function.returnType()).isInstanceOf(CompiledList.class);
        assertThat(((CompiledList) function.returnType()).elementType().name()).isEqualTo("Json");
    }

    private static CompiledProgram compileProgram(String code) {
        var result = CapybaraCompiler.INSTANCE.compile(
                List.of(new RawModule("InfixOperators", "/foo/boo", code)),
                new java.util.TreeSet<>()
        );
        if (result instanceof Result.Error<CompiledProgram> error) {
            fail(error.errors().toString());
        }
        return ((Result.Success<CompiledProgram>) result).value();
    }

    private static Result.Error.SingleError compileFailure(String code) {
        var result = CapybaraCompiler.INSTANCE.compile(
                List.of(new RawModule("InfixOperators", "/foo/boo", code)),
                new java.util.TreeSet<>()
        );
        if (result instanceof Result.Success<CompiledProgram> value) {
            fail("Expected compilation to fail but it succeeded: " + value);
        }
        var errors = ((Result.Error<CompiledProgram>) result).errors();
        assertThat(errors).isNotEmpty();
        return errors.first();
    }
}
