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

    @Test
    void shouldInferConcreteElementTypeForImportedGenericMethodCallAgainstCompiledLibraries() {
        var libraries = compileProgram(List.of(
                new RawModule("Seq", "/capy/lang", """
                        type Seq[T] = Cons[T] | End
                        data Cons[T] { value: T, rest: () => Seq[T] }
                        single End
                        type Option[T] = Some[T] | None
                        data Some[T] { value: T }
                        single None

                        fun to_seq(values: list[T]): Seq[T] = End
                        fun Seq[T].first_match(pred: T => bool): Option[T] = None
                        """)
        )).modules();

        var compiled = compileProgram(
                List.of(new RawModule("Consumer", "/foo/boo", """
                        from /capy/lang/Seq import { * }

                        fun has_even(): Option[int] =
                            let seq = to_seq([1, 3, 4, 7])
                            seq.first_match(x => x % 2 == 0)
                        """)),
                libraries
        );

        assertThat(compiled.modules()).extracting(CompiledModule::name).contains("Consumer");
    }

    @Test
    void shouldInferConcreteReturnTypeForImportedGenericFunctionAgainstCompiledLibraries() {
        var libraries = compileProgram(List.of(
                new RawModule("Seq", "/capy/lang", """
                        type Seq[T] = Cons[T] | End
                        data Cons[T] { value: T, rest: () => Seq[T] }
                        single End

                        fun to_seq(values: list[T]): Seq[T] = End
                        """)
        )).modules();

        var compiled = compileProgram(
                List.of(new RawModule("Consumer", "/foo/boo", """
                        from /capy/lang/Seq import { * }

                        fun make_seq() =
                            to_seq([1, 3, 4, 7])
                        """)),
                libraries
        );

        var function = compiled.modules().stream()
                .filter(it -> it.name().equals("Consumer"))
                .findFirst()
                .orElseThrow()
                .functions().stream()
                .filter(it -> it.name().equals("make_seq"))
                .findFirst()
                .orElseThrow();

        assertThat(function.returnType()).isInstanceOf(CompiledDataParentType.class);
        assertThat(((CompiledDataParentType) function.returnType()).typeParameters()).containsExactly("int");
    }

    private static CompiledProgram compileProgram(String code) {
        return compileProgram(List.of(new RawModule("InfixOperators", "/foo/boo", code)));
    }

    private static CompiledProgram compileProgram(List<RawModule> modules) {
        return compileProgram(modules, new java.util.TreeSet<>());
    }

    private static CompiledProgram compileProgram(List<RawModule> modules, java.util.SortedSet<CompiledModule> libraries) {
        var result = CapybaraCompiler.INSTANCE.compile(
                modules,
                libraries
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
