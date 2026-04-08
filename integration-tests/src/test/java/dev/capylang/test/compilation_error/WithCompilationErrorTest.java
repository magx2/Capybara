package dev.capylang.test.compilation_error;

import dev.capylang.compiler.CapybaraCompiler;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.Result;
import dev.capylang.compiler.parser.RawModule;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.List;
import java.util.TreeSet;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;

class WithCompilationErrorTest {
    @ParameterizedTest(name = "{index}: {0}")
    @MethodSource
    void compilationErrors(String name, String source, String expectedFragment) {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(new RawModule(name, "/foo/boo", source)), new TreeSet<>());

        assertThat(result).isInstanceOf(Result.Error.class);
        var error = ((Result.Error<CompiledProgram>) result).errors().first();
        assertThat(error.message()).contains(expectedFragment);
    }

    static Stream<Arguments> compilationErrors() {
        return Stream.of(
                Arguments.of(
                        "unknown field",
                        """
                                data Foo { a: int }
                                fun foo(foo: Foo): Foo = foo.with(b: 1)
                                """,
                        "Field `b` not found in type `Foo`"
                ),
                Arguments.of(
                        "duplicate field",
                        """
                                data Foo { a: int }
                                fun foo(foo: Foo): Foo = foo.with(a: 1, a: 2)
                                """,
                        "Field `a` is assigned more than once"
                ),
                Arguments.of(
                        "wrong field type",
                        """
                                data Foo { a: int }
                                fun foo(foo: Foo): Foo = foo.with(a: \"x\")
                                """,
                        "Expected `INT`, got `STRING`"
                ),
                Arguments.of(
                        "non data receiver",
                        """
                                fun foo(x: int): int = x.with(a: 1)
                                """,
                        "`.with(...)` requires data/type receiver"
                ),
                Arguments.of(
                        "parent cannot update subtype field",
                        """
                                type Letter { x: int } = A | B
                                data A { a: string }
                                data B { b: int }
                                fun foo(letter: Letter): Letter = letter.with(a: \"x\")
                                """,
                        "Field `a` not found in type `Letter`"
                )
        );
    }
}


