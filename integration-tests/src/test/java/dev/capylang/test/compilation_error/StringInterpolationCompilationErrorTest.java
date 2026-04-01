package dev.capylang.test.compilation_error;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import dev.capylang.compiler.CapybaraCompiler;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.Result;
import dev.capylang.compiler.parser.RawModule;

import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertAll;

class StringInterpolationCompilationErrorTest {
    @ParameterizedTest(name = "{index}: should fail when compiling `{0}.cfun`")
    @MethodSource
    void compilationError(
            String moduleName,
            String code,
            int expectedLine,
            int expectedColumn,
            String expectedMessageFragment
    ) {
        var errors = compileProgram(code, moduleName);

        assertThat(errors).hasSize(1);
        var error = errors.first();
        assertAll(
                () -> assertThat(error.file()).isEqualTo("/foo/boo/%s.cfun".formatted(moduleName)),
                () -> assertThat(error.line()).isEqualTo(expectedLine),
                () -> assertThat(error.column()).isEqualTo(expectedColumn),
                () -> assertThat(error.message()).contains(expectedMessageFragment)
        );
    }

    static Stream<Arguments> compilationError() {
        return Stream.of(
                Arguments.of(
                        "interpolation_empty_expression",
                        "fun foo(name: string): string = \"Hello {}\"",
                        1,
                        39,
                        "Cannot evaluate expression"
                ),
                Arguments.of(
                        "interpolation_unknown_variable",
                        "fun foo(name: string): string = \"Hello {surname}\"",
                        1,
                        39,
                        "Variable surname not found"
                ),
                Arguments.of(
                        "interpolation_missing_closing_brace",
                        "fun foo(name: string): string = \"Hello {name\"",
                        1,
                        39,
                        "missing `}`"
                ),
                Arguments.of(
                        "interpolation_missing_data_field",
                        """
                                data Foo { name: string }
                                fun foo(foo: Foo): string = \"Hello {foo.surname}\"
                                """,
                        2,
                        35,
                        "Field `surname` not found in type `Foo`"
                ),
                Arguments.of(
                        "interpolation_incomplete_field_access",
                        """
                                data Foo { name: string }
                                fun foo(foo: Foo): string = \"Hello {foo.}\"
                                """,
                        2,
                        35,
                        "Cannot evaluate expression"
                )
        );
    }

    private static SortedSet<Result.Error.SingleError> compileProgram(String fun, String moduleName) {
        var programResult = CapybaraCompiler.INSTANCE.compile(
                java.util.List.of(new RawModule(moduleName, "/foo/boo", fun)),
                new TreeSet<>()
        );
        if (programResult instanceof Result.Success<CompiledProgram> value) {
            throw new AssertionError("Expected compilation error but got CompiledProgram: " + value);
        }
        return ((Result.Error<?>) programResult).errors();
    }
}
