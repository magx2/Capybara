package pl.grzeslowski.capybara.test.compilation_error;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import pl.grzeslowski.capybara.compiler.Module;
import pl.grzeslowski.capybara.compiler.Program;
import pl.grzeslowski.capybara.linker.CapybaraLinker;
import pl.grzeslowski.capybara.linker.LinkedProgram;
import pl.grzeslowski.capybara.linker.ValueOrError;
import pl.grzeslowski.capybara.parser.CapybaraParser;

import java.util.List;
import java.util.SortedSet;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertAll;

public class CompilationErrorTest {
    @ParameterizedTest(name = "{index}: should fail when compiling `{0}.cfun`")
    @MethodSource
    void compilationError(
            String moduleName,
            String code,
            Position expectedPosition,
            String errorMessage) {
        // when
        var errors = compileProgram(code, moduleName);

        // then
        assertThat(errors).hasSize(1);
        var error = errors.first();
        assertAll("Assertions on error",
                () -> assertThat(error.line()).isEqualTo(expectedPosition.line()),
                () -> assertThat(error.column()).isEqualTo(expectedPosition.column()),
                () -> assertThat(error.file()).isEqualTo("/foo/boo/%s.cfun".formatted(moduleName)));
        assertThat(error.message()).isEqualTo(errorMessage);
    }


    static Stream<Arguments> compilationError() {
        return Stream.concat(simpleCompilationError(), multilineCompilationError());
    }

    static Stream<Arguments> simpleCompilationError() {
        return Stream.of(
                Arguments.of(
                        "function_wrong_return_type",
                        "fun foo(x: int): int = \"boo\"",
                        new Position(1, 23),
                        """
                                error: mismatched types
                                 --> /foo/boo/function_wrong_return_type.cfun:1:23
                                fun foo(x: int): int = "boo"
                                                       ^ expected `int`, found `string`
                                """
                ),
                Arguments.of(
                        "function_wrong_return_type_bool",
                        "fun foo(x: int): int = true",
                        new Position(1, 23),
                        """
                                error: mismatched types
                                 --> /foo/boo/function_wrong_return_type_bool.cfun:1:23
                                fun foo(x: int): int = true
                                                       ^ expected `int`, found `bool`
                                """
                ),
                Arguments.of(
                        "function_wrong_return_type_long",
                        "fun foo(x: int): int = 1L",
                        new Position(1, 23),
                        """
                                error: mismatched types
                                 --> /foo/boo/function_wrong_return_type_long.cfun:1:23
                                fun foo(x: int): int = 1L
                                                       ^ expected `int`, found `long`
                                """
                ),
                Arguments.of(
                        "function_wrong_return_type_float",
                        "fun foo(x: int): int = 1.5f",
                        new Position(1, 23),
                        """
                                error: mismatched types
                                 --> /foo/boo/function_wrong_return_type_float.cfun:1:23
                                fun foo(x: int): int = 1.5f
                                                       ^ expected `int`, found `float`
                                """
                ),
                Arguments.of(
                        "function_wrong_return_type_string",
                        "fun foo(x: int): string = 1",
                        new Position(1, 26),
                        """
                                error: mismatched types
                                 --> /foo/boo/function_wrong_return_type_string.cfun:1:26
                                fun foo(x: int): string = 1
                                                          ^ expected `string`, found `int`
                                """
                )
        );
    }

    static Stream<Arguments> multilineCompilationError() {
        return Stream.of(
                Arguments.of(
                        "multiline_function_wrong_return_type",
                        """
                                // normal comment
                                data Foo {
                                    option: string,
                                    x: int
                                }
                                
                                /// some documentation
                                /// line 2 of doc
                                fun foo(x: int): int =
                                    let x = Foo {
                                        "boo",
                                        5
                                    }
                                    "boo"
                                """,
                        new Position(14, 4),
                        """
                                error: mismatched types
                                 --> /foo/boo/multiline_function_wrong_return_type.cfun:14:4
                                  fun foo(x: int): int =
                                    let x = Foo {
                                        "boo",
                                        5
                                    }
                                    "boo"
                                    ^ expected `int`, found `string`
                                """
                ),
                Arguments.of(
                        "multiline_let_chain_wrong_return_type",
                        """
                                /// docs
                                fun foo(x: int): int =
                                    let a = x + 1
                                    let b = a + 2
                                    "x"
                                """,
                        new Position(5, 4),
                        """
                                error: mismatched types
                                 --> /foo/boo/multiline_let_chain_wrong_return_type.cfun:5:4
                                  fun foo(x: int): int =
                                    let a = x+1
                                    let b = a+2
                                    "x"
                                    ^ expected `int`, found `string`
                                """
                ),
                Arguments.of(
                        "multiline_data_return_wrong_type",
                        """
                                data Bar { value: int }
                                
                                fun foo(x: int): Bar =
                                    let y = x + 1
                                    y
                                """,
                        new Position(5, 4),
                        """
                                error: mismatched types
                                 --> /foo/boo/multiline_data_return_wrong_type.cfun:5:4
                                  fun foo(x: int): Bar =
                                    let y = x+1
                                    y
                                    ^ expected `Bar`, found `int`
                                """
                ),
                Arguments.of(
                        "multiline_new_data_return_wrong_type",
                        """
                                /* block comment */
                                data Foo { a: int, b: string }
                                
                                fun foo(): string =
                                    let f = Foo {
                                        a: 1,
                                        b: "x"
                                    }
                                    f
                                """,
                        new Position(9, 4),
                        """
                                error: mismatched types
                                 --> /foo/boo/multiline_new_data_return_wrong_type.cfun:9:4
                                  fun foo(): string =
                                    let f = Foo {
                                        a: 1,
                                        b: "x"
                                    }
                                    f
                                    ^ expected `string`, found `Foo`
                                """
                )
        );
    }

    private static SortedSet<ValueOrError.Error.SingleError> compileProgram(String fun, String moduleName) {
        var functional = CapybaraParser.INSTANCE.parseFunctional(fun);
        var programValueOrError = CapybaraLinker.INSTANCE.link(new Program(List.of(new Module(moduleName, "/foo/boo", functional))));
        if (programValueOrError instanceof ValueOrError.Value<LinkedProgram> value) {
            throw new AssertionError("Expected compilation error but got LinkedProgram: " + value);
        }
        return ((ValueOrError.Error<?>) programValueOrError).errors();
    }

    record Position(int line, int column) {
    }
}
