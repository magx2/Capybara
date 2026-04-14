package dev.capylang.test.compilation_error;

import dev.capylang.compiler.CapybaraCompiler;
import dev.capylang.compiler.Result;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.SourceKind;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.TreeSet;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;

class ObjectOrientedMethodReturnCompilationErrorTest {
    @ParameterizedTest(name = "{index}: should parse and then fail fast for `{0}.coo`")
    @MethodSource
    void shouldFailFastForReturnBearingMethods(String moduleName, String source) {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule(moduleName, "/foo/boo", source, SourceKind.OBJECT_ORIENTED)
        ), new TreeSet<>());

        assertThat(result).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error<?>) result).errors())
                .singleElement()
                .satisfies(error -> {
                    assertThat(error.file()).isEqualTo("/foo/boo/%s.coo".formatted(moduleName));
                    assertThat(error.message()).contains("Object-oriented `.coo` modules are parsed");
                    assertThat(error.message()).doesNotContain("mismatched input");
                });
    }

    static Stream<Arguments> shouldFailFastForReturnBearingMethods() {
        return Stream.of(
                Arguments.of(
                        "ExpressionReturn",
                        """
                                class ExpressionReturn {
                                    fun one(): int = 1
                                    fun choose(x: int): int =
                                        if x > 0 then x else -x
                                }
                                """
                ),
                Arguments.of(
                        "BlockReturn",
                        """
                                class BlockReturn {
                                    fun first(xs: list[int]): int {
                                        let first: int = xs[0]
                                        return first
                                    }

                                    fun nested(flag: bool, xs: list[int]): int {
                                        if flag {
                                            return match xs with
                                            case _ -> xs[0]
                                        } else {
                                            {
                                                return 0
                                            }
                                        }
                                    }
                                }
                                """
                )
        );
    }

    @Test
    void shouldReportParserErrorForInvalidReturnSyntax() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule(
                        "InvalidReturn",
                        "/foo/boo",
                        """
                                class InvalidReturn {
                                    fun broken(): int {
                                        return
                                    }
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                )
        ), new TreeSet<>());

        assertThat(result).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error<?>) result).errors())
                .singleElement()
                .satisfies(error -> {
                    assertThat(error.file()).isEqualTo("/foo/boo/InvalidReturn.coo");
                    assertThat(error.message()).contains("mismatched input");
                    assertThat(error.message()).doesNotContain("not yet supported by the compiler pipeline");
                });
    }

    @Test
    void shouldReportParserErrorForMalformedIfReturnBranch() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule(
                        "InvalidIfReturn",
                        "/foo/boo",
                        """
                                class InvalidIfReturn {
                                    fun broken(flag: bool): int {
                                        if flag return 1
                                        else {
                                            return 0
                                        }
                                    }
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                )
        ), new TreeSet<>());

        assertThat(result).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error<?>) result).errors())
                .singleElement()
                .satisfies(error -> {
                    assertThat(error.file()).isEqualTo("/foo/boo/InvalidIfReturn.coo");
                    assertThat(error.message()).contains("missing '{' at 'return'");
                    assertThat(error.message()).doesNotContain("not yet supported by the compiler pipeline");
                });
    }
}
