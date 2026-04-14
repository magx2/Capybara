package dev.capylang.test.compilation_error;

import dev.capylang.compiler.CapybaraCompiler;
import org.junit.jupiter.api.Test;
import dev.capylang.compiler.Result;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.SourceKind;

import java.util.List;
import java.util.TreeSet;

import static org.assertj.core.api.Assertions.assertThat;

class ObjectOrientedMethodReturnCompilationErrorTest {
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
                });
    }
}
