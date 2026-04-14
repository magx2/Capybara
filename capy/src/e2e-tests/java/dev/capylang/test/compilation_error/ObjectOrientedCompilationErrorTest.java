package dev.capylang.test.compilation_error;

import dev.capylang.compiler.CapybaraCompiler;
import dev.capylang.compiler.Result;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.SourceKind;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.TreeSet;

import static org.assertj.core.api.Assertions.assertThat;

class ObjectOrientedCompilationErrorTest {
    @Test
    void shouldRejectFunKeywordForObjectOrientedMethods() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule(
                        "Printable",
                        "/foo/boo",
                        """
                                interface Printable {
                                    fun print(): string
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                )
        ), new TreeSet<>());

        assertThat(result).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error<?>) result).errors())
                .singleElement()
                .satisfies(error -> {
                    assertThat(error.file()).isEqualTo("/foo/boo/Printable.coo");
                    assertThat(error.message()).contains("line 2");
                });
    }

    @Test
    void shouldReportParserErrorsForMalformedObjectOrientedModules() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule(
                        "Printable",
                        "/foo/boo",
                        """
                                interface Printable {
                                    def print(): string =
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                )
        ), new TreeSet<>());

        assertThat(result).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error<?>) result).errors())
                .singleElement()
                .satisfies(error -> {
                    assertThat(error.file()).isEqualTo("/foo/boo/Printable.coo");
                    assertThat(error.message()).contains("line 2");
                });
    }

    @Test
    void shouldRejectLoopWithoutStatementBlock() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule(
                        "BrokenLoop",
                        "/foo/boo",
                        """
                                class BrokenLoop {
                                    def run(values: list[int]): int {
                                        foreach value in values return value
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
                    assertThat(error.file()).isEqualTo("/foo/boo/BrokenLoop.coo");
                    assertThat(error.message()).contains("line 3:32");
                    assertThat(error.message()).contains("missing '{' at 'return'");
                });
    }
}
