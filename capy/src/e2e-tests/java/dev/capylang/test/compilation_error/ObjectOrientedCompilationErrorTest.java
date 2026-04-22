package dev.capylang.test.compilation_error;

import dev.capylang.compiler.CapybaraCompiler;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.Result;
import dev.capylang.generator.JavaGenerator;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.SourceKind;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.TreeSet;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

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

    @Test
    void shouldRejectAssigningToImmutableLetLocal() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule(
                        "Mutable",
                        "/foo/boo",
                        """
                                class Mutable {
                                    def mutable(): string {
                                        let x = "a"
                                        x = "2"
                                        return x
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
                    assertThat(error.file()).isEqualTo("/foo/boo/Mutable.coo");
                    assertThat(error.message()).contains("immutable local `x`");
                    assertThat(error.message()).contains("use `def` for mutable locals");
                });
    }

    @Test
    void shouldRejectNonCallExpressionStatement() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule(
                        "BrokenStatement",
                        "/foo/boo",
                        """
                                class BrokenStatement {
                                    def run(): int {
                                        let value: int = 1
                                        value
                                        return value
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
                    assertThat(error.file()).isEqualTo("/foo/boo/BrokenStatement.coo");
                    assertThat(error.message()).contains("line 5");
                });
    }

    @Test
    void shouldRejectTryWithoutCatchClause() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule(
                        "BrokenTry",
                        "/foo/boo",
                        """
                                class BrokenTry {
                                    def run(): string {
                                        try {
                                            throw "boom"
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
                    assertThat(error.file()).isEqualTo("/foo/boo/BrokenTry.coo");
                    assertThat(error.message()).contains("expecting 'catch'");
                });
    }

    @Test
    void shouldRejectAssigningToCatchVariable() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule(
                        "BrokenCatch",
                        "/foo/boo",
                        """
                                class BrokenCatch {
                                    def run(): string {
                                        try {
                                            throw "boom"
                                        } catch error {
                                            error = "again"
                                            return error
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
                    assertThat(error.file()).isEqualTo("/foo/boo/BrokenCatch.coo");
                    assertThat(error.message()).contains("immutable local `error`");
                });
    }

    @Test
    void shouldRejectLocalMethodCapturingMutableLocal() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule(
                        "BrokenLocalMethod",
                        "/foo/boo",
                        """
                                class BrokenLocalMethod {
                                    def run(): int {
                                        def total: int = 1
                                        def inc(value: int): int = value + total
                                        return inc(2)
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
                    assertThat(error.file()).isEqualTo("/foo/boo/BrokenLocalMethod.coo");
                    assertThat(error.message()).contains("cannot capture mutable locals");
                    assertThat(error.message()).contains("total");
                });
    }

    @Test
    void shouldRejectEntrypointClassThatRequiresConstructor() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule(
                        "Main",
                        "/foo/boo",
                        """
                                class Main(name: string) {
                                    def main(args: list[string]): int = args.size()
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                )
        ), new TreeSet<>());

        assertThat(result).isInstanceOf(Result.Success.class);
        var program = ((Result.Success<CompiledProgram>) result).value();
        assertThatThrownBy(() -> new JavaGenerator().generate(program))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("Entrypoint class `Main` cannot declare constructor state or init blocks");
    }

    @Test
    void shouldRejectEntrypointMethodThatUsesInstanceState() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule(
                        "Main",
                        "/foo/boo",
                        """
                                class Main {
                                    def helper(): int = 1

                                    def main(args: list[string]): int = args.size() + this.helper()
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                )
        ), new TreeSet<>());

        assertThat(result).isInstanceOf(Result.Success.class);
        var program = ((Result.Success<CompiledProgram>) result).value();
        assertThatThrownBy(() -> new JavaGenerator().generate(program))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("Entrypoint method `Main.main` cannot use instance state");
    }

}
