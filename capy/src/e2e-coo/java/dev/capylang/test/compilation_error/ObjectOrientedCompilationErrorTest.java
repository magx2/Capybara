package dev.capylang.test.compilation_error;

import dev.capylang.compiler.CapybaraCompiler;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.Result;
import dev.capylang.generator.JavaGenerator;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.SourceKind;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Stream;

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
                                    fun print(): String
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
                                    def print(): String =
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
                                    def run(values: List[int]): int {
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
                                    def mutable(): String {
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
                                    def run(): String {
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
                                    def run(): String {
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
    void shouldRejectObjectOrientedAnnotationTargetMismatch() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule("Annotations", "/foo/meta", "annotation Entity on class {}"),
                new RawModule(
                        "User",
                        "/foo/boo",
                        """
                                from /foo/meta/Annotations import { Entity }

                                class User {
                                    @Entity()
                                    field name: String = "name"
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                )
        ), new TreeSet<>());

        assertThat(result).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error<?>) result).errors())
                .anySatisfy(error -> {
                    assertThat(error.file()).isEqualTo("/foo/boo/User.coo");
                    assertThat(error.message()).contains("Annotation Entity is not valid on field declarations");
                });
    }

    @Test
    void shouldRejectUnknownObjectOrientedAnnotation() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule(
                        "User",
                        "/foo/boo",
                        """
                                @Missing()
                                class User {
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                )
        ), new TreeSet<>());

        assertThat(result).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error<?>) result).errors())
                .anySatisfy(error -> {
                    assertThat(error.file()).isEqualTo("/foo/boo/User.coo");
                    assertThat(error.message()).contains("Unknown annotation Missing");
                });
    }

    @Test
    void shouldRejectRecursiveAnnotationOnObjectOrientedMethod() {
        var errors = compileObjectOrientedSource(
                "Worker",
                """
                        from /capy/meta_prog/Recursive import { Recursive }

                        class Worker {
                            @Recursive
                            def run(): int = this.run()
                        }
                        """,
                List.of(new RawModule("Recursive", "/capy/meta_prog", "annotation Recursive on fun {}"))
        );

        assertThat(errors)
                .anySatisfy(error -> assertThat(error.message())
                        .contains("`@Recursive` is supported for functions, not method declarations"));
    }

    @Test
    void shouldRejectUnimportedObjectOrientedAnnotation() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule("Annotations", "/foo/meta", "annotation Entity on class {}"),
                new RawModule(
                        "User",
                        "/foo/boo",
                        """
                                @Entity()
                                class User {
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                )
        ), new TreeSet<>());

        assertThat(result).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error<?>) result).errors())
                .anySatisfy(error -> {
                    assertThat(error.file()).isEqualTo("/foo/boo/User.coo");
                    assertThat(error.message()).contains("Unknown annotation Entity");
                });
    }

    @ParameterizedTest
    @MethodSource("invalidObjectOrientedAnnotationSyntax")
    void shouldRejectInvalidObjectOrientedAnnotationSyntax(String moduleName, String source) {
        var errors = compileObjectOrientedSource(moduleName, source);

        assertThat(errors).isNotEmpty();
    }

    @ParameterizedTest
    @MethodSource("invalidObjectOrientedAnnotationValidation")
    void shouldRejectInvalidObjectOrientedAnnotationValidation(
            String moduleName,
            String source,
            String expectedMessage
    ) {
        var errors = compileObjectOrientedSource(moduleName, source, List.of(
                new RawModule("Annotations", "/foo/meta", """
                        annotation Label on class {
                            value: String
                        }
                        """)
        ));

        assertThat(errors)
                .anySatisfy(error -> assertThat(error.message()).contains(expectedMessage));
    }

    @Test
    void shouldAllowMainMethodInClassThatRequiresConstructor() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule(
                        "Main",
                        "/foo/boo",
                        """
                                class Main(name: String) {
                                    def main(args: List[String]): int = args.size()
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                )
        ), new TreeSet<>());

        assertThat(result).isInstanceOf(Result.Success.class);
        var program = ((Result.Success<CompiledProgram>) result).value();
        assertThat(new JavaGenerator().generate(program).modules()).isNotEmpty();
    }

    @Test
    void shouldAllowMainMethodThatUsesInstanceState() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule(
                        "Main",
                        "/foo/boo",
                        """
                                class Main {
                                    def helper(): int = 1

                                    def main(args: List[String]): int = args.size() + this.helper()
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                )
        ), new TreeSet<>());

        assertThat(result).isInstanceOf(Result.Success.class);
        var program = ((Result.Success<CompiledProgram>) result).value();
        assertThat(new JavaGenerator().generate(program).modules()).isNotEmpty();
    }

    private static Stream<Arguments> invalidObjectOrientedAnnotationSyntax() {
        return Stream.of(
                Arguments.of(
                        "EmptyAnnotationCall",
                        """
                                @()
                                class Broken {
                                }
                                """
                ),
                Arguments.of(
                        "CommaSeparatedAnnotationPrefix",
                        """
                                @A(), @B()
                                class Broken {
                                }
                                """
                ),
                Arguments.of(
                        "PositionalAnnotationArgument",
                        """
                                @Label("bad")
                                class Broken {
                                }
                                """
                ),
                Arguments.of(
                        "AnnotationAfterDeclarationStart",
                        """
                                class @Label() Broken {
                                }
                                """
                )
        );
    }

    private static Stream<Arguments> invalidObjectOrientedAnnotationValidation() {
        return Stream.of(
                Arguments.of(
                        "MissingAnnotationArgument",
                        """
                                from /foo/meta/Annotations import { Label }

                                @Label()
                                class Broken {
                                }
                                """,
                        "Missing required annotation argument value for Label"
                ),
                Arguments.of(
                        "DuplicateAnnotationArgument",
                        """
                                from /foo/meta/Annotations import { Label }

                                @Label(value: "one", value: "two")
                                class Broken {
                                }
                                """,
                        "Duplicate annotation argument value for Label"
                ),
                Arguments.of(
                        "UnknownAnnotationArgument",
                        """
                                from /foo/meta/Annotations import { Label }

                                @Label(value: "one", extra: "two")
                                class Broken {
                                }
                                """,
                        "Unknown annotation argument extra for Label"
                ),
                Arguments.of(
                        "WrongAnnotationArgumentType",
                        """
                                from /foo/meta/Annotations import { Label }

                                @Label(value: 1)
                                class Broken {
                                }
                                """,
                        "Annotation argument value for Label expects String, got int"
                )
        );
    }

    private static SortedSet<Result.Error.SingleError> compileObjectOrientedSource(String moduleName, String source) {
        return compileObjectOrientedSource(moduleName, source, List.of());
    }

    private static SortedSet<Result.Error.SingleError> compileObjectOrientedSource(
            String moduleName,
            String source,
            List<RawModule> extraModules
    ) {
        var rawModules = new java.util.ArrayList<>(extraModules);
        rawModules.add(new RawModule(moduleName, "/foo/boo", source, SourceKind.OBJECT_ORIENTED));
        var result = CapybaraCompiler.INSTANCE.compile(rawModules, new TreeSet<>());
        if (result instanceof Result.Success<CompiledProgram> value) {
            throw new AssertionError("Expected compilation error but got CompiledProgram: " + value);
        }
        return ((Result.Error<?>) result).errors();
    }

}
