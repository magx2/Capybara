package dev.capylang;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import dev.capylang.compiler.OutputType;
import dev.capylang.generator.Generator;
import dev.capylang.generator.JavaGenerator;
import dev.capylang.compiler.CapybaraCompiler;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.NativeProviderManifest;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.SourceKind;
import capy.lang.Either;

import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;

class CompilationTest {
    @ParameterizedTest(name = "{index}: should {0}")
    @MethodSource
    void test(String code) {
        var rawModules = List.of(rawModule("Main", "/capybara", code));
        System.out.println(" === PARSING === ");
        System.out.println(rawModules);

        var link = CapybaraCompiler.compile(rawModules, new LinkedHashSet<>(), emptyNativeProviders(), emptyNativeProviders()).unsafeRun();
        if (link instanceof Either.Right<?, ?> error) {
            var errors = (List<?>) error.value();
            throw new RuntimeException("Linking failed with " + errors.size() + " error(s): " + errors);
        }
        System.out.println("\n === LINKING === ");
        System.out.println(link);

        System.out.println("\n === GENERATION === ");
        Arrays.stream(OutputType.values())
                .parallel()
                .map(type -> {
                    var linkedProgram = (CompiledProgram) ((Either.Left<?, ?>) link).value();
                    var compiled = Generator.generate(linkedProgram, generatorOutputType(type));
                    return "\t === " + type + " === \n" + compiled;
                })
                .forEach(System.out::println);

    }

    @Test
    void shouldGenerateObjectOrientedCatchBranchesByErrorKind() {
        var resultSource = """
                data Error { kind: String, message: String }
                fun error_kind(kind: String, message: String): Error = Error { kind: kind, message: message }
                """;
        var objectSource = """
                from /capy/lang/Result import { * }

                class CatchByKind {
                    def recover(): String {
                        try {
                            throw error_kind("capy.test.alpha", "alpha")
                        } catch "capy.test.alpha" error {
                            return "alpha"
                        } catch "capy.test.beta" error {
                            return "beta"
                        } catch error {
                            return "fallback"
                        }
                    }
                }
                """;
        var program = compileProgram(List.of(
                rawModule("Result", "/capy/lang", resultSource, SourceKind.FUNCTIONAL),
                rawModule("CatchByKind", "/sample/app", objectSource, SourceKind.OBJECT_ORIENTED)
        ));

        var generated = JavaGenerator.javaGenerator(program);
        var code = generated.modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/CatchByKind.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code).contains("__capy_error_kind_equals");
        var alphaBranch = code.indexOf("\"capy.test.alpha\"");
        var betaBranch = code.indexOf("\"capy.test.beta\"");
        var fallbackBranch = code.indexOf("\"fallback\"");
        assertThat(alphaBranch).isGreaterThanOrEqualTo(0);
        assertThat(betaBranch).isGreaterThan(alphaBranch);
        assertThat(fallbackBranch).isGreaterThan(betaBranch);
    }

    private static String generatorOutputType(OutputType outputType) {
        return switch (outputType) {
            case JAVA -> "java";
            case PYTHON -> "python";
            case JAVASCRIPT -> "javascript";
        };
    }

    private static RawModule rawModule(String name, String path, String input) {
        return rawModule(name, path, input, SourceKind.FUNCTIONAL);
    }

    private static RawModule rawModule(String name, String path, String input, SourceKind sourceKind) {
        return new RawModule(name, path, input, sourceKind);
    }

    private static CompiledProgram compileProgram(List<RawModule> rawModules) {
        var result = CapybaraCompiler.compile(rawModules, new LinkedHashSet<>(), emptyNativeProviders(), emptyNativeProviders()).unsafeRun();
        if (result instanceof Either.Right<?, ?> error) {
            fail(error.value().toString());
        }
        return (CompiledProgram) ((Either.Left<?, ?>) result).value();
    }

    private static NativeProviderManifest emptyNativeProviders() {
        return new NativeProviderManifest(List.of());
    }

    static Stream<Arguments> test() {
        return Stream.of(
                Arguments.of("""
                                // algebraic type
                                union Shape = Circle | Rectangle
                                data Circle { radius: double }
                                data Rectangle { width: double, height: double }
                                
                                fun area(shape: Shape): double =
                                    match shape with
                                    case Circle { radius } -> 3.14 * radius ^ 2
                                    case Rectangle { width, height } -> width * height
                                
                                fun da_vinci(shape: Shape): Shape =
                                    match shape with
                                    case Circle { radius } -> Rectangle { width : radius * 2, height : radius * 2 }
                                    case Rectangle { width, height } -> Circle { radius : (width + height) / 4 }
                                
                                // type with common value
                                union Person { name: String, age: int } = Student | Teacher
                                data Student { grade: int }
                                data Teacher { subject: String }
                                """,
                        """
                                fun classify(x: int): String =
                                    if x > 0 then "positive"
                                    else "non-positive"
                                fun always_true(): bool = true
                                
                                // algebraic type
                                union Shape = Circle | Rectangle
                                data Circle { radius: double }
                                data Rectangle { width: double, height: double }
                                
                                fun area(shape: Shape): double =
                                    match shape with
                                    case Circle { radius } -> 3.14 * radius * radius
                                    case Rectangle { width, height } -> width * height
                                
                                fun da_vinci(shape: Shape): Shape =
                                    match shape with
                                    case Circle { radius } -> Rectangle { width = radius * 2, height = radius * 2 }
                                    case Rectangle { width, height } -> Circle { radius = (width + height) / 4 }
                                """,
                        """
                                // algebraic type
                                union Shape = Circle | Rectangle
                                data Circle { radius: double }
                                data Rectangle { width: double, height: double }
                                
                                fun area(shape: Shape): double =
                                    match shape with
                                    case Circle { radius } -> 3.14 * radius ^ 2
                                    case Rectangle { width, height } -> width * height
                                
                                fun da_vinci(shape: Shape): Shape =
                                    match shape with
                                    case Circle { radius } -> Rectangle { width : radius * 2, height : radius * 2 }
                                    case Rectangle { width, height } -> Circle { radius : (width + height) / 4 }
                                """,
                        """
                                // type with common value
                                union Person { name: String, age: int } = Student | Teacher
                                data Student { grade: int }
                                data Teacher { subject: String }
                                """,
                        """
                                union Option[T] = Some[T] | None
                                data Some[T] { value: T }
                                data None {}
                                fun tuple(): Tuple[int, String, double] = (1, "foo", 5.0)
                                fun tuple2(): Tuple[int, Option[String], double] = (1, Some { value: "foo" }, 5.0)
                                fun tuple_index(): Option[String] = (1, "foo", 5.0)[1]
                                fun tuple_index_negative(): Option[String] = (1, "foo", 5.0)[-2]
                                fun tuple_slice(): Tuple[String, double] = (1, "foo", 5.0)[1:]
                                fun tuple_slice_negative(): Tuple[int, String] = (1, "foo", 5.0)[:-1]
                                fun tuple_if(x: int): Tuple[int, String, float, String] =
                                    (5, if x > 4 then "big" else "small", 5.1f, "foo")
                                """)
        );
    }
}
