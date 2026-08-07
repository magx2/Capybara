package dev.capylang;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import dev.capylang.compiler.OutputType;
import dev.capylang.generator.Generator;
import dev.capylang.generator.JavaGenerator;
import dev.capylang.generator.JavaScriptGenerator;
import dev.capylang.generator.PythonGenerator;
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
import static org.assertj.core.api.Assertions.assertThatThrownBy;
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
        assertThat(code).contains("__capy_thrown_error");

        var javaScriptCode = JavaScriptGenerator.javaScriptGenerator(program).modules().stream()
                .map(module -> module.code())
                .reduce("", String::concat);
        assertThat(javaScriptCode)
                .contains("__capy_throw")
                .contains("__capy_try")
                .contains("__capy_enrich_thrown_error")
                .contains("function __capy_error(message) { return __capy_error_kind('capy.error', message); }")
                .contains("backend: 'javascript'");

        var pythonCode = PythonGenerator.pythonGenerator(program).modules().stream()
                .map(module -> module.code())
                .reduce("", String::concat);
        assertThat(pythonCode)
                .contains("__capy_throw")
                .contains("__capy_try")
                .contains("__capy_enrich_thrown_error")
                .contains("def __capy_error(message):\n    return __capy_error_kind('capy.error', message)")
                .contains("backend='python'");
    }

    @Test
    void shouldGenerateResultReducerErrorCallbacksWithStructuredError() {
        var resultSource = """
                data Error { kind: String, message: String }
                data Success[T] { value: T }
                union Result[T] = Success[T] | Error
                fun fail_kind(kind: String, message: String): Result[String] = Error { kind: kind, message: message }
                """;
        var consumerSource = """
                from /sample/lib/Result import { * }

                fun reducer_error_kind(): String =
                    fail_kind("capy.test.result.boom", "boom").reduce(_ => "success", error => error.kind)
                """;
        var program = compileProgram(List.of(
                rawModule("Result", "/sample/lib", resultSource, SourceKind.FUNCTIONAL),
                rawModule("UseResult", "/sample/app", consumerSource, SourceKind.FUNCTIONAL)
        ));

        var code = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/UseResult.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code).contains("__capy_result_error_value");
        assertThat(code).contains("__capy_data_field(error, \"kind\")");
        assertThat(code).doesNotContain("java.lang.String error = java.lang.String.valueOf(__capy_result_error_value");
    }

    @Test
    void shouldUseDeclaredFunctionalLambdaParameterTypes() {
        var source = """
                fun typed_map(values: List[String]): Seq[String] =
                    values | value: String => value + "!"

                fun typed_pair(values: Dict[int]): Dict[String] =
                    values | (key: String, value: int) => key + value
                """;
        var program = compileProgram(List.of(rawModule("TypedLambda", "/sample/app", source, SourceKind.FUNCTIONAL)));

        var code = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/TypedLambda.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code).doesNotContain("throw new UnsupportedOperationException(\"Unsupported CFUN expression at");
        assertThat(code).contains("value + \"!\"");
        assertThat(code).contains("java.lang.String key");
        assertThat(code).contains("int value");
    }

    @Test
    void shouldGenerateImportedUnqualifiedEnumValuesInJava() {
        var orderingSource = """
                enum Ordering { LESS, EQUAL, GREATER }
                """;
        var seqSource = """
                fun Seq[T].drop_until(pred: T => bool): Seq[T] = this
                """;
        var pathSource = """
                data Path { value: String }
                fun Path.`/`(segment: String): Path = this
                fun Path.`/`(other: Path): Path = this
                fun Path.normalize(): Path = this
                """;
        var dateTimeSource = """
                data DateTime {}
                fun from_timestamp(value: long): DateTime = DateTime {}
                fun DateTime.to_iso_8601(): String = "timestamp"
                """;
        var comparisonSource = """
                from /sample/lib/Ordering import { Ordering, EQUAL, GREATER }
                from /capy/collection/Seq import { * }
                from /capy/io/Path import { * }
                from /capy/date_time/DateTime import { * }
                import /capy/lang/Effect

                data Version { value: int }
                data Error { message: String }
                data Success[T] { value: T }
                union Result[T] = Success[T] | Error
                data Assertion { result: bool }
                data Check { assertions: List[() => Assertion] }
                data TestOutput { path: Path, content: String }

                fun compare_core(left: Version, right: Version): Ordering = GREATER

                fun compare_details(left: Version, right: Version): Ordering = GREATER

                fun compare_strings(left: String, right: String): Ordering = left.compare(right)

                fun pipe_then_or(result: Result[String]): Result[String] =
                    result
                    | (value => value)
                    .or(Success { "fallback" })

                fun unwrap_effects(effects: List[Effect[String]]): Effect[List[String]] =
                    match effects[0] with
                    case None -> Effect.pure([])
                    case Some { effect } -> {
                        let value <- effect
                        let rest <- unwrap_effects(effects[1:])
                        [value] + rest
                    }

                fun invoke_suppliers(suppliers: List[() => Assertion]): Option[Assertion] =
                    to_seq(suppliers)
                    | (supplier => supplier())
                    .drop_until(assertion => !assertion.result)
                    .first()

                fun create_parent(path: Path): Effect[Result[Path]] =
                    Effect.pure(Success { path })

                fun write_changed(path: Path, content: String): Effect[Result[String]] =
                    Effect.pure(Success { content })

                fun write_output(output_dir: Path, test_output: TestOutput): Effect[Result[Path]] =
                    let relative_path = test_output.path.normalize()
                    let output_path = output_dir / relative_path
                    create_parent(output_path).flat_map(parent_result =>
                        match parent_result with
                        case Error e -> Effect.pure(e)
                        case Success _ ->
                            write_changed(output_path, test_output.content).map(write_result =>
                                match write_result with
                                case Error e -> e
                                case Success _ -> Success { relative_path }
                            )
                    )

                fun normalize_paths(result: Result[List[Path]]): Result[List[Path]] =
                    match result with
                    case Error e -> e
                    case Success { paths } ->
                        let normalized = (paths | path => path.normalize()).as_list()
                        Success { normalized }

                fun timestamp(value: long): String = from_timestamp(value).to_iso_8601()

                fun check_error(result: Result[String], check: Error => Check): List[() => Assertion] =
                    match result with
                    case Error e -> check(e).assertions
                    case Success _ -> []

                fun join_values(values: List[String]): String =
                    (values |> "", (acc, item) => if acc == "" then item else acc + "," + item)

                fun contains_exponent(value: String): bool = value == "" | value ? "E" | value ? "e"

                fun escaped_controls(): String = "\\n\\r\\t"

                fun Version.compare(other: Version): Ordering =
                    match compare_core(this, other) with
                    case EQUAL -> compare_details(this, other)
                    case order -> order

                fun Version.`>`(other: Version): bool = this.compare(other) == GREATER
                """;
        var program = compileProgram(List.of(
                rawModule("Ordering", "/sample/lib", orderingSource, SourceKind.FUNCTIONAL),
                rawModule("Seq", "/capy/collection", seqSource, SourceKind.FUNCTIONAL),
                rawModule("Path", "/capy/io", pathSource, SourceKind.FUNCTIONAL),
                rawModule("DateTime", "/capy/date_time", dateTimeSource, SourceKind.FUNCTIONAL),
                rawModule("Comparison", "/sample/app", comparisonSource, SourceKind.FUNCTIONAL)
        ));

        var code = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/Comparison.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code)
                .contains("sample.lib.Ordering.GREATER")
                .contains("import static sample.lib.Ordering.GREATER;")
                .contains(".compareTo(")
                .contains("capy.lang.Ordering.EQUAL")
                .contains("import static capy.collection.Seq.*;")
                .contains("__capy_result_or_")
                .contains("flatMap(")
                .contains("supplier.get()")
                .contains(".contains(\"E\")")
                .contains("return \"\\n\\r\\t\";")
                .doesNotContain("Ordering.GREATER__")
                .doesNotContain("throw new UnsupportedOperationException(\"Unsupported CFUN expression at");
    }

    @Test
    void shouldGenerateJavaProgramMainEntrypoint() {
        var source = """
                from /capy/lang/Effect import { Effect, pure }
                from /capy/lang/Program import { Program, Success }

                fun main(args: List[String]): Effect[Program] =
                    pure(Success {})
                """;
        var program = compileProgram(List.of(rawModule("Main", "/sample/app", source, SourceKind.FUNCTIONAL)));

        var code = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/Main.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code)
                .contains("public static capy.lang.Effect<capy.lang.Program> main(java.util.List<java.lang.String> args)")
                .containsSubsequence(
                        "public static final void main(java.lang.String... args)",
                        "var __capybaraArgsList = java.util.List.of(args);",
                        "capy.lang.Program __capybaraProgram = main(__capybaraArgsList).unsafeRun();",
                        "if (__capybaraProgram instanceof capy.lang.Program.Failed __capybaraFailed)",
                        "java.lang.System.exit(__capybaraFailed.exit_code());"
                );
    }

    @Test
    void shouldGenerateQualifiedStandardLibraryCalls() {
        var source = """
                import /capy/lang/Effect
                import /capy/io/Console
                import /capy/lang/Primitives
                from /capy/lang/Option import { Some, None }
                import /capy/lang/Async
                from /capy/lang/Async import { Async }
                import /capy/lang/System
                import /capy/lang/Result
                from /capy/lang/Result import { Error }
                import /capy/lang/Math
                import /capy/io/Path
                import /capy/io/IO
                import /capy/collection/Seq
                import /capy/collection/List

                private fun pure(value: String): String = value

                fun qualified_pure(value: String): Effect[String] =
                    Effect.pure(value)

                fun qualified_delay(value: String): Effect[String] =
                    Effect.delay(() => value)

                fun qualified_print(value: String): Effect[String] =
                    Console.println(value)

                fun qualified_parse(value: String): /capy/lang/Result[int] =
                    Primitives.to_int(value)

                fun qualified_compute(value: int): Async[int] =
                    Async.compute(() => value + 1)

                fun qualified_millis(): Effect[long] =
                    System.current_millis()

                fun qualified_property() =
                    System.system_property("java.version")

                fun qualified_digits(value: int): int =
                    Math.digits(value)

                fun qualified_error(value: String): Error =
                    Result.error(value)

                fun qualified_path(value: String): Path =
                    Path.from_string(value)

                fun qualified_exists(path: Path): Effect[bool] =
                    IO.exists(path)

                fun qualified_seq(values: List[int]): Seq[int] =
                    Seq.to_seq(values)
                """;
        var program = compileProgram(List.of(rawModule(
                "QualifiedEffect",
                "/sample/app",
                source,
                SourceKind.FUNCTIONAL
        )));

        var code = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/QualifiedEffect.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code)
                .contains("return capy.lang.Effect.pure(value);")
                .contains("return capy.lang.Effect.delay(")
                .contains("dev.capylang.ConsoleUtil.println")
                .contains("return __capy_parse_int(value);")
                .contains("return capy.lang.Async.start(capy.lang.Effect.delay(")
                .contains("return capy.lang.System.currentMillis();")
                .contains("return capy.lang.System.systemProperty(\"java.version\");")
                .contains("java.lang.Integer.toString(value)")
                .contains("return __capy_error(value);")
                .contains("dev.capylang.PathUtil.fromString(value)")
                .contains("return capy.io.IO.exists(")
                .contains("return capy.collection.Seq.toSeq(values);")
                .doesNotContain("throw new UnsupportedOperationException(\"Unsupported CFUN expression at");
    }

    @Test
    void shouldFailJavaGenerationInsteadOfEmittingUnsupportedFunctionStubs() {
        var source = "fun broken(): int = missing()";
        var program = compileProgram(List.of(rawModule("Broken", "/sample/app", source, SourceKind.FUNCTIONAL)));

        assertThatThrownBy(() -> JavaGenerator.javaGenerator(program))
                .isInstanceOf(IllegalStateException.class)
                .hasMessage("Java generation failed for `sample/app/Broken.cfun` at 1:20 in function `broken`: "
                        + "unresolved function call `missing`. No Java source was written for this module.");
    }

    @Test
    void shouldGeneratePrimitiveBackedOperatorsAndStandardLibraryCalls() {
        var source = """
                from /capy/lang/Async import { Async, compute }
                from /capy/lang/Primitives import { to_int }
                from /capy/lang/Result import { Result }

                type digit -> int

                fun add(left: digit, right: digit): int = left + right
                fun compare(left: digit, right: digit): bool = left > right
                fun render(value: digit): String = value.to_string()
                fun parse(value: String): Result[int] = to_int(value)
                fun background(value: digit): Async[int] = compute(() => value + 1)
                """;
        var program = compileProgram(List.of(rawModule("BackendCoverage", "/sample/app", source, SourceKind.FUNCTIONAL)));

        var code = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/BackendCoverage.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code).doesNotContain("throw new UnsupportedOperationException(\"Unsupported CFUN expression at");
        assertThat(code).contains("__capy_parse_int(value)");
        assertThat(code).contains("capy.lang.Async.start(capy.lang.Effect.delay(");
        assertThat(code).contains("java.lang.String.valueOf(");
        assertThat(code).contains("__capy_data_field(left, \"value\")");
    }

    @Test
    void shouldGeneratePrimitiveBackedDataFieldsAndConstructorConditions() {
        var source = """
                data Error { kind: String, message: String }
                data Success[T] { value: T }
                union Result[T] = Success[T] | Error

                type digit -> int with constructor {
                    if value >= 0 & value <= 9
                    then Success { value }
                    else Error { kind: "invalid", message: "invalid digit" }
                }

                data Digits {
                    first: digit,
                    second: digit,
                    third: digit,
                    fourth: digit,
                } with constructor {
                    if first == second & second == third & third == fourth
                    then Error { kind: "invalid", message: "all digits match" }
                    else Success { * { first, second, third, fourth } }
                }

                fun total(value: Digits): int =
                    value.first * 1000 + value.second * 100 + value.third * 10 + value.fourth

                fun same(left: Digits, right: Digits): bool =
                    left.first == right.first & left.second == right.second

                fun render(value: Digits): String =
                    value.first.to_string() + value.second.to_string()

                fun from_list(values: List[digit]): Digits =
                    Digits! { values[0], values[1], values[2], values[3] }

                fun Digits.to_int(): int =
                    this.first * 1000 + this.second * 100 + this.third * 10 + this.fourth

                fun Digits.diff(): Digits =
                    let ordered = this
                    let diff: int = ordered.to_int() - ordered.to_int()
                    let a = diff / 1000
                    let b = (diff / 100) % 10
                    let c = (diff / 10) % 10
                    let d = diff % 10
                    Digits! { a, b, c, d }
                """;
        var program = compileProgram(List.of(rawModule("PrimitiveData", "/sample/app", source, SourceKind.FUNCTIONAL)));

        var code = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/PrimitiveData.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code).doesNotContain("throw new UnsupportedOperationException(\"Unsupported CFUN expression at");
        assertThat(code).contains("__capy_data_field(value, \"first\")");
        assertThat(code).contains("__capy_list_get_optional(values, 0)");
    }

    @Test
    void shouldLowerDynamicBackendResultAndImportedPrimitiveOperations() {
        var resultSource = """
                data Error { kind: String, message: String }
                data Success[T] { value: T }
                union Result[T] = Success[T] | Error
                """;
        var digitSource = """
                from /capy/lang/Result import { Success }

                type digit -> int with constructor {
                    Success { value }
                }

                data Item { value: int }

                fun Item.to_string(): String = this.value.to_string()

                fun digit.render(): String = this.to_string()

                fun raw_digit(): digit = digit! { 1 }
                """;
        var consumerSource = """
                from /capy/lang/Result import { Result, Success }
                from /sample/Digit import { digit, Item }
                import /capy/lang/Effect
                import /capy/io/Console
                import /capy/lang/Primitives

                fun parse_digits(values: List[int]): Result[List[digit]] =
                    values
                    | value => Success { value }
                    |> Success { [] }, (acc, int_result) => {
                        acc.flat_map(acc_list => {
                            int_result.flat_map(int_value => {
                                digit { int_value }.map(digit => acc_list + digit)
                            })
                        })
                    }

                fun map_results(values: List[Result[int]]): List[Result[int]] =
                    (values | result => result.map(value => value)).as_list()

                fun flat_map_results(values: List[Result[int]]): List[Result[int]] =
                    (values | result => result.flat_map(value => Success { value })).as_list()

                fun qualified_pure(value: String): Effect[String] = Effect.pure(value)

                fun qualified_print(value: String): Effect[String] = Console.println(value)

                fun qualified_parse(value: String): Result[int] = Primitives.to_int(value)

                fun render_items(result: Result[List[Item]]): List[String] =
                    match result with
                    case Success { value } -> (value | item => item.to_string()).as_list()
                    case Error _ -> []

                fun first_or_zero(values: List[int]): int =
                    match values[0] with
                    case Some { value } -> value
                    case None -> 0

                fun join(values: List[String]): String =
                    values.reduce("", (acc, value) => if acc.is_empty() then value else acc + value)
                """;
        var program = compileProgram(List.of(
                rawModule("Result", "/capy/lang", resultSource, SourceKind.FUNCTIONAL),
                rawModule("Digit", "/sample", digitSource, SourceKind.FUNCTIONAL),
                rawModule("UseDigit", "/sample", consumerSource, SourceKind.FUNCTIONAL)
        ));

        var generated = PythonGenerator.pythonGenerator(program);
        var code = generated.modules().stream()
                .filter(module -> module.relativePath().equals("sample/UseDigit.py"))
                .findFirst()
                .orElseThrow()
                .code();
        var digitCode = generated.modules().stream()
                .filter(module -> module.relativePath().equals("sample/Digit.py"))
                .findFirst()
                .orElseThrow()
                .code();
        var runtimeCode = generated.modules().stream()
                .filter(module -> module.relativePath().equals("capy/test/CapyTestRuntime.py"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code)
                .contains("__capy_result_flat_map(acc")
                .contains("__capy_result_flat_map(int_result")
                .contains("__capy_result_map(__capy_primitive_constructor_result(__import__(\"sample.Digit\", fromlist=['*'])")
                .contains("__capy_result_map(result")
                .contains("__capy_result_flat_map(result")
                .contains("return pure(value)")
                .contains("return println(value)")
                .contains("return __capy_parse_int(value)")
                .contains(" = sample.Digit.Item_to_string__")
                .contains("Item_to_string__")
                .contains("__capy_index(values, 0)")
                .doesNotContain("Effect_pure")
                .doesNotContain("Console_println")
                .doesNotContain("Primitives_to_int")
                .doesNotContain("__capy_dynamic_map(result")
                .doesNotContain("__capy_dynamic_flat_map(result")
                .doesNotContain("__capy_seq_flat_map(acc")
                .doesNotContain("__capy_seq_flat_map(int_result")
                .doesNotContain("int_value.map(");
        assertThat(digitCode).contains("return __capy_to_string(this)");
        assertThat(runtimeCode).contains("end=chr(10) if newline else ''");

        var javaScriptGenerated = JavaScriptGenerator.javaScriptGenerator(program);
        var javaScriptCode = javaScriptGenerated.modules().stream()
                .filter(module -> module.relativePath().equals("sample/UseDigit.js"))
                .findFirst()
                .orElseThrow()
                .code();
        var javaScriptDigitCode = javaScriptGenerated.modules().stream()
                .filter(module -> module.relativePath().equals("sample/Digit.js"))
                .findFirst()
                .orElseThrow()
                .code();
        var javaScriptRuntimeCode = javaScriptGenerated.modules().stream()
                .filter(module -> module.relativePath().equals("capy/test/CapyTestRuntime.js"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(javaScriptCode)
                .contains("__capy_primitive_constructor_result(require(\"../sample/Digit\")[")
                .contains("return pure(value)")
                .contains("return println(value)")
                .contains("return __capy_parse_int(value)")
                .contains("Item_to_string__")
                .contains("(__capy_size(acc) === 0)")
                .doesNotContain("__capy_import_capy_lang_Effect")
                .doesNotContain("__capy_import_capy_io_Console")
                .doesNotContain("__capy_import_capy_lang_Primitives")
                .doesNotContain("Effect_pure")
                .doesNotContain("Console_println")
                .doesNotContain("Primitives_to_int")
                .doesNotContain("item.to_string(")
                .doesNotContain("acc.is_empty(");
        assertThat(javaScriptDigitCode)
                .contains("return __capy_to_string(this_)")
                .contains("return 1")
                .contains("\"__capy_constructor_digit");
        assertThat(javaScriptRuntimeCode)
                .contains("function __capy_primitive_constructor_result(value)")
                .contains("String.fromCharCode(10)")
                .doesNotContain("split(/\\\n");
    }

    @Test
    void shouldKeepIndexExpressionsBeforePipeLambdas() {
        var source = """
                from /capy/lang/Option import { Option, Some }

                fun second(values: List[int]): Option[int] =
                    values[1] | value => Some { value }
                """;
        var program = compileProgram(List.of(rawModule("IndexedPipe", "/sample/app", source, SourceKind.FUNCTIONAL)));

        var code = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/IndexedPipe.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code).doesNotContain("throw new UnsupportedOperationException(\"Unsupported CFUN expression at");
        assertThat(code).contains("__capy_list_get_optional(values, 1)");
    }

    @Test
    void shouldGenerateFlatPythonStringConcatenation() {
        var source = """
                fun pieces(a: String, b: String, c: String, d: String): String =
                    a + "-" + b + "-" + c + "-" + d
                """;
        var program = compileProgram(List.of(rawModule("StringConcat", "/sample/app", source, SourceKind.FUNCTIONAL)));

        var code = PythonGenerator.pythonGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/StringConcat.py"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code).contains("return \"\".join([a, \"-\", b, \"-\", c, \"-\", d])");
    }

    @Test
    void shouldGeneratePythonCollectionReduceOperator() {
        var source = """
                fun joined(values: List[String]): String =
                    values |> "", (acc, value) => acc + value
                """;
        var program = compileProgram(List.of(rawModule("CollectionReduce", "/sample/app", source, SourceKind.FUNCTIONAL)));

        var code = PythonGenerator.pythonGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/CollectionReduce.py"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code).contains("__capy_reduce(");
        assertThat(code).doesNotContain("|>");
    }

    @Test
    void shouldGenerateUnionParentFieldsInJavaDataDeclarations() {
        var source = """
                union A { a: String } = B | C

                data B { x: int }
                data C { y: String }
                """;
        var program = compileProgram(List.of(rawModule("Main", "/sample/app", source, SourceKind.FUNCTIONAL)));

        var code = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/Main.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code).contains("""
                    public sealed interface A {
                        public String a();
                    }
                """);
        assertThat(code).contains("    public record B(String a, int x) implements A {}");
        assertThat(code).contains("    public record C(String a, String y) implements A {}");
    }

    @Test
    void shouldAvoidSealingUnionWithExternalJavaVariants() {
        var ownerSource = """
                union A { a: String } = B

                data B { b: String }
                """;
        var extensionSource = """
                from /sample/owner/Owner import { A }

                data C { ... A, c: String }
                """;
        var program = compileProgram(List.of(
                rawModule("Owner", "/sample/owner", ownerSource, SourceKind.FUNCTIONAL),
                rawModule("Extension", "/sample/ext", extensionSource, SourceKind.FUNCTIONAL)
        ));

        var generated = JavaGenerator.javaGenerator(program);
        var ownerCode = generated.modules().stream()
                .filter(module -> module.relativePath().equals("sample/owner/Owner.java"))
                .findFirst()
                .orElseThrow()
                .code();
        var extensionCode = generated.modules().stream()
                .filter(module -> module.relativePath().equals("sample/ext/Extension.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(ownerCode).contains("    public interface A {");
        assertThat(ownerCode).doesNotContain("public sealed interface A");
        assertThat(ownerCode).contains("    public record B(String a, String b) implements A {}");
        assertThat(extensionCode).contains("    public record C(String a, String c) implements sample.owner.Owner.A {}");
    }

    @Test
    void shouldGenerateGrandparentUnionFieldsInJavaDataDeclarations() {
        var source = """
                union A { a: String } = B

                union B { b: String } = C

                data C { c: String }
                """;
        var program = compileProgram(List.of(rawModule("Main", "/sample/app", source, SourceKind.FUNCTIONAL)));

        var code = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/Main.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code).contains("""
                    public sealed interface A {
                        public String a();
                    }
                """);
        assertThat(code).contains("    public sealed interface B extends A {");
        assertThat(code).contains("        public String b();");
        assertThat(code).contains("    public record C(String a, String b, String c) implements B {}");
    }

    @Test
    void shouldPreservePrimitiveBackedFieldTypesInJavaDataDeclarations() {
        var programSource = """
                union Program = Success | Failed

                data Success {}
                data Failed { exit_code: failed_exit_code }

                type failed_exit_code -> int
                """;
        var consumerSource = """
                from /capy/lang/Program import { * }

                fun fail(): Program = Failed { exit_code: 1 }
                """;
        var program = compileProgram(List.of(
                rawModule("Program", "/capy/lang", programSource, SourceKind.FUNCTIONAL),
                rawModule("Main", "/sample/app", consumerSource, SourceKind.FUNCTIONAL)
        ));

        var generated = JavaGenerator.javaGenerator(program);
        var programCode = generated.modules().stream()
                .filter(module -> module.relativePath().equals("capy/lang/Program.java"))
                .findFirst()
                .orElseThrow()
                .code();
        var consumerCode = generated.modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/Main.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(programCode).contains("    public record Failed(int exit_code) {}");
        assertThat(programCode).doesNotContain("record failed_exit_code");
        assertThat(consumerCode).contains("new capy.lang.Program.Failed(1)");
    }

    @Test
    void shouldRejectObjectOrientedThrowingNonError() {
        var objectSource = """
                class BadThrow {
                    def fail(): String {
                        throw "boom"
                    }
                }
                """;
        var result = CapybaraCompiler.compile(
                List.of(rawModule("BadThrow", "/sample/app", objectSource, SourceKind.OBJECT_ORIENTED)),
                new LinkedHashSet<>(),
                emptyNativeProviders(),
                emptyNativeProviders()
        ).unsafeRun();

        assertThat(result).isInstanceOf(Either.Right.class);
        var errors = (List<?>) ((Either.Right<?, ?>) result).value();
        assertThat(errors.toString()).contains("OO throw expression must have type `/capy/lang/Result.Error`.");
    }

    @Test
    void shouldRejectObjectOrientedNonFinalIfThatFallsThrough() {
        var objectSource = """
                class BadIf {
                    def label(): String {
                        return "side"
                    }

                    def run(flag: bool): String {
                        if flag {
                            this.label()
                        }
                        return "ok"
                    }
                }
                """;
        var result = CapybaraCompiler.compile(
                List.of(rawModule("BadIf", "/sample/app", objectSource, SourceKind.OBJECT_ORIENTED)),
                new LinkedHashSet<>(),
                emptyNativeProviders(),
                emptyNativeProviders()
        ).unsafeRun();

        assertThat(result).isInstanceOf(Either.Right.class);
        var errors = (List<?>) ((Either.Right<?, ?>) result).value();
        assertThat(errors.toString()).contains("Unsupported object-oriented construct");
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
