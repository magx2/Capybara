package dev.capylang;

import dev.capylang.cli.Capy;
import dev.capylang.compiler.BackendCompilationContext;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import javax.tools.ToolProvider;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class JavaGenerationDiagnosticsIntegrationTest {
    @TempDir
    Path tempDir;

    @Test
    void compilesJavaForRootModulesWithAbsoluteImports() throws Exception {
        var support = writeSource("Support.cfun", """
                from /capy/collection/Seq import { Seq, Cons, End }

                data Counter { value: int }

                fun increment(value: int): int = value + 1

                fun Counter.doubled(): int = this.value * 2

                fun countdown(value: int): Cons[int] =
                    @/capy/meta_prog/Recursive
                    fun rest(value: int): Seq[int] =
                        if value <= 0
                        then End {}
                        else Cons { value: value, rest: () => rest(value - 1) }
                    ---
                    Cons { value: value, rest: () => rest(value - 1) }
                """);
        var main = writeSource("main.cfun", """
                from /capy/collection/Seq import { Seq }
                from /Support import { Counter, increment, countdown }

                fun answer(): int = increment(41)

                fun doubled(counter: Counter): int = counter.doubled()

                fun values(): Seq[int] = countdown(3)

                fun render(counter: Counter): String = render(counter.value)

                fun render(value: int): String = "value"
                """);

        compileGenerate();

        assertThat(generatedPath(main))
                .content()
                .doesNotContain("import static Support.")
                .contains("Support.increment")
                .contains("Support.Counter_doubled")
                .doesNotContain("__capy_tail_counter");
        var seqRuntime = tempDir.resolve("java-runtime/capy/collection/Seq.java");
        Files.createDirectories(seqRuntime.getParent());
        Files.writeString(seqRuntime, """
                package capy.collection;

                import java.util.List;
                import java.util.function.Supplier;

                public interface Seq<T> {
                    record Cons<T>(T value, Supplier<Seq<T>> rest) implements Seq<T> {}
                    enum End implements Seq<Object> { INSTANCE }
                    static <T> Seq<T> toSeq(Object value) { return (Seq<T>) End.INSTANCE; }
                    default List<T> asList() { return List.of(); }
                }
                """);
        assertJavaCompiles(
                seqRuntime,
                generatedPath(support),
                generatedPath(main)
        );
    }

    @Test
    void generatesQualifiedStandardLibraryCallsThroughCompileGenerate() throws Exception {
        var source = writeSource("sample/QualifiedEffect.cfun", """
                import /capy/lang/Effect
                import /capy/io/Console
                import /capy/lang/Primitives

                fun qualified_pure(value: String): Effect[String] =
                    Effect.pure(value)

                fun qualified_print(value: String): Effect[String] =
                    Console.println(value)

                fun qualified_parse(value: String): /capy/lang/Result[int] =
                    Primitives.to_int(value)
                """);

        compileGenerate();

        assertThat(generatedPath(source))
                .content()
                .contains("return capy.lang.Effect.pure(value);")
                .contains("dev.capylang.ConsoleUtil.println")
                .contains("return __capy_parse_int(value);")
                .doesNotContain("Unsupported CFUN expression at");
    }

    @Test
    void reportsMalformedPipeExpressionAndDoesNotWriteJavaOutput() throws Exception {
        var source = writeSource("sample/PipeFailure.cfun", """
                fun broken(values: List[int]): List[int] =
                    values |* value.to_string()
                """);

        assertThatThrownBy(this::compileGenerate)
                .isInstanceOf(IllegalStateException.class)
                .hasMessage("Java generation failed for `sample/PipeFailure.cfun` at 2:4 in function `broken`: "
                        + "operator `|*` requires a lambda or function reference on its right-hand side; "
                        + "found a method call. No Java source was written for this module.");

        assertThat(generatedPath(source)).doesNotExist();
    }

    @Test
    void reportsUnresolvedCallAndDoesNotWriteJavaOutput() throws Exception {
        var source = writeSource("sample/CallFailure.cfun", """
                fun broken(): int =
                    missing()
                """);

        assertThatThrownBy(this::compileGenerate)
                .isInstanceOf(IllegalStateException.class)
                .hasMessage("Java generation failed for `sample/CallFailure.cfun` at 2:4 in function `broken`: "
                        + "unresolved function call `missing`. No Java source was written for this module.");

        assertThat(generatedPath(source)).doesNotExist();
    }

    @Test
    void doesNotReportImportedConsoleFunctionAsUnresolved() throws Exception {
        var source = writeSource("sample/ImportedConsoleFailure.cfun", """
                from /capy/io/Console import { println }
                from /capy/lang/Effect import { Effect }

                fun broken(value: String): String =
                    let printed: Effect[String] = println(value)
                    missing()
                """);

        assertThatThrownBy(this::compileGenerate)
                .isInstanceOf(IllegalStateException.class)
                .hasMessageContaining("in function `broken`: unresolved function call `missing`")
                .satisfies(exception -> assertThat(exception.getMessage())
                        .doesNotContain("unresolved function call `println`"));

        assertThat(generatedPath(source)).doesNotExist();
    }

    @Test
    void reportsNonCallableFlatMapArgument() throws Exception {
        var source = writeSource("sample/FlatMapFailure.cfun", """
                from /capy/io/Console import { println }
                from /capy/lang/Effect import { Effect, pure }

                fun broken(values: List[String]): Effect[String] =
                    values | value => println(value)
                    |> pure(''), (acc, print_effect) => acc.flat_map(print_effect)
                """);

        assertThatThrownBy(this::compileGenerate)
                .isInstanceOf(IllegalStateException.class)
                .hasMessageContaining("in function `broken`: method `flat_map` requires a callable mapper; "
                        + "variable `print_effect` is not callable in this context")
                .satisfies(exception -> assertThat(exception.getMessage())
                        .doesNotContain("unresolved function call `println`"));

        assertThat(generatedPath(source)).doesNotExist();
    }

    @Test
    void generatesEffectFlatMapCall() throws Exception {
        var source = writeSource("sample/EffectFlatMap.cfun", """
                import /capy/lang/Effect

                fun chained(effect: Effect[int], next: Effect[String]): Effect[String] =
                    effect.flat_map(_ => next)
                """);

        assertThat(compileGenerateStderr()).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .contains("flatMap((__) -> next)")
                .doesNotContain("Unsupported CFUN expression at");
    }

    @Test
    void reportsPipelineVariablesOutsideTheirLambdaScopeDuringCompilation() throws Exception {
        var source = writeSource("sample/UnboundPipelineVariables.cfun", """
                from /capy/lang/Result import { Result, Success }

                fun broken(values: List[Result[int]]): Result[List[int]] =
                    values
                    |> Success { [] }, (acc, int_result) =>
                        acc
                        | acc_list =>
                            int_result
                            | digit => Success { acc_list + digit }
                """);

        assertThat(compileGenerateStderr())
                .contains("Compilation failed with 2 error(s):")
                .contains("Unresolved variable `int_result`.")
                .contains("Unresolved variable `acc_list`.");

        assertThat(generatedPath(source)).doesNotExist();
    }

    @Test
    void generatesNestedPipelinesWhenLambdaBodiesAreGrouped() throws Exception {
        var source = writeSource("sample/GroupedPipelineVariables.cfun", """
                from /capy/lang/Result import { Result, Success }

                fun accumulate(values: List[Result[int]]): Result[List[int]] =
                    values
                    |> Success { [] }, (acc, int_result) => {
                        acc
                        | acc_list => {
                            int_result
                            | digit => Success { acc_list + digit }
                        }
                    }
                """);

        assertThat(compileGenerateStderr()).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .doesNotContain("Unsupported CFUN expression at");
    }

    @Test
    void generatesImportedToIntInsideStringPipe() throws Exception {
        var source = writeSource("sample/StringToInts.cfun", """
                from /capy/lang/Primitives import { to_int }
                from /capy/lang/Result import { Result }

                fun parse_chars(value: String): Seq[Result[int]] =
                    value | char => to_int(char)
                """);

        assertThat(compileGenerateStderr()).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .contains("__capy_parse_int")
                .doesNotContain("Unsupported CFUN expression at");
    }

    @Test
    void generatesGroupedResultReductionFromString() throws Exception {
        var source = writeSource("sample/StringResultReduction.cfun", """
                from /capy/lang/Primitives import { to_int }
                from /capy/lang/Result import { Result, Success }

                fun parse_digits(value: String): Result[List[int]] =
                    value
                    | char => to_int(char)
                    |> Success { [] }, (acc, int_result) => {
                        acc
                        | acc_list => {
                            int_result
                            | digit => Success { acc_list + digit }
                        }
                    }
                """);

        assertThat(compileGenerateStderr()).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .contains("__capy_parse_int")
                .doesNotContain("Unsupported CFUN expression at");
    }

    @Test
    void generatesResultMapAsResultInsideCollection() throws Exception {
        var source = writeSource("sample/ResultMapCollection.cfun", """
                from /capy/lang/Result import { Result }

                fun map_results(values: List[Result[int]]): List[Result[List[int]]] =
                    (values | result => result.map(value => [value])).as_list()
                """);

        assertThat(compileGenerateStderr()).isEmpty();

        var generated = generatedPath(source);
        assertThat(generated)
                .content()
                .contains("return __capy_data(\"Success\", __capy_value_fields(java.util.List.of(value)))")
                .doesNotContain("Unsupported CFUN expression at");
        assertJavaCompiles(generated);
    }

    @Test
    void generatesPrimitiveBackedConstructorFromResultValue() throws Exception {
        var source = writeSource("sample/PrimitiveResult.cfun", """
                from /capy/lang/Result import { Result, Success }

                type digit -> int with constructor {
                    Success { value }
                }

                fun convert(value: Result[int]): Result[digit] =
                    value | int_value => digit { int_value }
                """);

        assertThat(compileGenerateStderr()).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .doesNotContain("Unsupported CFUN expression at");
    }

    @Test
    void wrapsPrimitiveLiteralForPrimitiveBackedDataField() throws Exception {
        var source = writeSource("sample/PrimitiveBackedField.cfun", """
                from /capy/lang/Result import { Success }

                type digit -> int with constructor {
                    Success { value }
                }

                data Code { value: digit }

                const CODE: Code = Code! { value: 6 }
                """);

        assertThat(compileGenerateStderr()).isEmpty();

        var generated = generatedPath(source);
        assertThat(generated)
                .content()
                .contains("__capy_data(\"digit\", java.util.Map.ofEntries(java.util.Map.entry(\"value\", 6)))")
                .doesNotContain("Unsupported CFUN expression at");
        assertJavaCompiles(generated);
    }

    @Test
    void generatesDigitReductionFromString() throws Exception {
        var source = writeSource("sample/StringDigitReduction.cfun", """
                from /capy/lang/Primitives import { to_int }
                from /capy/lang/Result import { Result, Success }

                type digit -> int with constructor {
                    Success { value }
                }

                fun parse_digits(value: String): Result[List[digit]] =
                    value
                    | char => to_int(char)
                    |> Success { [] }, (acc, int_char) => {
                        acc.flat_map(acc_list => {
                            int_char.flat_map(int_value => {
                                digit { int_value }.map(digit => acc_list + digit)
                            })
                        })
                    }
                """);

        assertThat(compileGenerateStderr()).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .contains("__capy_parse_int")
                .doesNotContain("Unsupported CFUN expression at");
    }

    @Test
    void generatesImportedPrimitiveBackedConstructor() throws Exception {
        var importedSource = writeSource("sample/Digit.cfun", """
                from /capy/lang/Result import { Success }

                type digit -> int with constructor {
                    Success { value }
                }
                """);
        var source = writeSource("sample/ImportedDigit.cfun", """
                from /capy/lang/Primitives import { to_int }
                from /capy/lang/Result import { Result, Success }
                from /sample/Digit import { digit }

                fun parse_digits(value: String): Result[List[digit]] =
                    value
                    | char => to_int(char)
                    |> Success { [] }, (acc, int_char) => {
                        acc
                        | acc_list => {
                            int_char
                            | int_value => {
                                digit { int_value }
                                | digit => Success { acc_list + digit }
                            }
                        }
                    }
                """);

        assertThat(compileGenerateStderr()).isEmpty();

        assertThat(generatedPath(importedSource))
                .content()
                .contains("public static java.lang.Object __capy_constructor_digit");

        assertThat(generatedPath(source))
                .content()
                .contains("__capy_parse_int")
                .contains("sample.Digit.")
                .contains("java.lang.Object digit =")
                .contains("__capy_list_append(acc_list, digit)")
                .doesNotContain("int digit =")
                .doesNotContain("__capy_list_append(acc_list, ((java.lang.Integer) __capy_data_field(digit")
                .doesNotContain("Unsupported CFUN expression at");
    }

    @Test
    void generatesExtensionMethodOwnedByExplicitlyImportedType() throws Exception {
        var importedSource = writeSource("sample/Item.cfun", """
                data Item { value: String }

                fun Item.render(): String = this.value
                """);
        var source = writeSource("sample/ImportedItemMethod.cfun", """
                from /sample/Item import { Item }

                fun render(item: Item): String = item.render()
                """);

        assertThat(compileGenerateStderr()).isEmpty();

        assertThat(generatedPath(importedSource))
                .content()
                .contains("public static java.lang.String Item_render");

        assertThat(generatedPath(source))
                .content()
                .contains("import static sample.Item.")
                .doesNotContain("Unsupported CFUN expression at");
    }

    @Test
    void reportsUnsupportedMethodForAnotherKnownReceiverType() throws Exception {
        var source = writeSource("sample/StringMethodFailure.cfun", """
                from /capy/lang/Result import { Result }

                fun broken(value: Result[int]): Result[int] =
                    value.not_a_java_method()
                """);

        assertThat(compileGenerateStderr()).isEqualTo("""
                Compilation failed with 1 error(s):
                /sample/StringMethodFailure.cfun:4:4: Method `not_a_java_method` on `Result` is not supported by the Java backend.
                """);

        assertThat(generatedPath(source)).doesNotExist();
    }

    @Test
    void reportsUnsupportedListMethodDuringCompilation() throws Exception {
        var source = writeSource("sample/ListMethodFailure.cfun", """
                fun broken(values: List[int]): bool =
                    values.length() == 4
                """);

        assertThat(compileGenerateStderr()).isEqualTo("""
                Compilation failed with 1 error(s):
                /sample/ListMethodFailure.cfun:2:4: Method `length` on `List` is not supported by the Java backend.
                """);

        assertThat(generatedPath(source)).doesNotExist();
    }

    @Test
    void reportsNestedLambdaArgumentTypeDuringCompilation() throws Exception {
        var source = writeSource("sample/NestedLambdaTypeFailure.cfun", """
                from /capy/lang/Result import { Result }

                fun render(result: Result[List[int]]): String = ""

                fun broken(result: Result[List[int]]): String =
                    render(result.map(value => item => item))
                """);

        assertThat(compileGenerateStderr()).isEqualTo("""
                Compilation failed with 1 error(s):
                /sample/NestedLambdaTypeFailure.cfun:6:4: Argument 1 of function `render` has type `Result[function]`, but `Result[List[int]]` is required.
                """);

        assertThat(generatedPath(source)).doesNotExist();
    }

    @Test
    void reportsLocalFunctionArgumentTypeDuringCompilationForEveryBackend() throws Exception {
        var source = writeSource("sample/LocalFunctionTypeFailure.cfun", """
                from /capy/collection/Seq import { Seq, Cons, End }

                data Item { value: String }

                private fun render(result: Cons[Item]): String =
                    @/capy/meta_prog/Recursive
                    fun render(result: Seq[Item], acc: String): String =
                        match result with
                        case Cons cons -> render(cons.rest(), acc)
                        case End -> acc
                    ---
                    render(result.rest(), result.value)
                """);

        for (var outputType : List.of("java", "javascript", "python")) {
            assertThat(compileGenerateStderr(outputType)).isEqualTo("""
                    Compilation failed with 1 error(s):
                    /sample/LocalFunctionTypeFailure.cfun:12:4: Argument 2 of function `render` has type `Item`, but `String` is required.
                    """);
        }

        assertThat(generatedPath(source)).doesNotExist();
        assertThat(generatedPath(source, ".js")).doesNotExist();
        assertThat(generatedPath(source, ".py")).doesNotExist();
    }

    @Test
    void reportsInvalidImportedFunctionCallsInTestSourcesDuringCompilationForEveryBackend() throws Exception {
        writeSource("Kaprekar.cfun", """
                data Kaprekar { value: int }

                const KAPREKAR: Kaprekar = Kaprekar { 6174 }
                """);
        var testSource = writeTestSource("Kaprekar.test.cfun", """
                from /Kaprekar import { Kaprekar, KAPREKAR }
                from /capy/collection/Seq import { Seq, to_seq }

                fun wrong_argument_count(): Seq[Kaprekar] =
                    to_seq(KAPREKAR, KAPREKAR, KAPREKAR, KAPREKAR)
                """);

        for (var outputType : List.of("java", "javascript", "python")) {
            assertThat(compileGenerateWithTestsStderr(outputType))
                    .contains("Compilation failed with 1 error(s):")
                    .contains("Function `to_seq` does not accept 4 argument(s).");
        }

        assertThat(generatedTestPath(testSource)).doesNotExist();
    }

    @Test
    void reportsResultFlatMapReturningPlainValueDuringCompilationForEveryBackend() throws Exception {
        writeSource("sample/Values.cfun", """
                fun find_values(value: int): List[int] = [value]
                """);
        var source = writeSource("sample/ResultFlatMapTypeFailure.cfun", """
                from /capy/lang/Result import { Result }
                from /sample/Values import { find_values }

                fun broken(result: Result[int]): Result[List[int]] =
                    result.flat_map(value => find_values(value))
                """);

        for (var outputType : List.of("java", "javascript", "python")) {
            assertThat(compileGenerateStderr(outputType)).isEqualTo("""
                    Compilation failed with 1 error(s):
                    /sample/ResultFlatMapTypeFailure.cfun:5:4: Result.flat_map mapper must return `Result`, but it returns `List[int]`.
                    """);
        }

        assertThat(generatedPath(source)).doesNotExist();
        assertThat(generatedPath(source, ".js")).doesNotExist();
        assertThat(generatedPath(source, ".py")).doesNotExist();
    }

    @Test
    void rejectsImplicitConversionsBetweenSeqAndListForEveryBackend() throws Exception {
        var source = writeSource("sample/DistinctCollections.cfun", """
                fun list_as_seq(values: List[int]): Seq[int] = values

                fun seq_as_list(values: Seq[int]): List[int] = values

                fun consume_list(values: List[int]): String = "done"

                fun wrong_argument(values: Seq[int]): String = consume_list(values)

                fun wrong_binding(values: Seq[int]): List[int] =
                    let copied: List[int] = values
                    copied
                """);

        for (var outputType : List.of("java", "javascript", "python")) {
            assertThat(compileGenerateStderr(outputType)).isEqualTo("""
                    Compilation failed with 4 error(s):
                    /sample/DistinctCollections.cfun:1:0: Function `list_as_seq` returns `List[int]`, but declares `Seq[int]`; use an explicit `to_seq` or `as_list` conversion.
                    /sample/DistinctCollections.cfun:3:0: Function `seq_as_list` returns `Seq[int]`, but declares `List[int]`; use an explicit `to_seq` or `as_list` conversion.
                    /sample/DistinctCollections.cfun:7:47: Argument 1 of function `consume_list` has type `Seq[int]`, but `List[int]` is required; use an explicit `to_seq` or `as_list` conversion.
                    /sample/DistinctCollections.cfun:10:4: Binding `copied` has type `Seq[int]`, but declares `List[int]`; use an explicit `to_seq` or `as_list` conversion.
                    """);
        }

        assertThat(generatedPath(source)).doesNotExist();
        assertThat(generatedPath(source, ".js")).doesNotExist();
        assertThat(generatedPath(source, ".py")).doesNotExist();
    }

    @Test
    void compilesExplicitSeqToListConversionInsideAsyncEffect() throws Exception {
        var source = writeSource("sample/AsyncEffectBind.cfun", """
                import /capy/collection/Seq
                import /capy/lang/Async
                import /capy/lang/Effect
                from /capy/lang/Result import { Result }

                data Item { value: int }

                fun find_items(items: Seq[Item]): List[Item] = items.as_list()

                fun compute_items(result: Result[Seq[Item]]): Async[String] =
                    Async.compute(() => {
                        let mapped: Result[List[Item]] = result.map(items => find_items(items))
                        "done"
                    })

                fun collect(tasks: Seq[Async[String]]): Effect[String] =
                    Async.all(tasks).map(_ => "done")
                """);

        assertThat(compileGenerateStderr()).isEmpty();
        assertThat(generatedPath(source))
                .content()
                .contains("java.lang.Object items = __capy_result_success_value(")
                .contains("capy.lang.Async.all(")
                .contains("tasks).asList())");
        assertJavaCompiles(generatedPath(source));
    }

    @Test
    void generatesSupportedListMethodWithoutFreeFunctionSymbol() throws Exception {
        var source = writeSource("sample/ListSize.cfun", """
                fun count(values: List[int]): int =
                    values.size().value
                """);

        assertThat(compileGenerateStderr()).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .contains("values.size()")
                .doesNotContain("Unsupported CFUN expression at");
    }

    @Test
    void validatesTestProgramBeforeJavaGeneration() throws Exception {
        writeSource("sample/Main.cfun", """
                fun count(values: List[int]): int = values.size().value
                """);
        var testSource = writeTestSource("sample/MainTest.cfun", """
                fun broken(values: List[int]): bool =
                    values.length() == 4
                """);

        assertThat(compileGenerateWithTestsStderr()).isEqualTo("""
                Compilation failed with 1 error(s):
                /sample/MainTest.cfun:2:4: Method `length` on `List` is not supported by the Java backend.
                """);

        assertThat(generatedTestPath(testSource)).doesNotExist();
    }

    @Test
    void allowsJavaUnsupportedMethodForJavaScriptTarget() throws Exception {
        var source = writeSource("sample/EffectFlatMapJavaScript.cfun", """
                import /capy/lang/Effect

                fun supported(effect: Effect[String]): Effect[String] =
                    effect.flat_map(value => Effect.pure(value))
                """);

        assertThat(compileGenerateStderr("javascript")).isEmpty();

        assertThat(generatedPath(source, ".js")).exists();
    }

    @Test
    void reportsCollectionFlatMapReturningResult() throws Exception {
        var source = writeSource("sample/CollectionFlatMapFailure.cfun", """
                from /capy/lang/Result import { Result }

                fun broken(values: List[Result[int]]): Seq[Result[int]] =
                    values |* result => result.map(value => value + 1)
                """);

        assertThatThrownBy(this::compileGenerate)
                .isInstanceOf(IllegalStateException.class)
                .hasMessage("Java generation failed for `sample/CollectionFlatMapFailure.cfun` at 4:4 "
                        + "in function `broken`: collection operator `|*` requires its mapper to return "
                        + "a collection; `Result.map` returns `Result`. No Java source was written for this module.");

        assertThat(generatedPath(source)).doesNotExist();
    }

    private void compileGenerate() {
        compileGenerate("java");
    }

    private void compileGenerate(String outputType) {
        BackendCompilationContext.withOutputType(outputType, () -> Capy.runCompileGenerate(
                new Capy.CompileGenerateOptions(
                outputType,
                inputDir().toString(),
                outputDir().toString(),
                Optional.empty(),
                Optional.empty(),
                Optional.empty(),
                Optional.empty(),
                Optional.empty(),
                false,
                false,
                Capy.LogLevel.WARN
        )).unsafeRun());
    }

    private String compileGenerateStderr() {
        return compileGenerateStderr("java");
    }

    private String compileGenerateStderr(String outputType) {
        var originalError = System.err;
        var buffer = new ByteArrayOutputStream();
        try (var errorStream = new PrintStream(buffer, true, StandardCharsets.UTF_8)) {
            System.setErr(errorStream);
            compileGenerate(outputType);
        } finally {
            System.setErr(originalError);
        }
        return buffer.toString(StandardCharsets.UTF_8);
    }

    private String compileGenerateWithTestsStderr() {
        return compileGenerateWithTestsStderr("java");
    }

    private String compileGenerateWithTestsStderr(String outputType) {
        var originalError = System.err;
        var buffer = new ByteArrayOutputStream();
        try (var errorStream = new PrintStream(buffer, true, StandardCharsets.UTF_8)) {
            System.setErr(errorStream);
            BackendCompilationContext.withOutputType(outputType, () -> Capy.runCompileGenerate(
                    new Capy.CompileGenerateOptions(
                            outputType,
                            inputDir().toString(),
                            outputDir().toString(),
                            Optional.empty(),
                            Optional.of(testInputDir().toString()),
                            Optional.of(testOutputDir().toString()),
                            Optional.empty(),
                            Optional.empty(),
                            false,
                            false,
                            Capy.LogLevel.WARN
                    )).unsafeRun());
        } finally {
            System.setErr(originalError);
        }
        return buffer.toString(StandardCharsets.UTF_8);
    }

    private Path writeSource(String relativePath, String source) throws IOException {
        var path = inputDir().resolve(relativePath);
        Files.createDirectories(path.getParent());
        Files.writeString(path, source);
        return path;
    }

    private Path writeTestSource(String relativePath, String source) throws IOException {
        var path = testInputDir().resolve(relativePath);
        Files.createDirectories(path.getParent());
        Files.writeString(path, source);
        return path;
    }

    private Path generatedPath(Path source) {
        return generatedPath(source, ".java");
    }

    private Path generatedPath(Path source, String extension) {
        var relative = inputDir().relativize(source);
        var fileName = relative.getFileName().toString().replaceFirst("\\.cfun$", extension);
        return outputDir().resolve(relative).resolveSibling(fileName);
    }

    private Path generatedTestPath(Path source) {
        var relative = testInputDir().relativize(source);
        var fileName = relative.getFileName().toString().replaceFirst("\\.cfun$", ".java");
        return testOutputDir().resolve(relative).resolveSibling(fileName);
    }

    private Path inputDir() {
        return tempDir.resolve("input");
    }

    private Path outputDir() {
        return tempDir.resolve("output");
    }

    private Path testInputDir() {
        return tempDir.resolve("test-input");
    }

    private Path testOutputDir() {
        return tempDir.resolve("test-output");
    }

    private void assertJavaCompiles(Path... sources) throws IOException {
        var compiler = ToolProvider.getSystemJavaCompiler();
        assertThat(compiler).as("system Java compiler").isNotNull();
        var classes = Files.createDirectories(tempDir.resolve("compiled-java"));
        var diagnostics = new ByteArrayOutputStream();
        var arguments = new ArrayList<String>();
        arguments.add("-classpath");
        arguments.add(System.getProperty("java.class.path"));
        arguments.add("-d");
        arguments.add(classes.toString());
        for (var source : sources) {
            arguments.add(source.toString());
        }
        var exitCode = compiler.run(
                null,
                diagnostics,
                diagnostics,
                arguments.toArray(String[]::new)
        );
        assertThat(exitCode)
                .as(diagnostics.toString(StandardCharsets.UTF_8))
                .isZero();
    }
}
