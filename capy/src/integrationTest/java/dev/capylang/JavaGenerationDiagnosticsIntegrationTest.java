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
import java.util.Optional;
import javax.tools.ToolProvider;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class JavaGenerationDiagnosticsIntegrationTest {
    @TempDir
    Path tempDir;

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
                .contains("effect.flatMap((__) -> next)")
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

    private Path writeSource(String relativePath, String source) throws IOException {
        var path = inputDir().resolve(relativePath);
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

    private Path inputDir() {
        return tempDir.resolve("input");
    }

    private Path outputDir() {
        return tempDir.resolve("output");
    }

    private void assertJavaCompiles(Path source) throws IOException {
        var compiler = ToolProvider.getSystemJavaCompiler();
        assertThat(compiler).as("system Java compiler").isNotNull();
        var classes = Files.createDirectories(tempDir.resolve("compiled-java"));
        var diagnostics = new ByteArrayOutputStream();
        var exitCode = compiler.run(
                null,
                diagnostics,
                diagnostics,
                "-classpath", System.getProperty("java.class.path"),
                "-d", classes.toString(),
                source.toString()
        );
        assertThat(exitCode)
                .as(diagnostics.toString(StandardCharsets.UTF_8))
                .isZero();
    }
}
