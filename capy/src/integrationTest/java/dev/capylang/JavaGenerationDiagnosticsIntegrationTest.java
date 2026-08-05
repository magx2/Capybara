package dev.capylang;

import dev.capylang.cli.Capy;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;

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
        Capy.runCompileGenerate(new Capy.CompileGenerateOptions(
                "java",
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
        )).unsafeRun();
    }

    private Path writeSource(String relativePath, String source) throws IOException {
        var path = inputDir().resolve(relativePath);
        Files.createDirectories(path.getParent());
        Files.writeString(path, source);
        return path;
    }

    private Path generatedPath(Path source) {
        var relative = inputDir().relativize(source);
        var fileName = relative.getFileName().toString().replaceFirst("\\.cfun$", ".java");
        return outputDir().resolve(relative).resolveSibling(fileName);
    }

    private Path inputDir() {
        return tempDir.resolve("input");
    }

    private Path outputDir() {
        return tempDir.resolve("output");
    }
}
