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
