package dev.capylang.cli;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class CapyJarStdlibConstantIntegrationTest {
    @TempDir
    Path tempDir;

    @Test
    void shouldLowerImportedStdlibValuesForJavaScriptAndPythonFromPackagedJar() throws Exception {
        var input = Files.createDirectories(tempDir.resolve("input"));
        Files.writeString(input.resolve("main.cfun"), """
                from /capy/lang/Primitives import { digit, ONE_DIGIT, SIX_DIGIT }
                from /capy/lang/Result import { Result }

                data Digits { first: digit, second: digit }

                const DIGITS: Digits = Digits { first: SIX_DIGIT, second: ONE_DIGIT }

                fun checked(value: int): Result[digit] = digit { value }
                """);

        var javaScript = compileGenerate("javascript", input);
        assertThat(javaScript)
                .contains("{\"first\": 6, \"second\": 1}")
                .contains("__capy_digit(value)")
                .doesNotContain("__capy_import_capy_lang_Primitives")
                .doesNotContain("SIX_DIGIT__")
                .doesNotContain("ONE_DIGIT__");

        var python = compileGenerate("python", input);
        assertThat(python)
                .contains("{\"first\": 6, \"second\": 1}")
                .contains("__capy_digit(value)")
                .doesNotContain("__import__(\"capy.lang.Primitives\"")
                .doesNotContain(".SIX_DIGIT")
                .doesNotContain(".ONE_DIGIT");
    }

    private String compileGenerate(String backend, Path input) throws Exception {
        var output = tempDir.resolve("output-" + backend);
        var process = new ProcessBuilder(List.of(
                ProcessHandle.current().info().command().orElseThrow(),
                "-jar", System.getProperty("capy.jar.path"),
                "compile-generate", backend,
                "--input", input.toString(),
                "--output", output.toString()
        )).redirectErrorStream(true).start();
        var diagnostic = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);

        assertThat(process.waitFor()).describedAs(diagnostic).isZero();
        var extension = backend.equals("python") ? ".py" : ".js";
        return Files.readString(output.resolve("main" + extension));
    }
}
