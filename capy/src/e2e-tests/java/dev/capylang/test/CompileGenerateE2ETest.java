package dev.capylang.test;

import dev.capylang.Capy;
import dev.capylang.compiler.CompiledModule;
import dev.capylang.compiler.OutputType;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.TreeSet;

import static org.assertj.core.api.Assertions.assertThat;

class CompileGenerateE2ETest {
    @ParameterizedTest(name = "compile-generate fixture: {0}")
    @ValueSource(strings = {"simple", "imports"})
    void shouldCompileAndGenerateFromFixtures(String fixtureName, @TempDir Path tempDir) throws Exception {
        var input = Path.of("src/e2e-tests/resources/compile-generate", fixtureName);
        var generatedOutput = tempDir.resolve("generated");
        var linkedOutput = tempDir.resolve("linked");
        var errors = new ByteArrayOutputStream();

        int exitCode = Capy.compileGenerate(
                OutputType.JAVA,
                input,
                generatedOutput,
                linkedOutput,
                null,
                null,
                new TreeSet<CompiledModule>(),
                false,
                false,
                new PrintStream(errors)
        );

        assertThat(exitCode).isZero();
        assertThat(errors.toString()).isBlank();
        assertThat(Files.walk(generatedOutput).anyMatch(path -> path.toString().endsWith(".java"))).isTrue();
        assertThat(Files.walk(linkedOutput).anyMatch(Files::isRegularFile)).isTrue();
    }
}
