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
        boolean hasGeneratedJava;
        try (var generatedFiles = Files.walk(generatedOutput)) {
            hasGeneratedJava = generatedFiles.anyMatch(path -> path.toString().endsWith(".java"));
        }

        boolean hasLinkedFiles;
        try (var linkedFiles = Files.walk(linkedOutput)) {
            hasLinkedFiles = linkedFiles.anyMatch(Files::isRegularFile);
        }

        assertThat(hasGeneratedJava).isTrue();
        assertThat(hasLinkedFiles).isTrue();
    }
}
