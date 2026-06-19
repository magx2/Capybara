package dev.capylang.cli;

import capy.lang.Program;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class CapyDocsCommandIntegrationTest {
    @TempDir
    Path tempDir;

    @Test
    void rendersAsciiDocForSourceDirectory() throws Exception {
        var input = tempDir.resolve("src");
        var output = tempDir.resolve("docs");
        writeSource(input.resolve("sample/Docs.cfun"), """
                /// Function docs
                fun documented_function(message: String): String = message
                """);

        var program = Capy.main(List.of("docs", "-i", input.toString(), "-o", output.toString())).unsafeRun();

        assertThat(program).isSameAs(Program.Success.INSTANCE);
        var docsFile = output.resolve("sample/Docs.adoc");
        assertThat(docsFile).isRegularFile();
        assertThat(Files.readString(docsFile))
                .contains("= Module Docs")
                .contains("documented_function(message: String): String")
                .contains("Function docs")
                .doesNotContain("ExampleDocs");
    }

    private static void writeSource(Path file, String source) throws IOException {
        Files.createDirectories(file.getParent());
        Files.writeString(file, source);
    }
}
