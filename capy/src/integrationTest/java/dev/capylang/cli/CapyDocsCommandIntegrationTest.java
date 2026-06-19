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
                /// Box docs
                data Box { value: String }

                /// Describes a box
                fun Box.describe(): String = "box"

                /// Generic box docs
                data GenericBox[T] { value: T }

                /// Describes a generic box
                fun GenericBox[T].describe(): String = "generic"

                /// Shape docs
                union Shape = Circle | Square
                /// Circle docs
                data Circle { radius: int }
                data Square { size: int }

                /// Tone docs
                enum Tone { LOW, HIGH }

                /// Describes a tone
                fun Tone.describe(): String = "tone"

                /// Function docs
                fun documented_function(message: String): String = message
                """);

        var program = Capy.main(List.of("docs", "-i", input.toString(), "-o", output.toString())).unsafeRun();

        assertThat(program).isSameAs(Program.Success.INSTANCE);
        var indexFile = output.resolve("index.adoc");
        assertThat(indexFile).isRegularFile();
        assertThat(Files.readString(indexFile))
                .contains("= Capybara Documentation")
                .contains("* xref:sample/Docs.adoc[sample/Docs]");

        var docsFile = output.resolve("sample/Docs.adoc");
        assertThat(docsFile).isRegularFile();
        assertThat(Files.readString(docsFile))
                .contains("= Module Docs")
                .contains("documented_function(message: String): String")
                .contains("Function docs")
                .contains("=== data Box")
                .contains("Box docs")
                .contains("* `value`: `String`")
                .contains("===== public Box.describe(): String")
                .contains("Describes a box")
                .contains("=== data GenericBox[T]")
                .contains("Generic box docs")
                .contains("===== public GenericBox[T].describe(): String")
                .contains("Describes a generic box")
                .contains("=== union Shape")
                .contains("Shape docs")
                .contains("* `Circle`")
                .contains("* `Square`")
                .contains("=== enum Tone")
                .contains("Tone docs")
                .contains("* `LOW`")
                .contains("* `HIGH`")
                .contains("===== public Tone.describe(): String")
                .doesNotContain("\n=== public Box.describe")
                .doesNotContain("\n=== public GenericBox[T].describe")
                .doesNotContain("* `name`: `String`")
                .doesNotContain("__capy_schema_type|Box")
                .doesNotContain("ExampleDocs");
    }

    private static void writeSource(Path file, String source) throws IOException {
        Files.createDirectories(file.getParent());
        Files.writeString(file, source);
    }
}
