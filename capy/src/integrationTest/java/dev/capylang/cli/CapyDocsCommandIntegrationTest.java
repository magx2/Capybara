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

                /// Answer docs
                const ANSWER: int = 42

                /// Describes a tone
                fun Tone.describe(): String = "tone"

                /// Function docs
                fun documented_function(box: Box, wrapped: GenericBox[Box]): GenericBox[Box] = wrapped
                """);
        writeSource(input.resolve("sample/Objects.coo"), """
                /// Named docs
                interface Named {
                    /// Name docs
                    def name(): String
                }

                /// Greeting docs
                trait Greeting {
                    /// Greeting method docs
                    def greeting(): String = "hello"
                }

                /// Widget docs
                open class Widget(label: String): Named, Greeting {
                    /// Label field docs
                    field label: String = label

                    /// Init docs
                    init {
                        return "ready"
                    }

                    /// Name method docs
                    override def name(): String = this.label

                    /// Render docs
                    def render(prefix: String): String = prefix + this.name()
                }
                """);
        writeSource(input.resolve("sample/nested/More.cfun"), """
                /// More docs
                data More { value: String }
                """);

        var program = Capy.main(List.of("docs", "-i", input.toString(), "-o", output.toString())).unsafeRun();

        assertThat(program).isSameAs(Program.Success.INSTANCE);
        var indexFile = output.resolve("index.adoc");
        assertThat(indexFile).isRegularFile();
        assertThat(Files.readString(indexFile))
                .contains("= Capybara Documentation")
                .contains("=== sample")
                .contains("* xref:sample/Docs.adoc[Docs]")
                .contains("* xref:sample/Objects.adoc[Objects]")
                .contains("==== nested")
                .contains("* xref:sample/nested/More.adoc[More]")
                .doesNotContain("* xref:sample/Docs.adoc[sample/Docs]")
                .doesNotContain("=== sample/nested");

        var docsFile = output.resolve("sample/Docs.adoc");
        assertThat(docsFile).isRegularFile();
        var docsContent = Files.readString(docsFile);
        assertThat(docsContent)
                .contains("= Module Docs")
                .contains("documented_function(box: xref:#type-Box[Box], wrapped: xref:#type-GenericBox[GenericBox][xref:#type-Box[Box]]): xref:#type-GenericBox[GenericBox][xref:#type-Box[Box]]")
                .contains("Function docs")
                .contains("[[type-Box]]\n=== data Box")
                .contains("Box docs")
                .contains("* `value`: String")
                .contains("===== public Box.describe(): String")
                .contains("Describes a box")
                .contains("[[type-GenericBox]]\n=== data GenericBox[T]")
                .contains("Generic box docs")
                .contains("===== public GenericBox[T].describe(): String")
                .contains("Describes a generic box")
                .contains("[[type-Shape]]\n=== union Shape")
                .contains("Shape docs")
                .contains("* xref:#type-Circle[Circle]")
                .contains("* xref:#type-Square[Square]")
                .contains("=== enum Tone")
                .contains("Tone docs")
                .contains("* `LOW`")
                .contains("* `HIGH`")
                .contains("== Constants")
                .contains("=== const:public ANSWER: int")
                .contains("Answer docs")
                .contains("===== public Tone.describe(): String")
                .doesNotContain("=== const:public ANSWER(): int")
                .doesNotContain("\n=== public Box.describe")
                .doesNotContain("\n=== public GenericBox[T].describe")
                .doesNotContain("* `name`: `String`")
                .doesNotContain("__capy_schema_type|Box")
                .doesNotContain("ExampleDocs");
        assertThat(docsContent).containsSubsequence("* `HIGH`", "* `LOW`");
        assertThat(docsContent).containsSubsequence("== Functions", "=== public documented_function", "== Constants", "=== const:public ANSWER: int", "== Annotations");

        var objectsFile = output.resolve("sample/Objects.adoc");
        assertThat(objectsFile).isRegularFile();
        var objectsContent = Files.readString(objectsFile);
        assertThat(objectsContent)
                .contains("= Module Objects")
                .contains("== Object-Oriented")
                .contains("=== public interface Named")
                .contains("Named docs")
                .contains("===== public name(): String")
                .contains("Name docs")
                .contains("=== public trait Greeting")
                .contains("Greeting docs")
                .contains("===== public greeting(): String")
                .contains("Greeting method docs")
                .contains("=== public open class Widget(label: String)")
                .contains("Widget docs")
                .contains("==== Parents")
                .contains("* xref:#type-oo-sample-Greeting[Greeting]")
                .contains("* xref:#type-oo-sample-Named[Named]")
                .contains("==== Fields")
                .contains("* `public label`: String (default)")
                .contains("Label field docs")
                .contains("===== init 1")
                .contains("Init docs")
                .contains("===== public render(prefix: String): String")
                .contains("Render docs")
                .doesNotContain("__capy_oo_method|Widget|render")
                .doesNotContain("\n=== oo:");
        assertThat(objectsContent).containsSubsequence("* xref:#type-oo-sample-Greeting[Greeting]", "* xref:#type-oo-sample-Named[Named]");
    }

    private static void writeSource(Path file, String source) throws IOException {
        Files.createDirectories(file.getParent());
        Files.writeString(file, source);
    }
}
