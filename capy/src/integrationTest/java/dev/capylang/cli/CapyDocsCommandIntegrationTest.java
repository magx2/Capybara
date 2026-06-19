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

                /// Hides a box
                private fun Box.hidden(): String = "hidden"

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

                /// Hidden answer docs
                private const HIDDEN_ANSWER: int = 7

                /// Describes a tone
                fun Tone.describe(): String = "tone"

                /// Function docs
                fun documented_function(box: Box, wrapped: GenericBox[Box]): GenericBox[Box] = wrapped

                /// Hidden function docs
                private fun hidden_function(): String = "hidden"

                /// Hidden box docs
                private data HiddenBox { value: String }

                /// Hidden circle docs
                private data HiddenCircle { radius: int }

                /// Hidden visible circle docs
                private data HiddenVisibleCircle { radius: int }

                /// Visible shape docs
                union VisibleShape = HiddenVisibleCircle | Box

                /// Hidden square docs
                private data HiddenSquare { size: int }

                /// Hidden shape docs
                private union HiddenShape = HiddenCircle | HiddenSquare

                /// Hidden id docs
                private type hidden_id -> String

                /// Hidden marker docs
                private annotation HiddenMarker on fun {}
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

                    /// Internal field docs
                    private field internal: String = "secret"

                    /// Init docs
                    init {
                        return "ready"
                    }

                    /// Name method docs
                    override def name(): String = this.label

                    /// Render docs
                    def render(prefix: String): String = prefix + this.name()

                    /// Internal render docs
                    private def internal_render(): String = this.internal
                }
                """);
        writeSource(input.resolve("sample/Markers.cfun"), """
                /// Marker docs
                annotation DocMarker on fun {}
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
                .contains("* xref:sample/Markers.adoc[Markers]")
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
                .contains("===== fun Box.describe(): String")
                .contains("Describes a box")
                .contains("[[type-GenericBox]]\n=== data GenericBox[T]")
                .contains("Generic box docs")
                .contains("===== fun GenericBox[T].describe(): String")
                .contains("Describes a generic box")
                .contains("[[type-Shape]]\n=== union Shape")
                .contains("Shape docs")
                .contains("* xref:#type-Circle[Circle]")
                .contains("* xref:#type-Square[Square]")
                .contains("[[type-VisibleShape]]\n=== union VisibleShape")
                .contains("Visible shape docs")
                .contains("=== enum Tone")
                .contains("Tone docs")
                .contains("* `LOW`")
                .contains("* `HIGH`")
                .contains("== Constants")
                .contains("=== const ANSWER: int")
                .contains("Answer docs")
                .contains("===== fun Tone.describe(): String")
                .doesNotContain("=== const:public ANSWER(): int")
                .doesNotContain("=== const:public ANSWER: int")
                .doesNotContain("public")
                .doesNotContain("== Annotations")
                .doesNotContain("HiddenBox")
                .doesNotContain("HiddenCircle")
                .doesNotContain("HiddenVisibleCircle")
                .doesNotContain("HiddenSquare")
                .doesNotContain("HiddenShape")
                .doesNotContain("hidden_id")
                .doesNotContain("hidden_function")
                .doesNotContain("HIDDEN_ANSWER")
                .doesNotContain("HiddenMarker")
                .doesNotContain("Hidden box docs")
                .doesNotContain("Hidden answer docs")
                .doesNotContain("Hidden marker docs")
                .doesNotContain("Hides a box")
                .doesNotContain("\n=== fun Box.describe")
                .doesNotContain("\n=== fun GenericBox[T].describe")
                .doesNotContain("* `name`: `String`")
                .doesNotContain("__capy_schema_type|Box")
                .doesNotContain("ExampleDocs");
        assertThat(docsContent).containsSubsequence("* `HIGH`", "* `LOW`");
        assertThat(docsContent).containsSubsequence("== Functions", "=== fun documented_function", "== Constants", "=== const ANSWER: int", "== Types");

        var objectsFile = output.resolve("sample/Objects.adoc");
        assertThat(objectsFile).isRegularFile();
        var objectsContent = Files.readString(objectsFile);
        assertThat(objectsContent)
                .contains("= Module Objects")
                .contains("== Object-Oriented")
                .contains("=== interface Named")
                .contains("Named docs")
                .contains("===== def name(): String")
                .contains("Name docs")
                .contains("=== trait Greeting")
                .contains("Greeting docs")
                .contains("===== def greeting(): String")
                .contains("Greeting method docs")
                .contains("=== open class Widget(label: String)")
                .contains("Widget docs")
                .contains("==== Parents")
                .contains("* xref:#type-oo-sample-Greeting[Greeting]")
                .contains("* xref:#type-oo-sample-Named[Named]")
                .contains("==== Fields")
                .contains("* `field label`: String (default)")
                .contains("Label field docs")
                .contains("===== init 1")
                .contains("Init docs")
                .contains("===== def render(prefix: String): String")
                .contains("Render docs")
                .doesNotContain("public")
                .doesNotContain("internal")
                .doesNotContain("Internal field docs")
                .doesNotContain("Internal render docs")
                .doesNotContain("== Functions")
                .doesNotContain("== Constants")
                .doesNotContain("== Annotations")
                .doesNotContain("== Types")
                .doesNotContain("__capy_oo_method|Widget|render")
                .doesNotContain("\n=== oo:");
        assertThat(objectsContent).containsSubsequence("* xref:#type-oo-sample-Greeting[Greeting]", "* xref:#type-oo-sample-Named[Named]");

        var markersFile = output.resolve("sample/Markers.adoc");
        assertThat(markersFile).isRegularFile();
        assertThat(Files.readString(markersFile))
                .contains("= Module Markers")
                .contains("== Annotations")
                .contains("=== annotation @DocMarker")
                .contains("Marker docs")
                .contains("* Repeatable: no")
                .contains("* Targets: fun")
                .doesNotContain("public")
                .doesNotContain("== Functions")
                .doesNotContain("== Constants")
                .doesNotContain("== Types");

        var moreFile = output.resolve("sample/nested/More.adoc");
        assertThat(moreFile).isRegularFile();
        assertThat(Files.readString(moreFile))
                .contains("= Module More")
                .contains("== Types")
                .contains("=== data More")
                .doesNotContain("== Functions")
                .doesNotContain("== Constants")
                .doesNotContain("== Annotations");
    }

    private static void writeSource(Path file, String source) throws IOException {
        Files.createDirectories(file.getParent());
        Files.writeString(file, source);
    }
}
