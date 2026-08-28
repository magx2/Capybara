package dev.capylang.compiler;

import capy.lang.Program;
import dev.capylang.cli.Capy;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

class CapyLinkedProgramCompatibilityIntegrationTest {
    private static final String EMPTY_DOCUMENTATION_FIELD =
            "\"documentation\":{\"$capybaraTypedJson\":\"array\",\"kind\":\"List\",\"value\":[]},";

    @TempDir
    Path tempDir;

    @Test
    void readsLinkedFunctionsWithoutDocumentationField() throws Exception {
        var libraryInput = tempDir.resolve("library-input");
        var libraryOutput = tempDir.resolve("library-output");
        var libraryLinkedOutput = tempDir.resolve("library-linked");
        writeSource(libraryInput.resolve("legacy/Library.cfun"), """
                fun legacy_value(): int = 7
                """);

        assertSuccess(runCompileGenerate(libraryInput, libraryOutput, Optional.of(libraryLinkedOutput), Optional.empty()));
        removeEmptyDocumentationFields(libraryLinkedOutput.resolve("program.json"));

        var consumerInput = tempDir.resolve("consumer-input");
        var consumerOutput = tempDir.resolve("consumer-output");
        writeSource(consumerInput.resolve("consumer/App.cfun"), """
                from /legacy/Library import { legacy_value }

                fun use_legacy(): int = legacy_value()
                """);

        assertSuccess(runCompileGenerate(consumerInput, consumerOutput, Optional.empty(), Optional.of(libraryLinkedOutput)));
        assertThat(consumerOutput.resolve("consumer/App.java")).isRegularFile();
    }

    @Test
    void rejectsPrimitiveValuesForPrimitiveBackedFieldsFromLinkedDataSchemas() throws Exception {
        var libraryInput = tempDir.resolve("typed-library-input");
        var libraryOutput = tempDir.resolve("typed-library-output");
        var libraryLinkedOutput = tempDir.resolve("typed-library-linked");
        writeSource(libraryInput.resolve("support/Result.cfun"), """
                data Error { kind: String, message: String }
                data Success[T] { value: T }
                union Result[T] = Success[T] | Error
                """);
        writeSource(libraryInput.resolve("support/Digit.cfun"), """
                from /support/Result import { Success }

                type digit -> int with constructor {
                    Success { value }
                }
                """);
        writeSource(libraryInput.resolve("Kaprekar.cfun"), """
                from /support/Result import { Success }
                from /support/Digit import { digit }

                data Kaprekar {
                    first: digit,
                    second: digit,
                    third: digit,
                    fourth: digit
                } with constructor {
                    Success { * { first, second, third, fourth } }
                }
                """);

        assertSuccess(runCompileGenerate(
                libraryInput,
                libraryOutput,
                Optional.of(libraryLinkedOutput),
                Optional.empty()
        ));

        var consumerInput = tempDir.resolve("typed-consumer-input");
        var consumerOutput = tempDir.resolve("typed-consumer-output");
        writeSource(consumerInput.resolve("Kaprekar.test.cfun"), """
                from /support/Result import { Result }
                from /Kaprekar import { Kaprekar }

                const K1234: Result[Kaprekar] = Kaprekar { 1, 2, 3, 4 }
                """);

        var result = runCompileGenerate(
                consumerInput,
                consumerOutput,
                Optional.empty(),
                Optional.of(libraryLinkedOutput)
        );

        assertThat(result).isInstanceOf(Program.Failed.class);
        assertThat(consumerOutput.resolve("Kaprekar_test.java")).doesNotExist();
    }

    private static Program runCompileGenerate(
            Path input,
            Path output,
            Optional<Path> linkedOutput,
            Optional<Path> libs
    ) {
        return Capy.runCompileGenerate(new Capy.CompileGenerateOptions(
                "java",
                input.toString(),
                output.toString(),
                linkedOutput.map(Path::toString),
                Optional.empty(),
                Optional.empty(),
                libs.map(Path::toString),
                Optional.empty(),
                false,
                false,
                Capy.LogLevel.WARN
        )).unsafeRun();
    }

    private static void writeSource(Path file, String source) throws IOException {
        Files.createDirectories(file.getParent());
        Files.writeString(file, source);
    }

    private static void removeEmptyDocumentationFields(Path programJson) throws IOException {
        var content = Files.readString(programJson);
        assertThat(content).contains(EMPTY_DOCUMENTATION_FIELD);
        Files.writeString(programJson, content.replace(EMPTY_DOCUMENTATION_FIELD, ""));
    }

    private static void assertSuccess(Program program) {
        assertThat(program).isSameAs(Program.Success.INSTANCE);
    }
}
