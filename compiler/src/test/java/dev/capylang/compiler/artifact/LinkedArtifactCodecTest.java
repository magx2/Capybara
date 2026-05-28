package dev.capylang.compiler.artifact;

import dev.capylang.compiler.CompiledFunction;
import dev.capylang.compiler.CompiledModule;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.PrimitiveLinkedType;
import dev.capylang.compiler.expression.CompiledIntValue;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class LinkedArtifactCodecTest {
    @Test
    void readsLegacyJavaProducedLinkedProgram() throws Exception {
        var program = LinkedArtifactCodec.readProgram(resource("artifacts/legacy-linked-program.json"));

        assertMinimalProgram(program);
    }

    @Test
    void readsLegacyJavaProducedLinkedModule() throws Exception {
        var module = LinkedArtifactCodec.readModule(resource("artifacts/legacy-linked-module.json"));

        assertMinimalModule(module);
    }

    @Test
    void readsSchemaLinkedProgramWithoutPublicJavaClassNames() throws Exception {
        var json = resourceText("artifacts/schema-linked-program.json");

        assertThat(json).doesNotContain("@class", "dev.capylang.", "java.util.");
        assertMinimalProgram(LinkedArtifactCodec.readProgram(stream(json)));
    }

    @Test
    void readsSchemaLinkedModule() throws Exception {
        assertMinimalModule(LinkedArtifactCodec.readModule(resource("artifacts/schema-linked-module.json")));
    }

    @Test
    void writesSchemaLinkedProgramWithoutDefaultTypingMetadata() throws Exception {
        var json = LinkedArtifactCodec.writeProgramAsString(minimalProgram());

        assertThat(json)
                .contains("\"format\" : \"capybara.linked.program\"")
                .contains("\"schemaVersion\" : 1")
                .contains("\"primitive\" : \"INT\"")
                .doesNotContain("@class", "dev.capylang.", "java.util.");
        assertMinimalProgram(LinkedArtifactCodec.readProgram(stream(json)));
    }

    @Test
    void rejectsUnsupportedSchemaVersionsDeterministically() {
        assertThatThrownBy(() -> LinkedArtifactCodec.readProgram(resource("artifacts/unsupported-linked-program-version.json")))
                .isInstanceOf(LinkedArtifactCodec.LinkedArtifactSchemaException.class)
                .hasMessage("Unsupported linked artifact schema version `999` for `capybara.linked.program`. Supported versions: 1.");
    }

    private static CompiledProgram minimalProgram() {
        return new CompiledProgram(List.of(minimalModule()));
    }

    private static CompiledModule minimalModule() {
        return new CompiledModule(
                "Main",
                "foo",
                Map.of(),
                List.of(new CompiledFunction(
                        "answer",
                        PrimitiveLinkedType.INT,
                        List.of(),
                        new CompiledIntValue("7"),
                        List.of()
                )),
                List.of()
        );
    }

    private static void assertMinimalProgram(CompiledProgram program) {
        assertThat(program.modules()).hasSize(1);
        assertMinimalModule(program.modules().first());
        assertThat(program.objectOrientedModules()).isEmpty();
        assertThat(program.nativeProviders().providers()).isEmpty();
        assertThat(program.nativeProviderCatalog().declarations()).isEmpty();
        assertThat(program.nativeProviderCatalog().bindings()).isEmpty();
    }

    private static void assertMinimalModule(CompiledModule module) {
        assertThat(module.name()).isEqualTo("Main");
        assertThat(module.path()).isEqualTo("foo");
        assertThat(module.types()).isEmpty();
        assertThat(module.functions()).hasSize(1);
        var function = module.functions().first();
        assertThat(function.name()).isEqualTo("answer");
        assertThat(function.returnType()).isEqualTo(PrimitiveLinkedType.INT);
        assertThat(function.expression()).isEqualTo(new CompiledIntValue("7"));
    }

    private static ByteArrayInputStream resource(String name) throws IOException {
        return stream(resourceText(name));
    }

    private static String resourceText(String name) throws IOException {
        try (var input = LinkedArtifactCodecTest.class.getClassLoader().getResourceAsStream(name)) {
            if (input == null) {
                throw new IllegalArgumentException("Missing test resource: " + name);
            }
            return new String(input.readAllBytes(), StandardCharsets.UTF_8);
        }
    }

    private static ByteArrayInputStream stream(String json) {
        return new ByteArrayInputStream(json.getBytes(StandardCharsets.UTF_8));
    }
}
