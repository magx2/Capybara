package dev.capylang.compiler;

import capy.lang.Either;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.SourceKind;
import org.junit.jupiter.api.Test;

import java.util.LinkedHashSet;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;

class CapybaraCompilerDocumentationIntegrationTest {
    @Test
    void shouldCompileFunctionalDocumentation() {
        var source = """
                /// Data docs
                data DocumentedData { value: String }

                /// Primitive docs
                type documented_id -> String

                /// Annotation docs
                annotation DocMarker on fun {}

                /// Deriver docs
                deriver DocDeriver {
                    /// Deriver method docs
                    fun describe(): String = "value"
                }

                /// Constant docs
                const DefaultDoc: String = "value"

                /// Function docs
                fun documented_function(): String = "ok"
                """;

        var module = compileProgram(List.of(rawModule("Docs", "/sample", source, SourceKind.FUNCTIONAL)))
                .modules()
                .getFirst();

        assertThat(function(module, "__capy_schema_type|DocumentedData").documentation())
                .containsExactly("Data docs");
        assertThat(module.visiblePrimitiveBackedTypes().get("documented_id").documentation())
                .containsExactly("Primitive docs");
        assertThat(function(module, "__capy_schema_type|documented_id").documentation())
                .containsExactly("Primitive docs");
        assertThat(module.annotations().get("DocMarker").documentation())
                .containsExactly("Annotation docs");
        assertThat(module.derivers().get("DocDeriver").documentation())
                .containsExactly("Deriver docs");
        assertThat(module.derivers().get("DocDeriver").methods().getFirst().documentation())
                .containsExactly("Deriver method docs");
        assertThat(function(module, "DefaultDoc").documentation())
                .containsExactly("Constant docs");
        assertThat(function(module, "documented_function").documentation())
                .containsExactly("Function docs");
    }

    @Test
    void shouldCompileObjectOrientedDocumentation() {
        var source = """
                /// Interface docs
                interface Named {
                    /// Interface method docs
                    def name(): String
                }

                /// Class docs
                class Box {
                    /// Field docs
                    field value: String

                    /// Init docs
                    init {
                        return "ready"
                    }

                    /// Method docs
                    def get(): String = "value"
                }
                """;

        var program = compileProgram(List.of(rawModule("Objects", "/sample", source, SourceKind.OBJECT_ORIENTED)));
        var objectUnit = program.objectOrientedModules().getFirst();
        var objectInterface = objectUnit.interfaces().getFirst();
        var objectClass = objectUnit.classes().getFirst();
        var module = program.modules().getFirst();

        assertThat(objectInterface.documentation()).containsExactly("Interface docs");
        assertThat(objectInterface.methods().getFirst().documentation()).containsExactly("Interface method docs");
        assertThat(objectClass.documentation()).containsExactly("Class docs");
        assertThat(objectClass.fields().getFirst().documentation()).containsExactly("Field docs");
        assertThat(objectClass.initBlocks().getFirst().documentation()).containsExactly("Init docs");
        assertThat(objectClass.methods().getFirst().documentation()).containsExactly("Method docs");

        assertThat(function(module, "__capy_oo_interface|Named").documentation())
                .containsExactly("Interface docs");
        assertThat(function(module, "__capy_oo_interface_method|Named|name").documentation())
                .containsExactly("Interface method docs");
        assertThat(function(module, "__capy_oo_class|Box").documentation())
                .containsExactly("Class docs");
        assertThat(function(module, "__capy_oo_field|Box|0").documentation())
                .containsExactly("Field docs");
        assertThat(function(module, "__capy_oo_init|Box|0").documentation())
                .containsExactly("Init docs");
        assertThat(function(module, "__capy_oo_method|Box|get").documentation())
                .containsExactly("Method docs");
    }

    private static CompiledProgram compileProgram(List<RawModule> rawModules) {
        var result = CapybaraCompiler.compile(rawModules, new LinkedHashSet<>(), emptyNativeProviders(), emptyNativeProviders()).unsafeRun();
        if (result instanceof Either.Right<?, ?> error) {
            fail(error.value().toString());
        }
        return (CompiledProgram) ((Either.Left<?, ?>) result).value();
    }

    private static CompiledFunction function(CompiledModule module, String name) {
        return module.functions().stream()
                .filter(function -> function.name().equals(name))
                .findFirst()
                .orElseThrow();
    }

    private static RawModule rawModule(String name, String path, String input, SourceKind sourceKind) {
        return new RawModule(name, path, input, sourceKind);
    }

    private static NativeProviderManifest emptyNativeProviders() {
        return new NativeProviderManifest(List.of());
    }
}
