package dev.capylang.compiler;

import dev.capylang.compiler.parser.ObjectOriented;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.SourceKind;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.TreeSet;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;

class CapybaraCompilerDocumentationIntegrationTest {
    @Test
    void shouldCompileFunctionalComments() {
        var source = """
                /// Data docs
                data DocumentedData { value: String }

                /// Union docs
                union DocumentedUnion = DocumentedData

                /// Primitive docs
                type documented_id -> int

                /// Annotation docs
                annotation DocMarker on fun {}

                /// Constant docs
                const DefaultDoc: String = "value"

                /// Function docs
                fun documented_function(): String = "ok"
                """;

        var module = module(compileProgram(List.of(new RawModule("Docs", "/sample", source))), "Docs");

        assertThat(module.types().get("DocumentedData")).isInstanceOfSatisfying(CompiledDataType.class, type ->
                assertThat(type.comments()).containsExactly("Data docs"));
        assertThat(module.types().get("DocumentedUnion")).isInstanceOfSatisfying(CompiledDataParentType.class, type ->
                assertThat(type.comments()).containsExactly("Union docs"));
        assertThat(module.types().get("documented_id")).isInstanceOfSatisfying(CompiledPrimitiveBackedType.class, type ->
                assertThat(type.comments()).containsExactly("Primitive docs"));
        assertThat(module.annotations().get("DocMarker").comments())
                .containsExactly("Annotation docs");
        assertThat(function(module, "DefaultDoc").comments())
                .containsExactly("Constant docs");
        assertThat(function(module, "documented_function").comments())
                .containsExactly("Function docs");
    }

    @Test
    void shouldCompileObjectOrientedComments() {
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

        var objectModule = compileProgram(List.of(new RawModule("Objects", "/sample", source, SourceKind.OBJECT_ORIENTED)))
                .objectOrientedModules()
                .getFirst()
                .objectOriented();
        var named = objectModule.definitions().stream()
                .filter(ObjectOriented.InterfaceDeclaration.class::isInstance)
                .map(ObjectOriented.InterfaceDeclaration.class::cast)
                .findFirst()
                .orElseThrow();
        var box = objectModule.definitions().stream()
                .filter(ObjectOriented.ClassDeclaration.class::isInstance)
                .map(ObjectOriented.ClassDeclaration.class::cast)
                .findFirst()
                .orElseThrow();

        assertThat(named.comments()).containsExactly("Interface docs");
        assertThat(named.members().stream()
                .filter(ObjectOriented.MethodDeclaration.class::isInstance)
                .map(ObjectOriented.MethodDeclaration.class::cast)
                .findFirst()
                .orElseThrow()
                .comments()).containsExactly("Interface method docs");
        assertThat(box.comments()).containsExactly("Class docs");
        assertThat(box.members().stream()
                .filter(ObjectOriented.FieldDeclaration.class::isInstance)
                .map(ObjectOriented.FieldDeclaration.class::cast)
                .findFirst()
                .orElseThrow()
                .comments()).containsExactly("Field docs");
        assertThat(box.members().stream()
                .filter(member -> member.getClass().getSimpleName().contains("Init"))
                .findFirst()
                .orElseThrow()).extracting("comments")
                .isEqualTo(List.of("Init docs"));
        assertThat(box.members().stream()
                .filter(ObjectOriented.MethodDeclaration.class::isInstance)
                .map(ObjectOriented.MethodDeclaration.class::cast)
                .filter(method -> method.name().equals("get"))
                .findFirst()
                .orElseThrow()
                .comments()).containsExactly("Method docs");
    }

    private static CompiledProgram compileProgram(List<RawModule> rawModules) {
        var result = CapybaraCompiler.INSTANCE.compile(rawModules, new TreeSet<>());
        if (result instanceof Result.Error<CompiledProgram> error) {
            fail(error.errors().toString());
        }
        return ((Result.Success<CompiledProgram>) result).value();
    }

    private static CompiledModule module(CompiledProgram program, String name) {
        return program.modules().stream()
                .filter(module -> module.name().equals(name))
                .findFirst()
                .orElseThrow();
    }

    private static CompiledFunction function(CompiledModule module, String name) {
        return module.functions().stream()
                .filter(function -> function.name().equals(name))
                .findFirst()
                .orElseThrow();
    }
}
